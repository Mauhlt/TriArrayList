const std = @import("std");
const testing = std.testing;
const Alignment = std.mem.Alignment;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

/// Goals:
/// Fast Insertions = O(1)
/// Fast Deletions = O(1)
/// Fast Access = O(1)
/// Stable Ids
/// Tri Array List: Data, Indices, Ids
pub fn Aligned(comptime T: type, comptime alignment: ?Alignment) type {
    if (alignment) |a|
        if (a.toByteUnits() == @alignOf(T))
            return Aligned(T, null);

    return struct {
        /// Indices = array of indices into items
        /// Ids = array of positions from items into indices,
        /// Items = array of items,
        /// Capacity = # of items each array can maximally contain
        indices: []usize = &[_]usize{},
        ids: []usize = &[_]usize{},
        items: []T = &[_]T{},
        capacity: usize = 0,

        pub const empty = @This(){
            .indices = &.{},
            .ids = &.{},
            .items = &.{},
            .capacity = 0,
        };

        pub const Slice = if (alignment) |a| ([]align(a.toByteUnits()) T) else []T;

        const init_capacity: comptime_int = @max(1, std.atomic.cache_line / @sizeOf(T));

        pub fn SentinelSlice(comptime s: T) type {
            return if (alignment) |a| ([:s]align(a.toByteUnits()) T) else [:s]T;
        }

        /// Init with capacity to hold `num` items.
        /// The capacity will equal `num`.
        /// Deinit with `deinit` or `toOwnedSlice`.
        pub fn initCapacity(allo: Allocator, num: usize) Allocator.Error!@This() {
            var self: @This() = .empty;
            try self.ensureTotalCapacityPrecise(allo, num);
            return self;
        }

        /// Init with externally managed memory
        /// Buffer determines capacity + length set to 0
        /// All fns that accept allocator will cause illegal behavior
        pub fn initSlice(
            index_buffer: []usize,
            id_buffer: []usize,
            item_buffer: Slice,
        ) @This() {
            assert(index_buffer.len == id_buffer.len);
            assert(index_buffer.len == item_buffer.len);
            return @This(){
                .indices = index_buffer[0..0],
                .ids = id_buffer[0..0],
                .items = item_buffer[0..0],
                .capacity = index_buffer.len,
            };
        }

        /// Init with externally managed memory
        /// Buffer deterimines capacity + length set to 0
        /// All fns that accept allocator will cause illegal behavior
        /// Always assumes sentinels for index and id are 0.
        /// Allows user to change sentinel for item.
        pub fn initSentinelSlice(
            comptime sentinel: T,
            index_buffer: [:0]usize,
            id_buffer: [:0]usize,
            item_buffer: [:sentinel]T,
        ) @This() {
            const len = item_buffer.len + 1;
            assert(index_buffer.len == id_buffer.len);
            assert(index_buffer.len == item_buffer.len);
            return @This(){
                .indices = index_buffer[0..0],
                .ids = id_buffer[0..0],
                .items = item_buffer[0..0],
                .capacity = len,
            };
        }

        /// Release allocated memory
        pub fn deinit(self: *@This(), allo: Allocator) void {
            allo.free(self.allocatedIndexSlice());
            allo.free(self.allocatedIdSlice());
            allo.free(self.allocatedItemSlice());
            self.* = undefined;
        }

        /// TriArrayList takes ownership of slice.
        /// Create indices and ids using vectors and loops.
        /// Deinit with `deinit` or `toOwnedSlice` or `toOwnedSliceSentinel`.
        /// O(n)
        pub fn fromOwnedSlice(allo: Allocator, slice: Slice) Allocator.Error!@This() {
            const len = slice.len;
            const indices = try allo.alignedAlloc(usize, null, len);
            fillIndices(indices, 0);
            const ids = try allo.alignedAlloc(usize, null, len);
            @memcpy(ids, indices);
            return @This(){
                .indices = indices,
                .ids = ids,
                .items = slice,
                .capacity = slice.len,
            };
        }

        /// TriArrayList takes ownership of slice sentinel.
        /// Creates indices and ids using vectors and loops.
        /// Deinit with `deinit` or `toOwnedSlice` or `toOwnedSliceSentinel`.
        /// O(n)
        pub fn fromOwnedSliceSentinel(
            allo: Allocator,
            comptime sentinel: T,
            slice: [:sentinel]T,
        ) Allocator.Error!@This() {
            const len = slice.len + 1;
            const indices = try allo.alignedAlloc(usize, null, len);
            fillIndices(indices, 0);
            const ids = try allo.alignedAlloc(usize, null, len);
            @memcpy(ids, indices);
            return @This(){
                .indices = indices,
                .ids = ids,
                .items = slice,
                .capacity = slice.len + 1,
            };
        }

        /// Caller owns memory - returns slice of items.
        /// Empties TriArrayList.
        /// Capacity is cleared.
        /// Deinit is unnecessary but safe.
        pub fn toOwnedSlice(self: *@This(), allo: Allocator) Allocator.Error!Slice {
            const old_memory = self.allocatedItemSlice();
            if (allo.remap(old_memory, self.items.len)) |new_items| {
                allo.free(self.indices);
                allo.free(self.ids);
                self.* = .empty;
                return new_items;
            }

            const new_memory = try allo.alignedAlloc(T, alignment, self.items.len);
            @memcpy(new_memory, self.items);
            self.clearAndFree(allo);
            return new_memory;
        }

        /// Caller owns memory.
        /// Empties this TriArrayList.
        /// Deinit unnecessary but safe.
        pub fn toOwnedSliceSentinel(
            self: *@This(),
            allo: Allocator,
            comptime sentinel: T,
        ) Allocator.Error!SentinelSlice(sentinel) {
            try self.ensureTotalCapacityPrecise(allo, self.items.len + 1);
            self.appendAssumeCapacity(sentinel);
            const result = try self.toOwnedSlice(allo);
            return result[0 .. result.len - 1 :sentinel];
        }

        /// Creates deep copy of TriArrayList
        pub fn clone(self: *const @This(), allo: Allocator) Allocator.Error!@This() {
            var cloned: @This() = try .initCapacity(allo, self.capacity);
            cloned.appendSliceAssumeCapacity(self.items);
            return cloned;
        }

        /// O(1) append
        /// Checks if memory can be allocated if necessary - returns error if needed but can't
        /// Extends self.items by 1.
        /// If needed, extends self.ids and self.indices by 1.
        /// Never invalidates element pointers
        pub fn append(self: *@This(), allo: Allocator, item: T) Allocator.Error!void {
            try self.ensureTotalCapacity(allo, self.items.len + 1);
            self.appendAssumeCapacity(item);
        }

        /// O(1)
        /// Extends items by 1 item.
        /// Never invalidates element pointers
        /// Asserts lists can hold an additional item
        /// for indies and ids:
        /// if same size add 1 to length + store value
        /// otherwise use default value in that position
        pub fn appendAssumeCapacity(self: *@This(), item: T) void {
            const len = self.items.len;
            self.items.len += 1;
            self.items[len] = item;
            if (len == self.indices.len) {
                self.indices.len += 1;
                self.indices[len] = len;
            }
            if (len == self.ids.len) {
                self.ids.len += 1;
                self.ids[len] = len;
            }
        }

        /// O(n)
        /// Extends self.items by n items
        /// Never invalidates element pointers
        /// Asserts self.items can hold addition n items.
        /// for indices and ids:
        /// If larger, preset values are used.
        /// If smaller, allocates memory -> extends list -> sets values
        pub fn appendSlice(
            self: *@This(),
            allo: Allocator,
            items: []const T,
        ) Allocator.Error!void {
            try self.ensureTotalCapacity(allo, self.items.len + items.len);
            self.appendSliceAssumeCapacity(items);
        }

        /// O(n)
        /// Extends self.items by n items
        /// Never invalidates element pointers
        /// Asserts self.items can hold additional n items.
        /// for indices and ids:
        /// if larger, use pre-defined values
        /// if smaller, extend list, set new values as their positions
        pub fn appendSliceAssumeCapacity(self: *@This(), items: []const T) void {
            const old_len = self.items.len;
            const new_len = old_len + items.len;
            self.items.len = new_len;
            @memcpy(self.items[old_len..][0..items.len], items);
            if (self.indices.len < new_len) {
                const old_len_indices = self.indices.len;
                self.indices.len = new_len;
                fillIndices(self.indices[old_len_indices..], old_len_indices);
            }
            if (self.ids.len < new_len) {
                const old_len_ids = self.ids.len;
                self.ids.len = new_len;
                fillIndices(self.ids[old_len_ids..], old_len_ids);
            }
        }

        /// Remove and return last item from items
        /// O(1)
        pub fn pop(self: *@This()) T {
            const val = self.getLastItem();
            self.items.len -= 1;
            return val;
        }

        /// Remove and return last item from items
        /// If items is empty, return `null`
        pub fn popOrNull(self: *@This()) ?T {
            if (self.items.len == 0) return null;
            return self.pop();
        }

        /// O(1)
        /// Asserts that items is not empty
        pub fn remove(self: *@This(), index: usize) T {
            assert(index < self.indices.len);
            const id = self.indices[index];
            assert(id < self.items.len);
            const last_id = self.items.len - 1;
            if (id == last_id) return self.pop();
            std.mem.swap(T, &self.items[id], &self.items[last_id]);
            std.mem.swap(usize, &self.ids[id], &self.ids[last_id]);
            std.mem.swap(usize, &self.indices[self.ids[id]], &self.indices[self.ids[last_id]]);
            return self.pop();
        }

        pub fn removeOrNull(self: *@This(), index: usize) ?T {
            if (index >= self.indices.len) return null;
            const id = self.indices[index];
            if (id >= self.items.len) return null;
            const last_id = self.items.len - 1;
            if (id == last_id) return self.pop();
            std.mem.swap(T, &self.items[id], &self.items[last_id]);
            std.mem.swap(usize, &self.ids[id], &self.ids[last_id]);
            std.mem.swap(usize, &self.indices[self.ids[id]], &self.indices[self.ids[last_id]]);
            return self.pop();
        }

        /// Sort data by indices
        pub fn sortByIndex(self: *@This()) void {
            const ctx: Context = .{
                .indices = self.indices,
                .ids = self.ids,
                .items = self.items,
            };
            std.sort.pdq(usize, self.indices, ctx, ctx.lessThanFn);
        }

        /// Sort data by ids
        pub fn sortById(self: *@This()) void {
            const ctx: Context = .{
                .indices = self.indices,
                .ids = self.ids,
                .items = self.items,
            };
            std.sort.pdq(usize, self.ids, ctx, ctx.lessThanFn);
        }

        pub fn findId(self: *const @This(), id: usize) usize {
            for (self.ids, 0..) |curr_id, i| if (curr_id == id) return i;
            unreachable;
        }

        pub fn findIdOrNull(self: *const @This(), id: usize) ?usize {
            for (self.ids, 0..) |curr_id, i| if (curr_id == id) return i;
            return null;
        }

        pub fn findIndex(self: *const @This(), index: usize) usize {
            for (self.indices, 0..) |curr_index, i| if (curr_index == index) return i;
            unreachable;
        }

        pub fn findIndexOrNull(self: *const @This(), index: usize) ?usize {
            for (self.indices, 0..) |curr_index, i| if (curr_index == index) return i;
            return null;
        }

        /// Returns last item from items
        /// Asserts that items is not empty
        pub fn getLastItem(self: @This()) T {
            return self.items[self.items.len - 1];
        }

        /// Returns last item or null if empty.
        pub fn getLastItemOrNull(self: @This()) ?T {
            return if (self.items.len > 0) self.items[self.items.len - 1] else null;
        }

        /// Returns id of the last item.
        /// Asserts that items is not empty
        pub fn getLastItemId(self: @This()) usize {
            return self.ids[self.items.len - 1];
        }

        /// Returns id of last item or null if empty.
        pub fn getLastItemIdOrNull(self: @This()) ?usize {
            return if (self.items.len > 0) self.ids[self.items.len - 1] else null;
        }

        /// Returns last id.
        /// Asserts that ids is not empty.
        pub fn getLastId(self: @This()) usize {
            return self.ids[self.ids.len - 1];
        }

        /// Returns last id or null.
        pub fn getLastIdOrNull(self: @This()) ?usize {
            return if (self.ids.len > 0) self.ids[self.ids.len - 1] else null;
        }

        /// Get last index for last item
        pub fn getLastItemIndex(self: @This()) usize {
            return self.indices[self.items.len - 1];
        }

        /// Gets last index or null for last item
        pub fn getLastItemIndexOrNull(self: @This()) ?usize {
            return if (self.indices.len > 0) self.indices[self.items.len - 1] else null;
        }

        /// Returns last index.
        /// Asserts that index is not empty.
        pub fn getLastIndex(self: @This()) usize {
            return self.indices[self.indices.len - 1];
        }

        /// Returns last index or null if empty.
        pub fn getLastIndexOrNull(self: @This()) ?usize {
            return if (self.indices.len > 0) self.indices[self.indices.len - 1] else null;
        }

        /// Returns index slice
        pub fn allocatedIndexSlice(self: @This()) []usize {
            return self.indices.ptr[0..self.capacity];
        }

        /// Returns id slice
        pub fn allocatedIdSlice(self: @This()) []usize {
            return self.ids.ptr[0..self.capacity];
        }

        /// Returns item slice
        pub fn allocatedItemSlice(self: @This()) Slice {
            return self.items.ptr[0..self.capacity];
        }

        /// Returns slice of unused indices up to capacity
        pub fn unusedIndexSlice(self: @This()) []usize {
            return self.allocatedIndexSlice()[self.indices.len..];
        }

        /// Returns slice of unused ids up to capacity
        pub fn unusedIdSlice(self: @This()) []usize {
            return self.allocatedIdSlice()[self.ids.len..];
        }

        /// Returns slice of unused items up to capacity
        pub fn unusedItemSlice(self: @This()) Slice {
            return self.allocatedItemSlice()[self.items.len..];
        }

        /// Asserts new length is less than current length
        /// Resets len for each array to new length
        pub fn shrinkRetainingCapacity(self: *@This(), new_len: usize) void {
            assert(new_len < self.items.len);

            self.indices.len = new_len;
            self.items.len = new_len;
            self.ids.len = new_len;
        }

        /// Reduce length to 0.
        pub fn clearRetainingCapacity(self: *@This()) void {
            self.indices.len = 0;
            self.ids.len = 0;
            self.items.len = 0;
        }

        /// Invalidate all element pointers
        pub fn clearAndFree(self: *@This(), allo: Allocator) void {
            allo.free(self.allocatedIndexSlice());
            allo.free(self.allocatedIdSlice());
            allo.free(self.allocatedItemSlice());
            self.* = .empty;
        }

        /// Modify array to hold at least `new capacity` items/ids/indices.
        /// Impl super-linear growth to achieve amortized O(1) append ops.
        /// Invalidate element ptrs if additional memory needed.
        pub fn ensureTotalCapacity(
            self: *@This(),
            allo: Allocator,
            new_capacity: usize,
        ) Allocator.Error!void {
            // avoid unnecessary re-allocs
            if (self.capacity >= new_capacity) return;
            return self.ensureTotalCapacityPrecise(allo, growCapacity(new_capacity));
        }

        /// If current capacity is less than `new capacity`, this fn will modify the array
        /// so that it can hold exactly `new capacity` items.
        /// Invalidates element pointers if additional memory is needed.
        pub fn ensureTotalCapacityPrecise(
            self: *@This(),
            allo: Allocator,
            new_capacity: usize,
        ) Allocator.Error!void {
            if (@sizeOf(T) == 0) {
                self.capacity = std.math.maxInt(usize);
                return;
            }
            // avoid unnecessary re-allocations
            if (self.capacity >= new_capacity) return;
            // Avoid copying allocated but unused bytes by attempting resizing in place.
            // Fall back to allocating a new buffer and doing a copy.
            // With a realloc() call, the allocated impl would pointlessly copy our extra capacity.
            var min_capacity: usize = std.math.maxInt(usize);
            inline for (.{ &self.indices, &self.ids, &self.items }) |data| {
                const len = data.*.len;
                const old_memory = data.*[0..len];
                if (allo.remap(old_memory, new_capacity)) |new_memory| {
                    data.*.ptr = new_memory.ptr;
                    if (new_memory.len < min_capacity) min_capacity = new_memory.len;
                } else {
                    const t = @TypeOf(old_memory[0]);
                    const new_memory = try allo.alignedAlloc(t, null, new_capacity);
                    @memcpy(new_memory[0..len], old_memory);
                    allo.free(old_memory);
                    data.ptr = new_memory.ptr;
                    if (new_memory.len < min_capacity) min_capacity = new_memory.len;
                }
            }
            self.capacity = min_capacity;
        }

        /// Modify array to hold at least `additional count` items.
        /// Invalidates element pointers if additional memory is needed.
        pub fn ensureUnusedCapacity(
            self: *@This(),
            allo: Allocator,
            additional_count: usize,
        ) Allocator.Error!void {
            return self.ensureTotalCapacity(
                allo,
                try addOrOom(self.items.len, additional_count),
            );
        }

        /// Increase array length to match full capacity already allocated.
        /// New elements have `undefined` values.
        /// Never invalidates element pointers.
        pub fn expandToCapacity(self: *@This()) void {
            self.indices.len = self.capacity;
            self.ids.len = self.capacity;
            self.items.len = self.capacity;
        }

        /// Called when growing memory.
        /// Returns a capacity larger than minimum = super-linear growth.
        /// Superlinear growth amortizes memory calls to O(1).
        pub fn growCapacity(min: usize) usize {
            return min +| ((min / 2) + init_capacity);
        }

        /// Context - used to sort data
        const Context = struct {
            indices: []usize,
            ids: []usize,
            items: []T,

            pub fn lessThanFn(_: void, a: usize, b: usize) bool {
                return a < b;
            }

            pub fn swap(ctx: @This(), a: usize, b: usize) void {
                std.mem.swap(T, &ctx.items[a], &ctx.items[b]);
                std.mem.swap(usize, &ctx.ids[a], &ctx.ids[b]);
                std.mem.swap(usize, &ctx.indices[a], &ctx.indices[b]);
            }
        };
    };
}

/// Checks if adding new memory causes overflow = out of bounds
fn addOrOom(a: usize, b: usize) error{OutOfMemory}!usize {
    const result, const overflow = @addWithOverflow(a, b);
    if (overflow != 0) return error.OutOfMemory;
    return result;
}

/// Creates default type for ease of use.
pub fn TriArrayList(comptime T: type) type {
    return Aligned(T, null);
}

/// Adds new indices to list of usize.
fn fillIndices(list: []usize, start_index: usize) void {
    var i: usize = start_index;
    const stairs = std.simd.iota(usize, 64);
    const len = list.len;
    while (i + 64 < len) : (i += 64)
        list[i..][0..64].* = @as(@Vector(64, usize), @splat(i)) + stairs;
    for (i..len) |j| list[j] = j;
}

test "Empty" {
    const list: TriArrayList(i32) = .empty;
    try testing.expect(list.items.len == 0);
    try testing.expect(list.ids.len == 0);
    try testing.expect(list.indices.len == 0);
    try testing.expect(list.capacity == 0);
}

test "initCapacity deinit" {
    const allo = testing.allocator;
    var list: TriArrayList(i32) = try .initCapacity(allo, 200);
    defer list.deinit(allo);

    try testing.expect(list.items.len == 0);
    try testing.expect(list.ids.len == 0);
    try testing.expect(list.indices.len == 0);
    try testing.expect(list.capacity >= 200);
}

test "Init Slice" {
    var index_buffer: [1024]usize = undefined;
    var id_buffer: [1024]usize = undefined;
    var item_buffer: [1024]u8 = undefined;

    const index_slice: []usize = &index_buffer;
    const id_slice: []usize = &id_buffer;
    const item_slice: []u8 = &item_buffer;

    const list: TriArrayList(u8) = .initSlice(index_slice, id_slice, item_slice);

    try testing.expect(list.items.len == 0);
    try testing.expect(list.ids.len == 0);
    try testing.expect(list.indices.len == 0);
    try testing.expect(list.capacity >= 1024);
}

test "Init Sentinel Slice" {
    var index_buffer: [1024]usize = undefined;
    var id_buffer: [1024]usize = undefined;
    var item_buffer: [1024]u8 = undefined;

    const index_sentinel_slice: [:0]usize = @ptrCast(&index_buffer);
    const id_sentinel_slice: [:0]usize = @ptrCast(&id_buffer);
    const item_sentinel_slice: [:0]u8 = @ptrCast(&item_buffer);

    const list: TriArrayList(u8) = .initSentinelSlice(
        0,
        index_sentinel_slice,
        id_sentinel_slice,
        item_sentinel_slice,
    );

    try testing.expect(list.items.len == 0);
    try testing.expect(list.ids.len == 0);
    try testing.expect(list.indices.len == 0);
    try testing.expect(list.capacity >= 1024);
}

test "toOwnedSlice fromOwnedSlice" {
    const allo = testing.allocator;
    var list1: TriArrayList(u8) = try .initCapacity(allo, 16);
    defer list1.deinit(allo);
    try list1.appendSlice(allo, "foobar");

    const slice = try list1.toOwnedSlice(allo);
    var list2: TriArrayList(u8) = try .fromOwnedSlice(allo, slice);
    defer list2.deinit(allo);

    try testing.expectEqualStrings(list2.items, "foobar");
}

test "toOwnedSentinelSlice fromOwnedSentinelSlice" {
    const allo = testing.allocator;

    var list1: TriArrayList(u8) = try .initCapacity(allo, 16);
    defer list1.deinit(allo);
    try list1.appendSlice(allo, "foobar");

    const sentinel_slice = try list1.toOwnedSliceSentinel(allo, 0);
    var list2: TriArrayList(u8) = try .fromOwnedSliceSentinel(allo, 0, sentinel_slice);
    defer list2.deinit(allo);

    try testing.expectEqualStrings(list2.items, "foobar");
}
