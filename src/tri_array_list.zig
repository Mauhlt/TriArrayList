const std = @import("std");
const testing = std.testing;
const Alignment = std.mem.Alignment;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

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
        pub fn initCapacity(allo: Allocator, num: usize) @This() {
            var self: @This() = .empty;
            try self.ensureTotalCapacityPrecise(allo, num);
            return self;
        }

        /// Init with externally managed memory
        /// Buffer determines capacity + length set to 0
        /// All fns that accept allocator will cause illegal behavior
        pub fn initSlice(
            index_buffer: Slice,
            id_buffer: Slice,
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
        pub fn initSentinelSlice(
            comptime sentinel: T,
            index_buffer: [:sentinel]T,
            id_buffer: [:sentinel]T,
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
        /// Deinit with `deinit` and `toOwnedSlice`.
        pub fn fromOwnedSlice(allo: Allocator, slice: Slice) @This() {
            const len = slice.len;
            var indices = try allo.alignedAlloc(T, alignment, len);
            // use vectors to quickly create values
            const VEC_LEN: comptime_int = 64;
            const stair = std.simd.iota(usize, VEC_LEN);
            var i: usize = 0;
            while (i + 64 < len) : (i += 64)
                indices[i..][0..64].* = @as(@Vector(VEC_LEN, usize), @splat(i)) + stair;
            for (i..len) |j| indices[i] = j;
            // copy indices to ids
            const ids = try allo.dupe(usize, indices);
            // return constructor
            return @This(){
                .indices = indices,
                .ids = ids,
                .items = slice,
                .capacity = slice.len,
            };
        }

        pub fn fromOwnedSentinelSlice(
            allo: Allocator,
            comptime sentinel: T,
            slice: [:sentinel]T,
        ) @This() {
            const len = slice.len + 1;
            var indices = try allo.alignedAlloc(usize, @alignOf(usize), len);
            // uses vectors to quickly create values
            const VEC_LEN: comptime_int = 64;
            const stair = std.simd.iota(usize, VEC_LEN);
            var i: usize = 0;
            while (i + 64 < len) : (i += 64)
                indices[i..][0..64].* = @as(@Vector(VEC_LEN, usize), @splat(i)) + stair;
            for (i..len) |j| indices[i] = j;
            // copy indices to ids
            const ids = try allo.dupe(usize, indices);
            // return
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
        pub fn toOwnedSlice(self: *@This(), allo: Allocator) []T {
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
            comptime sentinel: T,
            allo: Allocator,
        ) Allocator.Error!SentinelSlice(sentinel) {
            try self.ensureTotalCapacityPrecise(self.items.len + 1);
            self.appendAssumeCapacity(sentinel);
            const result = try self.toOwnedSlice(allo);
            return result[0 .. result.len - 1 :sentinel];
        }

        /// Creates copy of TriArrayList
        pub fn clone(self: *const @This(), allo: Allocator) Allocator.Error!@This() {
            var cloned: @This() = try .initCapacity(allo, self.capacity);
            cloned.appendSliceAssumeCapacity(self.items);
            return cloned;
        }

        /// TODO:
        /// Append `item` to items
        /// Append `index` to indices
        /// Swap `index` to its index
        /// Append `id` to its id
        /// Brings operation down to O(1)
        pub fn insert(
            self: *@This(),
            allo: Allocator,
            index: usize,
            item: T,
        ) Allocator.Error!void {
            try self.append(allo, item);
        }

        /// TODO:
        /// 1. appends data
        /// 2. appends index
        /// 3. swaps index to correct position
        pub fn insertAssumeCapacity(self: *@This(), index: usize, item: T) void {
            assert(self.items.len < self.capacity);
            self.items.len += 1;
            @memmove(self.items[i + 1 .. self.items.len], self.items[i .. self.items.len - 1]);
            self.items[i] = item;
        }

        /// Append `item` to items
        /// Append `index` to indices
        /// Swap `index` to its position
        /// O(1)
        /// if list lacks unused capacity for additional items, return error.OutOfMemory
        /// Asserts that index is in bounds or equal to length
        pub fn insertBounded(self: *@This(), index: usize, item: T) error{OutOfMemory}!void {
            if (self.capacity - self.items.len == 0) return error.OutOfMemory;
            return insertAssumeCapacity(self, index, item);
        }

        /// Remove and return last item from items
        /// If items is empty, return `null`
        /// Invalidate pointers to last item
        pub fn pop(self: *@This()) ?T {
            if (self.items.len == 0) return null;
            const val = self.items[self.items.len - 1];
            self.items[self.items.len - 1] = undefined;
            self.items.len -= 1;
            return val;
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
        pub fn getLastItemIdOrNull(self: @This()) usize {
            return if (self.items.len > 0) self.ids[self.items.len - 1] else null;
        }

        /// Returns last id.
        /// Asserts that ids is not empty.
        pub fn getLastId(self: @This()) usize {
            return self.ids[self.ids.len - 1];
        }

        /// Returns last id or null.
        pub fn getLastIdOrNull(self: @This()) usize {
            return if (self.ids.len > 0) self.ids[self.ids.len - 1] else null;
        }

        /// Get last index for last item
        pub fn getLastItemIndex(self: @This()) usize {
            return self.indices[self.items.len - 1];
        }

        /// Gets last index or null for last item
        pub fn getLastItemIndexOrNull(self: @This()) usize {
            return if (self.indices.len > 0) self.indices[self.items.len - 1] else null;
        }

        /// Returns last index.
        /// Asserts that index is not empty.
        pub fn getLastIndex(self: @This()) usize {
            return self.indices[self.indices.len - 1];
        }

        /// Returns last index or null if empty.
        pub fn getLastIndexOrNull(self: @This()) usize {
            return if (self.indices.len > 0) self.indices[self.indices.len - 1] else null;
        }

        /// Returns index slice
        pub fn allocatedIndexSlice(self: @This()) Slice {
            return self.indices.ptr[0..self.capacity];
        }

        /// Returns id slice
        pub fn allocatedIdSlice(self: @This()) Slice {
            return self.ids.ptr[0..self.capacity];
        }

        /// Returns item slice
        pub fn allocatedItemSlice(self: @This()) Slice {
            return self.items.ptr[0..self.capacity];
        }

        /// Returns slice of unused indices up to capacity
        pub fn unusedIndexSlice(self: @This()) []T {
            return self.allocatedIndexSlice()[self.indices.len..];
        }

        /// Returns slice of unused ids up to capacity
        pub fn unusedIdSlice(self: @This()) []T {
            return self.allocatedIdSlice()[self.ids.len..];
        }

        /// Returns slice of unused items up to capacity
        pub fn unusedItemsSlice(self: @This()) []T {
            return self.allocatedItemSlice()[self.items.len..];
        }

        pub fn append(self: *@This(), allo: Allocator, item: T) Allocator.Error!void {
            const new_item_ptr = try self.addOne(allo);
            new_item_ptr.* = item;
        }

        /// Extends list by 1 element.
        /// Never invalidates element pointers.
        /// Assrts that list can hold an additional item.
        pub fn appendAssumeCapacity(self: *@This(), item: T) void {
            self.addOneAssumeCapacity().* = item;
        }

        /// Append slice of items to items.
        /// If necessary, allocates items.
        /// Invalidates element pointers if additional memory is needed.
        pub fn appendSlice(self: *@This(), items: []const T) void {
            try self.ensureUnusedCapacity(items.len);
            self.appendSliceAssumeCapacity(items);
        }

        pub fn appendSliceAssumeCapacity(self: *@This(), items: []const T) void {
            const old_len = self.items.len;
            const new_len = old_len + items.len;
            assert(new_len <= self.capacity);
            @memcpy(self.items[old_len..][0..items.len], items);
            for (old_len..new_len) |i| self.indices[i] = i;
            @memcpy(self.ids[old_len..new_len], self.indices[old_len..new_len]);
        }

        pub fn shrinkRetainingCapacity(self: *@This(), new_len: usize) void {
            assert(new_len < self.items.len);

            @memset(self.items[new_len], undefined);
            @memset(self.ids[new_len..], undefined);
            @memset(self.items[new_len..], undefined);

            self.indices.len = new_len;
            self.items.len = new_len;
            self.ids.len = new_len;
        }

        /// Reduce length to 0.
        /// Invalidate all element pointers
        pub fn clearRetainingCapacity(self: *@This()) void {
            @memset(self.indices, undefined);
            @memset(self.ids, undefined);
            @memset(self.items, undefined);

            self.indices.len = 0;
            self.ids.len = 0;
            self.items.len = 0;
        }

        /// Invalidate all element pointers
        pub fn clearAndFree(self: *@This(), allo: Allocator) void {
            allo.free(self.allocatedItemSlice());
            allo.free(self.allocatedItemSlice());
            allo.free(self.allocatedIdSlice());

            self.indices.len = 0;
            self.ids.len = 0;
            self.items.len = 0;

            self.capacity = 0;
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
            inline for (&.{ self.indices, self.ids, self.items }) |data| {
                const len = data.*.len;
                const old_memory = data.*[0..len];
                if (allo.remap(old_memory, new_capacity)) |new_memory| {
                    data.*.ptr = new_memory.ptr;
                    if (new_memory.len < min_capacity) min_capacity = new_memory.len;
                } else {
                    const new_memory = try allo.alignedAlloc(@TypeOf(old_memory[0]), @alignOf(@TypeOf(old_memory[0])), new_capacity);
                    @memcpy(new_memory[0..len], old_memory);
                    self.allo.free(old_memory);
                    data.*.ptr = new_memory.ptr;
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
            additonal_count: usize,
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

        /// Increase length by 1. Returns ptr to new item.
        /// Returned element ptr is invalidated when list is resized.
        pub fn addOne(self: *@This(), allo: Allocator) Allocator.Error!*T {
            // never overflows b/c `self.items` can never occupy outside range.
            const new_len = self.items.len + 1;
            try self.ensureTotalCapacity(allo, new_len);
            return self.addOneAssumeCapacity();
        }

        /// Increase len by 1. Return ptr to new item.
        /// Never invalidate element ptrs.
        /// Returned element ptr is invalidated when list is resized.
        /// Asserts that list can hold 1 more item.
        pub fn addOneAssumeCapacity(self: *@This()) *T {
            assert(self.items.len < self.capacity);
            assert(self.ids.len < self.capacity);
            assert(self.indices.len < self.capacity);

            self.items.len += 1;
            self.ids.len = self.items.len;
            self.indices.len = self.indices.len;

            return &self.items[self.items.len - 1];
        }

        /// Increase length by 1. Return pointer to new item
        /// Never invalidate element pointers.
        /// Return element pointer becomes invalid when list is resized.
        /// If list lacks unused capacity, returns error.OutOfMemory.
        pub fn addOneBounded(self: *@This()) error{OutOfMemory}!*T {
            if (self.capacity - self.items.len < 1) return error.OutOfMemory;
            return addOneAssumeCapacity(self);
        }

        /// Resize array. Add `n` elements with `undefined` values.
        /// Return value is an array pointing to newly allocated elements.
        /// Returned pointer invalidated when list is resized.
        pub fn addManyAsArray(self: *@This(), allo: Allocator, comptime n: usize) Allocator.Error!*[n]T {
            const prev_len = self.items.len;
            try self.resize(allo, try addOrOom(self.items.len, n));
            return self.items[prev_len..][0..n];
        }

        /// Resize array. Add `n` elements with `undefined` values.
        /// Return value = array pointing to newly allocated elements.
        /// Never invalidates element pointers.
        /// Returned pointer becomes invalid when list is resized.
        /// Asserts that list can hold additional items
        pub fn addManyAsArrayAssumeCapacity(self: *@This(), comptime n: usize) *[n]T {
            assert(self.items.len + n <= self.capacity);
            const prev_len = self.items.len;
            self.items.len += n;
            return self.items[prev_len..][0..n];
        }

        /// Resize array. Add `n` elements with `undefined` values.
        /// Return value is an array pointing to newly allocated elements.
        /// Never invalidates pointers.
        /// Returned element pointers become invalidated when list is resized.
        /// If list lacks unused capacity, returns error.OutOfMemory.
        pub fn addManyAsArrayBounded(
            self: *@This(),
            comptime n: usize,
        ) error{OutOfMemory}!*[n]T {
            if (self.capacity - self.items.len < n) return error.OutOfMemory;
            return addManyAsArrayAssumeCapacity(self, n);
        }

        /// Resize array. Add `n` elements with `undefined` values.
        /// Return value is a slice pointing to newly allocated elements.
        /// Returned pointer becomes invalid when list is resized.
        /// Resizes list if `self.capacity` is not large enough.
        pub fn addManyAsSlice(
            self: *@This(),
            allo: Allocator,
            n: usize,
        ) Allocator.Error![]T {
            const prev_len = self.items.len;
            try self.resize(allo, try addOrOom(self.items.len, n));
            return self.items[prev_len..][0..n];
        }

        /// Resize array. Add `n` elements with `undefined` values.
        /// Return a slice pointing to newly allocated elements.
        /// Never invalidates element pointers.
        /// Returned pointer is invalidated when list is resized.
        /// Asserts list can hold additional items.
        pub fn addManyAsSliceAssumeCapacity(self: *@This(), n: usize) []T {
            assert(self.items.len + n <= self.capacity);
            const prev_len = self.items.len;
            self.items.len += n;
            return self.items[prev_len..][0..n];
        }

        /// Resize array. Add `n` new elements with `undefined` values.
        /// Return slice pointing to newly allocated elements.
        /// Never invalidates element pointers. Returned pointer is invalidated
        /// when list is resized.
        /// IF list lacks unused capacity, returns error.OutOfMemmory.
        pub fn addManyAsSliceBounded(self: *@This(), n: usize) error{OutOfMemory}![]T {
            if (self.capacity - self.items.len < n) return error.OutOfMemory;
            return addManyAsSliceAssumeCapacity(self, n);
        }

        /// Called when growing memory.
        /// Returns a capacity larger than minimum = super-linear growth.
        /// Superlinear growth amortizes memory calls to O(1).
        pub fn growCapacity(min: usize) usize {
            return min +| ((min / 2) + init_capacity);
        }
    };
}

fn addOrOom(a: usize, b: usize) error{OutOfMemory}!usize {
    const result, const overflow = @addWithOverflow(a, b);
    if (overflow != 0) return error.OutOfMemory;
    return result;
}

pub fn TriArrayList(comptime T: type) type {
    return Aligned(T, null);
}

test "Init" {
    const list: TriArrayList(i32) = .empty();
    try testing.expect(list.items.len == 0);
    try testing.expect(list.ids.len == 0);
    try testing.expect(list.indices.len == 0);
    try testing.expect(list.capacity == 0);
}

test "initCapacity" {
    const allo = testing.allocator;
    var list: TriArrayList(i32) = try .initCapacity(allo, 200);
    defer list.deinit(allo);
    try testing.expect(list.items.len == 0);
    try testing.expect(list.ids.len == 0);
    try testing.expect(list.indices.len == 0);
    try testing.expect(list.capacity >= 200);
}

test "clone" {
    const allo = testing.allocator;
    var array: TriArrayList(i32) = .empty();
    try array.append(allo, -1);
    try array.append(allo, 3);
    try array.append(allo, 5);

    var cloned = try array.clone(allo);
    defer cloned.deinit(allo);

    try testing.expectEqualSlices();
}
