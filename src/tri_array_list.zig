const std = @import("std");
const Alignment = std.mem.Alignment;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

pub fn Aligned(comptime T: type, comptime alignment: ?Alignment) type {
    if (alignment) |a|
        if (a.toByteUnits() == @alignOf(T))
            return Aligned(T, null);

    return struct {
        indices: []usize = &[_]usize{},
        ids: []usize = &[_]usize{},
        items: []T = &[_]T{},
        capacity: usize = 0,

        pub const empty = @This(){};

        pub const Slice = if (alignment) |a| ([]align(a.toByteUnits()) T) else []T;

        const init_capacity: comptime_int = @max(1, std.atomic.cache_line / @sizeOf(T));

        pub fn SentinelSlice(comptime s: T) type {
            return if (alignment) |a| ([:s]align(a.toByteUnits()) T) else [:s]T;
        }

        /// Init with capacity to hold `num` items.
        /// The capacity will equal `num`.
        /// Deinit with `deinit` or `toOwnedSlice`.
        pub fn initCapacity(allo: Allocator, num: usize) @This() {
            var self: @This() = empty;
            try self.ensureTotalCapacityPrecise(allo, num);
            return self;
        }

        /// Init with externally managed memory
        /// Buffer determines capacity + length set to 0
        /// All fns that accept allocator will cause illegal behavior
        pub fn initBuffer(
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
        pub fn initSentinelBuffer(
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
            var ids = try allo.alignedAlloc(T, alignment, len);
            // use vectors to quickly create values
            const VEC_LEN: comptime_int = 64;
            const stair = std.simd.iota(usize, VEC_LEN);
            var i: usize = 0;
            while (i + 64 < len) : (i += 64) {
                indices[i..][0..64].* = @as(@Vector(VEC_LEN, usize), @splat(i)) + stair;
            }
            if (i < len)
                for (i..len) |j|
                    indices[i] = j;
            // copy indices to ids asymmetrically
            @memcpy(ids, indices);
            return @This(){
                .indices = indices,
                .ids = ids,
                .items = items,
                .capacity = capacity,
            };
        }

        pub fn fromOwnedSentinelSlice(
            comptime sentinel: T,
            allo: Allocator,
            slice: [:sentinel]T,
        ) @This() {
            const len = slice.len + 1;
            var indices = try allo.alignedAlloc(usize, @alignOf(usize), len);
            var ids = try allo.alignedAlloc(usize, @alignOf(usize), len);
            // uses vectors to quickly create values
            const VEC_LEN: comptime_int = 64;
            const stair = std.simd.iota(usize, VEC_LEN);
            var i: usize = 0;
            while (i + 64 < len) : (i += 64) {
                indices[i..][0..64].* = @as(@Vector(VEC_LEN, usize), @splat(i)) + stair;
            }
            if (i < len) for (i..len) |j| indices[i] = j;
            @memcpy(ids, indices);
            return @This(){
                .indices = indices,
                .ids = ids,
                .items = slice,
                .capacity = slice.len + 1,
            };
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

        pub fn toOwnedSlice() []T {}

        pub fn append(self: *@This(), allo: Allocator) !void {
            if (self.items.len < self.capacity) {}
        }

        pub fn appendAssumeCapacity(self: *@This()) void {}

        pub fn growCapacity(min: usize) usize {
            return min +| (min / 2 + init_capacity);
        }
    };
}

pub const TriArrayList = Aligned(usize);
