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
            index_buffer: SentinelSlice,
            id_buffer: SentinelSlice,
            item_buffer: SentinelSlice,
        ) @This() {
            return @This(){
                .indices = index_buffer[0..0],
                .ids = id_buffer[0..0],
                .items = item_buffer[0..0],
                .capacity = 0,
            };
        }

        /// Release allocated memory
        pub fn deinit(self: *@This(), allo: Allocator) void {
            allo.free(self.allocatedIndexSlice());
            allo.free(self.allocatedIdSlice());
            allo.free(self.allocatedItemSlice());
            self.* = undefined;
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
            if (@import("builtin").mode == .Debug) assert(self.items.len > 0);
            return self.items[self.items.len - 1];
        }

        /// Returns last item from items or null if empty.
        pub fn getLastItemOrNull(self: @This()) ?T {
            return if (self.items.len > 0) self.items[self.items.len - 1] else null;
        }

        pub fn getLastId(self: @This()) usize {
            return self.ids[self.items.len - 1];
        }

        pub fn getLastIdOrNull(self: @This()) usize {
            return if (self.items.len > 0) self.ids[self.items.len - 1] else null;
        }

        /// Get last index for last item
        pub fn getLastIndex(self: @This()) usize {
            return self.indices[self.items.len - 1];
        }

        /// Gets last index or null for last item
        pub fn getLastIndexOrNull(self: @This()) usize {
            return if (self.indices.len > 0) self.indices[self.items.len - 1] else null;
        }

        pub fn allocatedIndexSlice(self: @This()) Slice {
            return self.indices.ptr[0..self.capacity];
        }

        pub fn allocatedIdSlice(self: @This()) Slice {
            return self.ids.ptr[0..self.capacity];
        }

        pub fn allocatedItemSlice(self: @This()) Slice {
            return self.items.ptr[0..self.capacity];
        }

        pub fn unusedCapacityIndexSlice(self: @This()) []T {
            return self.allocatedIndexSlice()[self.indices.len..];
        }

        pub fn unusedCapacityIdSlice(self: @This()) []T {
            return self.allocatedIdSlice()[self.ids.len..];
        }

        pub fn unusedCapacityItemsSlice(self: @This()) []T {
            return self.allocatedItemSlice()[self.items.len..];
        }

        pub fn toOwnedSlice() []T {}

        pub fn fromOwnedSlice() @This() {}

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
