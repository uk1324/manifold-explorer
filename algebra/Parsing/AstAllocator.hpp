#pragma once

#include <Types.hpp>
#include <vector>
#include <span>

// This is allocator is just a bump allocator that expands it's memory if needed. It deallocates everything at once. This is done because `ast`s aren't modified after the are allocated.
// One distadvantange of using this kind of allocator that is that the only way to expand a list is to reallocate it fully leaving the previous allocation unused.
struct AstAllocator {
	template<typename T>
	struct List {
		List();

		std::span<const T> span() const;

		T* data_;
		usize size_;
		usize capacity_;
	};

	template<typename T>
	void listAppend(List<T>& list, const T& value);

	AstAllocator();

	void reset();

	static constexpr i64 BLOCK_DATA_SIZE = 4096;
	struct Block {
		u8* nextAvailable;
		Block* nextBlock;
		u8 data[BLOCK_DATA_SIZE];
	};
	Block* first = nullptr;
	Block* current = nullptr;

	void* allocate(i64 size, i64 alignment);
	template<typename T, typename ...Args>
	T* allocate(Args&&... args);
};

template<typename T>
void AstAllocator::listAppend(List<T>& list, const T& value) {
	if (list.size_ + 1 > list.capacity_) {
		usize newCapacity;
		if (list.capacity_ == 0) {
			newCapacity = 4;
		} else {
			newCapacity = list.capacity_ * 2;
		}
		const auto newData = reinterpret_cast<T*>(allocate(sizeof(T) * newCapacity, alignof(T)));
		for (usize i = 0; i < list.size_; i++) {
			new (&newData[i]) T(std::move(list.data_[i]));
		}
		list.capacity_ = newCapacity;
		list.data_ = newData;
	}

	new (&list.data_[list.size_]) T(value);
	list.size_++;
}

template<typename T, typename ...Args>
T* AstAllocator::allocate(Args&&... args) {
	void* memory = allocate(sizeof(T), alignof(T));
	new (memory) T(args...);
	return reinterpret_cast<T*>(memory);
}

template<typename T>
inline AstAllocator::List<T>::List()
	: data_(nullptr)
	, size_(0)
	, capacity_(0) {}

template<typename T>
std::span<const T> AstAllocator::List<T>::span() const {
	return std::span<const T>(data_, size_);
}