#include "AstAllocator.hpp"
#include <Assertions.hpp>
#include <iostream>
#include <memory>
#include <cstdlib>

//#define DEBUG_PRINT_BLOCK_ALLOCATIONS

#include <stdint.h>

template <typename A, typename B>
A roundUpToMultiple(A value, B alignment);

template<typename T>
T* roundAddressUpToMultiple(T* address, intptr_t alignment);

template<typename A, typename B>
A roundUpToMultiple(A value, B alignment) {
	const auto misalignment = value % alignment;
	return misalignment == 0
		? value
		: value + (alignment - misalignment);
}

template<typename T>
T* roundAddressUpToMultiple(T* address, intptr_t alignment) {
	return reinterpret_cast<T*>(roundUpToMultiple(intptr_t(address), alignment));
}

AstAllocator::AstAllocator()
	: current(nullptr)
	, first(nullptr) {

}

void AstAllocator::reset() {
	current = first;
	Block* block = first;
	while (block != nullptr) {
		block->nextAvailable = block->data;
		block = block->nextBlock;
	}
}

void* AstAllocator::allocate(i64 size, i64 alignment) {
	if (size > BLOCK_DATA_SIZE) {
		// TODO: 
		ASSERT_NOT_REACHED();
		return malloc(size);
	}

	if (first == nullptr) {
		first = reinterpret_cast<Block*>(malloc(sizeof(Block)));
#ifdef DEBUG_PRINT_BLOCK_ALLOCATIONS
		std::cout << "AST_ALLOCATOR: allocated first block";
#endif 
		if (first == nullptr) {
			ASSERT_NOT_REACHED();
			return nullptr;
		}
		first->nextAvailable = first->data;
		first->nextBlock = nullptr;
		current = first;
	}

	u8* nextAvailableAligned = roundAddressUpToMultiple(current->nextAvailable, alignment);
	if (nextAvailableAligned + size > current->data + BLOCK_DATA_SIZE) {
		// Adding alignment just to be safe nothing gets overriten after the rounding up.
#ifdef DEBUG_PRINT_BLOCK_ALLOCATIONS
		std::cout << "AST_ALLOCATOR: allocated new block";
#endif 
		Block* newBlock = reinterpret_cast<Block*>(malloc(sizeof(Block) + alignment));
		if (newBlock == nullptr) {
			ASSERT_NOT_REACHED();
			return nullptr;
		}
		newBlock->nextAvailable = newBlock->data;
		newBlock->nextBlock = nullptr;
		current->nextBlock = newBlock;
		current = newBlock;
		nextAvailableAligned = roundAddressUpToMultiple(current->nextAvailable, alignment);
	}
	current->nextAvailable = nextAvailableAligned + size;

	ASSERT(current->nextAvailable <= current->data + BLOCK_DATA_SIZE);

	return nextAvailableAligned;
}