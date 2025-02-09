#pragma once

#pragma once

#include <iterator>
#include <vector>
#include <Types.hpp>
#include <Assertions.hpp>

template<typename T>
class View {
public:
	static View empty();
	View(T* data, size_t size);
	/*Span(std::vector<T>& v);
	explicit Span(List<T>& l);
	Span(const std::vector<std::remove_const_t<T>>& v);
	template<usize SIZE>
	Span(const T(&array)[SIZE]);*/

	T& operator[](i64 index);
	const T& operator[](i64 index) const;

	const T& fromBack(i64 index) const;

	//T* data() -> T*;
	//auto data() const -> const T*;
	i64 size() const;
	T* data();
	const T* data() const;
	//auto back() const -> const T&;

	T* begin();
	T* end();
	const T* begin() const;
	const T* end() const;
	const T* cbegin() const;
	const T* cend() const;

	//auto rbegin() -> std::reverse_iterator<T*>;
	//auto rend() -> std::reverse_iterator<T*>;
	//auto crbegin() const->std::reverse_iterator<const T*>;
	//auto crend() const->std::reverse_iterator<const T*>;

private:
	T* data_;
	i64 size_;
};

template<typename T, usize SIZE>
View<const T> constView(const T(&array)[SIZE]);

template<typename T, usize SIZE>
View<const T> constView(const std::array<T, SIZE>& array);

template<typename T> 
View<const T> constView(const T& singleValue);

template<typename T>
View<const T> constView(const std::vector<T>& v);

//template<typename T>
//Span<T>::Span(T* data, size_t size)
//	: data_(data)
//	, size_(size) {}
//
//template<typename T>
//Span<T>::Span(std::vector<T>& v)
//	: data_{ v.data() }
//	, size_{ v.size() } {}
//
//template<typename T>
//Span<T>::Span(List<T>& l)
//	: data_(l.data())
//	, size_(l.size()) {}
//
//template<typename T>
//Span<T>::Span(const std::vector<std::remove_const_t<T>>& v)
//	: data_{ v.data() }
//	, size_{ v.size() } {}
//
//template<typename T>
//template<usize SIZE>
//Span<T>::Span(const T(&array)[SIZE])
//	: data_{ array }
//	, size_{ SIZE } {}
//
//template<typename T>
//auto Span<T>::operator[](usize index) -> T& {
//	ASSERT(index < size_);
//	return data_[index];
//}
//
//template<typename T>
//auto Span<T>::operator[](usize index) const -> const T& {
//	return const_cast<T&>((const_cast<Span<T>&>(*this))[index]);
//}
//
//template<typename T>
//auto Span<T>::data() -> T* {
//	return data_;
//}
//
//template<typename T>
//auto Span<T>::data() const -> const T* {
//	return data_;
//}
//
//template<typename T>
//auto Span<T>::size() const -> size_t {
//	return size_;
//}
//
//template<typename T>
//auto Span<T>::back() const -> const T& {
//	return data_[size_ - 1];
//}
//

template<typename T>
T* View<T>::begin() {
	return data_;
}

template<typename T>
T* View<T>::end() {
	return data_ + size_;
}

template<typename T>
const T* View<T>::begin() const {
	return data_;
}

template<typename T>
const T* View<T>::end() const {
	return data_ + size_;
}

template<typename T>
const T* View<T>::cbegin() const {
	return data_;
}

template<typename T>
const T* View<T>::cend() const {
	return data_ + size_;
}
//
//template<typename T>
//auto Span<T>::rbegin() -> std::reverse_iterator<T*> {
//	return std::reverse_iterator{ end() };
//}
//
//template<typename T>
//auto Span<T>::rend() -> std::reverse_iterator<T*> {
//	return std::reverse_iterator{ begin() };
//}
//
//template<typename T>
//auto Span<T>::crbegin() const -> std::reverse_iterator<const T*> {
//	return std::reverse_iterator{ cend() };
//}
//
//template<typename T>
//auto Span<T>::crend() const -> std::reverse_iterator<const T*> {
//	return std::reverse_iterator{ cbegin() };
//}

template<typename T>
View<T> View<T>::empty() {
	return View(nullptr, 0);
}

template<typename T>
View<T>::View(T* data, size_t size) 
	: data_(data)
	, size_(size) {}

template<typename T>
T& View<T>::operator[](i64 index) {
	DEBUG_ASSERT(index < size_);
	return data_[index];
}

template<typename T>
const T& View<T>::operator[](i64 index) const {
	return const_cast<View<T>&>(*this)[index];
}

template<typename T>
const T& View<T>::fromBack(i64 index) const {
	return operator[](size_ + index);
}

template<typename T>
i64 View<T>::size() const {
	return size_;
}

template<typename T>
T* View<T>::data() {
	return data_;
}

template<typename T>
const T* View<T>::data() const {
	return data_;
}

template<typename T, usize SIZE>
View<T> view(T(&array)[SIZE]) {
	return View<T>(array, SIZE);
}

template<typename T, usize SIZE>
View<const T> constView(const T(&array)[SIZE]) {
	return View<const T>(array, SIZE);
}

template<typename T, usize SIZE>
View<const T> constView(const std::array<T, SIZE>& array) {
	return View<const T>(array.data(), array.size());
}

// Maybe rename this to singleValueConstView
template<typename T>
View<const T> constView(const T& singleValue) {
	return View<const T>(&singleValue, 1);
}

template<typename T>
View<const T> constView(const std::vector<T>& v) {
	return View<const T>(v.data(), v.size());
}
