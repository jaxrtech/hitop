#pragma once

#include <thrust/device_ptr.h>
#include <thrust/device_new.h>
#include <thrust/device_malloc.h>

namespace thrust {

// Inline the implementation or else you will get a linker error from not having all the
// types used in the templated function from actually have their own templated function to use.

template<typename T>
thrust::device_ptr<T> device_new(const T& host_ptr)
{
	return thrust::device_new<T>(thrust::device_malloc<T>(1),
								 host_ptr);
}

}
