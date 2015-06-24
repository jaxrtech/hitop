#pragma once

#include <cinttypes>
#include <device_launch_parameters.h>

namespace hitop {
namespace algo {
namespace util {

__host__ __device__
uint32_t hash32(uint32_t a);

__host__ __device__
uint64_t hash64(uint64_t key);

__host__ __device__
uint32_t hash64_32(uint64_t key);

__host__ __device__
uint32_t get_entropy_with_ptr(const void* address);


} // namespace util
} // namespace algo
} // namespace hitop