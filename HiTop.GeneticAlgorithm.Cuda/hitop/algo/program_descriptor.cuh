#pragma once

#include <cinttypes>

#include <thrust/functional.h>
#include <thrust/device_ptr.h>

#include "settings.cuh"

namespace hitop {
namespace algo {
namespace program_descriptor {

typedef float score_t;

struct ProgramDescriptor
{
	size_t pos;
	size_t length;
	score_t score;
};


struct create_random : public thrust::unary_function<size_t, ProgramDescriptor>
{
private:
	Settings* settings_;

public:
	create_random(thrust::device_ptr<algo::Settings> settings_ptr);

	__host__ __device__
	ProgramDescriptor operator()(size_t index);
};

struct greater_score : public thrust::binary_function<ProgramDescriptor, ProgramDescriptor, bool>
{
	__host__ __device__
	bool operator()(const ProgramDescriptor& a, const ProgramDescriptor& b);
};

struct score : public thrust::unary_function<ProgramDescriptor, score_t>
{
	__host__ __device__
	score_t operator()(const ProgramDescriptor& x);
};

} // namespace program_descriptor

typedef typename program_descriptor::score_t score_t;
typedef typename program_descriptor::ProgramDescriptor ProgramDescriptor;

} // namespace algo
} // namespace hitop