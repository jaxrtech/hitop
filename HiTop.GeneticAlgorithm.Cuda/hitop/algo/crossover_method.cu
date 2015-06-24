#include "crossover_method.cuh"

#include "util.cuh"

namespace hitop {
namespace algo {
namespace crossover_method {

typedef typename detail::ProgramDesciptorPair ProgramDesciptorPair;

__host__ __device__
ProgramDesciptorPair::ProgramDesciptorPair(const ProgramDescriptor& a_, const ProgramDescriptor& b_)
	: a(a_)
	, b(b_)
{ }

//

point_crossover::point_crossover(thrust::device_ptr<algo::Settings> settings,
								 thrust::device_ptr<ProgramDescriptor> descriptors_old,
								 thrust::device_ptr<uint8_t> pool_old,
								 thrust::device_ptr<ProgramDescriptor> descriptors_new,
								 thrust::device_ptr<uint8_t> pool_new,
								 size_t pool_length)

	: settings_(settings.get())
	, descriptors_old_(descriptors_old.get())
	, descriptors_new_(descriptors_new.get())
	, pool_old_(pool_old.get())
	, pool_new_(pool_new.get())
	, pool_length_(pool_length)
{
	rng_ = thrust::default_random_engine(util::get_entropy_with_ptr(this));
}

__host__ __device__
ProgramDesciptorPair point_crossover::get_descriptor_pair(const SelectionResult& result)
{
	if (result.mode != SelectionResult::Mode::Pair) {
		printf("error: get_descriptor_pair() called with result of `Mode::Single`");
		// TODO: how to signal error from __device__ function
	}

	size_t a_idx = result.parent_index_a;
	size_t b_idx = result.parent_index_b;

	ProgramDescriptor& a_descriptor = descriptors_old_[a_idx];
	ProgramDescriptor& b_descriptor = descriptors_old_[b_idx];

	return ProgramDesciptorPair(a_descriptor, b_descriptor);
}

__host__ __device__
size_t point_crossover::get_child_length(const SelectionResult& result)
{
	switch (result.mode)
	{
	case SelectionResult::Mode::Single: {
		auto i = result.single_index;

		ProgramDescriptor descriptor = descriptors_old_[i];
		return descriptor.length;
	}

	case SelectionResult::Mode::Pair: {
		auto pair = get_descriptor_pair(result);

		size_t min_length = thrust::min(pair.a.length, pair.b.length);
		size_t max_length = thrust::max(pair.a.length, pair.b.length);

		thrust::uniform_int_distribution<size_t> length_range(min_length, max_length);
		size_t child_length = length_range(rng_);

		return child_length;
	}

	default: {
		printf("error: invalid SelectionResult::Mode enum value");
		return 0;
	}

	}
}

__host__ __device__
void point_crossover::operator()(SelectionResult& result)
{
	switch (result.mode)
	{
	case SelectionResult::Mode::Single: {
		size_t descriptor_idx = result.single_index;
		auto& descriptor = descriptors_old_[descriptor_idx];

		size_t pool_pos = settings_->block_size * result.destination_index;

		// Copy old program to new pool since there is nothing to cross over with

		// TODO: We are assuming the pool position will be the same
		size_t count = 0;
		size_t i = pool_pos;

		while (i < pool_length_
				&& count < descriptor.length) {

			pool_new_[i] = pool_old_[i];

			//

			i++;
			count++;
		}

		descriptors_new_[result.destination_index] = descriptors_old_[descriptor_idx];

		break;
	}

	case SelectionResult::Mode::Pair: {
		auto pair = get_descriptor_pair(result);
		size_t child_length = get_child_length(result);
		size_t child_pool_pos = settings_->block_size * result.destination_index;

		thrust::uniform_real_distribution<float> r01(0.0, 1.0);

		size_t count = 0;
		size_t a_idx = pair.a.pos;
		size_t b_idx = pair.b.pos;
		size_t x_idx = child_pool_pos;

		// Will be +1 from last element in array
		size_t end_idx = pool_length_;

		while (a_idx < end_idx
				&& b_idx < end_idx
				&& x_idx < end_idx
				&& count < child_length) {

			// Select parent randomly
			uint8_t value;
			if (r01(rng_) < 0.5) {
				value = pool_old_[a_idx];
			}
			else {
				value = pool_old_[b_idx];
			}

			pool_new_[x_idx] = value;

			//

			a_idx++;
			b_idx++;
			x_idx++;
			count++;
		}

		// Create the child program descriptor
		ProgramDescriptor child_descriptor;
		child_descriptor.pos = child_pool_pos;
		child_descriptor.length = child_length;
		child_descriptor.score = NAN;

		descriptors_new_[result.destination_index] = child_descriptor;
		break;
	}

	}

}


} // namespace crossover_method
} // namespace algo
} // namespace hitop