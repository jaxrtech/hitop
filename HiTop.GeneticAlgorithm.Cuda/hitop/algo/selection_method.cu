#include "selection_method.cuh"

#include "util.cuh"

namespace hitop {
namespace algo {
namespace selection_method {

tournament_selection::tournament_selection(thrust::device_ptr<Context> context_ptr)
	: context_(context_ptr.get())
{
	rng_ = thrust::default_random_engine(util::get_entropy_with_ptr(this));
}

__host__ __device__
SelectionResult tournament_selection::operator()(size_t index)
{
	// Only now can we dereference `context_` since we are now dereferencing a device pointer on
	// the device versus in the constructor which is solely on the host.
	indices_range_ = indices_range_distribution(0, context_->program_descriptors_count);

	return selection_result::create_pair_indices(index, index, get_next_winner_index());
}

__host__ __device__
size_t tournament_selection::get_random_index()
{
	return indices_range_(rng_);
}

__host__ __device__
size_t tournament_selection::get_next_winner_index()
{
	ProgramDescriptor *program_descriptors = context_->program_descriptors;

	auto cur_idx = get_random_index();
	auto cur_score = program_descriptors[cur_idx].score;

	for (uint32_t i = 0; i < context_->rounds; i++) {
		auto other_idx = get_random_index();
		auto other_score = program_descriptors[other_idx].score;

		if (other_score > cur_score) {
			cur_idx = other_idx;
		}
	}

	return cur_idx;
}


} // namespace selection_method
} // namespace algo
} // namespace hitop