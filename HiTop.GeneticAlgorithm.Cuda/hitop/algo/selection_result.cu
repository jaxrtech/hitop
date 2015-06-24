#include "selection_result.cuh"

namespace hitop {
namespace algo {
namespace selection_result {

__host__ __device__
SelectionResult create_single_index(const size_t index)
{
	SelectionResult result;
	result.mode = SelectionResult::Mode::Single;
	result.destination_index = index;
	result.single_index = index;

	return result;
}

__host__ __device__
SelectionResult create_pair_indices(const size_t destination_index,
									const size_t parent_index_a,
									const size_t parent_index_b)
{
	SelectionResult result;
	result.mode = SelectionResult::Mode::Pair;
	result.destination_index = destination_index;
	result.parent_index_a = parent_index_a;
	result.parent_index_b = parent_index_b;

	return result;
}

//

__host__ __device__
SelectionResult from_index::operator()(size_t index)
{
	return create_single_index(index);
}

} // namespace selection_result

typedef selection_result::SelectionResult SelectionResult;

} // namespace algo
} // namespace hitop