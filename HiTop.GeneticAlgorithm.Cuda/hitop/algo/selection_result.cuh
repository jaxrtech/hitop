#pragma once

#include <thrust/functional.h>

namespace hitop {
namespace algo {
namespace selection_result {

struct SelectionResult
{
	enum class Mode { Single, Pair };

	Mode mode;

	// The destination index of the child program descriptor
	size_t destination_index;

	union {
		struct { size_t single_index; };

		struct {
			size_t parent_index_a;
			size_t parent_index_b;
		};
	};
};

__host__ __device__
SelectionResult create_single_index(const size_t index);

__host__ __device__
SelectionResult create_pair_indices(const size_t destination_index,
									const size_t parent_index_a,
									const size_t parent_index_b);


struct from_index : public thrust::unary_function<size_t, SelectionResult>
{
	__host__ __device__
	SelectionResult operator()(size_t index);
};

} // namespace selection_result

typedef selection_result::SelectionResult SelectionResult;

} // namespace algo
} // namespace hitop