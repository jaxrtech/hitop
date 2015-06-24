#pragma once

#include <iostream>

namespace hitop {
namespace algo {

typedef std::streamsize buffer_size_t;

struct Settings
{
	buffer_size_t min_program_size;
	buffer_size_t max_program_size;
	buffer_size_t block_size;
};

} // end algo
} // end hitop
