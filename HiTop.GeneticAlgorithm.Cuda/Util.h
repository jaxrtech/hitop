#pragma once

#include <string>
#include <iostream>

namespace hitop {
	
namespace util {

struct AppSettings
{
	std::string input_path;
};

void output_header(std::ostream& stream);

void output_usage(std::ostream& stream);

std::string get_timestamp_readable();

bool try_parse_settings(int argc, char* argv[], AppSettings& settings);

}
}