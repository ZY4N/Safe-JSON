#include <iostream>
#include <chrono>
#include <sstream>
#include "simdjson.h"

using namespace simdjson;


int main(int num_args, char* args[]) {

	using clock = std::chrono::high_resolution_clock;

	if (num_args < 2) {
		throw std::runtime_error("Filename argument missing");
	}

	ondemand::parser parser;
    padded_string text = padded_string::load(args[1]);

	const auto begin_parse = clock::now();
	ondemand::document value = parser.iterate(text);
	const auto end_parse = clock::now();

	std::ostringstream os;

	const auto begin_serialize = clock::now();
	os << value;
	const auto end_serialize = clock::now();

	std::cout << ((end_parse - begin_parse).count() / 1000.0) << std::endl;
	std::cout << ((end_serialize - begin_serialize).count() / 1000.0) << std::endl;
}
