#include <sstream>
#include <iostream>
#include "../dynamic_json.hpp"

namespace jsd = ztu::json::dynamic;

int main() {

	auto is = std::stringstream(
		"{\n"
		"   \"Name\": \"Test\",\n"
		"   \"Mobile\": 12345678,\n"
		"   \"Boolean\": true,\n"
		"   \"Pets\": [\"Dog\", \"cat\"],\n"
		"   \"Address\": \n"
		"     {\n"
		"         \"Permanent address\": \"USA\",\n"
		"         \"Current Address\": \"AU\"\n"
		"     }\n"
		"}"
	);

	auto parser = jsd::parser(is);
	auto value = parser.parse();

	std::cout << value << std::endl;

	std::cout << "name: " << value["Name"] << std::endl;
	std::cout << "first pet: " << value["Pets"][0] << std::endl;

	std::cout << "mobile: " << value["Mobile"] << std::endl;
	value["Mobile"] = 1234.5678;
	std::cout << "mobile: " << value["Mobile"] << std::endl;

	auto &address = reinterpret_cast<jsd::data_types::object&>(value["Address"]);
	address.insert({ "Future address", jsd::data_types::string("Moon") });

	std::cout << "address: " << value["Address"] << std::endl;

}
