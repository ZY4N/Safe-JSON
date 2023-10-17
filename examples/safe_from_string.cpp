#include <sstream>
#include <iostream>
#include "../safe_json.hpp"

namespace jss = ztu::json::safe;

namespace my_object_builder {
	using namespace jss::literals;

	using my_default_object_t = object<
		set<"boolean", true_B>{},
		set<"integer", -1_I>{},
		set<"unsigned", 6_U>{},
		set<"floating", 3.5_N>{},
		set<"string", "test"_S>{},
		set<"enum",
			enumeration<"two",
				"one", "two", "three"
			>{}
		>{},
		set<"array",
			array<1_I,
				1_I, 2_I, 3_I
			>{}
		>{},
		set<"variant",
			variant<"one",
				holds<"one",
					set<"b", true_B>{}
				>{},
				holds<"two",
					set<"i", 1_I>{}
				>{},
				holds<"three",
					set<"f", 1.0_N>{}
				>{}
			>{}
		>{}
	>;
}

constexpr my_object_builder::my_default_object_t my_default_object{};

int main() {

	auto is = std::stringstream(
		"{\n"
		"	\"boolean\": true,\n"
		"	\"integer\": -3.33,\n"
		"	\"unsigned\": -345,\n"
		"	\"floating\": 2.0,\n"
		"	\"string\": \"test\",\n"
		"	\"enum\": \"one\",\n"
		"	\"numbers\": \"not-a-number\",\n"
		"	\"array\": {\n"
		"		\"test\": \"some-string\"\n"
		"	},\n"
		"	\"variant\": {\n"
		"      \"_type\": \"two\",\n"
		"      \"i\": 2\n"
		"    }\n"
		"}"
	);

	auto parser = jss::parser(is);
	auto value = parser.parse<my_default_object>();

	auto serializer = jss::serializer(std::cout);
	serializer.serialize<my_default_object>(value);
	std::cout << std::endl;
}
