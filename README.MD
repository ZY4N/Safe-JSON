# Safe JSON

A JSON library made for config files.

This library provides a parser, serializer and needed data structures to safely read and write config files,
by replacing missing or malformed data with provided default values.
For convenience the library also contains a dynamic parser that can deal with unknown JSON data.


## Example
```c++
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
```
More examples can be found in the [examples directory](examples/).

## Testing
The parser implementation is tested for correctness using the [JSON Parsing Test Suite](https://github.com/nst/JSONTestSuite/tree/master) `test_parsing` tests, with the following exceptions:
* Tests for "trailing garbage" have been removed as this parser is based on a data stream and should not parse past the first JSON data structure.
* Tests that limit the floating point number representation to less than the C++ standard

## Performance
Performance is not the main focus of this library, but here is a small list, comparing the parsing and serialization speed to other implementations.
These numbers represent the time taken to parse the `twitter.json` from a string buffer and serializing to a new string on an i7-10750H.
(Benchmakring is only provided for a basic overview and does not represent scientifically sound data) 

`node benchamrk.js ./implementations ../twitter.json 100`

| Implementation | avg parse time (us) | avg serialize time (us) |
| -------------- | ----------------:| ---------------------:|
| [simd_json](https://github.com/simdjson/simdjson) | 243.78 | 703.01 | 
| [node_json](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/parse) | 4487.61 | 4504.89 |
| [python_json](https://docs.python.org/3/library/json.html) | 4463.91 | 14890.70|
| dynamic_json | 12508.96 | 8813.86 |
| [java_json](https://github.com/stleary/JSON-java) | 55500.71 | 30942.80 |




