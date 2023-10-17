
#include <utility>
#include <cstdint>
#include <variant>
#include <vector>
#include <string>
#include <array>
#include <tuple>
#include <span>
#include <string_view>
#include <istream>
#include <ostream>
#include <iomanip>
#include <format>
#include <algorithm>
#include <stdexcept>

#include <cmath>
#include <bitset>

#ifndef ZTU_JSON_DETAIL
#define ZTU_JSON_DETAIL

namespace ztu::json_internal {

namespace literal_config {

	constexpr char left_brace[] = "{";
	constexpr char right_brace[] = "}";
	constexpr char left_bracket[] = "[";
	constexpr char right_bracket[] = "]";
	constexpr char colon[] = ":";
	constexpr char comma[] = ",";
	constexpr char null[] = "null";
	constexpr char boolean_false[] = "false";
	constexpr char boolean_true[] = "true";
	constexpr char string_begin = '"';
	constexpr char string_end = '"';
	constexpr char variant_type[] = "_type";

} // namespace literal_config

#define ztu_ic inline constexpr
#define ztu_nic [[nodiscard]] inline constexpr

namespace for_each {

template<auto Size>
ztu_ic bool index(auto &&f) {
	return [&]<auto... Indices>(std::index_sequence<Indices...>) {
		return (f.template operator()<Indices>() || ...);
	}(std::make_index_sequence<Size>());
}

template<typename... Types>
ztu_ic bool type(auto &&f) {
	return (f.template operator()<Types>() || ...);
}

template<auto... Values>
ztu_ic bool value(auto &&f) {
	return (f.template operator()<Values>() || ...);
}

template<auto... Values>
ztu_ic bool indexed_value(auto &&f) {
	return [&]<auto... Indices>(std::index_sequence<Indices...>) {
		return (f.template operator()<Indices, Values>() || ...);
	}(std::make_index_sequence<sizeof...(Values)>());
}

template<typename... Args>
ztu_ic bool indexed_argument(auto &&f, Args&&... args) {
	return [&]<auto... Indices>(std::index_sequence<Indices...>) {
		return (f.template operator()<Indices>(std::forward<Args>(args)) || ...);
	}(std::make_index_sequence<sizeof...(Args)>());
}

} // namespace for_each


using std::size_t;
using ssize_t = std::make_signed_t<size_t>;

template<ssize_t N>
	requires (N > 0)
struct string_literal {
	static constexpr auto max_length = N - 1;

	ztu_ic string_literal() = default;

	ztu_ic string_literal(const char (&str)[N]);

	template<ssize_t M> requires (M > 0 and M <= N)
	ztu_ic string_literal(const char (&str)[M]);

	template<typename... Chars> requires (sizeof...(Chars) == max_length)
	ztu_ic string_literal(Chars... chars) : value{chars..., '\0' } {}

	template<ssize_t M>
	ztu_nic bool operator==(const string_literal<M> &other) const;

	ztu_nic const char *c_str() const;

	ztu_nic ssize_t size() const;

	ztu_nic auto begin();
	ztu_nic auto begin() const;

	ztu_nic auto end();
	ztu_nic auto end() const;

	ztu_nic char& operator[](size_t index);
	ztu_nic const char& operator[](size_t index) const;

	template<ssize_t M>
	inline friend std::ostream& operator<<(std::ostream &out, const string_literal<M>& str);

public:
	std::array<char,N> value{};
};

template<ssize_t N>
	requires (N > 0)
ztu_ic string_literal<N>::string_literal(const char (&str)[N]) {
	std::copy_n(std::begin(str), N, value.begin());
}

template<ssize_t N> requires (N > 0)
template<ssize_t M> requires (M > 0 and M <= N)
ztu_ic string_literal<N>::string_literal(const char (&str)[M]) {
	std::copy_n(std::begin(str), M, value.begin());
	value[M-1] = '\0';
}

template<ssize_t N>
	requires (N > 0)
template<ssize_t M>
ztu_ic bool string_literal<N>::operator==(const string_literal<M> &other) const {
	const auto length = std::min(N,M);
	for (ssize_t i = 0; i < length; i++) {
		if (value[i] != other.value[i]) {
			return false;
		}
	}
	return true;
}

template<ssize_t N> requires (N > 0)
ztu_nic const char *string_literal<N>::c_str() const { return value.data(); }

template<ssize_t N> requires (N > 0)
ztu_nic ssize_t string_literal<N>::size() const {
	ssize_t length = 0;
	while (length < N && value[length] != '\0')
		length++;
	return length;
}

template<ssize_t N> requires (N > 0)
ztu_nic auto string_literal<N>::begin() { return value.begin(); }

template<ssize_t N> requires (N > 0)
ztu_nic auto string_literal<N>::begin() const { return value.begin(); }

template<ssize_t N> requires (N > 0)
ztu_nic auto string_literal<N>::end() { return value.begin() + size(); }

template<ssize_t N> requires (N > 0)
ztu_nic auto string_literal<N>::end() const { return value.begin() + size(); }

template<ssize_t M>
inline std::ostream& operator<<(std::ostream &out, const string_literal<M>& str) {
	return out << str.c_str();
}

template<ssize_t N> requires (N > 0)
ztu_nic char& string_literal<N>::operator[](size_t index) {
	return value[index];
}

template<ssize_t N> requires (N > 0)
ztu_nic const char& string_literal<N>::operator[](size_t index) const {
	return value[index];
}

namespace string_literals {
	template<string_literal Str>
	constexpr auto operator"" _sl() {
		return Str;
	}
}


namespace transcoding_error {

	enum class codes {
		OK = 0,
		WRONG_SPELLING,
		UNKNOWN_SPELLING_INDEX,
		NONE_HEX_CHAR_IN_UNICODE_ESCAPE,
		UNKNOWN_ESCAPE_SEQUENCE,
		UNESCAPED_CHARACTER,
		CUT_OF_UTF8_CHARACTER,
		EOF_IN_TOKEN
	};

	struct category : std::error_category {
		const char* name() const noexcept override {
			return "transcoding";
		}
		std::string message(int ev) const override {
			switch (static_cast<codes>(ev)) {
			case codes::WRONG_SPELLING:
				return "Character sequence does not match any of the valid spellings for this token.";
			case codes::UNKNOWN_SPELLING_INDEX:
				return "The specified index does not correspond to a spelling.";
			case codes::NONE_HEX_CHAR_IN_UNICODE_ESCAPE:
				return "None hex character in unicode escape sequence.";
			case codes::UNKNOWN_ESCAPE_SEQUENCE:
				return "Unknown escape sequence.";
			case codes::UNESCAPED_CHARACTER:
				return "Unescaped character string literal.";
			case codes::EOF_IN_TOKEN:
				return "Reached end of file while parsing token.";
			case codes::CUT_OF_UTF8_CHARACTER:
				return "utf8 character begin without second byte.";
			default:
				using namespace std::string_literals;
				return "unrecognized error ("s + std::to_string(ev) + ")";
			}
		}
	};
} // namespace transcoding_error


inline std::error_category& transcoding_error_category() {
	static transcoding_error::category category;
	return category;
}

namespace transcoding_error {
	inline std::error_code make_error_code(codes e) {
		return { static_cast<int>(e), transcoding_error_category() };
	}
} // namespace transcoding_error

} // ztu::json_internal

template <>
struct std::is_error_code_enum<ztu::json_internal::transcoding_error::codes> : public std::true_type {};

namespace ztu::json_internal::transcoding {

template<class T, typename Enum>
concept token_concept = (
	std::same_as<Enum, typename T::enum_t> &&
	requires(const T::value_t& value, std::vector<char>& buffer, std::istream& is, std::ostream& os, std::error_code& e) {
		{ T::begins_with(is)		} -> std::same_as<bool>;
		{ T::read(is, buffer, e)	} -> std::same_as<typename T::value_t>;
		{ T::write(os, value, e)	} -> std::same_as<void>;
		{ T::name()				} -> std::same_as<std::string_view>;
		{ T::type()				} -> std::same_as<typename T::enum_t>;
	}
);

namespace token_internal {

template<typename Enum, Enum Type, string_literal Name, typename ValueType = std::monostate>
struct token_base {
	using enum_t = Enum;
	using value_t = ValueType;

	constexpr static inline Enum type() {
		return Type;
	}

	constexpr static inline std::string_view name() {
		return { Name.c_str() };
	}
};


template<auto Type, string_literal Name, string_literal Spelling>
struct literal_token : public token_base<decltype(Type), Type, Name> {
	using base_t = token_base<decltype(Type), Type, Name> ;
	using typename base_t::value_t;

	inline static bool begins_with(std::istream &is) {
		return is.peek() == Spelling[0U];
	}

	inline static value_t read(std::istream &is, std::vector<char> &, std::error_code& error) {
		for (const auto& c : Spelling) {
			if (is.peek() != c) {
				using transcoding_error::make_error_code;
				using enum transcoding_error::codes;
				error = make_error_code(WRONG_SPELLING);
				break;
			}
			is.get();
		}
		return {};
	}

	inline static void write(std::ostream &os, const value_t&, std::error_code& error) {
		os.write(Spelling.c_str(), Spelling.size());
	}
};

template<auto Type, string_literal Name, std::integral IndexType, string_literal... Spellings>
struct multi_literal_token : token_base<decltype(Type), Type, Name, IndexType> {
	using base_t = token_base<decltype(Type), Type, Name, IndexType>;
	using typename base_t::value_t;

	inline static bool begins_with(std::istream &is) {
		const auto c = is.peek();
		return for_each::value<Spellings...>(
			[&]<auto Spelling>() {
				return c == Spelling[0U];
			}
		);
	}

	inline static value_t read(std::istream &is, std::vector<char> &, std::error_code& error) {
		value_t index = -1;
		const auto found_spelling = for_each::indexed_value<Spellings...>(
			[&]<auto Index, auto Spelling>() {
				for (const auto& c : Spelling) {
					if (is.peek() != c) {
						return false;
					}
					is.get();
				}
				index = Index;
				return true;
			}
		);
		if (not found_spelling) {
			using transcoding_error::make_error_code;
			using enum transcoding_error::codes;
			error = make_error_code(WRONG_SPELLING);
		}
		return index;
	}

	inline static void write(std::ostream &os, const value_t &index, std::error_code& error) {
		const auto found_spelling = for_each::indexed_value<Spellings...>(
			[&]<auto Index, auto Spelling>() {
				if (index == Index) {
					os.write(Spelling.c_str(), Spelling.size());
					return true;
				}
				return false;
			}
		);
		if (not found_spelling) [[unlikely]] {
			using transcoding_error::make_error_code;
			using enum transcoding_error::codes;
			error = make_error_code(UNKNOWN_SPELLING_INDEX);
		}
	}
};

template<auto Type, string_literal Name, char BeginChar, char EndChar>
struct string_literal_token : public token_base<decltype(Type), Type, Name, std::string_view> {

	using base_t = token_base<decltype(Type), Type, Name, std::string_view>;
	using typename base_t::value_t;

	inline static bool begins_with(std::istream &is) {
		return is.peek() == BeginChar;
	}

	inline static auto backslash_escape_chars = {
		std::pair{ '\\', '\\' },
		std::pair{ 'b', '\b' },
		std::pair{ 'f', '\f' },
		std::pair{ 'n', '\n' },
		std::pair{ 'r', '\r' },
		std::pair{ 't', '\t' },
		std::pair{ 'v', '\v' },
		std::pair{ '"', '"' },
		std::pair{ '/', '/' }
	};

	inline static uint16_t parse_hex_short(std::istream& is, std::error_code& error) {
		using transcoding_error::make_error_code;
		using enum transcoding_error::codes;

		uint16_t value = 0;
		for (int i = 0; i < 4; i++) {
			unsigned char c = is.get();
			if ('0' <= c and c <= '9') {
				c -= '0';
			} else if ('a' <= c and c <= 'f') {
				c -= ('a' - 10);
			} else if ('A' <= c and c <= 'F') {
				c -= ('A' - 10);
			} else [[unlikely]] {
				error = make_error_code(NONE_HEX_CHAR_IN_UNICODE_ESCAPE);
				break;
			}
			value = (value << 4) | c;
		}
		return value;
	}

	inline static value_t read(std::istream &is, std::vector<char> &buffer, std::error_code& error) {
		using transcoding_error::make_error_code;
		using enum transcoding_error::codes;

		char c;

		const auto next = [&]() { c = is.get(); };

		next(); // ignore BeginChar

		while (true) {

			next();

			if (is.eof()) {
				error = make_error_code(EOF_IN_TOKEN);
				goto on_error;
			}

			if (c == '\\') [[unlikely]] {
				next();
				auto backslash_escape = false;
				for (const auto& [ letter, replace ] : backslash_escape_chars) {
					if (c == letter) {
						c = replace;
						backslash_escape = true;
						break;
					}
				}

				if (not backslash_escape) [[unlikely]] {
					if (c == 'u') {
						const auto raw_value = parse_hex_short(is, error);
						if (error) goto on_error;

						auto lsb = uint8_t(raw_value & 0xff);
						auto msb = uint8_t(raw_value >> 8);

						// encode to utf-8 multichar
						if (raw_value < 0x80) {
							buffer.push_back(char(lsb));
						} else if (raw_value < 0x800) {
							buffer.push_back(char(0xc0 | ((msb & 0x7) << 2) | (lsb >> 6)));
							buffer.push_back(char(0x80 | (lsb & 0x3f)));
						} else {
							buffer.push_back(char(0xe0 | (msb >> 4)));
							buffer.push_back(char(0x80 | ((msb & 0xf) << 2) | (lsb >> 6)));
							buffer.push_back(char(0x80 | (lsb & 0x3f)));
						}
						continue;
					} else {
						error = make_error_code(UNKNOWN_ESCAPE_SEQUENCE);
						goto on_error;
					}
				}
			} else [[likely]] {
				auto unescaped = false;
				for (const auto& escape : {
					'\b', '\f', '\n', '\r', '\t', '\v'
				}) {
					if (c == escape) {
						unescaped = true;
						break;
					}
				}
				if (unescaped) [[unlikely]] {
					error = make_error_code(UNESCAPED_CHARACTER);
					goto on_error;
				}
				if (c == EndChar) [[unlikely]] {
					break;
				}
			}
			buffer.push_back(c);
		}

		return { buffer.data(), buffer.data() + buffer.size() };

	on_error:
		is.setstate(std::ios_base::failbit);

		return { buffer.data(), buffer.data() };
	}

	inline static void write(std::ostream &os, const value_t &value, std::error_code& error) {
		using transcoding_error::make_error_code;
		using enum transcoding_error::codes;

		os.put(BeginChar);

		const auto length = static_cast<ssize_t>(value.length());

		for (ssize_t i = 0; i < length; i++) {
			auto c = value[i];
			auto backslash_escape = false;

			for (const auto& [ replace, letter ] : backslash_escape_chars) {
				if (c == letter) {
					c = replace;
					backslash_escape = true;
					break;
				}
			}

			if (backslash_escape) {
				os.put('\\');
				os.put(c);
				continue;
			}

			const auto first_bit = c & 0x80;
			const auto third_bit = c & 0x20;
			const auto fourth_bit = c & 0x10;

			// check third and fourth prefix bit to distinguish encoding
			const auto num_utf8_bytes = first_bit ? (third_bit ? (fourth_bit ? 4 : 3) : 2) : 1;

			if ((i + num_utf8_bytes) > length) [[unlikely]] {
				error = make_error_code(CUT_OF_UTF8_CHARACTER);
				return;
			}

			uint32_t raw_value;
			if (num_utf8_bytes == 1) {
				raw_value = uint8_t(
					value[i]
				);
			} else if (num_utf8_bytes == 2) {
				raw_value = uint32_t(
					((value[i] & 0x1f) << 6) | (value[i+1] & 0x3f)
				);
			} else if (num_utf8_bytes == 3) {
				raw_value = uint32_t(
					((value[i] & 0xf) << 12) | ((value[i+1] & 0x3f) << 6) | (value[i+2] & 0x3f)
				);
			} else if (num_utf8_bytes == 4) {
				raw_value = uint32_t(
					((value[i] & 0x7) << 18) | ((value[i+1] & 0x3f) << 12) | ((value[i+2] & 0x3f) << 6) | (value[i+3] & 0x3f)
				);
			}

			if (
				(0x0    <= raw_value and raw_value < 0x20  ) or
				(0xd800 <= raw_value and raw_value < 0xe000)
			) {
				os << "\\u" << std::setw(4) << std::setfill('0') << std::hex << raw_value << std::dec;
			} else {
				os.write(&value[i], num_utf8_bytes);
			}

			// i is also incremented in loop expression
			i += num_utf8_bytes - 1;
		}

		os.put(EndChar);
	}
};

template<auto Type, string_literal Name>
struct number_literal_token : public token_base<decltype(Type), Type, Name, double> {
	using base_t = token_base<decltype(Type), Type, Name, double>;
	using typename base_t::value_t;

	inline static bool begins_with(std::istream &is) {
		const auto c = is.peek();
		return std::isdigit(c) or c == '-' or c == '+';
	}

	inline static value_t read(std::istream &is, std::vector<char> &, std::error_code& error) {
		value_t d;
		is >> d;
		return d;
	}

	inline static void write(std::ostream &os, const value_t &value, std::error_code& error) {
		os << std::setprecision(std::numeric_limits<double>::max_digits10 - 1) << std::defaultfloat << value;
	}
};

template<auto Type, string_literal Name>
struct end_token : public token_base<decltype(Type), Type, Name> {
	using base_t = token_base<decltype(Type), Type, Name>;
	using typename base_t::value_t;

	inline static bool begins_with(std::istream &is) {
		return is.eof();
	}

	value_t static read(std::istream &, std::vector<char> &, std::error_code& error) {
		return {};
	}

	inline static void write(std::ostream &os, const value_t &, std::error_code& error) {
		os.flush();
	}
};

} // namespace token_internal

namespace tokens {

enum class type : unsigned char {
	END,
	LBRACE, RBRACE,
	COLON, COMMA,
	STRING, NUMBER, BOOLEAN,
	LBRACKET, RBRACKET,
	UNDEFINED,
};

using left_brace = token_internal::literal_token<type::LBRACE, "left-brace", literal_config::left_brace>;
using right_brace = token_internal::literal_token<type::RBRACE, "right-brace", literal_config::right_brace>;
using left_bracket = token_internal::literal_token<type::LBRACKET, "left-bracket", literal_config::left_bracket>;
using right_bracket = token_internal::literal_token<type::RBRACKET, "right-bracket", literal_config::right_bracket>;
using colon = token_internal::literal_token<type::COLON, "colon", literal_config::colon>;
using comma = token_internal::literal_token<type::COMMA, "comma", literal_config::comma>;
using null = token_internal::literal_token<type::UNDEFINED, "null", literal_config::null>;
using boolean = token_internal::multi_literal_token<type::BOOLEAN, "boolean", bool, literal_config::boolean_false, literal_config::boolean_true>;
using string = token_internal::string_literal_token<type::STRING, "string", literal_config::string_begin, literal_config::string_end>;
using number = token_internal::number_literal_token<type::NUMBER, "number">;
using end = token_internal::end_token<type::END, "end">;

} // namespace tokens


template<typename Types, token_concept<Types>... Tokens>
class token_instance {
public:
	template<typename... Args>
	ztu_ic token_instance(Args &&... args);

	template<Types Type, typename... Args>
	ztu_nic static decltype(auto) make(Args &&... args);

	ztu_nic static std::string_view name_of(Types type);

	template<Types Type, typename... Args>
	ztu_ic void set(Args &&... args);

	template<Types Type>
	ztu_nic decltype(auto) get();

	template<Types Type>
	ztu_nic decltype(auto) get() const;

	ztu_nic Types type() const;

private:
	using enum_t = std::underlying_type_t<Types>;
	inline constexpr static auto names = std::array{ Tokens::name()..., std::string_view("unknown") };

private:
	using value_t = std::variant<typename Tokens::value_t...>;
	value_t m_value;
};


template<typename Types, token_concept<Types>... Tokens>
template<typename... Args>
ztu_ic token_instance<Types, Tokens...>::token_instance(Args &&... args)
	: m_value(std::forward<Args>(args)...) {}

template<typename Types, token_concept<Types>... Tokens>
template<Types Type, typename... Args>
ztu_nic decltype(auto) token_instance<Types, Tokens...>::make(Args &&... args) {
	constexpr auto index = static_cast<size_t>(static_cast<enum_t>(Type));
	return token_instance(std::in_place_index<index>, std::forward<Args>(args)...);
}

template<typename Types, token_concept<Types>... Tokens>
ztu_nic std::string_view token_instance<Types, Tokens...>::name_of(Types type) {
	const auto index = static_cast<size_t>(static_cast<enum_t>(type));
	return names[std::min(index, sizeof...(Tokens))];
}

template<typename Types, token_concept<Types>... Tokens>
template<Types Type, typename... Args>
ztu_ic void token_instance<Types, Tokens...>::set(Args &&... args) {
	constexpr auto index = static_cast<size_t>(static_cast<enum_t>(Type));
	m_value.template emplace<index>(std::forward<Args>(args)...);
}

template<typename Types, token_concept<Types>... Tokens>
template<Types Type>
ztu_nic decltype(auto) token_instance<Types, Tokens...>::get() {
	constexpr auto index = static_cast<size_t>(static_cast<enum_t>(Type));
	return std::get<index>(m_value);
}

template<typename Types, token_concept<Types>... Tokens>
template<Types Type>
ztu_nic decltype(auto) token_instance<Types, Tokens...>::get() const {
	constexpr auto index = static_cast<size_t>(static_cast<enum_t>(Type));
	return std::get<index>(m_value);
}

template<typename Types, token_concept<Types>... Tokens>
ztu_nic Types token_instance<Types, Tokens...>::type() const {
	return static_cast<Types>(static_cast<enum_t>(m_value.index()));
}


using default_token_instance = token_instance<tokens::type,
	tokens::end,
	tokens::left_brace,
	tokens::right_brace,
	tokens::comma,
	tokens::colon,
	tokens::string,
	tokens::number,
	tokens::boolean,
	tokens::left_bracket,
	tokens::right_bracket,
	tokens::null
>;


struct source_location {
	size_t row, column;
	inline friend std::ostream &operator<<(std::ostream &os, const source_location &location);
};

std::ostream &operator<<(std::ostream &os, const source_location &location) {
	return os << '(' << location.row << ", " << location.column << ')';
}


template<typename Types, token_concept<Types>... Tokens>
class tokenizer {
private:
	using token_instance_t = token_instance<Types, Tokens...>;
	using this_t = tokenizer<Types, Tokens...>;

public:
	ztu_ic tokenizer();
	ztu_ic tokenizer(std::istream &is);

	ztu_ic void set_is(std::istream &is);

	ztu_ic this_t& operator>>(std::tuple<token_instance_t &, source_location &> dst);

	ztu_ic void accept();

	template<Types... Options>
	ztu_nic ssize_t ignore_until();

private:
	source_location m_location{ 1, 1 };
	std::istream *m_is{ nullptr };
	std::vector<char> m_buffer{};
};

template<typename Types, token_concept<Types>... Tokens>
ztu_ic tokenizer<Types, Tokens...>::tokenizer() = default;

template<typename Types, token_concept<Types>... Tokens>
ztu_ic tokenizer<Types, Tokens...>::tokenizer(std::istream &is) : m_is{ &is } {}

template<typename Types, token_concept<Types>... Tokens>
ztu_ic void tokenizer<Types, Tokens...>::set_is(std::istream &is) {
	m_is = &is;
	m_location = { 1, 1 };
}

template<typename Types, token_concept<Types>... Tokens>
ztu_ic tokenizer<Types, Tokens...>& tokenizer<Types, Tokens...>::operator>>(std::tuple<token_instance_t &, source_location &> dst) {

	auto &[token, location] = dst;

	while (not m_is->eof() and std::isspace(m_is->peek()))
		accept();

	// The guaranteed lifetime of the previous string token ends here
	m_buffer.clear();

	location = m_location;
	const auto before_pos = m_is->tellg();

	std::error_code token_error;

	const auto found_token = for_each::type<Tokens...>([&]<class Token>() {
		if (Token::begins_with(*m_is)) {
			token.template set<Token::type()>(
				Token::read(*m_is, m_buffer, token_error)
			);
			return true;
		}
		return false;
	});

	if (not found_token) {
		throw std::runtime_error(
			std::format(
				"Unexpected character {} at position ({}, {})",
				static_cast<char>(m_is->peek()),
				m_location.row,
				m_location.column
			)
		);
	}

	if (token_error) {
		throw std::runtime_error(
			std::format(
				"Error while parsing token {} token starting at position ({}, {}): {}",
				token_instance_t::name_of(token.type()),
				m_location.row,
				m_location.column,
				token_error.message()
			)
		);
	}


	const auto after_pos = m_is->tellg();
	if ((after_pos == -1 or m_is->fail()) and not (
		m_is->eof() and (
			token.type() == tokens::type::END or token.type() == tokens::type::NUMBER
		)
	)) {
		throw std::runtime_error(
			std::format(
				"Error while parsing {} token starting at position ({}, {})",
				token_instance_t::name_of(token.type()),
				m_location.row,
				m_location.column
			)
		);
	}

	// assuming there are no linebreaks within a token_concept
	m_location.column += after_pos - before_pos;

	return *this;
}

template<typename Types, token_concept<Types>... Tokens>
template<Types... Options>
ztu_nic ssize_t tokenizer<Types, Tokens...>::ignore_until() {
	// This function only works if the first character of each literal is unique
	// even across the rest of all other literals letters.
	ssize_t index = -1;
	while (true) {
		auto found_token = false;
		if (not std::isspace(m_is->peek())) {
			found_token = json_internal::for_each::indexed_value<Options...>([&]<auto Index, auto Option>() {
				return json_internal::for_each::type<Tokens...>([&]<typename Token>() {
					if constexpr (Option == Token::type()) {
						if (Token::begins_with(*m_is)) {
							index = Index;
							return true;
						}
					}
					return false;
				});
			});
		}
		accept();
		if (found_token) {
			break;
		}
	}
	return index;
}

template<typename Types, token_concept<Types>... Tokens>
ztu_ic void tokenizer<Types, Tokens...>::accept() {
	++m_location.column;
	if (m_is->get() == '\n') {
		++m_location.row;
		m_location.column = 1;
	}
}


enum class detokenizer_instructions {
	space, tab, indent, outdent, clear_indent, newline
};

struct detokenizer_format {
	std::string_view space, tab, newline;
};

namespace detokenizer_formats {

constexpr detokenizer_format pretty_tab{
	.space{ " " },
	.tab{ "\t" },
	.newline{ "\n" },
};

constexpr detokenizer_format pretty_space{
	.space{ " " },
	.tab{ " " },
	.newline{ "\n" },
};

constexpr detokenizer_format minimal{
	.space{ "" },
	.tab{ "" },
	.newline{ "" },
};

} // namespace detokenizer_formats

template<typename Types, token_concept<Types>... Tokens>
class detokenizer {
private:
	using token_instance_t = token_instance<Types, Tokens...>;
	using this_t = detokenizer<Types, Tokens...>;

public:
	ztu_ic detokenizer(const detokenizer_format &fmt = detokenizer_formats::pretty_tab);
	ztu_ic detokenizer(std::ostream &os, const detokenizer_format &fmt = detokenizer_formats::pretty_tab);

	ztu_nic std::ostream*& os();
	ztu_nic std::ostream*const& os() const;

	ztu_nic detokenizer_format& format();
	ztu_nic const detokenizer_format& format() const;

	ztu_ic this_t& operator<<(const token_instance_t &token);
	ztu_ic this_t& operator<<(const detokenizer_instructions &instruction);

private:
	std::ostream *m_os{ nullptr };
	detokenizer_format m_format;
	int m_depth{ 0 };
};

template<typename Types, token_concept<Types>... Tokens>
ztu_ic detokenizer<Types, Tokens...>::detokenizer(const detokenizer_format &fmt)
	: m_format{ fmt } {};

template<typename Types, token_concept<Types>... Tokens>
ztu_ic detokenizer<Types, Tokens...>::detokenizer(std::ostream &os, const detokenizer_format &fmt)
	: m_os{ &os }, m_format{ fmt } {}

template<typename Types, token_concept<Types>... Tokens>
ztu_nic std::ostream*& detokenizer<Types, Tokens...>::os() {
	return m_os;
}

template<typename Types, token_concept<Types>... Tokens>
ztu_nic std::ostream*const& detokenizer<Types, Tokens...>::os() const {
	return m_os;
}

template<typename Types, token_concept<Types>... Tokens>
ztu_nic detokenizer_format& detokenizer<Types, Tokens...>::format() {
	return m_format;
}

template<typename Types, token_concept<Types>... Tokens>
ztu_nic const detokenizer_format& detokenizer<Types, Tokens...>::format() const {
	return m_format;
}

template<typename Types, token_concept<Types>... Tokens>
ztu_ic detokenizer<Types, Tokens...>::this_t& detokenizer<Types, Tokens...>::operator<<(const detokenizer_instructions &instruction) {
	switch (instruction) {
		using
		enum detokenizer_instructions;
		case space: {
			(*m_os) << m_format.space;
			break;
		}
		case tab: {
			(*m_os) << m_format.tab;
			break;
		}
		case indent: {
			m_depth++;
			break;
		}
		case outdent: {
			if (m_depth > 0) {
				m_depth--;
			}
			break;
		}
		case clear_indent: {
			m_depth = 0;
			break;
		}
		case newline: {
			(*m_os) << m_format.newline;
			for (int i = 0; i < m_depth; i++) {
				(*m_os) << m_format.tab;
			}
			break;
		}
		default: {
			throw std::runtime_error(std::format("Unsupported instruction '{}'", int(instruction)));
		}
	}
	return *this;
}

template<typename Types, token_concept<Types>... Tokens>
ztu_ic detokenizer<Types, Tokens...>::this_t& detokenizer<Types, Tokens...>::operator<<(const token_instance_t &token) {

	std::error_code token_error;

	const auto type = token.type();
	const auto found_token = for_each::type<Tokens...>([&]<class Token>() {
		if (Token::type() == type) {
			Token::write(*m_os, token.template get<Token::type()>(), token_error);
			return true;
		}
		return false;
	});

	if (not found_token) {
		using namespace std::string_literals;
		throw std::runtime_error(
			std::format(
					"Unknown token '{}'",
					token_instance_t::name_of(type)
			)
		);
	}

	if (token_error) {
		throw std::runtime_error(
			std::format(
				"Error while serializing token {}: {}",
				token_instance_t::name_of(token.type()),
				token_error.message()
			)
		);
	}

	return *this;
}


template<typename Types, token_concept<Types>... Tokens>
class parser {
protected:
	using token_instance_t = token_instance<Types, Tokens...>;

public:
	ztu_ic parser();
	ztu_ic explicit parser(std::istream &is);

	ztu_ic void set_is(std::istream &is);

protected:
	ztu_ic void accept();

	ztu_ic token_instance_t accept(Types type);

protected:
	token_instance_t token;
	source_location location;
	tokenizer<Types, Tokens...> tokens;
};

template<typename Types, token_concept<Types>... Tokens>
ztu_ic parser<Types, Tokens...>::parser() : tokens() {};


template<typename Types, token_concept<Types>... Tokens>
ztu_ic void parser<Types, Tokens...>::parser::accept() {
	tokens >> std::tie(token, location);
}

template<typename Types, token_concept<Types>... Tokens>
ztu_ic parser<Types, Tokens...>::token_instance_t parser<Types, Tokens...>::accept(Types type) {
	if (token.type() != type) {
		throw std::runtime_error(
			std::format(
				"Expected token '{}' but got token '{}' at position ({}, {})",
				token_instance_t::name_of(type),
				token_instance_t::name_of(token.type()),
				location.row,
				location.column
			)
		);
	}
	auto old_token = std::move(token);
	if (old_token.type() != tokens::type::END) {
		accept();
	}
	return old_token;
}

template<typename Types, token_concept<Types>... Tokens>
ztu_ic parser<Types, Tokens...>::parser(std::istream &is) : tokens(is) {
	accept();
};

template<typename Types, token_concept<Types>... Tokens>
ztu_ic void parser<Types, Tokens...>::set_is(std::istream &is) {
	tokens.set_is(is);
	accept();
}


template<typename Types, token_concept<Types>... Tokens>
class serializer {
private:
	using token_instance_t = token_instance<Types, Tokens...>;

public:
	template<typename... Args>
	ztu_ic serializer(Args&&... args);

	ztu_nic std::ostream*& os();
	ztu_nic std::ostream*const& os() const;

	ztu_nic detokenizer_format& format();
	ztu_nic const detokenizer_format& format() const;

protected:
	using enum tokens::type;

	ztu_ic static auto
		lbrace = default_token_instance::make<LBRACE>(),
		rbrace = default_token_instance::make<RBRACE>(),
		lbracket = default_token_instance::make<LBRACKET>(),
		rbracket = default_token_instance::make<RBRACKET>(),
		colon = default_token_instance::make<COLON>(),
		comma = default_token_instance::make<COMMA>(),
		null = default_token_instance::make<UNDEFINED>(),
		end = default_token_instance::make<END>();

	ztu_nic static default_token_instance boolean(bool b);

	ztu_nic static default_token_instance string(std::string_view s);

	ztu_nic static default_token_instance number(double d);

protected:
	detokenizer<Types, Tokens...> tokens;
};

template<typename Types, token_concept<Types>... Tokens>
template<typename... Args>
ztu_ic serializer<Types, Tokens...>::serializer(Args&&... args) : tokens{ std::forward<Args>(args)...} {}

template<typename Types, token_concept<Types>... Tokens>
ztu_nic std::ostream*& serializer<Types, Tokens...>::os() {
	return tokens.os();
}

template<typename Types, token_concept<Types>... Tokens>
ztu_nic std::ostream*const& serializer<Types, Tokens...>::os() const {
	return tokens.os();
}

template<typename Types, token_concept<Types>... Tokens>
ztu_nic detokenizer_format& serializer<Types, Tokens...>::format() {
	return tokens.format();
}

template<typename Types, token_concept<Types>... Tokens>
ztu_nic const detokenizer_format& serializer<Types, Tokens...>::format() const {
	return tokens.format();
}

template<typename Types, token_concept<Types>... Tokens>
ztu_nic default_token_instance serializer<Types, Tokens...>::boolean(const bool b) {
	return default_token_instance::make<BOOLEAN>(b);
}

template<typename Types, token_concept<Types>... Tokens>
ztu_nic default_token_instance serializer<Types, Tokens...>::string(const std::string_view s) {
	return default_token_instance::make<STRING>(s);
}

template<typename Types, token_concept<Types>... Tokens>
ztu_nic default_token_instance serializer<Types, Tokens...>::number(const double d) {
	return default_token_instance::make<NUMBER>(d);
}


using default_base_parser = parser<tokens::type,
	tokens::end,
	tokens::left_brace,
	tokens::right_brace,
	tokens::comma,
	tokens::colon,
	tokens::string,
	tokens::number,
	tokens::boolean,
	tokens::left_bracket,
	tokens::right_bracket,
	tokens::null
>;

using default_base_serializer = serializer<tokens::type,
	tokens::end,
	tokens::left_brace,
	tokens::right_brace,
	tokens::comma,
	tokens::colon,
	tokens::string,
	tokens::number,
	tokens::boolean,
	tokens::left_bracket,
	tokens::right_bracket,
	tokens::null
>;

} // namespace transcoding


// exports
namespace ztu::json {
	using format = json_internal::transcoding::detokenizer_format;
	namespace formats = json_internal::transcoding::detokenizer_formats;
}

#endif


#ifndef ZTU_JSON_SAFE_DETAIL
#define ZTU_JSON_SAFE_DETAIL

namespace ztu::json_safe_internal {

using namespace json_internal;

template<size_t NumKeys>
class string_indexer {
private:
	struct index_type {
		unsigned int hash;
		size_t index;

		ztu_nic auto operator<=>(const index_type &other) const {
			return hash <=> other.hash;
		}

		ztu_nic auto operator==(const index_type &other) const {
			return hash == other.hash;
		}

		ztu_nic auto operator<=>(const unsigned other_hash) const {
			return hash <=> other_hash;
		}
	};

	ztu_nic static unsigned hash(std::span<const char> str);

public:
	template<typename... Ts>
		requires (sizeof...(Ts) == NumKeys)
	consteval explicit string_indexer(const Ts&... keys) noexcept;

	ztu_nic std::optional<size_t> index_of(std::span<const char> str) const;

	ztu_nic std::optional<std::string_view> name_of(size_t index) const;

private:
	std::array<index_type, NumKeys> m_lookup{};
	std::array<std::string_view, NumKeys> m_keys{};
};

template<size_t NumKeys>
ztu_nic unsigned string_indexer<NumKeys>::hash(std::span<const char> str) {

	unsigned prime = 0x1000193;
	unsigned hashed = 0x811c9dc5;

	for (const auto &c : str) {
		hashed = hashed ^ c;
		hashed *= prime;
	}

	return hashed;
}

template<size_t NumKeys>
template<typename... Ts> requires (sizeof...(Ts) == NumKeys)
consteval string_indexer<NumKeys>::string_indexer(const Ts&... keys) noexcept {

	for_each::indexed_argument([&]<auto Index>(const auto &key) {
		// Since std::string_view does only truncate the '\0' of strings in the 'const char*' constructor
		// and does not deem otherwise equal views of truncated and untruncated strings equal,
		// all strings need to be truncated before constructing the view.
		const auto begin = std::begin(key), end = std::end(key);
		m_keys[Index] = { begin, std::find(begin, end, '\0') };
		m_lookup[Index] = { hash(m_keys[Index] ), Index };
		return false;
	}, keys...);

	std::sort(m_lookup.begin(), m_lookup.end());

	auto it = m_lookup.begin();
	while ((it = std::adjacent_find(it, m_lookup.end())) != m_lookup.end()) {
		const auto match = it->hash;
		for (auto it_a = it + 1; it_a != m_lookup.end() && it_a->hash == match; it_a++) {
			const auto &key_a = m_keys[it_a->index];
			for (auto it_b = it; it_b != it_a; it_b++) {
				const auto &key_b = m_keys[it_b->index];
				if (key_a == key_b) {
					throw std::logic_error("Duplicate keys");
				}
			}
		}
	}
}

template<size_t NumKeys>
ztu_nic std::optional<size_t> string_indexer<NumKeys>::index_of(std::span<const char> str) const {
	const auto sv = std::string_view(str.begin(), std::find(str.begin(), str.end(), '\0'));

	const auto hashed = hash(sv);
	const auto it = std::lower_bound(m_lookup.begin(), m_lookup.end(), hashed);

	if (it == m_lookup.end() or hashed != it->hash)
		return std::nullopt;

	do [[unlikely]] {
		const auto candidate_index = it->index;
		if (m_keys[candidate_index] == sv) [[likely]] {
			return candidate_index;
		}
	} while (it < m_lookup.end() && it->hash == hashed);

	return std::nullopt;
}

template<size_t NumKeys>
ztu_nic std::optional<std::string_view> string_indexer<NumKeys>::name_of(size_t index) const {
	if (index < NumKeys) {
		return m_keys[index];
	} else {
		return std::nullopt;
	}
}

template<string_literal... Values>
class named_enum {
private:
	static constexpr auto indexer = string_indexer<sizeof...(Values)>(Values...);

public:
	ztu_ic named_enum();

	ztu_ic explicit named_enum(size_t n_index);

	ztu_nic static size_t index_of(std::string_view name);

	ztu_nic static size_t size();

	ztu_nic size_t index() const;

	ztu_nic std::string_view name() const;

private:
	size_t m_index{ 0 };
};

template<string_literal... Values>
ztu_ic named_enum<Values...>::named_enum() = default;

template<string_literal... Values>
ztu_ic named_enum<Values...>::named_enum(size_t n_index)
	: m_index{ n_index } {};

template<string_literal... Values>
ztu_nic size_t named_enum<Values...>::index_of(std::string_view name) {
	return indexer.index_of(name);
}

template<string_literal... Values>
ztu_nic size_t named_enum<Values...>::size() {
	return sizeof...(Values);
}

template<string_literal... Values>
ztu_nic size_t named_enum<Values...>::index() const {
	return m_index;
}

template<string_literal... Values>
ztu_nic std::string_view named_enum<Values...>::name() const {
	return indexer.name_of(m_index).value();
}


/**
 * @brief Holds a string key and a type.
 * @tparam Key	The string key to be stored.
 * @tparam Type	The type to be stored.
 */
template<string_literal Key, typename Type>
struct named_type {
	static constexpr auto key = Key;
	using type = Type;
};


/**
 * @brief 			A minimal wrapper around @c std::tuple to access elements via names.
 * @tparam Elements	The key type pairs to use.
 */
template<named_type... Elements>
class named_tuple {
public:
	ztu_ic named_tuple();

	template<typename... Ts>
	ztu_ic named_tuple(Ts&&... args);

	ztu_nic static auto index_of(std::string_view name);

	ztu_nic static auto name_of(size_t index);

	ztu_nic static size_t size();

	template<size_t Index>
	ztu_nic auto &get();
	template<size_t Index>
	ztu_nic const auto &get() const;

	template<string_literal Name>
	ztu_nic auto &get();
	template<string_literal Name>
	ztu_nic const auto &get() const;

	template<class F>
	ztu_ic void apply(F &&visitor);
	template<class F>
	ztu_ic void apply(F &&visitor) const;

private:
	static constexpr auto indexer = string_indexer<sizeof...(Elements)>(decltype(Elements)::key...);

	template<string_literal Key, typename Type>
	struct named_value {
		static constexpr auto key = Key;
		Type value;
	};

	using value_tuple = std::tuple<
		named_value<decltype(Elements)::key, typename decltype(Elements)::type>...
	>;

private:
	value_tuple elements{};
};

template<named_type... Elements>
ztu_ic named_tuple<Elements...>::named_tuple() = default;

template<named_type... Elements>
template<typename... Ts>
ztu_ic named_tuple<Elements...>::named_tuple(Ts&&... args)
	: elements{ { std::forward<Ts>(args) }... } {}

template<named_type... Elements>
ztu_nic auto named_tuple<Elements...>::index_of(std::string_view name) {
	return indexer.index_of(name);
}

template<named_type... Elements>
ztu_nic auto named_tuple<Elements...>::name_of(size_t index) {
	return indexer.name_of(index);
}

template<named_type... Elements>
ztu_nic size_t named_tuple<Elements...>::size() {
	return sizeof...(Elements);
}

template<named_type... Elements>
template<size_t Index>
ztu_nic auto& named_tuple<Elements...>::get() {
	return std::get<Index>(elements).value;
}

template<named_type... Elements>
template<size_t Index>
ztu_nic const auto& named_tuple<Elements...>::get() const {
	return std::get<Index>(elements).value;
}

template<named_type... Elements>
template<string_literal Name>
ztu_nic auto& named_tuple<Elements...>::get() {
	constexpr auto index_opt = indexer.index_of(Name);
	static_assert(index_opt, "Unknown key");
	return get<*index_opt>();
}

template<named_type... Elements>
template<string_literal Name>
ztu_nic const auto& named_tuple<Elements...>::get() const {
	constexpr auto index_opt = indexer.index_of(Name);
	static_assert(index_opt, "Unknown key");
	return get<*index_opt>();
}

template<named_type... Elements>
template<class F>
ztu_ic void named_tuple<Elements...>::apply(F &&visitor) {
	std::apply(visitor, elements);
}

template<named_type... Elements>
template<class F>
ztu_ic void named_tuple<Elements...>::apply(F &&visitor) const {
	std::apply(visitor, elements);
}


template<string_literal Name>
struct in_place_name_t {
	static constexpr auto name = Name;
};

/**
 * @brief		Can be passed as first argument to @c named_variant to specify the name
 * @tparam Name	The name to be used for construction.
 */
template<string_literal Name>
static constexpr auto in_place_name = in_place_name_t<Name>{};

/**
 * @brief		Can be passed as first argument to @c named_variant to specify the dynamic type
 */
struct in_place_dynamic_index {
	size_t value;
};

/**
 * @brief	A minimal wrapper around @c std::variant to access alternatives via names.
 * @tparam Alternatives	The name value pairs to construct the variant.
 */
template<named_type... Alternatives>
class named_variant {
public:
	ztu_ic named_variant();
	template<typename... Args>
	ztu_ic explicit named_variant(Args&&... args);
	template<string_literal Name, typename... Args>
	ztu_ic explicit named_variant(in_place_name_t<Name>, Args&&... args);
	template<typename... Args>
	ztu_ic explicit named_variant(in_place_dynamic_index in_place_index, Args&&... args);
	template<string_literal Name, typename... Args>
	ztu_nic static named_variant<Alternatives...> make(Args&&... args);

	ztu_nic static auto index_of(std::span<const char> name);

	ztu_nic static auto name_of(size_t index);

	ztu_nic size_t index() const;

	ztu_nic static size_t size();

	template<string_literal Name, typename... Args>
	ztu_ic void emplace(Args&&... args);

	template<size_t Index>
	ztu_nic auto &get();
	template<size_t Index>
	ztu_nic const auto &get() const;

	template<string_literal Name>
	ztu_nic auto &get();
	template<string_literal Name>
	ztu_nic const auto &get() const;

	template<string_literal Name>
	ztu_nic auto *get_if();
	template<string_literal Name>
	ztu_nic const auto *get_if() const;

	template<class F>
	ztu_nic decltype(auto) visit(F &&visitor);
	template<class F>
	ztu_nic decltype(auto) visit(F &&visitor) const;

private:

	static constexpr auto indexer = string_indexer<sizeof...(Alternatives)>(
		decltype(Alternatives)::key...
	);

	template<string_literal Key, typename Type>
	struct named_value {

		template<typename... Args>
		constexpr named_value(Args&&... args)
			: value{ std::forward<Args>(args)... } {}

		static constexpr auto key = Key;
		static constexpr auto index = indexer.index_of(Key).value();

		Type value;
	};

	using value_variant = std::variant<
		named_value<decltype(Alternatives)::key, typename decltype(Alternatives)::type>...
	>;

private:
	value_variant alternatives{};
};

template<named_type... Alternatives>
ztu_ic named_variant<Alternatives...>::named_variant() = default;

template<named_type... Alternatives>
template<typename... Args>
ztu_ic named_variant<Alternatives...>::named_variant(Args&&... args)
	: alternatives(std::forward<Args>(args)...) {}

template<named_type... Alternatives>
template<string_literal Name, typename... Args>
ztu_ic named_variant<Alternatives...>::named_variant(in_place_name_t<Name>, Args&&... args)
	: alternatives(std::in_place_index<*indexer.index_of(Name)>, std::forward<Args>(args)...) {}

template<named_type... Alternatives>
template<typename... Args>
ztu_ic named_variant<Alternatives...>::named_variant(in_place_dynamic_index in_place_index, Args&&... args) {
	for_each::index<sizeof...(Alternatives)>([&]<auto Index>() {
		if (in_place_index.value == Index) {
			alternatives.template emplace<Index>(std::forward<Args>(args)...);
			return true;
		}
		return false;
	});
}

template<named_type... Alternatives>
template<string_literal Name, typename... Args>
ztu_nic named_variant<Alternatives...> named_variant<Alternatives...>::make(Args&&... args) {
	return named_variant<Alternatives...>(in_place_name<Name>, std::forward<Args>(args)...);
}

template<named_type... Alternatives>
ztu_nic auto named_variant<Alternatives...>::index_of(std::span<const char> name) {
	return indexer.index_of(name);
}

template<named_type... Alternatives>
ztu_nic auto named_variant<Alternatives...>::name_of(size_t index) {
	return indexer.name_of(index);
}

template<named_type... Alternatives>
ztu_nic size_t named_variant<Alternatives...>::index() const {
	return alternatives.index();
}

template<named_type... Alternatives>
ztu_nic size_t named_variant<Alternatives...>::size() {
	return sizeof...(Alternatives);
}

template<named_type... Alternatives>
template<string_literal Name, typename... Args>
ztu_ic void named_variant<Alternatives...>::emplace(Args&&... args) {
	constexpr auto index_opt = indexer.index_of(Name);
	static_assert(index_opt, "Unknown key");
	alternatives.template emplace<*index_opt>(std::forward<Args>(args)...);
}

template<named_type... Alternatives>
template<size_t Index>
ztu_nic auto& named_variant<Alternatives...>::get() {
	return std::get<Index>(alternatives).value;
}

template<named_type... Alternatives>
template<size_t Index>
ztu_nic const auto& named_variant<Alternatives...>::get() const {
	return std::get<Index>(alternatives).value;
}

template<named_type... Alternatives>
template<string_literal Name>
ztu_nic auto& named_variant<Alternatives...>::get() {
	constexpr auto index_opt = indexer.index_of(Name);
	static_assert(index_opt, "Unknown key");
	return get<*index_opt>();
}

template<named_type... Alternatives>
template<string_literal Name>
ztu_nic const auto& named_variant<Alternatives...>::get() const {
	constexpr auto index_opt = indexer.index_of(Name);
	static_assert(index_opt, "Unknown key");
	return get<*index_opt>();
}

template<named_type... Alternatives>
template<string_literal Name>
ztu_nic auto* named_variant<Alternatives...>::get_if() {
	constexpr auto index_opt = indexer.index_of(Name);
	static_assert(index_opt, "Unknown key");
	const auto ptr = std::get_if<*index_opt>(&alternatives);
	return ptr ? &ptr->value : nullptr;
}

template<named_type... Alternatives>
template<string_literal Name>
ztu_nic const auto* named_variant<Alternatives...>::get_if() const {
	constexpr auto index_opt = indexer.index_of(Name);
	static_assert(index_opt, "Unknown key");
	const auto ptr = std::get_if<*index_opt>(&alternatives);
	return ptr ? &ptr->value : nullptr;
}

template<named_type... Alternatives>
template<class F>
ztu_nic decltype(auto) named_variant<Alternatives...>::visit(F &&visitor) {
	return std::visit([&visitor](auto &value) {
		visitor(value);
	}, alternatives);
}

template<named_type... Alternatives>
template<class F>
ztu_nic decltype(auto) named_variant<Alternatives...>::visit(F &&visitor) const {
	return std::visit([&visitor](const auto& value) {
		visitor(value);
	}, alternatives);
}

} // namespace ztu::json_safe_internal

#endif


namespace ztu::json::safe {

enum class types {
	BOOLEAN, NUMBER, STRING,
	ARRAY, OBJECT, VARIANT, ADAPTER,
	INVALID
};

namespace data_types {

using boolean = bool;
using number = double;
using string = std::string;

template<typename ElementType>
using array = std::vector<ElementType>;

template<ztu::json_safe_internal::named_type... Elements>
using object = ztu::json_safe_internal::named_tuple<Elements...>;

template<ztu::json_safe_internal::named_type... Alternatives>
using varaint = ztu::json_safe_internal::named_variant<Alternatives...>;

} // namespace data_types

namespace default_types_internal {

using enum types;

//----------------------[ default types to enum ]----------------------//

template<types EnumType>
struct type_container {
	static constexpr auto value = EnumType;
};

template<typename T>
struct get_default_value_type {
	static constexpr auto value = INVALID;
};

} // namespace default_types_internal

template<typename T>
constexpr auto type_of_default(const T &) {
	return default_types_internal::get_default_value_type<std::remove_cvref_t<T>>::value;
}

//----------------------[ default types default_concepts ]----------------------//

namespace default_concepts {

template<class T>
concept type = default_types_internal::get_default_value_type<std::remove_cvref_t<T>>::value != types::INVALID;

template<class T, types EnumType>
concept of_type = default_types_internal::get_default_value_type<std::remove_cvref_t<T>>::value == EnumType;

template<class T>
concept boolean = of_type<T, types::BOOLEAN>;

template<class T>
concept number = of_type<T, types::NUMBER>;

template<class T>
concept string = of_type<T, types::STRING>;

template<class T>
concept array = of_type<T, types::ARRAY>;

template<class T>
concept object = of_type<T, types::OBJECT>;

template<class T>
concept variant = of_type<T, types::VARIANT>;

template<class T>
concept adapter = of_type<T, types::ADAPTER>;

template<class T>
concept primitive = (
		boolean<T> || number<T> || string<T>
);

template<class T>
concept compounds = (
		array<T> || object<T> || variant<T> || adapter<T>
);

} // namespace default_concepts

namespace default_types_internal {

template<typename LiteralType, typename Type>
	requires(std::is_nothrow_convertible_v<LiteralType, Type>)
struct primitive_default_t {
	using type = Type;

	LiteralType value;

	ztu_nic type operator()() const {
		return static_cast<type>(value);
	}
};



//----------------------[ data_types ]----------------------//

struct boolean : default_types_internal::primitive_default_t<bool, data_types::boolean> {};
template<> struct get_default_value_type<boolean> : public type_container<BOOLEAN> {};

struct number : default_types_internal::primitive_default_t<double, data_types::number> {};
template<> struct get_default_value_type<number> : public type_container<NUMBER> {};

template<size_t Length>
struct string {
	using type = std::string;

	ztu_ic string(const char (&str)[Length]) {
		std::copy_n(str, Length, value);
	}

	ztu_ic string(const ztu::json_internal::string_literal<Length> &str) {
		std::copy(str.begin(), str.end(), value);
	}

	ztu_nic auto size() const {
		return Length - 1;
	}

	ztu_nic type operator()() const {
		return { value };
	}

	char value[Length]{};
};

template<size_t Length>
struct get_default_value_type<string<Length>> : public type_container<STRING> {};

//----------------------[ Array ]----------------------//

template<default_concepts::type auto DefaultElement, default_concepts::type auto... DefaultElements>
	requires (
		std::same_as<
			typename decltype(DefaultElement)::type,
			typename decltype(DefaultElements)::type
		> && ...
	)
struct array {
	static constexpr auto default_element = DefaultElement;
	using element_type = decltype(default_element)::type;
	using type = std::vector<element_type>;

	ztu_nic type operator()() const {
		return { DefaultElement(), DefaultElements()... };
	}
};


template<default_concepts::type auto ElementType, auto... DefaultValues>
struct get_default_value_type<array<ElementType, DefaultValues...>> : public type_container<ARRAY> {};


//----------------------[ Object ]----------------------//

template<json_internal::string_literal Key, default_concepts::type auto Value>
struct named_entry {
	static constexpr auto key = Key;
	static constexpr auto value = Value;
	using value_type = decltype(Value);
};

template<named_entry... Entries>
struct object {
	using type = data_types::object<
		ztu::json_safe_internal::named_type<
			decltype(Entries)::key,
			typename decltype(Entries)::value_type::type
		>{}...
	>;

	template<size_t Index>
	[[nodiscard]] inline consteval auto get() const {
		auto find_element = []<size_t StepsLeft, named_entry First, named_entry...Rest>(auto&& self) {
			if constexpr (StepsLeft == 0) {
				return First.value;
			} else if constexpr (StepsLeft > 0 && sizeof...(Rest) > 0) {
				return self.template operator()<StepsLeft - 1, Rest...>(self);
			} else {
				decltype(First)::__index_out_of_range;
			}
		};
		return find_element.template operator()<Index, Entries...>(find_element);
	}

	template<ztu::json_internal::string_literal Name>
	[[nodiscard]] inline consteval auto get() const {
		constexpr auto index_opt = type::index_of(Name);
		static_assert(index_opt, "Unknown key");
		return get<*index_opt>();
	}

	[[nodiscard]] inline type operator()() const {
		return type(decltype(Entries)::value()...);
	}
};

template<named_entry... Entries>
struct get_default_value_type<object<Entries...>> : public type_container<OBJECT> {};


//----------------------[ Variant ]----------------------//

template<ztu::json_internal::string_literal Key, named_entry... Entries>
struct named_alternative {
	static constexpr auto key = Key;
	static constexpr auto alternative = object<Entries...>{};
	using alternative_type = decltype(alternative);
};

template<string DefaultKey, named_alternative... Alternatives>
	requires ( (json_internal::string_literal(DefaultKey.value) == decltype(Alternatives)::key) || ... )
struct variant {
	using type = data_types::varaint<
		ztu::json_safe_internal::named_type<
			decltype(Alternatives)::key,
			typename decltype(Alternatives)::alternative_type::type
		>{}...
	>;

	// Most interfaces were designed for 'string_literal', so this copy is needed
	static constexpr auto default_literal = json_internal::string_literal(DefaultKey.value);

	template<size_t Index>
	[[nodiscard]] inline static consteval auto get() {
		auto find_element = []<size_t StepsLeft, named_alternative First, named_alternative...Rest>(auto&& self) {
			if constexpr (StepsLeft == 0) {
				return First.alternative;
			} else if constexpr (StepsLeft > 0 && sizeof...(Rest) > 0) {
				return self.template operator()<StepsLeft - 1, Rest...>(self);
			} else {
				decltype(First)::__unknown_name;
			}
		};
		return find_element.template operator()<Index, Alternatives...>(find_element);
	}

	template<ztu::json_internal::string_literal Name>
	[[nodiscard]] inline static consteval auto get() {
		constexpr auto index_opt = type::index_of(Name);
		static_assert(index_opt, "Unknown key");
		return get<*index_opt>();
	}

	[[nodiscard]] inline type operator()() const {
		constexpr auto default_alternative = get<default_literal>();
		return type::template make<default_literal>(default_alternative() );
	}
};


template<string DefaultKey, named_alternative... Alternatives>
struct get_default_value_type<variant<DefaultKey, Alternatives...>> : public type_container<VARIANT> {};


//----------------------[ Adapter ]----------------------//


template<class T, typename X, typename Y>
concept two_way_converter = requires(const X x, const Y y) {
	{ T::convert(x) } -> std::same_as<std::optional<Y>>;
	{ T::revert(y)  } -> std::same_as<std::optional<X>>;
};


template<
	default_concepts::type default_t, typename runtime_t,
	default_types_internal::two_way_converter<typename default_t::type, runtime_t> converter
>
struct adapter {
	using type = runtime_t;
	using json_t = default_t::type;

	default_t default_value;

	ztu_nic runtime_t operator()() const {
		return *converter::convert(default_value());
	}

	ztu_nic runtime_t convert(const json_t &json_value) const {
		auto value = converter::convert(json_value);
		if (value) {
			return *value;
		} else {
			return *converter::convert(default_value());
		}
	}

	ztu_nic json_t revert(const runtime_t &from) const {
		auto value = converter::revert(from);
		if (value) {
			return *value;
		} else {
			return default_value();
		}
	}
};

template<
	default_concepts::type default_t, typename runtime_t,
	two_way_converter<typename default_t::type, runtime_t> converter
>
struct get_default_value_type<adapter<default_t, runtime_t, converter>> : public type_container<ADAPTER> {};


//----------------------[ Default Adapters ]----------------------//

struct number_unsigned_integer_converter {
	using x_t = data_types::number;
	using y_t = unsigned long long;

	static std::optional<y_t> convert(const x_t &x) {
		y_t y = static_cast<y_t>(std::round(x));

		const auto delta = static_cast<double>(y) - x;
		constexpr auto epsilon = std::numeric_limits<double>::epsilon();

		if (x >= 0 and std::abs(delta) < epsilon) {
			return y;
		} else {
			return std::nullopt;
		}
	};

	static std::optional<x_t> revert(const y_t &y) {
		return { static_cast<double>(y) };
	};
};

using unsigned_integer = adapter<number, unsigned long long, default_types_internal::number_unsigned_integer_converter>;

template<>
struct get_default_value_type<unsigned_integer> : public type_container<ADAPTER> {};

	struct number_integer_converter {
		using x_t = data_types::number;
		using y_t = long long;

		static std::optional<y_t> convert(const x_t &x) {
			y_t y = static_cast<y_t>(std::round(x));

			const auto delta = static_cast<double>(y) - x;
			constexpr auto epsilon = std::numeric_limits<double>::epsilon();

			if (std::abs(delta) < epsilon) {
				return y;
			} else {
				return std::nullopt;
			}
		};

		static std::optional<x_t> revert(const y_t &y) {
			return { static_cast<double>(y) };
		};
	};

	using integer_adapter = adapter<number, long long, number_integer_converter>;


struct integer : public default_types_internal::integer_adapter {
	constexpr integer operator -() {
		return integer{ -default_value.value };
	}
};

template<>
struct get_default_value_type<integer> : public type_container<ADAPTER> {};

template<ztu::json_internal::string_literal... Values>
struct string_enum_converter {
private:
	static constexpr auto indexer = json_safe_internal::string_indexer<sizeof...(Values)>(Values...);
public:
	using x_t = data_types::string;
	using y_t = unsigned short;

	static  std::optional<y_t> convert(const x_t &x) {
		const auto index_opt = indexer.index_of(x);
		if (index_opt) {
			return static_cast<y_t>(*index_opt);
		} else {
			return std::nullopt;
		}
	};

	static std::optional<x_t> revert(const y_t &y) {
		const auto name_opt = indexer.name_of(y);
		if (name_opt) {
			return x_t{ *name_opt };
		} else {
			return std::nullopt;
		}
	};
};

template<size_t StrLen, ztu::json_internal::string_literal... Values>
using enum_adapter = adapter<string<StrLen>, unsigned short, string_enum_converter<Values...>>;


template<string DefaultValue, ztu::json_internal::string_literal... Values>
struct enumeration : default_types_internal::enum_adapter<DefaultValue.size() + 1, Values...> {
	constexpr enumeration() : default_types_internal::enum_adapter<DefaultValue.size() + 1, Values...>{DefaultValue }{}
};

template<string DefaultValue, ztu::json_internal::string_literal... Values>
struct get_default_value_type<enumeration<DefaultValue, Values...>> : public type_container<ADAPTER> {};

} // namespace default_types_internal

namespace default_types {

using default_types_internal::boolean;
using default_types_internal::number;
using default_types_internal::string;
using default_types_internal::array;

using default_types_internal::named_entry;
using default_types_internal::object;
using default_types_internal::named_alternative;
using default_types_internal::variant;

using default_types_internal::adapter;
using default_types_internal::integer;
using default_types_internal::unsigned_integer;
using default_types_internal::enumeration;

} // namespace default_types

namespace literals {
	using namespace default_types;

	constexpr boolean operator""_B(unsigned long long int value) {
		return { static_cast<bool>(value) };
	}

	static constexpr boolean false_B { false };
	static constexpr boolean true_B { true };

	constexpr number operator""_N(long double value) {
		return { static_cast<double>(value) };
	}

	template<string Str>
	constexpr auto operator""_S() {
		return Str;
	}

	constexpr integer operator""_I(unsigned long long i) {
		return { number{ static_cast<double>(i) } };
	}

	constexpr unsigned_integer operator""_U(unsigned long long u) {
		return { number{ static_cast<double>(u) } };
	}

	template<json_internal::string_literal Key, auto Value>
	using set = named_entry<Key, Value>;

	template<json_internal::string_literal Key, named_entry... Entries>
	using holds = named_alternative<Key, Entries...>;

} // namespace literals


/**
 * @brief A parser for static/safe json values.
 *
 * @note The parser is not thread safe.
 */
class parser : public json_internal::transcoding::default_base_parser {
public:
	using json_internal::transcoding::default_base_parser::parser;

	/**
	 * @brief	Parses json structures fom the input stream and returns resulting value.
	 *
	 * This function template results in a custom parser based on the given @c DefaultType
	 * that can replace missing or faulty parts in the json input stream with the specified default values.
	 *
	 * @tparam DefaultType	The json structure and default values the parser is based on.
	 *
	 * @return The parsed json data structure.
	 */
	template<default_concepts::type auto DefaultType>
	inline decltype(DefaultType)::type parse();

private:
	using token_type = json_internal::transcoding::tokens::type;

	template<default_concepts::primitive auto DefaultPrimitive, token_type Type>
	inline decltype(auto) parse_primitive();

	template<default_concepts::array auto DefaultArray>
	inline decltype(DefaultArray)::type parse_array();

	template<default_concepts::variant auto DefaultVariant>
	inline decltype(DefaultVariant)::type parse_variant();

	template<default_concepts::object auto DefaultObject>
	inline decltype(DefaultObject)::type parse_object();

	template<default_concepts::type auto DefaultValue, types Type>
	inline decltype(DefaultValue)::type parse_value();

	template<default_concepts::object auto DefaultObject, size_t Size>
	inline void parse_entry(std::bitset<Size> &is_set, typename decltype(DefaultObject)::type &object);

	template<default_concepts::object auto DefaultObject>
	inline void parse_entries(typename decltype(DefaultObject)::type &object, bool inside = false);

	inline void skip_primitive();
	inline void skip_array();
	inline void skip_object(bool inside = false);
	inline void skip_value();
};

inline void parser::skip_primitive() {
	accept();
}

inline void parser::skip_array() {
	using enum token_type;

	accept(LBRACKET);

	int open_brackets = 1;
	do {
		const auto closed = tokens.ignore_until<LBRACKET, RBRACKET>();
		open_brackets += closed ? -1 : 1;
	} while (open_brackets);

	accept();
}

inline void parser::skip_object(bool inside) {
	using enum token_type;

	if (not inside) {
		accept(LBRACE);
	}

	int open_braces = 1;
	do {
		const auto closed = tokens.ignore_until<LBRACE, RBRACE>();
		open_braces += closed ? -1 : 1;
	} while (open_braces != 0);

	accept();
}

inline void parser::skip_value() {
	switch (token.type()) {
		using enum token_type;
		case LBRACE:
			return skip_object();
		case LBRACKET:
			return skip_array();
		case STRING:
		case NUMBER:
		case BOOLEAN:
		case UNDEFINED:
			return skip_primitive();
		default: {
			throw std::runtime_error(
				"Unexpected token_concept"
			);
		}
	}
}


template<default_concepts::primitive auto DefaultPrimitive, parser::token_type Type>
inline decltype(auto) parser::parse_primitive() {

	using default_primitive_t = decltype(DefaultPrimitive);
	using primitive_t = typename default_primitive_t::type;

	primitive_t primitive(token.get<Type>());

	accept();

	return primitive;
}

template<default_concepts::array auto DefaultArray>
inline decltype(DefaultArray)::type parser::parse_array() {
	using enum token_type;

	using default_array_t = decltype(DefaultArray);
	typename default_array_t::type array;

	accept();

	if (token.type() != RBRACKET) {
		array.push_back(parse<default_array_t::default_element>());
		while (token.type() != RBRACKET) {
			accept(COMMA);
			array.push_back(parse<default_array_t::default_element>());
		}
	}
	accept(RBRACKET);

	return array;
}


template<default_concepts::object auto DefaultObject, size_t Size>
inline void parser::parse_entry(std::bitset<Size> &is_set, typename decltype(DefaultObject)::type &object) {
	using enum token_type;

	using default_object_t = decltype(DefaultObject);
	using object_t = typename default_object_t::type;

	const auto key = accept(STRING).get<STRING>();
	// Lifetime of key ends with next accept so index_of needs to be done here
	const auto index_opt = object_t::index_of(key);

	accept(COLON);

	if (index_opt and not is_set[*index_opt]) {
		is_set[*index_opt] = true;
		// inline a switch for the correct member default value
		json_internal::for_each::index<Size>([&]<size_t Index>() {
			if (Index == *index_opt) {
				constexpr auto default_member = DefaultObject.template get<Index>();
				object.template get<Index>() = parse<default_member>();
				return true;
			}
			return false;
		});
	} else {
		// key is not specified in default object and can be ignored
		skip_value();
	}
}

template<default_concepts::object auto DefaultObject>
inline void parser::parse_entries(typename decltype(DefaultObject)::type &object, bool inside) {
	using enum token_type;

	using default_object_t = decltype(DefaultObject);
	using object_t = default_object_t::type;

	constexpr auto num_entries = object_t::size();

	// Since entries need to be parsed in the same order as the json file,
	// which might not match the order of entries in the default object,
	// every parsed key needs to be matched to a default entry and marked in 'is_set'
	std::bitset<num_entries> is_set{};

	if (not inside and token.type() != RBRACE) {
		parse_entry<DefaultObject>(is_set, object);
	}

	while (token.type() != RBRACE) {
		accept(COMMA);
		parse_entry<DefaultObject>(is_set, object);
	}

	// All entries that were not provided by the json file need to be filled in with default values
	json_internal::for_each::index<num_entries>([&]<size_t Index>() {
		if (not is_set[Index]) {
			constexpr auto default_member = DefaultObject.template get<Index>();
			object.template get<Index>() = default_member();
			// is_set[Index] = true;
		}
		return false;
	});

}

template<default_concepts::object auto DefaultObject>
inline decltype(DefaultObject)::type parser::parse_object() {
	using enum token_type;

	accept();

	using default_object_t = decltype(DefaultObject);
	using object_t = typename default_object_t::type;
	object_t object;

	parse_entries<DefaultObject>(object);
	accept(RBRACE);

	return object;
}

template<default_concepts::variant auto DefaultVariant>
inline decltype(DefaultVariant)::type parser::parse_variant() {
	using enum token_type;

	using default_variant_t = decltype(DefaultVariant);
	using variant_t = typename default_variant_t::type;

	accept();

	// variant needs to begin with "TYPE_LITERAL": "ENUN_VALUE"
	size_t type_index = SIZE_MAX;
	if (token.type() == STRING) {

		auto type_key = token.get<STRING>();
		const auto type_key_correct = type_key == json_internal::literal_config::variant_type;
		accept();

		accept(COLON);

		if (type_key_correct and token.type() == STRING) {
			const auto type_value = token.get<STRING>();
			type_index = variant_t::index_of(type_value).value_or(SIZE_MAX);
			accept();
		}
	}

	if (type_index == SIZE_MAX) {
		std::cout << "Could not find types. Skipping object" << std::endl;
		skip_object(true);
		std::cout << "Done skipping" << std::endl;
		return DefaultVariant();
	}

	variant_t variant(json_safe_internal::in_place_dynamic_index{ type_index });
	constexpr auto num_alternatives = variant_t::size();

	// inline a switch statement for every possible alternative
	json_internal::for_each::index<num_alternatives>([&]<size_t Index>() {
		if (Index == type_index) {
			constexpr auto default_object = default_variant_t::template get<Index>();
			// all members need to be initialized
			parse_entries<default_object>(variant.template get<Index>(), true);
			return true;
		}
		return false;
	});

	accept(RBRACE);

	return variant;
}

template<default_concepts::type auto DefaultType, types Type>
inline decltype(DefaultType)::type parser::parse_value() {
	using enum types;
	if constexpr (Type == BOOLEAN) {
		return parse_primitive<DefaultType, token_type::BOOLEAN>();
	} else if constexpr (Type == NUMBER) {
		return parse_primitive<DefaultType, token_type::NUMBER>();
	} else if constexpr (Type == STRING) {
		return parse_primitive<DefaultType, token_type::STRING>();
	} else if constexpr (Type == ARRAY) {
		return parse_array<DefaultType>();
	} else if constexpr (Type == OBJECT) {
		return parse_object<DefaultType>();
	} else if constexpr (Type == VARIANT) {
		return parse_variant<DefaultType>();
	} else {
		decltype(DefaultType)::_error_type;
	}
}

template<default_concepts::type auto BaseDefaultType>
inline decltype(BaseDefaultType)::type parser::parse() {

	constexpr auto base_type = type_of_default(BaseDefaultType);

	constexpr auto default_type = []() {
		if constexpr (base_type == types::ADAPTER) {
			return BaseDefaultType.default_value;
		} else {
			return BaseDefaultType;
		}
	}();

	constexpr auto type = type_of_default(default_type);
	using default_t = decltype(default_type);
	using value_t = default_t::type;

	value_t value{};

	auto found_type = false;
	json_internal::for_each::value<
		std::pair{types::BOOLEAN	, token_type::BOOLEAN	},
		std::pair{types::NUMBER	, token_type::NUMBER	},
		std::pair{types::STRING	, token_type::STRING	},
		std::pair{types::ARRAY	, token_type::LBRACKET	},
		std::pair{types::OBJECT	, token_type::LBRACE	},
		std::pair{types::VARIANT	, token_type::LBRACE	}
	>([&]<auto p>() {
		if constexpr (p.first == type) {
			if (token.type() == p.second) {
				value = parse_value<default_type, p.first>();
				found_type = true;
			}
			return true;
		}
		return false;
	});

	if (not found_type) {
		skip_value();
		value = default_type();
	}

	if constexpr (base_type == types::ADAPTER) {
		return BaseDefaultType.convert(value);
	} else {
		return value;
	}
}

/**
 * @brief A serializer for static/safe json values.
 *
 * @note The serializer is not thread safe.
 */
class serializer : public json_internal::transcoding::default_base_serializer {
public:
	using json_internal::transcoding::default_base_serializer::serializer;

	/**
	 * @brief	Serializes json structures to the output stream.
	 *
	 * This function template results in a custom serializer based on the given @c DefaultType.
	 *
	 * @tparam DefaultType	The json structure and default values the serializer is based on.
	 */
	template<default_concepts::type auto DefaultValue>
	inline void serialize(const decltype(DefaultValue)::type& value);

private:
	template<default_concepts::array auto DefaultArray>
	inline void serialize_array(const decltype(DefaultArray)::type& value);

	template<default_concepts::object auto DefaultObject>
	inline void serialize_object(const decltype(DefaultObject)::type& value);

	template<default_concepts::variant auto DefaultVariant>
	inline void serialize_variant(const decltype(DefaultVariant)::type& value);

	template<default_concepts::object auto DefaultObject>
	inline void serialize_entries(const decltype(DefaultObject)::type &object, bool inside = false);

	using instructions = json_internal::transcoding::detokenizer_instructions;
};

template<default_concepts::array auto DefaultArray>
inline void serializer::serialize_array(const decltype(DefaultArray)::type& value) {
	using default_array_t = decltype(DefaultArray);
	using enum instructions;

	tokens << lbracket;
	if (value.empty()) {
		tokens << space;
	} else {
		tokens << indent << newline;
		auto it = value.begin();
		while (it != value.end()) {
			serialize<default_array_t::default_element>(*it);
			if (++it != value.end()) {
				tokens << comma << newline;
			}
		}
		tokens << outdent;
	}
	tokens << newline << rbracket;
}

template<default_concepts::object auto DefaultObject>
inline void serializer::serialize_object(const decltype(DefaultObject)::type& value) {
	using enum instructions;

	tokens << lbrace << indent << newline;
	serialize_entries<DefaultObject>(value);
	tokens << outdent << newline << rbrace;
}


template<default_concepts::variant auto DefaultVariant>
inline void serializer::serialize_variant(const decltype(DefaultVariant)::type& value) {
	using default_variant_t = decltype(DefaultVariant);
	using enum instructions;

	tokens << lbrace << indent << newline;

	value.visit([&]<typename Entry>(const Entry &entry) {
		tokens << string(json_internal::literal_config::variant_type) << colon << space << string(Entry::key.c_str());
		constexpr auto default_alternative = default_variant_t::template get<Entry::index>();
		serialize_entries<default_alternative>(entry.value, true);
	});

	tokens << outdent << newline << rbrace;
}

template<default_concepts::object auto DefaultObject>
inline void serializer::serialize_entries(const decltype(DefaultObject)::type &object, bool inside) {
	using default_object_t = decltype(DefaultObject);
	using enum instructions;

	constexpr auto size = default_object_t::type::size();
	if constexpr (size > 0) {
		if (inside) {
			tokens << comma << newline;
		}

		const auto last_index = size - 1;

		object.apply([&](const auto &... entries) {
			json_internal::for_each::indexed_argument([&]<size_t Index, typename Entry>(const Entry &entry) {
				tokens << string(Entry::key.c_str()) << colon << space;

				constexpr auto default_member = DefaultObject.template get<Index>();
				serialize<default_member>(entry.value);

				if constexpr (Index != last_index)
					tokens << comma << newline;

				return false;
			}, entries...);
		});
	}
}

template<default_concepts::type auto DefaultValue>
inline void serializer::serialize(const decltype(DefaultValue)::type& value) {
	constexpr auto default_type = type_of_default(DefaultValue);

	using token_type = json_internal::transcoding::tokens::type;
	using token_instance = json_internal::transcoding::default_token_instance;

	if constexpr (default_type == types::BOOLEAN) {
		tokens << token_instance::make<token_type::BOOLEAN>(value);
	} else if constexpr (default_type == types::NUMBER) {
		tokens << token_instance::make<token_type::NUMBER>(value);
	} else if constexpr (default_type == types::STRING) {
		tokens << token_instance::make<token_type::STRING>(value);
	} else if constexpr (default_type == types::ARRAY) {
		serialize_array<DefaultValue>(value);
	} else if constexpr (default_type == types::ADAPTER) {
		serialize<DefaultValue.default_value>(DefaultValue.revert(value));
	} else if constexpr (default_type == types::OBJECT) {
		serialize_object<DefaultValue>(value);
	} else if constexpr (default_type == types::VARIANT) {
		serialize_variant<DefaultValue>(value);
	} else {
		DefaultValue.__unknown_type;
	}
}

} // namespace ztu::json::safe
