import sys, time, json

text = open(sys.argv[1], "r").read()

decoder = json.JSONDecoder()

begin_parse = time.monotonic_ns()
value = decoder.decode(text)
end_parse = time.monotonic_ns()

begin_serialize = time.monotonic_ns()
str = json.dumps(value, indent=4)
end_serialize = time.monotonic_ns()

print((end_parse - begin_parse) / 1000.0)
print((end_serialize - begin_serialize) / 1000.0)