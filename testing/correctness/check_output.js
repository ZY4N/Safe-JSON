const fs = require('fs');

const input_path = "./input";
const output_path = "./output";

function deep_compare(obj1, obj2) {
	// compare types
	if (typeof obj1 !== typeof obj2) {
		return false;
	}

	if (obj1 === undefined || obj1 === null) {
		return true;
	}

	if (typeof obj1 === 'object') {
		if (Array.isArray(obj1) !== Array.isArray(obj2)) {
			return false;
		}
		if (Array.isArray(obj1)) {
			if (obj1.length !== obj2.length) {
				return false;
			}
			for (let i = 0; i < obj1.length; i++) {
				if (!deep_compare(obj1[i], obj2[i])) {
					return false;
				}
			}
		} else {
			const keys1 = Object.keys(obj1);
			const keys2 = Object.keys(obj2);
			if (keys1.length !== keys2.length) {
				return false;
			}
			for (const key of keys1) {
				if (
					!Object.prototype.hasOwnProperty.call(obj2, key) ||
					!deep_compare(obj1[key], obj2[key])
				) {
					return false;
				}
			}
		}
	} else {
		if (obj1 !== obj2) {
			return false;
		}
	}

	return true;
}


let num_correct = 0;
let num_wrong = 0;
let num_skipped = 0;

for (const file of fs.readdirSync(output_path)) {

	if (!file.startsWith("y"))
		continue;

	const output_file = output_path + "/" + file;
	const input_file = input_path + "/" + file;


	const try_parse = (filename) => {
		try {
			return JSON.parse(fs.readFileSync(filename));
		} catch (e) {
			console.log(e);
			return undefined;
		}
	}

	const input = try_parse(input_path + "/" + file);
	if (input === undefined) {
		console.log("Skipping input: " + file);
		num_skipped++;
		continue;
	}

	const output = try_parse(output_path + "/" + file);
	if (output === undefined) {
		console.log("Cannot read output: " + file);
		num_wrong++;
	}

	if (deep_compare(input, output)) {
		console.log("correct:", file);
		num_correct++;
	} else {
		console.log("wrong:", file);
		console.log("input:", input);
		console.log("output:", output);
	}
}

console.log("\n|==========[ result ]==========|")
console.log("correct: ", num_correct);
console.log("wrong:  ", num_wrong);
console.log("skipped: ", num_skipped);
