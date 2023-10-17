const util = require('util');
const exec = util.promisify(require('child_process').exec);

const implementations = [
    {
        name: "dynamic_json",
        compile_cmd: "g++ -std=c++20 dynamic_json.cpp -Ofast -o dynamic_json",
        run_cmd: "./dynamic_json FILE"
    },
    {
        name: "simd_json",
        compile_cmd: "g++ -std=c++20 simd_json.cpp simdjson.cpp -Ofast -o simd_json",
        run_cmd: "./simd_json FILE"
    },
    {
        name: "java_json",
        compile_cmd: "javac -cp .:json-java.jar OrgJson.java",
        run_cmd: "java -cp .:json-java.jar OrgJson FILE"
    },
    {
        name: "node_json",
        compile_cmd: "/usr/bin/true",
        run_cmd: "node node_json.js FILE"
    },
    {
        name: "python_json",
        compile_cmd: "/usr/bin/true",
        run_cmd: "python python_json.py FILE"
    }
]


async function main() {

    const impl_dir = process.argv[2];
    const test_file = process.argv[3];
    const num_tests = +process.argv[4];

    process.chdir(impl_dir);

   for (const impl of implementations) {
        console.log(`Compiling '${impl.name}'...`);
        await exec(impl.compile_cmd);
   }

   const performance = {};

    for (const impl of implementations) {
        console.log(`Running '${impl.name}'...`);

        const run_cmd = impl.run_cmd.replace("FILE", test_file);

        let avg_parse = 0.0;
        let avg_serialize = 0.0;
        for (let i = 0; i < num_tests; i++) {
            const { stdout, stderr } = await exec(run_cmd);
            const results = stdout.split(/\n/).map(parseFloat);
            avg_parse += results[0] / num_tests;
            avg_serialize += results[1] / num_tests;
        }

        const n2str = (n, num_decimals = 2) => {
            const str = n.toString();
            return str.substring(0, str.indexOf(".") + num_decimals);
        }
        performance[impl.name] = {
            "parse": avg_parse,
            "serialize": avg_serialize
        };
    }
    console.log(performance);
}

main();
