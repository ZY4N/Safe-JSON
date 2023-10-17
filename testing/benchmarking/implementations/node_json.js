const fs = require('fs');

const text = fs.readFileSync(process.argv[2])

const begin_parse = process.hrtime();
const json = JSON.parse(text);
const end_parse = process.hrtime();

const begin_serialize = process.hrtime();
const str = JSON.stringify(json, null, 4);
const end_serialize = process.hrtime();

const hrtime2micro = (time) => time[0] * 1000000 + time[1] / 1000;
console.log(hrtime2micro(end_parse) - hrtime2micro(begin_parse));
console.log(hrtime2micro(end_serialize) - hrtime2micro(begin_serialize));
