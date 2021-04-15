/*
const fs = require('fs');

let rawdata = fs.readFileSync('composition.json');
let composition = JSON.parse(rawdata);
let newComposition = {};

for (kanji in composition) {
	let entry = composition[kanji];
	if (newComposition[kanji] === undefined) {
		newComposition[kanji] = entry;
	}
}

let newCompositionData = JSON.stringify(newComposition);
fs.writeFileSync('newComposition.json', newCompositionData);
*/
