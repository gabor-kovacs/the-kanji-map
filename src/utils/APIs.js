const axios = require('axios').default;
const JishoApi = require('unofficial-jisho-api');
const jisho = new JishoApi();

export async function getKanjiInfo(kanji) {
	let jishoData = null;
	let kanjialiveData = null;
	// GETTING KANJI INFO FROM JISHO
	const JISHO_SEARCH_URI = 'https://cors.bridged.cc/' + jisho.getUriForKanjiSearch(kanji);

	try {
		const res = await axios.request({ method: 'GET', url: JISHO_SEARCH_URI });
		const jishoRawData = await res.data;
		jishoData = await jisho.parseKanjiPageHtml(jishoRawData, kanji);
		console.log('Jisho data: ', jishoData);
	} catch (error) {
		console.log('No Jisho data found');
	}

	// GETTING KANJI INFO FROM KANJIALIVE
	const options = {
		method: 'GET',
		url: `https://kanjialive-api.p.rapidapi.com/api/public/kanji/${kanji}`,
		headers: {
			'x-rapidapi-key': `${process.env.REACT_APP_KANJIALIVE_API_KEY}`,
			'x-rapidapi-host': 'kanjialive-api.p.rapidapi.com',
		},
	};
	try {
		const res = await axios.request(options);
		kanjialiveData = await res.data;
	} catch (error) {
		console.log('No Kanjialive data found');
	}
	return { jishoData: jishoData, kanjialiveData: kanjialiveData };
}
