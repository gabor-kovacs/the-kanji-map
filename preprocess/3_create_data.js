// * pull the kanji data from Jisho.org and KanjiVG
// nextjs can be used directly to fetch this data in getStaticProps, however storing locally can speed up build times later on
const fs = require("fs");
const path = require("path");
const searchlist = require("./searchlist.json");
const axios = require("axios").default;
require("dotenv").config();
const JishoAPI = require("unofficial-jisho-api");
const jisho = new JishoAPI();

const sleep = (waitTimeInMs) =>
  new Promise((resolve) => setTimeout(resolve, waitTimeInMs));

const getKanjiData = async (id) => {
  // GETTING KANJI INFO FROM KANJIALIVE
  let kanjialiveData = null;
  const options = {
    method: "GET",
    url: `https://kanjialive-api.p.rapidapi.com/api/public/kanji/${encodeURIComponent(
      id
    )}`,
    headers: {
      "x-rapidapi-key": `${process.env.KANJIALIVE_API_KEY}`,
      "x-rapidapi-host": "kanjialive-api.p.rapidapi.com",
    },
  };
  try {
    const res = await axios.request(options);
    kanjialiveData = await res.data;
    console.log(`kanjialiveData found for ${id}`);
    // console.log(kanjialiveData);
  } catch (error) {
    console.log(error);
    console.log("No Kanjialive data found");
  }

  let jishoData = null;
  // const JISHO_SEARCH_URI = jisho.getUriForKanjiSearch(id);
  try {
    jishoData = await jisho.searchForKanji(id);
    console.log(`jishoData found for ${id}`);
    // console.log(jishoData);
  } catch (error) {
    console.log(error);
    console.log("No JISHO data found");
  }

  return {
    id,
    kanjialiveData,
    jishoData,
  };
};

const kanjilist = searchlist.map((el) => el.k);

const main = async () => {
  for (let i = 0; i < kanjilist.length; i++) {
    console.log(`${i}/${kanjilist.length}`);
    const kanji = kanjilist[i];
    await sleep(1000);
    const result = await getKanjiData(kanji);
    fs.writeFileSync(
      path.join(__dirname, "..", "data", "kanji", `${kanji}.json`),
      JSON.stringify(result),
      "utf-8"
    );
  }
};

main();
