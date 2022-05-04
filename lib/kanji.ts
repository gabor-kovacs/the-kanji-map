import axios from "axios";
import Composition from "../preprocess/composition.json";

export const getAllKanji = () => {
  return Object.entries(Composition).map(([kanji, _]) => {
    return {
      params: {
        id: kanji,
      },
    };
  });
};

export const getKanjiData = async (id: string) => {
  // GETTING KANJI INFO FROM KANJIALIVE
  let kanjialiveData = null;
  console.log(process.env.KANJIALIVE_API_KEY);
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
    console.log("kanjialiveData");
    console.log(kanjialiveData);
  } catch (error) {
    console.log("No Kanjialive data found");
  }

  return {
    id,
    kanjialiveData,
    // contentHtml,
    // ...matterResult.data,
  };
};
