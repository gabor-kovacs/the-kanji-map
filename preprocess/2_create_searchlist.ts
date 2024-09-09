// * Create array to store every possible kanji
import { joyoList } from "./joyo";
import { jinmeiyoList } from "./jinmeiyo";
import * as composition from "../data/composition.json";
import JishoAPI from "unofficial-jisho-api";
import * as fs from "fs";
import * as path from "path";

const jisho = new JishoAPI();

interface KanjiInfo {
  k: string; // Kanji character
  r: string; // Reading (Kunyomi)
  m: string; // Meaning
  g: number; // Group
}

const getKanjiInfo = async (id: string): Promise<any> => {
  let jishoData: any = null;
  try {
    jishoData = await jisho.searchForKanji(id);
    console.log(`jishoData found for ${id}`);
  } catch (error) {
    console.log(error);
    console.log("No JISHO data found");
  }
  return jishoData;
};

const getGroup = (id: string): number => {
  if (joyoList.includes(id)) return 1;
  if (jinmeiyoList.includes(id)) return 2;
  return 3;
};

(async () => {
  const searchList: KanjiInfo[] = [];
  const len = Object.entries(composition).length;
  let i = 1;
  for (const [kanji, _] of Object.entries(composition)) {
    console.log(`processing ${i}/${len}`);
    i++;
    const info = await getKanjiInfo(kanji);
    const group = getGroup(kanji);
    searchList.push({
      k: kanji,
      r: info?.kunyomi ? info?.kunyomi.join(", ") : "",
      m: info?.meaning ?? "",
      g: group,
    });
  }

  fs.writeFileSync(
    path.join(path.dirname(__dirname), "data", "searchlist.json"),
    JSON.stringify(searchList, null, 2),
    "utf-8"
  );
})();
