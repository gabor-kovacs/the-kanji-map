import axios from "axios";
import Composition from "../preprocess/composition.json";

import JishoAPI from "unofficial-jisho-api";
const jisho = new JishoAPI();

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
    // contentHtml,
    // ...matterResult.data,
  };
};

// recursively search in kanji
const findNodes = (array: string[]) => {
  array.forEach((el) => {
    Composition[el as keyof typeof Composition].in.forEach((node) => {
      if (!array.includes(node)) {
        array.push(node);
        findNodes(array);
      }
    });
  });
  return array;
};

const createInLinks = (array: string[]) => {
  const links: { source: string; target: string }[] = [];
  array.forEach((end) => {
    Composition[end as keyof typeof Composition].in.forEach((start) => {
      if (start !== end) {
        links.push({ source: start, target: end });
      }
    });
  });
  return links;
};

export const getGraphData = async (id: string) => {
  let inNodeList = [id];
  inNodeList = findNodes(inNodeList);
  const inLinks = createInLinks(inNodeList);

  let outLinks = Composition[id as keyof typeof Composition].out.map((node) => {
    return { source: id, target: node };
  });

  const outNodeList = Composition[id as keyof typeof Composition].out;

  const inNodes = await Promise.all(
    inNodeList.map(async (x) => {
      return {
        id: x,
        data: await getKanjiData(x),
      };
    })
  );

  const outNodes = await Promise.all(
    outNodeList.map(async (x) => {
      return {
        id: x,
        data: await getKanjiData(x),
      };
    })
  );

  const allNodes = inNodes.concat(outNodes);
  const allLinks = inLinks.concat(outLinks);

  const withOutLinks = {
    nodes: allNodes.filter(
      (value, index, self) => index === self.findIndex((t) => t.id === value.id)
    ),
    links: allLinks.filter(
      (value, index, self) =>
        index ===
        self.findIndex(
          (t) => t.source === value.source && t.target === value.target
        )
    ),
  };
  const noOutLinks = {
    nodes: inNodes.filter(
      (value, index, self) => index === self.findIndex((t) => t.id === value.id)
    ),
    links: inLinks.filter(
      (value, index, self) =>
        index ===
        self.findIndex(
          (t) => t.source === value.source && t.target === value.target
        )
    ),
  };
  return { withOutLinks, noOutLinks };
};

// export const getTheMap = () => {
//   const nodes = Object.entries(Composition).map(([kanji, _]) => {
//     return { id: kanji, group: 1 };
//   });

//   let inLinks: { source: string; target: string; value: number }[] = [];
//   let outLinks: { source: string; target: string; value: number }[] = [];

//   Object.entries(Composition).forEach(([kanji, data]) => {
//     data.in.forEach((node) => {
//       inLinks.push({ source: node, target: kanji, value: 1 });
//       outLinks.push({ source: kanji, target: node, value: 1 });
//     });
//   });

//   // remove duplicates
//   let links = inLinks.concat(outLinks);
//   links = links.filter(
//     (value, index, self) =>
//       index ===
//       self.findIndex(
//         (t) => t.source === value.source && t.target === value.target
//       )
//   );

//   return { nodes, links };
// };
