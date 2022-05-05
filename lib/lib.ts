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

export const getGraphData = (id: string) => {
  let inNodeList = [id];
  inNodeList = findNodes(inNodeList);
  const inLinks = createInLinks(inNodeList);

  let outLinks = Composition[id as keyof typeof Composition].out.map((node) => {
    return { source: id, target: node };
  });

  const outNodeList = Composition[id as keyof typeof Composition].out;

  const inNodes = inNodeList.map((x) => ({ id: x }));
  const outNodes = outNodeList.map((x) => ({ id: x }));

  const graphDataWithOutLinks = {
    nodes: inNodes.concat(outNodes),
    links: inLinks.concat(outLinks),
  };
  const graphDataNoOutLinks = {
    nodes: inNodes,
    links: inLinks,
  };
  return { graphDataNoOutLinks, graphDataWithOutLinks };
};