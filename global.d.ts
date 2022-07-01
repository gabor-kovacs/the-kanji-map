// import { GraphData } from 'react-force-graph-3d';
// import { KanjiParseResult } from "unofficial-jisho-api";

type KanjiInfo = {
  id: string;
  kanjialiveData?: any;
  jishoData?: KanjiParseResult | null;
};

interface BothGraphData {
  withOutLinks: GraphData;
  noOutLinks: GraphData;
}
