type KanjiInfo = {
  id: string;
  kanjialiveData?: any;
  jishoData?: KanjiParseResult | null;
};

interface BothGraphData {
  withOutLinks: GraphData;
  noOutLinks: GraphData;
}
