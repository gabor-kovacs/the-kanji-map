type KanjiInfo = {
  id: string;
  kanjialiveData?: any;
  jishoData?: KanjiParseResult | null;
};

type GraphNodeData = {
  kunyomi: string[];
  onyomi: string[];
  meaning: string;
};

type GraphNode = {
  id: string;
  data: GraphNodeData | null;
};

interface BothGraphData {
  withOutLinks: GraphData;
  noOutLinks: GraphData;
}
