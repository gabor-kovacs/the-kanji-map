/// <reference types="react-scripts" />

interface Layout {
  kanjiFocused: boolean;
  examplesFocused: boolean;
  radicalFocused: boolean;
}

interface KanjiInfo {
  jishoData?: JishoData;
  kanjialiveData?: KanjiAliveData;
}

interface JishoData {
  taughtIn?: string;
  jlptLevel?: string;
  newspaperFrequencyRank?: string;
  strokeCount?: number;
  meaning?: string;
  kunyomi?: string[];
  onyomi?: string[];
  onyomiExamples?: {
    example?: string;
    reading?: string;
    meaning?: string;
  }[];
  kunyomiExamples?: {
    example?: string;
    reading?: string;
    meaning?: string;
  }[];
  radical?: {
    symbol?: string;
    meaning?: string;
  };
}

interface KanjiAliveData {
  radical?: {
    strokes?: number;
    image?: string;
    name?: {
      hiragana?: string;
    };
    animation?: string[];
  };
  examples?: {
    japanese?: string;
    meaning?: {
      english?: string;
    };
    audio?: {
      mp3: string;
    };
  }[];
}

interface ChiseEntry {
  kanji: string;
  structure: string[];
  composition: string[];
}

interface Dimensions {
  height: number;
  width: number;
}

// interface GraphData {
//   nodes: { id: string }[];
//   links: { source: string; target: string }[];
// }
