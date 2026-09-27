import { atom } from "jotai";
import { atomWithStorage } from "jotai/utils";

// Graph preferences persisted in localStorage
const graphPreferenceAtom = atomWithStorage("graphPreference", {
  state: {
    style: "3D",
    rotate: true,
    outLinks: true,
    particles: true,
  },
  version: 0,
});

// Derived atoms for individual properties within the nested structure
const styleAtom = atom(
  (get) => get(graphPreferenceAtom).state.style,
  (get, set, newStyle: "3D" | "2D") => {
    const current = get(graphPreferenceAtom);
    set(graphPreferenceAtom, {
      ...current,
      state: { ...current.state, style: newStyle },
    });
  }
);

const rotateAtom = atom(
  (get) => get(graphPreferenceAtom).state.rotate,
  (get, set, newRotate: boolean) => {
    const current = get(graphPreferenceAtom);
    set(graphPreferenceAtom, {
      ...current,
      state: { ...current.state, rotate: newRotate },
    });
  }
);

const outLinksAtom = atom(
  (get) => get(graphPreferenceAtom).state.outLinks,
  (get, set, newOutLinks: boolean) => {
    const current = get(graphPreferenceAtom);
    set(graphPreferenceAtom, {
      ...current,
      state: { ...current.state, outLinks: newOutLinks },
    });
  }
);

const particlesAtom = atom(
  (get) => get(graphPreferenceAtom).state.particles,
  (get, set, newParticles: boolean) => {
    const current = get(graphPreferenceAtom);
    set(graphPreferenceAtom, {
      ...current,
      state: { ...current.state, particles: newParticles },
    });
  }
);

// Data of the kanji page currently on screen. Set by the [id] page and
// read by the graph layers in the root layout. The graph lives outside the
// [id] route segment so Next.js (which keys segment subtrees by param
// value) doesn't remount it on every kanji navigation; instead the graph
// swaps its data in place. The atom keeps the last published data until a
// new kanji page overwrites it (the bridge never nulls it on unmount): on a
// non-prefetched navigation the [id] segment suspends while its RSC payload
// is fetched, and during that window the layers need the previous page's
// data to stay mounted with their WebGL context intact. Off kanji routes
// useActiveGraphData ignores the retained data, so the layers unmount as
// usual; it is a bounded memory cost (one kanji's graph data) that the next
// kanji visit replaces.
const activeKanjiGraphAtom = atom<{
  kanjiInfo: KanjiInfo;
  graphData: BothGraphData;
} | null>(null);

// Set by the 404 page, cleared by a kanji page's bridge. While set,
// useActiveGraphData returns null even on a kanji-shaped route (unknown ids
// keep the single-segment [id] shape), so the graph layers unmount instead
// of showing the previous kanji's graph on an unknown page.
const graphClearedAtom = atom<boolean>(false);

export {
  styleAtom,
  rotateAtom,
  outLinksAtom,
  particlesAtom,
  activeKanjiGraphAtom,
  graphClearedAtom,
};
