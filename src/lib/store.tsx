import { atom } from "jotai";
import { atomWithStorage } from "jotai/utils";

// Custom `atomWithStorage` to wrap existing data structure coming from zustand
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

export {
  graphPreferenceAtom,
  styleAtom,
  rotateAtom,
  outLinksAtom,
  particlesAtom,
};
