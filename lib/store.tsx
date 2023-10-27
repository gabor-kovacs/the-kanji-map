import create, { SetState, GetState } from "zustand";
import { devtools, persist } from "zustand/middleware";

interface GraphPreferenceState {
  style: "3D" | "2D";
  rotate: boolean;
  outLinks: boolean;
  setStyle: (newStyle: "3D" | "2D") => void;
  setRotate: (newRotate: boolean) => void;
  setOutLinks: (newOutLinks: boolean) => void;
}

const graphPreferenceStore: (
  set: SetState<GraphPreferenceState>,
  get: GetState<GraphPreferenceState>
) => GraphPreferenceState = (set) => ({
  style: "3D",
  rotate: true,
  outLinks: true,
  setStyle: (newStyle) => {
    set((state: GraphPreferenceState) => ({ ...state, style: newStyle }));
  },
  setRotate: (newRotate) => {
    set((state: GraphPreferenceState) => ({ ...state, rotate: newRotate }));
  },
  setOutLinks: (newOutLinks) => {
    set((state: GraphPreferenceState) => ({ ...state, outLinks: newOutLinks }));
  },
});

export const useGraphPreferenceStore = create(
  devtools(persist(graphPreferenceStore, { name: "graphPreference" }))
);
