import { create } from "zustand";
import { devtools, persist } from "zustand/middleware";

interface GraphPreferenceState {
  style: "3D" | "2D";
  rotate: boolean;
  outLinks: boolean;
  setStyle: (newStyle: "3D" | "2D") => void;
  setRotate: (newRotate: boolean) => void;
  setOutLinks: (newOutLinks: boolean) => void;
}

export const useGraphPreferenceStore = create<GraphPreferenceState>()(
  devtools(
    persist(
      (set) => ({
        style: "3D",
        rotate: true,
        outLinks: true,
        setStyle: (newStyle) => {
          set((state: GraphPreferenceState) => ({ ...state, style: newStyle }));
        },
        setRotate: (newRotate) => {
          set((state: GraphPreferenceState) => ({
            ...state,
            rotate: newRotate,
          }));
        },
        setOutLinks: (newOutLinks) => {
          set((state: GraphPreferenceState) => ({
            ...state,
            outLinks: newOutLinks,
          }));
        },
      }),
      { name: "graphPreference" }
    )
  )
);
