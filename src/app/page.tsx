import { DrawInput } from "@/components/draw-input";
import { Header } from "@/components/header";
import { MobileLayout } from "@/components/mobile-layout";
import { SearchInput } from "@/components/search-input";
import { SearchIcon } from "lucide-react";
import { MOBILE_TAB_KEYS, MOBILE_TAB_LABELS } from "@/lib/kanji-routing";
import {
  DESKTOP_BOTTOM_ROW,
  DESKTOP_TOP_ROW,
} from "@/lib/graph-layout";

export const metadata = {
  title: "The Kanji Map",
  description:
    "Explore kanji decomposition, readings, radicals, and examples in an interactive graph.",
};

export default function Home() {
  return (
    <div className="size-full flex flex-col">
      <Header className="w-full" />
      {/* MOBILE */}
      <div className="w-full grow md:hidden">
        <MobileLayout
          tabs={MOBILE_TAB_KEYS.map((key, id) => ({
            id,
            label:
              key === "search" ? (
                <SearchIcon className="size-4 inline-block -translate-y-0.5" />
              ) : (
                MOBILE_TAB_LABELS[key]
              ),
            content:
              key === "kanji" || key === "search" ? (
                <div className="relative mt-8 p-4 flex flex-col items-center gap-12">
                  <SearchInput searchPlaceholder="Search kanji..." />
                  <DrawInput />
                </div>
              ) : (
                <div />
              ),
          }))}
          initialActiveTab={MOBILE_TAB_KEYS.indexOf("search")}
          disabled
        />
      </div>
      {/* DESKTOP */}
      <div
        className={`w-full grow hidden md:grid grid-cols-1 ${DESKTOP_TOP_ROW} overflow-hidden`}
      >
        <div className="top grid grid-cols-[252px_1.5fr_1fr] overflow-hidden border-b border-lighter">
          <div className="flex flex-col items-center gap-2 mt-3">
            <SearchInput searchPlaceholder="Search..." />
            <DrawInput />
          </div>
          <div className="p-4 border-l">
            <h1 className="text-lg font-semibold">Kanji</h1>
          </div>
          <div className="p-4 border-l">
            <h1 className="text-lg font-semibold">Radical</h1>
          </div>
        </div>
        <div className={`bottom grid ${DESKTOP_BOTTOM_ROW} overflow-hidden`}>
          <div className="p-4">
            <h1 className="text-lg font-semibold">Examples</h1>
          </div>
          <div className="p-4 border-l">
            <h1 className="text-lg font-semibold">Graph</h1>
          </div>
        </div>
      </div>
    </div>
  );
}
