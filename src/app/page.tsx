import { DrawInput } from "@/components/draw-input";
import { Header } from "@/components/header";
import { MobileLayout } from "@/components/mobile-layout";
import { SearchInput } from "@/components/search-input";
import { SearchIcon } from "lucide-react";
import type { Metadata } from "next";

{
  /* <head>
        <DefaultSeo
          openGraph={{
            type: "website",
            locale: "en_US",
            url: "https://thekanjimap.com/",
            site_name: "The Kanji Map",
          }}
          twitter={{
            handle: "@handle",
            site: "@site",
            cardType: "summary_large_image",
          }}
        />
      </head> */
}

export default function Home() {
  return (
    <div className="size-full flex flex-col">
      <Header className="w-full" />
      {/* MOBILE */}
      <div className="w-full flex-grow md:hidden">
        <MobileLayout
          tabs={[
            {
              id: 0,

              label: "漢字",
              content: (
                <div className="relative mt-8 p-4 flex flex-col items-center gap-12">
                  <SearchInput searchPlaceholder="Search kanji..." />
                  <DrawInput />
                </div>
              ),
            },
            {
              id: 1,
              label: "例",
              content: <div />,
            },
            {
              id: 2,
              label: "部首",
              content: <div />,
            },
            {
              id: 3,
              label: "図",
              content: <div />,
            },
            {
              id: 4,
              label: (
                <SearchIcon className="w-4 h-4 inline-block -translate-y-0.5" />
              ),
              content: (
                <div className="relative mt-8 p-4 flex flex-col items-center gap-12">
                  <SearchInput searchPlaceholder="Search kanji..." />
                  <DrawInput />
                </div>
              ),
            },
          ]}
          initialActiveTab={4}
          disabled
        />
      </div>
      {/* DESKTOP */}
      <div className="w-full flex-grow hidden md:grid grid-cols-1 md:grid-rows-[330px,1fr] ">
        <div className="top grid grid-cols-[252px_1fr_1fr] overflow-hidden border-b border-lighter">
          <div className="flex flex-col items-center gap-2 mt-3">
            <SearchInput searchPlaceholder="Search..." />
            <DrawInput />
          </div>
          <div className="p-4 border-l">
            <h1 className="text-lg font-extrabold">Kanji</h1>
          </div>
          <div className="p-4 border-l">
            <h1 className="text-lg font-extrabold">Radical</h1>
          </div>
        </div>
        <div className="bottom grid grid-cols-[2fr_3fr] overflow-hidden">
          <div className="p-4">
            <h1 className="text-lg font-extrabold">Examples</h1>
          </div>
          <div className="p-4 border-l">
            <h1 className="text-lg font-extrabold">Graph</h1>
          </div>
        </div>
      </div>
    </div>
  );
}
