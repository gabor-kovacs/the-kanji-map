"use client";
import {
  Carousel,
  CarouselContent,
  CarouselItem,
  type CarouselApi,
} from "@/components/ui/carousel";
import { cn } from "@/lib/utils";
import { motion } from "framer-motion";

import { Button } from "./ui/button";
import * as React from "react";

type Tab = {
  id: number;
  label: string | React.ReactNode;
  content: React.ReactNode;
};

export const MobileLayout = ({
  tabs,
  initialActiveTab = 0,
  disabled = false,
}: {
  tabs: Tab[];
  initialActiveTab?: number;
  disabled?: boolean;
}) => {
  const [api, setApi] = React.useState<CarouselApi>();

  const [activeTab, setActiveTab] = React.useState(initialActiveTab);

  React.useEffect(() => {
    if (!api) {
      return;
    }
    setActiveTab(initialActiveTab);
    api.on("select", () => {
      setActiveTab(api.selectedScrollSnap());
    });
  }, [api]);

  const handleTabClick = (newIdx: number) => {
    if (newIdx !== activeTab && !disabled) {
      if (newIdx !== activeTab) {
        setActiveTab(newIdx);
        api?.scrollTo(newIdx);
      }
    }
  };

  return (
    <div className="size-full overflow-hidden">
      <Carousel
        setApi={setApi}
        className="size-full pb-10"
        opts={{ watchDrag: false }}
      >
        <CarouselContent className="relative size-full">
          {tabs.map((tab, idx) => (
            <CarouselItem key={tab.id} className="min-h-full">
              {tab.content}
            </CarouselItem>
          ))}
        </CarouselContent>
      </Carousel>
      <div className="fixed bottom-0 left-0 right-0 flex gap-2 items-center">
        {Array.from({ length: 5 }).map((_, i) => (
          <Button size="icon" key={i} onClick={() => api?.scrollTo(i)}>
            {i + 1}
          </Button>
        ))}
      </div>
      <div
        className={cn(
          "absolute bg-background bottom-0 space-x-1 border-t cursor-pointer px-[3px] py-[3.2px] shadow-inner-shadow w-full grid grid-cols-5 shrink-0"
        )}
      >
        {tabs.map((tab, idx) => (
          <button
            key={idx}
            onClick={() => handleTabClick(idx)}
            disabled={activeTab === idx ? false : disabled}
            className={cn(
              "relative px-3.5 py-1.5 sm:text-sm font-medium transition focus-visible:outline-1 focus-visible:ring-1 focus-visible:outline-hidden flex gap-2 items-center",
              activeTab === idx ? "text-foreground!" : " text-foreground/50"
            )}
            style={{ WebkitTapHighlightColor: "transparent" }}
          >
            {activeTab === idx && (
              <motion.span
                layoutId="bubble"
                className="absolute inset-0 z-10 bg-muted/50 mix-blend-screen shadow-inner-shadow border rounded-md"
                transition={{ type: "spring", bounce: 0.19, duration: 0.4 }}
              />
            )}
            <span className="relative size-full text-center">{tab.label}</span>
          </button>
        ))}
      </div>
    </div>
  );
};
