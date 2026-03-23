"use client";
import {
  Carousel,
  CarouselContent,
  CarouselItem,
  type CarouselApi,
} from "@/components/ui/carousel";
import { cn } from "@/lib/utils";
import { motion } from "framer-motion";

import * as React from "react";

type Tab = {
  id: number;
  label: string | React.ReactNode;
  content: React.ReactNode;
};

export const MobileLayout = ({
  tabs,
  initialActiveTab = 0,
  activeTab: controlledActiveTab,
  onActiveTabChange,
  disabled = false,
}: {
  tabs: Tab[];
  initialActiveTab?: number;
  activeTab?: number;
  onActiveTabChange?: (index: number) => void;
  disabled?: boolean;
}) => {
  const [api, setApi] = React.useState<CarouselApi>();
  const isControlled = typeof controlledActiveTab === "number";
  const [internalActiveTab, setInternalActiveTab] =
    React.useState(initialActiveTab);
  const activeTab = isControlled ? controlledActiveTab : internalActiveTab;
  const initialCarouselTab = React.useRef(activeTab);

  React.useEffect(() => {
    if (!isControlled) {
      setInternalActiveTab(initialActiveTab);
    }
  }, [initialActiveTab, isControlled]);

  const setActiveTab = React.useCallback(
    (nextTab: number) => {
      if (!isControlled) {
        setInternalActiveTab(nextTab);
      }

      onActiveTabChange?.(nextTab);
    },
    [isControlled, onActiveTabChange],
  );

  React.useEffect(() => {
    if (!api) {
      return;
    }

    if (api.selectedScrollSnap() !== activeTab) {
      api.scrollTo(activeTab);
    }

    const handleSelect = () => {
      const nextTab = api.selectedScrollSnap();

      if (nextTab !== activeTab) {
        setActiveTab(nextTab);
      }
    };

    api.on("select", handleSelect);

    return () => {
      api.off("select", handleSelect);
    };
  }, [activeTab, api, setActiveTab]);

  const handleTabClick = (newIdx: number) => {
    if (newIdx !== activeTab && !disabled) {
      setActiveTab(newIdx);
      api?.scrollTo(newIdx);
    }
  };

  return (
    <div className="size-full overflow-hidden">
      <Carousel
        setApi={setApi}
        className="size-full pb-10"
        opts={{ watchDrag: false, startIndex: initialCarouselTab.current }}
      >
        <CarouselContent className="relative size-full">
          {tabs.map((tab) => (
            <CarouselItem key={tab.id} className="min-h-full">
              {tab.content}
            </CarouselItem>
          ))}
        </CarouselContent>
      </Carousel>
      <div
        className={cn(
          "absolute bg-background bottom-0 space-x-1 border-t cursor-pointer px-[3px] py-[3.2px] shadow-inner-shadow w-full grid grid-cols-5 shrink-0"
        )}
      >
        {tabs.map((tab, idx) => (
          <button
            key={tab.id}
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
