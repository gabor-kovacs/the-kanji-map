"use client";
import {
  Carousel,
  CarouselContent,
  CarouselItem,
  type CarouselApi,
} from "@/components/ui/carousel";
import { cn } from "@/lib/utils";
import { MOBILE_TAB_BAR_INSET } from "@/lib/graph-layout";
import { setMobileCarouselProgress } from "@/lib/mobile-carousel-progress";
import { LazyMotion, domAnimation, m } from "framer-motion";

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

  // Embla 8.x animates with friction physics, not a fixed duration: the
  // stock body (duration 25, friction 0.68) takes ~1.8s to settle, which
  // feels sluggish for a tab switcher. Programmatic tab switches use a
  // faster body (~0.7s, no overshoot, verified by simulating embla's exact
  // integrator); the public api.scrollTo and drag handlers keep stock values
  // (restored on settle, see below).
  const scrollToTab = React.useCallback(
    (idx: number) => {
      try {
        // internalEngine is a private API — if a future embla version
        // changes or removes it, fall back to the public (stock-speed)
        // scrollTo rather than breaking tab switches.
        const engine = api?.internalEngine();
        if (engine) {
          engine.scrollBody.useDuration(9).useFriction(0.57);
          engine.scrollTo.index(idx, 0);
          return;
        }
      } catch {
        // private API changed; use the public API below
      }
      api?.scrollTo(idx);
    },
    [api],
  );

  const isControlled = typeof controlledActiveTab === "number";
  const [internalActiveTab, setInternalActiveTab] = React.useState<number | null>(null);
  const activeTab = isControlled
    ? controlledActiveTab
    : internalActiveTab ?? initialActiveTab;
  const initialCarouselTab = React.useRef(activeTab);
  const activeTabRef = React.useRef(activeTab);

  React.useEffect(() => {
    activeTabRef.current = activeTab;
  }, [activeTab]);

  // Publish the starting position before paint (useLayoutEffect), so when the
  // [id] subtree remounts across kanji navigations the hoisted graph layer
  // never renders one frame with the previous page's stale position.
  const tabCountRef = React.useRef(tabs.length);
  React.useLayoutEffect(() => {
    const count = tabCountRef.current;
    setMobileCarouselProgress(
      count > 1
        ? Math.round((initialCarouselTab.current / (count - 1)) * 1000) / 1000
        : 0,
    );
  }, [initialCarouselTab]);

  const setActiveTab = React.useCallback(
    (nextTab: number) => {
      if (nextTab === activeTabRef.current) {
        return;
      }

      activeTabRef.current = nextTab;

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
      scrollToTab(activeTab);
    }

    const handleSelect = () => {
      const nextTab = api.selectedScrollSnap();

      if (nextTab !== activeTabRef.current) {
        setActiveTab(nextTab);
      }
    };

    // "scroll" only fires while the carousel is animating and stops just
    // before the exact snap position, so the exact position is also
    // published on "settle". Rounding to 0.001 (~4px) keeps values that are
    // one frame away from settling from sticking to the wrong side of
    // visibility thresholds in consumers.
    const publish = () => {
      setMobileCarouselProgress(
        Math.round(api.scrollProgress() * 1000) / 1000,
      );
    };

    const handleScroll = publish;

    // A programmatic tab switch above reconfigures the shared scroll body
    // to settle faster; restore the stock values when the body comes to
    // rest so a following user drag uses stock physics instead of the
    // leftover fast ones.
    const handleSettle = () => {
      publish();
      try {
        api
          .internalEngine()
          .scrollBody.useBaseDuration()
          .useBaseFriction();
      } catch {
        // private API changed; the public scrollTo fallback already
        // animates with stock values
      }
    };

    // Publish on (re)initialization too, since "scroll" never fires while
    // the carousel is at rest.
    publish();

    api.on("select", handleSelect);
    api.on("scroll", handleScroll);
    api.on("settle", handleSettle);

    return () => {
      api.off("select", handleSelect);
      api.off("scroll", handleScroll);
      api.off("settle", handleSettle);
    };
  }, [activeTab, api, setActiveTab, scrollToTab]);

  const handleTabClick = (newIdx: number) => {
    if (newIdx !== activeTab && !disabled) {
      setActiveTab(newIdx);
      scrollToTab(newIdx);
    }
  };

  return (
    <div className="size-full overflow-hidden">
      <LazyMotion features={domAnimation}>
        <Carousel
          setApi={setApi}
          className={cn("size-full", MOBILE_TAB_BAR_INSET)}
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
              <m.span
                layoutId="bubble"
                className="absolute inset-0 z-10 bg-muted/50 mix-blend-screen shadow-inner-shadow border rounded-md"
                transition={{ type: "spring", bounce: 0.19, duration: 0.4 }}
              />
            )}
            <span className="relative size-full text-center">{tab.label}</span>
          </button>
        ))}
      </div>
      </LazyMotion>
    </div>
  );
};
