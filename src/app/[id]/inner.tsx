"use client";

import { useMediaQuery } from "react-responsive";
import { Kanji } from "@/components/kanji";
import { MobileLayout } from "@/components/mobile-layout";
import { Radical } from "@/components/radical";
import { Examples } from "@/components/examples";
import { SearchInput } from "@/components/search-input";
import { DrawInput } from "@/components/draw-input";
import { ScrollArea } from "@/components/ui/scroll-area";
import { SearchIcon } from "lucide-react";
import {
  buildKanjiHref,
  getMobileTabIndex,
  getMobileTabKey,
  isMobileTabKey,
  MOBILE_TAB_KEYS,
  MOBILE_TAB_LABELS,
  MOBILE_TAB_PARAM,
  type MobileTabKey,
} from "@/lib/kanji-routing";
import {
  DESKTOP_BOTTOM_ROW,
  DESKTOP_GRAPH_CELL,
  DESKTOP_TOP_ROW,
  MOBILE_GRAPH_CELL,
} from "@/lib/graph-layout";
import { activeKanjiGraphAtom, graphClearedAtom } from "@/lib/store";
import { useSetAtom } from "jotai";
import { useRouter, usePathname, useSearchParams } from "next/navigation";
import React, { Suspense } from "react";

interface KanjiPageContentProps {
  requestedId: string;
  canonicalId: string;
  variantInfo: {
    aliases: string[];
  };
  kanjiInfo: KanjiInfo; // Replace 'any' with the actual type
  graphData: BothGraphData; // Replace 'any' with the actual type
  strokeAnimation: string | null; // Replace 'any' with the actual type
  navigableRadicalIds: string[];
}

export function KanjiPageContent({
  ...props
}: KanjiPageContentProps) {
  return (
    <Suspense fallback={<div className="w-full grow overflow-hidden" />}>
      <KanjiPageContentInner {...props} />
    </Suspense>
  );
}

function KanjiPageContentInner({
  requestedId,
  canonicalId,
  ...props
}: KanjiPageContentProps) {
  const isMobile = useMediaQuery({ query: "(max-width: 767px)" });
  const { replace } = useRouter();
  // The server renders the placeholder below. A plain useState, flipped in
  // a layout effect before the first paint, keeps the first client render
  // identical to the server's, so mobile viewports hydrate without a
  // mismatch: a store reading true on the client (as
  // useSyncExternalStore's client snapshot did) would already render the
  // real mobile branch against the server's desktop placeholder.
  const [isHydrated, setIsHydrated] = React.useState(false);
  React.useLayoutEffect(() => {
    setIsHydrated(true);
  }, []);

  // Redirect variant URLs to the canonical one. useSearchParams is
  // deliberately avoided here: it suspends during prerendering, which
  // would force this whole subtree (including the graphs) to remount on
  // every navigation. The tab param is a mobile-only concept, so on
  // desktop we read it straight from the URL instead.
  React.useEffect(() => {
    if (isHydrated && !isMobile && requestedId !== canonicalId) {
      const rawTab = new URLSearchParams(window.location.search).get(
        MOBILE_TAB_PARAM,
      );
      void replace(
        buildKanjiHref(canonicalId, {
          // Drop unknown values. The mobile redirect normalizes unknown
          // values to the first tab instead.
          tab: rawTab && isMobileTabKey(rawTab) ? rawTab : null,
        }),
      );
    }
  }, [isHydrated, isMobile, requestedId, canonicalId, replace]);

  // Placeholder with the same structure, rendered by the server and by the
  // first client render (no layout shift, no hydration mismatch)
  if (!isHydrated) {
    return (
      <>
        {/* Mobile placeholder */}
        <div className="w-full grow md:hidden overflow-hidden" />
        {/* Desktop placeholder */}
        <div
          className={`w-full grow hidden md:grid grid-cols-1 ${DESKTOP_TOP_ROW} overflow-hidden`}
        >
          <div className="top grid grid-cols-[252px_1.5fr_1fr] overflow-hidden border-b border-lighter">
            <div className="flex flex-col items-center gap-2 mt-3" />
            <div className="p-4 border-l" />
            <div className="p-4 border-l" />
          </div>
          <div className={`bottom grid ${DESKTOP_BOTTOM_ROW} overflow-hidden`}>
            <div />
            <div className={DESKTOP_GRAPH_CELL} />
          </div>
        </div>
      </>
    );
  }

  const { kanjiInfo, graphData } = props;

  return (
    <>
      <KanjiGraphBridge kanjiInfo={kanjiInfo} graphData={graphData} />
      {isMobile ? (
        <MobileKanjiPage
          requestedId={requestedId}
          canonicalId={canonicalId}
          {...props}
        />
      ) : (
        <DesktopKanjiPage {...props} />
      )}
    </>
  );
}

// Publishes this page's kanji data to the store so GlobalGraphLayer (root
// layout) can render the graph outside the [id] route segment. Next.js keys
// segment subtrees by param value, so anything inside the page remounts on
// every kanji navigation; the layout is not, so the graph lives there.
function KanjiGraphBridge({
  kanjiInfo,
  graphData,
}: Pick<KanjiPageContentProps, "kanjiInfo" | "graphData">) {
  const setGraph = useSetAtom(activeKanjiGraphAtom);
  const setGraphCleared = useSetAtom(graphClearedAtom);

  React.useEffect(() => {
    setGraph({ kanjiInfo, graphData });
    // Undo the 404 page's clear flag, if any.
    setGraphCleared(false);
    // Deliberately no null cleanup: the atom must keep this page's data
    // through the suspense window of the next navigation, so the graph
    // layers (and their WebGL context) stay mounted. The next kanji page
    // overwrites it; off kanji routes useActiveGraphData ignores it.
  }, [kanjiInfo, graphData, setGraph, setGraphCleared]);

  return null;
}

function DesktopKanjiPage({
  variantInfo,
  kanjiInfo,
  graphData,
  strokeAnimation,
  navigableRadicalIds,
}: Omit<KanjiPageContentProps, "requestedId" | "canonicalId">) {
  return (
    <div
      className={`w-full grow hidden md:grid grid-cols-1 ${DESKTOP_TOP_ROW} overflow-hidden`}
    >
      <div className="top grid grid-cols-[252px_1.5fr_1fr] overflow-hidden border-b border-lighter">
        <div className="flex flex-col items-center gap-2 mt-3">
          <SearchInput searchPlaceholder="Search..." />
          <DrawInput />
        </div>
        <ScrollArea className="w-full h-full">
          <div className="p-4 border-l">
            <Kanji
              screen="desktop"
              kanjiInfo={kanjiInfo}
              variantInfo={variantInfo}
              graphData={graphData}
              strokeAnimation={strokeAnimation}
            />
          </div>
        </ScrollArea>
        <div className="p-4 border-l">
          <Radical kanjiInfo={kanjiInfo} navigableRadicalIds={navigableRadicalIds} />
        </div>
      </div>
      <div className={`bottom grid ${DESKTOP_BOTTOM_ROW} overflow-hidden`}>
        <ScrollArea className="w-full h-full">
          <Examples kanjiInfo={kanjiInfo} />
        </ScrollArea>
        {/* The graph itself is rendered by GlobalGraphLayer (root layout) on
            top of this cell, so it survives kanji navigations. */}
        <div className={DESKTOP_GRAPH_CELL} />
      </div>
    </div>
  );
}

// The mobile layout needs usePathname/useSearchParams for the ?tab= param.
// Those hooks suspend during prerendering, so they live here — a component
// that only mounts client-side after the media query resolves — instead of
// in KanjiPageContentInner, keeping the server-rendered (desktop) tree free
// of suspending hooks so the page prerenders without a suspended boundary.
function MobileKanjiPage({
  requestedId,
  canonicalId,
  variantInfo,
  kanjiInfo,
  graphData,
  strokeAnimation,
  navigableRadicalIds,
}: KanjiPageContentProps) {
  const { replace } = useRouter();
  const pathname = usePathname();
  const searchParams = useSearchParams();
  // Depend on the raw value, not a bound function: searchParams.get.bind(...)
  // is a new function every render, which would re-run the redirect effect
  // below on every render instead of only when the tab param actually changes.
  const rawTabParam = searchParams.get(MOBILE_TAB_PARAM);
  const urlMobileTab = getMobileTabIndex(rawTabParam);
  const [mobileTabOverride, setMobileTabOverride] = React.useState<{
    pathname: string;
    tab: number;
  } | null>(null);
  const activeMobileTab =
    mobileTabOverride && mobileTabOverride.pathname === pathname
      ? mobileTabOverride.tab
      : urlMobileTab;

  React.useEffect(() => {
    if (
      mobileTabOverride &&
      mobileTabOverride.pathname === pathname &&
      mobileTabOverride.tab === urlMobileTab
    ) {
      setMobileTabOverride(null);
    }
  }, [mobileTabOverride, pathname, urlMobileTab]);

  // Drop unknown ?tab values from the URL (getMobileTabIndex silently falls
  // back to the first tab, so an invalid value would otherwise linger).
  // The write is a raw history.replaceState, which bypasses the Next.js
  // router, so useSearchParams is not notified and this cleanup does not
  // re-trigger.
  React.useEffect(() => {
    const rawTab = searchParams.get(MOBILE_TAB_PARAM);
    if (rawTab === null || isMobileTabKey(rawTab)) {
      return;
    }

    const nextParams = new URLSearchParams(searchParams.toString());
    nextParams.delete(MOBILE_TAB_PARAM);
    const nextQuery = nextParams.toString();
    const nextUrl = nextQuery ? `${pathname}?${nextQuery}` : pathname;
    window.history.replaceState(window.history.state, "", nextUrl);
  }, [searchParams, pathname]);

  const handleMobileTabChange = React.useCallback(
    (tabIndex: number) => {
      if (tabIndex === activeMobileTab) {
        return;
      }

      setMobileTabOverride({
        pathname,
        tab: tabIndex,
      });

      const nextParams = new URLSearchParams(searchParams.toString());
      nextParams.set(MOBILE_TAB_PARAM, getMobileTabKey(tabIndex));
      const nextQuery = nextParams.toString();
      const nextUrl = nextQuery ? `${pathname}?${nextQuery}` : pathname;
      window.history.replaceState(window.history.state, "", nextUrl);
    },
    [activeMobileTab, pathname, searchParams],
  );

  React.useEffect(() => {
    if (requestedId !== canonicalId) {
      void replace(
        buildKanjiHref(canonicalId, {
          tab: rawTabParam ? getMobileTabKey(activeMobileTab) : null,
        }),
      );
    }
  }, [activeMobileTab, canonicalId, requestedId, rawTabParam, replace]);

  // One entry per tab key; the list below derives id, order, and label
  // from MOBILE_TAB_KEYS so the tabs can't drift from the key the graph
  // layer indexes into.
  const tabContents: Record<MobileTabKey, React.ReactNode> = {
    kanji: (
      <div className="p-4">
        <Kanji
          kanjiInfo={kanjiInfo}
          variantInfo={variantInfo}
          graphData={graphData}
          strokeAnimation={strokeAnimation}
          screen="mobile"
        />
      </div>
    ),
    radical: (
      <div className="p-4">
        <Radical kanjiInfo={kanjiInfo} navigableRadicalIds={navigableRadicalIds} />
      </div>
    ),
    examples: (
      <ScrollArea className="size-full">
        <Examples kanjiInfo={kanjiInfo} />
      </ScrollArea>
    ),
    // The graph itself is rendered by MobileGraphLayer (root layout) on
    // top of this tab, so it survives kanji navigations without
    // recreating its WebGL context.
    graph: <div className={MOBILE_GRAPH_CELL} />,
    search: (
      <div className="relative mt-8 p-4 flex flex-col items-center gap-12">
        <SearchInput searchPlaceholder="Search kanji..." />
        <DrawInput />
      </div>
    ),
  };

  return (
    <div className="w-full grow md:hidden overflow-hidden">
      <MobileLayout
        tabs={MOBILE_TAB_KEYS.map((key, id) => ({
          id,
          label:
            key === "search" ? (
              <SearchIcon className="size-4 inline-block -translate-y-0.5" />
            ) : (
              MOBILE_TAB_LABELS[key]
            ),
          content: tabContents[key],
        }))}
        activeTab={activeMobileTab}
        onActiveTabChange={handleMobileTabChange}
      />
    </div>
  );
}
