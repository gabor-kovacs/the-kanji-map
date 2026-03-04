"use client";

import * as React from "react";
import { InfoIcon } from "lucide-react";
import { Button } from "./ui/button";
import { Popover, PopoverContent, PopoverTrigger } from "./ui/popover";

interface Props {
  showOutLinks: boolean;
  showParticles: boolean;
}

const NODE_LEGEND = [
  { label: "Selected kanji", color: "#2B99CF" },
  { label: "Joyo kanji", color: "#80c2e2" },
  { label: "Jinmeiyo kanji", color: "#d5ebf5" },
  { label: "Other", color: "#ffffff" },
];

export const GraphLegend: React.FC<Props> = ({
  showOutLinks,
  showParticles,
}) => {
  return (
    <div className="absolute bottom-4 left-4 z-50">
      <Popover>
        <PopoverTrigger
          render={
            <Button
              variant="outline"
              size="icon"
              aria-label="Open graph legend"
            >
              <InfoIcon className="size-4" />
            </Button>
          }
        />
        <PopoverContent align="start" side="top" className="w-72 space-y-3">
          <div>
            <p className="text-sm font-semibold">Node colors</p>
            <div className="mt-2 space-y-1.5">
              {NODE_LEGEND.map((entry) => (
                <div
                  key={entry.label}
                  className="flex items-center gap-2 text-xs"
                >
                  <span
                    className="inline-block size-3 rounded-full border border-foreground/70"
                    style={{ backgroundColor: entry.color }}
                  />
                  <span>{entry.label}</span>
                </div>
              ))}
            </div>
          </div>
          <div>
            <p className="text-sm font-semibold">Links</p>
            <div className="mt-2 space-y-1.5 text-xs">
              <div className="flex items-center gap-2">
                <svg width="24" height="10" viewBox="0 0 24 10" aria-hidden>
                  <line
                    x1="1"
                    y1="5"
                    x2="18"
                    y2="5"
                    stroke="currentColor"
                    strokeWidth="1.2"
                  />
                  <path d="M18 2 L23 5 L18 8 Z" fill="currentColor" />
                </svg>
                <span>Arrow shows link direction</span>
              </div>
              <p>Shared onyomi is shown as text on links when available.</p>
              <p>
                {showOutLinks
                  ? "Outgoing links are visible."
                  : "Outgoing links are hidden."}
              </p>
              <p>
                {showParticles
                  ? "Arrow particles are enabled."
                  : "Arrow particles are disabled."}
              </p>
            </div>
          </div>
        </PopoverContent>
      </Popover>
    </div>
  );
};
