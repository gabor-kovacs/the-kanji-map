import React, { useState, useEffect, useRef, useMemo } from "react";
import { motion, AnimatePresence } from "framer-motion";

type Props = {
  svgContent: string;
  strokeCount: number | null;
};

const SVG_STROKE_LENGTH = 3337; // Default path length for each stroke.

export function KanjiStrokeAnimation({ svgContent, strokeCount }: Props) {

  const svgContainerRef = useRef<HTMLDivElement>(null);
  const [isPlaying, setIsPlaying] = useState(true);
  const [drawProgress, setDrawProgress] = useState(0);
  const [isUserSeeking, setIsUserSeeking] = useState(false);
  const totalLength = SVG_STROKE_LENGTH * (strokeCount || 0);
  const [svgInjected, setSvgInjected] = useState(false);

  // Modify the SVG content to remove default animation
  const modifiedSvgContent = useMemo(() => {
    if (!svgContent) return "";
    return svgContent.replace(
      /animation:zk var\(--t\) linear forwards var\(--d\);/,
      "animation: none;"
    );
  }, [svgContent]); // only recompute when svgContent changes

  // only inject the modified SVG once
  useEffect(() => {
    if (svgContainerRef.current && !svgInjected) {
      svgContainerRef.current.innerHTML = modifiedSvgContent;
      setSvgInjected(true);
    }
  }, [modifiedSvgContent, svgInjected]);

  // Animation loop
  useEffect(() => {
  if (!isPlaying || !strokeCount || isUserSeeking || totalLength === 0) return;

  let frame: number;
  const FRAMES_PER_STROKE = 2 * 60; // 2s per stroke at 60fps
  const totalFrames = strokeCount * FRAMES_PER_STROKE;
  const INCREMENT = totalLength / totalFrames;
  const animate = () => {
    setDrawProgress((prev) => {
      const next = prev + INCREMENT;
      if (next >= totalLength) {
        return 0; // loop
      }
      return next;
    });
    frame = requestAnimationFrame(animate);
  };
  frame = requestAnimationFrame(animate);
  return () => cancelAnimationFrame(frame);
  }, [isPlaying, isUserSeeking, totalLength]);

  // Update the stroke dash offset based on drawProgress to control animation progress 
  useEffect(() => {
    if (!svgContainerRef.current) return;
    const animatedPaths = svgContainerRef.current.querySelectorAll(
      "svg.acjk path[clip-path]"
    );
    let lengthCoveredByPreviousStrokes = 0;

    animatedPaths.forEach((path) => {
      const strokeStartPoint = lengthCoveredByPreviousStrokes;
      const strokeEndPoint = lengthCoveredByPreviousStrokes + SVG_STROKE_LENGTH;
      let strokeOffset = SVG_STROKE_LENGTH;
      if (drawProgress >= strokeEndPoint) {
        strokeOffset = 0;
      } else if (drawProgress > strokeStartPoint) {
        const amountDrawn = drawProgress - strokeStartPoint;
        strokeOffset = SVG_STROKE_LENGTH - amountDrawn;
      }
      (path as HTMLElement).style.strokeDashoffset = String(strokeOffset);
      lengthCoveredByPreviousStrokes += SVG_STROKE_LENGTH;
    });
  }, [isPlaying, drawProgress]);

  // Play/Pause animation
  const handlePlayPauseClick = () => {
    setIsPlaying(prevIsPlaying => !prevIsPlaying);
  };

  // Restart animation on SVG click
  const handleSvgClick = () => {
    setDrawProgress(0);
  }

  // Slider handlers for seeking
  const handleSliderChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setDrawProgress(Number(e.target.value));
  };
  const handleSliderMouseDown = () => setIsUserSeeking(true);
  const handleSliderMouseUp = () => setIsUserSeeking(false);
  const handleSliderTouchStart = () => setIsUserSeeking(true);
  const handleSliderTouchEnd = () => setIsUserSeeking(false);

  return (
    <div className="kanji-svg-container flex flex-col items-center">
      <div
        ref={svgContainerRef}
        className="cursor-pointer"
        onClick={handleSvgClick}
      />
      <div className="grid grid-rows-[40px_40px]">
        <div className="flex justify-center items-center">
        <button
          type="button"
          onClick={handlePlayPauseClick}
          className="w-6 h-6 flex items-center justify-center rounded-full border-2 border-foreground dark:bg-foreground dark:fill-background active:border-3"
        >
          {!isPlaying ? (
            // Pause Icon (Square)
            <svg className="w-6 h-6" viewBox="0 0 24 24">
              <path d="M6 6h12v12H6z" />
            </svg>
          ) : (
            // Play Icon (Triangle)
            <svg className="w-7 h-7" viewBox="0 0 24 24">
              <path d="M8 5v14l11-7z" />
            </svg>
          )}
        </button>
        </div>
        <div className="flex justify-center items-start">
        <AnimatePresence>
            <motion.input
              type="range"
              min="0"
              max={totalLength}
              value={drawProgress}
              onChange={handleSliderChange}
              onMouseDown={handleSliderMouseDown}
              onMouseUp={handleSliderMouseUp}
              onTouchStart={handleSliderTouchStart}
              onTouchEnd={handleSliderTouchEnd}
              className="w-20 h-4 p-1 cursor-pointer rounded-full ring-2 ring-foreground"
              disabled={totalLength === 0}
              initial={{ width: 20 }}
              animate={{ width: "5rem" }}
              exit={{ width: 0 }}
              transition={{ duration: 0.3 }}
            />
        </AnimatePresence>
        </div>
      </div>
    </div>
  );
}