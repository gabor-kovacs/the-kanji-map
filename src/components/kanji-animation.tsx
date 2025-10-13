import { Button } from "@/components/ui/button";
import { CirclePauseIcon, CirclePlayIcon } from "lucide-react";
import * as React from "react";
import { Slider } from "./ui/slider";

type Props = {
  svgContent: string;
  strokeCount: number | null;
};

const SVG_STROKE_LENGTH = 3337; // Default path length for each stroke.

export function KanjiStrokeAnimation({ svgContent, strokeCount }: Props) {
  const svgContainerRef = React.useRef<HTMLDivElement>(null);
  const [isPlaying, setIsPlaying] = React.useState(true);
  const [drawProgress, setDrawProgress] = React.useState(0);
  const [isUserSeeking, setIsUserSeeking] = React.useState(false);
  const totalLength = SVG_STROKE_LENGTH * (strokeCount || 0);
  const [svgInjected, setSvgInjected] = React.useState(false);

  // Modify the SVG content to remove default animation
  const modifiedSvgContent = React.useMemo(() => {
    if (!svgContent) return "";
    return svgContent.replace(
      /animation:zk var\(--t\) linear forwards var\(--d\);/,
      "animation: none;"
    );
  }, [svgContent]); // only recompute when svgContent changes

  // only inject the modified SVG once
  React.useEffect(() => {
    if (svgContainerRef.current && !svgInjected) {
      svgContainerRef.current.innerHTML = modifiedSvgContent;
      setSvgInjected(true);
    }
  }, [modifiedSvgContent, svgInjected]);

  // Animation loop
  React.useEffect(() => {
    if (!isPlaying || !strokeCount || isUserSeeking || totalLength === 0)
      return;

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
  React.useEffect(() => {
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
    setIsPlaying((prevIsPlaying) => !prevIsPlaying);
  };

  // Restart animation on SVG click
  const handleSvgClick = () => {
    setDrawProgress(0);
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
      <div className="flex flex-row items-center gap-2 mt-2">
        <Button
          variant="link"
          size="icon"
          onClick={handlePlayPauseClick}
          className="h-6 w-6 p-0 m-0"
        >
          {isPlaying ? (
            <CirclePauseIcon className="h-4 w-4" />
          ) : (
            <CirclePlayIcon className="h-4 w-4" />
          )}
        </Button>
        <Slider
          min={0}
          max={totalLength}
          value={[drawProgress]}
          onValueChange={(vals: number[]) => setDrawProgress(vals[0])}
          className="w-20 h-4"
          disabled={totalLength === 0}
          onPointerDown={handleSliderMouseDown}
          onPointerUp={handleSliderMouseUp}
          onTouchStart={handleSliderTouchStart}
          onTouchEnd={handleSliderTouchEnd}
        />
      </div>
    </div>
  );
}
