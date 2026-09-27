import { Button } from "@/components/ui/button";
import { CirclePauseIcon, CirclePlayIcon } from "lucide-react";
import * as React from "react";
import { Slider } from "./ui/slider";

type Props = {
  svgContent: string;
  strokeCount: number | null;
};

const SVG_STROKE_LENGTH = 3337; // Default path length for each stroke.
const MILLIS_PER_STROKE = 2000; // 2s per stroke
const SLIDER_FLUSH_INTERVAL_MS = 50; // keep the slider in sync at 20Hz, not 60Hz

export function KanjiStrokeAnimation({ svgContent, strokeCount }: Props) {
  const svgContainerRef = React.useRef<HTMLButtonElement>(null);
  const strokePathsRef = React.useRef<SVGPathElement[]>([]);
  const progressRef = React.useRef(0);
  const isUserSeeking = React.useRef(false);
  const [isPlaying, setIsPlaying] = React.useState(true);
  const [drawProgress, setDrawProgress] = React.useState(0);
  const totalLength = SVG_STROKE_LENGTH * (strokeCount || 0);

  // Modify the SVG content to remove default animation
  const modifiedSvgContent = React.useMemo(() => {
    if (!svgContent) return "";
    return svgContent.replace(
      /animation:zk var\(--t\) linear forwards var\(--d\);/,
      "animation: none;",
    );
  }, [svgContent]); // only recompute when svgContent changes

  // Apply animation progress by writing to the stroke paths directly,
  // without re-rendering the component
  const applyProgress = React.useCallback((progress: number) => {
    let lengthCoveredByPreviousStrokes = 0;

    for (const path of strokePathsRef.current) {
      const strokeStartPoint = lengthCoveredByPreviousStrokes;
      const strokeEndPoint = lengthCoveredByPreviousStrokes + SVG_STROKE_LENGTH;
      let strokeOffset = SVG_STROKE_LENGTH;
      if (progress >= strokeEndPoint) {
        strokeOffset = 0;
      } else if (progress > strokeStartPoint) {
        const amountDrawn = progress - strokeStartPoint;
        strokeOffset = SVG_STROKE_LENGTH - amountDrawn;
      }
      path.style.strokeDashoffset = String(strokeOffset);
      lengthCoveredByPreviousStrokes += SVG_STROKE_LENGTH;
    }
  }, []);

  // Inject the SVG and cache the stroke paths.
  // Re-runs whenever the content changes (e.g. the kanji changes without the
  // page being remounted), so the animation always shows the current kanji.
  React.useEffect(() => {
    const container = svgContainerRef.current;
    if (!container || !modifiedSvgContent) return;

    container.innerHTML = modifiedSvgContent;
    strokePathsRef.current = Array.from(
      container.querySelectorAll<SVGPathElement>("svg.acjk path[clip-path]"),
    );
    progressRef.current = 0;
    setDrawProgress(0);
    applyProgress(0);
  }, [modifiedSvgContent, applyProgress]);

  // Animation loop.
  // The per-frame work is a direct DOM write (strokeDashoffset) inside
  // requestAnimationFrame - no React state update per frame. The slider is
  // synced a few times per second, which is enough for its thumb to follow.
  React.useEffect(() => {
    if (!isPlaying || !strokeCount || totalLength === 0) return;

    const incrementPerMillisecond = totalLength / (strokeCount * MILLIS_PER_STROKE);
    const maxDeltaMs = (1000 / 60) * 4; // clamp gaps from backgrounded tabs

    let frame: number;
    let lastTime = performance.now();
    let lastFlushTime = lastTime;

    const animate = (now: number) => {
      const deltaMs = Math.min(Math.max(now - lastTime, 0), maxDeltaMs);
      lastTime = now;

      if (!isUserSeeking.current) {
        progressRef.current += incrementPerMillisecond * deltaMs;
        if (progressRef.current >= totalLength) {
          progressRef.current = 0; // loop
        }
        applyProgress(progressRef.current);
      }

      if (now - lastFlushTime >= SLIDER_FLUSH_INTERVAL_MS) {
        lastFlushTime = now;
        setDrawProgress(progressRef.current);
      }

      frame = requestAnimationFrame(animate);
    };

    frame = requestAnimationFrame(animate);
    return () => cancelAnimationFrame(frame);
  }, [isPlaying, strokeCount, totalLength, applyProgress]);

  // The slider captures the pointer, so a pointercancel (gesture or system
  // interruption) or a window blur mid-drag can drop the pointerup, which
  // would leave the seek flag set and freeze the animation loop; clear the
  // flag on both edges.
  React.useEffect(() => {
    const onBlur = () => {
      isUserSeeking.current = false;
    };
    window.addEventListener("blur", onBlur);
    return () => window.removeEventListener("blur", onBlur);
  }, []);

  // Play/Pause animation
  const handlePlayPauseClick = () => {
    setIsPlaying((prevIsPlaying) => !prevIsPlaying);
  };

  // Restart animation on SVG click
  const handleSvgClick = () => {
    progressRef.current = 0;
    setDrawProgress(0);
    applyProgress(0);
  };

  const handleSeek = (value: number | readonly number[]) => {
    const values = Array.isArray(value) ? value : [value];
    const next = values[0] ?? 0;
    progressRef.current = next;
    setDrawProgress(next);
    applyProgress(next);
  };

  const handleSliderMouseDown = () => {
    isUserSeeking.current = true;
  };
  const handleSliderMouseUp = () => {
    isUserSeeking.current = false;
  };
  const handleSliderPointerCancel = () => {
    isUserSeeking.current = false;
  };
  const handleSliderTouchStart = () => {
    isUserSeeking.current = true;
  };
  const handleSliderTouchEnd = () => {
    isUserSeeking.current = false;
  };

  return (
    <div className="kanji-svg-container flex flex-col items-center">
      <button
        type="button"
        ref={svgContainerRef}
        className="kanji-stroke-svg cursor-pointer"
        onClick={handleSvgClick}
      />
      <div className="flex flex-row items-center gap-2 mt-2">
        <Button
          variant="icon-muted"
          size="icon-xs"
          onClick={handlePlayPauseClick}
          className="shrink-0"
        >
          {isPlaying ? (
            <CirclePauseIcon className="size-5" />
          ) : (
            <CirclePlayIcon className="size-5" />
          )}
        </Button>
        <Slider
          min={0}
          max={totalLength}
          value={[drawProgress]}
          onValueChange={handleSeek}
          className="w-20 h-4"
          disabled={totalLength === 0}
          onPointerDown={handleSliderMouseDown}
          onPointerUp={handleSliderMouseUp}
          onPointerCancel={handleSliderPointerCancel}
          onTouchStart={handleSliderTouchStart}
          onTouchEnd={handleSliderTouchEnd}
        />
      </div>
    </div>
  );
}
