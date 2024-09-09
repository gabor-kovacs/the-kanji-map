import * as React from "react";

interface TouchIsolatorProps {
  children: React.ReactNode;
}

export const TouchIsolator: React.FC<TouchIsolatorProps> = ({ children }) => {
  const ref = React.useRef<HTMLDivElement>(null);

  React.useLayoutEffect(() => {
    const stopBubbleAndPreventDefault = (e: TouchEvent) => {
      e.stopPropagation();
      e.preventDefault(); // This is important to prevent the default touch behavior like scrolling
    };

    const currentRef = ref.current;

    if (currentRef) {
      currentRef.addEventListener("touchstart", stopBubbleAndPreventDefault, {
        passive: false,
      });
      currentRef.addEventListener("touchmove", stopBubbleAndPreventDefault, {
        passive: false,
      });
      currentRef.addEventListener("touchend", stopBubbleAndPreventDefault, {
        passive: false,
      });
    }

    return () => {
      if (currentRef) {
        currentRef.removeEventListener(
          "touchstart",
          stopBubbleAndPreventDefault
        );
        currentRef.removeEventListener(
          "touchmove",
          stopBubbleAndPreventDefault
        );
        currentRef.removeEventListener("touchend", stopBubbleAndPreventDefault);
      }
    };
  }, []);

  return <div ref={ref}>{children}</div>;
};
