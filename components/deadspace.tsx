import * as React from "react";

interface DeadSpaceProps {
  children: React.ReactNode;
}
const DeadSpace: React.FC<DeadSpaceProps> = ({ children }) => {
  const ref = React.useRef<HTMLDivElement>(null);
  React.useLayoutEffect(() => {
    const stopBubble = (e: any) => e.stopPropagation();
    ref.current?.addEventListener("touchmove", stopBubble);
    return () => {
      ref.current?.removeEventListener("touchmove", stopBubble);
    };
  }, []);

  return <div ref={ref}>{children}</div>;
};

export default DeadSpace;
