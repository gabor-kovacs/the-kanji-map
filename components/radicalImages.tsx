import { useTheme } from "next-themes";
import React, { useState, useEffect } from "react";

import { useSprings, animated, config } from "react-spring";

interface Props {
  radicalImageArray: string[];
}

export const RadicalImages: React.FC<Props> = (props) => {
  const { radicalImageArray } = props;

  const { theme } = useTheme();

  const [index, setIndex] = useState(0);

  useEffect(() => {
    const interval = setInterval(
      () => setIndex((state) => (state + 1) % radicalImageArray.length),
      2500
    );
    return () => {
      clearInterval(interval);
    };
  }, []);

  const springs = useSprings(
    radicalImageArray.length,
    radicalImageArray.map((im, idx) => ({
      opacity: index === idx ? 1 : 0,
      config: config.molasses,
    }))
  );

  return (
    <>
      {springs.map((props, id) => (
        <animated.div
          key={id}
          style={{
            ...props,
            backgroundImage: `url(${radicalImageArray[id]})`,
            position: "absolute",
            width: "100%",
            height: "100%",
            backgroundRepeat: "no-repeat",
            backgroundSize: "contain",
            filter: theme === "dark" ? "invert(1)" : "invert(0)",
          }}
        />
      ))}
    </>
  );
};

export default RadicalImages;
