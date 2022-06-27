import { useTheme } from "next-themes";
import * as React from "react";

import { useSprings, animated, config } from "react-spring";

interface Props {
  radicalImageArray: string[];
}

export const RadicalImages: React.FC<Props> = (props) => {
  const { radicalImageArray } = props;

  const { theme, systemTheme } = useTheme();

  const [index, setIndex] = React.useState(0);
  const [invert, setInvert] = React.useState(0);

  React.useEffect(() => {
    console.log(theme);
    console.log(systemTheme);
    theme === "dark" && setInvert(1);
    theme === "light" && setInvert(0);
    if (theme === "system") {
      systemTheme === "dark" && setInvert(1);
      systemTheme === "light" && setInvert(0);
    }
  }, [theme, systemTheme]);

  React.useEffect(() => {
    const interval = setInterval(
      () => setIndex((state) => (state + 1) % radicalImageArray.length),
      2500
    );
    return () => {
      clearInterval(interval);
    };
  }, [radicalImageArray.length]);

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
            filter: `invert(${invert})`,
          }}
        />
      ))}
    </>
  );
};

export default RadicalImages;
