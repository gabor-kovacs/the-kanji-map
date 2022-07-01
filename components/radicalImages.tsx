import * as React from "react";
import { useSprings, animated, config } from "react-spring";
import useActualTheme from "../lib/useActualTheme";

interface Props {
  radicalImageArray: string[];
}

export const RadicalImages: React.FC<Props> = (props) => {
  const { radicalImageArray } = props;

  const [index, setIndex] = React.useState(0);
  const [invert, setInvert] = React.useState(0);

  const actualTheme = useActualTheme();

  React.useEffect(() => {
    actualTheme === "dark" && setInvert(1);
    actualTheme === "light" && setInvert(0);
  }, [actualTheme]);

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
