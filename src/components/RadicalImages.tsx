import React, { useState, useEffect } from "react";
import PropTypes from "prop-types";
import { useSprings, animated, config } from "react-spring";

interface Props {
  radicalImageArray: string[];
}

export const RadicalImages: React.FC<Props> = (props) => {
  const { radicalImageArray } = props;

  const [index, setIndex] = useState(0);

  useEffect(() => {
    const interval = setInterval(
      () => setIndex((state) => (state + 1) % radicalImageArray.length),
      3000
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
            width: "100px",
            height: "100px",
            backgroundRepeat: "no-repeat",
            backgroundSize: "contain",
          }}
        />
      ))}
    </>
  );
};

export default RadicalImages;
RadicalImages.propTypes = {
  radicalImageArray: PropTypes.arrayOf(PropTypes.string).isRequired,
};
