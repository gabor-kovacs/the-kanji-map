import { useTheme } from "next-themes";
import * as React from "react";
import { motion, AnimatePresence } from "framer-motion";

export const RadicalImages = ({
  radicalImageArray,
}: {
  radicalImageArray: string[];
}) => {
  const [index, setIndex] = React.useState(0);
  const [invert, setInvert] = React.useState(0);

  const { resolvedTheme } = useTheme();

  React.useEffect(() => {
    resolvedTheme === "dark" ? setInvert(1) : setInvert(0);
  }, [resolvedTheme]);

  React.useEffect(() => {
    const interval = setInterval(
      () => setIndex((state) => (state + 1) % radicalImageArray.length),
      2500
    );
    return () => {
      clearInterval(interval);
    };
  }, [radicalImageArray.length]);

  return (
    <div className="relative w-full h-full">
      <AnimatePresence>
        {radicalImageArray.map((image, idx) =>
          idx === index ? (
            <motion.div
              key={idx}
              initial={{ opacity: 0 }}
              animate={{ opacity: 1 }}
              exit={{ opacity: 0 }}
              transition={{ duration: 1, ease: "easeInOut" }}
              className={`absolute w-full h-full bg-no-repeat bg-contain ${
                invert ? "invert" : ""
              }`}
              style={{
                backgroundImage: `url(${image})`,
              }}
            />
          ) : null
        )}
      </AnimatePresence>
    </div>
  );
};

export default RadicalImages;
