import * as React from "react";
import { useTheme } from "next-themes";

const useActualTheme = () => {
  const { theme, systemTheme } = useTheme();
  const [actualTheme, setActualTheme] = React.useState<"light" | "dark">(
    "light"
  );
  React.useLayoutEffect(() => {
    if (theme === "light" || (theme === "system" && systemTheme === "light")) {
      setActualTheme("light");
    }
    if (theme === "dark" || (theme === "system" && systemTheme === "dark")) {
      setActualTheme("dark");
    }
  }, [theme, systemTheme]);

  return actualTheme;
};

export default useActualTheme;
