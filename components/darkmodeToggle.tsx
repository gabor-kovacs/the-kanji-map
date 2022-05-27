import { useTheme } from "next-themes";
import * as React from "react";
import { DarkModeSwitch } from "react-toggle-dark-mode";

interface Props {
  style?: React.CSSProperties | undefined;
  size?: number;
  moonColor?: string;
  sunColor?: string;
}

const DarkmodeToggle: React.FC<Props> = ({ size, style }) => {
  const [mounted, setMounted] = React.useState(false);
  const { theme, setTheme } = useTheme();
  // useEffect only runs on the client, so now we can safely show the UI
  React.useEffect(() => {
    setMounted(true);
  }, []);
  // * avoid hydration style mismatch
  if (!mounted) {
    return null;
  }
  const toggleDarkMode = (checked: boolean) => {
    checked && setTheme("dark");
    !checked && setTheme("light");
  };

  return (
    <DarkModeSwitch
      checked={theme === "dark"}
      onChange={toggleDarkMode}
      size={size}
      style={style}
    />
  );
};

export default DarkmodeToggle;
