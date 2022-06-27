import * as React from "react";
import styled from "@emotion/styled";

import TableChartIcon from "@mui/icons-material/TableChart";
import HomeIcon from "@mui/icons-material/Home";
import InfoIcon from "@mui/icons-material/Info";

import Logo from "../public/logo.svg";
import Link from "next/link";
import { useRouter } from "next/router";

import { css } from "@emotion/react";
import { useTheme } from "next-themes";

import LightModeIcon from "@mui/icons-material/LightMode";
import DarkModeIcon from "@mui/icons-material/DarkMode";

const Header: React.FC = () => {
  const router = useRouter();

  const { theme, systemTheme, setTheme } = useTheme();

  const [hovered, setHovered] = React.useState(false);

  const [actualTheme, setActualTheme] = React.useState<null | "light" | "dark">(
    null
  );

  React.useEffect(() => {
    if (theme === "light" || (theme === "system" && systemTheme === "light")) {
      setActualTheme("light");
    }
    if (theme === "dark" || (theme === "system" && systemTheme === "dark")) {
      setActualTheme("dark");
    }
  }, [theme, systemTheme]);

  return (
    <HeaderWrapper>
      <ImageWrapper>
        <Link href={`/`} passHref>
          <a>
            <Logo />
            <h1>The Kanji Map</h1>
          </a>
        </Link>
      </ImageWrapper>
      <Icons>
        <Link href={router.pathname === "/about" ? "/" : "about"}>
          <a>
            <InfoIcon
              className={router.pathname === "/about" ? "active" : ""}
            />
          </a>
        </Link>
        {actualTheme === "light" && (
          <DarkModeIcon onClick={() => setTheme("dark")} />
        )}
        {actualTheme === "dark" && (
          <LightModeIcon onClick={() => setTheme("light")} />
        )}
      </Icons>
    </HeaderWrapper>
  );
};

export default Header;

// * STYLES **************************************************************************************************

const HeaderWrapper = styled.div`
  height: 50px;
  display: flex;
  align-items: center;
  justify-content: space-between;

  border-bottom: 1px solid var(--color-lighter);

  h1 {
    display: inline-block;
    white-space: nowrap;
    height: 50px;
    margin: 0;
    line-height: 50px;
    font-size: 1.2rem;
  }

  a {
    color: var(--color-light);
    margin-right: 13px;
    margin-left: 8px;
    &:hover {
      color: var(--color-foreground);
    }
    &.active {
      color: var(--color-foreground);
    }
  }
`;

const ImageWrapper = styled.div`
  a {
    display: flex;
    text-decoration: none;
    color: var(--foreground);
    &:hover {
      color: var(--foreground);
    }
  }

  svg {
    padding: 8px 16px;
    height: 50px;
    height: 50px;
    width: 62.66px;
  }
`;
const Icons = styled.div`
  display: grid;
  grid-template-columns: 50px 50px;
  place-items: center;
  svg {
    color: var(--color-light);
    &:hover {
      color: var(--color-foreground);
    }
  }

  a {
    display: block;
    padding: 0;
    margin: 0;
    height: 24px;
  }

  a .active {
    color: var(--color-foreground) !important;
  }
`;
