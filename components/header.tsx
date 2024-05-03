import * as React from "react";
import Link from "next/link";
import styled from "@emotion/styled";
import { useRouter } from "next/router";
import { useTheme } from "next-themes";
import useActualTheme from "../lib/useActualTheme";
import InfoIcon from "@mui/icons-material/Info";
import LightModeIcon from "@mui/icons-material/LightMode";
import DarkModeIcon from "@mui/icons-material/DarkMode";

import Image from "next/image";
import LogoSVG from "./logo";

const Header: React.FC = () => {
  const router = useRouter();

  const { setTheme } = useTheme();
  const actualTheme = useActualTheme();

  return (
    <HeaderWrapper>
      <ImageWrapper>
        <Link href={`/`} passHref>
          <div style={{ padding: "0px 16px", height: "50px" }}>
            <LogoSVG />
          </div>
          <h1>The Kanji Map</h1>
        </Link>
      </ImageWrapper>
      <Icons>
        <Link href={router.pathname === "/about" ? "/" : "about"}>
          <InfoIcon className={router.pathname === "/about" ? "active" : ""} />
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
    padding: 8px 0px;
    height: 50px;
    height: 50px;
    width: 40px;
  }
`;

const Icons = styled.div`
  display: grid;
  cursor: pointer;
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
