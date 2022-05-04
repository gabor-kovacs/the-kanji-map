import styled from "@emotion/styled";

import TableChartIcon from "@mui/icons-material/TableChart";
import HomeIcon from "@mui/icons-material/Home";
import InfoIcon from "@mui/icons-material/Info";

import Logo from "../public/images/logo.svg";
import Link from "next/link";
import { useRouter } from "next/router";

const Header: React.FC = () => {
  const router = useRouter();

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
      <div>
        <Link href="/">
          <a className={router.pathname === "/" ? "active" : ""}>
            <HomeIcon />
          </a>
        </Link>
        <Link href="/kanji">
          <a className={router.pathname === "/kanji" ? "active" : ""}>
            <TableChartIcon />
          </a>
        </Link>
        <Link href="/about">
          <a className={router.pathname === "/about" ? "active" : ""}>
            <InfoIcon />
          </a>
        </Link>
      </div>
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

  svg {
    padding: 8px 16px;
    height: 50px;
    height: 50px;
    width: 62.66px;
  }

  h1 {
    display: inline-block;
    white-space: nowrap;
    height: 50px;
    margin: 0;
    line-height: 50px;
    font-size: 1.2rem;
  }

  a {
    color: #c4c4c4;
    margin-right: 13px;
    margin-left: 8px;
    &:hover {
      color: #dcdcdc;
    }
    &.active {
      color: #2b99cf;
    }
  }
`;

const ImageWrapper = styled.div`
  a {
    display: flex;
    text-decoration: none;
    color: #212121;
    &:hover {
      color: #212121;
    }
  }
`;
