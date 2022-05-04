import React, { useState, useEffect } from "react";

import styled from "@emotion/styled";
import Header from "./header";
// import Footer from "./footer";

// import SEO from "./seo";

interface Props {
  children: React.ReactNode;
}

const Layout: React.FC<Props> = ({ children }) => {
  return (
    <>
      {/* <SEO
        title={title ?? "Compass"}
        description={description ?? "Marketing & Consulting"}
      /> */}
      <Header />
      <main>{children}</main>
    </>
  );
};

export default Layout;
