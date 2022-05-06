import * as React from "react";
import Layout from "../components/layout";

import Head from "next/head";

import FirstChart from "../components/graphFull";
import { getTheMap } from "../lib/lib";

const Page: React.FC = () => {
  const [data, setData] = React.useState(null);

  React.useEffect(() => {
    const map = getTheMap();
    setData(map);
  }, []);

  return (
    <Layout>
      <Head>
        <title>{"The Kanji Map"}</title>
      </Head>
      {data && <FirstChart data={data} />}
    </Layout>
  );
};

export default Page;
