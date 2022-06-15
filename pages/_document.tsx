import Document, {
  Html,
  Head,
  Main,
  NextScript,
  DocumentContext,
} from "next/document";

import Script from "next/script";

import { globalStyles } from "../styles/global";

class MyDocument extends Document {
  static async getInitialProps(ctx: DocumentContext) {
    const initialProps = await Document.getInitialProps(ctx);
    return { ...initialProps };
  }

  render() {
    return (
      <>
        {/* <Script id="show-banner" strategy="beforeInteractive">
          {`console.log("BRUUUUUUUUUUUUUUUUUUUH");`}
        </Script> */}
        <Html>
          <Head>{globalStyles}</Head>
          <body>
            <Main />
            <NextScript />
            {/* <Script
              src="/handwriting.canvas.js"
              strategy="beforeInteractive"
            ></Script> */}
          </body>
        </Html>
      </>
    );
  }
}

export default MyDocument;
