import Script from "next/script";
import { ThemeProvider } from "next-themes";
import { DefaultSeo } from "next-seo";
import type { AppProps } from "next/app";

function MyApp({ Component, pageProps }: AppProps) {
  return (
    <ThemeProvider>
      <DefaultSeo
        openGraph={{
          type: "website",
          locale: "en_US",
          url: "https://thekanjimap.com/",
          site_name: "The Kanji Map",
        }}
        twitter={{
          handle: "@handle",
          site: "@site",
          cardType: "summary_large_image",
        }}
      />
      <Script type="text/javascript" src="/handwriting.canvas.js"></Script>
      <Component {...pageProps} />
    </ThemeProvider>
  );
}

export default MyApp;
