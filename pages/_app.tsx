import type { AppProps } from "next/app";
import { ThemeProvider } from "next-themes";
import Script from "next/script";

function MyApp({ Component, pageProps }: AppProps) {
  return (
    <ThemeProvider>
      <Script type="text/javascript" src="/handwriting.canvas.js"></Script>
      <Component {...pageProps} />
    </ThemeProvider>
  );
}

export default MyApp;
