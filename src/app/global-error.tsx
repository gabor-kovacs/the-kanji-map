"use client";
import { ThemeProvider } from "@/components/theme-provider";
import { Button } from "@/components/ui/button";
import { Noto_Sans_JP } from "next/font/google";
import { Header } from "@/components/header";
import "../styles/globals.css";

const notoSansJp = Noto_Sans_JP({
  weight: "variable",
  subsets: ["latin-ext"],
  display: "swap",
  preload: true,
  variable: "--font-sans",
  adjustFontFallback: false,
});

export default function GlobalError({ reset }: { reset: () => void }) {
  return (
    <html lang="en" suppressHydrationWarning className={notoSansJp.className}>
      <body className="w-screen h-screen overflow-hidden bg-background text-foreground selection:bg-primary">
        <ThemeProvider
          attribute="class"
          defaultTheme="system"
          enableSystem
          disableTransitionOnChange
        >
          <Header className="w-full" />
          <div className="size-full grid place-items-center gap-4">
            <h1 className="font-bold text-xl">Something went wrong!</h1>
            <Button onClick={() => reset()}>Try again</Button>
          </div>
        </ThemeProvider>
      </body>
    </html>
  );
}
