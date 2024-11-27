import { ThemeProvider } from "@/components/theme-provider";
import { TooltipProvider } from "@/components/ui/tooltip";
import { Provider as JotaiProvider } from "jotai";
import { Noto_Sans_JP } from "next/font/google";
import "../styles/globals.css";
import { Metadata } from "next";

export const metadata: Metadata = {
  title: { default: "The Kanji Map", template: "%s | The Kanji Map" },
  description:
    "The Kanji Map is a Japanese language learning tool that shows kanji information and decomposition in graph form.",
  openGraph: {
    title: "The Kanji Map",
    description:
      "The Kanji Map is a Japanese language learning tool that shows kanji information and decomposition in graph form.",
  },
  icons: {
    icon: "/favicon.ico",
    apple: "/apple-touch-icon.png",
  },
  manifest: "/manifest.json",
  // themeColor: "#2b99cf",
};

const notoSansJp = Noto_Sans_JP({
  weight: "variable",
  subsets: ["latin-ext"],
  display: "swap",
  preload: true,
  variable: "--font-sans",
  adjustFontFallback: false,
});

export default function RootLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <html lang="en" suppressHydrationWarning className={notoSansJp.className}>
      <body className="w-screen h-screen overflow-hidden bg-background text-foreground selection:bg-primary">
        <ThemeProvider
          attribute="class"
          defaultTheme="system"
          enableSystem
          disableTransitionOnChange
        >
          <TooltipProvider>
            <JotaiProvider>
              <div className="isolate size-full">{children}</div>
            </JotaiProvider>
          </TooltipProvider>
        </ThemeProvider>
      </body>
    </html>
  );
}
