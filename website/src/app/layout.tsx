import type { Metadata } from "next";
import "./globals.css";

export const metadata: Metadata = {
  title: "DNA-Lang | A Typed Programming Language for Biological Systems",
  description:
    "8 research papers + Haskell implementations mapping DNA sequence categories to programming language constructs. By the YonedaAI Research Collective.",
  openGraph: {
    title: "DNA-Lang",
    description: "A Typed Programming Language for Biological Systems",
    url: "https://yonedaai.github.io/dna-lang/",
    siteName: "DNA-Lang",
    type: "website",
  },
};

export default function RootLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <html lang="en">
      <head>
        <link rel="preconnect" href="https://fonts.googleapis.com" />
        <link
          rel="preconnect"
          href="https://fonts.gstatic.com"
          crossOrigin=""
        />
        <link
          href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&family=JetBrains+Mono:wght@400;500&display=swap"
          rel="stylesheet"
        />
      </head>
      <body
        style={{ fontFamily: "Inter, system-ui, -apple-system, sans-serif" }}
      >
        {children}
      </body>
    </html>
  );
}
