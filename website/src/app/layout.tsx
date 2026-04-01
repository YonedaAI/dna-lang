import type { Metadata } from "next";
import "./globals.css";

const siteUrl =
  process.env.NEXT_PUBLIC_SITE_URL ||
  process.env.VERCEL_PROJECT_PRODUCTION_URL
    ? `https://${process.env.VERCEL_PROJECT_PRODUCTION_URL}`
    : "https://dna-lang-01.vercel.app";

export const metadata: Metadata = {
  title: "DNA-Lang | A Typed Programming Language for Biological Systems",
  description:
    "8 research papers (207 pages) + Haskell implementations mapping DNA sequence categories to programming language constructs. By the YonedaAI Research Collective.",
  metadataBase: new URL(siteUrl),
  openGraph: {
    title: "DNA-Lang",
    description:
      "A Typed Programming Language for Biological Systems. 8 papers, 207 pages, Haskell implementations.",
    url: siteUrl,
    siteName: "DNA-Lang",
    type: "website",
    images: [
      {
        url: "/og-image.png",
        width: 1200,
        height: 630,
        alt: "DNA-Lang: A Typed Programming Language for Biological Systems",
      },
    ],
  },
  twitter: {
    card: "summary_large_image",
    title: "DNA-Lang",
    description:
      "A Typed Programming Language for Biological Systems. 8 papers, 207 pages.",
    images: ["/og-image.png"],
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
