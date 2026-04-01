"use client";

import Script from "next/script";

// tocScript is extracted at build time from our own pandoc-generated HTML papers.
// It contains the sidebar TOC builder — trusted, static content.
export default function KatexLoader({ tocScript }: { tocScript: string }) {
  return (
    <>
      <Script
        src="https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.js"
        strategy="afterInteractive"
      />
      <Script
        src="https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/contrib/auto-render.min.js"
        strategy="afterInteractive"
        onLoad={() => {
          if (typeof window !== "undefined" && (window as any).renderMathInElement) {
            (window as any).renderMathInElement(document.body, {
              delimiters: [
                { left: "$$", right: "$$", display: true },
                { left: "\\[", right: "\\]", display: true },
                { left: "$", right: "$", display: false },
                { left: "\\(", right: "\\)", display: false },
              ],
            });
          }
        }}
      />
      {tocScript && (
        <Script
          id="toc-builder"
          strategy="afterInteractive"
          dangerouslySetInnerHTML={{ __html: tocScript }}
        />
      )}
    </>
  );
}
