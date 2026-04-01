"use client";

import Script from "next/script";

// tocScript is extracted at build time from our own pandoc-generated HTML papers.
export default function KatexLoader({ tocScript }: { tocScript: string }) {
  function renderAllMath() {
    if (typeof window === "undefined") return;
    const katex = (window as any).katex;
    if (!katex) return;

    // Pandoc wraps math in <span class="math inline"> and <span class="math display">
    // We need to render each span directly with KaTeX
    document.querySelectorAll("span.math.inline").forEach((el) => {
      try {
        katex.render(el.textContent || "", el as HTMLElement, {
          displayMode: false,
          throwOnError: false,
        });
      } catch {
        // leave as-is if KaTeX can't parse it
      }
    });

    document.querySelectorAll("span.math.display").forEach((el) => {
      try {
        katex.render(el.textContent || "", el as HTMLElement, {
          displayMode: true,
          throwOnError: false,
        });
      } catch {
        // leave as-is
      }
    });

    // Also handle any remaining $...$ in text via auto-render
    if ((window as any).renderMathInElement) {
      (window as any).renderMathInElement(document.body, {
        delimiters: [
          { left: "$$", right: "$$", display: true },
          { left: "\\[", right: "\\]", display: true },
          { left: "$", right: "$", display: false },
          { left: "\\(", right: "\\)", display: false },
        ],
        throwOnError: false,
      });
    }
  }

  return (
    <>
      <Script
        src="https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.js"
        strategy="afterInteractive"
      />
      <Script
        src="https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/contrib/auto-render.min.js"
        strategy="afterInteractive"
        onLoad={() => renderAllMath()}
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
