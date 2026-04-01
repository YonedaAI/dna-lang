#!/usr/bin/env node
/**
 * Pre-render all KaTeX math in HTML paper files at build time.
 * Replaces <span class="math inline">...</span> and <span class="math display">...</span>
 * with pre-rendered KaTeX HTML. No client-side JS needed for math after this.
 */

import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import katex from "katex";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const htmlDir = path.join(__dirname, "..", "public", "read");

function cleanTex(raw) {
  // Strip HTML tags (e.g. <code>ProteinCode</code> -> ProteinCode)
  let tex = raw.replace(/<[^>]+>/g, "");
  // Decode HTML entities
  tex = tex.replace(/&amp;/g, "&")
    .replace(/&lt;/g, "<")
    .replace(/&gt;/g, ">")
    .replace(/&quot;/g, '"')
    .replace(/&#39;/g, "'")
    .replace(/&#8614;/g, "\\mapsto")
    .replace(/&#x27;/g, "'")
    .replace(/&middot;/g, "\\cdot")
    .replace(/&times;/g, "\\times")
    .replace(/&le;/g, "\\le")
    .replace(/&ge;/g, "\\ge")
    .replace(/&rarr;/g, "\\to")
    .replace(/&rArr;/g, "\\Rightarrow")
    .replace(/&infin;/g, "\\infty")
    .replace(/&hellip;/g, "\\ldots");
  return tex;
}

function renderMathSpans(html) {
  let rendered = 0;
  let errors = 0;

  // Replace <span class="math inline">...</span> ([\s\S]*? to handle nested tags)
  html = html.replace(
    /<span class="math inline">([\s\S]*?)<\/span>/g,
    (match, rawTex) => {
      try {
        const tex = cleanTex(rawTex);
        rendered++;
        return katex.renderToString(tex, {
          displayMode: false,
          throwOnError: false,
          output: "html",
        });
      } catch {
        errors++;
        return match;
      }
    }
  );

  // Replace <span class="math display">...</span>
  html = html.replace(
    /<span class="math display">([\s\S]*?)<\/span>/g,
    (match, rawTex) => {
      try {
        let tex = cleanTex(rawTex);
        // Strip display-mode delimiters if present
        tex = tex.replace(/^\\\[/, "").replace(/\\\]$/, "").trim();
        rendered++;
        return katex.renderToString(tex, {
          displayMode: true,
          throwOnError: false,
          output: "html",
        });
      } catch {
        errors++;
        return match;
      }
    }
  );

  return { html, rendered, errors };
}

function fixTikzDiagrams(html) {
  // Replace raw tikz-cd commutative diagrams with styled placeholders.
  // These can't be rendered in HTML — pandoc passes them through as raw LaTeX.

  // Pattern: \begin{equation} ... \begin{tikzcd} ... \end{tikzcd} ... \end{equation}
  // Also standalone \begin{tikzcd} ... \end{tikzcd}
  // They may be wrapped in <p> tags or <span class="math display">

  // First, try to extract a meaningful description from the surrounding context
  // Replace full equation+tikzcd blocks (may span multiple paragraphs)
  html = html.replace(
    /<p>\s*\\begin\{equation\}[\s\S]*?\\end\{equation\}\s*<\/p>/g,
    `<div style="background:var(--surface2,#1a1a2e);border:1px solid var(--border,#2a2a3e);border-left:3px solid var(--accent,#00b894);border-radius:8px;padding:1rem 1.2rem;margin:1.5rem 0;font-size:.85rem;color:var(--text-dim,#9494a7)"><strong style="color:var(--accent2,#55efc4)">Commutative Diagram</strong><br>This diagram is rendered in the PDF version of the paper. <a href="#" onclick="window.history.back();return false" style="color:var(--accent,#00b894)">Download PDF</a> for the full visual.</div>`
  );

  // Standalone tikzcd blocks
  html = html.replace(
    /<p>\s*\\begin\{tikzcd\}[\s\S]*?\\end\{tikzcd\}\s*<\/p>/g,
    `<div style="background:var(--surface2,#1a1a2e);border:1px solid var(--border,#2a2a3e);border-left:3px solid var(--accent,#00b894);border-radius:8px;padding:1rem 1.2rem;margin:1.5rem 0;font-size:.85rem;color:var(--text-dim,#9494a7)"><strong style="color:var(--accent2,#55efc4)">Commutative Diagram</strong><br>This diagram is rendered in the PDF version of the paper.</div>`
  );

  // tikzcd inside display math spans that KaTeX couldn't render
  html = html.replace(
    /<span class="katex-error"[^>]*>[\s\S]*?<\/span>/g,
    (match) => {
      // Only replace if it looks like a tikz diagram or very long formula
      if (match.includes("tikzcd") || match.includes("\\arrow") || match.length > 500) {
        return `<span style="background:var(--surface2,#1a1a2e);border:1px solid var(--border,#2a2a3e);border-left:3px solid var(--accent,#00b894);border-radius:8px;padding:.5rem .8rem;margin:.3rem 0;font-size:.8rem;color:var(--text-dim,#9494a7);display:inline-block"><strong style="color:var(--accent2,#55efc4)">Diagram</strong> — see PDF</span>`;
      }
      // For small KaTeX errors, just dim them
      return `<span style="color:var(--text-dim,#9494a7);font-size:.85em;font-style:italic">[formula — see PDF]</span>`;
    }
  );

  // Any remaining raw \begin{tikzpicture} or \begin{tikzcd} blocks
  html = html.replace(
    /\\begin\{tikz(?:cd|picture)\}[\s\S]*?\\end\{tikz(?:cd|picture)\}/g,
    `<span style="color:var(--text-dim,#9494a7);font-size:.85em">[diagram — see PDF]</span>`
  );

  // Clean up any remaining raw \begin{equation}...\end{equation} that contain only broken LaTeX
  html = html.replace(
    /\\begin\{equation\}[\s\S]*?\\end\{equation\}/g,
    (match) => {
      if (match.includes("katex") || match.includes("class=")) return match; // already processed
      return `<span style="color:var(--text-dim,#9494a7);font-size:.85em">[equation — see PDF]</span>`;
    }
  );

  return html;
}

function fixMobileCSS(html) {
  // Add mobile-friendly overrides to the existing <style> block
  const mobileCSS = `
/* Mobile readability fixes */
@media(max-width:900px){
  .main-content{padding:1.2rem 1rem !important;margin-left:0 !important}
  .paper-header h1{font-size:1.4rem !important}
  pre,.sourceCode{font-size:.72rem !important;padding:.8rem !important}
  .math-display,.katex-display{overflow-x:auto;max-width:100%}
  .katex{font-size:0.9em !important}
  table{font-size:.75rem;display:block;overflow-x:auto}
  .theorem,.definition,.proposition,.lemma,.corollary,.remark{padding:.8rem 1rem !important}
  blockquote{padding:.6rem .8rem !important}
  h2{font-size:1.2rem !important}
  h3{font-size:1.05rem !important}
  body{font-size:15px !important;line-height:1.65 !important}
  .paper-meta-row{flex-direction:column;gap:.3rem}
}
@media(max-width:480px){
  .main-content{padding:1rem .75rem !important}
  .paper-header h1{font-size:1.2rem !important}
  pre,.sourceCode{font-size:.65rem !important}
  .katex{font-size:0.8em !important}
  body{font-size:14px !important}
}
/* Ensure math doesn't overflow */
.katex-display{overflow-x:auto;overflow-y:hidden;padding:0.5rem 0}
.katex-display>.katex{max-width:100%;overflow-x:auto;overflow-y:hidden}
`;

  // Insert before </style>
  html = html.replace("</style>", mobileCSS + "\n</style>");
  return html;
}

// Process all HTML files
const files = fs.readdirSync(htmlDir).filter((f) => f.endsWith(".html"));
console.log(`Processing ${files.length} HTML files...`);

let totalRendered = 0;
let totalErrors = 0;

for (const file of files) {
  const filePath = path.join(htmlDir, file);
  let html = fs.readFileSync(filePath, "utf-8");

  // Pre-render math
  const { html: mathHtml, rendered, errors } = renderMathSpans(html);
  html = mathHtml;
  totalRendered += rendered;
  totalErrors += errors;

  // Fix tikz-cd diagrams (replace with styled placeholders)
  html = fixTikzDiagrams(html);

  // Fix mobile CSS
  html = fixMobileCSS(html);

  // Remove the KaTeX script tags (no longer needed — math is pre-rendered)
  html = html.replace(
    /<link rel="stylesheet" href="https:\/\/cdn\.jsdelivr\.net\/npm\/katex[^>]*>/g,
    ""
  );
  html = html.replace(
    /<script[^>]*katex[^>]*>[\s\S]*?<\/script>/g,
    ""
  );

  // Add KaTeX CSS (still needed for styling the rendered HTML)
  html = html.replace(
    "<head>",
    '<head>\n<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.css">'
  );

  fs.writeFileSync(filePath, html);
  console.log(`  ${file}: ${rendered} math elements rendered, ${errors} errors`);
}

console.log(
  `\nDone. ${totalRendered} total math elements rendered, ${totalErrors} errors.`
);
