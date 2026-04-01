import { notFound } from "next/navigation";
import type { Metadata } from "next";
import fs from "fs";
import path from "path";

const slugs = [
  "coding-sequences",
  "regulatory-sequences",
  "noncoding-rna",
  "structural-dna",
  "repetitive-elements",
  "epigenetic-marks",
  "developmental-programs",
  "dna-lang",
];

const titles: Record<string, string> = {
  "coding-sequences": "Coding Sequences as Executable Functions",
  "regulatory-sequences": "Regulatory Sequences as Control Flow",
  "noncoding-rna": "Non-Coding RNAs as Signals and Middleware",
  "structural-dna": "Structural DNA as Memory Architecture",
  "repetitive-elements": "Repetitive Elements as Self-Modifying Code",
  "epigenetic-marks": "Epigenetic Marks as Runtime State",
  "developmental-programs": "Developmental Programs as Orchestration",
  "dna-lang": "DNA-Lang: A Typed Orchestration Language",
};

export function generateStaticParams() {
  return slugs.map((slug) => ({ slug }));
}

export async function generateMetadata({
  params,
}: {
  params: Promise<{ slug: string }>;
}): Promise<Metadata> {
  const { slug } = await params;
  const title = titles[slug] || slug;
  return {
    title: `${title} | DNA-Lang`,
    description: `Read the full paper: ${title}`,
  };
}

function extractParts(html: string): { css: string; body: string; js: string } {
  const styleMatch = html.match(/<style>([\s\S]*?)<\/style>/);
  const css = styleMatch ? styleMatch[1] : "";

  const bodyMatch = html.match(/<body>([\s\S]*?)<\/body>/);
  const body = bodyMatch ? bodyMatch[1] : html;

  // Extract the TOC builder script
  const scriptMatch = html.match(/<\/main>[\s\S]*?<script>([\s\S]*?)<\/script>/);
  const js = scriptMatch ? scriptMatch[1] : "";

  // Extract any <link> tags from <head> (e.g., KaTeX CSS)
  const linkMatches = html.match(/<link[^>]*katex[^>]*>/g);
  const links = linkMatches ? linkMatches.join("\n") : "";

  return { css, body, js, links } as any;
}

// HTML content is generated at build time from our own LaTeX papers via pandoc,
// with math pre-rendered by KaTeX Node.js. Trusted, static content.
export default async function ReadPage({
  params,
}: {
  params: Promise<{ slug: string }>;
}) {
  const { slug } = await params;
  if (!slugs.includes(slug)) notFound();

  const htmlPath = path.join(process.cwd(), "public", "read", `${slug}.html`);
  let htmlContent: string;
  try {
    htmlContent = fs.readFileSync(htmlPath, "utf-8");
  } catch {
    notFound();
  }

  const { css, body, js } = extractParts(htmlContent);

  // Extract KaTeX CSS link from head
  const katexCSSMatch = htmlContent.match(
    /<link[^>]*href="([^"]*katex[^"]*\.css)"[^>]*>/
  );
  const katexCSS = katexCSSMatch ? katexCSSMatch[1] : "";

  return (
    <>
      {/* KaTeX CSS for pre-rendered math styling */}
      {katexCSS && <link rel="stylesheet" href={katexCSS} />}

      {/* Paper styles from the HTML template */}
      <style dangerouslySetInnerHTML={{ __html: css }} />

      {/* Paper body with pre-rendered math */}
      <div dangerouslySetInnerHTML={{ __html: body }} />

      {/* TOC builder runs client-side */}
      {js && (
        <script dangerouslySetInnerHTML={{ __html: js }} />
      )}
    </>
  );
}
