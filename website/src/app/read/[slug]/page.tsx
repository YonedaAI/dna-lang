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

// HTML content is generated at build time from our own LaTeX papers via pandoc.
// This is trusted, static content — not user input.
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

  // Render the full standalone HTML page as an iframe-like embed
  // since the HTML papers are complete documents with their own <html>, <head>, <style>
  return (
    <iframe
      srcDoc={htmlContent}
      title={titles[slug] || "Paper"}
      className="w-full border-0"
      style={{ minHeight: "100vh", height: "100vh" }}
    />
  );
}
