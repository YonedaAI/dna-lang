import { notFound } from "next/navigation";

const paperData: Record<
  string,
  { title: string; subtitle: string; number: string; color: string; type: string }
> = {
  "coding-sequences": {
    title: "Coding Sequences as Executable Functions",
    subtitle: "A Type-Theoretic Framework for the Central Dogma",
    number: "I",
    color: "#00b894",
    type: "ProteinCode<T>",
  },
  "regulatory-sequences": {
    title: "Regulatory Sequences as Control Flow",
    subtitle: "A Type-Theoretic Framework for Gene Expression",
    number: "II",
    color: "#0984e3",
    type: "Regulator<ExpressionLevel>",
  },
  "noncoding-rna": {
    title: "Non-Coding RNAs as Signals and Middleware",
    subtitle: "A Type-Theoretic Framework for Post-Transcriptional Control",
    number: "III",
    color: "#6c5ce7",
    type: "RNAControl<Process>",
  },
  "structural-dna": {
    title: "Structural DNA as Memory Architecture",
    subtitle: "A Type-Theoretic Framework for Genome Organization",
    number: "IV",
    color: "#e17055",
    type: "Structure<GenomeLayout>",
  },
  "repetitive-elements": {
    title: "Repetitive Elements as Self-Modifying Code",
    subtitle: "A Type-Theoretic Framework for Genome Plasticity",
    number: "V",
    color: "#fdcb6e",
    type: "Repeat<SelfModifying>",
  },
  "epigenetic-marks": {
    title: "Epigenetic Marks as Runtime State",
    subtitle: "A Type-Theoretic Framework for Chromatin Accessibility",
    number: "VI",
    color: "#e84393",
    type: "State<Accessibility>",
  },
  "developmental-programs": {
    title: "Developmental Programs as Orchestration",
    subtitle: "A Type-Theoretic Framework for Morphogenesis",
    number: "VII",
    color: "#00cec9",
    type: "Program<OrganismDevelopment>",
  },
  "dna-lang": {
    title: "DNA-Lang: A Typed Orchestration Language",
    subtitle: "For Biological Programming",
    number: "VIII",
    color: "#55efc4",
    type: "Genome",
  },
};

export function generateStaticParams() {
  return Object.keys(paperData).map((slug) => ({ slug }));
}

export default async function PaperPage({
  params,
}: {
  params: Promise<{ slug: string }>;
}) {
  const { slug } = await params;
  const paper = paperData[slug];
  if (!paper) notFound();

  return (
    <div className="min-h-screen" style={{ background: "var(--bg)" }}>
      <nav
        className="border-b backdrop-blur-xl"
        style={{
          background: "rgba(10,14,20,0.85)",
          borderColor: "var(--border)",
        }}
      >
        <div className="max-w-4xl mx-auto px-6 h-14 flex items-center justify-between">
          <a
            href="/"
            className="text-sm flex items-center gap-2"
            style={{ color: "var(--text-dim)" }}
          >
            &larr; All Papers
          </a>
          <div className="flex items-center gap-3 text-xs">
            <a
              href={`/papers/${slug}.pdf`}
              className="px-3 py-1.5 rounded-md font-medium"
              style={{
                background: `${paper.color}15`,
                color: paper.color,
                border: `1px solid ${paper.color}30`,
              }}
            >
              Download PDF
            </a>
            <a
              href={`https://github.com/YonedaAI/dna-lang/tree/main/src/${slug}`}
              target="_blank"
              rel="noopener noreferrer"
              className="px-3 py-1.5 rounded-md font-medium"
              style={{
                background: "var(--surface2)",
                color: "var(--text-dim)",
                border: "1px solid var(--border)",
              }}
            >
              Haskell Source
            </a>
          </div>
        </div>
      </nav>

      <main className="max-w-3xl mx-auto px-6 py-12">
        <div className="mb-10">
          <span
            className="text-xs font-bold px-2 py-0.5 rounded inline-block mb-3"
            style={{ background: `${paper.color}20`, color: paper.color }}
          >
            Part {paper.number}
          </span>
          <h1
            className="text-4xl font-extrabold tracking-tight mb-2"
            style={{
              background: `linear-gradient(135deg, #e5e7eb, ${paper.color})`,
              WebkitBackgroundClip: "text",
              WebkitTextFillColor: "transparent",
            }}
          >
            {paper.title}
          </h1>
          <p className="text-lg mb-4" style={{ color: "var(--text-dim)" }}>
            {paper.subtitle}
          </p>
          <div
            className="flex items-center gap-2 text-sm"
            style={{ color: "var(--text-dim)" }}
          >
            <span>Matthew Long</span>
            <span>&middot;</span>
            <span>YonedaAI Research Collective</span>
            <span>&middot;</span>
            <code
              className="text-xs px-2 py-0.5 rounded"
              style={{
                background: `${paper.color}10`,
                color: paper.color,
                fontFamily: "JetBrains Mono, monospace",
              }}
            >
              {paper.type}
            </code>
          </div>
        </div>

        <div
          className="rounded-xl p-8 text-center"
          style={{
            background: "var(--surface)",
            border: "1px solid var(--border)",
          }}
        >
          <p className="text-lg mb-4" style={{ color: "var(--text-dim)" }}>
            Full paper content available in PDF and HTML formats.
          </p>
          <div className="flex items-center justify-center gap-4">
            <a
              href={`/papers/${slug}.pdf`}
              className="px-5 py-2.5 rounded-lg font-semibold"
              style={{ background: paper.color, color: "#0a0e14" }}
            >
              Download PDF
            </a>
            <a
              href={`https://yonedaai.github.io/dna-lang/papers/${slug}.html`}
              className="px-5 py-2.5 rounded-lg font-semibold"
              style={{
                background: "var(--surface2)",
                color: "var(--text)",
                border: "1px solid var(--border)",
              }}
            >
              Read HTML (GitHub Pages)
            </a>
          </div>
        </div>
      </main>
    </div>
  );
}
