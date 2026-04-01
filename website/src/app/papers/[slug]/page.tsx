import { notFound } from "next/navigation";
import type { Metadata } from "next";
import Link from "next/link";

const paperData: Record<
  string,
  {
    title: string;
    subtitle: string;
    number: string;
    color: string;
    type: string;
    abstract: string;
    sections: string[];
  }
> = {
  "coding-sequences": {
    title: "Coding Sequences as Executable Functions",
    subtitle: "A Type-Theoretic Framework for the Central Dogma",
    number: "I",
    color: "#00b894",
    type: "ProteinCode<T>",
    abstract:
      "We formalize protein-coding sequences as a typed compilation pipeline where the central dogma DNA\u2192mRNA\u2192Protein maps directly to Source\u2192IR\u2192Binary compilation. The codon table becomes an opcode mapping, reading frames correspond to parser state, alternative splicing produces dependent type families, and mutations are classified by their type-theoretic impact: silent (identity refactoring), missense (type-compatible substitution), nonsense (type error), and frameshift (parse error).",
    sections: [
      "The Central Dogma as Compilation Pipeline",
      "The Codon Type System",
      "ProteinCode<T> \u2014 The Core Type",
      "Mutations as Type-Theoretic Events",
      "Alternative Splicing as Dependent Types",
      "Post-Translational Modification as Runtime Decoration",
      "Formal Properties and Theorems",
    ],
  },
  "regulatory-sequences": {
    title: "Regulatory Sequences as Control Flow",
    subtitle: "A Type-Theoretic Framework for Gene Expression",
    number: "II",
    color: "#0984e3",
    type: "Regulator<ExpressionLevel>",
    abstract:
      "Promoters, enhancers, and silencers are formalized as a control flow algebra for gene expression. Promoters serve as function entry points with expression level parameters, enhancers act as remote configuration (environment variables), silencers implement access control, and insulators enforce namespace isolation. Gene regulatory networks form categories with regulatory effects as morphisms, and feedback loops implement fixed-point computations.",
    sections: [
      "Promoters as Function Entry Points",
      "Enhancers as Remote Configuration",
      "Silencers as Access Control",
      "The Regulator<ExpressionLevel> Type",
      "Gene Regulatory Networks as Categories",
      "Feedback Loops as Fixed Points",
      "Boolean Gene Circuits",
    ],
  },
  "noncoding-rna": {
    title: "Non-Coding RNAs as Signals and Middleware",
    subtitle: "A Type-Theoretic Framework for Post-Transcriptional Control",
    number: "III",
    color: "#6c5ce7",
    type: "RNAControl<Process>",
    abstract:
      "Non-coding RNAs are formalized as the signaling and middleware layer of the genome. miRNA functions as runtime guard clauses, siRNA implements typed exception handling through the RISC complex, lncRNA serves as middleware scaffolds assembling regulatory complexes, tRNA acts as a natural transformation between codon and amino acid functors, and rRNA constitutes the virtual machine itself.",
    sections: [
      "miRNA as Guard Clauses",
      "siRNA and RNA Interference as Exception Handling",
      "lncRNA as Middleware / Scaffolds",
      "tRNA as Natural Transformation",
      "rRNA as Virtual Machine",
      "The RNAControl<Process> Type",
      "piRNA as Security Monitor",
    ],
  },
  "structural-dna": {
    title: "Structural DNA as Memory Architecture",
    subtitle: "A Type-Theoretic Framework for Genome Organization",
    number: "IV",
    color: "#e17055",
    type: "Structure<GenomeLayout>",
    abstract:
      "Structural DNA elements are formalized as the memory architecture of the genome. Telomeres implement linear type resources consumed on each cell division. Centromeres serve as synchronization primitives for chromosome segregation. TADs partition the genome into memory segments with CTCF/cohesin boundaries. Chromatin loops function as pointers bringing distant genomic addresses into proximity.",
    sections: [
      "Telomeres as Linear Resources",
      "Centromeres as Synchronization Primitives",
      "TADs as Memory Segments",
      "The Structure<GenomeLayout> Type",
      "Chromatin Loops as Pointers",
      "Nuclear Organization as Memory Hierarchy",
      "Formal Theorems and Proofs",
    ],
  },
  "repetitive-elements": {
    title: "Repetitive Elements as Self-Modifying Code",
    subtitle: "A Type-Theoretic Framework for Genome Plasticity",
    number: "V",
    color: "#fdcb6e",
    type: "Repeat<SelfModifying>",
    abstract:
      "Transposable elements, comprising over 45% of the human genome, are formalized as self-modifying code. DNA transposons implement cut-and-paste (move semantics), retrotransposons implement copy-and-paste (template instantiation). Transposition defines an endofunctor on the genome category. Domestication of transposons for host functions is modeled as naturalization \u2014 a natural transformation from the foreign code functor to the host function functor.",
    sections: [
      "DNA Transposons as Cut-and-Paste",
      "Retrotransposons as Copy-and-Paste",
      "The Repeat<SelfModifying> Type",
      "Transposition as Endofunctor",
      "Satellite DNA as Memory Alignment",
      "Endogenous Retroviruses as Legacy Imports",
      "Transposon Domestication as Naturalization",
    ],
  },
  "epigenetic-marks": {
    title: "Epigenetic Marks as Runtime State",
    subtitle: "A Type-Theoretic Framework for Chromatin Accessibility",
    number: "VI",
    color: "#e84393",
    type: "State<Accessibility>",
    abstract:
      "Epigenetic modifications are formalized as runtime state and permissions. DNA methylation implements persistent configuration flags with a monadic structure. Histone modifications serve as access permissions (H3K4me3 = chmod +x, H3K27me3 = chmod -r). Chromatin remodeling acts as garbage collection. Bivalent chromatin represents a product type that resolves to a sum type upon differentiation.",
    sections: [
      "DNA Methylation as Configuration Flags",
      "Histone Modifications as Access Permissions",
      "The State<Accessibility> Type",
      "Epigenetic State Machine",
      "Methylation as Monad",
      "Chromatin Remodeling as Garbage Collection",
      "Bivalent Chromatin as Type Superposition",
    ],
  },
  "developmental-programs": {
    title: "Developmental Programs as Orchestration",
    subtitle: "A Type-Theoretic Framework for Morphogenesis",
    number: "VII",
    color: "#00cec9",
    type: "Program<OrganismDevelopment>",
    abstract:
      "Developmental programs are formalized as the orchestration layer of the genome. Hox genes serve as the main module specifying body plan. Morphogen gradients implement distributed configuration with concentration-dependent types. Cell differentiation follows the Waddington landscape as progressive refinement type narrowing. Stem cells implement the factory pattern with polymorphic constructors.",
    sections: [
      "Hox Genes as the Main Module",
      "Morphogen Gradients as Distributed Configuration",
      "Cell Fate as Type Refinement",
      "Regulatory Cascades as Boot Sequences",
      "Apoptosis as Graceful Shutdown",
      "Stem Cells as Factory Patterns",
      "Hox Collinearity as Monotone Functor",
    ],
  },
  "dna-lang": {
    title: "DNA-Lang: A Typed Orchestration Language",
    subtitle: "For Biological Programming",
    number: "VIII",
    color: "#55efc4",
    type: "Genome",
    abstract:
      "We synthesize seven biological data types into DNA-Lang, a unified typed orchestration language for biological programming. The language features a complete type system with refinement types, a four-layer compiler architecture (IR, DSL, target compilers, verification), and produces four artifacts per program (design, AI pipeline, lab protocol, compliance). We present a full worked example: a PCSK9 knockout cholesterol therapy program.",
    sections: [
      "The Seven Biological Types",
      "The Unified Type System",
      "DNA-Lang Syntax and Grammar",
      "The Type Checker",
      "Four-Layer Compiler Architecture",
      "AI as Typed Service",
      "CRISPR as Backend",
      "Worked Example: PCSK9 Knockout",
    ],
  },
};

export function generateStaticParams() {
  return Object.keys(paperData).map((slug) => ({ slug }));
}

export async function generateMetadata({
  params,
}: {
  params: Promise<{ slug: string }>;
}): Promise<Metadata> {
  const { slug } = await params;
  const paper = paperData[slug];
  if (!paper) return {};

  const title = `${paper.title} | DNA-Lang`;
  const description = paper.abstract;

  return {
    title,
    description,
    openGraph: {
      title: paper.title,
      description,
      type: "article",
      url: `/papers/${slug}`,
      siteName: "DNA-Lang",
      authors: ["Matthew Long"],
      images: [
        {
          url: "/og-image.png",
          width: 1200,
          height: 630,
          alt: `${paper.title} — DNA-Lang`,
        },
      ],
    },
    twitter: {
      card: "summary_large_image",
      title: paper.title,
      description,
      images: ["/og-image.png"],
    },
  };
}

export default async function PaperPage({
  params,
}: {
  params: Promise<{ slug: string }>;
}) {
  const { slug } = await params;
  const paper = paperData[slug];
  if (!paper) notFound();

  const isSynthesis = slug === "dna-lang";

  return (
    <div className="min-h-screen" style={{ background: "var(--bg)" }}>
      {/* Nav */}
      <nav
        aria-label="Paper navigation"
        className="sticky top-0 z-50 border-b backdrop-blur-xl"
        style={{
          background: "rgba(10,14,20,0.88)",
          borderColor: "var(--border)",
        }}
      >
        <div className="max-w-4xl mx-auto px-4 sm:px-6 h-14 flex items-center justify-between">
          <Link
            href="/"
            className="text-sm flex items-center gap-2 no-underline"
            style={{ color: "var(--text-dim)" }}
          >
            <svg
              width="16"
              height="16"
              viewBox="0 0 24 24"
              fill="none"
              stroke="currentColor"
              strokeWidth="2"
            >
              <polyline points="15 18 9 12 15 6" />
            </svg>
            All Papers
          </Link>
          <div className="flex items-center gap-2 sm:gap-3 text-xs">
            <a
              href={`/read/${slug}`}
              className="px-3 py-1.5 rounded-md font-medium inline-flex items-center gap-1.5"
              style={{
                background: `${paper.color}15`,
                color: paper.color,
                border: `1px solid ${paper.color}30`,
              }}
            >
              <svg
                width="12"
                height="12"
                viewBox="0 0 24 24"
                fill="none"
                stroke="currentColor"
                strokeWidth="2"
              >
                <path d="M4 19.5A2.5 2.5 0 0 1 6.5 17H20" />
                <path d="M6.5 2H20v20H6.5A2.5 2.5 0 0 1 4 19.5v-15A2.5 2.5 0 0 1 6.5 2z" />
              </svg>
              Read Full Paper
            </a>
            <a
              href={`/papers/${slug}.pdf`}
              className="px-3 py-1.5 rounded-md font-medium inline-flex items-center gap-1.5"
              style={{
                background: "var(--surface2)",
                color: "var(--text-dim)",
                border: "1px solid var(--border)",
              }}
            >
              <svg
                width="12"
                height="12"
                viewBox="0 0 24 24"
                fill="none"
                stroke="currentColor"
                strokeWidth="2"
              >
                <path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4" />
                <polyline points="7 10 12 15 17 10" />
                <line x1="12" y1="15" x2="12" y2="3" />
              </svg>
              <span className="hidden sm:inline">Download</span> PDF
            </a>
            <a
              href={`https://github.com/YonedaAI/dna-lang/tree/main/src/${slug}`}
              target="_blank"
              rel="noopener noreferrer"
              className="px-3 py-1.5 rounded-md font-medium hidden sm:inline-flex items-center gap-1.5"
              style={{
                background: "var(--surface2)",
                color: "var(--text-dim)",
                border: "1px solid var(--border)",
              }}
            >
              <svg
                width="12"
                height="12"
                viewBox="0 0 24 24"
                fill="none"
                stroke="currentColor"
                strokeWidth="2"
              >
                <polyline points="16 18 22 12 16 6" />
                <polyline points="8 6 2 12 8 18" />
              </svg>
              Haskell
            </a>
          </div>
        </div>
      </nav>

      <main className="max-w-3xl mx-auto px-4 sm:px-6 py-8 sm:py-12">
        {/* Header */}
        <div className="mb-8 sm:mb-10">
          <span
            className="text-[10px] sm:text-xs font-bold px-2 py-0.5 rounded inline-block mb-3"
            style={{ background: `${paper.color}20`, color: paper.color }}
          >
            {isSynthesis ? "Synthesis" : `Part ${paper.number}`}
          </span>
          <h1
            className="text-3xl sm:text-4xl font-extrabold tracking-tight mb-2"
            style={{
              background: `linear-gradient(135deg, #e5e7eb, ${paper.color})`,
              WebkitBackgroundClip: "text",
              WebkitTextFillColor: "transparent",
            }}
          >
            {paper.title}
          </h1>
          <p
            className="text-base sm:text-lg mb-4"
            style={{ color: "var(--text-dim)" }}
          >
            {paper.subtitle}
          </p>
          <div
            className="flex items-center gap-2 text-xs sm:text-sm flex-wrap"
            style={{ color: "var(--text-dim)" }}
          >
            <span>Matthew Long</span>
            <span>&middot;</span>
            <span>YonedaAI Research Collective</span>
            <span>&middot;</span>
            <span>March 2026</span>
            <span>&middot;</span>
            <code
              className="text-[10px] sm:text-xs px-2 py-0.5 rounded"
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

        {/* Abstract */}
        <section
          className="rounded-xl p-5 sm:p-6 mb-6"
          style={{
            background: "var(--surface)",
            border: "1px solid var(--border)",
            borderLeft: `3px solid ${paper.color}`,
          }}
        >
          <h2
            className="text-xs font-bold uppercase tracking-wider mb-3"
            style={{ color: paper.color }}
          >
            Abstract
          </h2>
          <p
            className="text-sm sm:text-base leading-relaxed"
            style={{ color: "var(--text-dim)" }}
          >
            {paper.abstract}
          </p>
        </section>

        {/* Table of Contents */}
        <section
          className="rounded-xl p-5 sm:p-6 mb-6"
          style={{
            background: "var(--surface)",
            border: "1px solid var(--border)",
          }}
        >
          <h2
            className="text-xs font-bold uppercase tracking-wider mb-3"
            style={{ color: "var(--text-dim)" }}
          >
            Sections
          </h2>
          <ol className="space-y-1.5">
            {paper.sections.map((section, i) => (
              <li
                key={i}
                className="flex items-baseline gap-3 text-sm"
                style={{ color: "var(--text-dim)" }}
              >
                <span
                  className="text-xs font-mono shrink-0"
                  style={{ color: paper.color }}
                >
                  {i + 1}.
                </span>
                <span>{section}</span>
              </li>
            ))}
          </ol>
        </section>

        {/* Actions */}
        <div className="flex flex-col sm:flex-row items-stretch sm:items-center gap-3">
          <a
            href={`/read/${slug}`}
            className="flex-1 px-5 py-3 rounded-lg font-semibold text-center text-sm sm:text-base"
            style={{ background: paper.color, color: "#0a0e14" }}
          >
            Read Full Paper (HTML)
          </a>
          <a
            href={`/papers/${slug}.pdf`}
            className="flex-1 px-5 py-3 rounded-lg font-semibold text-center text-sm sm:text-base"
            style={{
              background: "var(--surface2)",
              color: "var(--text)",
              border: "1px solid var(--border)",
            }}
          >
            Download PDF
          </a>
        </div>
      </main>
    </div>
  );
}
