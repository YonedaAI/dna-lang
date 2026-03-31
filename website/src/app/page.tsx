import Link from "next/link";

const papers = [
  {
    slug: "coding-sequences",
    number: "I",
    title: "Coding Sequences as Executable Functions",
    subtitle: "A Type-Theoretic Framework for the Central Dogma",
    type: "ProteinCode<T>",
    analogy: "Executable Functions",
    description:
      "Formalizes the central dogma (DNA\u2192mRNA\u2192Protein) as a typed compilation pipeline. Codons as opcodes, alternative splicing as dependent types, mutations as type errors.",
    color: "#00b894",
    category: "bio.PL",
  },
  {
    slug: "regulatory-sequences",
    number: "II",
    title: "Regulatory Sequences as Control Flow",
    subtitle: "A Type-Theoretic Framework for Gene Expression",
    type: "Regulator<ExpressionLevel>",
    analogy: "Control Flow",
    description:
      "Promoters as function entry points, enhancers as environment variables, silencers as access control. Gene regulatory networks as typed dataflow graphs.",
    color: "#0984e3",
    category: "bio.PL",
  },
  {
    slug: "noncoding-rna",
    number: "III",
    title: "Non-Coding RNAs as Signals and Middleware",
    subtitle: "A Type-Theoretic Framework for Post-Transcriptional Control",
    type: "RNAControl<Process>",
    analogy: "Signals & Middleware",
    description:
      "miRNA as guard clauses, siRNA as exception handlers, lncRNA as middleware scaffolds, tRNA as natural transformations between codon and amino acid functors.",
    color: "#6c5ce7",
    category: "bio.PL",
  },
  {
    slug: "structural-dna",
    number: "IV",
    title: "Structural DNA as Memory Architecture",
    subtitle: "A Type-Theoretic Framework for Genome Organization",
    type: "Structure<GenomeLayout>",
    analogy: "Memory Layout",
    description:
      "Telomeres as linear resources, centromeres as synchronization primitives, TADs as memory segments, chromatin loops as pointers.",
    color: "#e17055",
    category: "bio.PL",
  },
  {
    slug: "repetitive-elements",
    number: "V",
    title: "Repetitive Elements as Self-Modifying Code",
    subtitle: "A Type-Theoretic Framework for Genome Plasticity",
    type: "Repeat<SelfModifying>",
    analogy: "Self-Modifying Code",
    description:
      "Transposons as endofunctors on the genome category. Cut-and-paste as move semantics, copy-and-paste as template instantiation, domestication as naturalization.",
    color: "#fdcb6e",
    category: "bio.PL",
  },
  {
    slug: "epigenetic-marks",
    number: "VI",
    title: "Epigenetic Marks as Runtime State",
    subtitle: "A Type-Theoretic Framework for Chromatin Accessibility",
    type: "State<Accessibility>",
    analogy: "Runtime State",
    description:
      "DNA methylation as monadic configuration, histone modifications as access permissions, chromatin remodeling as garbage collection, bivalent chromatin as type superposition.",
    color: "#e84393",
    category: "bio.PL",
  },
  {
    slug: "developmental-programs",
    number: "VII",
    title: "Developmental Programs as Orchestration",
    subtitle: "A Type-Theoretic Framework for Morphogenesis",
    type: "Program<OrganismDevelopment>",
    analogy: "Orchestration",
    description:
      "Hox genes as the main module, morphogen gradients as distributed config, cell fate as refinement type narrowing, Waddington landscape as type lattice.",
    color: "#00cec9",
    category: "bio.PL",
  },
  {
    slug: "dna-lang",
    number: "VIII",
    title: "DNA-Lang: A Typed Orchestration Language",
    subtitle: "For Biological Programming",
    type: "Genome",
    analogy: "Complete Language",
    description:
      "Synthesizes all 7 types into a unified language specification with parser, type checker, compiler (4 targets), and runtime. Includes PCSK9 knockout worked example.",
    color: "#55efc4",
    category: "bio.PL",
    isSynthesis: true,
  },
] as const;

export default function Home() {
  return (
    <div className="min-h-screen" style={{ background: "var(--bg)" }}>
      {/* Subtle grid bg */}
      <div
        className="fixed inset-0 opacity-[0.025] pointer-events-none"
        style={{
          backgroundImage: `radial-gradient(circle at 1px 1px, rgba(0,184,148,0.4) 1px, transparent 0)`,
          backgroundSize: "32px 32px",
        }}
      />

      {/* Nav */}
      <nav
        aria-label="Main navigation"
        className="fixed top-0 left-0 right-0 z-50 border-b backdrop-blur-xl"
        style={{
          background: "rgba(10,14,20,0.88)",
          borderColor: "var(--border)",
        }}
      >
        <div className="max-w-6xl mx-auto px-4 sm:px-6 h-14 sm:h-16 flex items-center justify-between">
          <Link href="/" className="flex items-center gap-2.5 no-underline">
            <div
              aria-hidden="true"
              className="w-7 h-7 sm:w-8 sm:h-8 rounded-lg flex items-center justify-center text-xs sm:text-sm font-bold"
              style={{
                background: "var(--accent-dim)",
                color: "var(--accent)",
                fontFamily: "JetBrains Mono, monospace",
              }}
            >
              {"{}"}
            </div>
            <span
              className="font-bold text-base sm:text-lg"
              style={{ color: "var(--text)" }}
            >
              DNA-Lang
            </span>
          </Link>
          <div className="flex items-center gap-3 sm:gap-6 text-sm">
            <a
              href="#papers"
              className="hidden sm:inline transition-colors"
              style={{ color: "var(--text-dim)" }}
            >
              Papers
            </a>
            <a
              href="#architecture"
              className="hidden sm:inline transition-colors"
              style={{ color: "var(--text-dim)" }}
            >
              Architecture
            </a>
            <a
              href="https://github.com/YonedaAI/dna-lang"
              target="_blank"
              rel="noopener noreferrer"
              className="px-3 py-1.5 sm:px-4 sm:py-2 rounded-lg text-xs sm:text-sm font-medium"
              style={{
                background: "var(--accent-dim)",
                color: "var(--accent2)",
                border: "1px solid rgba(0,184,148,0.2)",
              }}
            >
              GitHub
            </a>
          </div>
        </div>
      </nav>

      {/* Hero */}
      <section className="pt-24 sm:pt-32 pb-12 sm:pb-20 px-4 sm:px-6 relative">
        <div className="max-w-4xl mx-auto text-center">
          <div
            className="inline-block px-3 sm:px-4 py-1 sm:py-1.5 rounded-full text-[10px] sm:text-xs font-semibold tracking-wider uppercase mb-4 sm:mb-6"
            style={{
              background: "var(--accent-dim)",
              color: "var(--accent)",
              border: "1px solid rgba(0,184,148,0.2)",
            }}
          >
            8 Research Papers &middot; Haskell &middot; bio.PL
          </div>

          <h1
            className="text-4xl sm:text-5xl md:text-7xl font-extrabold tracking-tight leading-[1.1] mb-4 sm:mb-6"
            style={{
              background:
                "linear-gradient(135deg, #e5e7eb 0%, #55efc4 50%, #00b894 100%)",
              WebkitBackgroundClip: "text",
              WebkitTextFillColor: "transparent",
            }}
          >
            DNA-Lang
          </h1>

          <p
            className="text-lg sm:text-xl md:text-2xl font-medium mb-3 sm:mb-4"
            style={{ color: "var(--text)" }}
          >
            A Typed Programming Language for Biological Systems
          </p>

          <p
            className="text-sm sm:text-base max-w-2xl mx-auto mb-8 sm:mb-10 leading-relaxed"
            style={{ color: "var(--text-dim)" }}
          >
            DNA contains seven functional data types that compose into a
            complete programming model. We formalize each as a typed construct,
            implement them in Haskell, and synthesize a unified orchestration
            language for biological intent, AI prediction, genome editing, and
            regulatory traceability.
          </p>

          <div className="flex items-center justify-center gap-3 sm:gap-4 flex-wrap">
            <a
              href="#papers"
              className="px-5 sm:px-6 py-2.5 sm:py-3 rounded-lg font-semibold text-sm sm:text-base transition-all hover:brightness-110 hover:shadow-lg hover:shadow-[rgba(0,184,148,0.25)]"
              style={{
                background: "var(--accent)",
                color: "#0a0e14",
              }}
            >
              Read the Papers
            </a>
            <a
              href="https://github.com/YonedaAI/dna-lang/tree/main/src"
              target="_blank"
              rel="noopener noreferrer"
              className="px-5 sm:px-6 py-2.5 sm:py-3 rounded-lg font-semibold text-sm sm:text-base transition-all hover:brightness-125 hover:border-[var(--accent)]"
              style={{
                background: "var(--surface2)",
                color: "var(--text)",
                border: "1px solid var(--border)",
              }}
            >
              View Haskell Source
            </a>
          </div>
        </div>
      </section>

      {/* Type System Overview */}
      <section className="py-12 sm:py-16 px-4 sm:px-6" id="architecture">
        <div className="max-w-5xl mx-auto">
          <h2
            className="text-2xl sm:text-3xl font-bold mb-2 text-center"
            style={{ color: "var(--text)" }}
          >
            The Genome Type System
          </h2>
          <p
            className="text-center mb-8 sm:mb-12 text-xs sm:text-sm"
            style={{ color: "var(--text-dim)" }}
          >
            Seven biological data types compose into a complete programming
            model
          </p>

          <div
            className="rounded-xl p-4 sm:p-6 overflow-x-auto"
            style={{
              background: "var(--surface)",
              border: "1px solid var(--border)",
            }}
          >
            <pre
              className="text-xs sm:text-sm leading-relaxed whitespace-pre"
              style={{
                fontFamily: "JetBrains Mono, monospace",
                color: "var(--text-dim)",
              }}
            >
              <span style={{ color: "#6c5ce7" }}>data</span>{" "}
              <span style={{ color: "var(--accent2)" }}>Genome</span>{" "}
              <span style={{ color: "#6c5ce7" }}>=</span>{" "}
              <span style={{ color: "var(--accent2)" }}>Genome</span>
              {"\n"}
              {"  { "}
              <span style={{ color: "#fdcb6e" }}>code</span>
              {"        :: ["}
              <span style={{ color: "#00b894" }}>ProteinCode</span>
              {"]      "}
              <span style={{ color: "#636e72" }}>
                -- Paper I: Executable Functions
              </span>
              {"\n"}
              {"  , "}
              <span style={{ color: "#fdcb6e" }}>regulators</span>
              {"  :: ["}
              <span style={{ color: "#0984e3" }}>Regulator</span>
              {"]       "}
              <span style={{ color: "#636e72" }}>-- Paper II: Control Flow</span>
              {"\n"}
              {"  , "}
              <span style={{ color: "#fdcb6e" }}>ncRNAs</span>
              {"      :: ["}
              <span style={{ color: "#6c5ce7" }}>RNAControl</span>
              {"]      "}
              <span style={{ color: "#636e72" }}>
                -- Paper III: Signals
              </span>
              {"\n"}
              {"  , "}
              <span style={{ color: "#fdcb6e" }}>structure</span>
              {"   :: ["}
              <span style={{ color: "#e17055" }}>Structure</span>
              {"]       "}
              <span style={{ color: "#636e72" }}>
                -- Paper IV: Memory Layout
              </span>
              {"\n"}
              {"  , "}
              <span style={{ color: "#fdcb6e" }}>repeats</span>
              {"     :: ["}
              <span style={{ color: "#fdcb6e" }}>Repeat</span>
              {"]          "}
              <span style={{ color: "#636e72" }}>
                -- Paper V: Self-Modifying
              </span>
              {"\n"}
              {"  , "}
              <span style={{ color: "#fdcb6e" }}>epigenetics</span>
              {" :: ["}
              <span style={{ color: "#e84393" }}>State</span>
              {"]           "}
              <span style={{ color: "#636e72" }}>
                -- Paper VI: Runtime State
              </span>
              {"\n"}
              {"  , "}
              <span style={{ color: "#fdcb6e" }}>programs</span>
              {"    :: ["}
              <span style={{ color: "#00cec9" }}>Program</span>
              {"]         "}
              <span style={{ color: "#636e72" }}>
                -- Paper VII: Orchestration
              </span>
              {"\n"}
              {"  }"}
            </pre>
          </div>
        </div>
      </section>

      {/* Papers Grid */}
      <section className="py-12 sm:py-16 px-4 sm:px-6" id="papers">
        <div className="max-w-6xl mx-auto">
          <h2
            className="text-2xl sm:text-3xl font-bold mb-2 text-center"
            style={{ color: "var(--text)" }}
          >
            Research Papers
          </h2>
          <p
            className="text-center mb-8 sm:mb-12 text-xs sm:text-sm"
            style={{ color: "var(--text-dim)" }}
          >
            Matthew Long &middot; The YonedaAI Collaboration &middot; Chicago,
            IL &middot; March 2026
          </p>

          <div className="grid grid-cols-1 md:grid-cols-2 gap-4 sm:gap-5">
            {papers.map((paper) => (
              <div
                key={paper.slug}
                className="rounded-xl p-5 sm:p-6 transition-all duration-200 group hover:translate-y-[-2px]"
                style={{
                  background: "var(--surface)",
                  border: `1px solid ${"isSynthesis" in paper && paper.isSynthesis ? "rgba(85,239,196,0.3)" : "var(--border)"}`,
                }}
              >
                <div className="flex items-start justify-between mb-3">
                  <div className="flex items-center gap-2">
                    <span
                      className="text-[10px] sm:text-xs font-bold px-2 py-0.5 rounded"
                      style={{
                        background: `${paper.color}20`,
                        color: paper.color,
                      }}
                    >
                      {"isSynthesis" in paper && paper.isSynthesis
                        ? "Synthesis"
                        : `Part ${paper.number}`}
                    </span>
                    <span
                      className="text-[10px] sm:text-xs px-2 py-0.5 rounded"
                      style={{
                        background: "var(--surface2)",
                        color: "var(--text-dim)",
                      }}
                    >
                      {paper.category}
                    </span>
                  </div>
                  <code
                    className="text-[10px] sm:text-xs hidden sm:inline"
                    style={{
                      color: paper.color,
                      fontFamily: "JetBrains Mono, monospace",
                    }}
                  >
                    {paper.type}
                  </code>
                </div>

                <h3 className="font-bold text-base sm:text-lg mb-1">
                  <a
                    href={`/read/${paper.slug}`}
                    className="no-underline hover:underline"
                    style={{ color: "var(--text)" }}
                  >
                    {paper.title}
                  </a>
                </h3>
                <p
                  className="text-[11px] sm:text-xs mb-2 sm:mb-3 font-medium"
                  style={{ color: "var(--text-dim)" }}
                >
                  {paper.subtitle}
                </p>
                <p
                  className="text-xs sm:text-sm leading-relaxed mb-4"
                  style={{ color: "var(--text-dim)" }}
                >
                  {paper.description}
                </p>

                <div className="flex items-center gap-2 sm:gap-3 text-xs flex-wrap">
                  <a
                    href={`/read/${paper.slug}`}
                    
                    className="px-3 py-1.5 rounded-md font-medium transition-all inline-flex items-center gap-1 hover:brightness-125 relative z-10"
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
                    Read
                  </a>
                  <a
                    href={`/papers/${paper.slug}.pdf`}
                    
                    className="px-3 py-1.5 rounded-md font-medium transition-all inline-flex items-center gap-1 hover:brightness-125 relative z-10"
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
                    PDF
                  </a>
                  <a
                    href={`https://github.com/YonedaAI/dna-lang/tree/main/src/${paper.slug}`}
                    target="_blank"
                    rel="noopener noreferrer"
                    
                    className="px-3 py-1.5 rounded-md font-medium transition-all inline-flex items-center gap-1 hover:brightness-125 relative z-10"
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
            ))}
          </div>
        </div>
      </section>

      {/* Compiler Architecture */}
      <section
        className="py-12 sm:py-16 px-4 sm:px-6"
        style={{
          background:
            "linear-gradient(180deg, var(--bg) 0%, var(--surface) 50%, var(--bg) 100%)",
        }}
      >
        <div className="max-w-4xl mx-auto">
          <h2
            className="text-2xl sm:text-3xl font-bold mb-6 sm:mb-8 text-center"
            style={{ color: "var(--text)" }}
          >
            Compiler Architecture
          </h2>

          <div className="grid grid-cols-1 sm:grid-cols-2 gap-4 sm:gap-5">
            {[
              {
                layer: "Layer 1",
                name: "IR / Schema",
                desc: "Typed intermediate representation for targets, sequences, edits, assays, evidence",
                icon: "{ }",
              },
              {
                layer: "Layer 2",
                name: "DSL",
                desc: "Declarative language for programs: design, edit, screen, validate",
                icon: "fn",
              },
              {
                layer: "Layer 3",
                name: "Compilers",
                desc: "Targets: SBOL, Benchling, AlphaFold, LIMS/ELN, assay planning",
                icon: ">>",
              },
              {
                layer: "Layer 4",
                name: "Verification",
                desc: "Off-target, payload, manufacturability, provenance constraints",
                icon: "\u2713",
              },
            ].map((layer) => (
              <div
                key={layer.name}
                className="rounded-xl p-4 sm:p-5"
                style={{
                  background: "var(--bg)",
                  border: "1px solid var(--border)",
                }}
              >
                <div className="flex items-center gap-3 mb-2">
                  <div
                    className="w-9 h-9 sm:w-10 sm:h-10 rounded-lg flex items-center justify-center font-mono font-bold text-xs sm:text-sm shrink-0"
                    style={{
                      background: "var(--accent-dim)",
                      color: "var(--accent)",
                    }}
                  >
                    {layer.icon}
                  </div>
                  <div>
                    <span
                      className="text-[10px] sm:text-xs font-medium"
                      style={{ color: "var(--accent)" }}
                    >
                      {layer.layer}
                    </span>
                    <h3
                      className="font-bold text-sm sm:text-base"
                      style={{ color: "var(--text)" }}
                    >
                      {layer.name}
                    </h3>
                  </div>
                </div>
                <p
                  className="text-xs sm:text-sm"
                  style={{ color: "var(--text-dim)" }}
                >
                  {layer.desc}
                </p>
              </div>
            ))}
          </div>

          <div className="mt-6 sm:mt-8 grid grid-cols-1 sm:grid-cols-2 md:grid-cols-4 gap-3 sm:gap-4">
            {[
              { name: "Design", desc: "Sequences, guides, constructs" },
              { name: "AI Pipeline", desc: "Predictions, scores, uncertainty" },
              { name: "Lab Protocol", desc: "Assay plan, controls, criteria" },
              { name: "Compliance", desc: "Provenance, lineage, decisions" },
            ].map((artifact) => (
              <div
                key={artifact.name}
                className="text-center p-3 sm:p-4 rounded-lg"
                style={{
                  background: "var(--accent-dim)",
                  border: "1px solid rgba(0,184,148,0.15)",
                }}
              >
                <div
                  className="font-bold text-xs sm:text-sm mb-1"
                  style={{ color: "var(--accent2)" }}
                >
                  {artifact.name}
                </div>
                <div
                  className="text-[10px] sm:text-xs"
                  style={{ color: "var(--text-dim)" }}
                >
                  {artifact.desc}
                </div>
              </div>
            ))}
          </div>
        </div>
      </section>

      {/* Sample Program */}
      <section className="py-12 sm:py-16 px-4 sm:px-6">
        <div className="max-w-3xl mx-auto">
          <h2
            className="text-2xl sm:text-3xl font-bold mb-2 text-center"
            style={{ color: "var(--text)" }}
          >
            Sample Program
          </h2>
          <p
            className="text-center mb-6 sm:mb-8 text-xs sm:text-sm"
            style={{ color: "var(--text-dim)" }}
          >
            PCSK9 Knockout Therapy in DNA-Lang
          </p>

          <div
            className="rounded-xl overflow-hidden"
            style={{
              background: "#0d1117",
              border: "1px solid var(--border)",
            }}
          >
            <div
              className="px-4 py-2 flex items-center gap-2 text-xs font-medium"
              style={{
                background: "var(--surface)",
                borderBottom: "1px solid var(--border)",
                color: "var(--text-dim)",
              }}
            >
              <span
                className="w-2.5 h-2.5 rounded-full"
                style={{ background: "var(--accent)" }}
              />
              <span>pcsk9_therapy.dna</span>
            </div>
            <pre
              className="p-4 sm:p-5 text-xs sm:text-sm leading-relaxed overflow-x-auto"
              style={{
                fontFamily: "JetBrains Mono, monospace",
                color: "var(--text-dim)",
              }}
            >
              <code>
                <span style={{ color: "#6c5ce7" }}>program</span>{" "}
                <span style={{ color: "#55efc4" }}>PCSK9_KO</span>
                {" {\n\n"}
                {"  "}
                <span style={{ color: "#6c5ce7" }}>target</span> t ={" "}
                <span style={{ color: "#fdcb6e" }}>gene</span>
                {"("}
                <span style={{ color: "#e17055" }}>&quot;PCSK9&quot;</span>
                {")\n\n"}
                {"  "}
                <span style={{ color: "#6c5ce7" }}>edit</span> e ={" "}
                <span style={{ color: "#fdcb6e" }}>knockout</span>
                {"(target=t)\n\n"}
                {"  "}
                <span style={{ color: "#6c5ce7" }}>guides</span> g ={" "}
                <span style={{ color: "#fdcb6e" }}>design_guides</span>
                {"(\n"}
                {"    target=t,\n"}
                {"    method="}
                <span style={{ color: "#55efc4" }}>CRISPR_Cas9</span>
                {",\n"}
                {"    require PAM="}
                <span style={{ color: "#e17055" }}>&quot;NGG&quot;</span>
                {"\n  )\n\n"}
                {"  "}
                <span style={{ color: "#6c5ce7" }}>guides</span> safe_g ={" "}
                <span style={{ color: "#fdcb6e" }}>filter</span>
                {"(g, off_target_score < "}
                <span style={{ color: "#00b894" }}>0.05</span>
                {")\n\n"}
                {"  "}
                <span style={{ color: "#6c5ce7" }}>require</span>
                {" v.payload_size <= "}
                <span style={{ color: "#fdcb6e" }}>delivery_capacity</span>
                {"("}
                <span style={{ color: "#55efc4" }}>LNP</span>
                {")\n\n"}
                {"  "}
                <span style={{ color: "#6c5ce7" }}>assay</span> a ={" "}
                <span style={{ color: "#fdcb6e" }}>plan_assay</span>
                {"(\n"}
                {"    system = "}
                <span style={{ color: "#55efc4" }}>hepatocyte_organoid</span>
                {",\n"}
                {"    readouts = [editing_rate, LDL_reduction]\n"}
                {"  )\n\n"}
                {"  "}
                <span style={{ color: "#6c5ce7" }}>run</span>
                {" a\n"}
                {"  "}
                <span style={{ color: "#6c5ce7" }}>collect</span>
                {" evidence\n"}
                {"}"}
              </code>
            </pre>
          </div>
        </div>
      </section>

      {/* Footer */}
      <footer
        className="py-8 sm:py-12 px-4 sm:px-6 border-t"
        style={{ borderColor: "var(--border)" }}
      >
        <div className="max-w-4xl mx-auto text-center">
          <p
            className="text-xs sm:text-sm"
            style={{ color: "var(--text-dim)" }}
          >
            Matthew Long &middot; The YonedaAI Research Collective &middot;
            Chicago, IL &middot; 2026
          </p>
          <p className="text-xs mt-2" style={{ color: "var(--text-dim)" }}>
            <a
              href="https://github.com/YonedaAI/dna-lang"
              target="_blank"
              rel="noopener noreferrer"
              style={{ color: "var(--accent)" }}
            >
              github.com/YonedaAI/dna-lang
            </a>
          </p>
        </div>
      </footer>
    </div>
  );
}
