# DNA-Lang

**A Typed Programming Language for Biological Systems**

DNA is not one type — it contains seven functional data types that compose into a complete programming model. DNA-Lang formalizes each as a typed construct, implements them in Haskell, and synthesizes a unified orchestration language for biological intent, AI prediction, genome editing, and regulatory traceability.

## Papers

| Part | Title | Type | Pages |
|------|-------|------|-------|
| I | Coding Sequences as Executable Functions | `ProteinCode<T>` | 41 |
| II | Regulatory Sequences as Control Flow | `Regulator<ExpressionLevel>` | 23 |
| III | Non-Coding RNAs as Signals and Middleware | `RNAControl<Process>` | 23 |
| IV | Structural DNA as Memory Architecture | `Structure<GenomeLayout>` | 28 |
| V | Repetitive Elements as Self-Modifying Code | `Repeat<SelfModifying>` | 19 |
| VI | Epigenetic Marks as Runtime State | `State<Accessibility>` | 26 |
| VII | Developmental Programs as Orchestration | `Program<OrganismDevelopment>` | 21 |
| VIII | **DNA-Lang: A Typed Orchestration Language** | `Genome` | 26 |

**Total: 207 pages across 8 papers**

## The Genome Type System

```haskell
data Genome = Genome
  { code        :: [ProteinCode]      -- Paper I:   Executable Functions
  , regulators  :: [Regulator]        -- Paper II:  Control Flow
  , ncRNAs      :: [RNAControl]       -- Paper III: Signals & Middleware
  , structure   :: [Structure]        -- Paper IV:  Memory Layout
  , repeats     :: [Repeat]           -- Paper V:   Self-Modifying Code
  , epigenetics :: [State]            -- Paper VI:  Runtime State
  , programs    :: [Program]          -- Paper VII: Orchestration
  }
```

## What DNA-Lang Does

DNA-Lang is a **typed orchestration language** — not "DNA as code" directly — that spans:

1. **Biological intent** — declare targets, edits, assays
2. **AI model invocation** — typed calls to AlphaFold, structure predictors, guide rankers
3. **Sequence/edit design** — CRISPR guide design, donor templates, vector construction
4. **Experimental execution** — assay planning, controls, readouts
5. **Safety/constraint checking** — off-target scoring, payload limits, toxicity thresholds
6. **Provenance and regulatory traceability** — full decision log, model versions, sequence lineage

## Compiler Architecture

```
Layer 1: IR / Schema          — typed intermediate representation
Layer 2: DSL                  — declarative language (design, edit, screen, validate)
Layer 3: Target Compilers     — SBOL, Benchling, AlphaFold, LIMS/ELN
Layer 4: Verification         — off-target, payload, manufacturability, provenance
```

Four compiler artifacts:
- **Design** — sequences, guides, constructs, edit plans
- **AI Pipeline** — structure predictions, rankings, uncertainty estimates
- **Lab Protocol** — assay plan, controls, acceptance criteria
- **Compliance** — provenance, model versions, decision log

## Building

### Papers

```bash
cd papers/latex
pdflatex -interaction=nonstopmode <paper>.tex
pdflatex -interaction=nonstopmode <paper>.tex  # second pass for refs
```

### Haskell

```bash
cd src/<subject>
ghc -o main Main.hs
./main
```

## Repository Structure

```
papers/latex/    — 8 LaTeX source files
papers/pdf/      — 8 compiled PDFs
src/             — Haskell implementations (one directory per paper)
docs/            — GitHub Pages site
website/         — Next.js/Vercel site
scripts/         — Build tools (latex2html.py, paper-template.html)
```

## Author

Matthew Long — The YonedaAI Research Collective — Chicago, IL — 2026

## Website

- Vercel: https://dna-lang.vercel.app
- GitHub Pages: https://yonedaai.github.io/dna-lang/
- Papers: available as PDF and HTML with KaTeX math rendering
