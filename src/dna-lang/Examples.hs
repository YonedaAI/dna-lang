{-
  Examples.hs — Sample DNA-Lang Programs

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Example programs expressed as AST values, including the canonical
  PCSK9 knockout therapy program and other demonstration programs.
-}

module Examples (
    pcsk9Program,
    pcsk9Source,
    baseEditProgram,
    geneActivationProgram
) where

import Types

-- ══════════════════════════════════════════════════════════════════
-- PCSK9 Knockout Therapy — The Canonical Example
-- ══════════════════════════════════════════════════════════════════

-- | Source text of the PCSK9_KO program for parser demonstration.
pcsk9Source :: String
pcsk9Source = Prelude.unlines
    [ "program PCSK9_KO {"
    , "  target pcsk9 = \"PCSK9\""
    , "  sequence ref_seq : DNA = lookup_sequence(pcsk9)"
    , "  guide top_guide = design_guide(pcsk9, ref_seq)"
    , "  require no_off_targets"
    , "  require specificity >= 0.90"
    , "  require efficiency >= 0.70"
    , "  edit ko_edit : Knockout = knockout(pcsk9, top_guide)"
    , "  cell_line hepg2 = select_cell_line(\"HepG2\")"
    , "  assay ngs_assay = design_assay(ko_edit, hepg2)"
    , "  run validate_safety(ko_edit)"
    , "  run run_assay(ngs_assay, hepg2)"
    , "  collect results = generate_report(ngs_assay, ko_edit)"
    , "}"
    ]

-- | The PCSK9 knockout program as an AST value.
pcsk9Program :: Program
pcsk9Program = Program "PCSK9_KO"
    -- Declarations
    [ DTarget "pcsk9" (EStringLit "PCSK9")

    , DSequence "ref_seq" DNA
        (EApp "lookup_sequence" [EVar "pcsk9"])

    , DGuideRNA "top_guide"
        (EApp "design_guide" [EVar "pcsk9", EVar "ref_seq"])

    , DRequire CNoOffTarget

    , DRequire (CSpecificity 0.90)

    , DRequire (CEfficiency 0.70)

    , DEdit "ko_edit" Knockout
        (EApp "knockout" [EVar "pcsk9", EVar "top_guide"])

    , DCellLine "hepg2"
        (EApp "select_cell_line" [EStringLit "HepG2"])

    , DAssay "ngs_assay"
        (EApp "design_assay" [EVar "ko_edit", EVar "hepg2"])
    ]
    -- Actions
    [ ARun "validate_safety" [EVar "ko_edit"]
    , ARun "run_assay" [EVar "ngs_assay", EVar "hepg2"]
    , ACollect "results" (EApp "generate_report" [EVar "ngs_assay", EVar "ko_edit"])
    ]

-- ══════════════════════════════════════════════════════════════════
-- Base Edit Program — Sickle Cell Disease
-- ══════════════════════════════════════════════════════════════════

-- | A base editing program targeting the sickle cell mutation in HBB.
baseEditProgram :: Program
baseEditProgram = Program "SCD_BaseEdit"
    [ DTarget "hbb" (EStringLit "HBB")

    , DSequence "hbb_seq" DNA
        (EApp "lookup_sequence" [EVar "hbb"])

    , DGuideRNA "scd_guide"
        (EApp "design_guide" [EVar "hbb", EVar "hbb_seq"])

    , DRequire (CAnd CNoOffTarget (CSpecificity 0.95))

    , DEdit "scd_edit" BaseEdit
        (EApp "base_edit" [EVar "hbb", EVar "scd_guide", EVar "hbb_seq"])

    , DCellLine "cd34" (EStringLit "CD34+ HSPCs")

    , DAssay "scd_assay"
        (EApp "design_assay" [EVar "scd_edit", EVar "cd34"])
    ]
    [ ARun "validate_safety" [EVar "scd_edit"]
    , ARun "run_assay" [EVar "scd_assay", EVar "cd34"]
    , ACollect "results" (EApp "generate_report" [EVar "scd_assay", EVar "scd_edit"])
    ]

-- ══════════════════════════════════════════════════════════════════
-- CRISPRa Gene Activation Program
-- ══════════════════════════════════════════════════════════════════

-- | A CRISPRa program to upregulate a therapeutic gene.
geneActivationProgram :: Program
geneActivationProgram = Program "FLDL_Activate"
    [ DTarget "fldl" (EStringLit "FLDLR")

    , DSequence "promoter_seq" DNA
        (EApp "lookup_sequence" [EVar "fldl"])

    , DGuideRNA "activation_guide"
        (EApp "design_guide" [EVar "fldl", EVar "promoter_seq"])

    , DRequire (CSpecificity 0.85)

    , DSequence "protein_seq" Protein
        (EStringLit "MFLDLR_PROTEIN_SEQUENCE")

    , DModelResult "structure_pred"
        (EApp "predict_structure" [EVar "protein_seq"])

    , DAssay "qpcr_assay"
        (ERecord [("type", EStringLit "qPCR"), ("target", EStringLit "FLDLR mRNA")])
    ]
    [ ARun "predict_off_target" [EVar "activation_guide"]
    , ALog (EStringLit "Gene activation program initiated")
    , AReturn (EVar "structure_pred")
    ]
