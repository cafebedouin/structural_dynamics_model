# Deferential Realism

**A formal framework for systems where what you see depends on where you stand**

Deferential Realism (DR) is a framework for analyzing observer-dependent classification of social structures. Its core claim: power modulates the perception of extraction, and this dependence has formal mathematical structure. The framework models classification as a presheaf on a site of observer contexts, deliberately refusing the sheaf gluing axiom so that perspectival disagreement becomes a measurable structural feature rather than a defect to be resolved. A 79-module Prolog engine implements the axioms computationally and checks whether the predicted structural consequences actually emerge against a living corpus of 3,256 constraint stories. The engine makes the philosophy falsifiable — but the engine is not the point. The philosophy is the point.

---

## The Core Idea

### Perspectival Structure, Not Perspectival Relativism

Classification of social structures — laws, norms, institutions, regulatory mechanisms — depends on who is classifying. A labor regulation that appears as an immutable feature of the economic landscape to a worker trapped within it may appear as a reformable coordination mechanism to a legislator, and as an extractive rent-seeking device to an analyst examining its distributional effects.

The standard response to this is to resolve it — identify the "correct" classification by privileging one observer or aggregating across observers. DR takes the opposite approach: disagreement is the signal. The framework models perspectival dependence as a presheaf on a site of observer positions and deliberately refuses sheafification. The tools of topos theory — cohomology, descent, spectral analysis — then produce quantitative invariants that characterize the structure of disagreement in any domain where classification depends on perspective.

This is not relativism (all views equally valid) or absolutism (one correct view). It is realism about the dependence of classification on position. Constraints have objective structural properties — extractiveness, suppression, coordination function — that exist independently of any observer. But the *classification* of those properties depends irreducibly on where the observer stands.

### The Empirical Anchor

The framework encodes a single core hypothesis as its empirical anchor (Axiom 2): **power reduces experienced extraction.** Each constraint has a base extractiveness ε that is observer-independent. The *experienced* extractiveness varies by observer:

> χ = ε × f(d(P)) × σ(S(P))

where f is a sigmoid over the observer's directionality (beneficiary, victim, neither), and σ encodes verification difficulty. At the canonical institutional position, f(d) goes negative — the institutional observer experiences extraction as coordination. Everything downstream inherits this axiom's empirical status. If it maps correctly to the world, the theorems describe structural necessities. If not, the mathematics remains valid within the model but the model no longer describes reality.

### The Constraint Taxonomy

Six structural types, organized around extraction and coordination:

| Type | What It Means | Example |
|------|---------------|---------|
| **Mountain** | Appears as natural law across all positions | Speed of light, mortality |
| **Rope** | Coordination constraint, low extraction | Traffic conventions |
| **Tangled Rope** | Coordination and extraction simultaneously present | HOA covenants, non-compete clauses |
| **Snare** | Pure extraction | Carried interest taxation |
| **Scaffold** | Temporary structure in formation | Emerging regulations |
| **Piton** | Local anchoring, position-specific | Informal workplace norms |

These are not metaphors — they are classification outputs determined by the observer's experienced extractiveness χ. The same constraint is objectively different types depending on who is observing.

### The Four Theorems

From the axioms, four non-obvious consequences follow deductively:

1. **Extraction requires perspectival cover** (Theorem 1). Under power-modulated perception, extraction cannot be universally recognized as extraction. For any high-extraction constraint, there exists at least one observer position where it is classified as coordination. The demand to "prove extraction exists" from the perspective of its beneficiaries is a structural impossibility.

2. **Disagreement clusters in discrete blocs** (Theorem 2). On the 4-element linear site, H¹ values 1 and 2 are forbidden. Perspectival disagreement does not distribute smoothly — it clusters in blocs of size ≥ 3, determined by the site geometry. The contextuality fraction takes values only at {0, 0.5, 0.667, 0.833, 1.0}.

3. **Institutional spectral dominance** (Theorem 3). The institutional observer carries 97% of the spectral weight in classification disputes. The Sheaf Laplacian on the path-graph site is dominated by the phase transition at the institutional position.

4. **The Oracle Gap** (Theorem 4). Single-position analysis with complete information detects less than 3% of the cross-position structure that multi-position analysis reveals. Even with full information about a constraint's metrics, an analyst at any single position cannot recover the perspectival disagreement structure.

### Why This Matters

The framework provides formal machinery to measure perspectival fracture — how much of a domain is fractured, which positions disagree, and whether the disagreement is structurally determined or cognitively modulated. The implications are direct: if institutional perception is spectrally decoupled from other positions, institutional actors designing reforms are working from a classification structurally orthogonal to the experience of those affected — not because they ignore the data, but because their position transforms it.

---

## The Computational Engine

The Prolog engine is a test bed for the philosophy. It exists so the axioms can be implemented computationally and their consequences checked against data.

**Architecture:**
- 79 Prolog modules implementing the axioms as a deterministic rule cascade
- 3,256-constraint living corpus (grows with each analytical run)
- 12+ diagnostic subsystems: classification, MaxEnt shadow, sheaf cohomology, game theory (Nash distance, cover story detection, two-regime extraction), parametric persistence, Wasserstein transport, variance decomposition, cognitive displacement, cultural cognition profiles, contamination network, boundary normality, boolean independence
- 324 enhanced reports with per-constraint structural analysis stacks — theorem instantiation, orbit analysis, Nash profiles, CCDP/margin analysis

**Key validation results:**
- Structural invariants (H¹ gap, spectral eigenvalues, contextuality fraction gap, institutional dissent direction) are identical across two independently generated corpora with inverted input distributions
- All invariants survive FCR ablation, parameter sweeps (154 numeric params at ±25%), directionality sweeps (17 constants), cognitive displacement sweeps (δ ∈ [−0.15, +0.15]), and corpus growth from 887 to 3,256 constraints
- These confirm the invariants are properties of the axioms, not of any dataset

The engine exists so the philosophy can be tested computationally, not so users can process inputs into outputs.

**Running the engine:**
```bash
cd prolog && swipl -g "[stack], [validation_suite], run_dynamic_suite, halt" -t "halt(1)"
```

---

## The Analytical Pipeline

The engine connects to analytical output through a constraint story pipeline:

**Constraint story generation:** Any domain → formal Prolog model with metrics, classifications, structural signatures. The generation prompt (`prompts/constraint_story_generation_prompt.md`) specifies the full extraction. LLMs (Gemini, Haiku, others) execute it; the Prolog engine validates the output.

**DR pipeline:** constraint story → Prolog validation → enhanced report → structural analysis stack. The enhanced report (`python/enhanced_report.py`) computes per-constraint theorem instantiation, orbit structure, Nash profiles, and game-theoretic analysis.

**UKE protocol suite:** Protocols for writing, editing, grounding, auditing, and reviewing analytical output. The suite includes UKE Write (essay synthesis from structural analysis), UKE Edit, UKE Ground, UKE Audit, UKE Review, and narrative transformation protocols. These live across `protocols/`, `prompts/`, and `agent/analysis/`.

**Blog output:** [cafebedouin.org](https://cafebedouin.org) publishes daily using the pipeline — geopolitical analysis, philosophical essays, constraint stories, literary fiction, written in the zuihitsu tradition. The pipeline is infrastructure for the blog and for the paper, not a product.

---

## The Paper

The current paper is v6.6: *Axioms and Consequences of Observer-Dependent Classification* (`docs/deferential_realism_paper_v6.6.md`).

- Six axioms (one empirical anchor, five structural), four theorems derived deductively
- Empirical findings on a 3,254-constraint corpus: Wasserstein transport, variance decomposition, game-theoretic analysis, parametric persistence, cognitive displacement
- Honest assessment framework distinguishing STRICT categorical correspondences (formally verified), STRUCTURAL analogies (mechanism confirmed, interpretation argued), and LOOSE interpretive claims
- Key sections: cognitive displacement analysis (§5.6), δ-band population analysis, Cultural Cognition psychometric bridge, 13 open questions for strengthening the framework (§6.6)

---

## Repository Structure

```
structural_dynamics_model/
├── prolog/                    # The engine
│   ├── stack.pl               # Module loader
│   ├── config.pl              # Single source of truth for parameters
│   ├── drl_core.pl            # Primary classifier (classify_from_metrics/6)
│   ├── constraint_indexing.pl # χ = ε × f(d(P)) × σ(S) computation
│   ├── structural_signatures.pl # NL/Coordination/Constructed detection
│   ├── validation_suite.pl    # Test runner
│   ├── data_repair.pl         # Auto-repair for missing measurements
│   ├── [75 additional modules]
│   └── testsets/              # 3,256 constraint story files (.pl)
├── python/                    # Post-processing and diagnostics
│   ├── enhanced_report.py     # Per-constraint structural analysis
│   ├── run_pipeline.py        # Pipeline orchestration
│   ├── cognitive_displacement_sweep.py
│   ├── cc_diagnostic.py       # Cultural Cognition profiles
│   ├── game_theory_*.py       # Nash, cover story, stability analysis
│   ├── config_sensitivity_sweep.py
│   └── [50+ diagnostic scripts]
├── prompts/                   # Generation specifications
│   ├── constraint_story_generation_prompt.md
│   ├── constraint_story_generation_prompt_json.md
│   └── uke_write_v2.1.md
├── protocols/                 # UKE protocol suite
│   ├── uke_write.md
│   └── uke_write_v2.1.md
├── agent/                     # Blog generation and narrative transform
│   ├── analysis/              # UKE Audit, Edit, Ground, Review, Reality
│   ├── narrative_transform/   # UKE narrative architecture
│   └── orchestrator.py        # Blog pipeline
├── docs/                      # Paper and framework documentation
│   ├── deferential_realism_paper_v6.6.md
│   ├── logic.md               # Formal classification rules
│   └── [framework docs, audit reports, analysis notes]
├── outputs/                   # Generated reports and data
│   ├── enriched_pipeline.json # Corpus data
│   ├── constraint_reports/    # 324 enhanced reports
│   └── [diagnostic outputs, game theory results]
├── json/                      # Constraint stories in JSON format
├── essays/                    # Analytical essays
├── examples/                  # Example compiled analyses
├── audit/                     # Audit data and reports
├── scripts/                   # Shell utilities
└── CLAUDE.md                  # Development instructions
```

---

## Current Status and Open Questions

**Current state:**
- Corpus: 3,256 constraints, 324 enhanced reports
- Paper: v6.6
- Engine: 79 Prolog modules, 910/0 tests passing
- Research infrastructure under active development — not production software

**Falsification tests:**
- Colombia 2026 elections (May–June 2026): model predicts forced binary convergence with specific failure criteria
- Tracked predictions maintained across analytical output

**Key open questions** (from the paper §6.6):
- **ε validation:** Does LLM-generated base extractiveness correlate with domain-expert judgment? Requires human-coded validation dataset.
- **Non-linear site extension:** Which results survive relaxing the 4-element linear chain to a DAG or lattice?
- **Temporal coupling:** The mechanism is implemented but dormant — requires longitudinal measurement data.
- **Empirical δ calibration:** Can cognitive displacement be mapped to psychometric instruments?
- **Per-constraint metric robustness:** Do individual classifications survive ε perturbation at ±10%?

**Known limitations:**
- The corpus is LLM-generated. Both corpora inherit whatever latent political grammar their training data shares. Structural invariants under inversion demonstrate axiom-derived stability, but a real-world corpus with expert-assigned metrics would be a stronger test.
- The psychometric bridge (Cultural Cognition profiles) is uncalibrated — it maps framework categories to Kahan's dimensions but has no empirical validation.
- The scope modifier σ is architecturally fixed at [0.8, 1.0, 1.0, 1.2] for all constraints. Whether this masks meaningful per-constraint variation is an open empirical question.

---

## Contributing

This is research infrastructure under active development, not production software.

**Valuable contributions:**
- New constraint stories (run the generation prompt on novel domains, validate with the engine)
- Falsification tracking (document where model predictions fail)
- Cross-framework comparison (how does DR classification compare to other analytical traditions?)
- Real-world validation data (expert-assigned metrics for constraints in domains you know)

**Please don't:**
- Rewrite the engine in Python — Prolog's logic programming enables the auto-repair and validation architecture
- Add constraint types without theoretical justification — the six-type space is empirically validated as sufficient
- Optimize for user-friendliness before methodology is validated

---

## Citation

```bibtex
@software{deferential_realism_2026,
  author = {cafebedouin},
  title = {Deferential Realism: Formal Framework for Observer-Dependent
           Classification of Social Structures},
  year = {2026},
  publisher = {GitHub},
  url = {https://github.com/cafebedouin/structural_dynamics_model},
  note = {79-module Prolog engine, 3,256-constraint corpus,
          paper v6.6. Framework tested across finance, governance,
          protocols, history, algorithms, theology, and literary domains.}
}
```

---

## License

**Creative Commons Zero v1.0 Universal**

You can copy, modify, and distribute this work, even for commercial purposes, without asking permission. If you improve the methodology, consider publishing your refinements so others can benefit.

---

**Last updated:** March 15, 2026
