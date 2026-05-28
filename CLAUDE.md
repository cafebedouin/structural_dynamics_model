## Priority Gate

At the start of every session, read `PRIORITIES.md`. In your first response, state the
current top priority in one sentence.

If a requested task does not appear to advance any item in `PRIORITIES.md`, say so in
one sentence at the start of your response — then proceed. Do not block or ask for
confirmation; a note is enough. Example: *"Note: PRIORITIES.md lists §2.3 paper
correction as the top priority; this task addresses X instead."*

The note is for the user's benefit, not yours. They may have a good reason; they do not
need to explain it.

**If you know a better way:** When a request has a cleaner implementation, a simpler
approach, or an unintended consequence the user likely did not see, say so in one
sentence before doing the work — then proceed. The user sometimes asks for suboptimal
things without realizing it. A one-sentence flag is enough; do not withhold action
pending approval.

## Context Window and File Size Constraints

If a file or task grows large enough that you anticipate context limitations will affect
your ability to work with it cleanly, **prompt the user with how you'd like it resolved**
before proceeding. Examples of useful prompts:

- "This file is 2,000+ lines. I can read it in chunks, or you could split it. What's best?"
- "Reading the full corpus would exceed context. Shall I sample by constraint type instead?"
- "The corpus + docs + code together would leave little room for draft output. Would you prefer I focus on analysis only and save drafts separately?"

Do not silently work around context constraints by using suboptimal approaches. Make the
constraint visible and ask for the user's preference — they may have information about
what matters most for this task.

## Project Context

Prolog+Python research infrastructure implementing Deferential Realism (DR).
91+ Prolog modules, 3,337 main-corpus constraints (`prolog/testsets/`), 189 SOTU
constraints (`prolog/testsets_sotu/`), 100+ Python analysis scripts.

Key constraint: Correctness and reproducibility matter most. Model provenance
(which LLM built which testsets) is a feature, not a problem.

**Start here:** `docs/project_orientation.md` is the canonical operational reference for
any model entering this repo. It covers repo layout, classification architecture, the full
paper sequence with summaries, empirical findings inventory, open work items, and
methodological practices. Line-number anchors are anchored to git HEAD `55df084a`
(2026-05-09); verify before citing, as high-churn files drift.

**Open questions tracker:** `ISSUES.md` (OQ-01 – OQ-13) logs unresolved
engine-level, schema-level, and paper-synchronization issues with status, evidence, and
what resolution would change. Check it before touching drl_core.pl, product_site_export.pl,
or the rope gate — OQ-01 and OQ-02 are directly relevant to those files.

## Typical Workflow

The primary authoring loop is:

```
python3 agent/c-orchestrator.py "some topic"
```

This chains six steps automatically:
1. **Research** — web search grounding via Haiku
2. **Decompose** — UKE_SCOPE protocol selects axes (default 3) and produces a manifest
3. **Generate** — Sonnet generates one constraint story per axis; saves JSON to `json/`
   and Prolog testset to `prolog/testsets/`
4. **Corpus update** — runs `python/run_pipeline.py` to re-classify the full corpus
5. **Reports** — `python/enhanced_report.py` writes `outputs/constraint_reports/<id>_report.md`
   for each new constraint
6. **Essay** — Sonnet synthesizes a draft essay from the constraint reports; saved to
   `outputs/essays/` and `agent/analysis/essays/`

After the run, take `outputs/constraint_reports/*.md` and the essay draft to a model for
final essay synthesis. Finished essays are posted to cafebedouin.org and are not
committed to the repo beyond what the pipeline writes automatically.

**Corpus growth:** To expand the corpus without a full topic run, use
`python3 -m agent.generate_json_haiku` (reads `prolog/beta_seeds.json`, generates via
Haiku batch API with prompt caching). This is how the corpus grew from ~1,000 to 3,337.

## Running the System

- Full pipeline (analysis only, no generation): `python3 python/run_pipeline.py`
- Prolog tests (corpus validation): `cd prolog && swipl -g "[stack], [validation_suite], run_dynamic_suite, halt" -t "halt(1)"`
- Prolog unit tests (engine): `cd prolog && swipl -g "[stack], [tests/test_snapshot_migration], run_tests, halt" -t "halt(1)"` — substitute any file in `prolog/tests/` (except `test_battery_variants.pl` which is a variant harness, not a plunit test)
- Linter: must be imported as library (`from linter import lint_file`), not run directly
- Config sensitivity: `python3 python/config_sensitivity_sweep.py`
- Directionality sensitivity: `python3 python/directionality_sensitivity_sweep.py`

## Known State

- Last audit (2026-02-28): passing tests / param sweep — live items migrated to ISSUES.md (OQ-11 – OQ-13); historical record in AUDIT.md
- Config params: see `prolog/config.pl` for current count (`grep -c "^param(" prolog/config.pl`)
- All numeric params inert at ±25%; all 17 directionality constants inert at ±25%
- Corpus is actively growing; param count and testset numbers will drift — cite the manifest
- **2026-05-28: green cut applied to `product_site_export.pl:75–77`** — added `!` after
  `write_one_entry` in `write_entries` clause 3 to enable LCO and fix OOM under
  compressed-ceiling sigmoid variants. Zero-diff verified (3,380 constraints, before/after
  outputs in `outputs/cut_proof_*.json`). Underlying choice-point question is OQ-02 in
  `ISSUES.md`.
- **2026-05-28: python/ phase-1 reorganization** — 8 tests → `python/tests/`, 12 sweeps
  → `python/sweeps/`, 19 audits → `python/audits/`. Frozen CLI commands
  (`run_pipeline.py`, `enhanced_report.py`, `config_sensitivity_sweep.py`,
  `directionality_sensitivity_sweep.py`) and all load-bearing pipeline modules stay in
  `python/` root. ~30 exploratory scripts stay (phase 2 pending). sys.path fixes applied
  to all 39 moved files. Verification script: `python3 python/verify_reorg.py`.

## Pipeline Output Manifest Convention

Pipeline output JSONs carry a `manifest` top-level key with provenance information:
timestamp (`pipeline_run_at`), corpus counts (`n_constraints`, `n_sotu_constraints`),
git commit (`code_commit`, `code_commit_short`), dirty-tree flag (`code_dirty`), and
`schema_version`. Audits running against pipeline output should cite the manifest in
their writeups. The corpus is continuously extending (orchestrator runs add constraints),
so "the corpus" is meaningful only relative to a timestamp; the manifest makes the
timestamp citable. See `when_apparatus_sharpens_taxonomy.md` §4.1 for context.

## Architecture Invariants

- All classification routes through classify_from_metrics/6 in drl_core.pl
- config.pl is single source of truth for param/2 facts
- Dual threshold: both χ AND ε must be checked
- .tsx artifacts are outputs, not infrastructure
- Archive testsets document build provenance, not active code

## Critical Distinctions

**`json/` files are LLM-generated constraint specifications, not analysis output.**
Each file in `json/` is produced by step 3 of the orchestrator (Sonnet generates it
from an axis in the UKE_SCOPE manifest). The orchestrator writes the JSON to `json/`
and the corresponding Prolog testset to `prolog/testsets/`. These files are inputs to
the analysis pipeline — `run_pipeline.py` reads them; it does not write them. Analysis
output lives in `outputs/`.

**Canonical framework paper: `docs/deferential_realism_paper_v6.13.md`.** Files
`deferential_realism_paper.md` through `deferential_realism_paper_v6.12.md` in `docs/`
are superseded. When the framework spec is needed, use v6.13.

**Formal classification rules: `docs/logic.md`.** This is the spec document; `config.pl`
must match it. UTF-8 encoding was repaired Feb 2026 (prior versions had mojibake from
double-encoded characters). Edit tool fails on files with multi-byte mojibake — use sed
or Python if you encounter this.

**`dr_type_at/4` and `classify_snapshot/3` have been replaced (2026-05-17):** Both carried
DEPRECATED markers using the legacy `power_modifier` χ path (χ = ε × π, omitting σ).
Replaced by `classify_at_time/4` (`drl_composition.pl`) and `snapshot_type/3`
(`transition_paths.pl`) using the canonical sigmoid pipeline (χ = ε × f(d) × σ(S)).
Callers `constraint_history/3` and `degradation_chain/3` updated accordingly.

**`site_contexts_product/1` scope exclusion is calibration-based.** The product site
excludes `regional`, `continental`, `universal` scopes (`constraint_indexing.pl:954–955`).
σ(universal) = σ(national) = 1.0 (`config.pl:117,120`) — no differential χ effect between
the two. The actual reason: these three scope atoms appear in no canonical context and their
scope_modifier values have not been validated against corpus classifications.

**Pre-computed values live in `outputs/pipeline_output.json`.** H¹, Arakelov heights,
MaxEnt distributions, and classifications are pre-computed by the pipeline. Read from
there; do not recompute from scratch.

## Math Employed in the Prolog Engine

**Calculus / Analysis:** sigmoid/logistic function, exponential function, linear slope
(least-squares accumulation), monotonicity test, drift velocity (first-order temporal
rate), drift acceleration (second-order).

**Probability / Information Theory:** Shannon entropy (normalized H/log N), maximum
entropy principle, Gaussian log-likelihood, log-sum-exp normalization, prior
distribution, Kullback-Leibler divergence (threshold), Boltzmann distribution /
partition function.

**Algebra / Arithmetic:** weighted sum, absolute value, normalization to [0,1]
(clamping), rounding/floor/ceiling, accumulation via fold, sum/mean/min/max aggregation.

**Threshold / Classification:** dual-threshold classification (χ AND ε), priority cascade
(mountain > piton(dead-coordination) > snare > scaffold > rope > tangled_rope > piton(fallback) > naturalized > unknown),
complexity-adjusted threshold (entropy-weighted).

**Power Scaling:** χ = ε × f(d) × σ(S) (canonical extraction formula), power modifier
π(P), scope modifier σ(S), sigmoid directionality f(d), cognitive displacement δ.

**Purity / Contamination Algebra:** Gaussian inverted-U (exp(−(x−μ)²/2σ²)), purity
degradation (max(0, intrinsic − contamination × immunity)), type contamination strength
coefficients, type immunity/susceptibility factors, information-theoretic excess
extraction.

**Graph / Network:** BFS on constraint graph, network purity (weighted cluster average),
contamination cascade/propagation, edge coupling strength.

**Fixed-Point / Iteration:** Jacobi iteration (simultaneous-update), greatest fixed-point
convergence (monotone convergence theorem), Scott-continuity / monotone endofunctor
— all in `drl_fpn.pl`.

**Category Theory / Structural:** presheaf evaluation (restriction across observer
contexts), site/coverage structure (observer-accessible stalks), logical isomorphism
(fingerprint equivalence), lattice operations (meet/join via max/min).

**Modal / Decision Logic:** modal operators (necessity/possibility) as composition rules,
Boltzmann factorization test (independence check), sorting/ranking.

Key files: `drl_core.pl`, `constraint_indexing.pl`, `drl_boltzmann_analysis.pl`,
`boltzmann_compliance.pl`, `drl_purity_network.pl`, `drl_fpn.pl`,
`maxent_classifier.pl`, `drl_composition.pl`, `logical_fingerprint.pl`.

## End-of-Session Documentation Review

When completing work that changes code, produces empirical findings, or resolves an
open question — whether or not the session ends with a PR — review and offer updates to:

- **CLAUDE.md** `Known State` — new mitigations, resolved issues, code changes with proofs
- **AGENTS.md** — any change to architecture, testing commands, naming conventions, or
  invariants that a future agent would need to know before touching the relevant files
- **`ISSUES.md`** — status changes (open → mitigated → resolved), new OQ
  items surfaced by the work, updated evidence or file references
- **AGENDA.md** — work packages started, completed, or newly identified
- **PRIORITIES.md** — promote, demote, or retire items based on what the session
  completed or unblocked; add new blockers that emerged

Offer the updates as a diff or edit proposal, not just a verbal summary. The four files
only stay useful if they reflect the current state of the code and open questions.

**Before any `git push`:** verify the four files above are current with respect to the
changes being pushed. A push that makes the docs stale is documentation debt that
compounds across sessions.

## Audit Methodology

Completed audit passes follow: **recon** → **proposal** → **execution** → **writeup**.
Recon establishes what data exists and what questions are answerable. Proposal states
exactly what will be run and what would constitute each verdict. Execution runs scripts
and saves raw output. Writeup analyzes from evidence only — never from documentation
restated as findings. See `docs/project_orientation.md` §8.1 for detail.
