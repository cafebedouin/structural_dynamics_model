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
99 Prolog modules, 223 main-corpus constraints (`prolog/testsets/`), 189 SOTU
constraints (`prolog/testsets_sotu/`), 100+ Python analysis scripts.

Key constraint: Correctness and reproducibility matter most. Model provenance
(which LLM built which testsets) is a feature, not a problem.

**Start here:** `docs/project_orientation.md` is the canonical operational reference for
any model entering this repo. It covers repo layout, classification architecture, the full
paper sequence with summaries, empirical findings inventory, open work items, and
methodological practices. Line-number anchors are anchored to git HEAD `db218d8c`
(2026-05-28); verify before citing, as high-churn files drift.

**Open questions tracker:** `ISSUES.md` (OQ-01 – OQ-28) logs unresolved
engine-level, schema-level, and paper-synchronization issues with status, evidence, and
what resolution would change. Check it before touching drl_core.pl, product_site_export.pl,
or the rope gate — OQ-01 and OQ-02 are directly relevant to those files.

**Implementation wiring notes:** `docs/technical/` contains file-level notes on
non-obvious wiring, operator-precedence bugs, fact-adapter patterns, and query gotchas
discovered during implementation sessions. Scope is narrow: things that caused real bugs
or confusion, not general architecture. Read the relevant file before modifying
`config_validation.pl`, `cs_kernel_registry.pl`, or the CS fact schema.
`build_discipline.md` documents two recurring cross-subsystem defect patterns
(produced-but-not-consumed; silent fork) with diagnostics — consult before adding a step
that writes output or copying a file to test it.

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

- **Corpus is 223 constraints (not 3,337).** The reduction reflects a deliberate rebuild:
  exploratory committer-axis generation runs reused constraint IDs across runs (the
  "chimera" documented in OQ-25 and v7 §5.11 "corpus provenance" note). Cleanup triaged
  collisions, archived stale duplicates, and reduced testsets/ to a single coherent run
  (kernel_run_03: 109 CS readings + ~114 observer-axis constraints). §5.11 trifurcation
  figures are verified single-run coherent. The 3,337 figure predates the rebuild.
- **Run-tagged subdirs (`prolog/testsets/<run_tag>/`) are isolated** — `corpus_loader.pl`
  uses a non-recursive glob (`testsets/*.pl`), so subdir stories are NOT loaded by default.
  This is **load-time** safety, not generation-time dedup. If `corpus_path` is ever changed
  to include a run-tagged subdir, or runs are flattened together, duplicate loading becomes
  live. The shield is the glob; removing it reopens the question.
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
- **2026-05-28: v6 of observers_not_humans paper — §2.3 correction** — Sign-flip is
  load-bearing only in tangled_rope constraint family, not corpus-wide. Empirical
  concentration: Jaccard +0.21 in tangled_rope vs +0.014 in snare+rope (14.6× difference).
  H0 (sign-flip is load-bearing) conditionally confirmed; condition is that rope-gate
  bypass behavior is treated as given (OQ-01 in `ISSUES.md`). Corrected universality-class
  claim from corpus-wide to regime-specific. Unified §2.3 and §3.3 as one mechanism
  (institutional sign-flip at d < d_zero) viewed at two resolutions. Jaccard range
  corrected to 0.697–0.833 from published v5 range 0.685–0.828 (full-corpus rerun,
  3,380 constraints, testsets_3000). See `docs/observers_not_humans_v6.md` and witness
  files `outputs/alt_power_transform_results.json`, `outputs/range_sweep_results.json`.
  OQ-05 and OQ-09 resolved.
- **2026-05-28: OQ-25 resolved — ε coherence load guard** — `config_validation.pl`
  now includes a `config_violation/1` clause that fires inside `validate_config_postcorpus`
  (called at end of `corpus_loader:load_all_testsets`). Rejects any load where the same
  ConstraintAtom carries two distinct `constraint_metric(C, extractiveness, E)` values —
  the chimera failure mode. Grouping key is ConstraintAtom (not KernelAtom; OQ-26
  rationale). §5.11 divergence count confirmed unchanged (79 pairs / 34 kernels).
  See `docs/cs_load_discipline.md` (regeneration protocol) and
  `docs/technical/config_validation_wiring.md` (implementation notes).
- **2026-05-29: kernel-linkage join wired** — `agent/generate_kernel_corpus.py` is now
  canonical (6 evidence signals; `commitment_corpus/generate_kernel_corpus.py` and
  `commitment_corpus/uke_scope_v2_json.md` deleted). Fix applied: `story_uid` now minted
  before `_kernel_id` injection in `process_batch_results` (ordering gate); `stamp_kernel_linkage`
  post-batch function added. Migration script `python/migrate_kernel_linkage.py` wrote
  `cs_contradiction_of` facts into 32 `*_contradictions.pl` files (idempotent, all SKIP on
  second run). 22 orphaned readings listed in bucket B (hand-confirm worklist); 72
  candidate standalones in bucket C (eyeball only). Validation suite: clean after all edits.
  `cross_reading_diff.py` on `end_of_life_decision_authority`: 3 readings, no warnings.
- **2026-05-29: build-discipline patterns documented** — two recurring defects named in
  `docs/technical/build_discipline.md`: produced-but-not-consumed and silent-fork.
  See build_discipline.md for diagnostics and the corpus-you-want naming rule.
- **2026-05-29: perturb() primitive implemented** — `python/sweeps/perturb.py` is the
  type-stability sweep primitive: `perturb(param, values) → re-export → fold-survival per
  kernel`. Uses Dialect A1 overlay (retract/asserta on config:param/2) + product_site_export
  re-export. Output schema: {fold_survival, stable, flipped, touched, coverage, per_reading}
  per kernel per param value. coverage=0 means "blind, not stable" (param didn't reach
  kernel's decision path at this value). Verified: determinism (byte-identical double-export
  diff=0), identity (snare_epsilon_floor=0.46: 0 kernels affected), detection (0.50:
  end_of_life_decision_authority fold_survival=0.917, coverage=0.167, 39 flips in
  vulnerability_protection_reading institutional contexts tangled_rope→naturalized).
  product_site_export must be explicitly loaded in overlay ([stack] alone does not load it).
  OQ-29 opened: 19/19 results files have no corpus_hash; bifurcation_results.json confirmed
  stale (7 flipping constraints are testsets_3000/ archive only, absent from live testsets/).
  dval_sweep does not exist in repo (grep exit 1). cross_reading_diff.diff() is the design
  model for the diff shape; the primitive has its own re-export loop. 5 type-stability sweeps
  collapse to perturb(); 9 resistant sweeps stay separate by design (see ISSUES.md OQ-29,
  plan file audit-only-do-not-functional-kay.md §6.1).
- **2026-05-29: stability band wired into enhanced_report.py (Phase 1 + Phase 2)** —
  `python/enhanced_report.py` now runs perturb() at generation time for kernel-linked
  constraints with confirmed governing params, renders a stability band section (E5), and
  writes `stability_band` to the JSON sidecar. Confirmed governing param: `snare_epsilon_floor`
  × `end_of_life_decision_authority` kernel (boundary at +8.7%, 39 flips; floor at +4.3%,
  no coverage). All other kernels render "not yet witnessed." Unlinked constraints render "no
  kernel linkage." Architectural finding: 76/97 kernel-linked readings have `false_natural_law`
  signature (unconditional tangled_rope) — chi_floor params reach the metric decision path
  (coverage>0) but the final type is signature-locked; they are NOT valid governing params.
  17/97 have `false_ci_rope` (conditional); 3/97 `coupling_invariant_rope`; 1/97
  `constructed_low_extraction`. `tangled_rope_chi_floor` is blind or signature-locked on all
  tested kernels. Phase 2 restructure: kernel cross-reading panel moved to top (immediately
  after verdict banner); Wasserstein, cohomology, game-theory, Level-3 distribution and
  structural sections stubbed out. File: 2698 lines (was 2836). Sidecar validator unchanged
  (extra fields pass silently).

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

## Build Discipline (recurring failure modes — check before declaring work done)

This repo was built fast and solo, and two defects recur across unrelated subsystems
because the producing step is the interesting part and the reconciling step is the boring
one that gets deferred. They are invisible at the moment they're introduced because the
producer *looks* complete. Name them; do not reproduce them.

**1. Produced-but-not-consumed (the dangling wire).** Information is correctly generated,
written to disk, and then nothing reads it back into the thing that needs it. Instances
already in this repo: sensitivity sweeps write `*_sensitivity_results.json` that no
consumer reads; SCOPE writes `kernel_grouping.json` but the grouping is (was) not stamped
into the `.pl` files, leaving stories with `cs_story_uid` and no `cs_kernel_id`; the
manifest convention exists so audits *can* cite provenance but nothing enforces that they
do. **Rule: a producer is not done until something consumes its output.** When you add a
step that writes data, either wire the consumer in the same change or add a check that
fails loudly when the output is unconsumed. A meter with no dial is not a meter.

**2. One-canonical-thing-became-two (the silent fork).** A file or record gets copied to
a scratch/test location, edited, and now two versions exist with no queryable fact saying
which is canonical — the knowledge lives only in someone's memory. Instances: the
duplicated `generate_kernel_corpus.py` (`commitment_corpus/` test copy vs `agent/`);
historically, multiple tracking surfaces (ISSUES / AGENDA / PRIORITIES / TODO) where the
update protocol named only some. **Rule: one canonical location per thing, and which one
is canonical must be a checked fact (a path in docs, a CI check), not a memory.** Before
duplicating a file to test it, prefer a branch or a clearly-marked-temporary copy with a
deletion plan. When you find a fork, resolve by evidence (which path do run-commands
invoke, which imports resolve, git recency) — not by preference — and record the verdict
in `Known State` so the next agent does not re-fork it.

3. Destructive-replace without proof (the faith merge). Before
deleting, retiring, or overwriting any script, sweep, data file,
or generator that a downstream step or another version relies on:
run old and new, paste both outputs, diff them, show identity or
justify every difference in the same change. The old version is
not removed until the new is shown faithful. "Structurally
equivalent" is a code-read, not proof — the diff is
proof. Consolidating N into one is N separate old-vs-new diffs,
each before its standalone is retired. Instance already in this
repo's near future: collapsing the 5 type-stability sweeps onto
perturb.py — each sweep gets an old-vs-primitive diff before its
bespoke version is deleted, or the consolidation is faith, not
fact.

The first two reduce to the same root: **the corpus/codebase you are building for is not the one
on disk now.** Build naming schemes, linkage rules, and reports to be correct for the
corpus you intend (thousands of stories, regeneration under schema change, found-article
ingestion), not the sample that happens to exist. A scheme that *cannot* collide by
construction beats one that *happens not to* collide today. See
`docs/technical/build_discipline.md`.

## Critical Distinctions

**`json/` files are LLM-generated constraint specifications, not analysis output.**
Each file in `json/` is produced by step 3 of the orchestrator (Sonnet generates it
from an axis in the UKE_SCOPE manifest). The orchestrator writes the JSON to `json/`
and the corresponding Prolog testset to `prolog/testsets/`. These files are inputs to
the analysis pipeline — `run_pipeline.py` reads them; it does not write them. Analysis
output lives in `outputs/`.

**Canonical framework paper: `docs/deferential_realism_paper_v7.md`.** v7 promotes
the committer axis from "commentary-grade annotation" (v6.12 §4.2) to a co-equal second
axis: Axiom 7 (authored commitment structure with computed consequence), Theorem 7
(detection independence — observer-coherent readings can be committer-foreclosed),
Theorem 8 (licensed plurality vs. real closure), §4.5 (two-axis engine), §5.11
(trifurcation profile). Axioms 1–6 and Theorems 1–4 are unchanged from v6.13.
`docs/deferential_realism_paper_v6.13.1.md` is a parallel amendment to the observer-only
line, formally updating Axiom 2 for OQ-26 (ε is reading-relative across generation runs).
Files through v6.13 are superseded for the full framework; use v7.

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

Done includes the next step, landed in substrate — not stated in
chat. A task is not complete until a fresh instance, reading only
the repo (not this conversation), could pick up the next forward
move. If the session surfaced a next step, a sequencing
constraint, or a fact that currently lives only in the
conversation, write it where the cold read will find it: the
relevant OQ in ISSUES.md, the ordering note in
PRIORITIES.md/AGENDA.md, or a comment at the code it concerns. A
next step spoken in chat and not written to substrate is a handoff
that did not happen — the produced-but-not-consumed defect at the
seam between sessions. State the next step and its sequencing
constraint (why this one, what it's gated behind), because the
bare next-step is the one a cold reader most easily gets wrong.

## Audit Methodology

Completed audit passes follow: **recon** → **proposal** → **execution** → **writeup**.
Recon establishes what data exists and what questions are answerable. Proposal states
exactly what will be run and what would constitute each verdict. Execution runs scripts
and saves raw output. Writeup analyzes from evidence only — never from documentation
restated as findings. See `docs/project_orientation.md` §8.1 for detail.
