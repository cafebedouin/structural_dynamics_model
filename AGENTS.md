# AGENTS.md — Bot Onboarding Reference

Canonical operational reference for AI agents working in this repository.
Start here before touching any file. See also `docs/project_orientation.md`
for the full research context and `ISSUES.md` for unresolved
engine-level issues.

---

## Contents

1. [Tech Stack](#1-tech-stack)
2. [Repo Layout](#2-repo-layout)
3. [Naming Conventions](#3-naming-conventions)
4. [Architectural Rules and Invariants](#4-architectural-rules-and-invariants)
5. [Testing Requirements](#5-testing-requirements)
6. [Generation Pipeline](#6-generation-pipeline)
7. [What Not to Do](#7-what-not-to-do)

---

## 1. Tech Stack

### Prolog engine

- **Runtime:** SWI-Prolog, invoked as `swipl`. No version pinned; use whatever
  `swipl --version` returns on the host. The engine uses standard ISO predicates
  plus SWI-Prolog extensions (nb_getval/nb_setval, setup_call_cleanup, assertion
  dynamic predicates).
- **Entry point:** `prolog/stack.pl`. Loading it pulls in all 47+ engine modules
  in dependency order. Never load individual modules directly in production — always
  load through stack.
- **Module system:** each file declares `:- module(name, [exports]).`. The engine
  uses qualified calls (`module:predicate`) everywhere — do not rely on implicit
  imports.

**Stack layers (from `prolog/stack.pl`):**

| Layer | Key modules |
|---|---|
| Schema | `narrative_ontology`, `config` |
| Data | `corpus_loader`, `domain_priors`, `constraint_instances` |
| Classification | `constraint_indexing`, `structural_signatures`, `drl_core` |
| Composition | `drl_composition`, `drl_counterfactual`, `drl_fpn` |
| Diagnostics | `drl_boltzmann_analysis`, `drl_purity_network`, `sheaf_analysis`, `arakelov_height` |
| Commitment Systems | `cs_pattern_detection`, `cs_drift_engine`, `cs_axiom_engine`, `cs_kernel_registry`, `cs_drift_mismatch`, `cs_trifurcation` (within-kernel A/B/C disagreement router, OQ-55) |
| Management | `scenario_manager`, `data_repair`, `report_generator` |

### Python tooling

- **Python:** ≥ 3.10 (from `pyproject.toml`).
- **Core dependencies:** `pandas`, `jinja2>=3.1.6`
- **Optional stats:** `scipy`, `scikit-learn`, `statsmodels` (`pip install -e ".[stats]"`)
- **Optional AI:** `anthropic`, `google-genai` (`pip install -e ".[ai]"`)
- **Additional:** `google-api-core` (from `requirements.txt`)

### API keys (required for generation steps)

| Variable | Used by |
|---|---|
| `ANTHROPIC_API_KEY` | All Claude calls (orchestrator, story generation) |
| `GOOGLE_API_KEY` | Haiku batch generation (`agent.generate_json_haiku`) |

Neither key is needed for read-only analysis of the existing corpus.

---

## 2. Repo Layout

```
structural_dynamics_model/
├── prolog/
│   ├── stack.pl                  # Module loader — always load through this
│   ├── config.pl                 # SINGLE SOURCE OF TRUTH for all param/2 facts
│   ├── config_schema.pl          # Schema validation for config params
│   ├── drl_core.pl               # Primary classifier — classify_from_metrics/6
│   ├── constraint_indexing.pl    # χ = ε × f(d) × σ(S) computation
│   ├── structural_signatures.pl  # NL / Coordination / Constructed detection
│   ├── validation_suite.pl       # Test runner
│   ├── testsets/                 # LIVE corpus — post-de-leak rebuild (reset 2026-06-05;
│   │                             # cite the pipeline manifest for size, never a memorized count)
│   ├── archives/datasets/        # ALL previous corpora: kernel_v1/ (1,106 pre-reset),
│   │                             # original_v6/ (3,380 chimera-era), sotu/ (189), older sets
│   └── tests/                    # Engine unit tests (plunit); run from prolog/ with stack loaded
├── python/
│   ├── run_pipeline.py               # LOAD-BEARING pipeline orchestrator
│   ├── enhanced_report.py            # LOAD-BEARING per-constraint report generator
│   ├── linter.py                     # LOAD-BEARING library — from linter import lint_file
│   ├── config_sensitivity_sweep.py   # documented CLI sweep
│   ├── directionality_sensitivity_sweep.py  # documented CLI sweep
│   ├── [~23 pipeline modules]        # imported by run_pipeline.py
│   ├── [~30 exploratory scripts]     # game theory, SOTU, harvest — phase 2 pending
│   ├── shared/                       # utility package (loader, constants, maxent,
│   │                                 #   role_flip + independence read-site flags OQ-188/186)
│   ├── reports/                      # report query subpackage
│   ├── tests/                        # standalone test scripts (21 files, count 2026-08-12)
│   ├── sweeps/                       # parameter variation scripts (23 files, count 2026-08-12)
│   └── audits/                       # audit, diagnostic, probe scripts (76 files, count 2026-08-12)
├── agent/
│   ├── c-orchestrator.py         # Primary authoring entry point (7-step chain; step 7 = gated auto-commit)
│   ├── llm_call.py               # Canonical Anthropic call path (ModelCallError, count_tokens) —
│   │                             #   import this, NOT c-orchestrator (its hyphen blocks import)
│   ├── make_brief.py             # Compress oversized/refusing sources → NEUTRAL structural brief
│   └── generate_json_haiku.py    # Batch corpus expansion via Haiku
├── json/                         # LLM-generated constraint specs (INPUTS, not outputs)
├── audits/                       # Completed audits — one audits/<YYYY-MM-DD>_<slug>/ per
│                                 # audit, writeup + evidence together (MANDATE: new audits
│                                 # go here, not docs/ or outputs/; see audits/README.md)
├── outputs/                      # All pipeline output (classifications, reports, essays)
│   ├── pipeline_output.json      # Pre-computed H¹, heights, classifications
│   ├── constraint_reports/       # Per-constraint enhanced reports
│   └── essays/                   # Synthesized essay drafts
├── docs/
│   ├── project_orientation.md    # Canonical operational reference
│   ├── deferential_realism_paper_v8.md  # Canonical entry-point paper (v7/v6.13.1 = detailed records)
│   ├── logic.md                  # Formal classification rules (must match config.pl)
│   ├── ISSUES.md                 # Unresolved engine/schema issues (OQ-01 – OQ-13)
│   └── [100+ specialized docs]
├── CLAUDE.md                     # Development instructions (read before editing code)
├── ISSUES.md                     # Single tracking surface (OQs; status grammar + checker in footer)
└── README.md                     # Framework overview and documentation index
```

**Key distinction:**
- `json/` → LLM-generated constraint specifications. The orchestrator writes here;
  `run_pipeline.py` reads from here. Do not write analysis results here.
- `outputs/` → all pipeline output. Read pre-computed values from
  `outputs/pipeline_output.json`; do not recompute H¹, Arakelov heights, or
  MaxEnt distributions from scratch.

---

## 3. Naming Conventions

### Prolog testset files

**File name:** `snake_case.pl` — one constraint per file, matching the constraint ID.
Example: `abolition_reading.pl` contains constraint `abolition_reading`.

**Module declaration (line 1 of body):**
```prolog
:- module(constraint_<basename>, []).
```
Example: file `abolition_reading.pl` → `:- module(constraint_abolition_reading, []).`

**Required multifile block** — every testset must declare all of these, even if
the file does not use all of them:
```prolog
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.
```

**Required section headers** (use `/* === N. SECTION NAME === */` style):
1. Constraint Identity Rule (DP-001 comment block)
2. Namespace Hooks
3. Narrative Context
4. Base Properties (Domain Priors)
5. Indexed Classifications (P, T, E, S)
6. Validation Tests
7. Omega Variables
8. Integration Hooks
9. Temporal Measurements

**Context tuple** — exactly arity 4, always in this order:
```prolog
context(agent_power(P), time_horizon(T), exit_options(E), spatial_scope(S))
```
Do not add extra arguments. Linter rule `CONTEXT_ARITY` rejects any other arity.

**Power atoms** (P): `powerless | moderate | powerful | organized | institutional | analytical`

**Time horizon atoms** (T): `biographical | generational | civilizational`

**Exit option atoms** (E): `trapped | limited | analytical`

**Spatial scope atoms** (S): `local | regional | national | continental | global | universal`

**Classification type atoms**: `mountain | rope | tangled_rope | snare | scaffold | piton | naturalized`

### Prolog engine / infrastructure modules

**Prefix scheme (ratified by the OQ-16 rename, 2026-06-25):** file prefixes are
*concept markers*, not arbitrary. A new engine module reuses an existing concept
prefix; do **not** invent a new family.

- `cs_*` — the CS (commitment-systems) subsystem (e.g. `cs_drift_engine`,
  `cs_pattern_detection`). `cs_` marks the *concept*, not "a Prolog file."
- `metric_*` / `context_profile_*` — self-identifying names on the metric/observer
  side, kept deliberately distinct from the CS-side concepts they used to collide with
  (`metric_drift_events` ≠ CS commitment-drift; `context_profile_mining` ≠ CS
  commitment-trajectory). This is why the OQ-16 rename chose `metric_*`, **not** `dr_*`.
- **There is no `dr_` file-prefix scheme** — no file carries it. Naming a new module
  `dr_foo.pl` would create a lone one-member family that splits an existing cluster.
  Pick the concept prefix the module's behavior belongs to.

### JSON constraint specs

**File name:** same `snake_case.json` as the Prolog counterpart.

**Required top-level keys:**
`header`, `base_properties`, `perspectives`, `omegas`, `measurements`,
`interval`, `commentary`, `boltzmann`, `network`, `directionality_overrides`

Manifest key (`manifest`) is added automatically by the pipeline; do not hand-write it.

### Python scripts

No enforced naming convention beyond the existing `snake_case.py` pattern.
The linter must be imported as a library — it cannot be run directly as
`python3 python/linter.py`; it exposes `lint_file(filepath)` and
`lint_dir(directory)`.

**Filesystem paths — import from `python/paths.py`, never re-derive.** It is the
single source of truth: `REPO_ROOT, PROLOG_DIR, TESTSETS_DIR, JSON_DIR, OUTPUTS,
SCHEMAS, PROMPTS, DOCS, AUDITS, AGENT_DIR, PYTHON_DIR`. Do **not** write
`Path(__file__).resolve().parents[N]` (depth-fragile — the wrong N silently yields
the wrong root) and never hardcode an absolute `/home/...` path. Root detection
walks up to the `pyproject.toml` marker, so it is depth-agnostic and survives
worktrees/tarball checkouts (unlike `.git`).

- Top-level `python/foo.py` (run as `python3 python/foo.py`): `from paths import REPO_ROOT, PROLOG_DIR, OUTPUTS`.
- Nested `python/audits|sweeps|tests/foo.py` (any depth): prepend the **byte-identical, depth-agnostic** bootstrap — copy it from any neighbor at any depth and it still resolves correctly (there is no `parents[N]` to copy wrong):

  ```python
  import sys
  from pathlib import Path
  _here = Path(__file__).resolve()
  _root = next(c for c in (_here, *_here.parents) if (c / "pyproject.toml").is_file())
  sys.path.insert(0, str(_root / "python"))
  from paths import REPO_ROOT, PROLOG_DIR, OUTPUTS
  ```

(~69 older scripts still re-derive the root inline; migrating them — and the
package-vs-`paths.py` question that decides the target — is tracked as OQ-132.)

---

## 4. Architectural Rules and Invariants

### The classification invariants

**Rule 1 — Single entry point.** All classification routes through
`classify_from_metrics/6` in `prolog/drl_core.pl`. This is the canonical threshold
predicate. If you find a code path that classifies without calling this predicate,
it is a bug.

```prolog
% Signature:
classify_from_metrics(+Constraint, +BaseEps, +Chi, +Supp, +Context, -Type)
```

**Rule 2 — Dual threshold.** Both χ (chi, power-scaled extractiveness) AND
ε (epsilon, base extractiveness) must be checked. A predicate that checks only
one is wrong. The rope, snare, and tangled_rope clauses in `drl_core.pl` all
check both.

**Rule 3 — Config is the single source of truth.** All numeric parameters live
in `prolog/config.pl` as `param/2` facts. Do not hardcode threshold values
anywhere else. If `logic.md` specifies a threshold, it must match `config.pl` —
`logic.md` is the spec; `config.pl` is the implementation.

**Rule 3b — Thresholds never reach the authoring LLM (de-leak, 2026-06-05).**
The engine's classification bands must NOT appear in any author-facing surface:
the generator prompt (`prompts/constraint_story_generation_prompt_json.md`), the
JSON schema (`schemas/constraint_story_schema.json` — its text ships verbatim in
the generation prompt via `story_generator_base.build_prompt`), or LLM retry/
feedback text (`regenerate_stories.py` filters `THRESHOLD_COUPLED_LINT` codes;
`c-orchestrator._sanitize_feedback_error` scrubs bound values). The authored
claim vs. computed type diff is the research signal; disclosing a band lets the
author back-compute the claim and collapses it (KNOWN_STATE.md 2026-06-05).
Before adding ANY numeric threshold, metric band, or "required when ε > X" rule
to those surfaces, run the assembled-payload check: dump
`story_generator_base.build_prompt(...)` and grep it for band values near type
names — it must stay clean.

**Rule 3c — `prolog/axiom_concept_registry.pl` is baked, never hand-edited (OQ-72,
2026-07-04).** The ratified concept seat for `axiom_diff`'s `concept` key. Regenerate ONLY
via `python/axiom_concept_bake.py` from a ratified assignments TSV (fail-closed on
unratified rows) — a hand-edit loses its ratification provenance and voids the seat. The
registry is NAME-keyed (a mapping applies wherever the axiom name occurs, on any corpus
leg); an axiom absent from it reads `unmapped` = NOT-YET-RATIFIED, never "no shared
subjects" (GAP-24; `axiom_concept_tranche_kernel/1` marks tranche coverage). Regression
guard: `prolog/tests/test_axiom_diff.pl` (corpus-independent).

**Rule 4 — Priority cascade.** Classification priority (highest to lowest):
`mountain > piton(dead-coordination) > snare > scaffold > rope > tangled_rope > piton(fallback) > naturalized > unknown`

The cascade is implemented as ordered clauses in `classify_from_metrics/6`. The
order of clauses in `drl_core.pl` IS the priority.

**Rule 4b — dispatch clauses use fresh-variable heads + unify-after-cut (2026-08-17).**
`classify_from_metrics/6`, `constraint_signature/2`, and `classify_by_signature/3` bind
their output AFTER the cut, never in the head, so a bound call means "the engine
assigns" rather than "this clause body holds in isolation" (build_discipline Pattern 7).
Do not rewrite a head back to the atom form — gate row `dispatch head`
(`python/dispatch_head_check.py`) and `prolog/tests/test_dispatch_bound_call.pl` both go
red on the revert. New dispatch predicates use the same idiom; an atom-headed one is
flagged by the gate row until declared or converted. Witnesses:
`audits/2026-08-17_bound_dispatch_hardening/`.

**Rule 4c — the idiom is valid ONLY where the last argument is an OUTPUT (2026-08-19).**
Applied to a predicate whose last argument is an INPUT the caller supplies, the rewrite makes
the first clause match every call, cut, and render every later clause unreachable — silently,
with every structural check green (witnessed: 129/279 `testsets` to 1106/1106 `kernel_v1`).
`dispatch_head_check.py` now carries `LAST_ARG`, one row per registry entry with its verdict
(`output` / `input` / `generator`) and the evidence that settled it; a row with no fact, or one
recorded `input`, turns the gate row red. **Before converting anything, read the predicate's
`%%` mode line** — that is where the answer already is, and it is what nothing in the instrument
chain was reading. Registry classes now include `unreached` (called by no corpus) and
`generator` (never cut-ordered dispatch). Witnesses:
`audits/2026-08-18_classb_conversion_rollout/`.

### The chi formula

```
χ = ε × f(d(P, E)) × σ(S)
```

- **ε** (epsilon): base extractiveness — structural property of the constraint,
  observer-independent. Read from `narrative_ontology:constraint_metric/3` via
  `constraint_data:base_extractiveness/2`.
- **f(d)**: sigmoid over directionality d — computed by `sigmoid_f/2` in
  `constraint_indexing.pl`. Default params: L=−0.20, U=1.50, d₀=0.50, k=6.0.
- **d**: directionality — derived from observer's power position (P) and
  beneficiary/victim structure. Continuous [0, 1].
- **σ(S)**: scope modifier — from `config.pl`. Values: local=0.8, regional=0.9,
  national=1.0, continental=1.1, global=1.2, universal=1.0.

**Power modifiers (π)** — used to derive d, not directly in χ:

| Atom | π value |
|---|---|
| powerless | 1.5 |
| moderate | 1.0 |
| powerful | 0.6 |
| organized | 0.4 |
| institutional | −0.2 (net beneficiary) |
| analytical | 1.15 |

### Classification thresholds (from `config.pl`)

| Gate | Parameter | Value |
|---|---|---|
| mountain ε ceiling | `mountain_extractiveness_max` | 0.25 |
| rope χ ceiling | `rope_chi_ceiling` | 0.35 |
| rope ε ceiling | `rope_epsilon_ceiling` | 0.45 |
| tangled_rope χ floor | `tangled_rope_chi_floor` | 0.40 |
| tangled_rope χ ceiling | `tangled_rope_chi_ceil` | 0.90 |
| tangled_rope ε floor | `tangled_rope_epsilon_floor` | 0.30 |
| snare χ floor | `snare_chi_floor` | 0.66 |
| snare ε floor | `snare_epsilon_floor` | 0.46 |

### The Two-Hub Architecture

Classification variation across contexts comes from exactly two independent sources:

- **Hub 1 (Power-Scaling Sigmoid):** `constraint_indexing.pl` — `derive_directionality/3`
  → `sigmoid_f/2` → χ = ε × f(d) × σ(S). Drives the chi-threshold gates.
- **Hub 2 (Effective Immutability):** `effective_immutability/3` in
  `constraint_indexing.pl` — discrete (time_horizon, exit_options) → mountain|rope.
  Drives the mountain gate and the snare immutability check.

When χ and type disagree across contexts, the disagreement originates in one or
both hubs. Identify which hub before proposing a fix.

### Structural signatures

`structural_signatures.pl` and `signature_detection.pl` provide overrides that
fire after metric-based classification via
`integrate_signature_with_modal(C, MetricType, FinalType)` in `dr_type/3`.

Override hierarchy:
1. ~~**NL (Natural Law) + `emerges_naturally`:** forces mountain regardless of metrics.~~
   **RETIRED 2026-06-17 (OQ-128).** The natural_law overwrite
   (`resolve_modal_signature_conflict(_, natural_law, mountain)`, `signature_detection.pl:867`) is gone —
   the engine ROUTES disagreement, it does not RECLASSIFY (only review reclassifies). The DETECTOR
   (`natural_law_signature`/`constraint_signature(C,natural_law)`) survives as a router input —
   **but it fires ZERO times, by construction, on every corpus** (OQ-113; re-witnessed 2026-08-17):
   `has_viable_alternatives/2`'s two clauses bind arg 2 to the head literals `true`/`unknown`, so the
   `HasAlternatives == false` leg can never be met. "Survives as a router input" means WIRED, not
   FIRING — ~20 consumers read that constant zero (roster + hazards: OQ-296). Do not read a
   `natural_law` branch as reachable without checking OQ-296 first.
2. **Constructed sub-signatures:** three variants keyed by ε level.
3. Metric type passes through unchanged if no signature fires.

Signatures override; they do not replace the metric path. `dr_type/3` always
runs `metric_based_type_indexed/3` first.

**Engine ROUTES, never RECLASSIFIES (OQ-128, 2026-06-17) — `prolog/routing_sink.pl`.** Do NOT add a new
signature *overwrite* (a clause that rewrites `dr_type` to manufacture a verdict) and do NOT revive `:867`.
The author↔engine `dr_type` disagreement is the PRODUCT, routed per-SEAT to a review address by
`routing_sink:seat_diff/7` (seven typed MECE addresses: `generation_gap` / `authoring_review` /
`engine_exit_table_review` / `no_route` / `both_silent` / `engine_abstained` / `author_engine_divergence` —
no catch-all). It taps `dr_claim_mismatch/4` UNMODIFIED, emits `outputs/routing_sink.json`
(wired into `run_pipeline.py` Phase 2) — consumed by `enhanced_report.py` (per-seat, in CONSTRAINT IDENTITY).
**Tripwire:** the leaf record is per-SEAT — never collapse seats into
one constraint verdict (KILL §9b.4; the aggregate-merge recurred 3× in the OQ-128 arc). Author side: per-seat
`constraint_classification` keyed by `agent_power` (archives) else the seat-blind `constraint_claim` (live
corpus authors one claim, not per-seat). Detail: `audits/2026-06-17_mountain_authoring_sweep/ROUTING_SINK_DESIGN.md`.

**Suppression is reclassification too (OQ-122, 2026-06-18).** The forbidden move is not only an
*overwrite* — *gating a detector OFF* on a condition is the same anti-pattern in disguise: the engine
deciding not-to-flag instead of routing the flag. The dropped FSM victim-gate
(`once(narrative_ontology:constraint_victim(C,_))` so `false_summit_mountain` stops firing on no-victim
cases) reads as safe — `dr_type` byte-identical, no overwrite added — and that is the trap. The
route-consistent fix is to **discriminate the signature's SEVERITY**, never suppress the detector: let
it fire and route the non-diagnostic case as `informational` (no headline floor) while the diagnostic
case keeps its floor. Template: the type_1 ε-split (`drl_core.pl:629–638`) and the FSM victim-split
(OQ-138, **LANDED 2026-06-21**). The price of doing this is a clean discriminant + a written KILL condition.

**FSM converted RECLASSIFY→ROUTE, the reusable severity template (OQ-138, 2026-06-21) —
`signature_detection.pl`.** `false_summit_mountain` no longer overwrites `dr_type` (config default
`false_summit_override_target` is now `mountain`; hook stays an ablation lever). The grade contract
gained a **converted-signature path**: `converted_signature/1` + `signature_diagnostic_severity/3`
grade a converted signature on its OWN discriminant (FSM: `vic>0→moderate/correction`,
`vic=0→informational/commentary`), NOT on a `MetricType≠FinalType` delta (which is always zero once a
signature reverts) — mirroring `dr_claim_mismatch/4`. **Contract change to note:** a converted
signature's `informational` severity IS emitted as an alert (the visible route, distinct from
"dropped"); the legacy "commentary-grade gets NO alert" rule holds only for still-overwriting
signatures. **For a NON-seat-split signature** (all its cascade-winners genuinely overridden, like FSM): add it to
`converted_signature/1`, give it a `signature_diagnostic_severity/3` discriminant, AND remove it from
`abductive_helpers:known_override_signature/1`+`override_target/2` (else `probe_signature/3`/P1/P7
misfire). **For a SEAT-SPLIT signature** (FCR, OQ-138 2026-06-21 — false_ci_rope is 9 routed / 3 piton /
13 inert on ONE signature): the signature-level mechanism does NOT transfer — it would convert the inert
seats and disturb the carve-out. Build seat-aware instead: a `*_routed/1` predicate keyed on the stable
dispatch GATES + the dr_type OUTCOME (NOT a `metric_based_type_indexed` proxy — it diverges from the live
ModalType; the cross-corpus generality sweep catches this) AND keyed on the **UNBOUND cascade winner**
(`constraint_signature(C,Sig), Sig==<sig>` — under the PRE-2026-08-17 atom heads a bound-arg
`constraint_signature(C,<sig>)` tripped on the DETECTOR even when a higher-priority signature shadowed it,
§1 gotcha, and wrongly caught an FCR seat in `constructed_routed`; the 2026-08-17 head conversion (Rule 4b)
makes the bound form honest, but keep the unbound+`==` shape — it is regime-independent); `converted_at_seat/2` (signature-level for non-split, seat-level for split) feeding
`signature_grade`/`signature_severity`; and `seat_overrides/2`
(`abductive_helpers`) threaded through `probe_signature/3`+P1/P7 instead of removing the row from
`known_override_signature/1`. **For a LEVER-GLOBAL conversion** (FNL, OQ-138 2026-07-03,
commits `d248a6b1`/`82aa372e` — the overwrite dies EVERYWHERE at the config default,
`false_natural_law_override_enabled=0`: typed seats route, unknown seats abstain): the consumer recipe
differs from BOTH shapes above — key `seat_overrides/2` and the maxent boost on the LEVER, not on the
`*_routed/1` seat predicate. Reason (tripwire): `*_routed/1` predicates are DEFAULT-CONTEXT-keyed while
`resolve_modal_signature_conflict` overwrites fire at EVERY context (witnessed: `organization_floor_c0`
routes tangled_rope→scaffold at institutional while default-unknown), and the maxent boost applies
PER-CONTEXT (all 4 Wasserstein contexts) — a default-keyed guard goes silently stale at non-default
positions. Unifying rule across all three shapes: **the boost mirrors the LIVE type-layer overwrite at
that seat** (FCR non-routed seats keep boost because `fcr_override_enabled` defaults 1; FNL at lever=0
boosts nowhere). Default-keying stays fine for grade/severity (`converted_at_seat` — default-headlined
like verdict_join); the specific kill is a future consumer reading `*_routed` for ORBIT-SENSITIVE
override-liveness. Witness: `audits/2026-07-02_oq138_fnl_evidence/FNL_CONVERSION_DIFF.md`.
**Standing gate before converting any override:** decompose BOTH the seat's
own floor after revert AND what OTHER consumers read the manufactured type (the maxent distribution boost
`maxent_classifier.pl` `apply_override_for_sig/4` is now ALSO seat-aware — OQ-173, 2026-06-21: thread `C`,
skip the boost at `fcr_routed/1`/`constructed_routed/1`, the third surface after `dr_type` + the
override-artifact consumers; recipe in `signature_detection_wiring.md §4` Tripwire C; and the
corpus-relative maxent ENSEMBLE can ripple a multi-seat reroute to a carved-out seat, so a seat-split
carve-out is "TYPES unchanged", not "verdict byte-identical"). **Routed false-summits read RED at the report surface, not green** — reverting the type to
mountain unmasks the dirac(`second_class`)+cohomology(`fails_descent`)+abductive tensions the override
was hiding; that is honest commentary, classification unchanged (operator ruling 2026-06-21: the
engine comments, does not reclassify, and diagnostics may render different verdicts). Witness:
`audits/2026-06-21_oq138_fsm_route_conversion/`.

**Piton is an FCR-branch refinement (OQ-90, 2026-06-11):** `piton` is NOT a signature;
`dr_signature` stays `false_ci_rope` while `dr_type` becomes `piton`. The refinement fires inside
`resolve_with_perspectival_check/4` (between the dead-coordination piton clause and the generic FCR
clause), keyed on `narrative_ontology:piton_candidate/1` (= authored-`diffuse` gain_flow ∧
`prohibitive` fixing_cost — uncaptured AND prohibitive to fix), guarded by
`config:param(piton_refinement_enabled, 1)` (a separate axis from `fcr_override_enabled` — it fires
even when that is 0). Any new report/consumer that keys on signature will see `false_ci_rope` for a
piton; key on `dr_type` to surface the piton. The old `Supp ≤ 0.2` `piton_signature` profile gate is
retired. Reading "piton sparse" requires the upstream-shadow caveat: a piton_candidate that is
CI_Rope-certified upstream of FCR never reaches the refinement (designed shadow). Audit:
`audits/2026-06-11_oq90_piton_refinement/`.

### Deprecated predicates — do not call

| Deprecated | Replaced by | File |
|---|---|---|
| `dr_type_at/4` | `classify_at_time/4` | `drl_composition.pl` |
| `classify_snapshot/3` | `snapshot_type/3` | `transition_paths.pl` |

The legacy predicates used the old `power_modifier` χ path (χ = ε × π, omitting σ).
The replacements use the canonical sigmoid pipeline.

**That legacy path is FULLY DRAINED as of 2026-07-25 (OQ-67, commit `a8ec22f0`).** `drl_audit_core.pl`
is deleted, and `constraint_indexing:power_modifier/2` went with it as its sole reader — χ = ε × π
no longer exists anywhere in the engine, so there is no second χ path to compare against. It was
*unreachable*, not merely deprecated: its call sites sat behind `constraint_data/2` / `agent_index/2`
fail-stubs nothing ever asserted. Also deleted from `drl_composition.pl`: `is_snare/1`,
`is_mountain/1`, `is_rope/1`, `detect_perspectival_risk/4` (the per-seat type question is served by
`drl_core:is_snare/3`, `is_mountain/3`, `is_rope/3` on the sigmoid path — note the arity change).
The six `power_modifier_*` config params survive with **no reader**, as calibration anchors for
`canonical_d_*`; a null sensitivity result for them means "no consumer," not "no sensitivity."
Retired capability declared at `design_gaps.md` GAP-29. Do not re-add `transition_paths.pl` to any
list of legacy-path members — that was already false before the deletion.

Note: the two replacements are NOT
equivalent to each other — `snapshot_type/3` is deliberately un-threaded (static-fallback
semantics, clears the temporal nb-globals at entry) and diverges from `classify_at_time/4`
at points where temporal and static metrics differ; see
`audits/2026-06-11_oq83_close/STEP1_REPORT.md` before comparing their outputs.

### Repair transitions (OQ-91, commentary-grade)

`transition_paths.pl:repair_transition/4` is the **upward dual** of the decay-only
`transition_path/4` (the 8 heads are all downward). It reuses `degradation_chain/3` (the
`snapshot_type` series) as its source — "upward" = the transitive closure of the 8 decay
edges read backwards (`unknown` excluded). 4th arg = the named repair op
(`maintain`/`splice`/`replace` rope line-ops; `scaffold_struck` the held-apart construction
op), a deterministic function of from/to + chain prefix. **It is COMMENTARY-GRADE: never wire
it into `classify_from_metrics/6`, the signature layer, or `verdict_join`** — it comments on
the authored numbers, it does not reclassify. Serialized as the additive `repair_transitions`
per-constraint field in `json_report.pl` (inside `preserve_classify_globals/1` so the
snapshot_type nb-globals cannot leak into later classification reads), rendered by
`enhanced_report.py:build_repair_section` (single data direction Prolog→field→Python; silent
on decay-only constraints = honest absence). If you extend the op map, keep clause selection
keyed on from/to/pre (NOT a bound 4th arg) so it stays correct under bound queries.

### Product site scope exclusion

`constraint_indexing.pl:954–955` excludes `regional`, `continental`, `universal`
scopes from the product site (156-context curated site). Their scope_modifier values
are not calibrated against corpus classifications. Do not add them back without
validation.

### Pre-computed values

**Read from `outputs/pipeline_output.json`, do not recompute:**
H¹, Arakelov heights, MaxEnt distributions, and per-constraint classifications
are pre-computed. The corpus is continuous-growing; the manifest key records
the timestamp and commit at which values were computed. Always cite the manifest
when reporting findings.

### Purity absence contract (OQ-60, 2026-07-23)

Purity carries **two absence tokens that must never be coerced, averaged, or unified**:
engine `unknown` (no-data — a subscore's evidence is unauthored; e.g. no `coordination_type`
⇒ no Boltzmann floor ⇒ `excess_extraction` fails; the old fabricated
`boltzmann_floor_default` fallback is REMOVED) vs `-1.0` (epistemic-gate-fail sentinel).
JSON serializes BOTH as `null` (`purity_score`/`purity_band`). Consumer rules: never
`.get("purity_score", 0)`; guard `number/1` before arithmetic OR any sort/max (atoms sort
before numbers — silent misorder); dispositive clean verdicts (pristine/`stable`/`pure_*`)
gate at coverage 1.0 → distinct abstention token (`inconclusive(no_data)`, `undetermined`)
while positive existentials (`purity_fail`/cascading) fire through unknown members;
descriptive stats carry `n_scored`/`n_total` (json `diagnostic.purity_n_scored/_n_total`).
**Fixture rule:** synthetic test constraints needing scorable purity must AUTHOR
`coordination_type` (+ `extractiveness`). Rulings R1–R4: ISSUES.md OQ-60; witnesses:
`audits/2026-07-17_oq60_purity_absence/`.

**Counting what a purity guard EXCLUDES: name which of FOUR causes you mean (OQ-356,
2026-08-24).** The canonical rejecting conjunction is
`catch(effective_purity(C,Ctx,EP,_), _, fail), number(EP), EP >= 0.0`, and it drops four
distinguishable populations: **(a)** `effective_purity` SUCCEEDS with a non-number (the `unknown`
defect class), **(b)** it THROWS (dropped by the `catch/3`), **(c)** it FAILS (the conjunct
fails), **(d)** it returns a NUMBER below 0.0 (the `-1.0` gate-fail sentinel, dropped by the
comparison, not by the guard). An excluded-count written as "count the unknowns" covers only (a).
Measured on `testsets_haiku`'s giant component the split is **4 (a) + 6 (d)** — so such a count
would have missed 6 members and any `kept + excluded == total` identity would then break **as a
false alarm attributed to the guard**, which is worse than no check. Name the variable for what
it holds (`NExcluded`, never `NUnknown`), derive it from the SAME conjunction the guard rejects
in ONE pass (never a second, independently-written test), and if you write an EXPECTED value for
it, say which causes you are predicting: the OQ-356 plan itself predicted 0 on two legs from
"zero unknown-purity members" and measured 1 and 2, all cause (d).

**Naming the COUNT correctly does not name the LABEL correctly — they are two separate acts, and
the label is the one a reader sees.** OQ-356's first implementation got the variable right and
then printed *"members with no numeric effective purity"*, which means `NUnknown` and was false
of 6 of the 10 members it counted. Whatever you print next to such a count must be true of ALL
four causes. Corollary for anything that classifies legs by their exclusion behaviour:
**define degeneracy against cause (a), never against `NExcluded`** — a leg with zero cause-(a)
members (so the guard changes nothing on it, making it a perfect invariance oracle) can still
have a non-zero `NExcluded` and exercise the conservation identity non-trivially. The two
readings come apart. Witness:
`audits/2026-08-24_oq356_purity_guard/` (`exclusion_cause_census.pl` + WRITEUP).

**And a conservation check over such counts needs TWO identities, not one.** With four action
bands covering `[-0.01, 1.01)` while the filter admits ANY numeric `EP >= 0.0`, a value at or
above 1.01 lands in no band. Assert `NS+NB+NW+ND == NKept` (band coverage) and
`NKept + NExcluded == |Members|` (partition totality) separately, and **accumulate `NKept`
independently rather than deriving it as `|Members| - NExcluded`** — derived, the second identity
is true by construction and tests nothing.

**Band vocabularies are disjoint by design (OQ-62, 2026-07-25).** Four predicates band a purity
scalar and they are NOT interchangeable: `logical_fingerprint:purity_zone/2` is the canonical
spec bander and the only one still called `purity_zone` (.9/.7/.5/.3); the others are
`fpn_report:ep_band/2` (.7/.5/.3), `giant_component_analysis:action_band/2` (config
`purity_action_*` floors) and `abductive_helpers:fpn_band/2` (.8/.6/.4/.2). Three formerly shared
the name and three words meant two ranges each — unifying them again fails silently. All four
return the same `unknown`, which is a deliberate shared token, not an oversight. The categorical
verdict `structural_purity/2` emits `purity_fail(Reasons)` (was `contaminated(Reasons)`), which
means "≥1 of four boolean tests failed" at ANY scalar value — not "scalar in the contaminated
band." Convention table: `docs/logic_extensions.md` §2.3.1.

### Headline verdict contract (OQ-98, 2026-06-11)

Per-constraint entries in `pipeline_output.json` / `enriched_pipeline.json` carry BOTH
`diagnostic_verdict` (raw 12-subsystem synthesis) and `verdict_join` (the joined headline:
base verdict + severity-floored alerts + grid/measurement provenance + signature grade;
introduced at manifest `schema_version` 2; the manifest is at 3 since OQ-306).
Any consumer that summarizes a constraint must headline
`verdict_join.verdict`; `diagnostic_verdict.verdict` is a raw input — rendering it as a
summary recreates the GREEN-over-severe-alerts defect. Producer:
`diagnostic_summary:verdict_join/3`; serialization: `json_report.pl`; contract:
`python/shared/schemas.py`. Evidence: `audits/2026-06-11_oq98_verdict_join/`.

### `cs_verdict/2` clause placement — the orthogonal-guard cut trap (OQ-39, 2026-06-25)

Adding a new `cs_verdict(C, …)` clause to `cs_pattern_detection.pl`: **every existing clause ends
in `!`.** Those cuts are harmless *among themselves* only because each is gated on a DISTINCT,
single-valued `cs_pattern` (the clauses are mutually exclusive, so the cut prunes nothing
reachable). A clause gated on anything ORTHOGONAL to `cs_pattern` (e.g. `dr_type=scaffold`, as
`scaffold_suppression_escalating` is) is NOT mutually exclusive with the family — so on a
constraint that matches both, **placed below the family an earlier clause's `!` silently prunes the
new verdict; given a trailing `!` of its own it prunes the others.** Either way `findall(V,
cs_verdict(C,V), Vs)` drops a verdict, with no error. **Rule: a new orthogonally-gated clause MUST
be the FIRST `cs_verdict/2` clause and commit with `once/1` (a local cut over its inner goals only —
NO trailing `!`)**, leaving sibling clauses reachable on backtracking. The consumer
(`json_report.pl:562`, `findall` over `catch(cs_verdict(C,V),_,fail)`) gathers this verdict PLUS any
`cs_pattern` verdict. Mode caveat: such a clause needs **C bound** (it calls `dr_type(C,…)`); the
consumer always binds C, but a probe must iterate `corpus_constraint/1` — `cs_verdict(C, …)` with C
unbound returns 0, not the real set. Regression control: `tests/test_oq39_scaffold_escalation.pl`
(dual-verdict case proves BOTH verdicts survive). Provenance: KNOWN_STATE 2026-06-25.

### Completion-witness-or-fail-closed (OQ-112 item 2, 2026-06-23)

A pipeline stage that can be **absent or voided** must emit a **positive completion
witness** at genuine completion, and the consumer must **fail closed on its absence** —
never read green/clean over a stage that was attempted but did not complete. This is the
inverted default: a positive "I finished" fact, not a caught exception. `catch/3` is blind
to plain failure (it absorbs throws only), so absence-of-witness — not a recorded exception
— is the test; it catches throw AND clause-failure alike. **Per stage, its OWN distinct
fact:** sharing one witness across stages lets a completed stage mask a voided sibling.

- Witnesses (maxent): `maxent_classifier:maxent_run_info/3` (classical, `:555`/`:734`) and
  the **distinct** `maxent_indexed_run_info/3` (indexed, asserted after
  `maxent_classify_all_indexed`). Each asserted STRICTLY AFTER its per-constraint loop, so
  a mid-loop throw leaves it absent.
- Attempt markers: `diagnostic_summary:maxent_attempted/1` (`classical`|`indexed`), set at
  the `json_report.pl` stage boundary BEFORE the absorbing catch — distinguishing "not in
  this pipeline" (no marker → gate inert) from "attempted but voided" (marker present,
  witness absent → fail closed).
- Gate: `diagnostic_summary:maxent_void_alerts/1` injects `alert(maxent_voided(Stage),
  moderate, maxent_completion)` into `verdict_join` when a marked stage's own witness is
  absent for the consumed (default) context → headline floors to **yellow** (a void is
  absence-of-measurement, not a measured-severe finding; red stays reserved for genuine
  structure). The alert Type is machine-legible so a consumer keying on severity may branch
  on void specifically.
- Absorbers widened to `( catch(G,_,fail) -> true ; true )` so a stage failure continues
  the run (not a mid-pipeline crash) and the gate surfaces the void.
- **Any new stage that the report relies on must add its own completion witness + attempt
  marker + the void-alert clause.** Evidence: `audits/2026-06-22_oq112_round2/`.

### Network existence contract (OQ-95, 2026-06-10)

`drl_purity_network:constraint_neighbors/3` is **fail-closed on zero-fact atoms**
(`phantom_subject/1`): a constraint participates in the neighbor graph only if it
has a `constraint_claim/2` or any `constraint_metric/3`. Dangling authored
`affects_constraint/2` targets (LLM cross-references to testsets that don't exist)
are silently excluded in BOTH directions — as endpoints and as subjects. If a test
or probe asserts a synthetic constraint and expects it in the network, it must
also assert a `constraint_claim/2` (see `tests/test_forecloses_fpn_injection.pl`
setup). This is an existence test, not corpus membership — engine demos and
probsets pass. Regression suite: `tests/test_phantom_neighbor_filter.pl`.

### Cross-axis one-seat invariant — machine-enforced (OQ-15, 2026-06-24)

The two axes — DR/observer (`drl_*`, `dr_type`, `constraint_metric`) and CS/committer
(`cs_*`) — have a **machine-enforced boundary**: *no committer field reaches observer
computation except as the entailment-typed payload on the single forward bridge
`influences` → `detect_necessity_inheritance`.* The guard `prolog/check_axis_boundary.pl`
walks the loaded call graph and emits each committer→observer edge; `python/check_axis_boundary.py`
diffs them against `prolog/axis_boundary_allowlist.txt` (fail-closed on any un-allowlisted
edge). It runs in **both** `scripts/gate.sh` (`--selftest`) and `python/run_pipeline.py`
(beside the load-warning gate). **If you add a read of a `cs_` predicate from an
observer-side module, the gate goes RED** — that is by design. **This holds via
`--selftest`, which is two-sided**: its first case is `("negative (clean corpus)",
PROBE, 0)`, running the live scan against the live allowlist and requiring zero
un-allowlisted edges, alongside two planted-violation positives. (Confirmed
2026-08-20 at OQ-310's close, after that pass wrongly claimed the live arm was
unwired — it read `main()`'s dispatch and not `selftest()`'s case list.)
- A new **tooling** edge (comparison/validation, not feeding an observer verdict) is
  allowlistable with its role tag.
- A new **verdict** edge (a genuine *second* committer→observer bridge) is the **OQ-15
  synthesis trigger** — the cardinality convention is breaking, so re-open the OQ-15
  Phase-2 decision (relocate to a v7 named mediator) before allowlisting. Do not just
  add it. (Single-bridge is principled in KIND — `influences` is the entailment carrier;
  `forecloses`/`coexists_with` are committer-modal and never cross — but "exactly one"
  in CARDINALITY is convention-not-theorem, which is why the guard polices it.)
- The guard is corpus-independent (it walks code, not data; live/haiku/flash all → the
  same 8 edges). Provenance: `audits/2026-06-23_oq15_crossaxis_witnesses/`, OQ-15/OQ-135.

### Module-boundary bypass — decide by WHO OWNS THE WRITE (OQ-68, 2026-08-18)

A call written `other_module:pred(...)` reaches past `other_module`'s export list. SWI
permits it unconditionally, so an internal signature change fails **silently** at every
bypass site. The operator ruling settles what to do about any given one, and the axis is
**not** exported-vs-qualified:

| situation | repair |
|---|---|
| The module **asserts** and outsiders only read (`maxent_dist/3`, `fpn_neighbors_cache/3`) | encapsulation is real → **add an exported accessor** and swap the call |
| Outsiders **assert** into a namespace the module merely **hosts** (the `narrative_ontology` corpus-schema family) | nothing to breach → **qualification is the idiom**: add an allowlist row, do **not** export |
| An outsider asserts, **but the owner enforces an invariant** over what was written (`diagnostic_summary:maxent_attempted/1`) | the owner *means something* by the facts → **write accessor, fail-loud** — no allowlist row |

The third row is an **extension** of the ruling recorded 2026-08-18, not an application of it:
the hosting test is not "who asserts" alone but whether the module merely holds the facts or
also interprets them. Read the axis as four dispositions, not three — a pure host takes a row,
a host with an invariant takes a write accessor.

Exporting the corpus-schema predicates was ruled **against**: writers would still have to
qualify their heads or declare `multifile` locally, so it changes name resolution across
100+ modules and still leaves qualified writes. `ROLE=internal-state` is consequently not a
legal value in the allowlist — if you are reaching for it, you want the accessor.

Note the write direction cuts both ways: a store the module owns but an **outsider writes**
wants a **write** accessor, not a read one (`diagnostic_summary:maxent_attempt_reset/0` +
`maxent_mark_attempted/1`, whose caller is `json_report`).

Declared bypasses live in **`prolog/module_boundary_allowlist.txt`** (117 rows; grammar
`mod:pred/arity  ROLE=<role>  <reason>`, reason REQUIRED). The guard is
`python/module_boundary_check.py`, gate row **`module bounds`**, **eight** arms. Over the
allowlist: **A** every non-exported cross-module reference has a row; **B** every
`ROLE=corpus-schema` row has a **production-side** `:- multifile`; **C** every
`narrative_ontology:P(...)` head a testset writes has a row; **D** a schema predicate declared
for load-correctness but unwired goes RED the moment it **acquires a consumer** — because
declaring it turned it from *undefined* (throws) into *defined-but-empty* (fails silently) on
legs with no writers, and the first consumer must decide what an empty read means there.

Over **`prolog/schema_shape.txt`** (added by OQ-308, 2026-08-18): **E** two-way closure against
the register, plus the allowlist derivation as an IFF and a `DISPOSITION=` requirement for dead
members; **F** authored-value conformance on enforced argument shapes; **G** declared per-leg
emptiness against the head census; **H** a `narrative_ontology:P/N` reference whose arity the
namespace does not resolve. One run scans all five legs (~16.7s) and the GREEN line prints which;
**`--full` is retired** (accepted with a note, `--check` is a strict superset).

**`schema_shape.txt` is anchored on the DECLARATION set, not the corpus-schema rows.** Its
register is every `(name, arity)` declared `:- multifile`/`:- dynamic` into `narrative_ontology`
by any non-tests module — **63 members, computed by scanning, never a list**. The 40 corpus-schema
allowlist rows are a **derived view**: a row exists there IFF a story file writes that qualified
head, keyed name/arity (`measurement/5` says nothing about `measurement/2`). **23 register members
correctly have no allowlist row** — do not add one to close a perceived gap; arm E will red.
Do not narrow the scan to named modules: `narrative_ontology.pl` alone finds 57 and misses six.
**Arms F and G are drift ratchets, not specifications** — green means the schema has not changed
unnoticed, not that it is right; 54 of 162 argument positions are enforced, the rest documentation.

**If you add a corpus-schema predicate, it needs a `:- multifile` in
`prolog/narrative_ontology.pl`, an allowlist row, AND a `schema_shape.txt` row, in the same
change.** (A declaration that no story writes needs the `schema_shape.txt` row only — arm E
requires it, and arm E's derivation check forbids the allowlist row.) Registration is
opt-in — the same silent-escape shape as `reading_registry` registration and the spec-enum
sentinels. Two things that look like they defend a schema predicate and do not:

- **A `:- multifile` in a `tests/` file.** The production load chain never consults those
  files. `has_sunset_clause/1` lived this way undetected.
- **Every writing testset self-declaring.** It works until one generator revision drops the
  local declaration. Witnessed: strip it from all 28 loaded `flat_control_of/2` writers and
  the predicate loads **1 of 28** with 27 × "Redefined static procedure" (the
  `story_provenance/8` tombstone in `narrative_ontology.pl`); with the central declaration
  it loads 28/28. Stripping **one** writer proves nothing — `multifile` is a property of the
  predicate, set by whichever file declares it first.

**Do not "simplify" arm C into arm B.** They check opposite directions (B: row →
declaration; C: authored head → row), and arm C is specifically buying back a typo detector:
once a predicate is `multifile`, SWI stops warning on redefinition, and that warning was the
only thing catching a misspelled qualified head in a story file.

**Writing another source-text sweep over Prolog?** Three shapes broke this one before it was
trusted, all now fixture-controlled in the checker: predicate **indicators** (`mod:pred/2`
in a directive) are not 0-arity calls; **closures** passed to meta-predicates
(`maplist(m:pair_snd, ...)`) carry their real arity elsewhere; and **facade** modules
re-export (`drl_lifecycle` declares an empty export list and `:- reexport`s four modules, so
every call through it looks like a bypass).

---

## 5. Testing Requirements

### Running `scripts/gate.sh` — it resolves its own interpreter now

**The PATH workaround this section used to prescribe is obsolete (fixed 2026-08-18); plain
`./scripts/gate.sh` is correct.** `gate.sh` resolves the interpreter once —
`$SDM_PYTHON` → `.venv/bin/python` → `python3` — uses it for every row, and prints the
resolved path in its banner, so a gate transcript says which interpreter produced it.

```bash
./scripts/gate.sh                          # uses .venv automatically
SDM_PYTHON=/usr/bin/python3 ./scripts/gate.sh   # override, e.g. to reproduce a system-python red
```

**Correction to what this section previously said.** It called the `gap surfaces` red "an
invocation artifact, not a defect in the row — do not fix the check," and that read was
wrong in a way worth keeping: the row was reporting truthfully that *the interpreter running
it could not import pandas*. Treating it as an artifact is what let the condition persist —
and the single red badly understated the damage, because ~20 other affected tools are not
gate rows at all (`scipy` ×15, `anthropic` ×14, `numpy` ×16, `sklearn` ×6). One red row read
as one broken check; it was an empty interpreter.

The replacement is the FIRST gate row, `python env` (`python/python_env_check.py`): it
AST-scans `python/` + `agent/` and asserts the *running* interpreter can import everything
they import, deriving the required set rather than declaring it (a hand manifest would be a
second canonical list and would rot). It is first on purpose — **if it is red, later reds may
be downstream of a missing import rather than real findings.**

Two rules this leaves standing:
- **Never `pip install` into system python to clear a `ModuleNotFoundError`.** Install into
  `.venv` and, if the module is a new import, add it to `pyproject.toml` in the same change.
- **Never spawn `["python3", …]` from Python — use `sys.executable`.** A literal hands the
  child the system interpreter even when the parent runs under `.venv`; that bug was live at
  three orchestrator sites until 2026-08-18.

### Run the Prolog test suite

```bash
cd prolog && swipl -g "[stack], [validation_suite], run_dynamic_suite, halt" -t "halt(1)"
```

Expect: all tests pass, 0 failures. Any failure blocks merging.

### Discover and run python tools (single entry point)

```bash
python3 python/cli.py list                  # grouped tree of every tool + summaries
python3 python/cli.py <group> <name> [args] # run a tool (argv forwarded verbatim)
```

`cli.py` (OQ-163) is a transparent subprocess dispatcher — logical command groups point at scripts
wherever they physically sit (no file moves). `cli.py report ...` delegates to the reports package;
`cli.py menu` delegates to `omega_resolver.py menu`. `cli selftest` is wired into `scripts/gate.sh`.

### The pattern taxonomy: SETTLED numbering, and how it is kept settled (OQ-278)

**`CLAUDE.md` and `docs/technical/build_discipline.md` publish the same numbered defect taxonomy
— seven members at eight indices, index 3 a grave — and since 2026-08-17 they publish the same
member at every index.** They disagreed at 3 and 4 for 79 days and it went unnoticed for 151
commits, because the member COUNTS converged at `220739b8` (2026-05-30), the exact commit the
CONTENTS diverged; both copies read as a complete, coherent six. **So agreement is checked per
index on NAMES, never on totals.**

**The roster is not reproduced here.** Read it from either publishing document, or run
`doc_pattern_check.py --list` for both extractions side by side — a third authored copy in this
file would be the silent fork all over again, in the guard's own instructions, and nothing checks
this file. What is recorded here is only what the documents cannot tell you: which indices moved.

**Reading the historical record:** a pre-2026-08-17 `Pattern 3`/`P3`/`BD-P3` means the
*mechanism* — *bound-probe* (now 7) if it names `build_discipline.md`, the vacated
*destructive-replace* if it names `CLAUDE.md`. Their swept populations are declared per file in
`pattern_citation_check.py`. **Writing new ones:** the interim `CM-Pn`/`BD-Pn` freeze is lifted,
but `Pattern N`/`PN` is still seven-way overloaded (the concealment paper's `CWC:P3`,
`diagnostic_summary.pl`'s `P1`–`P10` conflict catalog, `Priority:` levels, essay enumerations,
decompose-manifest `candidate_pattern`, a Prolog variable), so name the taxonomy when the
surrounding text does not already say *build discipline*. A prohibition gate on the bare form is
not buildable at that false-positive rate.

```bash
python3 python/doc_pattern_check.py --check          # gate: index->name agreement, per index
python3 python/doc_pattern_check.py --list           # both extractions + the manifest
python3 python/doc_pattern_check.py --pairwise REV   # manifest-free agreement at a git rev
python3 python/pattern_citation_check.py --check     # gate: unswept consumers of a DISPLACED member
python3 python/pattern_citation_check.py --sweep     # regenerate the OQ-278 label set
```

Both run in `scripts/gate.sh` (`doc patterns`, `displaced cites`). Four things that bite:

- **Never store the pattern NAMES in either checker.** The manifests hold *locations* and
  *states*; agreement is computed from the documents. An authored copy of the taxonomy inside the
  checker that guards the taxonomy is the fork it exists to detect.
- **`DECLARED_COLLISIONS` and `DECLARED_SPINE_LAG` are now EMPTY, which is the strong state.**
  Nothing is exempted, so any divergence is a new fork and reds immediately. If you ever need to
  allowlist one, give it a STATE, and retire the entry in the **same change** as the repair — a
  *silent resolution* goes red as well as a silent fork.
- **`CLAUDE.md`'s items hard-wrap inside the bold run**, so the extraction regex needs
  `re.DOTALL`. Without it those items silently extract to nothing and the check reads GREEN while
  measuring a subset. After any edit to that list, run `--list` and confirm all eight indices
  resolve on both sides — a green `--check` alone does not witness that they are being seen.
- **Renumbering a member is a consumer sweep, not an edit.** Declare its citations in
  `pattern_citation_check.DISPLACED` *before* the move, while mechanism recovery is still cheap;
  once the old index resolves to it in neither document the distinguishing evidence is gone.

Full detail: `docs/technical/doc_pattern_check.md`; provenance
`audits/2026-08-14_oq278_index_collision/`.

### Check cross-document claim citations (OQ-287 standing guard)

```bash
python3 python/claim_cite_check.py --check      # gate mode
python3 python/claim_cite_check.py --selftest   # 10 red-capable controls
python3 python/claim_cite_check.py --list --unpinnable
```

Both run in `scripts/gate.sh`. `docs/amnesiac_institution/amnesiac_institution_v0_6.md` cites the
derivation it used to carry from `docs/concealment/concealment_without_a_concealer_v0_4.md`
(canonical; see each directory's `README.md`), and every citation is pinned to the **content** of
the Appendix A row it names, not merely to its label:

```
CWC:A2@31548228        # namespace : claim label : 8-hex digest of the whole row
```

Four things that bite:

- **The digest covers the WHOLE row, kill condition included.** Editing a row's kill condition moves
  the pin and fires every citing site even though the quoted claim is untouched. **That is the
  mechanism working. On a fire, RE-READ the site and decide — never bump the hex.** (Witnessed
  2026-08-14: one row's `Owed` cell was corrected, six sites fired, all six were re-read and one
  improved; the other fifteen digests recomputed identical.)
- **Get digests from the script, never by hand or by copying one out of a document:**
  `audits/2026-08-13_oq287_defork/claim_digest.sh <label>`. It is *the* definition of the recipe —
  a prose version was implemented two incompatible ways in one turn (trailing newline) and every
  digest was wrong. Do not reimplement it.
- **Opt-in, same silent-escape shape as `reading_registry` registration above:** a NEW citation is
  unguarded until its digest lands in the same change. The label class set is **open** (`A`, `E`,
  `P`, and the corollary `C1`) — never hardcode `[AEP]`. Concealment §5.1/§5.4/§9.1/§3.2 have no
  Appendix A row and are **unpinnable by construction**; write them `` `CWC` §5.4 ``, and do not
  mint rows to make citations checkable.
- **It verifies pin-matches-row and is BLIND TO APTNESS** — whether the cited row is the *right* one.
  A green tick over 60+ citations reads as verification of the citation set and is not; treat it as
  a hazard, not a caveat (`audits/2026-08-13_oq287_defork/EXTRACTION_PROMPT.md` §8 R2).

`amnesiac_institution_v0_6.md` §2.1–2.7 are **vacated and their numbers never reused**. Structure is
asserted by `audits/2026-08-13_oq287_defork/checks.sh all`.

**THREE papers now, not two, and each owns a different thing (2026-08-20, OQ-287 both limbs
discharged).** `docs/practice/practice_paper_v0_1.md` is canonical for **the practice** (the
discipline documents read as a development method); `docs/concealment/…_v0_4.md` for **the
derivation**; `docs/amnesiac_institution/…_v0_6.md` for **the institution**. Each directory's
`README.md` names its canonical file. The ordering is acyclic and none restates another.

**`amnesiac_institution_v0_6.md` §2.8 and §2.9 are now the SUPERSEDED side** — they keep their
numbers (and §2.9 keeps its (a)/(b) letters, because already-sent correspondence cites `§2.9(b)`),
and they carry **forward pointers** to `practice_paper_v0_1.md` §III and §V. **Cite the practice
paper for that material, not v0.6.** §7–§10 did NOT move and v0.6 remains canonical for them; the
distinction is `EXTRACTION_PROMPT.md` §3's two tables, and getting it backwards in either direction
recreates the duplication OQ-287 closed.

**v0.6's editing policy is `content edits, MARKED, never silent` — NOT "pointer-only."** The
pointer-only sentence stood in both the README and the paper's front matter while nine content-edit
commits landed against it; corrected at the README 2026-08-20 and carried to the paper the same day.
Corrections go in the house form (a dated `> **Correction marked (…)**` blockquote, or a
`[COST CORRECTED]`/`[RETIRED]` box) leaving the corrected text intact above them.

### Run the reading-totality suite (OQ-137 standing guard)

```bash
cd prolog && swipl -g "[stack], [reading_registry], [commentary_census], \
  corpus_loader:load_all_testsets, [tests/test_reading_totality], \
  run_tests(reading_totality), halt" -t "halt(1)"
```

Registry-driven: every `reading_registry:aggregatable_reading/3` entry classed
`total_on_domain` is proven exactly-one-solution over its declared domain (typed-absence
convention, `docs/design/design_discipline.md` §5). The SAME suite runs as a sequential
fail-fast gate at the top of `run_pipeline.py`'s Prolog phase — a red suite stops the
pipeline before the census. **When you add a reading predicate an aggregate could consume,
register it in `prolog/reading_registry.pl` in the same change** (registration is opt-in —
an unregistered reading escapes the guard; OQ-137 close, residual risk).

### Run the stakeholder-frame H¹ suite (OQ-207)

```bash
cd prolog && swipl -g "[stack], corpus_loader:ensure_corpus_loaded, \
  [tests/test_h1_spectrum], [tests/test_h1_stakeholder_spectrum], \
  run_tests([h1_spectrum, h1_stakeholder_spectrum]), halt" -t "halt(1)"
```

Live-corpus spectrum validity (`h1_stakeholder` ∈ proven H(n_real)), the
consensus_provenance↔H¹ coherence table (EXACT biconditional since OQ-217, 2026-07-13 —
the former D4 divergence cells are retired and fixture-pinned as
insufficiency/annotated-unanimity), and the fixture-pinned boundary cases. **Tripwire: two absence tokens — `untyped` (census-facing) vs `unknown`
(kernel-facing) — must never unify** (KNOWN_STATE 2026-07-12). Scope `run_tests/1` as
shown: a bare `run_tests` after corpus load also sweeps the testset-embedded units
(pre-existing failures unrelated to these suites).

### Run the empty-chair detector suite (OQ-151)

```bash
cd prolog && swipl -g "[stack], [tests/test_empty_chair], \
  run_tests(empty_chair), halt" -t "halt(1)"
```

`stakeholder_seats:empty_chair_state/2` (typed refinement of the mcc candidate set; census
source `empty_chair`): 8-token partition, dissent-wins multi-chair semantics, the
excluded_untyped fail-open, the mcc-Excl anti-fork mirror, and live refinement coherence
against (consensus verdict × h1_stakeholder). **Tripwire: in
`empty_chair_dissent*(T, DissentTypes, AllTypedExNames)` the third argument includes
CONCURRING chairs** (stated at the clause + registry entry) — never read it as "the chairs
that dissented". Corpus-facing vacuity guards fail by design on chairless corpora (e.g. a
kernel_v1 overlay) — that is the negative-domain control firing, not a regression.

### Run the ε-declaration suite (OQ-205 standing guard)

```bash
cd prolog && swipl -g "[stack], [data_validation], \
  corpus_loader:load_all_testsets, [tests/test_epsilon_declaration], \
  run_tests(epsilon_declaration), halt" -t "halt(1)"
```

Enforcement for the spec §3 fail-closed ε-provenance rule
(`docs/design/epsilon_declaration_discipline.md`): three-site equality
(`domain_priors:base_extractiveness/2` = `constraint_metric/3` = `epsilon_provenance/5`
ValueAsWritten — drift is gate-red), orphan-provenance and census-partition checks, with
planted in-memory controls keeping the gate non-vacuous on the pre-build corpus. Runs as
the second sequential fail-fast gate in `run_pipeline.py`'s Prolog phase, followed by a
second swipl over the Control P fixture corpus (`prolog/tests/fixtures/eps_controls/` via
`corpus_path` overlay — violations must equal exactly the planted set). Missing provenance
on pre-build stories is warning-grade (the declared loud-null stratum, operator ruling
2026-07-03 — NO backfill of any `testsets*` leg; the compiler emits `epsilon_provenance/5`
generator-forward). The ε-STABILITY sweep (`python/sweeps/epsilon_stability.py`, r=0.02,
data-side) runs in the post-parallel slot with its own fail-closed Control S selftest;
flags are commentary-grade (R4) and surface in `enriched_pipeline.json`, the report E5
section, and the report sidecar. Probe authors: took-effect guards on
`drl_core:base_extractiveness/2` must pin the FIRST solution (`once/1`) — the predicate is
multifile and an unpinned read backtracks past a shadowing direct fact (witnessed,
KNOWN_STATE 2026-07-03).

### Run the agency suite (OQ-66 standing guard)

```bash
cd prolog && swipl -g "[stack], [tests/test_agent_beneficiary], \
  run_tests(agent_beneficiary), halt" -t "halt(1)"
```

Enforcement for the TWO-GATE PRINCIPLE at `prolog/narrative_ontology.pl:398-419`: a
`non_agent_beneficiary/1` entry RELEASES a natural-law certification on its host, so it
needs both an ontology-kind gate and a host-convergence gate; an unlisted value defaults to
AGENT (fail-open to status quo). The suite checks registry contents, that the filter is
EXACTLY registry membership (single clause, static, no kind inference), that
`drl_core:natural_law_without_beneficiary/1` reads the filtered `agent_beneficiary/2` view
(landed 2026-07-25, ruling 63-A), and that the three snare floors are still config
constants. Runs as the fourth sequential fail-fast gate in `run_pipeline.py`'s Prolog phase
(`_prolog_agency_gate`), followed by a second swipl over the fixture corpus.

**Fixture-corpus convention (shared with Control P).** A planted fixture leg lives in
`prolog/tests/fixtures/<name>/` and is loaded through the REAL path — `corpus_path` overlay
asserted BEFORE `load_all_testsets`, in a **FRESH process**: the `corpus_loaded/0` guard
silently ignores an in-process overlay-after-load, and process exit is the cleanup. The
fixture pass always carries a count guard (`NFix =:= 4`) so an empty or half-loaded leg
cannot pass by absence. For `nlwb_controls/` the fixture pass is not a nicety — it is the
ONLY place the gate can fail: no beneficiary fact in any of the five live legs carries a
registered non-agent value, so raw and filtered reads are extensionally identical on the
live corpus and a revert of `drl_core.pl` would keep the live-corpus suite GREEN. All four
fixtures author identical metrics; the only variable is beneficiary composition, which is
what makes the pairing two-sided (`agent_only` must still reach snare while
`nonagent_only` must not). Fixtures that need scorable purity must AUTHOR
`coordination_type` (OQ-60).

**Do NOT read MaxEnt in this suite or any `[stack]`-loaded probe without refitting first** —
a plain `[stack]` load leaves MaxEnt unfitted and its reads fail soft (CLAUDE.md, Running
the System). That defect made the predecessor of this suite vacuous for its whole life.

### Run the purity-absence suite (OQ-60 standing guard)

```bash
cd prolog && swipl -l stack.pl -l covering_analysis.pl -l maxent_classifier.pl \
  -l dirac_classification.pl -l diagnostic_summary.pl -l post_synthesis.pl \
  -l json_report.pl -l giant_component_analysis.pl \
  -g "[tests/test_purity_absence], run_tests([purity_absence, purity_absence_producers, purity_absence_floor]), halt" -t "halt(1)"
```

17 tests, three units: preflight injection end-to-end (0a/0a.2 guards live, not shadowed),
two-sided golden + sentinel-XOR-unknown precedence, token totality, fpn/gc ingest-boundary
collapse (ordering guard), the four no-data producer termini (m1–m4), R3 aggregation
polarity, and the C-FLOOR m5 fixture (floor fails on absent `coordination_type`; authored
override/typed paths unchanged). Needs the PIPELINE load chain shown — `json_report` /
`giant_component_analysis` are not loaded by `[stack]`; the tests fail loudly (not silently)
on a short chain. Scope `run_tests/1` as shown (bare `run_tests` sweeps testset-embedded
units).

### Run the full analysis pipeline

```bash
python3 python/run_pipeline.py
```

This re-classifies the full corpus and updates `outputs/pipeline_output.json`.
Single-writer convention (2026-06-04): the swipl export writes
`outputs/pipeline_output.raw.json`; `run_pipeline.py` alone writes the canonical
manifest-bearing `pipeline_output.json`. A direct swipl re-export cannot clobber it.

The `trajectory` stage (HAC clustering, O(N²)) runs **sequentially after** the parallel
Phase-2 Prolog block, not within it — it must never co-reside with `giant_comp` (also O(N²));
co-residency intermittently stalled the pipeline (OQ-182, 2026-06-27, `_phase_prolog`).

### Classify a NON-default corpus (twin / comparison runs)

```python
from run_pipeline import classify_corpus
classify_corpus('testsets_haiku', 'pipeline_output.haiku.json', 'claude-haiku-4-5')
```

`classify_corpus(corpus_path, output_name, expected_model)` classifies an alternate
corpus into its OWN `outputs/<output_name>` without running the full pipeline and without
touching the canonical `pipeline_output.json`. Overlays `config:param(corpus_path)` with a
single deterministic clause and refuses on swap/zero-glob/load-incomplete/seen≠classified
(`expected_model` is a `story_provenance` model prefix every loaded story must match;
`None` for mixed-model corpora). Used by `python/audits/twin_comparison.py`. Serial only
(one classify run at a time — OQ-77).

### Run the REPORT stages on a non-default corpus (OQ-352, 2026-08-23)

```bash
python3 python/report_legs.py --legs testsets_sonnet2 testsets_sonnet3
python3 python/report_legs.py --selftest        # gate row `report legs`
```

`classify_corpus` runs only `run_json_report`, so a per-leg output carries per-story fields
plus the top-level `diagnostic` block and nothing else. `run_pipeline.report_corpus(corpus_path,
out_dir, ...)` is its SIBLING for the Phase-2 report stages, writing `outputs/legs/<leg>/`. Eleven
stages, **one fresh swipl per stage** (OQ-246) run **strictly serially** — which satisfies OQ-182
(`trajectory`/`giant_comp` never co-resident) for free; do not parallelize it. Gates the OUTPUT as
well as the input: each artifact must exist, be non-empty, carry its owed marker, and carry a
manifest sidecar whose **top-level** `corpus_hash` `assert_corpus_current` accepts.

**Four things that will bite before anything else.**

1. **A leg needs a SAME-COMMIT classify output** or the run refuses `MISSING_CLASSIFY_OUTPUT` —
   three of OQ-353's statistics are `json_report.pl` products, not report-stage ones. The name is
   `pipeline_output.<short>.json`, mirroring `python/audits/leg_diagnostic_table.py:57-59`
   exactly. **Consequence: classify and report must run at ONE frozen HEAD.** Any pass that
   commits AFTER classifying leaves its own artifact one commit stale.
2. **`giant_comp` currently throws on 17 of 20 corpora** (OQ-356) — 16 of 19 live legs plus
   `original_v6`. Exclude it with `--stages` until that lands, or the run refuses
   `ARTIFACT_ABSENT`. `testsets` passing is NOT evidence it works: its giant component is under
   `run_phase3`'s `GCFrac > 0.10` gate, so the block never executes there.
3. **Only `giant_comp`'s timeout is parameterized** (OQ-363). The other eight stages sit on
   `run_prolog`'s 300 s default, sized for n≈285; `abductive` times out 3×300 s on `original_v6`.
4. **Three artifacts are hard-coded to `../outputs/` in Prolog** (`orbit_data.json`,
   `abductive_data.json`, `giant_component_analysis.raw.json`) and are protected by
   `_TransitGuard`: flock lock, an on-disk journal written BEFORE the first delete,
   restore-then-refuse recovery. Guard state lives in `.report_corpus/`, never under `outputs/`.
   **A killed run leaves a journal; the next start restores and REFUSES rather than proceeding.**

### `manifest.code_dirty` changed meaning (2026-08-24, `e9ca54785`)

It was a bare `git status --porcelain`, which counts **every** untracked file — so writing an
audit file stamped `code_dirty: True` on artifacts whose code matched HEAD. It is now a
**fail-closed denylist**: `*.md`, `audits/**` (by location — an audit's `.py`/`.txt` is a record
OF a run, never an input to one), corpus trees and `outputs/` are excluded; **everything else
counts, including paths nobody anticipated.** Denylist not allowlist deliberately — an allowlist
fails OPEN, letting a new source location read clean on a run no commit reproduces.

**Reading older manifests:** a **pre-`e9ca54785` `code_dirty: True` may mean only that an
untracked file was present.** Do not infer non-reproducibility from it without checking what was
actually dirty. Corpus trees are excluded because corpus identity is `corpus_hash`'s job (OQ-29);
counting a leg mid-fill would pin the flag True for as long as generation runs.

**Twin comparison is N-general (≥2 legs; OQ-213(a), 2026-07-06).** `twin_comparison.py` takes
**two or more** `--twin label=path` legs, not exactly two — it crosses every unordered pair and
emits an N-way agreement partition (odd-leg tally per structural field, missingness complement
carried). Join guard: **ALL legs must share one `code_commit`** (any pair differing is refused —
model-difference would alias onto code-difference), and no two may share a `corpus_path`. So the
**precondition** for any multi-leg run is to re-classify **every** leg at ONE commit in a **single
serialized `classify_corpus` batch** (they share `pipeline_output.raw.json`, and no working-tree
edit may fall between the calls, else the commit stamps identical while the engine differs).
Example (three legs at HEAD):

```bash
python3 python/audits/twin_comparison.py \
  --twin haiku=outputs/pipeline_output.haiku.json \
  --twin flash=outputs/pipeline_output.flash.json \
  --twin sonnet=outputs/pipeline_output.sonnet.json \
  --permute 1000 --outdir audits/<date>_<slug>
```

Output JSON keys: `structural_H1_pairs` / `continuous_H2_pairs` (per-pair, tagged `"pair"`) +
`structural_agreement_nway`. Per-pair permutation RNG is salted by the sorted pair labels
(`random.Random(f"{seed}:{x}:{y}")`) → order-independent, but permutation-derived numbers are NOT
byte-comparable to the old bare-seed binary (deterministic fields ARE). The `sonnet_control` +
conditioned (OQ-125/123) block stay a 2-leg design (first two legs).

**A fifth leg exists (2026-07-20): `testsets_kimi/` (kimi-k2.6, n=1005)** — a full twin alongside
haiku/flash/sonnet (`expected_model='kimi-k2.6'`). **Read cross-model differences as dispositional,
NOT capability:** kimi was generated thinking-ON while the Claude twins were thinking-OFF (regime
confound), and the corpus measures unprompted authoring disposition, which dissociates from elicited
capability (witnessed — OQ-228, GAP-25, `audits/2026-07-20_five_leg_twin_comparison/`). A standalone
five-leg harness (`python/audits/five_leg_twin_comparison.py` + `five_leg_deeper_cuts.py`) exists
alongside `twin_comparison.py`.

### Stack consistency check (wrong-qualifier / undefined-predicate detection)

```bash
cd prolog && swipl -l check_stack.pl -g "run_check_stack, halt" -t "halt(1)"
```

Compare against the recorded baseline (KNOWN_STATE.md 2026-06-04); new findings are
regressions introduced by your change. As of 2026-06-25 it also loads the trajectory-mining
chain (`context_profile_mining`/`context_profile_report`, loaded by run_pipeline outside
`[stack]`) so their wrong-qualifier calls are covered too; baseline unchanged. Other standalone
report scripts remain uncovered (honest boundary noted in `check_stack.pl`).

### Orphan / dead-code census (export-vs-caller; OQ-38)

```bash
cd prolog && swipl -l orphan_xref.pl -g "run_orphan_xref, halt" -t "halt(1)"   # static census -> outputs/oq38_orphan_xref.tsv
python3 python/audits/oq38_orphan_sweep.py                                      # + dynamic-surface mask + funnel
```

`orphan_xref.pl` is a `library(prolog_xref)` clause-head-vs-body separator (sibling of
`check_stack.pl`: load-path-independent, **diagnostic NOT a pipeline gate**). It classes each
defined predicate `LIVE`/`ENTRYPOINT_CLI`/`STATIC_ORPHAN`; caller matching is global `Name/Arity`,
conservative-by-design (biases LIVE — a false orphan is the only dangerous error). The Python driver
masks static orphans against the dynamic surface (Python goal-strings + Prolog name-construction)
and emits the tool-native funnel. **`STATIC_ORPHAN` is an upper bound on "dead", never a strip list**
— value-adjudicate each (CLAUDE.md *Unwired ≠ worthless*). Provenance: `audits/2026-06-30_oq38_orphan_xref/`.

### In-session overlay probes

Use `probe_harness:with_retracted/2` / `with_overlay/3` (snapshot-first, verified restore,
automatic cache clearing via `cache_registry:clear_all_caches/0`) instead of hand-rolled
retract/assert. Corpus membership/denominator: enumerate `corpus_loader:corpus_constraint/1`.
Tests: `cd prolog && swipl -g "[stack], [tests/test_probe_harness], run_tests, halt" -t "halt(1)"`.

**IT NOW REPORTS THAT IT OVERLAID NOTHING (OQ-326 RESOLVED 2026-08-21).** Until 2026-08-21 the
harness verified RESTORE and nothing verified INSTALL: a template matching a RULE — or an
undefined predicate, a wrong arity, an unloaded corpus, an absent id — retracted nothing, warned at
most, and the asserted facts landed *after* the original clauses, so the "counterfactual" arm
measured the unmodified program and both arms came back identical with no error.

Six checks now run BEFORE the single mutation point (nothing throws past it, because
`setup_call_cleanup/3` registers Cleanup only once Setup succeeds), in the ruled order
**2 → 3 → 1 → 4/4′ → 5**:

| # | check | throw | escape |
|---|---|---|---|
| 2 | template resolvable | `probe_overlay_unresolvable/2` | none — always a defect |
| 3 | no RULE clause matched | `probe_overlay_partial/2` | `allow_partial` |
| 1 | per-template snapshot non-empty | `probe_overlay_empty/1` | `expect_empty` (an empty template LIST stays legal) |
| 4 | replacement reachable at TEMPLATE shape | `probe_overlay_shadowed/3` | `allow_shadowed` |
| 4′ | reachability decidable at all | `probe_overlay_reach_undecidable/1` | `reach_undeclared` (uncovered facts only) |
| 5 | target dynamic | `probe_overlay_immutable/1` | none — always a defect |

Check 2 is TEMPLATES ONLY: `assertz` into an undefined predicate is legal and creates it dynamic,
which is the ordinary fixture-planting idiom. Check 5 covers BOTH sides but guards on *defined*
first, for the same reason. Every escape carries `retrofit(Date,Text)` or `authored(Text)` —
anything else is a type error — suppresses ITS OWN clause only, and no combination clears 2 or 5.

**A bare `with_asserted/2` now throws `probe_overlay_reach_undecidable`**: no template means no
declared query shape, so reachability has no ground. Migrate with `reach_undeclared`, never
`allow_shadowed` (that one means "I checked and accept the shadowing"; these sites never had a
check to accept).

**Still true, and narrowed rather than retired:** structural install is NOT semantic effect. The
checks prove the clauses moved and that the replacement is reachable at the declared query shape;
they do not prove the observable changed. A probe still owes its own assertion inside the overlay
(`oq110`'s Control C is the model: it asserts the flip *disappears* under retraction and fails
loudly if it survives). `with_overlay/4` returns `overlay_report/4` so that assertion can paste an
install witness. Gate row: `probe harness`.

Retroactive census (OQ-326 Phase 1, DONE): 44 call sites / 27 files / 13 distinct retract-side
templates; 12 rule-free, 1 rule-bearing
(`constraint_indexing:constraint_classification/3` at `a1_probe.pl:77`) and that site checks out
safe — its rule clauses are keyed to the two engine demo constraints. **No prior audit is voided.
But `probe_harness.pl`'s own header example is the unsafe form of that same call.** Substitute
idioms: `docs/technical/swipl_load_path_and_probe_gotchas.md` §12. Evidence:
`audits/2026-08-19_oq302_bound_false_repair/overlay_template_census.md`.

### FPN sibling-contamination canary (OQ-23/OQ-24 regression)

`compute_edge_contamination/7` zeroes contamination from a same-kernel sibling donor (sibling
readings are linked by `affects_constraint` only to document ε-distinctness, not as a contamination
conduit). The canary guards this:
`cd prolog && swipl -g "[stack], [tests/test_coexists_fpn_canary], run_tests, halt" -t "halt(1)"`
(positive/negative/sentinel controls + the `no_coexists_or_forecloses_leak_on_loaded_corpus`
regression gate). Cross-leg measurement: `run_coexists_census/0` / `run_forecloses_census/0` under a
`corpus_path` overlay (per-leg census logs in `audits/2026-06-29_oq23_coexists_fpn_canary/`). Do NOT
extend the same-kernel guard into `constraint_neighbors_existing/2` (giant_comp topology): **OQ-193
RESOLVED (c) 2026-07-02 — same-kernel sibling edges are INTENDED topology and stay in the graph for
all 5 `constraint_neighbors/3` consumers.** The per-consumer price probe confirmed FPN is unaffected
(the OQ-23 contamination guard already zeroes sibling edges) while json_report/network_dynamics
change without feeding any re-classification; the misleading pooled count is fixed at the report
surface instead. **Report-build LANDED 2026-07-04** (`giant_component_analysis.pl`
`## Provenance split (OQ-193)` md section + same-run `giant_component_analysis.raw.json`;
`enhanced_report.py` per-constraint "NETWORK POSITION (OQ-193)" section + `network_position` sidecar).
Strip method = retract-recompute (NOT a post-hoc `gc_edge` filter — `deduplicate_neighbors` keeps the
strongest edge per pair, so an inferred edge resurfaces on recompute and only a substrate strip is
faithful), placed DEAD-LAST in `run_giant_component_analysis` so it is never restored. `run_pipeline.py`
pre-deletes the raw.json and stamps `giant_component_analysis.manifest.json` only when the `giant_comp`
step is `status==ok` — a stale raw.json can never pair with a fresh stamp. Behavior-preserving:
`pipeline_output.json` `per_constraint` byte-identical. Evidence:
`audits/2026-07-02_oq193_giant_comp_ruling/`; KNOWN_STATE 2026-07-04.

### Run the linter on a testset

The linter must be called as a library, not a script:

```python
from linter import lint_file
errors = lint_file('prolog/testsets/my_constraint.pl')
```

To lint a directory: `lint_dir('prolog/testsets/')`. Outputs go to
`outputs/lint_errors.txt` when run via the pipeline.

### Linter error codes (41 codes across 31 checks)

Any linted file that produces an ERROR-level code blocks acceptance into the corpus.
WARNING-level codes are data completeness issues logged but not blocking.

| Code | What triggers it |
|---|---|
| `BARE_CONTEXT` | Context declaration missing required wrapper |
| `CONTEXT_ARITY` | `context/N` where N ≠ 4 |
| `DEPRECATED_TERM` | `noose` used instead of `snare` (renamed v3.4) |
| `DUPLICATE_MEASUREMENT` | Same measurement point declared twice |
| `FLOOR_EXCEEDS_EXTRACTION` | `boltzmann_floor_override > base_extractiveness` |
| `GENERIC_GROUP` | Beneficiary/victim uses vague names (stakeholders, general_public, etc.) |
| `IDENTICAL_EXIT_OPTIONS` | Same exit option atom repeated across perspectives |
| `INSUFFICIENT_TEMPORAL_DATA` | High-extraction constraint (ε > 0.46) has < 6 measurement facts |
| `INSUFFICIENT_VARIANCE` | Fewer than 2 distinct types across indexed perspectives |
| `INVALID_CLAIM_TYPE` | `constraint_claim` type not in the 6 valid atoms |
| `INVALID_COORDINATION_TYPE` | `coordination_type` not in `{information_standard, resource_allocation, enforcement_mechanism, global_infrastructure}` |
| `INVALID_D_VALUE` | `directionality_override` d value outside [0.0, 1.0] |
| `INVALID_FLOOR_OVERRIDE` | `boltzmann_floor_override` outside [0.0, 1.0] |
| `INVALID_POWER_ATOM` | `directionality_override` power atom not in the 6 valid atoms |
| `INVALID_SCOPE` | `spatial_scope` not in the 6 valid atoms |
| `LOW_THEATER_RATIO` | Piton requires `theater_ratio ≥ 0.70` |
| `METRIC_SOURCE_INCONSISTENCY` | Same metric declared via both `domain_priors` and `constraint_metric` |
| `MISSING_BENEFICIARY` | Tangled rope, scaffold, or non-mountain constraint lacks `constraint_beneficiary/2` |
| `MISSING_CLAIM` | `constraint_classification/3` present but no matching `constraint_claim/2` |
| `MISSING_ENFORCEMENT` | Tangled rope requires `requires_active_enforcement/1` |
| `MISSING_HOOK` | Missing `narrative_ontology:interval/3` |
| `MISSING_METRICS` | No `extractiveness` or `suppression` metric found |
| `MISSING_MODULE` | File does not begin with `:- module(id, [])` |
| `MISSING_MULTIFILE` | Uses `directionality_override/3` without declaring it multifile |
| `MISSING_NL_PROFILE` | Natural law (mountain) constraint missing NL profile metrics |
| `MISSING_OMEGA` | High-extraction constraint (ε > 0.46) has no `omega_variable/5` |
| `MISSING_PERSPECTIVE` | Non-uniform type distribution missing powerless or institutional context |
| `MISSING_SUNSET_CLAUSE` | Scaffold with `requires_active_enforcement` lacks `has_sunset_clause/1` |
| `MISSING_TEMPORAL_DATA` | ε > 0.46 but no `narrative_ontology:measurement/5` facts |
| `MISSING_THEATER_RATIO` | Piton lacks `domain_priors:theater_ratio/2` |
| `MISSING_VICTIM` | Tangled rope or snare lacks `constraint_victim/2` |
| `MOUNTAIN_METRIC_CONFLICT` | Mountain classification conflicts with ε or suppression metrics |
| `MULTI_ID` | Multiple constraint IDs in one file (one constraint per file required) |
| `OUTDATED_HOOK` | Missing `constraint_indexing:constraint_classification/3` (v4.0 hook) |
| `REDUNDANT_MEASUREMENT` | Measurement point already exists |
| `SCAFFOLD_DANGER_ZONE` | ε ≤ 0.30, beneficiary data present, but no enforcement/sunset/theater |
| `SELF_REFERENCE` | `affects_constraint(X, X)` — constraint affects itself |
| `STUB_MISMATCH` | Claimed type inconsistent with actual metric profile |
| `UNRESOLVED_MANDATROPHY` | ε > 0.70 requires resolution hook or `[RESOLVED MANDATROPHY]` annotation |
| `VACUOUS_TEST` | `test/1` clause with unbound comparison variable |
| `VARIABLE_IN_AFFECTS` | `affects_constraint` uses a Prolog variable as target |

### Config sensitivity sweep

To verify that a parameter change does not alter any classification:

```bash
python3 python/config_sensitivity_sweep.py      # 154 numeric params at ±25%
python3 python/directionality_sensitivity_sweep.py  # 17 directionality constants at ±25%
```

Results to `python/config_sensitivity_results.json` and
`python/directionality_sensitivity_results.json`. The "all 154 params inert at ±25%"
finding (2026-02-28 audit) is **PRE-RESET / kernel_v1-regime** (measured before the
2026-06-05 corpus reset, OQ-29) — re-run both sweeps against the live corpus before
treating it as current. If you change a param, re-run and confirm it is still inert;
the result files now carry a `corpus_hash` (unstamped/mismatched ⇒ stale).

**Convention (OQ-29): every result-`*.json` producer stamps `corpus_hash`, every consumer
checks it.** Import the single source — `from corpus_hash import compute_corpus_hash,
assert_corpus_current` (`python/corpus_hash.py`) — never re-define the sha256 body (it forked
four ways before consolidation). A new sweep/producer stamps `compute_corpus_hash(<the corpus it
actually loaded>)` into its output dict at write time; a producer running against an ARCHIVE corpus
stamps the archive's testsets dir, not the live one (a wrong-corpus stamp is worse than none). A
consumer guards its input with `assert_corpus_current(path, testsets_dir)` (raise) or surfaces a
STALE banner for report-style output — fail-closed on absence/mismatch, never read a dead-corpus
file as authoritative.

### No pytest setup

There is no `pytest.ini`, `conftest.py`, or `setup.cfg`. Testing is Prolog-native
(validation_suite) plus Python linter. Do not add a pytest layer without discussing
with the maintainer.

---

## 6. Generation Pipeline

The primary authoring command:

```bash
python3 agent/c-orchestrator.py "some topic"
```

This chains seven steps automatically:

| Step | What it does | Writes to |
|---|---|---|
| 1 Research | Web search grounding via Haiku | (memory, no file) |
| 2 Decompose | UKE_SCOPE protocol selects every §3-distinct axis (no fixed count; `--axes N` is an optional ceiling, default none — changed 2026-06-05) | (manifest in memory) |
| 3 Generate | Sonnet generates one constraint story per axis. NOTE: resolves only flat `manifest["axes"]`; kernel-reading entries are skipped — kernel topics go through `agent/generate_kernel_corpus.py`, which since 2026-06-05 ALSO auto-generates a forced-flat control per kernel (`<kernel_id>_flat_control`, alignment key `narrative_ontology:flat_control_of/2`; OQ-76 — do not remove, and do not generalize to kernel-on-every-flat) | `json/`, `prolog/testsets/` |
| 4 Corpus update | Runs `python/run_pipeline.py` to re-classify full corpus | `outputs/pipeline_output.json` |
| 5 Reports | `python/enhanced_report.py` writes per-constraint reports | `outputs/constraint_reports/<id>_report.md` |
| 6 Tensions ledger | Deterministic extraction (`python/tensions_ledger.py`) — NOT an essay (OQ-101, 2026-06-10) | `outputs/tensions_ledger.md` |
| 7 Commit | `_step_commit` git-commits this run's `json/<cid>.json` + `prolog/testsets/<cid>.pl`. GATED (skips on `--no-commit`, run-tag, or failed corpus update) and SCOPED to the run's cids — never `git add -A`, refuses if the index already holds unrelated staged changes; local commit only, never pushes | git (local) |

**Big or refusing source files** (e.g. a 1.6 MB S-1, a paper the safety classifier refuses):
the orchestrator auto-compresses to a NEUTRAL brief only when the topic exceeds its MEASURED
ingest ceiling (`--brief`/`--no-brief` to force/suppress). A content safety-refusal STOPs by
default with a manual-route message; `--auto-bypass-refusal` is opt-in and logs the witness.
Standalone: `python3 agent/make_brief.py <file.txt>`. Caveat: a neutral brief of a single-voice
source (a prospectus) routes FLAT without research grounding — the kernel emerges once research
reintroduces the external contest (KNOWN_STATE 2026-06-08).

**To expand the corpus without a full topic run** (faster, uses Haiku batch API):

```bash
python3 -m agent.generate_json_haiku
```

Reads `prolog/beta_seeds.json`, generates via Haiku with prompt caching. This is
how the chimera-era corpus grew to 3,337 constraints (archived: `prolog/archives/datasets/original_v6/`)
archive; live `testsets/` is now 223 after the chimera-collision rebuild (see CLAUDE.md
Critical Distinctions / OQ-25). Cite the pipeline manifest, not a fixed count.

### Pipeline output manifest convention

Every pipeline output JSON carries a `manifest` top-level key:

```json
{
  "manifest": {
    "pipeline_run_at": "<ISO timestamp>",
    "n_constraints": "<live count — read from the manifest, do not memorize>",
    "n_sotu_constraints": "<0 since the 2026-06-05 reset; sotu archived at prolog/archives/datasets/sotu/>",
    "code_commit": "<full SHA>",
    "code_commit_short": "<short SHA>",
    "code_dirty": false,
    "n_stories": "<story members — THE denominator for any per-story rate>",
    "n_nonstory_members": "<known non-story members, e.g. *_contradictions.pl axiom files>",
    "nonstory_kinds": "<{kind: count}, sorted>",
    "n_unclassified": "<unknown + dual_family; 0 on a live leg or the run refuses>",
    "schema_version": "<version — 3 since OQ-306>"
  },
  ...
}
```

**When writing findings:** always cite `manifest.pipeline_run_at` and
`manifest.code_commit_short`. "The corpus" is only meaningful relative to a
timestamp; the manifest makes the timestamp citable.

---

## 7. What Not to Do

These rules are absolute. Violating them silently changes the system's semantics.

**Never:**
- Add classification logic outside `classify_from_metrics/6`. Any new type or
  threshold goes into `drl_core.pl` as a new clause in the correct priority position,
  and into `config.pl` as a new `param/2` fact, and into `logic.md` as a formal rule.
- `grep -v Warning` over swipl load output. Load warnings are gated:
  `python3 python/load_warning_gate.py` vs `prolog/load_warning_allowlist.txt`
  (wired into run_pipeline; a dead-module warning hid for four months this way — OQ-96).
- Synthesize `stakeholder_gain_flow/2` / `fixing_cost_class/2` from metrics or defaults
  anywhere (fabrication ban, OQ-92; `data_repair.pl` is the named door). Authored-or-absent,
  fail-closed; capture is `narrative_ontology:constraint_captured/1`, computed positively.
  **And treat `constraint_captured/1` as a NAME-IDENTITY join, not a flag (OQ-190, 2026-08-17):**
  it matches `stakeholder_gain_flow`'s receiver against the `constraint_stakeholder` roster *by
  name*, and gates `drl_core.pl` scaffold + `signature_detection.pl:1220/1378/1477` + the maxent
  mirror — so it is on the `dr_type` path, and both its authored sources score 1/6 positive
  draw-stable. A change that widens or loosens the match changes classification.
- Reintroduce grid injection/imputation in ANY form: the DR-AUDIT grid shim was RETIRED
  2026-06-11 (OQ-93 ruling (b); the `grid_shim_enabled` flag no longer exists) — the leveled
  grid is authored-or-absent (`coercion_grid` block in story JSON, compiled by
  `generate_constraint_pl.py` with fail-loud endpoint/duplicate integrity); absent points
  report `[OPEN]`, never manufactured 0.5s/priors. `data_repair:source_class/2` keeps its
  injected/imputed buckets for archive replays only.
- Call `coercion_projection:system_gradient/3` expecting `0.0` on an empty read: it FAILS
  on absence (OQ-93 coverage-carrying read killed the fabricated default). Use
  `system_gradient/4` (coverage(PresentLevels, AllLevels) travels with the value) or
  `system_gradient_for/4` (consumer-named levels: gradient(...) | open(...)).
- Add a new `config.pl` param without its `config_schema.pl` `param_spec` — the validator
  halts the stack load.
- Add numeric thresholds as hardcoded values. They go in `config.pl` only.
- Modify `config.pl` default values without re-running the sensitivity sweep.
- Call `dr_type_at/4` or `classify_snapshot/3` — both are deprecated with stale
  chi paths.
- Recompute H¹, Arakelov heights, or MaxEnt distributions from scratch. Read them
  from `outputs/pipeline_output.json`.
- Run the linter directly with `python3 python/linter.py`. It has no `__main__`
  block useful for direct invocation; use `from linter import lint_file`.
- Import from `python/tests/`, `python/sweeps/`, or `python/audits/` — those
  directories contain standalone scripts, not library modules. If you need to import
  something from a script in those directories, verify that import with a full-tree
  grep (Python imports AND subprocess path invocations) before adding it.
- Add new power, time, exit, or scope atoms without updating `config_schema.pl`,
  `config.pl`, and the linter's `INVALID_*` check lists.
- Load individual Prolog modules without going through `[stack]`.

**Canonical papers:**
- Framework entry point: `docs/deferential_realism_paper_v8.md` (seat/gauge/orientation
  vocabulary; OQ-135 adoption 2026-07-02). Detailed records: `_v7.md` (committer axis) and
  `_v6.13.1.md` (observer axis; internally v6.13.2) remain authoritative for proofs and
  empirical findings. Files `deferential_realism_paper.md` through `v6.13.md` are
  superseded. Do not cite them as current. NB: v7's "seat" = v8's "gauge" (v8 §5.4).
- Formal classification rules: `docs/logic.md`. This is the spec; `config.pl`
  must match it.

**Archive testsets** (`prolog/archives/`): document build provenance, not active
code. Do not modify them and do not treat them as the current corpus.

**The `.tsx` artifacts** in `outputs/` are generated output. Do not edit them
directly — regenerate via the pipeline.

**UTF-8 note:** `logic.md` had double-encoded UTF-8 (mojibake) repaired February
2026. If you encounter Edit-tool failures on any file with multi-byte characters,
use `sed` or Python to make the change instead.

---

*Last updated: 2026-05-28. If architectural rules change, update this file in the
same commit.*
