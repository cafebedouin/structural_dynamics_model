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
│   ├── shared/                       # utility package (loader, constants, maxent)
│   ├── reports/                      # report query subpackage
│   ├── tests/                        # standalone test scripts (8 files)
│   ├── sweeps/                       # parameter variation scripts (12 files)
│   └── audits/                       # audit, diagnostic, probe scripts (19 files)
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

**Rule 4 — Priority cascade.** Classification priority (highest to lowest):
`mountain > piton(dead-coordination) > snare > scaffold > rope > tangled_rope > piton(fallback) > naturalized > unknown`

The cascade is implemented as ordered clauses in `classify_from_metrics/6`. The
order of clauses in `drl_core.pl` IS the priority.

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
   (`natural_law_signature`/`constraint_signature(C,natural_law)`) survives as a router input.
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
(`constraint_signature(C,Sig), Sig==<sig>` — a bound-arg `constraint_signature(C,<sig>)` trips on the DETECTOR
even when a higher-priority signature shadows it, §1 gotcha; it wrongly caught an FCR seat in
`constructed_routed`); `converted_at_seat/2` (signature-level for non-split, seat-level for split) feeding
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
The replacements use the canonical sigmoid pipeline. Note: the two replacements are NOT
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

### Headline verdict contract (OQ-98, 2026-06-11)

Per-constraint entries in `pipeline_output.json` / `enriched_pipeline.json` carry BOTH
`diagnostic_verdict` (raw 12-subsystem synthesis) and `verdict_join` (the joined headline:
base verdict + severity-floored alerts + grid/measurement provenance + signature grade;
manifest `schema_version` 2). Any consumer that summarizes a constraint must headline
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
observer-side module, the gate goes RED** — that is by design.
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

---

## 5. Testing Requirements

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
surface instead (giant_comp reports both pooled + cross-kernel — OWED report-build, see OQ-193).
Evidence: `audits/2026-07-02_oq193_giant_comp_ruling/`.

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
    "schema_version": "<version>"
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
