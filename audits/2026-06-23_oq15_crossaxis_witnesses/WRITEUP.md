# OQ-15 cross-axis surface — Phase 0a substrate witnesses

**Date:** 2026-06-23 · **Mode:** read-only deciding pass (no files changed; only this audit dir written).
**Plan:** `~/.claude/plans/review-oq-15-from-issues-md-purrfect-leaf.md`.
**Raw evidence:** `raw_witnesses.txt` (reproducible via `capture_witnesses.sh`, run from `prolog/`).

Every finding below cites a line-witness from the raw log or an in-turn grep/read; none rides on a
doc claim. Where a preliminary read was carried into this plan, it is confirmed or **corrected** here.

## W1 — Form of every cross-axis call site (static `module:goal` vs transitive)

| Site | Module | Direction | Form | Witness |
|---|---|---|---|---|
| `cs_drift_mismatch/2` | cs_drift_mismatch.pl | observer→CS (CS reads DR stability) | **TRANSITIVE** | `cs_drift_mismatch.pl:69` calls helper `cs_is_metric_stable/1`; helper reaches `network_dynamics:detect_network_drift/3` (:94) + `network_drift_velocity/4` (:96) |
| `cs_kernel_divergence/4`, `compare_kernel_readings/3` | cs_kernel_registry.pl | observer→CS (reads classify_at_time) | static | direct `drl_composition:classify_at_time` calls at :67–68, :101 |
| `detect_necessity_inheritance/2` | drl_composition.pl | CS→observer (influences→entailment) | static | `drl_core:dr_type(Source, mountain)` :140 + `narrative_ontology:cs_reading_relation(Source, Derived, influences)` :141 |
| `constraint_neighbors/3` | drl_purity_network.pl | CS→DR exclusion filter | static | `cs_kernel_id` exclusion `\+ (...)` at :105 |
| json aggregators | json_report.pl | reads both → JSON only | static | `dr_type` :955/:1177; `cs_axiom_foreclosed` :600; `cs_kernel_divergence` :1682; `cs_drift_mismatch` :2007 |

**Verdict: MIXED — at least one transitive case (`cs_drift_mismatch`).** Therefore grep/import checks
are blind under *either* architecture (v7 relocation or v8 in-place), and **the dataflow taint guard
is load-bearing**, not belt-and-suspenders. v8 §8's "grep is a trap" is vindicated for in-place reads.

## W2 — Is `detect_necessity_inheritance` categorically unlike the bucket-1 comparisons?

**Categorically different — by direction.**
- `detect_necessity_inheritance` is the unique **committer→observer forward dataflow**: a
  committer-authored typed edge (`cs_reading_relation(_, _, influences)`) is consumed to *produce* an
  observer-side derivation (necessity entailment over `suppression_requirement`). Committer field →
  observer computation.
- `cs_drift_mismatch` / `cs_kernel_divergence` / `compare_kernel_readings` run the **other
  direction**: a CS predicate *consumes* an observer verdict (network-drift stability;
  `classify_at_time` type) to form a comparison. No committer field flows into observer computation.

v8's standing invariant (ii) is explicitly directional — "**no committer field reaches observer
computation** by any path except as entailment-typed payload on `influences`" (`v8_..._spec.md:150`,
:284). The bucket-1 comparisons read the unrestricted direction (observer result → CS). So
`detect_necessity_inheritance` is the *one* surface the invariant governs, and **v8's single-bridge
whitelist is a principle, not an arbitrary exception**. This **informs** Phase 2 (favors v8's reading
of the boundary) but does **not** decide the operator's value ruling on "structural."

## BC — Positive control on "zero back-channel violations"

OQ-15 asserts "no module asserts facts the other axis reads at runtime." **How it stands today:**
- **No `cs_` module asserts any runtime fact** (raw log: cs_ assert grep empty).
- The only runtime asserts are observer-internal memoization caches in drl_fpn.pl (`fpn_intrinsic`,
  `fpn_type_cache`, `fpn_neighbors_cache`, `fpn_ep`, `fpn_iteration_info`, :111–287); **no CS
  predicate reads `fpn_*`.**

So the claim is **borne out by inspection** — but inspection is exactly the method v8 calls a trap. A
planted `assertz(narrative_ontology:cs_drift_state(...))` inside a drl_ module, later read by a CS
predicate, would be a genuine back-channel that an assert-grep or a count does **not** distinguish
from a benign cache write. **There is no existing runtime/dataflow probe** that could flag it. The
plant-and-flag positive control therefore **cannot be discharged in 0a** — it is precisely what Phase
1's taint guard must provide, and is the BC acceptance test for that guard (clean corpus → silent;
planted cross-read → flagged).

## XR — Cross-ref witness (gates 0b's repoint)

- **OQ-17** (`ISSUES.md:434`): "`testsets_3000/` quarantined from loader" — **Status: disposed**
  (superseded by 2026-06-05 archive consolidation; content now at
  `prolog/archives/datasets/original_v6/`, :472).
- **OQ-40** (`ISSUES.md:1512`): "G5: scalar-vs-temporal representation splits" — Ω_C, open; body
  names `classify_at_time` (drl_composition) as the temporal-vs-scalar split (:1519, :1526).

OQ-15's line 387 ties "where `classify_at_time` would split" to "(OQ-17)" — a **misattribution**; the
`classify_at_time` split is **OQ-40**. **Repoint confirmed.**

## Reverse-read claim — constraint_bridge.pl (CORRECTS the plan)

`compute_veto_actors/1` reads `drl_core:dr_type(C, Type)` (:94) + `narrative_ontology:constraint_beneficiary(C, Actor)`
(:96). `constraint_beneficiary` is an **authored substrate field** (narrative_ontology.pl:356 "defined
in individual test files") read pervasively by observer-side code (drl_core.pl:300, signature_detection.pl,
constraint_indexing.pl). **constraint_bridge.pl contains NO `cs_` committer read** (raw log). So
`compute_veto_actors` is **observer-axis-internal** (DR verdict + DR substrate input), **not a
cross-axis read.**

**Consequences:** (a) constraint_bridge.pl is *correctly* omitted from OQ-15's `Files:` — do **NOT**
add it in 0b; (b) the Phase 1 whitelist must **NOT** include `compute_veto_actors`; (c) drop
constraint_bridge.pl from the plan's "Critical files / cross-axis surface" list.

## SA — Spec-anchor witness

`v8_seat_gauge_orientation_design_spec.md` §8 (:273–316):
- **Item 1 [LOAD-BEARING — priority 1]** (:281): promote the §3 *transitive* invariant to a
  reachability/taint check over the whole cross-axis surface; (i) committer→observer dependency =
  exactly one forward bridge, (ii) no committer field reaches observer computation except as
  entailment-typed payload on `influences`→`detect_necessity_inheritance`. **Two required positive
  controls** (:286–292): (1) inject grounding field into `influences` payload → fires (count passes);
  (2) wire a read-only seam feeding observer computation by a route never touching `influences` →
  fires (count *and* per-bridge payload pass). Feasibility note (:293): needs a dataflow trace, not a
  static per-edge check; candidate home = stack-consistency-style check or plunit over the surface.
- **Item 4 [LOW-STAKES BULK]** (:304): documentation/vocabulary (seat/gauge/orientation) migration —
  human-gated.
- **Out of scope** (:314): no change to `classify_from_metrics`, signature layer, contamination
  network, or any verdict threshold.

**All Phase-1 attributions in the plan match the spec text.**

## Net effect on the plan

- Phase 1 guard is **load-bearing** (W1) — proceed.
- W2 favors v8's reading but Phase 2 stays the operator's value ruling.
- 0b: **do** the OQ-17→OQ-40 repoint; **do** record v7/v8 as two live architectures; **do NOT** add
  constraint_bridge.pl to Files (reverse-read false).
- Phase 1 whitelist drops `compute_veto_actors`; cross-axis surface = the four genuine sites +
  json_report aggregator.
- BC positive control is deferred into Phase 1's guard acceptance test (no probe exists in 0a).

## Phase 1 — the transitive taint guard (built; commit `fd1ee561`)

`prolog/check_axis_boundary.pl` walks the LOADED call graph (clause/2 over every engine predicate,
descending control constructs, meta-calls, and nested module qualifiers) and emits each
committer→observer boundary edge; `python/check_axis_boundary.py` diffs them against
`prolog/axis_boundary_allowlist.txt` (load_warning_gate pattern). `--selftest` runs the negative case
+ both required positive controls; wired into `scripts/gate.sh`.

**Reachability census — 8 boundary edges (the hand inventory was incomplete):**

| Edge | Role |
|---|---|
| `drl_composition:detect_necessity_inheritance/2 → cs_reading_relation/3` | SANCTIONED-BRIDGE (observer-verdict) |
| `drl_purity_network:constraint_neighbors_existing/2 → cs_kernel_id/2` | BUCKET-3-EXCLUSION (observer-verdict) |
| `axiom_diff:axioms_of/2 → cs_axiom/3, cs_axiom_grounding/3, cs_story_uid/2` | COMPARISON-TOOLING (OQ-59) |
| `axiom_diff:report_ax_header/2 → cs_kernel_id/2` | COMPARISON-TOOLING (OQ-59) |
| `reading_diff:report_header/2 → cs_kernel_id/2` | COMPARISON-TOOLING (OQ-59) |
| `config_validation:config_violation/1 → cs_story_uid/2` | VALIDATION-TOOLING |

Only the first two are observer-VERDICT reads ⇒ **v8's "exactly one forward bridge" (plus the bucket-3
exclusion OQ-15 already flagged) is confirmed in place.** The other six are comparison/validation
tooling in modules (`axiom_diff`, `reading_diff`, `config_validation`) **absent from OQ-15's `Files:`
inventory** — the reachability check found cross-axis reads the hand census missed. (The bucket-1
`cs_drift_mismatch`/`cs_kernel_registry` sites are observer→committer — the *other* direction — and
correctly do not appear among the guarded-direction edges.)

**Positive controls (both fire; v8 §8 item 1):**
- path-b payload widening → `detect_necessity_inheritance → cs_axiom_foreclosed` (bridge count stays 1,
  so a count check passes; guard fires). exit 1.
- path-c non-influences seam → `drl_core:axis_control_seam → cs_kernel_id` (never touches `influences`,
  so a per-bridge payload check passes; guard fires). exit 1.

The controls **caught a real guard defect before it landed**: `body_calls/2` took `functor/3` of a
double-qualified `user:(narrative_ontology:cs_X(..))` body and yielded `(:)/2`, missing the cs_ sink;
fixed by recursing through `M:Goal`. The clean negative pass is trusted only because the controls show
the guard would flag a planted read.

**Net:** the architecture-neutral, load-bearing half of OQ-15 / v8 §8 item 1 is built and gate-wired;
GAP-12 closed. **Phase 2 — relocate (v7) vs policed-in-place (v8) vs synthesis — is the operator's
value ruling, staged behind these witnesses, not decided here.**
