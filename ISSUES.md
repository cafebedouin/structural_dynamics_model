# Open Questions and Issue Tracker

Persistent tracker for unresolved questions surfaced by audits and correctness
work. Each entry records: origin, the specific question whose answer would close
the item, evidence so far, and what would change once resolved.

Statuses: **open** | **investigating** | **mitigated** | **resolved**

---

## OQ-01 — Rope gate Chi ≤ 0 bypass: intentional modeling or artifact?

**Ω-type:** Ω_C (design choice — modeling decision to ratify, guard, or record in logic.md).

**Status:** open
**Priority:** 1
**Origin:** alt_power_transform corrected T2 run, Arm A/B range sweep, May 2026.  
**File:** `prolog/drl_core.pl:356`

```prolog
(Chi =< 0 -> true ; config:param(rope_epsilon_ceiling, EpsCeil), BaseEps =< EpsCeil),
```

**Specific question:** Does the `Chi =< 0 → true` branch encode intentional modeling
content — i.e., when the experienced chi is negative the agent is a net beneficiary
of the constraint and epsilon magnitude no longer distinguishes rope from extraction,
so the ceiling is irrelevant — or is it an implementation artifact whose theoretical
status was never isolated?

**Evidence so far:** The bypass rarely triggers under the sigmoid baseline because
the default transform spends little time at chi ≤ 0. Under the Arm A3 variant
(L=−0.20, U=+0.65, span=0.85, snare gate unreachable), the bypass routes high-ε
constraints to ROPE at low-d contexts while high-d contexts route to TANGLED_ROPE,
producing 1,417 spurious presheaves and collapsing Jaccard from 0.864 (A2) to 0.319
(A3). Arm B3 at matched span ([0.02, 0.87], no sign-flip) gives Jaccard 0.780 —
confirming the A3 collapse is the bypass × sign-flip × compressed-ceiling interaction,
not range alone. The v6.0 comment at drl_core.pl:353–355 asserts the bypass is
intentional ("agent is a net beneficiary — skip base extraction gate"), but the
comment predates the range-sweep evidence that reveals the bypass's behavioral
consequences at compressed ceilings.

**What resolution changes:** The v6 paper currently reads H0 as "sign-flip
load-bearing when Hub 1 spans snare gate AND rope-gate bypass behavior is treated as
given." Resolving the bypass either (a) justifies its current form (the AND clause
becomes a documented modeling choice with a known boundary condition), (b) reveals it
as an artifact requiring removal or guarding (a guard such as "only bypass when chi <
−threshold_magnitude, not at all negative values" would change A3 behavior and the
paper's conditional confirmation may strengthen), or (c) confirms it as defensible but
requiring an explicit design-decision record in logic.md.

---

## OQ-02 — write_entries choice-point accumulation: cut hides symptom

**Status:** mitigated
**Origin:** T2 OOM diagnosis, May 2026.  
**Files:** `prolog/product_site_export.pl:75–77`; `prolog/drl_core.pl` (rope gate
disjunction at lines 357–358)

**Specific question:** Are the predicates called inside `type_at/3` — specifically
`integrate_signature_with_modal/3` and the rope gate's `(effective_immutability_for_context
; emerges_naturally)` disjunction — fully deterministic, or are they leaving residual
choice points that the green cut now suppresses but which may surface again under
different default transforms or future code paths?

**Evidence so far:** The green cut (`!` after `write_one_entry` in `write_entries`
clause 3) was verified classification-preserving: 0 diff across 3,380 constraints
before and after (see `outputs/cut_proof_before.json`, `outputs/cut_proof_after.json`).
Runtime dropped 30% (53.4s → 37.1s), confirming LCO is now firing. The OOM
diagnosis identified the rope gate disjunction as the likely source of residual choice
points under compressed-flip chi values. The cut resolves the OOM without resolving
whether those predicates are genuinely deterministic.

**What resolution changes:** If predicates are fully deterministic, the cut is
documentation-only and can be replaced with an explicit determinism annotation
(e.g., `once/1`) with no behavioral difference. If they are not deterministic, the
cut is masking a latent nondeterminism that could re-surface under a different default
transform or future export variants — a structural issue, not a performance issue.
Resolution requires either a determinism analysis of `integrate_signature_with_modal/3`
and the rope gate disjunction, or an explicit `once/1` wrapping with a proof that the
first solution is always correct.

---

## OQ-03 — Self-application: where does DR sit in its own ontology?

**Ω-type:** Ω_P (constitutively-open design choice — the engine witnesses, a declared seat settles).

**Status:** open
**Priority:** 1
**Origin:** `docs/deferential_realism_paper_v6.13.md` §6 open questions; reinforced by
`docs/seat-theorem-v1.md` §7–§8.  
**File:** `prolog/tests/test_forecloses_fpn_injection.pl` (closest precedent)

**Specific question:** When the engine is run against DR-the-framework itself as a
constraint, does it classify as Mountain, Rope, Snare, or Tangled Rope? Paper §6
states the honest expectation: "probably Tangled Rope, because most real
classificatory frameworks are."

**Evidence so far:** None — the engine has not been run on this constraint. The
§3 Correction to the Seat Theorem (test_forecloses_fpn_injection.pl) is the closest
precedent: a case where engine output reshaped a foundational document via the
FPN-injection mechanism. Self-application is the same operation applied one level up:
DR-as-constraint-story submitted to the engine. The Seat Theorem §7–§8 observes that
the engine's framing Π is a seat the framework cannot self-certify; self-application
would produce empirical evidence about where that seat sits.

**What resolution changes:** Either confirms paper §6's stated expectation
(probably Tangled Rope — coordination function + extraction from users who must adopt
the framework's categories), or surfaces a different classification requiring
theoretical accounting. Either result is publishable. Currently §6 stands as a stated
open question without engine output. Requires authoring a DR constraint story for the
framework itself and running the pipeline.

---

## OQ-04 — Cyclopean-point kernel: 1:N reading structure not encodable in current schema

**Ω-type:** Ω_C (design choice — schema-expressiveness cut: add a 1:N predicate or accept 1:1).

**Status:** open
**Priority:** 1
**Origin:** `docs/unknown_reading_review.md` audit; `docs/altar-to-the-unknown-reading.md`;
`agent/analysis/essays/cyclopean_point_epistemic_synthesis.md` [path corrected — item
originally cited docs/; file is in agent/analysis/essays/].  
**Files:** `prolog/testsets/disparity_as_depth_signal.pl`,
`prolog/testsets/cyclopean_point_as_manufactured_center.pl`,
`prolog/testsets/power_asymmetry_in_legibility.pl`;
`prolog/testsets/autonomy_reading.pl` (template)

**Specific question:** How should the three cyclopean-point constraints be encoded as
CS facts? They are ONE reading (the analytical-observer reading per the altar essay)
of one kernel, expressed through three constraints — not three sibling readings as the
autonomy_reading template assumes. The schema currently encodes 1:1
reading-to-constraint. The cyclopean-point case is 1:N (one reading, N=3 constraints).

**Evidence so far:** `docs/altar-to-the-unknown-reading.md` §"The diptych" explicitly
states "the engine evaluated one kernel under one reading" and names the
analytical-observer reading. The autonomy_reading template has no predicate for "this
constraint is part of reading X along with these other constraints." The UUID surrogate
identity work (Phase A+B, May 2026) re-keyed readings to UUID surrogates but did not
add a 1:N predicate.

**Related schema gaps:** `cs_reading_enumeration_status(kernel, closed|open)` — the
cyclopean-point kernel positions its readings as open by the kernel's own content; the
autonomy template assumes closed enumeration.

**What resolution changes:** Enables authoring CS facts for the cyclopean-point
constraints without flattening them into the autonomy template's structure. Required
before re-running c-orchestrator.py on those testsets in a way that produces honest
kernel-reading sections. The 1:N predicate (`cs_reading_covers/2` or similar) would
also apply to any future kernel where a single reading produces multiple constraint
stories.

---

## OQ-05 — §2.3 paper correction: empirical work complete, v6 not yet authored

**Status:** resolved
**Origin:** alt_power_transform full-corpus run + Arm A/B sweep, May 2026.
**Resolution:** v6 authored 2026-05-28 (`docs/observers_not_humans_v6.md`). Sign-flip is
load-bearing only in tangled_rope (T4 gap +0.21 vs +0.014 in snare+rope, 14.6×), not corpus-wide;
§2.3 and §3.3 unified as one mechanism (institutional sign-flip at d < d_zero); rope-gate bypass
(OQ-01) flagged as the conditional assumption. Witnesses:
`outputs/alt_power_transform_results.json`, `outputs/range_sweep_results.json`.
*(Body compressed 2026-06-04 per footer rule; full hypothesis ledger in git history.)*

---

## OQ-06 — cs_drift_unacknowledged / cs_axiom_foreclosed: off-case fixtures missing

**Status:** open
**Priority:** 1
**Origin:** Tranche 2 correctness pass, Phase 1 audit, May 2026.  
**Files:** `prolog/cs_drift_engine.pl` (predicates); `prolog/cs_corpus_analysis.pl:158–162`
(callers); `prolog/json_report.pl:531–533` (report emission)

**Specific question:** Four conjuncts of `cs_drift_unacknowledged/2` and
`cs_axiom_foreclosed/2` have no live disconfirming UID in the corpus — they have only
been tested in the on-case direction. Specifically:

- `cs_drift_unacknowledged` C3 (direction=stable): no live off-case UID
- `cs_drift_unacknowledged` C4 (magnitude=minor): no live off-case UID
- `cs_axiom_foreclosed` C2 (grounding ≠ empirically_contingent): no live off-case UID
- `cs_axiom_foreclosed` C4 (magnitude=minor + empirically_contingent): no live off-case UID

**Evidence so far:** Phase 1 audit identified the four missing off-cases. The
predicates succeed on known on-cases and the engine produces correct on-case output.
Whether the conjuncts correctly stay silent when the off-case condition holds has not
been tested with live corpus data.

**What resolution changes:** A predicate that has never been observed to stay silent
when it should has only half its specification tested. Authoring off-case fixtures
would close this. Caveat (raised in audit): purely synthetic fixtures are
kernel-without-reading and should be marked as such if added to the testset corpus.
An alternative is to author a real constraint story that genuinely has
direction=stable or magnitude=minor and observe that the predicate stays silent.

---

## OQ-07 — Hand-traced mismatch candidate unverified at runtime

**Status:** resolved — (2026-06-09, runtime probe). Verdict: **does NOT fire** — the
hand-trace's foreclosure half holds at runtime (`cs_drift_trajectory → axiom_foreclosure`)
but `cs_is_metric_stable` FAILS (DR detects network drift), so `cs_drift_mismatch/2`
correctly stays silent for this UID. Positive control: detector fires on 11 UIDs on the
same load, so the predicate IS exercised end-to-end on real corpus data; this candidate
is a witnessed off-case where exactly one named conjunct blocks. Corpus note: the UID
exists only in `archives/datasets/kernel_test/` (per-generation surrogate; the live
post-reset corpus lacks the testset; other archive copies carry different UIDs).
Evidence: `audits/2026-06-09_oq07_mismatch_runtime_probe/` (probe.pl, raw output, writeup).
**Origin:** Tranche 2 correctness pass, Phase 1 audit, May 2026.
**UID:** `72c8aa61-6909-40a1-83ef-a460510f3b82` *(body compressed at close per footer rule)*

---

## OQ-08 — DR/CS context asymmetry not surfaced in mismatch reports

**Status:** resolved — (2026-06-09, both report layers annotated). When `cs_drift_mismatch`
fires, `json_report.pl` now emits a `cs_drift_mismatch_note` field and
`enhanced_report.py`'s kernel-reading section appends a note line: Π-asymmetric by design —
DR classifies instance-blind at the fixed analytical context; CS reads context-free authored
facts; cross-frame disagreement, not two answers to one question. Witnessed both directions:
Prolog side on the kernel_test archive (note present + fragment parses as JSON on a firing
UID; absent on the OQ-07 silent UID); Python side via mock-pipeline witness (note present iff
a reading carries `cs_drift_mismatch`). The OQ-15 mediator layer is the note's eventual
permanent home (cross-ref'd in both code comments). *(body compressed at close per footer rule)*
**Origin:** Tranche 2 audit Item 2.
**File:** `prolog/json_report.pl` (~1797), `python/enhanced_report.py` (~2192),
`prolog/cs_drift_mismatch.pl` (~line 52, the "by design" comment)

---

## OQ-09 — sqrt_flip and quadratic_flip Jaccard slightly above paper §2.3 ceiling

**Status:** resolved
**Origin:** alt_power_transform full-corpus run (testsets_3000, 3380 constraints), May 2026.
**Resolution:** v6 corrected the claimed Jaccard range to 0.697–0.833 (2026-05-28); sqrt_flip
(0.833) and quadratic_flip (0.830) sit at the upper end of the corrected range, not above it.
The shift was corpus-snapshot drift (original range computed on an earlier snapshot).
*(Body compressed 2026-06-04 per footer rule; details in git history.)*

---

## OQ-10 — Cross-reading comparison: no tooling to compare engine output across readings of the same kernel

**Status:** open
**Priority:** 1
**Deps:** blocked_on OQ-04
**Origin:** User-identified capability gap, May 2026; architectural context in
`docs/unknown_reading_review.md` §4 and §5.  
**Files:** `prolog/testsets/autonomy_reading.pl` (template with cs_reading_relation);
`prolog/cs_kernel_registry.pl`; `python/enhanced_report.py`

**Specific question:** Given two or more authored readings of the same kernel (linked via
`cs_reading_relation/3`), can the engine run both and report which findings are
reading-robust (same classification, H¹, CS verdicts across readings) vs reading-specific
(only appears in one reading)? Currently there is no predicate, script, or report
section that performs this comparison.

**Evidence so far:** The infrastructure required for comparison exists:
- `cs_kernel_id/2` identifies which kernel a reading belongs to
- `cs_reading_relation/3` links readings of the same kernel (`coexists_with`, etc.)
- `dr_type/3` can be called on any constraint at any context
- The product-site export already runs 156 contexts per constraint

What does not exist: a tool that (1) discovers all readings of a kernel via
`cs_reading_relation`, (2) runs the engine on each, (3) computes agreement/disagreement
per context and per finding type, (4) reports which findings are reading-invariant.
The review document (`docs/unknown_reading_review.md` §4) notes this explicitly:
"the engine provides the infrastructure for this comparison but cannot perform it in
a single run." It is not an architectural impossibility — it requires authoring
multiple readings and a comparison tool.

**Precondition:** Gap A (OQ-04) must be closed first — the
cyclopean-point testsets need `cs_kernel_id` and `cs_reading_relation` populated before
the comparison tool has a real multi-reading kernel to operate on.

**What resolution changes:** Reading robustness becomes a first-class output. A finding
currently reported as "the engine classifies X as snare (analytical)" becomes "X is
classified as snare (analytical) in reading R1; in reading R2, the same context gives
rope — this finding is reading-specific." The altar essay's Ω_E (whether the
cyclopean-point verdicts are reading-robust) becomes answerable. Reports gain a
"reading robustness" section alongside the existing theorem instantiation and orbit
analysis. This is the engine practicing what the kernel-reading architecture requires:
showing what it cannot say about readings it was not asked to run.

**Implementation sketch:**
- Prolog: `compare_kernel_readings/3` — given a kernel_id and context list, finds all
  readings via cs_reading_relation, runs dr_type/3 on each, returns agreement map
- Python: a script or enhanced_report.py extension that calls compare_kernel_readings,
  computes Jaccard similarity between reading pairs' presheaf sets, and emits a
  reading-comparison section
- Smallest useful version: a standalone Python script that takes a kernel_id, finds its
  readings from the loaded corpus, runs the product-site export on each, and diffs the
  H¹ and orbit_signature outputs

---

---

## OQ-11 — Two truly dead config params: `logic_engine` and `version`

**Status:** resolved — (2026-06-04, ledger close — already done in substrate). Both params are
commented out at `config.pl:291-292`; grep finds zero live references in `prolog/ python/ agent/`
(archives excluded) and zero `config_schema.pl` entries.
**Origin:** AUDIT.md finding W3, 2026-02-28. Audit evidence:
`audits/2026-02-28_codebase_audit_data/config_params_unused.txt`.
*(Body compressed 2026-06-04 per footer rule.)*

---

## OQ-12 — `.env` not in `.gitignore`

**Status:** resolved — (2026-06-04, ledger close — already done in substrate). `.gitignore:27`
contains `.env`; `git ls-files` confirms no `.env` is tracked. Closed without a change.
**Origin:** AUDIT.md finding §6 security review, 2026-02-28.
*(Body compressed 2026-06-04 per footer rule.)*

---

## OQ-13 — Four pylint E-level errors in Python code

**Status:** resolved — (2026-06-04 — the four audited sites no longer exist). Witness:
`all_metrics_by_id` appears nowhere in `classification_confidence.py` (item 1 refactored away);
the audited line targets no longer match (3+ months of drift). A fresh `pylint -E` (pylint 4.0.5,
Python 3.10.12) on the three named files reports a DIFFERENT, environmental error set: E0401
import-errors (repo-root cwd path resolution — the modules import fine on their runtime paths),
E1131 unsupported-binary-operation on PEP-604 unions (`dict | None`, valid at runtime on 3.10 —
pylint config false positive; note `.pylintrc:1` itself carries an unrecognized-option E0015).
None are engine bugs; the OQ's specific question (genuine bugs vs dead code in the four audited
sites) is moot. Pylint-config hygiene, if ever wanted, is new work, not this question.
**Origin:** AUDIT.md §5 pylint summary, 2026-02-28.
*(Body compressed 2026-06-04 per footer rule; the four audited sites in git history.)*

---

## OQ-14 — Two-axis architecture doc stale on `influences` bridge

**Status:** resolved — (2026-06-09). `docs/design/two_axis_architecture_v7.md` (the file
moved to `docs/design/`; the OQ's original path was stale too) now carries a dated
**Amendment (2026-06-09)** section recording the decision that previously existed in
conversation only: the `influences` bridge is unblessed (one of OQ-15's 16 inventoried
cross-axis surfaces, not the sole join); the decided sole join is the comparison/mediator
layer (decided, NOT built — OQ-15 stays open); three grep-enforceable invariants and the
three-bucket triage recorded. All four single-bridge claim sites (Purpose, "one
intentional bridge", recurring-principle item 3, Summary) edited to historical/superseded
phrasing; the mediator added to "Open by deferral" with OQ-08/OQ-17 cross-refs.
*(body compressed at close per footer rule)*
**Origin:** Cross-axis comparison layer design pass, May 2026.

---

## OQ-15 — Cross-axis comparison/mediator layer: designed but not built

**Ω-type:** Ω_C (design choice — architecture decided, build deferred).

**Status:** open
**Priority:** 1
**Origin:** Tranche 2 cross-axis surface inventory, May 2026.  
**Files:** `prolog/cs_drift_mismatch.pl`, `prolog/cs_kernel_registry.pl`,
`prolog/cs_pattern_detection.pl`, `prolog/cs_axiom_engine.pl`,
`prolog/drl_composition.pl`, `prolog/drl_purity_network.pl`,
`prolog/json_report.pl`, `python/enhanced_report.py`

**Specific question:** The inventory of cross-axis predicates surfaced 16
distinct surfaces threaded through 7 modules in both directions. The agreed
architecture is a third layer — neither CS nor DR — that is the sole reader of
both axes, with both axes becoming read-only sources, the layer writing only to
JSON output, and three grep-enforceable invariants (no axis reads the other, no
axis reads the mediator, only the mediator may read both). The triage into
three buckets (genuine comparisons → mediator; substrate-level story-field
readers → a fourth substrate layer; sanctioned-bridge/exclusion-filter cases →
decision per item) and the design itself were paused when the temporal
excavation took priority. When does the mediator design pass resume, and what
becomes of the 16 inventoried surfaces?

**Evidence so far:** Inventory complete. Three buckets identified:
1. Genuine comparisons that read both axes (`cs_drift_mismatch`, the
   `json_report.pl` cross-axis aggregators, and — per OQ-14 — the unblessed
   `detect_necessity_inheritance`).
2. Substrate-level pattern detectors in `cs_pattern_detection.pl` that read only
   constraint-story fields (not DR-derived; they belong to a substrate layer
   that names the shared input both axes interpret). Six predicates.
3. `constraint_neighbors/3` exclusion filter using `cs_kernel_id` to keep CS
   out of the DR network — defensible as-is, named for decision.

Zero back-channel violations: no module asserts facts the other axis reads at
runtime. The architecture is decoupled in practice; what's missing is the named
layer that makes the boundary structural rather than nominal.

**What resolution changes:** Until built, every CS module reaching into DR (and
the two DR→CS reads) is a nominal boundary violation that happens to be
behaviorally clean. A named mediator layer with prefix-enforceable invariants
converts "currently clean" to "mechanically guaranteed clean." Also closes
OQ-08 (the DR/CS Π-difference annotation lives naturally in the mediator's
output, not bolted onto `cs_drift_mismatch`). The unbuilt comparison layer is
also where `classify_at_time` would split (OQ-17) — the keystone-within-the-
keystone is the mediator design itself.

---

## OQ-16 — Temporal vocabulary rename pass deferred

**Status:** open
**Priority:** 1
**Origin:** Temporal excavation audit, May 2026.  
**Files:** `prolog/drift_events.pl`, `prolog/drift_report.pl`,
`prolog/trajectory_mining.pl`, `prolog/trajectory_report.pl`,
`prolog/network_dynamics.pl` (`detect_network_drift/3`)

**Specific question:** The temporal excavation found that "drift" and
"trajectory" name two structurally different concepts on opposite axes
(committer-drift in `cs_drift_engine` vs. network-contamination /
metric-rate-of-change in `drift_events`/`network_dynamics`; CS commitment-
trajectory vs. observer-context-profile in `trajectory_mining`). The word
collision is the source of much of the cross-axis confusion that drove the
mediator-layer work. Renames were proposed but explicitly not executed,
sequenced as a separate pass. When does the rename pass run?

**Evidence so far:** Proposed renames recorded:
- `drift_events.pl` → `metric_drift_events.pl` (or `dr_drift_events.pl`)
- `drift_report.pl` → `metric_drift_report.pl`
- `trajectory_mining.pl` → `context_profile_mining.pl`
- `trajectory_report.pl` → `context_profile_report.pl`
- `detect_network_drift/3` → `detect_network_contamination/3`

All five are same-word-different-concept findings (network/metric drift ≠ CS
commitment drift; observer-context "trajectory" ≠ CS commitment trajectory).
Each is a name-only change — no logic moves, no algorithm changes. The pass
touches import lines across 5+ files plus `stack.pl` load order plus test
files.

**What resolution changes:** Eliminates a recurring source of confusion that
showed up at every layer of the temporal excavation (and arguably contributed
to `cs_drift_mismatch`'s original miswiring, since the word collision made the
cross-axis boundary harder to see). Cosmetic relative to behavior, but
disambiguating naming is the same hygiene that the UUID surrogate work applied
to identity collisions — the analog at the vocabulary layer. Low risk, ~30
minutes per module if done deliberately (one rename + load-check at a time, per
the `stack.pl` discipline established during Phase A of the UUID work).

---

## OQ-17 — `testsets_3000/` quarantined from loader: abandoned or unwired?

**Status:** disposed — superseded by the 2026-06-05 archive consolidation
**Origin:** Temporal excavation, May 2026.  
**Files:** `prolog/corpus_loader.pl` (flat `testsets/*.pl` glob);
`prolog/cs_drift_mismatch.pl:113` and `prolog/cs_corpus_analysis.pl:194`
(hardcoded `expand_file_name('testsets/*.pl', Files)`)

**Specific question:** `testsets_3000/` contains 3,380 constraint-story files,
97% with rich multi-timepoint `measurement/5` data (3–7 distinct timepoints per
constraint), and the corpus has zero `cs_drift_state/3` (it's pure DR-
measurement). The current loader globs flat `testsets/*.pl` only and never
ingests the subdirectory. Two `cs_*` modules hardcode the same flat path. Is
`testsets_3000/` an abandoned generation run that should be archived or
deleted, or is it the corpus you intend to eventually load and have not yet
wired in?

**Evidence so far:** Audit run on the directory confirmed: 3,287 files meet
the >2-timepoint trajectory criterion (the headline data the dormant temporal
machinery would need), max 7 timepoints, zero `cs_drift_state`. The contrast
with `testsets/` is sharp: `testsets/` has the smaller measurement corpus *and*
all 103 `cs_drift_state` facts (the committer-axis layer). The two corpora
appear to have been built around different temporal regimes — `testsets_3000`
as a pure DR-measurement corpus, `testsets/` as where the CS layer was added
on top. The hardcoded flat-path globs in two CS modules suggest the quarantine
is by intent, but it's never been recorded as such.

**What resolution changes:** Either (a) `testsets_3000/` becomes an archive/
delete decision (the same way `testsets_archive_20260525/` already is) and
disappears from the working tree, removing 3,380 files of latent
"is-this-loaded" confusion, or (b) it gets wired into the loader and the
pipeline runs against a 10× larger corpus — which is a corpus-scope decision
with substantial downstream consequences (every report figure, every
distribution, every "X% of corpus" claim recomputes). One of those decisions
should be on the record; "sitting there ignored" is the unmarked state.

---

**Disposition (2026-06-05):** `testsets_3000/` moved to `prolog/archives/datasets/original_v6/` in the corpus-reset reorganization (commit `29889e50`); the loader-quarantine question is moot — all archived corpora load only via explicit `corpus_path` overlay.

## OQ-18 — `metric_delta/5` first/last reduction: safe-as-event-gate, latent if reused

**Status:** open
**Priority:** 1
**Origin:** Temporal wiring spike, faithfulness audit, May 2026.  
**File:** `prolog/drift_events.pl:72-79`

**Specific question:** `metric_delta/5` reads the full `measurement/5` time
series, sorts it, and returns *only the first and last* T-V pair's delta —
discarding all intermediate timepoints. This is currently safe because
`metric_delta`'s output is only used as a boolean threshold gate (delta >
threshold → fire event). It is silently wrong if ever reused as a trajectory
source: a series that spikes and recovers (V(0)=0.35, V(peak)=0.68,
V(end)=0.58) produces the same delta as a monotone climb to the same endpoint.
Should the predicate be renamed, deprecated, or annotated to prevent this
reuse?

**Evidence so far:** The faithfulness audit during the temporal spike found
this and three other temporal predicates (`metric_trend/3`, `drift_velocity/3`,
`drift_acceleration/3`) all use first/last or first-3-points reductions. They
are all event-gates, all safely-reductive *for their current use*, and all
hazardous as trajectory sources. The temporal wiring deliberately sourced from
raw `measurement/5` rather than any of these collapsing predicates. The
hazard is structural: a future developer (or a future Claude) looking for "the
predicate that gives me drift velocity" will find `drift_velocity/3` and use
it, getting silently wrong answers on any non-monotone series. The temporal
wiring code excluded these predicates explicitly with a do-not-use comment,
but the predicates themselves carry no such marking.

**What resolution changes:** Either rename to make the collapse explicit
(e.g. `drift_velocity_endpoint/3` instead of `drift_velocity/3`), add a
docstring/comment at each predicate marking the reduction as event-gate-only
and pointing to the faithful source, or deprecate in favor of series-faithful
replacements. Lowest cost: comment annotation. Highest robustness: rename so
the limitation is in the call site every time the predicate is invoked. The
current state (no marking, current use safe, latent hazard on reuse) is the
exact "currently-true-by-accident" pattern ISSUES.md exists to prevent
becoming an unmarked assumption.

---

## OQ-19 — Temporal-shape trigger thresholds are corpus-specific magic numbers

**Status:** open
**Priority:** 1
**Origin:** Temporal-shape report section build, May 2026.  
**File:** `python/enhanced_report.py` (build_drift_trajectory section,
trigger logic)

**Specific question:** The `drift_trajectory` report section in
`enhanced_report.py` fires on three triggers with corpus-specific magic
numbers: non-monotone reversal ≥ 0.04, cross-metric divergence ≥ 0.06 both
metrics, plateau requires sustained negative acceleration + last-rate < 20%
of first-rate + total rise ≥ 0.05. Each threshold is justified against the
current corpus's 2-decimal-place measurement granularity (0.04 = 4× the
minimum representable movement, etc.). If the corpus is ever regenerated at
3-decimal precision, do these thresholds silently become too coarse?

**Evidence so far:** Thresholds were calibrated and validated: floor sweep
showed trigger B (divergence) stable across 0.04–0.10 (16→14 constraints),
confirming it sits in a gap not a cluster; trigger A's 0.04 is 4× the 0.01
granularity floor; trigger C's parameters were tuned to fire only on genuine
ceiling approaches (24 metrics across 20 constraints). Result: 48 distinct
constraints triggered, 142 silent, all four spot-checks correct. The
calibration is sound for *this* corpus. The thresholds are embedded in the
trigger logic with comments justifying each value against the granularity
argument.

**What resolution changes:** Either the thresholds are recorded somewhere
durable (a config param, a comment block tying each value to the assumption
that makes it valid, a regeneration-time check that asserts measurement
granularity ≤ assumed) or they become silent assumptions the moment the
measurement precision changes. The recurring pattern from this engagement —
"currently-justified constant becomes unmarked assumption when the thing that
justified it changes" — applies. Cheap insurance: each threshold carries its
own rationale in code, with a one-line "if measurement granularity changes,
recalibrate" marker.

---

## OQ-20 — DR-regression baseline diff against `v3-dev-baseline` tag never run

**Status:** open
**Priority:** 1
**Origin:** Tranche 2 correctness pass, May 2026.  
**Files:** git tag `v3-dev-baseline` (pre-CS-work); current DR pipeline
output

**Specific question:** All CS-era work (the original CS wiring, the kernel_id
keystone, the UUID surrogate migration, the temporal wiring) landed on top of
mature DR code. The audit confirmed a pre-CS DR baseline exists as the git tag
`v3-dev-baseline`. A byte-diff of current DR output against the baseline would
establish whether any of the CS-era work perturbed DR. This has never been
run. Did the CS-era work leave DR output byte-identical to the pre-CS
baseline, or are there perturbations?

**Evidence so far:** The audit confirmed the tag's existence. Test suite
passes after each phase (zero errors, zero warnings as of last run), which
establishes that DR didn't *break*. It does not establish that DR output is
unchanged — multifile clause additions, new `stack.pl` imports, and shared
predicate-resolution-order changes could shift DR output without breaking it.
The before/after for the original CS imports is no longer recoverable
(everything is now committed on top of those changes), so this is not a clean
"before vs after a specific change" diff — it's "compare current DR to the
pre-CS-era tag."

**What resolution changes:** Either confirms DR was untouched by all CS work
(strong evidence the axis separation held in practice, not just in
architecture), or surfaces specific DR outputs that shifted (each shift becomes
its own investigation: intentional consequence of a shared-predicate change,
or accidental perturbation needing tracing). Either result is informative.
Required mechanism: check out `v3-dev-baseline`, run the DR pipeline, capture
output, return to HEAD, run again, byte-diff.

---

## OQ-21 — A12 multi-instance render branch never exercised on pipeline data

**Status:** open
**Priority:** 1
**Origin:** UUID surrogate migration, May 2026.  
**File:** `prolog/json_report.pl` (A12 per-constraint CS block,
`cs_instance_count` > 1 branch)

**Specific question:** A12's multi-instance render branch (the path that
fires when a constraint name `C` has multiple `cs_story_uid` UIDs, picks the
latest by `cs_created_at` with `@<` fallback when timestamps are absent) was
verified by manual dual-consult in the post-migration isolation check (check
6) but has never executed in the actual pipeline. The pipeline's
`corpus_loader` only globs flat `testsets/*.pl`, and the flat corpus is
single-instance-per-name by construction (the colliding `kernel_run_02/`
instances are in a subdirectory the loader doesn't read). When does the
multi-instance branch get exercised on real pipeline data?

**Evidence so far:** The branch is correct code for a condition the current
load discipline doesn't produce. This is the reserved-branch fate, not
vestigial — the architecture explicitly supports multi-instance identity and
the code is built for it, but the loader is single-run by design. The branch's
correctness depends on a load-discipline assumption (single-run-per-flat-
corpus) holding. If that assumption changes (e.g. multi-run merging becomes
a feature, or `testsets_3000/` wiring happens per OQ-17 with multi-run data),
the branch suddenly starts firing and its pipeline-time correctness becomes
empirically testable for the first time.

**What resolution changes:** The branch moves from "verified by manual dual-
consult" to "exercised end-to-end by the pipeline." Until then it's the
correct-but-untested-on-real-data case. The natural unblock: any future task
that deliberately constructs a multi-instance load (a deliberate two-run
co-load, or OQ-17 resolving toward "wire `testsets_3000`") would exercise the
branch as a side effect. The branch is also the natural exercise vehicle for
OQ-10's cross-reading comparison work — building that comparison tool would
load multi-reading data and incidentally validate the branch.

---

## OQ-22 — Hub 1 / Hub 2 fall-through behavior at threshold starvation

**Status:** open
**Priority:** 1
**Deps:** bundled_with OQ-01
**Origin:** alt_power_transform first T2 run (compressed variants), May 2026. Diagnosed
during the H1 investigation but not pursued as a separate item once the corrected Arm A/B
sweep resolved H1.

**Specific question:** When chi values compress below the classification threshold spacing,
Hub 1 (chi-driven gates) effectively disengages and classification falls through to Hub 2
(effective immutability). This is a regime change in the engine's classification mechanism.
Under what conditions, if any, does this fall-through occur in normal operation across the
existing corpus — for which constraints, in which contexts, under the default transform?
Is there a documented boundary at which Hub 1 stops contributing meaningfully?

**Evidence so far:** The starvation regime was entered accidentally in the first T2 run
(compressed-flip variant, chi span 0.20, ceiling 0.15). Under that configuration, presheaf
classifications were being decided by Hub 2 alone because Hub 1 had no dynamic range to
discriminate across thresholds. The corrected Arm A/B sweep explicitly avoided this region
by keeping all variants chi-spanning the gates (verified against rope floor, snare gate,
tangled_rope gate, mountain ceiling from `prolog/config.pl`). The two-hub architecture
comment in `drl_core.pl:160–210` describes the hubs as independent contributors driving
classification variation across observers; the starvation finding indicates they can also
substitute under degenerate transforms, which the architecture comment does not address.

**What resolution changes:** If no real corpus configuration ever enters the starvation
region under the default sigmoid, this is a non-issue for current operation but a
constraint on what alternative transforms can safely be tested. If any subset of
constraints or contexts does enter starvation under the default transform (e.g.,
constraints with very low ε producing chi values that crowd the rope/tangled_rope
boundary), then classifications in that subset are Hub 2 decisions reported as if they
were two-hub decisions, and the framework should distinguish them. Resolution likely also
affects how OQ-01 is read: the rope-gate bypass surfaces at compressed ceilings, but the
broader question of when Hub 1 is meaningfully contributing is upstream of that specific
clause.

**Related:** OQ-01 (rope-gate bypass) — the A3 collapse at compressed ceilings was the
bypass × sign-flip × compressed-ceiling interaction. The starvation regime is the adjacent
phenomenon: not the bypass firing, but Hub 1 failing to discriminate at all. They share
the property of being behaviors contingent on the default transform's range, surfaced
only when that range was deliberately altered.

---

## OQ-23 — coexists_with exclusion is unenforced design intent, not structural

**Ω-type:** Ω_C (design choice — loud documentation vs mechanical guard).

**Status:** open
**Priority:** 1
**Origin:** FPN convergence-test run, Branch E verdict, May 2026.  
**Files:** `prolog/drl_purity_network.pl` (constraint_neighbors/3, compute_edge_contamination/7); `prolog/test_forecloses_fpn_injection.pl` (Case 2 — coexists_with_label_blindness)

**Specific question:** The architecture note's claim that coexists_with's contamination weight is "zero by definition" is — per the run — an unimplemented design intent, not a mathematical property. The FPN is label-blind; injecting coexists_with as an affects_constraint fact produces non-zero contamination identical to any other edge with the same purity delta. Unlike forecloses (which is structurally inert in its semantically correct direction, gradient-orthogonal to the network — genuinely unrepresentable regardless of code), coexists_with is structurally admissible (just a scalar) and excluded only by the fact that nothing currently routes it into constraint_neighbors/3. Should the exclusion remain documented-only, or should an edge-type guard be added so the decoupling is enforced rather than incidental?

**Evidence so far:** The FPN injection test (Case 2) ran an identical injection to forecloses_1b and got identical scalar flow. The architecture note has been rewritten to claim only what each edge earned: forecloses excluded structurally (gradient-orthogonality demonstrated), coexists_with excluded by unenforced design intent with constraint_neighbors/3 named as the actual gap. No live code path currently routes a coexists_with edge into the network, so the gap is latent — but the next code change that does will inject as a label-blind scalar and nothing will catch it. This is the same shape as the chimera latency (OQ-related — pre-cleanup, the chimera was inert until the next cs_kernel_id addition would have made it live): documented-and-inert versus enforced-by-code.

**What resolution changes:** Either (a) the architecture note's open-item clause is made loud enough that any future edit routing coexists_with into constraint_neighbors/3 cannot proceed without first reading the warning (self-flagging documented gap), or (b) an edge-type guard is added in compute_edge_contamination/7 that returns zero contamination when the underlying cs_reading_relation is coexists_with, converting "currently unenforced" to "mechanically enforced." Option (a) is consistent with the build's mark-drift-rather-than-armor discipline; option (b) closes the latent leak. The decision is "loud documentation versus actual guard," and either should be on the record rather than left as ambient.

---

## OQ-24 — forecloses requires no FPN representation, but the absence is undocumented in the engine

**Ω-type:** Ω_C (design choice — whether to record the structural-exclusion rationale in code).

**Status:** resolved — (2026-06-04). The comment now exists at
`drl_purity_network.pl:compute_edge_contamination/7` — structural exclusion stated
(gradient-orthogonality, inert-or-inverting), witness pointer to
`prolog/tests/test_forecloses_fpn_injection.pl`, and an explicit "absent BY THIS DECISION, not by
omission" tripwire. Note: a pointer at `drl_purity_network.pl:63` citing that comment had been
written EARLIER without the comment it cited — a dangling doc-pointer (produced-but-not-consumed
in documentation form); the comment now makes the pointer true. Module load verified post-edit.
**Origin:** FPN convergence-test run, May 2026.
*(Body compressed 2026-06-04 per footer rule.)*

---

## OQ-25 — testset directory chimera: resolved-inert but resolution mechanism is latent

**Status:** resolved — (2026-05-28)
**Origin:** Corpus cleanup, May 2026 (ε-variability investigation).
**Resolution:** both options implemented. (a) Documentation: `docs/cs_load_discipline.md`
(invariant, grouping-key decision, regeneration protocol). (b) Enforcement:
`prolog/config_validation.pl` `config_violation/1` wired into `validate_config_postcorpus/0`
(end of `load_all_testsets`) — halts (exit 1) when any ConstraintAtom carries two distinct
`constraint_metric(C, extractiveness, E)` values. Grouping key = **ConstraintAtom, NOT
KernelAtom** (OQ-26: ε is reading-relative; KernelAtom would false-positive on legitimate
multi-reading kernels). Verified: clean load passes; injected conflict rejected with
`CS ERROR (OQ-25)`; §5.11 divergence count unchanged (79 pairs / 34 kernels). See also
`docs/technical/config_validation_wiring.md`.
*(Body compressed 2026-06-04 per footer rule; chimera history in git.)*

---

## OQ-26 — ε is generated, not observer-invariant in the sense Axiom 2 assumes

**Ω-type:** Ω_C (design choice — what ε-invariance the framework claims; RESOLVED via Axiom 2 amendment).

**Status:** resolved
**Origin:** ε-variability investigation, May 2026; testset-chimera cleanup surfaced the mechanism.
**Resolution:** option (a) implemented 2026-05-28 — Axiom 2 amended
(`docs/deferential_realism_paper_v6.13.1.md` lines 66–91): ε-invariance holds **across observer
positions** but **not across generation runs**; all ε-dependent statistics (H¹, classification
proportions, divergence counts) are scoped to "one coherent generation." Key evidence: same topic
yields genuinely different readings with different ε under different SCOPE decompositions (birth
0.58 vs 0.12; mourning 0.18 vs 0.58) — ε is a property of a reading, not a topic. **Option (b)
(constraining generation for run-reproducible ε) deferred** as separate architectural work.
*(Body compressed 2026-06-04 per footer rule.)*

---

## OQ-27 — H¹ definition: signature-resolved vs raw-type orbit is not stated in the engine

**Ω-type:** Ω_C (design choice — definitional: which orbit H¹ measures).

**Status:** open
**Priority:** 1
**Origin:** Theorem 7 anchor verification, May 2026 (the precision note that changed v7's H¹=0 phrasing).  
**Files:** `prolog/cohomological_obstruction.pl` (or wherever orbit_vector is constructed); `prolog/drl_core.pl` (dr_type, integrate_signature_with_modal); `docs/deferential_realism_paper_v6.13.md` (theorem 2 statement)

**Specific question:** H¹ in this framework is computed over the signature-resolved dr_type orbit — the path cohomological_obstruction → orbit_vector → type_at_context → dr_type applies the structural signature before H¹ counts disagreement. Raw classify_from_metrics types may be — and for the prohibition anchor are — maximally heterogeneous ([naturalized, snare, rope, snare]) while signature-resolved types are uniform ([tangled_rope × 4]), yielding H¹=0. The v6.13 paper describes H¹ as "a partition functional over the classifications assigned by the observer positions" without specifying which classification — raw or signature-resolved. The v7 draft was loose at exactly the same point and had to be corrected. Does the v6.13 statement need a precision amendment to make the signature-resolution explicit?

**Evidence so far:** The verification run for the prohibition anchor produced the precision note: H¹ measures coherence of the signature-resolved orbit, not raw-type uniformity. v7 was revised to state both orbits explicitly (raw and signature-resolved), report them together as the corpus confirmation, and tie the result to Theorem 1 (the signature is the cover story). v6.13's Theorem 2 statement and the surrounding orientation prose use "classification orbit" without disambiguation; a careful reader running classify_at_time directly would get raw types and not know they don't match what H¹ counts. This is the same kind of silent precondition the FPN convergence proof carried before the run (an assumption stated in prose, never made code-visible).

**What resolution changes:** Either (a) v6.13 Theorem 2 is amended to specify "signature-resolved classification orbit" wherever "classification orbit" currently appears, and the engine carries a comment at cohomological_obstruction confirming the path goes through dr_type (signature-resolved), not classify_from_metrics (raw); or (b) the paper stands with the looser phrasing and v7 carries the precision as a v7-specific clarification. Option (a) eliminates the ambiguity at the source; option (b) leaves the source loose and patches it downstream. The latter is the pattern that produced the hanbali misattribution (v7 inherited v6.13's loose phrasing, then I wrote a theorem on top of it).

---

## OQ-28 — Seat Theorem v1.1 honesty edits batched but not all witnessed by runs

**Status:** resolved — (2026-06-09, option (a) as the entry pre-ruled: "the asymmetry is
acceptable as long as it's named"). An **Amendment provenance** section was added to
`docs/seat-theorem-v1.md` (now v2.4) naming the witness-asymmetry: edit (1), the §3
seat-orthogonality correction, is a result-claim and carries its run-witness
(`test_forecloses_fpn_injection.pl`); edits (2) (§5 moral-not-mechanism downgrade) and
(3) (§8 P3-locality) are scope-clarifications — they narrow what the prose claims rather
than assert measurable properties, so they owe declaration, not run-grounding. All three
edits were already present in the doc body; what was missing (and is now added) was the
asymmetry being named. *(body compressed at close per footer rule)*
**Origin:** Seat Theorem amendment work, May 2026.
**File:** `docs/seat-theorem-v1.md`

---

## OQ-29 — Results stale against a moved corpus (general form of OQ-25 §5.11 scope)

**Status:** open
**Priority:** 1
**Origin:** Sweep-consolidation audit, 2026-05-29.
**Files:** `python/bifurcation_results.json` (confirmed stale instance); 19 `*_results.json` files total (full list below).

**Specific question:** All 19 `*_results.json` files carry no record of which corpus they were computed against (no `corpus_hash` or `manifest.code_commit` field — grep confirmed: 0/19 have either). How many are stale against the current `testsets/` corpus (corpus_path='testsets', 223 constraints)? Is it possible to tell from the file contents alone?

**Mechanism (grep-confirmed):**
`bifurcation_results.json` reports flip findings for 7 constraints:
`autonomy_as_character_ideal`, `conformity_extraction`, `distributed_memory_as_counter_disposal`,
`epistemic_authority_erosion_through_unresolvable_anomaly`, `meritocratic_ideology_as_error_propagation`,
`role_capture_through_cost_asymmetry`, `solidarity_mirror_trap`.
All 7 are absent from `prolog/testsets/` (empty grep) and present only in `prolog/testsets_3000/` (archive). The file reads authoritative but describes a corpus that no longer exists. The sweep-consolidation audit initially proposed using these flips as detection witnesses before discovering the file is stale.

This defect class is NOT produced-but-not-consumed (most files have consumers) and NOT silent-fork (one canonical location per file). It is **produced-against-a-substrate-that-moved**: a result outliving the corpus it describes. OQ-25's staleness concern scoped to a single ε-number in a chimera scenario; OQ-29 is the general form across all result files.

**Population (run-confirmed, 2026-05-29):** 19 results files, 0 with corpus_hash:
`axiom_reachability_results.json`, `epsilon_sensitivity_results.json`,
`metric_audit_results.json`, `sheaf_audit_results.json`,
`alt_power_transform_results.json`, `alt_power_transform_results_3k.json`,
`bifurcation_results.json`, `cognitive_displacement_results.json`,
`config_sensitivity_results.json`, `config_sensitivity_results_test.json`,
`config_sensitivity_results_v3.json`, `directionality_sensitivity_results.json`,
`oracle_gap_results.json`, `persistence_results.json`, `product_site_delta_results.json`,
`representation_robustness_results.json`, `structural_config_sensitivity_results.json`,
`structural_config_sensitivity_results_original.json`, `test_battery_results.json`.

Confirmed stale: `bifurcation_results.json` (constraint-level grep). Others unverified — they may be stale (if run against testsets_3000/) or current, but the file itself cannot tell you which.

**What resolution changes:** Every `*_results.json` producer stamps a `corpus_hash` field (sha256 of the sorted list of loaded `.pl` filenames, or the git HEAD of `prolog/testsets/` at run time). The new `perturb()` primitive (`python/sweeps/perturb.py`) stamps `baseline_hash` at the orbit level — extend this pattern to all producers. Consumers check staleness: if current testsets hash ≠ stored hash, flag stale rather than read as authoritative. This is the file-level analogue of the Step-2 coverage gate in the sweep-consolidation primitive: a result computed against a dead corpus is the file-level cousin of a verdict computed with zero coverage.

**Partial resolution (2026-05-29):**
- `python/sweeps/perturb.py` now stamps `corpus_hash` (sha256 of sorted filename+content pairs from `prolog/testsets/*.pl`) in its output dict and checks the orbits file's stored hash before running
- `python/run_pipeline.py` now stamps `corpus_hash` into `outputs/product_site_orbits.json` in its `_manifest_step` (the step that also stamps pipeline_output.json)
- `python/sweeps/demotion_pass.py` stamps `corpus_hash` in its output JSON
- Guard is **dormant** for orbits files lacking a `corpus_hash` field (all files produced before 2026-05-29). Soft warning emitted, not hard error. Guard becomes active on the next orbits regeneration + pipeline run. Confirm engagement with: run perturb and check that no "corpus_hash" warning appears in stderr.
- Remaining unstamped: 18 `*_results.json` producers (the 19 from above minus demotion_pass.py). Not closed.
- Standalone swipl path (`run_product_export`) does NOT auto-stamp — user must run `python3 python/run_pipeline.py` or call `_stamp_orbits_corpus_hash` manually after regenerating orbits.
- content hash (not filename-only): detects in-place edits; does NOT detect changes in `testsets/<run_tag>/` subdirs.

**Known weakness in the stamp-then-check pattern (2026-05-29):**
The guard's invariant requires stamp-happens-atomically-with-generation. `_stamp_orbits_corpus_hash` in `_manifest_step` post-processes whatever orbits file exists — it does NOT regenerate orbits. If the orbits file is stale and the user runs `python3 python/run_pipeline.py` without first regenerating orbits, the pipeline will stamp the stale file with the current corpus hash, causing Guard 2 to silently pass on a stale baseline. This is OQ-29 one layer in.

The current `outputs/product_site_orbits.json` was stamped in place without regeneration (2026-05-29): it covers 191/223 current readings (all 191 are in current testsets; 32 current readings absent — produced from an earlier partial run). The corpus_hash check passes because the testset FILES haven't changed since the orbits were produced. The 32 missing readings are a coverage gap not detected by the hash guard.

**Correct fix (not yet implemented):** Stamp must happen inside (or immediately after, in the same subprocess invocation as) the swipl step that produces the orbits. The Python-side `_stamp_orbits_corpus_hash` is only trustworthy when called directly after `run_product_export` in the same process — not as a separate pipeline step that runs regardless of whether orbits were regenerated.

---

## OQ-31 — enhanced_report.py: five sections stubbed, not deleted — record why

**Status:** resolved — option (a) taken, deletion confirmed by git diff
**Resolved:** 2026-05-29 (commit 7af6b945)
**Origin:** Phase 2 restructure, 2026-05-29.
**Resolution:** all five section builders DELETED (not stubbed) in 7af6b945 — five `-def`
removals confirmed; render verified end-to-end. Cut because none fed the iteration-prompt
contract (orchestrator.py:785–807) or the model-flinch layer: `build_level3_distribution`
(corpus stats, no actionable signal), `build_structural_section` (stale pattern_mining/covering
inputs), `build_wasserstein_section` (verdict banner + hard_disagreement cover it),
`build_cohomology_section` (H¹ already in Level 1 + sidecar), `build_game_theory_section`
(stale sweep data — OQ-29; E5 stability band supersedes). **Re-adding any requires showing those
criteria changed, not "might be useful."**
*(Body compressed 2026-06-04 per footer rule; full per-section rationale in git history.)*

---

## OQ-30 — Stability band witness set incomplete (one confirmed pair only)

**Status:** mitigated — (2026-05-30); Surface-2 lock front WITNESSED (2026-05-31). 24 params
witnessed across 18 kernels; the Surface-1-unwitnessable remainder are signature-locked.
Signature audit (2026-05-30) + Surface-2 sweep (2026-05-31, see end of this OQ): of 96
Boltzmann-gated locked readings, 56 load-bearing / 40 over-included; `boltzmann_coupling_threshold`
flips 48/56 load-bearing final types (Surface 2 = critical path, via the coupling threshold NOT
the floor — original floor hypothesis falsified); 6 load-bearing remain Surface-2-immovable.
Tool: `python/sweeps/surface2_lock_sweep.py`; results `outputs/surface2_lock_sweep_results.json`.

**Origin:** Step-2 wire audit, 2026-05-29.

**Specific question:** `enhanced_report.py`'s stability band (`_WITNESSED_PARAMS`) had one
confirmed (param, kernel) pair: `snare_epsilon_floor × end_of_life_decision_authority`. The
±10% batch (2026-05-29, `outputs/witness_backlog_results.json`) expanded this to 24 params
witnessed across 18 kernels. 22 kernels still render "not yet witnessed."

**Mechanism (empirically confirmed):**  
96/97 kernel-linked readings have signature overrides. Breakdown:  
- `false_natural_law` (76 readings): unconditional tangled_rope lock — chi_floor params
  reach the metric decision path (coverage>0) but the final type is signature-locked. NOT
  valid governing params.  
- `false_ci_rope` (17 readings): conditional — may preserve metric type if perspectival
  variance detected. Needs per-kernel witness.  
- `coupling_invariant_rope` (3): unconditional rope lock.  
- `constructed_high_extraction` (vulnerability_protection_reading, confirmed flip):
  preserves metric type for non-unknown → `snare_epsilon_floor` produces flips on this.

**Broad-spectrum finding (±10% batch):** `sigmoid_midpoint`, `sigmoid_steepness`,
`sigmoid_upper`, `sigmoid_lower`, and `snare_chi_floor` are broad-spectrum governing params
affecting nearly every `false_ci_rope` kernel.

**Epsilon param characterization (2026-05-30):** All 4 epsilon params were present in
`outputs/witness_backlog_results.json` — swept at the end of the prior batch (priority bug
meant they ran last, not that they were skipped). `--resume` confirmed all 141 backlog
params already done (0 new sweeps). Corrected tiering:

- `rope_epsilon_ceiling` (original=0.45) — **split tier:**
  - +10% (0.495): unperturbable-by-construction. `config_schema.pl:482–487`
    `classification_rope_snare` invariant: `rope_epsilon_ceiling >= snare_epsilon_floor`
    fires → `export_failed`. This direction is permanently blocked.
  - −10% (0.405): reachable-stable. 23/38 kernels reached (top: `kami_buddha_ontology`
    cov=0.625, `press_reformation_causality` cov=0.500), fold_survival=1.0 on all, 0 flips.

- `tangled_rope_epsilon_floor` (original=0.3) — **perturbable-but-unperturbed (earned).**
  25–26 kernels reached across the full ±10% band (all three values tested), fold_survival=1.0
  on all reached kernels. Top: `equal_protection_clause` cov=0.375 at −10%. Genuine stability
  finding — the threshold was exercised and held, not untouched.

- `fpn_epsilon` (original=0.001) — **unreached-at-tested-range.** Coverage=0 at all three
  values (0.0009, 0.001, 0.0011). The ±10% band around 0.001 never contacts any kernel's
  metric decision path. Flip potential unknown; wider range required.

- `piton_epsilon_floor` (original=0.1) — **unreached-at-tested-range (near-blind).** 2/38
  kernels at +10% only (`latin_correctness` cov=0.083, `personhood_boundary` cov=0.050),
  fold_survival=1.0, 0 flips. Blind at baseline and −10%.

**Bucket counts (2026-05-30, 191 total unchanged):**
```
shadowed:                       6
errored-untested:               0
unperturbable:                 20
reachable-locked:               0
witnessed (survivors):         24
backlog:                      141
  of which:
    unreached-at-tested-range:  2  (fpn_epsilon, piton_epsilon_floor — coverage=0 or
                                    near-0; ±10% non-informative; flip potential unknown)
    remainder:                139  (includes rope_epsilon_ceiling one-sided split and
                                    tangled_rope_epsilon_floor full-band stable)
─────────────────────────────────
total:                        191
```
`unreached-at-tested-range` is distinct from `errored-untested` (that bucket means
integer-typed params that rejected under the float sweep). These two params did not error;
they produced structurally valid zero-coverage results at ±10%. They need a wider range, not
a corrected type.

**Signature audit (2026-05-30) — 51 triples, UNBOUND query, Pattern 3 verified:**

UNBOUND query form used (Sig not pre-bound):
```prolog
findall(K-C-Sig, (member(K, UnwitnessedKernels), cs_kernel_id(C, K),
                  constraint_signature(C, Sig)), Triples)
```
Pattern 3 self-check: `behavioral_competence_reading → false_summit_mountain` (not natural_law).
One verbatim result: `federation_membership_kernel-integration_reading-false_natural_law`.

Signature distribution (51 readings across 20 kernels):
- `false_natural_law`: 48 readings (17 kernels all-FNL; 2 kernels mixed FNL + coupling_invariant_rope)
- `coupling_invariant_rope`: 2 readings (personhood_boundary/birth_reading, unconditional_income_support/freedom_floor_reading)
- `constructed_low_extraction`: 1 reading (qwerty_persistence_mechanism/naturalization_reading)
- `false_ci_rope`: **0 readings**
- `false_summit_mountain`: **0 readings**

Per-kernel MaxCoverage (witness_backlog_results.json, float ±10% batch):
kami_buddha_ontology=0.625, federation_membership_kernel=0.500,
press_reformation_causality=0.500, speech_protection_boundary=0.417,
unconditional_income_support=0.375, climate_response_imperative=0.333,
living_language_status=0.333, acceptable_risk_for_energy=0.250,
border_normative_status=0.250, monetary_anchor_principle=0.250,
personhood_boundary=0.250, plural_marriage_mandate=0.250,
preparedness_retention=0.250, qwerty_persistence_mechanism=0.250,
state_execution_authority=0.250, state_killing_authority=0.250,
substance_control_authority=0.250, territorial_sovereignty_legitimacy=0.250,
us_constitution_text=0.250, catastrophe_memory_transmission=0.167.
All 20 have MaxCoverage > 0, AnyFlip = NO. Zero UNREACHED kernels.

**Confirmed false_natural_law lock mechanism** (`signature_detection.pl:829–847`):
```prolog
false_natural_law(C, fnl_evidence(...)) :-
    claimed_natural(C, Claim),           % structural + partial metric-dependence
    boltzmann_compliant(C, BoltzmannResult),
    BoltzmannResult = non_compliant(_, _),   % ← the lock gate (coupling-driven, NOT the floor)
    ...
```
`claimed_natural/2` has three sources: explicit mountain claim (structural), indexed mountain
classification (metric-dependent), natural_law_signature profile match (metric-dependent).

**CORRECTION (2026-05-31, Surface-2 sweep — `outputs/surface2_lock_sweep_results.json`,
`python/sweeps/surface2_lock_sweep.py`): the earlier annotation conflated two distinct
Boltzmann levers.** The FNL lock gate is `boltzmann_compliant(C, non_compliant(_,_))`, which is
`cross_index_coupling(C, Score) =< complexity_adjusted_threshold(C, Thr)` where
`Thr = boltzmann_coupling_threshold(0.25) + coordination_type_offset` (`boltzmann_compliance.pl:380-383`).
It depends on the coupling SCORE and the coupling THRESHOLD — **not** on `excess_extraction`, which
is what the `boltzmann_floor_*` overlay (and the −0.52 proof-of-life) moves. The two FNL/CI_rope
override gates do not consume `excess_extraction` (`signature_detection.pl:927-930` — gating on it
was deliberately removed). So perturbing the **floor** is the WRONG lever for the FNL lock: it moves
excess but leaves `boltzmann_compliant` and the final `dr_type` unchanged (witnessed: `abolition_reading`
holds `tangled_rope` across floor 0.0→0.99). The lock IS Surface-2-displaceable — via
`boltzmann_coupling_threshold` (and the additive `coordination_type_offset`), which flips
`non_compliant→compliant` and the final type. *Tier: perturb-confirmed (full 96-reading sweep below).*

**Three-tier classification (corrected from initial draft):**
- **REACHABLE-BUT-LOCKED (Surface-1): 19 kernels.** Params reach metric path (coverage>0)
  but signature override eats the change. Lock is displaceable on Surface 2 via
  boltzmann_floor overlay — untested, open. This is NOT "unwitnessable"; it is
  "unwitnessable on Surface 1."
- **UNREACHED-AND-LOCKED: 0 kernels.** All 20 kernels were reached at ±10%.
- **UNLOCKED-REACHED-BUT-HELD: 1 reading** (qwerty/naturalization_reading,
  constructed_low_extraction, coverage>0, AnyFlip=NO). Wider-range rope_chi_ceiling
  sweep is the targeted next step, but lock_in_reading (false_natural_law) will still
  contribute tangled_rope to kernel fold_survival even if naturalization_reading flips.

**Critical-path implication:** Surface 2 (Boltzmann-floor overlay) is now the only
instrument that can witness 19 of 20 remaining kernels. Before this audit, Surface 2 was
a novelty front. After it, it is the critical path for the largest stuck pile. The Surface-1
kernel instrument has hit its empirical ceiling — not from insufficient coverage, but because
all 19 locked kernels were reached and watched the override eat every metric change.

**What resolution changes:** Witnessing the 19 requires a Surface-2 sweep primitive
(boltzmann_floor overlay per-constraint, observable = constraint_signature/2 or
integrate_signature_with_modal output). If the boltzmann_floor overlay can shift a kernel
from `non_compliant` to `compliant`, false_natural_law fails, and the reading falls through
to the next clause. Whether that next clause produces a flip depends on the reading's other
structural facts (claimed_natural third source, false_ci_rope eligibility, etc.).

**Next move:** Build a Surface-2 sweep primitive (boltzmann_floor overlay per-kernel,
observable = `constraint_signature(C, Sig)` or `integrate_signature_with_modal/3` output).
Target the 19 REACHABLE-BUT-LOCKED kernels. Size the defaults-inventory pass (Surface 3)
first: if front-sized (temporal surface on fabricated inputs), Surface 3 is compromised and
Surface 2 is the clean next build; if afternoon-sized, Surface 2 is unambiguously next.
qwerty/naturalization_reading wider-range rope_chi_ceiling sweep is an independent, lower-priority action.

**Query trap (2026-05-30):** Always use the unbound form:
```prolog
findall(K-C-Sig, (member(K, Kernels), cs_kernel_id(C, K),
                  constraint_signature(C, Sig)), Pairs)
```
The bound form `findall(C, constraint_signature(C, false_natural_law), Cs)` bypasses lock cuts
and over-counts — live demo: `behavioral_competence_reading` appears bound but resolves to
`false_summit_mountain` under the unbound query. See `docs/technical/build_discipline.md` Pattern 3.

**SURFACE-2 PRIMITIVE BUILT + HYPOTHESIS WITNESSED (2026-05-31).**
Tool: `python/sweeps/surface2_lock_sweep.py` (one swipl process, corpus loaded once, in-memory
`retract/assertz` overlay of three Boltzmann levers swept INDEPENDENTLY; observables
`excess_extraction`, `boltzmann_compliant`, `dr_type/2` at default/analytical context).
Results: `outputs/surface2_lock_sweep_results.json`. Both positive controls pass (PoL floor flip
on `civic_eugenic_reading` reproduced; coupling overlay moves `boltzmann_compliant` on
`abolition_reading`).

Target derived in-engine, NOT inherited: **96 Boltzmann-gated locked readings** (FNL 76, FCR 17,
CI_rope 3) across the live corpus (the handoff's "19 of 20 / 51 readings" is superseded — corpus
grew to 97 kernel-linked readings). Of the 96, **56 are LOAD-BEARING** (override changes final
type: metric_type ≠ final dr_type) and **40 are over-included** (final == metric — the bare
signature-read over-includes by 40, confirming Item-1b's clause-read prediction by witness).
All 31 load-bearing kernels are REACHED on Surface 1 (`perturb.py snare_chi_floor`, coverage>0,
never-reached = []).

Witnessed flip counts (perturb-confirmed):
- **`boltzmann_coupling_threshold` flips final dr_type for 48/96 (48/56 load-bearing; 0/40
  over-included).** By signature: 45 FNL, 3 CI_rope. The over-included never flip — a clean
  control that load-bearing is the right discriminator. → **CORRECTED HYPOTHESIS WITNESSED:
  Surface 2 IS the critical path, via the coupling threshold.**
- **`boltzmann_floor_*` flips only 5/96** — and only the *Boltzmann-compliant* CI_rope/FCR-scaffold
  cluster (coupScore=0), where high excess (low floor) trips `false_ci_rope` (excess-gated via
  `collect_fcr_failures`, priority 77 > CI_rope 114) → tangled_rope, vs low excess → CI_rope → rope.
  For the 76 FNL-locked majority the floor moves `excess_extraction` but `boltzmann_compliant` /
  `false_natural_law` / `tangled_rope` all hold (the FNL gate is coupling-driven, excess is not on
  it). → **ORIGINAL "floor flips the locked kernels" hypothesis FALSIFIED as the primary lever**
  (set-not-count: a non-uniform floor — 5 flips — would have hidden under an aggregate "0/N").
- **`coordination_type_offset` is a real SECOND lever** (48 flips, same set, additive into
  `complexity_adjusted_threshold`) — flips at offset (+0.50,+0.75] up / (−0.30,−0.25] down.
- **Surface-2 combined (any lever) witnesses 50/56 load-bearing.** Residual **6 Surface-2-immovable
  load-bearing**: 5 with metric_type=unknown (FNL; when the lock breaks they re-pin to tangled_rope
  via another route, no observable flip) + `theological_climb_reading` (FCR with a per-constraint
  `boltzmann_floor_override` → floor shadowed, coverage=0; coupling-compliant). What would close
  these: a different observable (the metric=unknown ones need the metric path repaired, OQ-37
  territory; the override-shadowed one needs the `boltzmann_floor_override` fact perturbed).

Boundary distribution (the signal): FNL non-compliant cluster flips at `coupling_threshold`
∈ (+0.83,+1.00] (= coupScore − coordination_offset, coupScore mostly 1.0); CI_rope compliant
cluster flips downward at (−0.10,+0.00]. Bimodal, both directions required.

**Signature before→after (per-reading, perturb-confirmed).** Of the 48 coupling type-flips: 44 are
`false_natural_law → false_ci_rope` with `tangled_rope → snare`; 1 (`existential_matrix_reading`) is
`false_natural_law → coupling_invariant_rope` with `tangled_rope → rope` (genuine-coordination
certification when forced compliant); 3 CI_rope readings (`birth_reading`, `study_as_exercise`,
`freedom_floor_reading`) break downward to `false_ci_rope`/`false_natural_law` with `rope → tangled_rope`.

**Per-kernel rollup (units: the above are PER-READING).** The 96/56/40 are readings; they roll up
to **38 kernels** (corpus grew — handoff 7's drift note), of which **31 carry ≥1 load-bearing reading**,
**28 have ≥1 coupling type-flip**, 29 have ≥1 Surface-2 flip (any lever). The handoff's "19 of 20
kernels" is superseded on both axes (kernels 20→31; readings 51→96 at ~2.5 readings/kernel). 2 of 31
load-bearing kernels are fully Surface-2-held (their only load-bearing readings are metric=unknown
holdouts).

**Calibration (is the flip boundary precarious or robust?).** Robust — on the **live sweep alone**
(`outputs/surface2_lock_sweep_results.json`), with **no surviving external corroboration**. The coupling
SCORE is bimodal at the extremes (71/96 at 1.0, 7 at 0.0 = 78/96 = 81% extreme; only 16 in the
(0.40,0.90) mid-band), and the margin (score − threshold) is correspondingly wide (median 0.67; 79/89
readings >0.5 from their flip boundary; the baseline threshold band ~0.25–0.40 sits in the empty gap
between the score clusters). **Bimodality and margin are two VIEWS of ONE distribution — the
coupling-score distribution — not two independent witnesses.** The witnessed flip needs ~0.92 (+268%),
far outside any plausible threshold uncertainty. → The classifications rest on the coupling-score
*structure*, not on the threshold value; the FNL lock is a **wide-margin partition, not a knife-edge**.
*Tier: single-source (live sweep); two-views-of-one-distribution; no external corroboration.*

**WITHDRAWN witness (2026-06-01).** An earlier draft of this block claimed "Robust, **three independent
witnesses**," the third being an independent ±25% sweep (`config_sensitivity_results.json`) rating
`boltzmann_coupling_threshold` **Inert** (910/0). That witness is **withdrawn**: the harness rates
154/154 params Inert with **zero failures on any perturbation**, so it has **no positive control** — its
"Inert" on the coupling threshold is *unfalsifiable, not corroborating* (and it pre-dates the current
corpus/engine). Do **not** re-run it as a fix; the harness *design* is the problem, not its staleness.
The wide-margin finding stands on the live sweep above; the "three independent witnesses" framing does
not — the calibration rests on one source, viewed two ways.

(What this does NOT settle: whether the coupling-score *computation* is the right operationalization of
natural-vs-constructed — the seat-free-certification limit, undecidable from inside the engine;
human-stakes / OPEN-by-construction.)

**PARTIAL-LOCK finding (signature flips, type holds — its own row).** For ~35 readings the coupling
overlay flips the *gate* (`boltzmann_compliant non_compliant→compliant`) and the *signature*
(`false_natural_law → false_ci_rope`) but the final `dr_type` HOLDS at `tangled_rope`. These are
exactly the readings whose metric type is already `tangled_rope` (the 40 over-included) or `unknown`
(5 of the 8 load-bearing holdouts): when FNL hands off to `false_ci_rope`, the FCR override resolves
to the metric type, which for these is already `tangled_rope` — so the lock is *partial* (diagnostic
signature moves, classification does not). This is why the type-flip count (48) is below the
gate/signature-flip count: the coupling threshold moves the Boltzmann signature corpus-wide on the FNL
population, but only changes the final type where the metric type differs from `tangled_rope`. Witness:
`outputs/surface2_lock_sweep_results.json` (per-value `sig` field).

---

## OQ-32 — bifurcation_sweep.py: path resolution broken after 2026-05-28 reorg

**Status:** resolved — 2026-05-29 (6 scripts fixed)
**Origin:** Task 5 probe run, 2026-05-29.
**Resolution:** `parent.parent` → `parents[2]` in 6 sweep scripts (bifurcation,
cognitive_displacement, persistence ×2 sites, product_site_delta, representation_robustness,
structural_config_sensitivity). **Remaining (backlog, not a path bug):** a current-corpus
bifurcation witness for no-kernel readings is UNWITNESSED on the live corpus — the old witness
(14 flips, `snare_chi_floor=0.655`) was testsets_3000; the 6 stale `python/*.json` result files
are HELD pending re-run.
*(Body compressed 2026-06-04 per footer rule.)*

---

## OQ-33 — classify_at_time fabricates suppression on 100% of temporal corpus; 279/647 temporal classifications misclassified as tangled_rope instead of snare

**Status:** resolved — 2026-06-11
**Origin:** Fabricated-default inventory session, 2026-05-30. Tripwire graduated 2026-05-30.
**Resolution:** options (c)+(a); (b) rejected (collapses Surface-1/3 independence). Engine
fail-close landed 2026-05-31 (OQ-41 row 23, commit `39630182`): temporal `measurement/5` →
authored-scalar STOPGAP (`SuppBacked=false`) → fail-closed `unknown`; the post-reset template
authors suppression at source. Re-witnessed 2026-06-11 (`audits/2026-06-11_oq33_close/`):
0 unknown-floor / 0 residual-0.5 over 209 live + 3,497 kernel_v1 rows, per-process controls;
D2 static else-branch 0/46. Blocks: (1) Surface-3 primitive unblocked for temporal-backed
constraints (Probe A); (2) pre-reset classifications mooted — witnessed clean post-disposition
(Probe D re-scan, archive-side control in-run; artifacts relocated, writeup §5); (3)
cross-surface divergence carries the `Backed` bit (`drl_composition.pl:238`). Consumer-side
`Backed` verification stays with OQ-83; the scalar fallback was ruled SANCTIONED, not retirable
(OQ-46 resolved 2026-06-11 — the prompt deliberately authors scalar-only for static enforcement);
ε fallback rows 24-27 = OQ-41.

## OQ-34 — Estimator-classifier independence audit: does the prompt expose MI-decision-rule thresholds to authors?

**Status:** resolved — generalized and executed as the 2026-06-05 generation-pipeline de-leak

**Question:** The NL circularity audit (2026-05-31) established a design principle:
thresholds that are *definitional bounds* on the substrate (what the author is
estimating) should be visible to authors; thresholds that are *measurement-independent
decision rules* the engine applies to author-estimated values should not be. When an
author sees a MI-decision-rule threshold, they satisfy it mechanically, the diff between
their estimate and the engine's verdict collapses to zero, and the classification stops
carrying information.

The fix for `accessibility_collapse ≥ 0.85` and `resistance ≤ 0.15` has been applied
(`fix/stripped_prompt.md`, `fix/stripped_schema.json`). The question is whether the
remaining thresholds exposed in the generation prompt and schema follow the same
discipline — or whether other MI-decision-rule thresholds are still visible to authors,
producing silent stamp patterns analogous to the 84.3% AC=0.92 finding.

**Thresholds currently exposed in `prompts/constraint_story_generation_prompt_json.md`
and `python/constraint_story_schema.json` (inventory as of 2026-05-31):**

| Threshold | Where exposed | Class | Settled? |
|---|---|---|---|
| `ε ≤ 0.25`, `suppression ≤ 0.05` | Mountain | **Definitional** — mountain = low-extraction substrate by definition | Yes |
| `ε ≥ 0.46`, `suppression ≥ 0.60` | Snare | Definitional or MI — needs classification | Open |
| `χ ≥ 0.66` | Snare | **MI by construction** — χ = ε × f(d) × σ(S) is engine-computed; author never authors χ | Yes |
| `χ ≤ 0.35` | Rope | **MI by construction** — same formula | Yes |
| `ε ≤ 0.45` | Rope | Definitional or MI — needs classification | Open |
| `theater_ratio ≥ 0.70` | Piton | Definitional or MI — needs classification | Open |
| `ε ≤ 0.05` | Rope-only archetypes | **Definitional** | Yes |

**χ rows are settled.** χ = ε × f(d) × σ(S) is entirely engine-computed: the author
sets ε; f(d) and σ(S) are derived from the structural context the engine reads. The author
never authors χ. This makes every χ threshold in the prompt MI-decision-rule by
construction — no audit required to classify it. The back-solve concern confirms, not
softens, this: an author who knows "snare needs χ ≥ 0.66" and understands the formula
sets ε to clear the bar under the expected context — which is the AC stamp mechanism one
indirection deeper (the stamp lands on ε, not χ, but it is still rule-satisfaction, not
domain estimation). The classification question is closed. The remaining open question for
χ-exposed rows is **empirical**: does ε cluster at the back-solved value in the corpus?
That is a grep, not a classification debate.

**Addendum (2026-05-31): three constraints on the resolution procedure.
The audit should start from these, not rediscover them.**

**Constraint 1 — χ rows are MI by derivation, not by hypothesis.**
χ = ε × f(d) × σ(S) is entirely engine-computed; the author never writes χ. Any χ
threshold (`χ ≥ 0.66` snare, `χ ≤ 0.35` rope) is therefore MI by construction — there
is nothing to classify. Mark them MI in the inventory and skip straight to step 2 (the
grep). The table above already records Settled=Yes for χ rows.

**Constraint 2 — The classification rubric needs a third case: definitional-in-direction,
MI-in-value.**
The binary (definitional vs. MI) misclassifies snare `ε ≥ 0.46`. Snare-ness is not
defined as "high ε" the way mountain-ness *is* defined as "low ε" — snare requires high
extraction AND χ clearing the floor AND victims. The *direction* (high extraction) is
definitional substrate the author needs; the *exact value* 0.46 is the engine's decision
rule and should not be exposed. Fix for this class is not strip-or-keep but **keep the
direction, strip the number**: tell the author "snare is a high-extraction constraint,"
never "ε ≥ 0.46." Contrast mountain `ε ≤ 0.25`, which is definitional in both direction
and value (the author only needs "low," and the precise cap does little further work) —
keep as-is. Classify each ε/suppression open row as:
- **D-both**: direction and value are definitional — keep as-is
- **D-direction / MI-value**: keep the directional descriptor, strip the number
- **MI-both**: strip entirely

**Constraint 3 — The stamp grep must target the authored input field, not the threshold
field.**
The AC audit was directly grep-able because AC is authored and the threshold bounds the
authored value. χ-driven thresholds contaminate **ε** (the field the author actually
writes), not χ (which is never authored). For any χ threshold: (a) back-calculate the ε
value that produces the threshold-clearing χ under canonical context (canonical d, σ(S))
first; then (b) grep the ε distribution for clustering at that back-calculated value.
Grepping for the χ value (0.66, 0.35) will find nothing authored and falsely report "no
stamp" on the thresholds most suspected. Expected-stamp target for snare: the ε that
makes χ = 0.66 at canonical d and σ, not 0.66 itself.

**What resolution looks like:**

1. **Classify the open rows** (`ε ≥ 0.46`/`suppression ≥ 0.60` for snare, `ε ≤ 0.45`
   for rope, `theater_ratio ≥ 0.70` for piton) using the three-way rubric (D-both /
   D-direction+MI-value / MI-both). χ rows skip this step — they are already MI.

2. **Audit the corpus for stamp patterns on all MI and D-direction/MI-value rows.**
   For ε/suppression rows: grep authored values, tabulate the distribution, look for
   clustering at or just above/below the threshold number. For χ rows: back-calculate the
   ε that clears the χ threshold under canonical context, then grep ε for clustering there.
   A clustered ε distribution at the back-calculated value is a stamp; a spread is an
   independent estimate.

3. **Verify FSM threshold exposure**: FSM's gate involves `constraint_beneficiary/2`
   (static authored facts, not a metric value), so the stamp mechanism differs from metric
   stamps. Flag if FSM knowledge produces strategic beneficiary declaration patterns.

4. **Write findings to `docs/`** as a witnessed audit document (grep counts, value
   distributions, stamp verdict per threshold). Only thresholds with witnesses belong in
   the resolution note.

**What resolution changes:**

- MI rows with stamp patterns → extend the prompt strip; update `fix/stripped_prompt.md`
  and `fix/stripped_schema.json`.
- MI rows without stamp patterns → document as MI-but-clean and note that stripping is
  still correct in principle (the stamp may simply not have formed yet at corpus scale).
- Definitional rows → no action; record the rationale so future audits don't relitigate.
- Either way, the resolution document is the canonical record of which thresholds the
  engine uses, which are author-visible, and whether each is structurally safe to expose.

**Evidence in hand:** AC=0.92 stamp (84.3% of 465 declared values), T.1 bucket split
(404/404 = 100% metric-real), prompt stable from `51033e8a 2026-02-21`.
See `docs/technical/build_discipline.md` § Estimator-classifier independence.

---

## Wiring-Gap Census stubs (OQ-35 – OQ-42) — opened 2026-05-31

Source: `audits/2026-05-31_wiring_gap_census/wiring_gap_census.md` (read-only census; git HEAD `220739b8`, live corpus
226 / archive 3380). The census **characterizes** every prompt↔schema↔engine disagreement and
**routes** each for adjudication — it resolves none. Stubs below are grouped by adjudication
decision; the row references map all 27 census rows so nothing is unrouted.
**Adjudication (cruft-vs-wire) is a separate session.**

**Resolution (2026-06-05):** the question's premise was confirmed and fixed at scale: the prompt (and, binding, the SCHEMA shipped inside the prompt) exposed MI-decision-rule thresholds; all stripped (`audits/2026-06-05_generation_pipeline_deleak/` — assembled-payload witness: band-near-type 19→0, threshold-comparisons 28→0, NL-value grep 0 hits post). Definitional-vs-decision-rule line now enforced by AGENTS.md Rule 3b (assembled-payload check before adding any numeric threshold to author-facing surfaces).

## OQ-35 — G1: authored fields the engine never consumes (or consumes only inertly)

**Status:** open — Census rows 1–6.
**Priority:** 1
- Row 1 `mandatrophy_resolved`: schema requires `=true` at ε>0.70; compiler emits no fact; engine
  uses a *separate hardcoded* `is_mandatrophy_resolved/1` (2 names, `narrative_ontology.pl:317-318`).
  **DECISION (D6, 2026-05-31): document and defer — do NOT wire.** Read-only count is near-zero: the
  two hardcoded names (`gale_shapley`, `planetary_boundaries`) appear in **0 live testsets**, and only
  ~7 live constraints sit in the ε>0.70 schema-gate zone (none of them the hardcoded pair). The
  machinery is dormant on the live corpus — wiring earns nothing now. Revisit only if a corpus with
  authored `mandatrophy_resolved` + ε>0.70 arrives (then: emit the fact, have `detect_omega` read it,
  retire the hardcoded list).
- Rows 2–3 `accessibility_collapse`/`resistance`: emitted, consumed only by the cosmetic NL
  signature (T.1: removing NL override = 0 mountain-count change). Decision: strip from
  classification intent or retain as NL-profile documentation. **Low-stakes.**
- Row 4 `cs_reference_frame/2`: emitted (`generate_constraint_pl.py:500`), zero readers. Strip
  emission or wire a committer-drift reader. **Low-stakes.**
- Rows 5–6 `uke_scope.*`, `commentary.*`: provenance / documentation by design — likely no action.

**Resolution would change:** which authored fields are load-bearing vs vestigial.

## OQ-36 — G2: the `intent_*` subsystem (7 predicates) is read but never authored

**Ω-type:** Ω_C (design choice — populate the subsystem or delete it; part of the satisfy-on-absence policy, OQ-44).

**Status:** disposed — operator ruled declare-absence (2026-06-05, triage Item 2 option A): the intent_* layer is a registered design gap (`design_gaps.md` GAP-08), not a feature; populate-vs-delete deferred until a research question consumes intent data (do not grow the authoring surface mid-baseline). Verification found the residual consumer `has_viable_alternatives` defaults PASS-OPEN for NL certification — logged as OQ-43's fifth instance; the gate change is an output-changing call deliberately not made. Originally Census row 7. `intent_power_change`, `intent_beneficiary_class`,
`intent_viable_alternative`, `intent_alternative_rejected`, `intent_suppression_level`,
`intent_resistance_level`, `intent_norm_strength` — **all 0 facts in BOTH corpora**. Read by
`intent_engine` (loaded `stack.pl:43`, empty import) + `signature_detection`; `intent_engine` is
called only by `report_generator`/`test_harness`. SILENT-SAT consequence (witnessed): in
`count_power_beneficiaries` the `affects_constraint(I,C)` conjunct is **live** (`retributive_reading`
binds 2 sources), but the `intent_power_change(I,_,_)` conjunct has 0 facts → join collapses →
Count=0 → the `natural_law` gate's `BeneficiaryCount==0` is vacuously true corpus-wide. This is a
**reached-but-empty-join**, not unreached: `affects_constraint` already supplies the join keys, so
populating `intent_power_change` *alone* would activate the gate. Dead on **both** classification and
committer-drift pipelines. **Decision:** populate (author + schema + prompt) the whole subsystem, or
delete `intent_engine` and its readers. **High-judgment.**

## OQ-37 — G2: engine-read `constraint_metric` names with no author and empty corpus

**Ω-type:** Ω_E (design-relevant — taxonomy-coverage; χ-partition closed structurally, suppression-floor recalibration deferred to OQ-48).

**Status:** open — Census rows 8–12. `inevitability` (`constraint_bridge.pl:22`),
**Priority:** 1
**Deps:** blocked_on OQ-90
`internalization_depth` (`psych_bridge.pl:19`), `resistance_to_change`
(`data_validation`/`json_report`/`utils`), `accumulation_speed` (`utils.pl:211`, explicit 0.0
default) — all 0/0 both corpora, none compiler-emitted. Plus compound measurement metrics
`accessibility_collapse(Level)`/`stakes_inflation(Level)`/`suppression(Level)` read by
`coercion_projection.pl:15` but never emitted (compiler emits `measurement/5` only for
theater_ratio/base_extractiveness/suppression_requirement). **Decision per metric:** author it,
or remove the dead read. **Low-stakes each.**

**`resistance_to_change` row update (2026-06-11, OQ-44 evidence pass).** Re-witnessed never-authored
on the live corpus AND kernel_v1 (0 facts; positive control: sibling name `resistance` fires, 34
live facts) — consumer-only vocabulary at 8 sites across 6 modules. One consumer was a
success-shaped vacuous pass in the validation channel: `data_validation:validate_edge_cases`'s
piton check (`S > 0.3, R < 0.1` join) printed "✓ No pitons detected" unconditionally — the join is
`[]` by absence on every corpus ever, and `validate_all` runs in `validation_suite.pl` every
pipeline. **Fixed 2026-06-11 (honest line, Pattern-6/OQ-96 pass-carries-witness):** the pass branch
now prints joined-table sizes, and an empty resistance table prints
`⚠ piton check VACUOUS: 0 resistance_to_change facts authored — check cannot fire (OQ-37)` — never
the checkmark. Witness: suite run 2026-06-11, line pasted in
`audits/2026-06-11_oq46_backed_reconciliation/`. **Removal/replacement of the heuristic itself was
gated on OQ-90 — now UNBLOCKED:** OQ-90 RESOLVED 2026-06-11 (the FCR-branch capture-keyed piton
refinement is the legitimate successor; the old `Supp≤0.2` `piton_signature` dispatch was already
retired in `fc724ab2`). The `validate_edge_cases` resistance-keyed piton check can now be removed
outright (its successor exists and fires); doing so is the remaining OQ-37 step for this consumer. The row's other consumers are unchanged
and still under this OQ's author-or-remove fork: `drift_events.pl:141,214` (`safe_metric` has no
default, so the `function_obsolescence` detector silently never fires — zero-findings-by-absence;
siblings `alternatives_available`, `sunset_time`, and `has_sunset_clause` are equally unauthored,
killing `sunset_violation` and `scaffold_temporality_check` clause 1 too), `json_report.pl:250`,
`report_generator.pl:507` (prints MISSING — honest-flagged), `utils.pl:205,213,346`.

**Surface-2 holdouts — honest band-gap `unknown` + override removed (2026-06-01).** The 5
load-bearing readings the Surface-2 coupling sweep could not flip (their `metric_type=unknown`
re-pinned to tangled_rope) were diagnosed per-reading with **errors surfaced** (no-throw engine
run + independent band-membership recompute from authored ε/χ/supp vs config thresholds — the
mandatory errored-vs-clean control; a swallowed compute error reads identically to an honest
fall-through). Result: **0 compute bugs (b), 4 taxonomy holes (c), 1 authored gap (a):**
- **(c) ×4** — `diversity_reading`, `competence_reading` (ε0.28 χ0.384 supp0.35: χ in the
  (rope_ceil 0.35, tr_floor 0.40) gap, ε<tr_ε_floor 0.30, supp<tr_supp_floor 0.40);
  `republican_reading` (supp0.35<0.40 the sole blocker, χ/ε in band); `living_constitutionalist_reading`
  (supp0.35<0.40; χ0.658 just <snare 0.66). All inputs **authored and present**; they land in a
  genuine uncovered metric region. The `unknown` (`drl_core.pl:394`) is honest.
- **(a) ×1** — `endogenous_reinterpretation_reading`: tangled-rope metric thresholds all pass, but
  `constraint_victim` is **absent** (has_asymmetric_extraction=NO) and `requires_active_enforcement`
  is **no**. → **authoring gap.** Route the missing victim + enforcement declaration to the
  generation/authoring front; **do not author values here.**

**Override removed (ruling 2026-06-01; commit `c90c5482`).** The "never preserve unknown" behavior
(`signature_detection.pl` FNL `:738` + FCR perspectival `:685`) laundered an honest `unknown` modal
type into tangled_rope. Guarded `ModalType == unknown` at both sites so `unknown` **surfaces**.
Corpus-wide set delta (default context, full corpus): `unknown → tangled_rope : 8` became
`unknown → unknown : 8`; **every other (metric→final) row byte-identical** (snare→tangled 90,
scaffold→tangled 6, mountain→tangled 3, …). N=8 is the **masked-unknown population**, not the
"band-gap population": 5 diagnosed (4c/1a) + **3 UNCHARACTERIZED** (`constitutional_supremacy_reading`,
`hybrid_atrophy_reading`, `relational_autonomy`). Same-path positive control: catastrophic_tail /
husk / abolition (metric=snare, sig=false_natural_law — same `:738` clause, non-unknown modal type)
**stay tangled_rope** (guard does not over-fire). Validation suite 0 errors / 0 warnings.

**Downstream consumption of the surfaced `unknown` — ruled in OQ-51 (2026-06-02):** now that `unknown`
surfaces, `cohomological_obstruction`/`count_disagreeing_pairs` must treat it as **N/A, not a
disagreeing type** (a constraint with <2 real-type seats → H1=N/A, not 0). That is a *consumption*
rule for the honest `unknown` this OQ created — it does **not** re-suppress it here. Design + projected
impact (26 constraints leave `manifest_presheaf`, 5 → undetermined) live in OQ-51's "What resolution
changes"; scoped output-changing task, not yet built.

**Opened by this work (escalated, not self-resolved):**
1. **Taxonomy-coverage.** Does the type system need a band/type for the uncovered region
   χ∈(rope_ceil 0.35, tr_floor 0.40) and/or supp<tr_supp_floor 0.40 with χ/ε in the tangled band?
   4/5 holdouts are honest residents of that hole. This is the forward step if (c) dominates.
   **χ-partition portion CLOSED (Move 1, 2026-06-01, commit `3ab3ace4`):** `tangled_rope_chi_floor`
   lowered 0.40→0.35 (strict) to abut `rope_chi_ceiling`, closing the (0.35,0.40) χ gap structurally
   (0 transitions; the gap was a value-setting artifact, not calibration). The **supp/ε-floor portion
   remains open and is now OQ-48** (recalibration, deferred to post-rebuild — not a structural fix).
2. **Characterize the N−5 = 3 uncharacterized masked unknowns** (constitutional_supremacy_reading,
   hybrid_atrophy_reading, relational_autonomy) — same a/b/c diagnosis. Surfacing them was correct
   regardless of cause (a swallowed compute-error unknown must not be laundered either); their cause
   is simply not yet witnessed.

## OQ-38 — G3: dead-code / orphan triage (export-vs-caller)

**Status:** open — Census rows 13, 4 + §5. **Confirmed dead:** `predict_transformation/3`
**Priority:** 1
(`drl_composition.pl`, 0 callers anywhere), `cs_reference_frame/2`. The exhaustive sweep yields
528 exports → 422 zero-external-caller → {65 `/0` CLI, 114 meta-called, 26 ext-only, **217 candidate**}.
The 217 is an **upper bound**, not an orphan list — it conflates genuinely-dead with
over-exported-but-internally-called; separating them needs clause-head-vs-body parsing per
predicate. **Decision:** scope a clause-level dead-code pass to convert 217 → a real orphan list.
Do **not** strip from the 217 directly — that is the false-orphan trap (cf. the `mandatrophy_resolved`
read-vs-declare canary).

## OQ-39 — G4: prompt rules with no engine enforcer

**Ω-type:** Ω_C (design choice — where the prompt/engine enforcement boundary sits).

**Status:** open — Census rows 14–18.
**Priority:** 1
- Row 14 scaffold "suppression must decline over time": **no trajectory check** exists; scaffold
  uses scalar `Chi` + `has_sunset_clause`. **Decision:** add a trajectory gate or drop the rule.
- Row 15 "final measurement = base extractiveness": unenforced (no validator). **Low-stakes.**
- Rows 16–18 (piton atrophy / Goodhart / perspective-min): narrative-only, committer-only, or
  schema/linter-enforced respectively — likely no engine action.

**Resolution would change:** whether the prompt's temporal rules are real engine constraints.

## OQ-40 — G5: scalar-vs-temporal representation splits

**Ω-type:** Ω_C (design choice — authoritative representation per metric, or document the axis split as intended).

**Status:** open — Census rows 19–22. `extractiveness`, `base_extractiveness`,
**Priority:** 1
`suppression_requirement` each read as scalar `constraint_metric` (observer `drl_core`) **and** as
`measurement/5` (committer `drl_composition`/`drift_events`) — the two representations can carry
different values per axis. Plus `compute_temporal_stability` (`signature_detection`) folds scalar
`constraint_metric` as a pseudo-time-series instead of `measurement/5`. **Decision:** pick the
authoritative representation per metric, or document the axis split as intended.
**Cross-axis-live — see census cross-axis subset.**

**`suppression_requirement` sub-split CLOSED (2026-05-31, side-effect of Commit A / OQ-41 row 23).**
Inside the temporal path there was a *second*, silent G5 split: `classify_at_time` (drl_composition)
fabricated `Supp=0.5` on absent temporal series, while `snapshot_type/3` (transition_paths) already
fell back to the authored **scalar** via `drift_events:safe_metric` — so the two temporal classifiers
disagreed on every constraint lacking a temporal suppression series (650/656 rows). The row-23
scalar-fallback fix **converges them** (both now: temporal → authored scalar → floor). The
convergence is now permanent: OQ-46 (resolved 2026-06-11) ruled the scalar fallback SANCTIONED —
it is not removed, so the split stays closed. (`snapshot_type`/`degradation_chain` additionally
have zero consumers — witnessed grep with positive control, `audits/2026-06-11_oq46_close/`.)
The `extractiveness`/`base_extractiveness` sub-splits (rows 19–20) remain open.

## OQ-41 — G6: fabricated defaults for absent data (fail-closed vs impute)

**Ω-type:** Ω_C (design choice — fail-closed vs impute; subsumed by the OQ-44 satisfy-on-absence policy).

**Status:** partial — row 23 MITIGATED (2026-05-31, Commit A); rows 24–27 open (row 26 NEUTRAL for 3 of 6 sites — 4 OPEN, see coverage correction below). Census rows 23–27. A silent
**Priority:** 1
**Deps:** blocked_on OQ-46
fixed default (`0.5`, `0.0`) substitutes for absent authored data, so the engine computes on a value
nobody authored — distinct from G5 (this is fail-closed-vs-impute, not representation choice).
- **Row 23 `drl_composition.pl` `classify_at_time` `Supp=0.5` — FIXED via scalar-fallback STOPGAP.**
  Adjudication ruling was "return `unknown`," but the **positive control corrected the premise**: of
  656 temporal-timeline rows, **650 had no temporal `suppression_requirement` *measurement* but ALL
  650 carry an authored *scalar* `constraint_metric(C, suppression_requirement, _)`** — genuine-no-data
  = 0. So both `Supp=0.5` (old) and `unknown` (literal ruling) discard real authored data. The census's
  "279/647 flip" undersold the blast radius: the old code ran ~99% of the temporal classification on a
  fabricated constant. Fix: temporal measurement → else authored **scalar** → else `unknown`
  (fail-closed floor, fires 0× now). **268 rows corrected** vs fabricated 0.5 (185 tangled_rope→snare,
  58 unknown→snare, 9 scaffold→mountain, 6 rope→mountain, 10 tangled_rope→unknown) — mostly the
  snare_suppression_floor=0.60 low-mis-sort the census predicted. Validation suite clean (0/0).
  **The scalar fallback was a labeled STOPGAP until OQ-46 (resolved 2026-06-11) ruled it
  SANCTIONED** — scalar-as-constant is legitimate authoring for static-enforcement stories;
  the clause is permanent.
- Rows 24–25 `BaseX=0.5` (`drl_composition.pl:201`): **REACHABLE-BUT-LOCKED, not latent — the
  prior "0 changes; extractiveness required-authored" reason is STALE** (corrected, witnessed
  2026-06-08). Re-witness on the live corpus: the `; BaseX = 0.5` branch *would* fire at **11
  (C,T) cells** (e.g. `attribution_erosion-3`) — constraints with an authored
  `suppression_requirement` measurement at time T but no `base_extractiveness` at that T, so
  extractiveness is NOT required-authored per-timepoint. What locks it is the **call path, not
  authoring**: all 11 cells are at **non-zero times (3,5,8,10,16,19), 0 at t=0**, and the only
  live caller (`cs_kernel_registry`, `classify_at_time(...,0,...)`) classifies at **t=0**; the
  non-zero times are reached only via `constraint_history`, which is **dormant** (consumed by
  nothing — positive-controlled: the same consumer-probe finds `classify_at_time`'s consumer
  but none for `constraint_history`/`snapshot_type`/`degradation_chain`). So it does not touch
  live classification today, but the fix is a member of the OQ-44 fail-closed-vs-impute class
  (decide once for the class), not a free per-site hardening. (extractiveness→0.5 elsewhere:
  required-authored at the static path; tracked under OQ-44. OQ-44 has since been resolved —
  class policy ruled fail-closed-on-absence, operator 2026-06-11.)
- **Row 26 analysis-path `0.5` cluster — MEASURED NEUTRAL (`outputs/tripwire_row26_results.json`).**
  Direct branch-reachability tripwire (patch `0.5→999.9`, count constraints that emit 999.9):
  `purity_scoring:57`, `drl_boltzmann_analysis:135`, `:154` all **default_fired=0/194** (branch
  unreachable); `:302` neutral (guard `reformability_score` proven total by bogus-constraint positive
  control); `drl_fpn:197` is LIVE-COSMETIC at most (dynamic cache, fires only when `dr_type` fails in
  precompute, feeds FPN contamination EP only — never `dr_type`). **No row-26 site is a classification
  trap.** NB: the guard-falsity *count* shortcut was caught **vacuous** by its positive control —
  `cross_index_coupling`/`reformability_score` succeed even for a bogus constraint, so "0 absent" did
  not prove "branch unreached"; the 999.9 tripwire is the sound test. Row 27 by-design.
- **Row 26 — COVERAGE CORRECTION (VERIFY-OR-CORRECT pass, 2026-05-31).** The witness
  `outputs/tripwire_row26_results.json` and the script's `ROW26_SITES`
  (`python/sweeps/tripwire_fabricated_defaults.py`) actually cover **only 3 rows / 2 of the 6
  site-names** the handoff attributes to this sweep: `purity_scoring:factorization_subscore`,
  `drl_boltzmann_analysis:coupling_factor`, `drl_boltzmann_analysis:excess_extraction_factor` — all
  re-run **NEUTRAL** (default_fired=0/194). **`covering_analysis:486`, `gap_diagnostic:120`,
  `omega1_audit:102` (BaseEps), and the `drl_fpn:197` LIVE-COSMETIC verdict are ABSENT from both the
  artifact and `ROW26_SITES`** — NOT witnessed here. So **"no second classification-changing
  fabricated default beyond row 23" is perturb-confirmed for the 3 covered sites and OPEN for the
  other 4.** *Mechanism sharpening:* 2 of the 3 covered sites (`factorization_subscore`,
  `coupling_factor`) are NEUTRAL because their guard `cross_index_coupling/2` is **total** (succeeds
  with score `0.0` for any atom, incl. two bogus atoms tested) → the `; …= 0.5` else-branch is **dead
  code**, not live-but-corpus-empty; only `excess_extraction_factor`'s default branch is reachable
  (synthetic no-data constraint → `0.5`, positive-controlled). **First write-pass item to close
  this:** expand `ROW26_SITES` to the full 6 sites and re-run — flagged, not done (out of this audit's
  read/transcription scope).

**Decision per remaining site:** fail-closed vs keep impute. Connects to the empty-table
satisfy-on-absence pattern (`get_metric_average:169`).

## OQ-42 — Documentation correction: `affects_constraint` is NOT empty in testsets_3000

**Status:** resolved — (2026-06-04, ledger close — correction already in substrate). The correction
lives in KNOWN_STATE.md (2026-05-31 "Empty-table pattern scoped" entry: "`affects_constraint` is
NOT empty — a populated network edge"), and the original wrong sentence ("empty across all of
testsets_3000") no longer appears in KNOWN_STATE.md or CLAUDE.md (grep witnessed). For the record:
the census grep found **9305 emitted facts** in the archive (520 live); the 2026-05-31 note had
conflated `affects_constraint/2` with the genuinely-empty `intent_*` tables (OQ-36).
## OQ-43 — Satisfy-on-absence gate class: the NL beneficiary gate is the fourth instance

**Ω-type:** Ω_C (design choice — one fail-closed-vs-vacuous-pass policy across the class; generalized by OQ-44).

**Status:** resolved — the class policy was made once at OQ-44 (operator ruling 2026-06-11; see
that entry's ruling block). **Fifth-instance disposition recorded here: FAIL-CLOSED, output-changing,
un-certification accepted.** `has_viable_alternatives/2` default `false`→`unknown` (commit
`8b5a34b8`): the empty `intent_viable_alternative/3` table (GAP-08) no longer satisfies
`natural_law_signature`'s `HasAlternatives == false`, so `thermal_dissipation_constraint` lost its
NL certification (signature → `ambiguous`; the NL→mountain modal override dropped, surfacing
rope at moderate/institutional and a `perspectival_incoherence` red verdict). NL certifications
return when the intent layer or an authored alternatives table exists. Witnesses:
`audits/2026-06-11_oq44_policy_close/`.

**Origin (compressed; full history in git).** Named the class — gates satisfied by data-absence
reading as positive findings — across four then five instances: G6 fabricated defaults (OQ-41),
empty `intent_*` consumers (OQ-36/OQ-37), `get_metric_average` 0.5, the NL `BeneficiaryCount == 0`
gate over empty `intent_power_change` (re-sourced to `agent_beneficiary`, D3 fail-close), and
`has_viable_alternatives` (above). Kernel_v1-regime evidence (404 NL certs = "no beneficiary
authored," not "none exists") and the FSM-is-the-only-beneficiary-screen correction are preserved
in git history. **Still-operative pointer:** auditing whether any NL constraint hides a real winner
is a content re-audit — OQ-45, not engine maintenance.

## OQ-44 — Engine-wide audit: no gate may be satisfied by absence (authored-zero vs absent)

**Ω-type:** Ω_C (design choice — the engine-wide fail-closed-on-absence policy; decided once for OQ-41/36/37/43).

**Status:** resolved — operator ruling 2026-06-11; the ruling block below is still-operative
(compress-on-close exception). Audit evidence: `audits/2026-06-11_oq46_backed_reconciliation/`
(probe1 gate census, live corpus); dispositions witnessed: `audits/2026-06-11_oq44_policy_close/`.

**POLICY RULING (operator, 2026-06-11) — extracted from converged practice, the clock as
confirmation only.** Five independent conversions went fail-closed on real data and none was
reverted: `count_power_beneficiaries` re-source (D3), `get_metric_average` → `unknown` sentinel
(2026-06-08), `classify_at_time` row-23 fail-close + bucketed Backed (OQ-41/OQ-46),
`verify_vector_at` witness-carrying pass (OQ-96), the piton vacuity line (OQ-37). The common-law
phase converged; the instance-counter condition from adjudication instance #1 (gate-type +
report-type spanned, next instance arrived 2026-06-10) is satisfied, but it is the *confirmation*
the jurisprudence ran its course — not the ground of the ruling. The ruling:

- **Statute for new or modified gates:** a gate over a possibly-empty table or a defaulted metric
  **fails closed on absence** — establish the datum was authored before the gate may pass;
  defaults-on-empty return `unknown`/OPEN, never a plausible value; the pass carries its witness
  (joined-table sizes, coverage). This is Build Discipline Pattern 5 made binding at build time.
- **Absence-to-provenance carve-out (the `suppression_profile` precedent, OQ-46):** absence may be
  converted into legitimate authored provenance **only by positive-control inference at
  authoring/compile time** — where the artifact's other authored content proves the omission
  deliberate (the compiler stamps `suppression_profile(C, static)` because other series WERE
  authored) — **never by emptiness-inference at the consumption site.** A marker-less absence at
  read time always fails closed.
- **Common-law for existing gates:** ruled per-instance with their witness, prioritized by
  success-shapedness of the vacuous pass (a green checkmark or certification outranks an
  annotation). CARRY-with-provenance remains an available per-instance verdict (instance #1's two
  CARRYs were CLOSED 2026-06-11 by the OQ-93 migration — see the closure note below; full
  text in git history at this entry).

**Dispositions of the three open sites (2026-06-11, all witnessed in
`audits/2026-06-11_oq44_policy_close/`):**
1. **`get_raw_suppression` `Supp=0` → CONVERTED** (commit `966d53c8`): `unknown` sentinel +
   `number/1` guard clause at `classify_from_metrics/6`. The witness corrected the pre-derivation:
   the fabricated 0 WAS consumed — the two non-story `cs_axiom_contradiction` files exported
   `suppression: 0` and each carried a `fingerprint_voids` agreement computed on it; both now
   honest (null / agreement absent). Classification output unchanged for all 46 stories;
   `shared/schemas.py` marks suppression nullable (null = no authored scalar).
2. **`report_generator`/`utils` 0.0 defaults → CONFORMING as-is**, no change: they print
   `MISSING (using default 0.0)` — the pass already carries its witness. (Instance #2's
   conditional-verdict caveat stands; sibling sites :481/:500 same status.)
3. **`has_viable_alternatives` → FAIL-CLOSED** (ruled; commit `8b5a34b8`, output-changing, landed
   alone): default `false`→`unknown`; the empty `intent_viable_alternative` table no longer
   satisfies NL's `HasAlternatives == false`. **Un-certification of
   `thermal_dissipation_constraint` ACCEPTED** — a known-vacuous certification is tolerated rather
   than undetected; NL certs return when the intent layer or an authored alternatives table exists
   (GAP-08). Witnessed consequence: signature → `ambiguous`, the NL→mountain modal override
   dropped (rope at moderate/institutional), diagnostic verdict green→red with a
   `perspectival_incoherence` alert; all 277 pipeline diffs attribute to this single cause
   (thermal's own fields + corpus-relative wasserstein/arakelov/signature_pressure aggregates).

**Class members in the DR-trajectory (observer) subsystem — DORMANT/LOCKED, not live
(witnessed 2026-06-08; merged from the `sdm-temporal-records` review).** Flagged here so the
once-for-class ruling covers them, NOT so they are fixed per-site: (i) `drl_composition.pl:201`
`BaseX = 0.5` — reachable-but-locked (OQ-41 rows 24–25; fires only via the dormant
`constraint_history` sweep); (ii) `transition_paths.pl` `snapshot_type/3` fabricated
`default_extractiveness=0.10` / `default_suppression=0.10` (`config.pl:300–301`) — entirely
inside the dormant trajectory classifier (`snapshot_type`/`degradation_chain` consumed by
nothing, positive-controlled). These are the same fail-closed-vs-impute decision as the live
sites; reviving the surface that would make them live is the coupled rebuild ruling (KNOWN_STATE
2026-06-08 "three deferred temporal threads = one ruling"; since adjudicated in part — OQ-83
resolved 2026-06-11, D-fork ruled NO-OPEN at OQ-110; revive-or-gap of the dormant trajectory
classifier is OQ-91's candidate-home question).

**Origin (compressed; full audit scaffolding, gate-shape taxonomy, instance #1/#2 texts in git
history).** Premise: zero-because-measured and zero-because-missing collapse to one value at a
comparison site, so a gate that cannot tell them apart tests nothing when its table is empty.
The 2026-06-11 evidence pass (probe1 census with per-process positive controls) found most of the
named class already fail-closed by accretion and the remainder enumerated as the three sites
above. Pattern-5/Pattern-6 candidate-site census across the wider engine remains OQ-97's scope.
Connects to OQ-43 (class naming, resolved with this ruling), OQ-41, OQ-36/OQ-37, OQ-45 (content
re-audit, still open), OQ-93 (the CARRY instances' gate), OQ-97.

**OQ-93 fork ruled + migration landed (2026-06-11): the two CARRY instances are CLOSED** —
`verify_vector_at` reports absence OPEN per slot-group (pass carries its witness), the
`[INTENT]` line prints OPEN on absence with authored-or-absent inputs (`coercion_grid`), and
the dead intent top verdict moved to OQ-106. Witnesses:
`audits/2026-06-11_oq93_grid_migration/`.

## OQ-45 — Content audit: do any of the 404 NL constraints hide asymmetric winners?

**Status:** open — (corpus-quality audit, NOT engine maintenance). Spun off from the D3 ruling so the
**Priority:** 1
wiring fix (NL beneficiary gate fail-close, Commit B1) and the content question stay separate. The
gate fail-close makes "no beneficiary authored" honestly-conditional rather than a vacuous pass; it
does **not** decide whether any of the 404 natural-law certifications are *mis-authored* false-naturals
with a real winner hidden behind an emergence claim. Populating `intent_power_change` faithfully for a
genuine natural law yields 0 beneficiaries / 0 flips (OQ-43), so this audit only bites mis-authoring.
Audit the 404 on their own merits; do **not** populate `intent_*` as maintenance. Connects to OQ-43.

## OQ-46 — D4-for-suppression is a GENERATION-TEMPLATE requirement; it retires the row-23 stopgap

**Status:** resolved — operator ruling 2026-06-11: **the scalar fallback is SANCTIONED, not a stopgap; the premise was wrong.** Full evidence + ruling: `audits/2026-06-11_oq46_close/`.

**Why the premise was wrong:** the generation prompt has instructed since 2026-05-30 (commit
`220739b8`, predating the live corpus): "Do NOT author `suppression_requirement` measurements
unless the story's narrative specifically tracks enforcement-capacity change"
(`constraint_story_generation_prompt_json.md:457`); the schema requires neither `measurements`
nor suppression among them. The 7 scalar-only live constraints are prompt-conformant
static-enforcement stories, not template failures — the wait-state never terminates by design.
Second unrecorded load: 21 of 47 fallback rows were time-grid misalignment inside 10
series-authoring constraints (suppression sampled coarser than other metrics), so universal
series-authoring alone would not have retired the clause. Deletion counterfactual: 16/46
timelines change, 7 collapse to `[unknown]`, 9 gain phantom transitions in `drift_trajectory`.

**Still-operative ruling:** `classify_at_time`'s read ladder — temporal `measurement/5` at T →
authored scalar `constraint_metric` as constant → fail-closed `unknown` — is the **permanent,
sanctioned** representation policy for suppression (D4 ruled: scalar = static enforcement is
legitimate authoring; the series is authored only when enforcement capacity changes). Do not
delete the scalar clause. A future Surface-3 temporal-suppression primitive is gated on
per-constraint `Backed` coverage, not on corpus-wide series universality.

**Backed semantics BUCKETED (same-day follow-on ruling, 2026-06-11; evidence:
`audits/2026-06-11_oq46_backed_reconciliation/`).** The close above left `Backed=false` on ALL
scalar-supplied rows; the follow-on evidence pass split them and the operator ruled bucketed,
keyed on an EXPLICIT sanction, never emptiness-inference (OQ-44 Pattern 5):
- **Sanctioned static scalar backs:** `suppression_profile(C, static)` — compiler-stamped
  (`generate_constraint_pl.py` section 8) only on positive-control absence (other series
  authored, suppression deliberately omitted) — plus no series anywhere → `SuppBacked=true`.
  The scalar IS the story's authoring for every Time. Zero flip/fab_adjacent delta witnessed
  (59/20 unchanged; only `backed_times` rises, 7 constraints × 4 contexts).
- **Misalignment substitution stays excluded** (series exists, off-grid Time): `SuppBacked=false`.
  Spun off as **OQ-105** — the substitution is anti-causal (scalar ≈ series endpoint, 37/39
  exact) and currently sets flip TIMING in 2 witnessed timelines.
- **Unmarked seriesless fails closed** (`SuppBacked=false`): a missing series without the
  marker (partial regen, generation bug) is excluded, not silently sanctioned.
- **Blanket rejected with witness:** counting all scalars as backed graduates the 20
  `fab_adjacent` transitions into the real-flip count (59→79) that decides the OQ-83 D-fork —
  laundering substitution-dated motion.
The earlier "do not add a scalar/temporal equivalence check — nothing to reconcile by
definition" rationale is RETIRED in favor of a witnessed basis: a one-time endpoint-membership
query (2026-06-11) found 0 violations (39 dual-rep constraints: 37 exact scalar==endpoint,
2 within 0.05), so the lint question is closed-no-demonstrated-content — no standing lint;
re-run the query (`audits/2026-06-11_oq46_backed_reconciliation/`) before re-opening.

## OQ-47 — Audit the SCOPE→seed seam BEFORE the de-stamp regeneration batch

**Status:** resolved — superseded by the 2026-06-05 de-leak + corpus reset; the confirmed example-file leak path is removed The behavior-preserving prompt/schema
changes (D5 row-14 strip, D7 schema-gate strip) only take effect on **regenerated** stories. Sequencing:
- **De-stamp A/B is the FIRST regeneration batch** (the D7 schema-gate strip + row-14 removal, run as
  an A/B against current stories) and it **gates batch two** — do not bulk-regenerate until the A/B
  confirms the prompt fixes land as intended.
- **Audit the SCOPE→seed seam first.** UKE_SCOPE writes the seed/manifest (`uke_scope.*`, census row 5);
  if SCOPE re-injects the stripped gate language or fields downstream, it **silently undoes the prompt
  fix** — the upstream leak. Verify the seam carries no stripped-field provenance before regenerating.
Connects to OQ-46 (the regen that retires the row-23 stopgap) and D7/D5 (the prompt changes regen propagates).

**Path-resolution audit (2026-05-31) — the leak is the EXAMPLE file, not SCOPE.** Both generators
(`agent/generate_kernel_corpus.py`, `agent/c-orchestrator.py`) import path constants from
`agent/story_generator_base.py`. Findings:
- **Schema: no regen-path fork.** Every schema resolution — `SCHEMA_PATH` (prompt-text + `load_schema`
  validation), `generate_constraint_pl._load_schema` (kernel strip/validate), c-orchestrator's
  `load_schema` — resolves to the canonical `python/constraint_story_schema.json` (env `DR_SCHEMA`
  overrides). **B4 lands in both pipelines.** `agent/data/constraint_story_schema.json` is referenced
  ONLY by `commitment_corpus/apply_schema_patch.py` (orphan w.r.t. generation), not a regen fork.
- **Prompt: canonical.** Generation prompt → `prompts/constraint_story_generation_prompt_json.md`
  (env `DR_GEN_PROMPT`); SCOPE prompt → `prompts/uke_scope_v2_json.md`. **B3 lands in both.** The SCOPE
  prompt does NOT carry the stripped gate fields (only an unrelated "net-zero resistance" phrase).
- **CONFIRMED LEAK — the few-shot example file.** `c-orchestrator` injects `json/antifragility.json`
  as its example, which hard-codes **`accessibility_collapse: 0.9` and `resistance: 0.08`** — the exact
  mountain-gate pattern B4 stripped from the schema. The model will pattern-match and reproduce it,
  **silently undoing B3+B4** for the c-orchestrator path. (The kernel pipeline's example,
  `agent/verification_bottleneck.json`, is clean — 0 occurrences.) The two generators also use
  DIFFERENT example files (divergence). **The de-stamp A/B MUST scrub `json/antifragility.json`'s
  accessibility_collapse/resistance exemplar values, not just the prompt+schema** — else the leak
  re-injects the stripped gate. This is the upstream leak this OQ anticipated; it is the example file.

---

**Resolution (2026-06-05):** the seam audit this OQ demanded ran as the de-leak's assembled-payload witnesses (prompt, schema-in-prompt, example all grepped at the build_prompt boundary). The confirmed leak — `json/antifragility.json` (accessibility_collapse 0.9 / resistance 0.08) injected by c-orchestrator — turned out to be a DANGLING LOAD by 2026-06-05 (`protocols["example"]`/`["gen_prompt"]` loaded, never consumed; generation uses story_generator_base.build_prompt with the clean verification_bottleneck example). Dangling entries deleted with a do-not-rewire comment. The de-stamp A/B regeneration arc itself is mooted by the corpus reset: the live corpus is born under the de-leaked pipeline.

## OQ-48 — Classification thresholds never recalibrated against the live (post-rebuild) corpus

**Ω-type:** Ω_E (design-relevant — closeable by recalibration; deferred by policy until the corpus is worth calibrating against).

**Status:** open — (table-setting for the rebuild). The χ / ε / suppression classification
**Priority:** 1
thresholds (`config.pl` §5B + §5) are documented as **"Calibrated: Derived from 691-constraint
corpus analysis (2024–2026)"** (`logic_thresholds.md:15`), with the known limitation that the
calibration corpus is **Western/WEIRD** and needs non-WEIRD validation (`logic_thresholds.md:58–59`,
the §Known Issues block). That 691-constraint corpus **predates the rebuild** — the live `testsets/`
is now ~194–226 readings (a single coherent run; see Critical Distinctions). **The thresholds were
never recalibrated against the live corpus.**

**Why this is logged now (OQ-37 Move 1, 2026-06-01).** Move 1 closed the (0.35,0.40) χ-partition
gap *structurally* (corpus-independent: 0 transitions, commit `3ab3ace4`). It deliberately did
**not** touch `tangled_rope_suppression_floor` (0.40) or `tangled_rope_epsilon_floor` (0.30) — those
are **calibrated values**, and moving them is recalibration, not a structural cleanup. They are the
**binding gates on the four surfaced `unknown` holdouts**:
- `republican_reading`, `living_constitutionalist_reading`: blocked solely by **supp 0.35 < 0.40**
  (χ and ε in the tangled band).
- `diversity_reading`, `competence_reading`: blocked by **both ε 0.28 < 0.30 and supp 0.35 < 0.40**.

So `tangled_rope_suppression_floor` (0.40) and `tangled_rope_epsilon_floor` (0.30) are **deferred
recalibration targets, not structural constants.** Whether the four holdouts *should* classify as
tangled_rope is a calibration question answerable only against a corpus we trust — i.e. after the
rebuild, not by nudging a floor to rescue four readings.

**What resolution requires:** a recalibration pass that re-derives the χ/ε/supp thresholds (and the
σ/π modifiers, same provenance) against the live corpus — ideally with a non-WEIRD extension — and
records the new values + the corpus snapshot (pipeline manifest) they were fit to. Until then, treat
every classification-gate numeric as **691-corpus-provenanced**, and do not move a *calibrated* floor
to change a specific reading's type (that is curve-fitting to the holdout, not calibration). Move 1's
χ-floor edit is exempt: it is a partition-geometry fix, value-independent.

**Cross-refs:** OQ-37 (the holdouts + the surfacing override); OQ-30 (coupling-threshold calibration
likewise rests on the live sweep, the external ±25% harness withdrawn). The pipeline manifest
convention (CLAUDE.md) is the mechanism for citing *which* corpus a future recalibration was fit to.

---

## OQ-49 — Signature-override prevalence at 3000-scale: 1730 confident-overwrites, 0 unknown-fills (laundering vs load-bearing escalation)

**Ω-type:** Ω_C (design choice — is the override layer laundering or correction; FNL→tangled_rope correction RESOLVED v6.13.2). The coupling-score-validity sub-question is Ω_P (constitutively open).

**Status:** resolved — SPLIT-CLOSE on the live corpora (2026-06-14, `audits/2026-06-14_oq49_remeasure/`).
The (a)/(b) escalation dissolves: the dominant FNL→tangled_rope effect collapsed by construction
(OQ-70) and the residual is two-seat signal handed to OQ-74. Read-only; no clause removed or edited.

---

### RE-MEASURE / SPLIT-CLOSE (2026-06-14, live corpora — supersedes the testsets_3000 analysis below)

The original (a)/(b) ruling was un-answerable as posed: the substrate is gone twice over — (1)
`testsets_3000` (`original_v6`) is a dead corpus (reset 2026-06-05); (2) the FNL driver was deleted
(OQ-70, commit `72ec2cdd` — removed `claimed_natural`'s bait source that read any single authored
mountain *perspective* as a story-level *claim*, 827/1106 pre-reset firings). So OQ-49 closes by
**re-measuring on live** (`testsets` 57, `testsets_haiku` 960, `testsets_flash` 960 — twins are exact
base-name mirrors, a matched paired design) and dispositioning under OQ-74's seat frame — **not** by
ruling clauses. Probe: `python/audits/oq49_override_remeasure.py` (read-only; MT=`metric_based_type_indexed/3`,
FT=`dr_type/3`, Sig=`constraint_signature/2`; override-effective iff MT≠FT; positive controls
`PC_CLAUSE878`/`PC_SOURCE1`/`PC_LIVECHANGE` passed in every process).

**Live results (manifest-matched 57/960/960):**

| corpus | FNL firings | FNL src split | FNL override-effective | total override-effective |
|---|---|---|---|---|
| testsets | 1 | source1:1 | 0 | 17 |
| testsets_haiku | 13 | source1:13 | 6 | 168 |
| testsets_flash | 8 | source1:8 | 8 | 181 |

1. **Obsolete-prevalence half — CLOSED on source-attribution (the witness is structural, not numeric).**
   **Every FNL firing on all three corpora tags source-1** (`constraint_claim(_,mountain)`); zero
   source-2, zero unaccounted. **Kill condition** = any surviving FNL firing tagged *neither* (a
   third, unaccounted path) — **not triggered**. The 827 bait-driven firings are gone *by
   construction* (the bait path is unreachable), corpus size irrelevant. The raw `1661 → ≤8` drop is
   size-confounded (3380 vs 960) and is **color, not the witness.** Cite `72ec2cdd` + the reset.
2. **Inert clauses — CLOSED working-as-designed.** `natural_law → mountain` (`signature_detection.pl:867`)
   fires 0 (no `natural_law` signature on live); `unknown, false_natural_law → unknown` (`:877`) fires
   0 (every live FNL firing has a definite metric, never unknown). Witnessed by the live tables.
   FNL is no longer the override layer's dominant effect — override-effective (eff=1):
   `false_ci_rope → tangled_rope` = 6/56/78 and `coupling_invariant_rope → rope` = 3/18/34, distinct
   signatures, vs `false_natural_law → tangled_rope` = 0/6/8 (FCR→TR leads FNL→TR ~10× on the twins).
3. **Residual — DISPOSITIONED, not ruled.** The full FNL override-effective union is **0/6/8 = 14**
   across both twins (snare→TR 0/4/4 — the pre-reset dominant case — plus scaffold→TR 0/2/4). Coord
   crossed over **all 14**, not just the snare→TR 8: **14/14 carry both coordination AND asymmetric
   extraction** (`fnl_survivor_coordination.txt`). **Positive control:** the coord=0 arm fires on the
   corpus (haiku 18 `no_coord`, 119 `no_asym`) → all-14-coord+asym is a real finding, not a stuck-true
   predicate. Under the pre-reset partition (coord=0 = clean laundering candidate; coord+asym =
   override-supplies-omitted-structure, correction-leaning) the **clean-laundering coord=0
   pure-extraction subset is EMPTY on live, both twins** — the human-stakes escalation **dissolves to
   zero**. The 14 coord+asym readings are two-seat signal (the snare/scaffold clause is
   coordination-blind; FNL→TR supplies the omitted coordination-awareness) → handed to **OQ-74**, not
   ruled here.
4. **Twin paired check.** Model-invariance (real second witness): FNL collapse holds on **both**
   twins. Generator-convention signal (analogue, NOT a seat-frame witness): per-id override-effective
   sets diverge — **81 shared, 87 haiku-only, 100 flash-only** — two models authoring the same id
   differently (OQ-26/OQ-78-analog), logged as such.
5. **Hand-off.** Any witness-not-verdict engine change is **OQ-74's** gated pass (the cyclopean
   cluster OQ-50/116/122), not OQ-49's. No clause removed or edited; no engine write performed.

*Cross-refs: OQ-70 (bait deletion, `72ec2cdd`), OQ-74 (two-seat / witness-not-verdict), the
cyclopean cluster (OQ-50/116/122). Full evidence: `audits/2026-06-14_oq49_remeasure/`.*

---

### Archived analysis — testsets_3000 (dead corpus; pre-OQ-70; retained for provenance)

**Corpus:** `testsets_3000` (3380 readings; `[corpus] Loaded 3380 testsets successfully`). Replaces
the toy-derived 96/56/40 — that did not carry; prevalence re-established at scale.

**Method.** For every reading: `Sig = constraint_signature/2`, `MT = metric_based_type_indexed/3`
(override suppressed = raw metric read), `FT = dr_type/3` (override active). Override is
*effective* iff MT≠FT. Mechanical split of effective overrides: **confident-overwrite**
(MT≠unknown, override overwrote a definite band) = laundering-candidate; **unknown-fill**
(MT=unknown, override supplied a verdict) = load-bearing-candidate.

**Positive control (mandatory — "0 unknown-fill" could be a dead probe).** Re-ran the test in a
worktree at `c90c5482^` (=2f7dc3fa, before the unknown-surfacing guard), testsets_3000: the test
detected **19 `unknown → tangled_rope`** override changes (11 false_natural_law + 8 false_ci_rope).
So the test *does* register unknown-fill changes → the current engine's "0 unknown-fill" is a real
finding, not a blind probe. **The control also exposes a definitional flaw:** those 19 were
`unknown→tangled_rope`, which the mechanical "unknown-fill = load-bearing" rule mislabels — but they
were *ruled laundering* (honest-unknown fabrication, OQ-37). c90c5482 already surfaces them as
`unknown` (override no-op). So **unknown-fill is NOT automatically load-bearing**; an honest-unknown
filled by the override is laundering, a genuinely-failed-unknown filled is load-bearing — that
sub-split is itself the (a)/(b) ruling.

**Prevalence (current engine, testsets_3000):**
- Every reading carries a signature (none=0): false_natural_law 2014, false_ci_rope 932,
  natural_law 404, false_summit_mountain 15, constructed_high_extraction 10, coupling_invariant_rope 5.
- **Override changed the verdict: 1730.** No-change (over-included / confirmatory): 1650. Honest
  unknowns surfaced (unknown→unknown, c90c5482 no-op): 19.
- **Of the 1730: confident-overwrite (laundering-candidate) = 1730 (100%); unknown-fill
  (load-bearing) = 0.** The unknown-fill clauses (:778, :786–:790) fired zero type-changes. The
  `natural_law → mountain` clause (:738) is purely confirmatory (404 readings, metric already
  mountain, 0 changes).
- **Delta from toy:** toy 96/56/40 (signature-locked / changed-type / over-included). At scale the
  "56 load-bearing" does NOT carry — under the unknown-fill definition, **load-bearing = 0**; all
  override-effective changes are confident-overwrites.

**Per-clause laundering-candidate table (all confident-overwrite; OPEN, awaiting (a)/(b) ruling):**

| clause | signature → output | metric→final | N | sample readings |
|---|---|---|---|---|
| :749 | false_natural_law → tangled_rope | snare→TR | **1641** | a_level_grading_inflation, academic_journal_peer_review_gatekeeping |
| :760 | false_ci_rope → tangled_rope | scaffold→TR | 46 | ai_as_fourth_node, artificial_scarcity_scaffold |
| :772 | false_summit_mountain → tangled_rope | mountain→TR | 15 | capability_velocity_mismatch, clinical_authority_topology |
| :749 | false_natural_law → tangled_rope | scaffold→TR | 10 | |
| :749 | false_natural_law → tangled_rope | rope→TR | 7 | antikythera_knowledge_loss |
| :753 | coupling_invariant_rope → rope | scaffold→rope | 5 | guinea_worm_eradication, open_source_commons |
| :760 | false_ci_rope → tangled_rope | snare→TR | 3 | |
| :749 | false_natural_law → tangled_rope | naturalized→TR | 2 | |
| :749 | false_natural_law → tangled_rope | piton→TR | 1 | |

Grouped: **:749 false_natural_law→tangled_rope = 1661** (the override layer's dominant effect);
:760 false_ci_rope→tangled_rope = 49; :772 false_summit_mountain→tangled_rope = 15;
:753 coupling_invariant_rope→rope = 5.

**The (a)/(b) ruling (human-stakes — CER does not self-rule):** for each clause, is the displaced
metric read RIGHT and the override corrupts it (laundering → remove), or WRONG and the override is
a correction the metric layer can't make (load-bearing → keep)?
- **:749 (1661, dominant) — PARTITIONED 2026-06-01 (snare→TR sub-bucket, 1641; perturb-confirmed
  testsets_3000).** *Correction of an earlier draft of this bullet, which claimed these readings
  "failed the tangled_rope gate — lacked coordination/asymmetric-extraction." That was wrong;
  witnessed below.*
  - **Triple axis (the requested earned-vs-default partition) is DEGENERATE.** All **1641/1641**
    cleared the FULL snare triple (χ≥0.66 ∧ ε≥0.46 ∧ supp≥0.60; witnessed bx=be=bs=1 for every one).
    `metric=snare` ⟺ triple cleared (the snare clause requires it and runs *before* tangled_rope), so
    the **DEFAULT-SNARE class is structurally empty (0)** and the "fail-TR-gates-only → default-snare"
    positive-control arm is unsatisfiable. On this axis the computed result is REAL-SNARE=1641,
    load-bearing=0 — but the axis cannot separate, so it is not the discriminating one.
  - **Structure axis (the discriminating one).** The snare clause is **coordination-blind** (it never
    checks coordination-function or asymmetric-extraction). Cross of the 1641:
    | structure | χ≤0.90 (TR band) | χ>0.90 | total |
    |---|---|---|---|
    | coord+asym (both TR markers present) | 1391 | 207 | **1598** |
    | coord=0 (no coordination) | 11 | 32 | **43** |
    So only **43** are coordination-free pure-extraction snares (the clean laundering candidates: pure
    extraction forced to the less-extractive hybrid). **1598** carry both tangled_rope structural
    markers and were labeled snare *only* by cascade precedence + coordination-blindness — for these,
    FNL→tangled_rope supplies the coordination-awareness the snare clause omits (correction-leaning),
    though 207 of them have χ>0.90 (forced below their extraction band). Samples: coord=0 →
    `ad_synaptic_deficit` (χ0.93), `awareness_without_leverage` (χ0.71); coord+asym →
    `a_level_grading_inflation` (χ0.79), `academic_journal_peer_review_gatekeeping` (χ0.74).
  - **Escalated (do NOT auto-resolve):** the user's earned/default axis yields 1641/0, but the
    coordination-blindness of the snare clause means "earned pure-extraction snare" is true for only
    43; the 1598 are coordination-bearing readings the metric mislabeled. The (a)/(b) ruling therefore
    splits: **43 coord=0 = clean laundering candidates; 1598 coord+asym = override-supplies-omitted-
    structure (correction-leaning); 207 of those χ>0.90 are the boundary cases** (TR structure but
    extraction above TR's ceiling). Ruling is yours per sub-population. *Tier: perturb-confirmed.*
  - **Predicate identity independently run-verified (2026-06-01).** The firing clause IS
    `false_natural_law` (`signature_detection.pl:749`), input metric = `snare`, output =
    `tangled_rope`; 1730/1641 reproduced on testsets_3000. The audit's attribution stands
    (ruling (a), not (b)): `naturalized` is unreachable from FNL by construction
    (`resolve_modal_signature_conflict(mountain, false_natural_law) = tangled_rope`); it is the
    metric cascade `classify_from_metrics:388` (ε>rope ceiling, χ<TR floor) that produces
    `naturalized`. This corrected the paper, not the audit: v6.13.1→v6.13.2 errata fixes
    §4.2/§4.4's mountain→naturalized mis-description (commit 6ca6ca2b). *Tier: run-witnessed +
    blob-witnessed.*
- **:772 (15).** mountain→tangled_rope on "mountain with beneficiary." Plausibly load-bearing (a
  natural-mountain read that missed an authored beneficiary), but the mountain read was confident →
  needs ruling.
- **:760 (49), :753 (5).** scaffold→{tangled_rope, rope}. e.g. guinea_worm_eradication,
  open_source_commons → rope (genuine-coordination certification) reads plausibly load-bearing;
  scaffold→tangled_rope reads need a look.

**What resolution changes:** if :749 is ruled (a) laundering, the dominant behavior of the entire
signature-override layer (1661/1730 effective overrides) is corrupting confident snare reads —
a major engine change (separate gated commit, manual approval). If (b), it is the layer's core
correctness function and stays. **No clause removed without ruling.**

**Cross-refs:** OQ-37 (unknown-surfacing, c90c5482), OQ-30 (FNL coupling lock), OQ-43 (the 404 NL
certifications — here confirmed as override-no-ops: natural_law→mountain changed 0 types).

**Phase 0 side-finding (naturalized decouple).** The naturalized set is NON-empty on testsets_3000
(3 readings: antikythera_knowledge_loss, gig_economy_worker_protections, normalization_error_propagation;
all ε>0.45, χ<0.24) — correcting the prior toy "empty/inert" record: the shared
`tangled_rope_chi_floor` ↔ naturalized-ceiling coupling is **LIVE**. But Move 1 (0.40→0.35) shifted
**zero** of them (no reading has ε>0.45 ∧ χ∈[0.35,0.40); same 3 at both floors). Disposition: the
coupling is real but was not exercised by Move 1 — decouple moves from "deferred (looked inert)" to
"**latent-hazard, required-before any OQ-48 recalibration that moves the χ-floor across the naturalized
χ-range**." Not decoupled this session.

## OQ-50 — False-summit forensic: explainer/detector coherence + sibling-clause bound-Context latent trap (post-repair follow-ups)

**Ω-type:** OPEN-1 is Ω_C (design choice — what "false summit" explains against). OPEN-2 is an
engine-hardening question, not a design Omega.

**Status:** resolved — both follow-ups closed 2026-06-14 (OPEN-1 + OPEN-2; see RESOLUTION below).
The core detector bug was RESOLVED earlier — see KNOWN_STATE.md
2026-06-02 "False-summit forensic detector repaired." Summary of what was fixed (do not re-open):
`drl_core.pl:548` `type_1_false_summit` used `is_mountain(C, Context, fail)`, which matches the
unconditional catch-all clause `is_mountain(_,_,fail)` (`:123`) and never ran a test; with the cut it
returned the first mountain-claimer with `Context` unbound. Now negates `dr_type/3` over
`standard_context` (no cut). `report_generator.pl:445` queried the never-produced atom
`type_1_false_mountain` (→ always "all validated"); `:447` counted (C,Context) pairs as constraints.
All three fixed. Live result: 4 false summits (papal, press, statutory, total_war) across 14
observer-context instances; 4 genuine mountains correctly excluded.

**OPEN-1 — explainer disagrees with the (now-correct) detector.** `forensic_explain_false_mountain`
(`report_generator.pl:459+`) re-derives its verdict from raw `suppression_requirement` /
`base_extractiveness` heuristics (suppression-vs-mountain-ceiling), **independent of `dr_type`**. It
printed FORENSIC VERDICT "AMBIGUOUS (review data)" for `papal_temporal_authority_mountain` even
though the detector flagged it correctly because `dr_type=scaffold ≠ mountain` at the
moderate/institutional contexts. The detection layer and the explanation layer now use **different
notions of "is a mountain"** (post-signature `dr_type` vs pre-signature raw metrics), so the
explanation can contradict a correct flag. **What resolution changes:** rebase the explainer on
`dr_type` (report the actual per-context `ActualType` and why it departs from the claim), or
explicitly state the two-layer split in the output. Until then, treat the explainer's verdict as a
**metric-level annotation, not the detector's reason**. This is the same metric-vs-`dr_type` seam
the detector fix turned on.

**OPEN-2 — sibling claim-mismatch clauses share the bound-Context latent trap.**
`type_3_snare_as_rope` (`drl_core.pl:555`, `is_snare(C, Context, snare)`) and `type_5_piton_as_snare`
(`:562`, `is_piton(C, Context, piton)`) require `Context` **bound** — clause 1 of `is_snare`/`is_piton`
computes Chi via `extractiveness_for_agent(C, Context, _)`. Unlike the old `type_1`, they are **not**
vacuous: they ask for the positive type atom, so clause-head unification selects the real-test clause
1, not the `(_,_,fail)` catch-all. But if either is ever called with `Context` unbound (as the report
called `type_1` via `dr_mismatch/4` with an unbound `Ctx`), they would silently **no-op / mis-bind**
rather than enumerate. Currently both are only reached with bound Context (via `dr_mismatch/3` →
`default_context`, or `genuine_findings_query.pl:157` with C bound and Ctx2 collected). **What
resolution changes:** either give them the same `standard_context(Context)` enumeration `type_1` now
has (so they locate the break per context and survive unbound-Ctx callers), or assert a bound-Context
precondition. Decide alongside whether claim-mismatch detection is uniformly per-context.

**RESOLUTION (2026-06-14, both follow-ups closed — engineering, no design ruling needed):**
- **OPEN-1 CLOSED — explainer rebased on `dr_type`** (`report_generator.pl` `forensic_explain_false_mountain/2`,
  commit on `oq122-oq50-oq74`). The FORENSIC VERDICT now headlines the post-signature `dr_type`
  ActualType (the detector's own notion), and the suppression/extractiveness heuristic is relabeled a
  non-headline **METRIC-LEVEL ANNOTATION** — took BOTH prongs of the fork (rebase + keep annotation),
  so it removed a detector/explainer disagreement rather than adjudicating a live design choice (the
  detector was already rebased on `dr_type` at the OQ-50 core repair; the explainer is downstream
  coherence, evidence-settled). Fail-closed no-solution guard prints `dr_type: unbound` rather than
  going silent; `dr_type/3` is **total** over the audit's reached set (0/44 reached (C,Ctx) pairs lack
  a solution; returns `unknown` even on a malformed context), so the guard is defense-in-depth — the
  comment forbids calcifying totality into an invariant. Witness: `actinide_…_flat_control` AMBIGUOUS →
  `dr_type` tangled_rope/scaffold across its seats.
- **OPEN-2 CLOSED — type_3/type_5 given the per-context enumeration** (`drl_core.pl:622,629`, same
  commit cluster). Both now lead with `standard_context(Context)` and drop the trailing cut (matching
  type_1 — answers the design micro-decision: **yes, uniformly per-context**). The bound-Context trap
  is gone: an unbound-Ctx call previously mis-bound type_3 to a single non-standard context
  (`time_horizon(immediate)`); it now returns the per-seat list (live: 1 phantom-context solution → 4
  standard-seat solutions; type_5 0→0, no theater≥0.70 claimers). Multiplicity falsifier cleared by a
  full caller census — every `dr_mismatch`/`dr_claim_mismatch` caller collects (setof/findall) or is
  `\+`-negation: `report_generator.pl:72/480/520`, `genuine_findings_query.pl:157`,
  `diagnostic_summary.pl:665`, `abductive_triggers.pl:296`; no first-solution/per-solution-side-effect
  caller. The `/3` legacy path (Context=default_context == analytical seat) stays single-solution.
  Regression: `test_contradiction_signatures` 5 fail/12 pass identical to baseline; `validation_suite` 57/0.

**Out of OQ-50 scope (do NOT fold in):** the mid-power-mountain→`rope` power-scaling phenomenon
(`drl_core.pl:605-613`) is **not** an OQ-50 item — it is a separate design-laden Ω_C minted as
**OQ-128** and cross-referenced from OQ-122. Do not write "the OQ-50 power-scaling fix" anywhere; no
such settled artifact exists.

**Cross-refs:** the original `type_1` defect is a **Pattern-5 sibling** — a gate satisfied by
*absence of a real test* via clause-head unification (the `(_,_,fail)` catch-all), distinct from
OQ-44's empty-table satisfy-on-absence but the same spine ("absence presents as presence"). Connects
to OQ-44 (engine-wide no-gate-satisfied-by-absence), build_discipline Pattern 5. Item 3 of the
2026-06-02 orbit-work recon (false-mountain orbit) depends on this detector being correct.

---

## OQ-51 — Discrete (H1) vs continuous (W1) obstruction disagree on 58 constraints: what does the off-diagonal mean?

**Ω-type:** Ω_C (design choice — whether/how to reconcile two obstruction measures the framework
calls complements).

**Status:** resolved — (both off-diagonal cells probed and explained, 2026-06-02). 2×2 at n=563:
430 / **36** (H1=0∧W1>0) / **22** (H1>0∧W1≈0) / 76. **The 22 = `unknown`-driven** — MaxEnt
distributions byte-identical across all 4 seats while `dr_type` returns `unknown` at ≥1 seat;
`count_disagreeing_pairs` counts the abstention as a disagreeing type (absence-as-presence), so
these manifest classifications have zero distributional support. **The 36 = chain-conditional
MaxEnt movement under a constant type label** — real for ~6 constraints, threshold-dust (W1<0.06)
for ~30. Root cause: **two classifiers, not one** (H1 reads `dr_type`, W1 reads MaxEnt); the
"continuous complement" framing (`json_report.pl:389`) is inexact. Evidence:
`outputs/w1_sheaf_join.{json,md}` (n=563, commit b5ccee0); probe narratives in git history.

**RULED 2026-06-02 (design decision, human) — decided, NOT yet built (output-changing task):**
`unknown` is N/A, not a disagreeing type. `count_disagreeing_pairs` counts only real-type pairs;
**<2 real seats ⇒ H1=N/A (never 0)** — else an all-unknown constraint reads `genuine_sheaf`
(Pattern 5). `sheaf_status/2` gains a 4th value (`undetermined`/`insufficient_data`); H1 emits
`null`, not `0`; `contextuality_fraction` = H1/comparable-pairs. Projected impact (n=771): 741
unaffected, 26 manifest→genuine/fragile, 5 →N/A. ~30 consumers + paper figures move together
under output-changing discipline, positive control = all-unknown constraint reaches
`undetermined`. **Second absence route (2026-06-04):** the fragile clause reads
`arakelov_height/2`, which fails on unauthored ε ⇒ falls through to `genuine_sheaf` by absence —
the 4th-value build must also fail-N/A on uncomputable height, not only on RealSeats < 2.
(`diagnostic.arakelov_threshold` now emitted for run-level provenance.)

**Standing gates:** W1 is chain-conditional only (off-chain mass invisible — join
`wasserstein_incomparable_mass` to see it) and corpus-relative (~100× swings between n=563 and
n=771, an OQ-26 instance) — **never rank/select on inherited W1**; recompute on the analysis
snapshot with a real threshold (≥~0.05), and state W1/H1 as readings of two different
classifiers. Related: OQ-37 (do not re-suppress `unknown`), OQ-52 (false-mountain cross gated by
the W1 rules above).
*(Investigation narratives compressed 2026-06-04 per footer rule; full probes in git history.)*
## OQ-52 — False-mountain cross: do the naturalized→snare manifest rows have an authored beneficiary?

**Ω-type:** Ω_C (design choice — what the false-mountain / false-summit read is *for*, and whether the
beneficiary channel is the right disqualifier). Related: OQ-43 (NL certifications mean "no beneficiary
*authored*," not "none exists"), OQ-50 (false-summit detector repaired but post-fix firings unwitnessed
at scale).

**Status:** partial — core question resolved (provisional, type-vector based, 2026-06-02); W1-magnitude ranking
**Priority:** 1
deferred behind the OQ-51 W1 rebuild. **Result:** of the 98 `manifest_presheaf`, **16 are
false-mountain-shaped** (13 strict: powerless=`naturalized` ∧ analytical=`snare`; 3 loose:
`naturalized`+`snare` both present), and **all 16 carry both an authored beneficiary AND an authored
victim** — 0 observer-relative / no-disqualifier cases. So the naturalized→snare reading is **authored
extraction** that reads as natural to the powerless seat and surfaces as `snare` only at the analytical
seat (corrective-grade, not commentary-grade). Canonical signature: 7 of the 13 strict share the
identical vector `[naturalized, tangled_rope, rope, snare]` at H1=6 — a monotone climb up the power
axis. None of the 16 contain `unknown`, so this set is **disjoint from the OQ-51 unknown-artifact**.
This rests only on (a) the four-seat type vectors (confirmed **100% corpus-stable**, 563/563 identical
n=563→771) and (b) authored `constraint_beneficiary/victim` — **W1 was not used**, so the result does
not inherit W1's corpus-ephemerality.

**Origin:** W1 × sheaf_status join, 2026-06-02; the flagged forward step from that build. Tool to
extend: `python/w1_sheaf_join.py` (join `sheaf_status × beneficiary/victim`).
**Files:** `prolog/sheaf_analysis.pl:54-63` (`sheaf_status/2`); the disqualifier channel is
`narrative_ontology:constraint_beneficiary/2` + `constraint_victim/2` (authored, **populated** 552/504
of 563; emitted via findall in `json_report.pl:302-312` as `[]` when none authored) — **note: this is
NOT `intent_power_change`**, the empty channel of OQ-43; that conflation is resolved here; false-summit
detector (`drl_core.pl:548`, repaired — see KNOWN_STATE.md 2026-06-02).

**Specific question:** Among the high-W1 `manifest_presheaf` rows, a distinct shift-vector signature
recurs: **`naturalized` at the powerless seat collapsing to `snare` at the analytical seat** — e.g.
`quran_9_5_scope__abrogating_universal` ([naturalized, snare, naturalized, snare], W1=1.20, H1=4),
`article_9_war_renunciation__strict_pacifist_reading` ([naturalized, tangled_rope, snare, snare],
W1=1.13, H1=5), `abrahamic_covenant__isaac_covenant_reading` ([naturalized, tangled_rope, rope, snare],
W1=1.02, H1=6), `july_charter_sovereign_legitimacy__guided_nationalism_reading`, `doomsday_clock_metric__objective_index_reading`.
This is the **false-mountain orbit**: a constraint that *reads as natural* (`naturalized`) to the
powerless seat but is *seen as extraction* (`snare`) by the analytical seat. The question: **do these
constraints carry an authored beneficiary/victim** — i.e., is the naturalized→snare gap backed by an
authored extraction structure, or is it an observer-relative reading with no authored disqualifier?

**Evidence (provisional read done 2026-06-02):** selection on the stable type vectors
(`outputs/orbit_data.json`, confirmed 563/563 identical n=563→771) × `sheaf_status` × authored
`constraint_beneficiary/victim` from `pipeline_output.json`. Fail-closed check satisfied: the channel
is genuinely authored (552/563 have ≥1 beneficiary, 504/563 ≥1 victim; `[]` = none authored, no
fabricated default), so "all 16 authored" is a real finding, not pass-by-absence (Build Discipline
Pattern 5). The 16: 13 strict + 3 loose (full list with H1 / vector / beneficiary+victim counts is in
the session probe; reproducible by the selection above).

**What resolution changes — ANSWERED (corrective-grade):** all 16 false-mountain-shaped manifest rows
have authored beneficiaries *and* victims → the naturalized→snare signature is **authored extraction
visible only to the analytical seat**, not observer-relative naturalization. No commentary-grade
(no-disqualifier) cases exist in the current corpus. The provisional read is complete on the stable
inputs.

**Deferred (Option-1, before any W1-magnitude claim):** the *magnitude* of each false-mountain
fracture (how hard it cleaves) needs the OQ-51 W1 rebuild — snapshot-pinned, `incomparable_mass`
joined, real threshold — because inherited W1 is chain-conditional and corpus-ephemeral. The H1
ranking (H1=6 strongest; the 7 `[naturalized,tangled_rope,rope,snare]` monotone-climb rows) is the
**stable** ranking to use until then. The original "high-W1" selection criterion is **dropped** in
favor of the type-vector signature, which is corpus-stable.
**Sequencing satisfied:** ran on the repaired false-summit detector (OQ-50 core fixed); selects by
type-vector shape (stable) not W1; fail-closed check passed. OQ-51 framing is now settled (both corners
explained), so the prior "do not scope until OQ-51 settled" gate is lifted.

---

## OQ-53 — Observer and reading axes are conflated in the classifier (no kernel-fixed reading comparison)

**Ω-type:** Ω_C (design choice — is the kernel/reading axis first-class?).

**Status:** open
**Priority:** 1
**Deps:** blocked_on OQ-56
**Origin:** Kernel/reading review, 2026-06-02 (first kernel/reading corpus landed this day).
**Files:** `cs_kernel_registry.pl`, `logical_fingerprint.pl`, `json_report.pl`; readings as
`kernel__reading_name` sibling files.

**Specific question:** Should "hold a kernel fixed and compare its readings as a set" — and its
transpose, "hold a reading-stance fixed and sweep across kernels" — be a first-class engine operation,
distinct from the observer (power×scope) pipeline that currently classifies each reading independently?

**Evidence so far:** Manifest `ae10e7e` (run 2026-06-02T18:54:31Z, n=772): 542 readings under 183
multi-reading kernels; 231 standalone constraints. Readings differentiate within a kernel (ε in 176/183,
victim-set in 183/183, `claimed_type` in 113/183) but the kernel is only an implicit string prefix and
the two axes are not distinguished by the classifier. Pairs **GAP-04**.

**What resolution changes:** If first-class, the kernel becomes a queryable object with enumerable
readings and the transpose query becomes available — but it is gated on a reading-stance vocabulary
(OQ-56). If not, the kernel/reading structure stays a naming convention over independent constraints.

---

## OQ-54 — The reading axis has no cross-index gluing test; how to operationalize it (and keep it distinct from observer Boltzmann)

**Ω-type:** Ω_C (design — define the reading-axis site and obstruction).

**Status:** mitigated — structural obstruction built (`cs_kernel_obstruction/4`); residue is OQ-58.
**Origin:** Kernel/reading review, 2026-06-02. Build + decision recorded same day.
**Files:** `cs_kernel_registry.pl` (`cs_kernel_obstruction/4`, `cs_kernel_obstruction_status/2`,
`cs_kernel_obstruction_report/0`); KNOWN_STATE 2026-06-02.

**Resolution (2026-06-02):** Decided **establish** (build), not operationalize, with two of three legs
already done. (1) **Orthogonality is discharged** by Theorem 7 / detection-independence
(`test_forecloses_fpn_injection.pl` branch E, gradient-orthogonality Δ=0) — the obstruction is built
observer-blind (reads only `cs_reading_relation`, never χ/`live_index`), so the discharge holds and the
none/compliant cross-tab and `live_index` are NOT prerequisites. (2) **Inputs existed** (the committer
edges). (3) **The obstruction was absent** and is now built: a committer-axis analog of the observer H¹
over the reading cover, counting foreclosing reading-pairs (`real_closure`) vs coexisting
(`licensed_plurality`) vs none (`untyped`, fail-closed on absence). **The open sub-question is answered:
distinct axis from OQ-51's observer H¹/W1, not the same object.** Build surfaced and fixed a data defect
(86 name-form-mismatched edges under-counting closure across this probe + `cs_corpus_analysis` +
`json_report`); repaired in-place with a predicted-delta control (`real_closure` 84→94, 10 named kernels);
generator canonicalization added (`generate_constraint_pl.py`); dangling residue → **OQ-58**. Remaining
for full closure: per-kernel JSON field + the **OQ-55** trifurcation router (the real consumer).

**Original question (for provenance):** The only cross-index test, `boltzmann_compliant/2`, factorizes
classification across Power×Scope — the **observer** axis only. What is the analogous test on the
**reading** axis ("do a kernel's readings glue into a global section, or are they irreducibly plural?"),
and how should it relate to the existing H¹/W1 obstruction (OQ-51) and to seat-orthogonality?

**Evidence so far:** Corrected framing (Seat Theorem §4 Coupling Theorem): an index-invariant verdict is
seat-free/contentless, so Boltzmann invariance is a *partial test for Mountain-ness* and non-compliance =
the verdict is **seated** on the observer index — not a pathology. The reading axis needs its own gluing
test; Seat Theorem §3's correction (two seats can be mutually non-representable — detection-independence,
Theorem 7) means a reading-axis obstruction may be gradient-orthogonal to the observer one and **must not
be reduced to it**. `w1_sheaf_join`/`sheaf_status` is the candidate home but is not yet wired to the
kernel/reading grouping as a gluing test. Pairs **GAP-05**.

**What resolution changes:** Defines whether reading-plurality is measured by reusing the W1/H¹ obstruction
on a reading-indexed site or by a new construction; sets the input to the OQ-55 trifurcation router. Open
sub-question: is the reading-axis obstruction the same object as OQ-51's discrete/continuous obstruction,
or a third axis?

---

## OQ-55 — Reading-disagreement is not trifurcated (Type A/B/C router absent)

**Ω-type:** Ω_C (design — adopt the debugging-philosophy trifurcation as the disagreement router).

**Status:** open
**Priority:** 1
**Deps:** blocked_on OQ-56
**Origin:** Kernel/reading review, 2026-06-02.
**Files:** `sheaf_status`/`h1_band`, `cs_pattern`, `cs_axiom_foreclosed`, `drift_events.pl`;
spec in `docs/debugging_philosophy.md` §6.

**Specific question:** When two readings of a kernel disagree, can the engine classify *why* per the
three-stage diagnostic: **Type C** (index ambiguity — different declared seats, the correct case for
genuine plurality → specify index, do not collapse), **Type A** (frame drift — criterion slides within
one seat → frame-fix), **Type B** (structural — the kernel's commitments are inconsistent → fracture)?

**Evidence so far:** The raw diagnostics exist (`sheaf_status`, `cs_pattern`, `cs_axiom_foreclosed`,
`drift_events`) but no predicate maps a kernel's reading-set onto {ambiguity, drift, structure}. Natural
wiring: OQ-54's reading-axis gluing test feeds it (H¹≠0 with each reading internally coherent ⇒ Type C;
internal incoherence ⇒ Type B; same-seat criterion drift ⇒ Type A). Pairs **GAP-06**.

**What resolution changes:** Turns "the readings disagree" from an undifferentiated fact into a routed
verdict that says whether the disagreement is a specification choice (most plural kernels), a fixable
drift, or a genuine fracture — the operational core of the kernel/reading engine.

---

## OQ-56 — Reading-stance taxonomy: the selection-seat is blocked on cross-kernel clustering

**Ω-type:** Ω_P (preference/stakeholder — a declared, contestable selection premise, not derivable).

**Status:** open
**Priority:** 1
**Origin:** Kernel/reading review, 2026-06-02.
**Files:** readings (`kernel__reading_name`), `cs_kernel_registry.pl`.

**Specific question:** The transpose query (OQ-53) and any cross-kernel comparison need a reading-stance
vocabulary *comparable across kernels* (e.g. every kernel's naturalizing vs. coordination vs.
power-revealing reading). What is that vocabulary?

**Evidence so far:** Readings currently have free-form names (`isaac_covenant_reading`,
`abolitionist_reading`) that do not align across kernels. Per Seat Theorem Cor 2b, the selection rule for
*which* seats matter is a **declared, contestable premise, not a theorem** — so this is a human-ruled
choice (Ω_P), not one the engine can derive. Critically, choosing it well requires first seeing **how
readings actually cluster across kernels**, and that analysis was impossible until a kernel/reading corpus
existed (landed 2026-06-02). Pairs **GAP-04** (step 2).

**What resolution changes:** Unblocks OQ-53's transpose and OQ-55's cross-kernel reading comparison.
**Sequencing:** the cross-kernel clustering analysis is the prerequisite step and is now possible; it
should run before the taxonomy is declared, and the taxonomy is the user's to rule, not the engine's.

---

## OQ-57 — Drift report throws on a constraint missing `requires_active_enforcement/1`

**Ω-type:** Ω_E (empirical — data-completeness defect; run the corpus, find the gap).

**Status:** resolved — 2026-06-04 — wrong-module qualifier fixed at `drift_events.pl:230`
(`narrative_ontology:` → `domain_priors:`). Witnesses: before = direct call throws
`existence_error` (both working tree and HEAD worktree); after = kodashim fires
`evidence(extraction,0.08,theater,0.85)`; corpus-wide emitter set derived pre-edit then
confirmed = exactly {kodashim_obligation__memorial_archival,
statutory_debt_ceiling__constitutional_nullity_reading}; `run_dynamic_suite` completes
(0 [FAIL] / Errors: 0); pipeline re-run diff = 0 per_constraint rows.

**Resolution finding — the bug had TWO behaviors, one per load path (keep):** suite/REPL path
threw (no definition under `narrative_ontology:`); the PIPELINE path was **correct by
accident** — `json_report.pl` is a non-module file, its `use_module(drl_core)` imports into
`user`, and modules inherit from `user`, so the wrong-qualified call silently resolved to
drl_core's bridge. General tripwire: **a wrong-module-qualified call can throw on one load path
and silently resolve through user-inheritance on another whenever any non-module file imports
the predicate's home module — REPL behavior is not pipeline behavior.** Diagnose on the
consumer's exact `-l` chain (see `docs/technical/swipl_load_path_and_probe_gotchas.md`).
*(Body compressed 2026-06-04 per footer rule; investigation history in git.)*

**Re-witnessed 2026-06-10 (post-reset; original witnesses were pre-reset/corpus-specific).** The
2026-06-04 behavioral witnesses ran on the corpus reset 2026-06-05; re-verified across live +
archives. (a) Fix durable in source (`drift_events.pl:236`). (b) **Diagnostic positive control:**
the PRE-FIX qualifier `narrative_ontology:requires_active_enforcement/1` still throws
`existence_error`; the FIXED `domain_priors:` qualifier resolves — qualifier change is load-bearing,
probe not vacuously clean. (c) **Original emitter set reproduced exactly** on `kernel_v1` (1,106):
`{kodashim_obligation__memorial_archival, statutory_debt_ceiling__constitutional_nullity_reading}`
both fire CLEAN, `kodashim` → `evidence(extraction,0.08,theater,0.85)` byte-identical to the record
above. (d) **Corpus-independent synthetic positive control** proves the clause fires when reached
regardless of corpus content (`evidence(extraction,0.05,theater,0.85)`, no throw). (e) Full
`drift_event/3` scan threw on none of 4,525 constraints across live(39)+kernel_v1(1,106)+
original_v6(3,380); `run_dynamic_suite` live = 39 pass / 0 fail / 0 err. **Caveat carried:** the
`internalized_piton` clause is currently UNREACHED on the live 39-constraint corpus
(correct-but-dormant) — a future "no drift throw on live" read must not be mistaken for "exercised."
Witnesses: `audits/2026-06-10_oq57_live_rewitness/` (FINDINGS.md + raw probe outputs).

---

## OQ-58 — Dangling cs_reading_relation targets: edges naming a reading that does not exist

**Ω-type:** Ω_P (content decision). **Disposition policy ruled 2026-06-02 (below); the per-edge
missing-vs-typo-vs-noise sort is a later narrative-read pass, not a mechanical rule.**

**Status:** partial — policy ruled; narrative-read pass run on the 13 forecloses (6 repaired); the residue is a
**Priority:** 1
**kernel-completeness** problem, not edge-patching — see the finding below.
**Origin:** Reading-axis obstruction build (OQ-54), 2026-06-02. Surfaced by the independent
must-flag oracle when partitioning non-canonical `cs_reading_relation` targets.
**Files:** authored testset `.pl` files; emitter `python/generate_constraint_pl.py:482`;
quarantine view `cs_kernel_registry:cs_reading_relation_unresolved/4`; linter/reporter
`python/audits/reading_reference_linter.py`.

**Cross-ref (2026-06-02):** the *network-layer* analog — dangling `affects_constraint/2` targets
(1710/2548 edges, no referential-integrity guard), the 9 delimiter typos there, and the finding that
such danglers resolve into a **bounded** structural-class space (not an open frontier) — is tracked in
**GAP-07** (`docs/design/design_gaps.md`). Its structural-resolver basis is gated behind repairing
those 9 typos first.

**FINDING (2026-06-02, linter-as-reporter) — it's "complete kernels," not "patch edges."** A
reference-census linter (`reading_reference_linter.py`; census of `cs_reading_relation` +
`affects_constraint`; three rules each with a synthetic positive control) gives the
**incompleteness rate**: **143 dangling committer edges → 119 distinct missing readings across 69
kernels** (37 missing >1; e.g. `scriptural_authority_contest`, `soteriological_kernel_contest` each
missing 4). The dangling edges are the *symptom*; the disease is that ~69 contested kernels were
decomposed (into N readings named in their own commentary) but only some readings were authored. The
6 GENERATE edges found by the narrative read are a small visible slice — the unit of work is **kernel
completion**, with the quarantine/linter as the backlog spec. Two reference *kinds* must stay
separate: `cs_reading_relation` targets MUST be sibling readings (integrity applies → the rate above);
`affects_constraint` targets are causal-network nodes that may be abstract (1680 "danglings" — NOT a
clean integrity signal, excluded from the rate). Also: 4 forecloses edges repaired this pass
(genesis/magna_carta/fair_use/npt → existing readings, commentary-cited; `real_closure` 95→98); 6
more non-canonical (delimiter-typo) `cs_reading_relation` refs in coexists/influences are repairable
to existing readings (R2).

**Specific question:** After the name-form normalization (short → `<kernel>__<short>`) repaired 86
edges, **99 `cs_reading_relation` edges remain dangling** — their target resolves to no reading in the
kernel even normalized: forecloses **13**, coexists_with **59**, influences **27**. Two sub-classes:
(a) a full-form target `<kernel>__<name>` whose `<name>` reading was never generated (e.g.
`all_men_created_equal__originalist_reading` — the kernel has the textualist reading but not the
originalist one); (b) a mistyped target (e.g. `magna_carta_universal_rights_reading` missing the
`_1215`, `john_1_1_logos_orthodox_christological` with a single `_`). What should happen to each?

**Why it is NOT folded into the name-form sweep:** "fix" here is a *content* decision with no default —
authoring the missing reading is invention, deleting the edge destroys authored commitment structure,
flagging the kernel defers. The normalization pass deliberately left these untouched (witnessed: dangling
counts unchanged 13/59/27 before and after the 86-edge repair) so the sweep did not silently rule a
question that is the user's.

**Disposition policy (ruled 2026-06-02).** The two sub-classes are real and the right seam —
ontology-incompleteness (target conceptually intended, node not yet generated) vs data-malformation
(typo against an existing node) — and conflating them is the actual loss (it hides whether to grow the
reading inventory or clean the edge corpus). But the sub-class is the **output of a review with the
kernel's source narrative in hand, not an automated classification** — because the witness that
distinguishes "missing reading the author meant" from "typo into a well-formed string never meant" is
in the *narrative*, not in the edge. So there are exactly **two automated outcomes**:

1. **canonical → attach** — target resolves to a declared reading in the same kernel.
2. **everything else → quarantine** — surfaced for reviewed disposition, never auto-written.

What was **rejected**, and why (both are a green-check standing in for a witness):
- **No auto-rewrite tier.** "High-confidence unique match → rewrite the edge" writes to authored
  commitment structure on an uncalibrated confidence threshold, on a corpus whose whole problem is
  inconsistent authoring. One wrong rewrite silently edits what a reading *forecloses* — the content
  the committer axis exists to preserve. This contradicts the fail-closed default chosen everywhere
  else (untyped-on-absence, dirty-edges-fail-loudly, the no-resolver tripwire). Even "obvious" typo
  repairs go through an explicit reviewed batch — every edit to authored structure has a human seat.
- **No plausible-form tier.** "No match but `<kernel>__<name>` is structurally plausible → record a
  missing-reading defect" resolves a content ambiguity on a *syntactic* tell (well-formed ≠ intended)
  and does so toward *inventing* a reading. A plausible-but-absent target is **unresolved → quarantine**,
  same bucket as noise, flagged for the narrative check — not pre-classified as a generation gap.

The sub-class sort (missing-reading-defect vs typo vs noise) is the **review's** output, made on the
narrative; the policy's automated job is only canonical-vs-quarantine.

**Generator shape set by the policy (this is the flow side).** The referential-integrity check is
**hard-fail**: a generated `cs_reading_relation` target must resolve to a declared reading in the same
kernel, with **no plausible-form escape hatch** (a well-formed `<kernel>__<name>` that names no declared
reading fails, same as a typo). Its output channel for failures is the **quarantine bucket**, so the
check and the bucket are one mechanism. The name-form canonicalization already in
`generate_constraint_pl.py:482` is a *normalize* step and is **not** the referential-integrity
guarantee — it has no manifest at the per-constraint emit site, so the hard-fail belongs at the
kernel-level pass in `agent/generate_kernel_corpus.py` where the declared-reading set is known.

**Stock quarantine bucket (queryable now):** `cs_kernel_registry:cs_reading_relation_unresolved/4` —
the review queue of existing unresolved edges (the 99). It is a derived view (no new storage); the
obstruction stays fail-closed on these (does not invent a gluing status for an absent referent) and
this predicate makes them *loud* for the narrative-read pass.

**What resolution changes:** The per-edge narrative-read pass (later) empties the quarantine bucket by
the policy above — each unresolved edge → {generate the missing reading | repair to an existing reading
| delete the edge}, decided on the source narrative. Closing it reclassifies the OQ-54 `untyped`/
absent-referent residue.

**SCOPED BACKLOG + RANKING (2026-06-02, linter witness-split).** The 119 missing readings split by
in-degree (distinct source readings per missing target): **in-degree≥2 (commentary + independent edges)
= 21 readings / 20 kernels — the DEFENSIBLE backlog**; in-degree==1 = 98 / 49 (the long tail, below).
The 20 defensible kernels are ranked by incompleteness (missing / intended); **first batch =
`marriage_authority` (40%: 3 generated, 2 missing — `federalist_millet_reading` ε≈0.25 Rope,
`judicial_harmonization_reading` ε≈0.40 Scaffold; both spec'd in its own decomposition statement, line
323 "decomposed into five")**. Then a 33% band (1 of 3: rome_statute, constitutional_text, basic_law,
cultural_property, us_constitution_1787, second_amendment_boundary, …). **Method (user-set):** author
the most-incomplete kernel first, **re-measure the obstruction before authoring all 21** — a completed
kernel should move from artificial under-count toward its true structure; verify it moves sensibly
before generating 21 nodes on faith. **Exclude `westphalian` from the generate-backlog** — its "1
missing" is the OQ-59 `absolutist_reading` alias (rename/edge fix, not a generate).

**ROOT CAUSE FOUND + FIXED AT SOURCE (2026-06-02).** The dangling edges / incompleteness are an artifact
of a **declared-vs-queued cap**: the SCOPE manifest's `commitment_system_recognition.readings` declares
the full reading set (with siblings), but `generation_sequence` is capped (axes=3, an orchestrator-era
limit) and queues only a subset; `generate_kernel_corpus.py:flatten_manifests` seeded from the *queue*,
so declared readings were silently dropped — their siblings' edges then dangled. Witnessed in
`outputs/decompose/manifests/indian_personal_law_pluralism.manifest.json` (marriage_authority): `readings`
declared **5**, `generation_sequence` queued **3**; the 2 dropped are exactly `federalist_millet_reading`
+ `judicial_harmonization_reading`. **Fix (corpus script, not the orchestrator):** `flatten_manifests`
now also emits a seed for every *declared* reading not already queued (bare reading_id, deduped) — so
completion is automatic and **cap-independent**, fixing existing manifests (their `readings` already name
the dropped siblings) and future ones in one place. Witnessed: flatten on the marriage_authority manifest
now emits **5/5** seeds, 0 duplicates.

**AUTHORITATIVE BACKLOG (manifest source, supersedes the dangling-edge proxy):** scanning all 595
manifests for `len(readings) > len(generation_sequence)` gives **49 declared-but-dropped readings across
32 contested kernels** — the provably-intended generate-backlog (cleaner than the 21/119 dangling-edge
estimate, which mixed in typos/noise and kernels lacking manifests). The fixed flatten produces exactly
these 49 seeds. (3 of the 32 carry `kernel_id=None` — a separate SCOPE data quirk the fix correctly
skips.) Generation of the seeds is still the LLM step (Sonnet/API) — the fix only ensures the seeds are
*complete*; the obstruction re-measure gate (author marriage_authority first, verify sensible movement)
is unchanged.

**FIRST BATCH DONE + GATE PASSED (2026-06-02).** marriage_authority's 2 dropped readings generated via
Sonnet batch (`msgbatch_01YBozr…`, scoped to 2; commit `df3ebb63`). Obstruction moved sensibly: kernel
3→5 readings, its dangling edges 6→0, `real_closure` H1r=4 (true structure), corpus quarantine 87→81,
new readings author CANONICAL edges. Quality flags noted (one type-deviation Scaffold→tangled_rope; lint
warnings) for a later regen-polish pass.

**BACKLOG CURATED (2026-06-02) — the manifest count needed cleaning, not all 49 are generate targets.**
New seed-builder `agent/build_completion_seeds.py` (scans manifests, enriches dropped-reading seeds with
existing siblings' `json/` context). Curation passes: (1) **partial kernels only** (`existing>0`) — 28
kernels / 35 readings; the broader manifest scan returns 132/344 because it also catches **103
scoped-but-never-generated kernels** that are NOT completion targets (near-duplicate / superseded /
exploratory decompositions — e.g. `state_killing_legitimacy`/`_authority`/`_execution_authority`). (2)
**∩ live dangling edges** (manifest-declared AND actually referenced by the live corpus) → 30/25; drops
the stale `westphalian__gradated_reading` (renamed → `governance_quality_reading` last turn — the
manifest is stale). (3) **Group 1 = 18 unambiguous readings / 16 kernels** (generated, background-polled).

**CORRECTION (2026-06-03) — the "near-duplicate kernel pairs" were NOT duplicates; I misdiagnosed from
filename + shared-reading-name similarity (the unfalsified-diagnostic trap).** The kernel_id keys the
reading, so a shared reading *name* means different things per kernel. Verified by reading the content:
`basic_law_interpretive_boundary` (where the boundary between branches *sits*, ε=0.38) vs
`_interpretive_authority` (who holds *final* authority, ε=0.52); `geneva_conventions_protective_scope`
(the **whole** framework, scaling across AP I / AP II / Common Art. 3) vs `common_article_3_scope` (**just**
Common Article 3's reach); `constitutional_interpretive_authority` (who decides) vs `constitutional_text`
(how the text reads). All **distinct** — complete them all (**group 2 = 11 readings / 8 kernels**). The
**only** genuine deferral is `westphalian__absolutist_reading` — a *within-kernel* alias
(absolutist ≈ absolute_sovereignty, same kernel) → OQ-59 edge-repair, not a generate. **The same caution
applies to the 103 never-generated kernels:** "near-named ⇒ duplicate" is the same trap — they need
*content* examination before any prune, not a filename verdict. Stale-manifest tripwire stands: a
rename/merge must update the SCOPE manifest too (the ∩-live-edges pass caught `gradated_reading`).

**The commentary-only tail (98 readings / 49 kernels) is parked behind the defensible set — as the
LINTER'S STANDING JOB, not "ignore."** `python/audits/reading_reference_linter.py` is the recurring
referential-integrity + incompleteness check: run it as the corpus grows to (a) catch new dangling /
non-canonical refs at the source, and (b) keep the in-degree split current so newly-corroborated tail
items graduate into the defensible backlog. *(Candidate: wire it as a non-gating reporting step in
`run_pipeline.py`.)*

---

## OQ-59 — Within-kernel reading duplication corrupts the obstruction cover (westphalian instance)

**Ω-type:** Ω_E (empirical — detectable by scanning reading-name stems per kernel; a content/merge
decision once sized).

**Status:** disposed — **(2026-06-03): preserve-and-diff, not merge** — all four follow-ons done.
The user ruled against merging near-doctrine readings (averaging two readings is the cyclopean
move; the disparity *is* the depth); near-named readings are usually DISTINCT positions
(gradated=governance-values vs graduated=state-capacity; nws/nnws; homoousios/homoiousios), so
the linter's R3 is a review-trigger, never a verdict. Rename for de-confusion where needed
(`gradated_reading` → `governance_quality_reading`, witnessed clean), never auto-merge.

**Disposition record (#1–#4, all witnessed 2026-06-03):**
- **#1 edge-repair:** 4 dangling `absolutist_reading` edges retargeted → `absolute_sovereignty`
  (alias ruled); R1 dangling 89→87, affects_constraint 1668→1666.
- **#2 operator:** `prolog/reading_diff.pl` + `report_pair/3` (authored-cells-only,
  kernel-agnostic, order-independent stability verdict). See KNOWN_STATE.md 2026-06-03 +
  `tests/test_reading_diff.pl`.
- **#3 census:** `prolog/reading_diff_census.pl`, 615 within-kernel pairs → 53.7% key_fragile /
  39.5% robustly_binocular / 6.8% undersampled — the alignment seat governs the verdict
  corpus-wide. Results: `audits/2026-06-03_reading_diff_census/`.
- **#4 axiom diff:** `prolog/axiom_diff.pl` — 0 of 935 reading-pairs share an axiom NAME;
  alignment requires a DECLARED `axiom_concept/2` map (empty by default, never baked); demo
  found a grounding INVERSION on the westphalian absolute pair. `tests/test_axiom_diff.pl`.

**Residue (content pass, not tracked here):** prose at `governance_quality_reading.pl:226`
names the old alias AND mis-characterizes it vs `absolute_sovereignty`'s authored cells.
**Cross-refs:** westphalia vs westphalian = distinct sibling kernels (user ruling), so those
pairs are cross-kernel probes; OQ-58 repair progress that was logged here (2 delimiter typos;
5 naming-variant edges, 16→8 dangling after never-generated generation; quarantine
`prolog/cs_reading_relation_quarantine.json`) is OQ-58's record — see OQ-58 and GAP-07.
*(Body compressed 2026-06-04 per footer rule; sizing/comparison narratives in git history.)*

---

## OQ-60 — Latent absence-reward in the purity scalar: zero-evidence constraint scores pristine 1.0

**Ω-type:** Ω_E (empirical — mechanism witnessed by synthetic probe; zero current corpus victims).

**Status:** open — (log-only by ruling — do NOT fix in auto; see "What resolution changes")
**Priority:** 1
**Origin:** Purity audit 2026-06-03 (`audits/2026-06-03_purity/purity_audit_20260603.md` §3, K5).
**Files:** `purity_scoring.pl:62-71` (scope_invariance_subscore), `boltzmann_compliance.pl`
(`scope_invariance_test` — `variant([])` case; `compute_cross_index_coupling` — `GridSize < 2 ->
CouplingScore = 0.0`), `purity_scoring.pl:41-50` (final clamp).

**Specific question:** A constraint holding *nothing but* the 3 classification facts needed to pass
`epistemic_access_check` scores `purity_score = 1.0 → pristine` (witnessed,
`audits/2026-06-03_purity/census_control.tsv`). Three stacked mechanisms: (1)
`scope_invariance_test` returns `variant([])` when `classify_at_context` fails at every test scope,
and the SI formula `1-(N-1)·0.25` yields **1.25** — out of range, then concealed by the final
`min(1.0,…)` clamp; (2) `cross_index_coupling` is total — "not enough data points" returns coupling
0.0 → F = 1.0 (perfect factorization for absence), which also makes the documented F = 0.5 neutral
default **unreachable** (the historical "default_fired 0/194" at ISSUES.md:1631 and this audit's
fresh 0/1106 are vacuous truths, not data-completeness facts); (3) CC/EX default clean on missing
data. Should absence fail-closed (sentinel/no-data subscore) rather than scoring as perfection?

**Evidence so far:** Corpus census 0/1106 `variant_0`, 0/1106 SI>1.0 — **latent, no current
victim**: every live constraint classifies at the test grid. Positive control: a nonexistent ID
gets `has_coupling=1, F=1.0`. Related authored-zero-vs-absent instances: OQ-43/OQ-44
(Pattern 5); `boltzmann_floor_for` clause 3 silently uses `boltzmann_floor_default=0.05` when
`coordination_type` is absent (spec logic_extensions.md:746 says sentinel) — how many corpus
constraints hit the default floor is uncensused.

**What resolution changes:** Fixing changes the no-data fixed point for constraints that are not
currently misbehaving — the latent-path-fix risk — so the fix is held for a deliberate pass, not
auto. Resolution would: treat `variant([])` as no-data (0.5 neutral or propagate insufficiency),
clamp SI to [0,1] at the subscore so out-of-range can't hide under the final clamp, and make
`cross_index_coupling` fail or return a sentinel below GridSize 2. Until then, the correction key
applies: a surprising-*clean* reading on a sparse-data story is suspect-by-construction.

---

## OQ-61 — Corpus header purity/cascade line: saturated cascade flag, type-composition restatement, hidden no-access count

**Ω-type:** Ω_C (conceptual/design — three operator rulings, none resolvable from code).

**Status:** open — awaiting operator ruling on three linked questions
**Priority:** 1
**Origin:** Purity audit 2026-06-03 (`audits/2026-06-03_purity/purity_audit_20260603.md` §6, K1/K2/K7).
**Files:** `json_report.pl:1244,1267-1273` (producers), `network_dynamics.pl:197-255`
(`network_stability_assessment`, `ep_base_severity`), `config.pl:483`
(`network_cascade_count_threshold=3`), `enhanced_report.py:262` (header render).

**Specific question:** The header line ("Network stability: cascading … purity N/M contaminated")
is one signal wearing two costumes, and both halves need rulings. (1) **Cascade saturation:**
severity derives from effective purity (`EP<0.70→warning`), and the cascade trigger is an
*absolute* count of 3 — witnessed 633 severe vs threshold 3 (211×) on N=1106. Should the threshold
be proportional/band-aware, or the flag dropped from the header? (2) **What the purity line should
mean:** the contaminated band is 98.1% tangled_rope+snare (witnessed cross-tab: TR 73.4%
contaminated, snare 87.4%, vs rope 7.7%, mountain 0.0%; converse control rope 92.3%
pristine+sound, mountain 95.1% pristine) — corpus-wide purity mostly restates type composition.
Should the header report purity conditioned on type (the spec §2.1 "coordination health" use
case), keep the raw bands, or both? (3) **Denominator:** purity_summary silently drops no-access
constraints (sums 1104 of 1106; M=2 today) — should it emit `no_access: M` alongside the bands?

**Evidence so far:** Runtime witnesses in the audit doc §6; raw data
`audits/2026-06-03_purity/` (census.tsv joined to pipeline per_constraint for the type
cross-tab). Contaminated fraction stable across corpus growth (~68.8% at N≈770, 68.2% at N=1104) —
consistent with structural property of scoring-on-this-composition, not story drift.

**What resolution changes:** (1) makes the cascade flag informative again (or removes a dead
indicator); (2) decides whether the header's purity line is a type-distribution echo or a
within-type health metric; (3) makes the unmeasured population visible. All three change report
text/aggregation only — no classification path is touched.

---

## OQ-62 — Purity band vocabulary fork: fpn_zone vs purity_zone, and sentinel→worst-zone in both banders

**Ω-type:** Ω_C (conceptual — which vocabulary wins is a design ruling; the guard is mechanical).

**Status:** open — do NOT auto-unify (choice of which range wins is the units-fork resurfacing as
**Priority:** 1
an implementation decision)
**Origin:** Purity audit 2026-06-03 (`audits/2026-06-03_purity/purity_audit_20260603.md` §4, K4/K6).
**Files:** `abductive_helpers.pl:97-103` (`fpn_zone`: pure/clean/contaminated/compromised/critical
at .80/.60/.40/.20), `logical_fingerprint.pl:605-611` (`purity_zone`:
pristine/sound/borderline/contaminated/degraded at .9/.7/.5/.3 — matches canonical spec
logic_extensions.md §2.3).

**Specific question:** Two band vocabularies over the same scalar share the word "contaminated"
for different ranges — [0.3,0.5) via `purity_zone` (reports, purity_summary) vs [0.4,0.6) via
`fpn_zone` (abductive evidence lines). Unify, rename one, or document the fork? Separately: both
banders map the −1.0 sentinel to their worst zone when fed directly (witnessed:
`fpn_zone(-1.0)=critical`, `purity_zone(-1.0)=degraded`) — latent only because every current
gating caller filters −1.0 first. Should both get an explicit `< 0.0 → unknown` guard clause
(mechanical, fail-closed) independent of the vocabulary ruling?

**Evidence so far:** Audit doc §4. The categorical needle's cut also differs from both:
post-bound-probe-fix `structural_purity` says 96.6% contaminated (excess ≤ 0.05 pass bar) vs the
scalar bands' 68% — three different "contaminated" cuts now live on three surfaces. Whether the
≤0.05 categorical bar is intended is part of this ruling.

**What resolution changes:** One word stops meaning three things. The guard clauses convert a
latent unknown→worst mechanism to fail-closed and can land independently of (and before) the
vocabulary ruling if desired.

---

## OQ-63 — Directionality's beneficiary read: agency-dependent or not? (corpus-wide χ stakes — HIGHEST-STAKES item of the agency-gate family)

**Ω-type:** Ω_C (design ruling: which beneficiary view should d-derivation consume), with an
Ω_E diagnostic attached.

**Status:** resolved — operator ruled 63-A (2026-06-05), sequenced after 64-A: `beneficiary_victim_directionality` consumes `agent_beneficiary` (commit `28f2dfc8`). Witnesses: ZERO-DIFF cutover on the live corpus (80/80 constraint×seat rows of d + dr_type identical) + positive control (probe whose only beneficiary is the registry non-agent entropic_universe_hypothesis → structural derivation refuses, falls to canonical d). Rationale: non-agent entries feeding d→χ are SILENT corruption; the metric path must not read what the signature path refuses. Previously ranked FIRST among the OQ-63..66 agency-gate family. NOT co-equal with the
inert drl_core:287 site (OQ-66): :287 is witnessed behaviorally inert today;
this one feeds χ for every constraint with beneficiary/victim facts.
**Origin:** FSM agency-gate session 2026-06-03 (KNOWN_STATE entry of same date). The two-site
fix (signature_detection.pl FSM gate + count_power_beneficiaries) deliberately did NOT touch
this consumer.
**File:** `constraint_indexing.pl:420` (`beneficiary_victim_directionality/3` —
`HasBeneficiaries` from a raw `constraint_beneficiary/2` existence check, → `power_role_heuristic`
→ d → f(d) → χ).

**Specific question:** `beneficiary_victim_directionality` treats ANY authored beneficiary —
including proposition-kind values (a doctrine/hypothesis the constraint vindicates, see
`narrative_ontology:non_agent_beneficiary/1`) — as evidence of gain-flow when deriving d. Is the
d-derivation's beneficiary read agency-dependent (a proposition cannot be a flow endpoint, so it
should read `agent_beneficiary/2`) or agency-independent (the authored fact marks the reading's
asymmetry structure regardless of referent kind)? Unresolved either way at the 2026-06-03 ruling;
status honestly: **undetermined**.

**Stakes:** corpus-wide χ. Every constraint whose ONLY beneficiaries are proposition-kind currently
gets structural d instead of canonical d(P) fallback — if the read is ruled agency-dependent, χ has
been mis-derived for that class (today: maxwell_demon_impossibility — masked because the natural_law
signature override decides its type upstream; the class grows with the corpus). This is the
highest-stakes instance of the beneficiary-field overloading, NOT a 12-mountain question.

**Diagnostic RUN (2026-06-04, read-only; full tables in KNOWN_STATE entry of same date):**

*Mechanics measured:* `power_role_heuristic` ignores the beneficiary flag at every power level
EXCEPT institutional (true→0.15 / false→0.10); so the effect decomposes into **class A** (no
victims: filtering empties HasBeneficiaries → structural derivation fails → regime-switch to
canonical d at ALL contexts) and **class B** (victims present: institutional-only Δd −0.05).
Hand-derived deltas from config.pl:140-145 matched the engine measurement exactly (class-A
d structural→canonical: powerless 0.95→1.00, moderate 0.65→0.6459, institutional 0.12→0.00,
analytical 0.72→0.725; class-B institutional 0.12→0.07).

*Per-item verdict (χ-delta × referent-kind — the kind is the witness, not the count):*
- maxwell_demon (PROPOSITION, ruled): d structurally derived from a vindication value at all 4
  contexts — **real mis-derivation, materially small** (|Δχ| ≤ 0.0062; ε=0.08).
- press_reformation__technological_inevitability (PROPOSITION, gate-2-held): identical numbers —
  real mis-derivation, small.
- statutory_debt_ceiling__constitutional_nullity (PROPOSITION, scoped-out): d mis-derived but
  authored ε=0.0 (testset :114) ⇒ χ ≡ 0 at every d — **no χ effect at all**.
- total_war_winnability_p45 (AGENT by ruling — control row): filtering WOULD have moved a
  genuinely-agent value's d/χ (Δχ to −0.0093) — demonstrates what the tagging gate protects.
- church_turing__physical_claim (AGENT — suffix lied a SECOND time: ":90 Primary beneficiary —
  gains epistemological monopoly, resource concentration, … funding" is explicit gain-talk; the
  testset's own narrative :236 predicts "institutional … pure beneficiary status (d ≈ 0.15)",
  matching the heuristic's beneficiary-aware value): **consumer working correctly**.
- preparedness_commitment__husk (AGENT — RULED 2026-06-04, witnessed in-file): :219's "the
  beneficiary (institutional continuity narrative) captures the legitimacy value" is the
  mechanism-label; :225's directionality logic names the experiencer of benefit ("The institution
  itself experiences the constraint as moderately beneficial (legitimacy, resource
  justification)"), corroborated by :100 and Perspective 5 (:178) — a self-aware institution
  protecting its own legitimacy and resources. **Consumer working correctly** (same verdict as
  church_turing). Ruling footing: cross-sibling perturbation (vs preparedness_transmission, a
  DISTINCT kernel) generated the AGENT hypothesis; the in-file witness confirmed it — where an
  in-file witness is absent and only the cross-kernel analogy carries, a tag is INFERRED, not
  ruled. Sibling closes identically: `bureaucratic_continuity`
  (preparedness_transmission__husk_reading :151; in-file :103 "preserves its organizational
  identity, budget lines, and staff roles") = AGENT, no-write, inert.

*Band crossings:* ZERO across the population — all Δχ ≤ 0.022 and ≥ 0.18 from the nearest edge
(rope_chi_ceiling 0.35 / tangled 0.35–0.90 / snare_chi_floor 0.66). Side observation, independent
of filtering: church_turing's analytical χ = 0.6576 sits 0.0024 below snare_chi_floor — a
knife-edge unrelated to this OQ.

*Controls:* per-host restore PASS ×11 (checked after EACH re-assert, not once globally); dispatch
control fired (class-A institutional d moved); candidate-only filtering on 3 partial hosts
(agent co-beneficiary retained) moved nothing; analytic table treated as falsifiable prediction —
substrate agreed. Raw side cross-checked against pipeline JSON to 6 decimals (maxwell,
church_turing).

**SCOPE QUALIFIER (closed 2026-06-04 by the OQ-65 census):** the blast radius above was
measured across the SUFFIX-PROBE-REACHABLE population (6 all-candidate hosts + 17 partial,
from 24 suffix values minus 2 witnessed agent false-positives), and the probe was known to
undercount. **The OQ-65 per-file census has now run (n=1106, every extracted item read):
read-based bait-authored value population = {entropic_universe_hypothesis,
hypothetical_survivors_counterfactual} — exactly the 2 already known, ZERO new bait hosts.**
The undercount worry is closed; "only 6 affected" now stands on a read, not a probe. The
census also flags 29 nonagent_referent_candidate hosts (`audits/2026-06-04_oq65_bait_census/oq65_bait_census.json`,
flag field) as the candidate pool for future registry/OQ-64 vindication-split work — those
are referent-kind questions, not bait.

*What the evidence says about resolution:* the mis-derivation is real but currently χ-immaterial
(low-ε hosts, institutional-only class-B effect, zero band crossings). Filtering :420 through
agent_beneficiary would be semantically correct for proposition values but moves NOTHING type-level
today and would WRONGLY move d for agent values if tagging ever lags (total_war control row). The
risk grows with corpus growth (a high-ε proposition-only host would put Δχ ≈ ε-scaled deltas near
band edges). The per-item escalation queue is EMPTY (preparedness ruled AGENT 2026-06-04, see
verdict row; the proposition rows are all ruled/held from the agency-gate pass). ~~Remaining
before a filtering ruling: the OQ-65 census only.~~ **The census ran 2026-06-04 (see scope
qualifier above): zero new bait, population read-complete. The filtering-ruling precondition
is MET — the :420 filtering question is now ripe for an operator ruling on the evidence
already in this entry (filtering is semantically correct for proposition values, χ-immaterial
today, wrong-on-lag risk per total_war control row).**

---

## OQ-64 — constraint_vindicates/2 split: proposition-vindication is overloaded into constraint_beneficiary

**Ω-type:** Ω_C (schema design).

**Status:** resolved — operator ruled 64-A (2026-06-05): schema split. `vindicated_propositions` array added to base_properties; compiler emits `narrative_ontology:constraint_vindicates/2` (feeds NO metric or gate); prompt rule: propositions are never beneficiaries (commit `e5fbc2e8`, witnesses in message: schema+pattern guard+compiler+engine+negative control). The non-agent hand registry stays as legacy defense for archived corpora. Live-20 referent scan: born clean (20/20 agent-shaped).
**Origin:** FSM agency-gate session 2026-06-03, Steps 1–2 calibration reads.
**Files:** `narrative_ontology.pl` (agent_beneficiary/2 + non_agent_beneficiary/1 registry — the
split's forerunner; see its comment block), generation templates (`agent/generate_*`).

**Specific question:** `constraint_beneficiary/2` is authored with at least three distinct
intents: (i) actual agent beneficiaries; (ii) propositions the constraint vindicates
(maxwell's `entropic_universe_hypothesis`, debt-ceiling's `constitutional_supremacy_doctrine`,
press_reformation's `technological_inevitability_interpretation`); (iii) detector-bait /
adjudication-expectation entries written so FSM would "evaluate" them (see OQ-65). Should (ii)
get its own authored field `constraint_vindicates/2` so generation stops overloading the
beneficiary slot?

**Evidence so far (authored text, witnessed 2026-06-03):**
- total_war_winnability_post1945 `:212`: beneficiary "included to trigger false-summit (FSM)
  evaluation only … FSM will correctly identify the lack of true beneficiary extraction and
  confirm the Mountain classification" — explicit bait + the misconception.
- maxwell_demon: "The beneficiary declaration … is included to evaluate whether this genuine
  natural law might contain a false-summit candidate" — the flagship case is also bait.
- environmental_instability `:193/:196` + omega `false_summit_beneficiary_ambiguity`: author
  expects FSM to adjudicate beneficiary-vs-less-victimized empirically; FSM is presence-only.
All share one misconception: **FSM-as-extraction-evaluator** (it is a presence trigger).

**The MIRROR direction (2026-06-04 ruling): proposition-shaped name, agent referent.**
`institutional_continuity_narrative` (preparedness_commitment__husk_reading :142) and
`bureaucratic_continuity` (preparedness_transmission__husk_reading :151) carry vindication-shaped
NAMES but agent-kind REFERENTS: the files' own directionality logic names the institution as the
experiencer of benefit (commitment :225 "the institution itself … (legitimacy, resource
justification)"; transmission :103 "preserves its organizational identity, budget lines, and
staff roles"). The maxwell/total_war cases are proposition referents in the agent-shaped field;
these are agent referents under proposition-shaped names — together they prove **the value
string's morphology is orthogonal to the referent's kind in BOTH directions** (which is why the
suffix heuristic had to be abandoned twice). The disambiguator is the authored directionality/
gain logic, never the name. When the `constraint_vindicates/2` split lands, the migration
tooling must classify by referent text, not value naming.

**What resolution changes:** the registry (non_agent_beneficiary/1) becomes a migration shim:
proposition-kind values move to `constraint_vindicates/2` facts at generation time; the agency
predicate stops needing per-value rulings for new corpus. Ruling 2026-06-03 stands meanwhile:
authorial purpose NEVER flips an agency tag.

---

## OQ-65 — Detector-bait authoring as committer-axis discriminator (and FSM-statistics validity)

**Ω-type:** Ω_E (corpus measurement), with a committer-axis Ω_C edge.

**Status:** mitigated — per-file census complete 2026-06-04 (results block below); ε-caveat
quantified. Remaining for resolved: the operator's ADOPTION ruling (see "What resolution
changes").
**Origin:** [EDGE] directive + scan, FSM agency-gate session 2026-06-03.

**Specific question:** How much of FSM's corpus-wide firing measures authored convention rather
than detected naturalization — and can the bait-vs-omega-routed split be used as a SIGNAL, not
just a caveat? Framing per 2026-06-03 ruling: the gaming is *structured and checkable against
the authored omega* — explicit-bait (beneficiary authored to exercise the detector) vs
omega-routed (author routes the open question through an omega and expects adjudication) are
distinguishable committer-axis postures, i.e. a discriminator, not only contamination.

**CENSUS RESULTS (2026-06-04 — per-file read of every extracted item, n=1106):**
Method: `python/audits/oq65_bait_census.py` — 5 read channels (A beneficiary×FSM sentences,
B purpose-verb sentences, C-ben balanced-captured beneficiary-mentioning omegas, D ±200-char
dual-anchor windows, F all beneficiary sentences in no-FSM-mention files) + 2 mechanical
(C-fsm, E); 10-assertion self-test with 4 real positive controls; ~2,500 items read by a
classifying model over 26 chunks; verdicts `audits/2026-06-04_oq65_bait_census/oq65_census_verdicts.jsonl`; final
artifact `audits/2026-06-04_oq65_bait_census/oq65_bait_census.json` (per-file category/flags/evidence/verdict_source;
sum exact over 1106); archived package with writeup:
`audits/2026-06-04_oq65_bait_census/`. Blind decoys: no-marker-bait and false-positive controls PASS; the
omega-routed decoy VOID by construction flaw, which the reader itself caught while blind
(documented in `audits/2026-06-04_oq65_bait_census/decoy_key.json`). Classification rule: `omega_routed` requires
FSM/detector/signature/engine NAMED with fires/triggers-class verb tied to the omega's
resolution; predictions-of-correct-firing on authored data = substantive.
- **explicit_bait: 2/1106** — maxwell + total_war structural_contraction ONLY; the census
  found ZERO new bait. Read-based bait-value population =
  {entropic_universe_hypothesis, hypothetical_survivors_counterfactual}, both already
  ruled/registered → the OQ-63 suffix-probe-undercount qualifier is CLOSED.
- **omega_routed: 75/1106 (6.8%)** — ~37× the 2 previously known; FSM-behavior-tied-to-omega
  authoring is a corpus-wide template-supported posture (3 files cite "Rule 2: route committer
  structure to omegas" / "required by FSM schema gate" verbatim — flag template_rule2_citation).
- adjudication_expectation_prose (FSM-as-evaluator misconception, prose vehicle): 13 (10 not
  also bait/omega-routed). **Expectation-authored union: 87/1106 = 7.9%.**
- Categories: fsm_aware_substantive 558, fsm_aware_no_beneficiary_link 264, no_fsm_commentary
  207. Other flags: bait_adjacent_phrasing 5; fsm_aware_nonauthoring 1 (tsunami
  catastrophe_validation explains deliberate NON-authoring); nonagent_referent_candidate 29
  (list fed to OQ-64).
- **Firing crossing** (fresh pipeline 2026-06-04T13:46Z commit c463b17; dirty = docs + the
  new audit script only; firing set 10, zero delta): **6/10 expectation-authored** — 1 bait
  (total_war) + 5 omega_routed (animal_moral property, environmental_instability,
  nuclear_impossibility, press_reformation tech_inevitability, technology_reformation
  tech_determinism). Rest: article_27 + humane_treatment substantive-aware,
  reformation_composite tech_mediation no-beneficiary-link, statutory_debt nullity no FSM
  text. Replaces 4/12: post-registry-fix the expectation-authored share of firings ROSE to 60%.
- **Discriminator readout (observation):** bait and omega-routed are textually
  distinguishable postures — bait = NL-physics host + non-agent value + authored-to-exercise
  purpose statement; omega-routed = named-engine-behavior tied to empirical omega resolution,
  template-supported, domain-spanning. Both are distinct from the corpus-dominant
  prediction-of-correct-firing (descriptive). The press_reformation gate-two host (OQ-66) is
  omega_routed — consistent with its authored-open hold.

**Evidence so far (witnessed 2026-06-03; first two items SUPERSEDED by the census above):**
- Per-item read of the 12 FSM-firing constraints: explicit bait 2 (maxwell, total_war);
  omega-routed adjudication-expectation 2 (environmental_instability, nuclear_impossibility);
  FSM-aware substantive 6; no FSM commentary 2 (article_27, statutory_debt_ceiling).
  ⇒ 4/12 firings were expectation-authored.
- 445/~1106 corpus files co-discuss beneficiaries with the false-summit detector within 200
  chars — the generation TEMPLATE is FSM-aware corpus-wide.
- Phrase-level scans UNDERCOUNT: exact "included to trigger" found only total_war; maxwell's
  bait uses "included to evaluate" — found only by per-file reading. Any corpus-wide bait census
  needs per-file reads (or an LLM pass), not greps.
- **Engine/shadow "maximal disagreement" is wiring-determined for the FSM class, not per-item
  calibration signal (2026-06-04, witnessed; LL table in KNOWN_STATE same date):** FSM's
  override target is tangled_rope (config.pl:469 → `resolve_modal_signature_conflict`
  signature_detection.pl:779), but the shadow's tangled_rope carries three `required` boolean
  specs (maxent_classifier.pl:177–179); `has_asymmetric_extraction` reads `constraint_victim/2`,
  so for every victim-less FSM host the shadow assigns p(tangled_rope)≈0 *by boolean gate*
  (witnessed: debt-ceiling TR boolean LL = −8.0, two missing required features —
  `requires_active_enforcement` is the characteristic second). Engine=TR vs shadow≈0-on-TR is
  therefore structurally guaranteed for the class; a confidence≈0 row cannot distinguish FSM
  miscalibration from this wiring (the diagnostic layer already patterns it as
  `signature_override_artifact`, yellow). Any FSM/shadow disagreement census under this OQ must
  bucket these rows separately. The residual discriminator that DOES carry signal is the
  shadow's top type: mountain at high p (maxwell pre-fix, 0.990) ⇒ beneficiary-field dirt →
  registry/OQ-64; non-mountain at artifact certainty (debt-ceiling rope=1.0/entropy=0, an
  ~11-nat least-bad win over a constraint authored outside every cluster's support) ⇒
  outside-support/taxonomy hole, not classifier error.

**What resolution changes:** §4.4 ("false positives corrupt corpus-level statistics") and the §6
ε-caveat get a measured input [DONE 2026-06-04: 6/10 firings, 7.9% of corpus
expectation-authored]; the committer axis gains a checkable signature (asserted structure
in tension with authored doubt — the press_reformation "suspicious uniformity" pattern that did
the gate-two discriminating work in this session's registry ruling). **Remaining for
`resolved`: the operator's ADOPTION ruling** — whether the explicit_bait / omega_routed /
adjudication_expectation_prose flags are promoted to a formal committer-axis signature
(engine- or report-level) or stay census-level annotation. Inputs: per-file flags + evidence
quotes in `audits/2026-06-04_oq65_bait_census/oq65_bait_census.json`; any future FSM/shadow disagreement census must
bucket the wiring-determined rows (evidence bullet above) separately from these
expectation-authored buckets.

---

## OQ-66 — Agency-gate boundary ledger: gate-two holds, scoped-out twins, and the :287 deferral surface

**Ω-type:** Ω_C (taxonomy/ruling record).

**Status:** open — (ledger — items graduate individually)
**Priority:** 1
**Origin:** FSM agency-gate session 2026-06-03; rulings recorded in KNOWN_STATE same date.
**Files:** `narrative_ontology.pl` (registry + two-gate principle comment block),
`prolog/tests/test_agent_beneficiary.pl` (the :287 inertness tripwire), `drl_core.pl:284-287/:333/:362`,
`maxent_classifier.pl:173,176,191`.

**Held at gate two** (two-gate principle: a NON-AGENT registry entry needs gate 1 ontology-true
AND gate 2 host-deserves-the-released-certification; AGENT tags need gate 1 only):
- `technological_inevitability_interpretation` (press_reformation_causality__technological_
  inevitability): gate 1 passes (an interpretation is proposition-kind); gate 2 FAILS on authored
  openness — metrics/shadow are maxwell-identical (MaxEnt mountain 0.990 / entropy 0.031, a
  near-perfect forgery the shadow cannot separate), but `:215` self-describes the 4×mountain
  uniformity as "suspicious", omegas `natural_law_vs_contingent_framework` and
  `technology_determinism_assumption` are authored OPEN ("if deployment requires choice:
  false_summit fires"), and the file forecloses its beneficiary_deployment sibling. Stays
  unlisted ⇒ default-agent ⇒ FSM keeps firing. Graduation: the host's omegas close toward
  genuine-law and the convergence read passes.
- `constitutional_supremacy_doctrine` (statutory_debt_ceiling__constitutional_nullity_reading):
  scoped OUT by ruling — maxwell's metric twin (NL profile identical except BC); no honest agency
  line separates them; the separating lever is its authored `emerges_naturally` (a statute) or
  Fix-C taxonomy work, NOT the registry. Its shadow remains the rope=1.0/entropy=0 vacuum —
  per the OQ-65 wiring-determined-disagreement evidence (2026-06-04), that vacuum is the
  outside-support case (no type cluster admits ε=0 ∧ theater=0.95: corpus pitons are degraded
  snares, μ_ε=0.65 — a zero-extraction-pure-ceremony cell does not exist), so its confidence≈0
  is not FSM-calibration evidence either way.
- `drl_core.pl:287` (`natural_law_without_beneficiary`) deferral: semantically agency-dependent
  ("no identifiable human beneficiary" — filtering would be correct) but witnessed behaviorally
  inert for the current divergence set. Consumer surface ON RECORD: snare block (drl_core.pl:333),
  tangled_rope block (drl_core.pl:362), MaxEnt shadow forbidden-features for snare AND
  tangled_rope (maxent_classifier.pl:173,176,191), plus diagnostics (invertibility_analysis.pl:123,
  omega1_audit.pl:128). Staleness guard: `test_agent_beneficiary:nlwb_287_inertness_direct`
  classifies every divergence candidate raw-vs-filtered (dr_type ×4 canonical contexts +
  maxent_top_type) and fails loudly on first divergence — when that test fails, this deferral has
  expired; do not silently re-green it.

---

## OQ-67 — Legacy power-modifier χ path in drl_audit_core: migrate or rule exempt

**Ω-type:** Ω_C (design ruling — one classification formula, or a declared-separate audit path).

**Status:** open
**Priority:** 1
**Origin:** TODO.md item 1 ("Kill the Legacy Chi Path"), inherited at tracking-surface
consolidation 2026-06-04; the in-code TODO predates it.
**Files:** `drl_audit_core.pl:18` ("TODO: Migrate to sigmoid pipeline. See issue:
legacy-power-modifier-migration."), `config.pl:67` (same tag; notes zero dr_type flips at
[0.5x, 2.0x]).

**Specific question:** `drl_audit_core` still computes on the legacy power-modifier χ path
(χ = ε × π) rather than the canonical sigmoid pipeline (χ = ε × f(d) × σ(S)). Every other
deprecated caller was migrated 2026-05-17 (`classify_at_time/4`, `snapshot_type/3`). Is the
audit-core path (a) the last unmigrated caller — finish the migration — or (b) deliberately
separate by design (it is a quick-check operating on pre-computed Chi values, documented as
"deliberately separate, different purpose")? If (b), the TODO at :18 should be replaced by a
declared-exemption comment so the migration tag stops reading as unfinished work; if (a), the
migration needs an old-vs-new diff per Build Discipline Pattern 3 before the legacy path is
removed. config.pl:67's note (zero dr_type flips across [0.5x, 2.0x]) suggests low blast radius
either way — verify, don't assume.

---

## OQ-68 — Module-internal dynamic facts read cross-module by qualification bypass (maxent_dist/3 instance)

**Ω-type:** Ω_C (API-boundary design), with the load-path gotcha as context.

**Status:** open
**Priority:** 1
**Origin:** AGENDA.md Item I-3 (2026-05-18, then about `maxent_profile/3`), inherited at
tracking-surface consolidation 2026-06-04 and re-verified against the live tree.
**Files:** `maxent_diagnostic.pl:129,137,183` (reads `maxent_classifier:maxent_dist/3`),
`maxent_classifier.pl:69` (`:- dynamic maxent_dist/3.` — NOT in the module export list).

**Specific question:** the original I-3 instance (`maxent_profile/3`) was overtaken by the
profile-indexing fix (`maxent_profile/4` shipped), but the CLASS is live: `maxent_diagnostic`
reaches past `maxent_classifier`'s public API into the unexported dynamic fact `maxent_dist/3`
by module qualification. Any internal signature change fails silently at the bypass sites (the
exact mechanism that made the profile-accumulation bug's blast radius hard to enumerate).
Should internal dynamic stores get public read accessors (export a `maxent_dist/3` accessor or
a dump predicate), or is qualification-bypass acceptable for diagnostic-only consumers if
declared? Related context: `docs/technical/swipl_load_path_and_probe_gotchas.md` §1 — module
boundaries in this repo are already porous via non-module report files importing into `user`,
so the export list is the only honest API statement there is.

**Diagnostic when picked up:** grep for `maxent_classifier:` qualified calls outside the module;
classify each against the export list; the unexported set is the leak inventory (repeat for the
other stateful modules: purity caches, fingerprint stores).

---

## OQ-69 — Research-frontier backlog inherited from retired AGENDA.md / TODO.md (ledger)

**Ω-type:** Ω_P (research program — items graduate to their own OQ or work package when picked up).

**Status:** open — ledger; items graduate individually
**Priority:** 1
**Origin:** Tracking-surface consolidation 2026-06-04: AGENDA.md, AUDIT.md, TODO.md reviewed
item-by-item against the substrate and deleted (Pattern 2: ISSUES.md is the single tracker).
Items below were verified STILL UNTRACKED and still live at consolidation; everything else in
those files was verified shipped (maxent_profile/4; OQ-59 #1–#4; never-generated #1), already
tracked (regen-polish backlog + 4 hard-fails in OQ-58), or moot (UNRESOLVED_MANDATROPHY count
from the pre-rebuild corpus; "scope has zero classification effect" — σ(S) is now in the
canonical χ).

- **Engine-hardening pair from the apparatus paper** (`when_apparatus_sharpens_taxonomy.md`,
  "Two engine extensions… remain unimplemented"): (a) scope-design validator on
  `site_contexts/N` predicates (catch the σ(universal)=1.0 class of site-design failure before
  the next site is added); (b) MaxEnt parameterization for arbitrary sites — unlocks
  (c) **Arakelov fragility on 10-slice contexts** (`project_orientation.md` §8.3, marked Open).
- **Spec-encoding unit tests for load-bearing measurement primitives** (AGENDA D-1): encode
  paper-documented behavior as assertions — χ argument structure (d, σ), entropy normalization,
  H¹ (signature-resolved orbit — pairs with OQ-27), MaxEnt profile context-independence, purity
  propagation rate. Two witnessed spec-vs-code drifts motivate it; drift detection moves to
  commit-time.
- **Cover-story detector enrichments** (AGENDA Package B): wire drift_event predicates into
  `cs_pattern_detection.pl` verdict clauses (e.g. extraction_accumulation + coupling_drift →
  anchored_fixity_with_accretion). One-clause additions + regression test each; ship as
  drive-bys.
- **Scaffold/renewal audit** (Package D): exercised renewal = scaffold without drift;
  performative renewal = scaffold + extraction_accumulation + theater_rising. Testable with
  existing predicates.
- **Cluster-level analysis** (Package F): cluster-signature statistics + cluster-level CS
  inference in `enhanced_report.py`; then Package G (systematic clustering exploration) after.
- **Empirical second/third cases** (Package C): 2026 US midterm constitutional-legitimacy axes,
  Colombia 2026; Roman Empire backtest queued for a dedicated session.
- **δ → baseline-deviation reframing** (Package E): theory session first; δ not load-bearing in
  current implementation.
- **Python toolset consolidation** (TODO.md item 2): group `python/` scripts into
  subdirectories + a single CLI entry point. Pure maintainability; note OQ-32's lesson (the
  last reorg broke 6 scripts' path resolution — budget for the witness pass).
- **Parameterize the 17 directionality constants** (AUDIT W2/E1): hardcoded in
  `constraint_indexing.pl` (`power_role_heuristic/4`, `exit_modulation/2`); swept inert at
  ±25%, so maintainability-only — but OQ-63's d-derivation work touches the same table, so do
  them together if either is picked up.
- **T4 (confirmed_liminal) one-case category**: re-examine when a second T4 case appears.
- **framing_notes invitation calibration**: does it produce conceptual or empirical-leaning
  omegas? Calibration signal for generation.
- **check_stack baseline cleanup → then wire as pipeline gate** (infra hardening 2026-06-04,
  KNOWN_STATE same date): `prolog/check_stack.pl` baseline holds 4 undefined-predicate refs —
  `data_repair:constraint_beneficiary/2` + `data_repair:constraint_victim/2`
  (data_repair.pl:123/136/163 — wrong-qualifier candidates, OQ-57 class; each needs a
  per-call-site witness before fixing), `narrative_ontology:requires_active_enforcement/1`
  (drift_events.pl:175 — verify against the resolved OQ-57 fix), `validation_suite:test_case/4`
  (test_harness.pl:26 — generated-file coupling). When the baseline is empty, wire
  `run_check_stack` next to the ISSUES status-grammar gate at run_pipeline entry.
- **Incremental tabling to replace hand-rolled memo caches**: SWI `:- table ... as incremental`
  with `as incremental` dynamics auto-invalidates on retract/assert, retiring the manual
  `cache_registry:clear_all_caches/0` discipline. Output-affecting on the hottest path
  (classify_at_context) — OQ-02's LCO history says zero-diff witness first.
- **Output write-path anchoring**: exporters/probe scripts still write cwd-relative
  `../outputs/...`; anchoring writes the way corpus reads are now anchored
  (`resolve_corpus_dir/2`) would complete swipl location-independence and retire the
  remaining `cd prolog/` requirement (gotchas §9).
- **Author "the mint" as a testset** (2026-06-11 Pew-typology review exchange): the information
  regime as a constraint in its own right — own beneficiaries, own suppression signature, own
  perspectival split. It is a constraint hypothesis the corpus does not contain, generated *by*
  the essay rather than by the engine — the first deliberate instance of the loop the exchange
  named: essay → constraint hypothesis → formalization → engine resistance → sharper next essay
  (the writing as the corpus's generation mechanism, the Prolog as its immune system; see
  design_discipline §4, false-summit discipline). Author it with the essay's honest prior and
  let the engine fight it the way it refused `institutional_trust_erosion`'s mountain claim.

---

## OQ-70 — FNL fires on template-authored bait perspectives: FNL prevalence measures authoring convention, not detection

**Ω-type:** Ω_E (corpus measurement), with an Ω_C committer-axis edge (same family as OQ-65).

**Status:** resolved — operator ruled option A as the CLASS (2026-06-05): no signature may read a single authored perspective as a story-level claim. claimed_natural source 2 AND appears_as_rope's sibling clause removed (commit `72ec2cdd`). Witness on the live 20: FCR 16→5 (remaining are low-ε profile-driven), FNL 3→1; POSITIVE CONTROL manpower_exhaustion_trap (explicit claim-mountain, non-compliant) still fires FNL via source 1 — wrong path removed, detector intact. Signature prevalence is citable as a claims statistic from the rebuild's story 1.
**Statistics reset No. 2 (2026-06-11, OQ-109 Phase B2):** the one-shot example cut over from
`verification_bottleneck.json` (the bait-template source) to the stakeholder-surface
`agent/example_platform_commission.json` at the commit landing this note — signature-prevalence
statistics reset AGAIN at that commit. The example-inherited signature list (what post-reset
prevalence must discount as copied-from-example vs pre-existing) is
`audits/2026-06-11_oq109_phase_b/EXAMPLE_INHERITED_SIGNATURES.md`.
**Origin:** 2026-06-04 session: "is the ~95% disguise-signature dominance substantive or a
generator artifact?" (older-evaluation question re-examined on the live corpus). Probes 0–5;
raw artifacts: `audits/2026-06-04_fnl_bait_confound/fnl_probe0_file_constraint_map.json`,
`audits/2026-06-04_fnl_bait_confound/fnl_probe0_reconciliation.json`, `audits/2026-06-04_fnl_bait_confound/fnl_probe1_attribution.{pl,jsonl}`,
`audits/2026-06-04_fnl_bait_confound/fnl_probe2_counterfactual.{pl,jsonl}`, `audits/2026-06-04_fnl_bait_confound/fnl_probe3_coupling_by_band.json`.

**Specific question:** FNL (827/1106 readings) is supposed to detect constraints that *claim
naturality* and fail Boltzmann independence. All 827 firings ride `claimed_natural/2` source 2
(`constraint_classification(C, mountain, _)` — any single authored perspective), and that
perspective is a generation-template convention (the one-shot example
`agent/verification_bottleneck.json` contains "PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL
LAW VIEW (MOUNTAIN)" at (analytical, analytical); 908/1106 constraints author one at exactly
that tuple, 922 at some context). Which remediation lever applies — and until one is ruled,
**no FNL prevalence figure may be cited as a detection result**.

**Evidence (all witnessed 2026-06-04, denominator 1106 = testset constraints, 1:1 with files;
per_constraint's 1107th entry is `catholic_church_1200`, an engine demo in
`constraint_instances.pl`, excluded — the extra is NON-testset and no testset constraint is
missing from per_constraint: only_pc = {catholic_church_1200}, only_files = ∅):**
- **Probe 0 (funnel, exact):** FNL ⟺ (≥1 authored mountain perspective) ∧ Boltzmann
  non_compliant, with zero exceptions both directions. 922 mountain-authoring → 827 FNL +
  {48 FCR, 26 NL, 11 CI, 10 FSM} (the 95 non-FNL are exactly the 93 compliant + 2
  inconclusive). 184 complement → 0 FNL. FNL ∩ inconclusive = ∅ (asserted).
- **Probe 1 (attribution):** `fnl_evidence` Claim slot = `indexed_mountain_classification`
  for **827/827**. Sources 1 (explicit claim, 41 files) and 3 (NL profile) contribute zero.
  Controls: source-1 dispatch proven live; no natural_law id in the FNL set. In-session
  signatures reproduce the pipeline 1106/1106.
- **Probe 2 (counterfactual, the load-bearing witness):** retracting the 915 tuple-T
  mountain FACTS (= 908 constraints holding ≥1; clearing
  `cached_coupling`/`cached_classification`) gives the destination histogram FNL→FCR **809**,
  FNL→FNL 14 (every one holds a non-T mountain perspective — set-equal; 922−908 = 14, the
  plan's "~7" was a files-minus-facts subtraction; 13 hold only non-T mountains, 1 held both
  and the non-T fact kept source 2 alive), FNL→constructed_high_extraction 4 (no rope
  perspective, ε ≥ 0.62), FNL→{genuine natural_law, CI_rope} **0**. 809+14+4 = 827, closed. Sensitivity control flipped to its pre-named prediction
  (`abrahamic_covenant__land_promise_constraint` → false_ci_rope); specificity control: 41
  explicit-claim constraints unchanged; collateral: 0. **Read as bait fungibility, NOT
  substance:** the disguise aggregate barely moves (1046→1042) because the ex-FNL mass lands
  in FCR via the *other* template perspective (`appears_as_rope` source 2 — 1063/1106 files
  author a rope perspective, the prompt's Mandatory Perspective #2). An aggregate FNL+FCR
  delta is the wrong observable; the histogram is the witness.
- **Probe 1b (FCR is the same gate pattern, attribution-level):** FCR appearance source:
  indexed_rope_classification 174 / explicit_rope_claim 45 / low_extraction_profile 0.
  No FCR counterfactual was run — conclusions about the combined "94.5% disguise" figure are
  NOT licensed by this session's witnesses.
- **Probe 5 (the detector did ZERO discriminating work on FNL):** of the 189 FNL firings that
  overrode a non-tangled_rope authored claim (snare 166, rope 13, scaffold 5, piton 5), the
  discriminating subset {override} ∩ {source 1 or 3} = **∅**; 188/189 were metrically
  consistent with their own claimed type's context-free gates — the lock steamrolled
  consistent claims. The single gate-inconsistent case
  (`decalogue_image_prohibition__moderate_iconoclast_reading`: snare claim, supp 0.58 vs the
  0.60 floor — a 0.02 miss; ε 0.52 passes) ALSO fired via source-2 bait
  (`fnl_claim_source: indexed_mountain_classification`, probe1 jsonl; bait perspective at
  `.pl:181`) — a coincidental landing on a marginally mislabeled file, credit to author
  error, not detection. Substantive yield of 827 firings: 0 detected, 1 coincidental. Second
  bait witness: `acceptable_risk_energy__catastrophic_tail_reading.pl:186` ("Engine will flag
  this as false summit").
- **Probe 3 (context, confounded run):** non-compliance ≥ 85% for every ε band ≥ 0.3 (100%
  at 0.3 and ≥ 0.6); compliance lives almost entirely at ε < 0.2. Mid-ε constraints fail the
  4×3 grid essentially by construction.
- Downstream stakes: the tangled_rope ~70% dominance is inherited from authored
  `claimed_type` (779/1106 tangled_rope; 638/827 FNL were already claimed tangled_rope — the
  FNL→tangled_rope lock confirms the authored claim). Any paper text citing the trifurcation
  / tangled_rope dominance / "95% disguised" inherits the artifact.

**What resolution changes:** Three levers, mutually compatible, ruling is the user's:
(1) narrow `claimed_natural` source 2 / `appears_as_rope` source 2 (engine change; reshapes
current-corpus statistics and cascades into papers); (2) de-bait the generation example/prompt
(`agent/verification_bottleneck.json`, `prompts/constraint_story_generation_prompt_json.md`
mandatory perspectives — future generation only); (3) adopt the OQ-65 framing corpus-wide
(bait as committer-axis signal; bucket FNL statistics into bait-driven vs substantive in all
reporting). Cross-links: OQ-65 (same phenomenon, FSM, n=12), OQ-43/OQ-44 (gate-quality
family), OQ-48 (threshold recalibration would change Probe 3's bands), OQ-49 (override
prevalence at 3000-scale).

**Post-de-leak persistence check (2026-06-05, live-corpus first 20):** the bait convention SURVIVES at reduced rate — 5/20 stories author a mountain perspective, and all 5 put it at the (analytical, *) bait tuple (pre-de-leak: 908/1106 ≈ 82%). The assembled payload still ships the one-shot example containing the ANALYTICAL OBSERVER mountain perspective. The remediation ruling (claimed_natural source-2 semantics, and/or example de-baiting) is now a PRE-CORPUS-BUILD item: every story generated before it inherits the confound.

## OQ-71 — Depth-lineage probe: does kernel-nesting depth re-open structural-class discovery, or saturate like breadth?

**Ω-type:** Ω_E (corpus measurement — falsification probe aimed at `docs/design/a_hypothesis_about_corpus_size.md` §3).

**Status:** partial — scale run complete 2026-06-04 (438/449 stories): H1 and H3 falsifiers FIRED beyond resampling noise, H2 mixed; attribution between nesting-depth and seed-authorship awaits the authorship-controlled breadth arm (the named discriminating experiment at the end of this entry)
**Priority:** 1
**Deps:** blocked_on_human operator-spend-go
**Origin:** 2026-06-04 session: kernel/reading recursion thought experiment (constitutional_government ⊃ us_constitution ⊃ amendments ⊃ doctrines, and upward toward `government`). Plan: `~/.claude/plans/virtual-inventing-allen.md` (mirror the operative content here as steps complete).

**Specific question:** The corpus-size hypothesis claims the structural-class vocabulary is a bounded attractor. Depth-correlated authoring (a designed kernel-nesting lineage, ~600 stories) is the hardest test: does a depth arm mint new 5-dim structural classes (props, voids, actors, drift, zone — shift tracked separately) at a rate exceeding a breadth control at matched n? Pre-registered: H1 excess-over-control ≤ 0 (cumulative + rarefaction slope at n=300); H2 within-level saturation (L2/L3/L4 only, n≥100 each) with persisting ε/χ spread; H3 MI couplings ≈ live-recomputed corpus baseline.

**Evidence so far (Step 0, witnessed 2026-06-04):**
- Original v5 fingerprint dump salvaged from volatile `/tmp`: `audits/2026-06-04_oq71_depth_lineage/v5_sixdim.txt` (3,380 lines, md5 `0493c253…` matches `/tmp` original); `cur_sixdim.txt` (772 lines) likewise. These are the reproduction targets for the rebuilt probe — **multiset-exact match on `prolog/archives/prolog_v5/` (3,380 .pl files, counted) or the depth readout HALTS**. Note: CLAUDE.md's "`testsets_3000/`" is name-drift; no such dir exists — the archive is `prolog/archives/prolog_v5/`.
- Control arm = the 300 stories of commit `64cc249a` (2026-06-03 never-generated batch): `audits/2026-06-04_oq71_depth_lineage/control_membership.json` (n=300, git-derived).
- Control seed authoring measured (`outputs/completion_seeds/never_generated_seeds.json`, 300/300 matched): `sibling_reading_ids` length distribution {1:6, 2:284, 3:5, 4:5}; summaries 504–1,519 chars (median 875); keys incl. `expected_structural_delta`, `family_id`. **Depth-arm seed policy: match these (≈2 siblings, range 1–4) — the earlier draft cap of ≤6 would itself have been an arm-asymmetry confound.**
- **Zero-generation gates (all run 2026-06-04, before any further authoring — sequencing ruled cheapest-falsifier-first):**
  (1) **v5 reproduction: EXACT MULTISET MATCH** — `python/lineage_fingerprint_probe.py --corpus archives/prolog_v5` reproduces all 3,380 lines of the salvaged original, missing=0 extra=0 errors=0, after one localized documented exclusion (`catholic_church_1200`, the non-corpus engine demo `[stack]` asserts from `constraint_instances.pl`; first run dumped 3,381 with the +1 exactly there). Binning faithful; serialization read off the artifact (`six(shift(P,M,I,A),Props,Voids,actors(B,V),drift(E,S,T),zone(EZ,SZ))`, `~w` emission).
  (2) **Novelty-classifier discrimination: PASS both directions** (known-in-corpus → not-novel; synthetic → novel). Live corpus: 1,106 constraints, 157 distinct 5-dim structural classes, 39 shift classes (`audits/2026-06-04_oq71_depth_lineage/live_sixdim_tagged.tsv`).
  (3) **Sibling-length stratification: length-1 SLOPES, lengths 2–4 flat.** Control len-1 (n=6): 1/6 within len-2 realized set (bootstrap pctile 0.002 vs held-out len-2 sextets, K=4000, seed 71), mean|props| 2.67 vs 3.76 (pctile 0.001); len-3/4 unremarkable. **H1 therefore reports within-length-stratum excess; powered stratum = length-2+ (control n=294); length-1 reported separately. Scale-up tree: prefer fan ≥3 to minimize the depth arm's length-1 stratum.**
- Exemplar authored and validated: `audits/2026-06-04_oq71_depth_lineage/tree_spec/{upward,spine,us}.json` → 35 kernels / 112 seeds, fans ∈[2,5], levels 0–8 (`python/build_lineage_seeds.py`; lineage sidecar at `audits/2026-06-04_oq71_depth_lineage/lineage_probe_01/lineage.json`). **Pilot ruling: run Steps 2–6 on the 112 exemplar first; pilot success criterion is MACHINERY ONLY (112 < matched-n 300, so the pilot cannot test H1 and its excess number must not be read as signal); scale to ~600 only if machinery holds.**
- **Pilot run (2026-06-04): machinery HOLDS on all checks.**
  - Step 2 `--run-tag` routing in `run_no_scope` (`agent/generate_kernel_corpus.py:1087`), dual-gated: flag-off batch payload byte-identical to pre-change baseline; flag-on payload byte-identical to flag-off (routing-only). Harness: `audits/2026-06-04_oq71_depth_lineage/gate2_capture.py`.
  - Step 3 generation: **109/112** into `prolog/testsets/lineage_probe_01/` (batches `msgbatch_01Mf48hoJ6MrarCrqGPDgq4F`, `…01P33gKVCTfDsN1yU3nuNdgK`, `…012TB1g6rC1q1b3qhVCzWn2p`); 3 hard-fails after 3 attempts (`failures.json`; rate 2.7% ≈ control's 4/304). Isolation witnessed: flat `testsets/` still exactly 1,106; 109/109 stories carry `cs_story_uid` + `cs_kernel_id`.
  - Step 4 OQ-58 integrity sweep run manually: 12 edges quarantined — 9 point at the 3 failed stories, 3 are model naming-drift (`double_jeoparty_bar` typo, bare `strict_scrutiny_tier`, `third_amendment_quartering_protection`); disposition pending with the failed-story regeneration. **Wiring hazard for scale run: the sweep writes its quarantine to the FLAT `prolog/testsets/cs_reading_relation_quarantine.json` even for run-tagged input** (pilot's copy relocated to `audits/2026-06-04_oq71_depth_lineage/lineage_probe_01/quarantine.json`; pass a run-scoped path before the scale sweep or a flat-corpus sweep will be clobbered).
  - Steps 5–6 probe + analysis: 109/109 fingerprinted, 0 errors; `python/lineage_excess_analysis.py` (re-runnable; results `audits/2026-06-04_oq71_depth_lineage/pilot_machinery_results.json`). **Pilot excess at matched n=83: +24.0 distinct classes (depth 55.0 vs control 31.0, control resample band [25,38]) — QUARANTINED per the pilot ruling: machinery-sane output, NOT an H1 readout** (n<300; pilot tree is shallow-structure-heavy). Per-level census runs (L0–L8).
  - **Next step (gated on user scale ruling):** author remaining ~490 readings (branches: british, westminster_export, roman/athenian, french_1791, soviet_1936, german, indian, japanese subtrees + non-constitutional governance contrast subtrees), prefer fan ≥3 (length-1 stratum minimization), regenerate failed 3 with the scale batch, re-run Steps 3–6 at ~600, then the powered H1/H2/H3 readout. Sequencing constraint: scale-run integrity sweep needs the run-scoped quarantine path fix first.

**Sequencing (why gated):** Step 1 (tree spec + `python/build_lineage_seeds.py`) ends at a user checkpoint gating all API spend (~$9.5–21 at n=600, batch). Step 2 (`--run-tag` in `run_no_scope`, `agent/generate_kernel_corpus.py:1087`) is dual-gated: flag-off byte-identity + flag-on request-payload identity. Generator (model/prompt/schema/example) is frozen — it must match the control batch byte-for-byte or the comparison is confounded. Output is run-tagged (glob-isolated): 600 depth-correlated stories flat in `testsets/` would shift corpus composition ~35% and contaminate prevalence stats (OQ-70).

**What resolution changes:** If depth saturates like breadth (H1–H2 hold), the bounded-attractor verdict survives its hardest authoring test and the §6 "drift over a fixed class set" framing extends to nested kernels. If depth keeps minting classes, §0's boundedness verdict weakens in a way breadth growth could not show, and the non-linear site question (DAG of abstraction levels; subobject-classifier audit's open end) gains an empirical motivation. Cross-links: OQ-58 (dangling-string resolver — kernel atoms as labels-without-nodes is the same problem one level up), OQ-26 (ε reading-relative: ε comparisons within-arm only), OQ-70 (isolation rationale).

**Scale-run results (2026-06-04; witnesses: `audits/2026-06-04_oq71_depth_lineage/powered_readout_results.json`, `depth_sixdim_tagged.tsv`; scripts `python/lineage_fingerprint_probe.py`, `python/lineage_excess_analysis.py`):**
- Tree: 449 seeds / 140 kernels / 10 levels, fan 2–5 throughout; depth length-2 stratum (285) ≈ control's (284). Generated 438/449 (11 hard-fails, 2.4%, `failures.json`); one story (`ghq_drafting_imposition__embraced_revolution_reading`) emitted without `cs_structure` — unstampable by `stamp_kernel_linkage`, kept in fingerprint analysis (joins on lineage.json by constraint_id, not CS stamps). Isolation re-verified: flat corpus exactly 1,106 before/after. Integrity sweep run-scoped (fix `c41e70b0`): 46 edges quarantined, OQ-58 disposition pending.
- **H1 FALSIFIED beyond noise at every matched n** (powered length-2+ stratum, K=2000, seed 71): E[distinct 5-dim classes] depth 71.3/88.0/103.4 vs control 47.8/58.7/68.7 at n=150/200/250 — 95% resample bands non-overlapping at all three (n=200: [79,97] vs [52,65]); slope at n=294 also higher (0.271 vs 0.192). Depth arm: 156 classes in 438 stories vs the entire 806-story live baseline's 118. NOT list-inflation: mean |props| 3.83 vs 3.77, |voids| 1.98 vs 2.25.
- **H3 FALSIFIED — couplings reshaped, not dissolved:** matched-n=300 MI props↔actors 0.48±0.03 vs live 0.71±0.07 (weaker, ~5σ); voids↔zone 1.31±0.03 vs 1.05±0.05 (stronger). Coupling pattern is regime-dependent.
- **H2 MIXED:** within-band novelty at matched n=100: L5 47.9 → L6 57.9 → L7+ 51.0 (rises, then the deepest band turns down; new-vs-shallower falls to 5/3 at L8/L9 — the arm begins saturating toward its own S_max, punctuated-equilibrium-consistent). ε-spread persists at all bands (sd 0.13–0.18, full range).
- Doc updated: `docs/design/a_hypothesis_about_corpus_size.md` §10 — boundedness survives only as within-regime; coupling-invariance revised; H1/H3 firing is observationally equivalent to a §9 generator punctuation because the arm bundled depth with seed-authorship (named threat; sibling-shape and richness controlled, authorship not).

**The discriminating experiment (what closes this OQ): an authorship-controlled breadth arm.** ~300 NON-nested seeds authored by the same hand/model and template as the lineage (Opus-written commitments/deltas, control summary template, flat kernels with ≈2-sibling fans), run-tagged, same probes. If it also mints ~1.5× classes at matched n → the excess is seed-authorship (a §9 punctuation), depth adds nothing. If it tracks the old control → depth itself re-opens discovery. Mechanics: reuse `build_lineage_seeds.py` seed shape with parent_kernel=null kernels; everything downstream runs unchanged. Until it runs, cite this probe as falsifying *unconditional* boundedness and coupling-invariance only — not as proving depth-specific discovery.

## OQ-72 — Mechanical alignment key for the axiom axis (axiom diff is seat-constituted)

**Ω-type:** Ω_C (what counts as "the same axiom" across readings is a conceptual ruling), with an engineering build once ruled.

**Status:** open — evidence witnessed; design not started (deferred out of the 2026-06-05 de-leak work item by user ruling)
**Priority:** 1
**Origin:** 2026-06-05 generation-pipeline audit (brief F5). Evidence: `axiom_diff.pl:49-72` —
`exact_name` alignment is structurally all-blind (0 of 935 within-kernel reading-pairs share an
axiom NAME; see the OQ-59 axiom-diff demo), so all non-trivial alignment requires the hand-authored
`axiom_concept/2` declared seat (empty by default, never baked). The observer diff (`reading_diff.pl`)
aligns mechanically on (P,T,E,S) with zero hand-authoring; the axiom axis has no analogous key, so
axiom-axis invariance claims are seat-constituted rather than discovered.

**Specific question:** Can a mechanical alignment key (a controlled `axiom_concept` vocabulary
emitted at SCOPE time, or post-hoc lexical/embedding clustering with human ratification) make the
axiom diff discoverable without smuggling the analyst's ontology into the seat? The declared-seat
design (`design_discipline.md`) is intentional — any mechanical key must remain a *proposal* the
human ratifies, not a silent override.

**What resolution changes:** the cross-axis invariance correlation (OQ-75;
`the_perturbation_principle.md` §7.1) currently has a hand-authored axiom axis — a mechanical key
would put all three axes (observer/axiom/time) on the same discovered footing.

## OQ-73 — Cross-cultural frame probe: does the reading-set move when the generation frame moves?

**Ω-type:** Ω_E (one controlled generation run answers it).

**Status:** open — deferred out of the 2026-06-05 de-leak work item (verification hygiene: a second change variable; the de-leak's Stage-1/Stage-2 must read clean first)
**Priority:** 1
**Deps:** blocked_on OQ-75
**Origin:** 2026-06-05 generation-pipeline audit (brief F6). Reading-sets inherit one jurisprudential
tradition's dialectic (the originalist/positivist/realist kit); the framework claims the
site/reference frame is normative and chooseable (v7 Axiom 7).

**Specific question:** author one existing kernel under a second cultural/jurisprudential frame
(civil-law, religious-law, customary) and diff the reading-set and type-distribution against the
original. If neither moves, "the site is normative" is doing less work than claimed. **Sequencing
constraint:** run only as its own controlled probe AFTER the de-leak Stage-2 baseline (OQ-75)
exists, and attribute any shift against the lens-diversity commit (`d179423d`) separately — that
instruction is already live and is itself a frame-diversity lever.

## OQ-74 — coordination_type: kernel-intrinsic or reading-relative? (55% sibling disagreement awaits a ruling)

**Ω-type:** Ω_C (what KIND of fact coordination_type is), with an Ω_P edge (the ruling is the operator's, not the evidence's).

**Status:** resolved — core RULED reading-relative 2026-06-14; OQ-49 hand-up limb moot (positive-controlled)
**Origin:** 2026-06-05 generation-pipeline audit (brief F9 + §3). Re-witnessed on the live corpus:
158 of 286 kernels with ≥2 coordination-typed readings have siblings DISAGREEING on
coordination_type (55%; e.g. `border_normative_status` spans enforcement_mechanism /
identity_coordination / resource_allocation). coordination_type is signature-layer-ACTIVE via the
complexity offset (OQ-30: 48 type flips), so the disagreement is not cosmetic.

**Specific question (the operator's ruling):** if coordination_type is constraint-intrinsic, the
158 disagreements are authored contradictions needing adjudication; if genuinely reading-relative,
it is a seventh authored field (each reading legitimately sees a different coordination function)
and the disagreement is signal. **Do not promote coordination_type into `classify_from_metrics` as
an authored input before this ruling** — it would add authored control over the computed output
(the de-leak principle, in reverse). No-regret items already done 2026-06-05: linter 4→6 fix +
canonical 6-value table + offset-active/floor-inactive asymmetry recorded in
`docs/logic_extensions.md` (commit `29cd45d4`).

**RULED 2026-06-14 (operator, Ω_C/Ω_P) — coordination_type is READING-RELATIVE.** It is a
**seventh authored field**: each reading legitimately sees a different coordination function, so the
158/286 (55%) sibling disagreement is **signal, not authored contradiction** — no adjudication of the
disagreements is owed. **The standing guard HOLDS:** do NOT promote coordination_type into
`classify_from_metrics` as an authored input (it would add authored control over the computed output —
the de-leak principle in reverse). coordination_type stays signature-layer-active via the complexity
offset (OQ-30), which is the reading-relative effect, not a metric-classification input. The
`docs/logic_extensions.md` 6-value table + offset-active/floor-inactive note already frame it this way.

**Hand-up FROM OQ-49 (2026-06-14, `audits/2026-06-14_oq49_remeasure/`) — characterized, not raw.**
OQ-49's split-close re-measured the signature-override layer on live and dispositioned its residual
here under the seat frame. The residual is the **full FNL override-effective union = 14** across the
twins (testsets 0 / haiku 6 / flash 8; snare→TR 0/4/4 + scaffold→TR 0/2/4; ids in
`fnl_survivor_coordination.txt`). It arrives **already characterized: 14/14 carry coord+asym**
(coordination-function AND asymmetric-extraction present), positive-controlled (the coord=0 arm fires
elsewhere — haiku 18 `no_coord` — so the empty coord=0 subset is a fact about the union, not a dead
probe). **What the tag means for the witness-not-verdict pass: these are genuine metric-vs-signature
seat divergence, not laundering** — the structure (coord+asym) is present, the metric clause labeled
it lower (snare/scaffold) by cascade precedence + coordination-blindness, and FNL→TR supplies the
omitted coordination-awareness. The clean-laundering coord=0 pure-extraction subset is **empty** on
live (the human-stakes sub-population the (a)/(b) ruling was reserved for — there is none to rule).
Carry the coord+asym tag across; do not re-derive it. *(Distinct from OQ-74's coordination_type
core — shares the seat frame, lands as a metric/signature-seat residual.)*

**OQ-49 hand-up limb — MOOT (positive-controlled 2026-06-14).** The "coord=0 clean-laundering subset
is empty" claim drops a *reserved human-stakes (a)/(b) sub-ruling* on a 0-count, so it required a
positive control before the limb could be tagged moot ("I didn't find it ≠ it isn't there"). The
README's original control fired the coord=0 and asym arms *separately*; the **conjunction** probe
(`clean_laundering(C) :- has_asymmetric_extraction(C), \+ has_coordination_function(C)`, asserting the
base `constraint_victim`/`constraint_beneficiary` facts since the rule heads are static) is now
positive-controlled on **both twins**: a synthetic coord=0+asym row **is returned** (PC ok), a
coord+asym row is **excluded** (NC ok), and clean_laundering over the FNL override-effective union
returns **0** (haiku 6 ids, flash 8 ids). The empty subset is a real fact about the corpus, not a dead
probe ⇒ the (a)/(b) limb is **moot**. Witness:
`audits/2026-06-14_oq49_remeasure/coord0_conjunction_positive_control.txt`. The 14 coord+asym
override-effective readings remain two-seat metric/signature signal under the OQ-74 core ruling above.

## OQ-75 — Stage-2 corpus rebuild under the de-leaked pipeline: diff distribution + cross-axis invariance correlation

**Ω-type:** Ω_E (the staked prediction is measurable).

**Status:** open — gated on operator go; Stage-1 single-example gate passed 2026-06-05 (KNOWN_STATE.md 2026-06-05)
**Priority:** 1
**Deps:** blocked_on_human operator-go
**Origin:** 2026-06-05 de-leak work item (audit brief §4 Stage 2). The generation pipeline no
longer hands the author the engine's decision boundaries (schema bands stripped `9f2d050a`, prompt
de-leaked `b6c4e113`, retry-path scrub `07f7b1c0`, axes cap optional `7ad86c5a`); the
authored-vs-computed diff is authorable again (witnessed: synthetic claimed-mountain/ε=0.6
validates, compiles, and fires `type_1_false_summit-severe`).

**Specific question:** rebuild a corpus under the changed pipeline and read (a) the diff
distribution — which `false_*` / `dr_claim_mismatch` signatures fire, how often, on what (the
"too-small-a-diff = the author held the key" health check); (b) the cross-axis correlation of
invariance (observer/axiom/time — `the_perturbation_principle.md` §7.1). **Staked prediction:
weak/positive-but-far-from-1; strong cross-axis correlation would falsify the form-unity reading.**
Cite the run manifest. **Sequencing notes:** lens-diversity (`d179423d`) is a separate change
variable — attribute reading-set shifts to it, not the de-leak. ~~Axis-count distribution needs a
re-check (7-7-7 uniform watch)~~ **RESOLVED 2026-06-05 by the SCOPE count-distribution probe**
(`audits/2026-06-05_scope_count_distribution/`): an 8-topic richness-spanning battery × 2 arms
(current prompt vs pre-`d179423d`) shows selected-axis counts 3→11 tracking richness, upper tiers
(T4–T7) spreading among themselves (A: 5/6/6/11), deferrals firing (six non-zero cells), replicate
noise ±1, arms agreeing (lens instruction not pinning counts), T7's 11 axes shown pairwise-distinct
(one borderline composite). The original 7-7-7 was mid-richness coincidence + run noise (bridge
topic gig-economy: 7 originally → 5 in the re-run). Stage-2 is NOT gated on a SCOPE-framing fix;
report the axis-count distribution at scale as a readout, not a gate. **The OQ-76 routing condition is now addressed by construction (2026-06-05): generate-both is
landed — every kernel topic gets a forced-flat control with a mechanical alignment key
(`flat_control_of/2`), so a recognizer miss can no longer cost the axiom axis. Remaining for
this rebuild's analysis design: report the construction-pair diff as its own stratum in the
correlation readout (OQ-76 Remaining).** **OQ-70's bait confound is RESOLVED (operator option A,
2026-06-05; `claimed_natural` source 2 + `appears_as_rope`'s sibling clause removed, `72ec2cdd`) —
no signature now reads a single authored perspective as a story-level claim, so FNL prevalence in the
rebuilt corpus is readable as a detection result (part (a)) and no longer held back by the
single-perspective convention. (Engine-side robustness independently witnessed 2026-06-13: on the
mountain probe the false-foundational rejection rides Boltzmann non-compliance + type_1_false_summit
+ computed type, gates independent of the FNL signature — OQ-117 ENGINE ADJUDICATION.)

**Stability-table dependency (2026-06-12, from OQ-109):** this rebuild's readouts are
cross-story claims, so the stability-table-gates-claims rule (OQ-109 item 4) applies —
Stage-2 analysis design consumes cohort zero's replicate-probe stability table (compare
only draw-stable fields, or size n-per-cohort if the OQ-109 homogeneity falsifier fires).
Cohort zero (~60 stories) is BELOW Tier 1 power and is OQ-109's, not this OQ's — no
OQ-75 verdict rides it; conversely, Stage-2 cohorts share cohort zero's provenance
regime, which is when OQ-109's homogeneity falsifier (item 6) becomes checkable.

**Power tiers for the readouts (recorded 2026-06-05, before the data — pre-register criteria at each tier BEFORE looking):** Tier 1 machinery shakedown ~100 stories (~15 topics; pipeline-holds criterion only, no verdicts; e-digit grid needs >=5 expected/cell). Tier 2 diff-distribution verdicts ~250-300 stories (proportions to +-4-5pp; per-claimed-type cells need ~30+ each). Tier 3 cross-axis correlation ~100-150 KERNELS (~500-600 stories; r=0.3 at 80% power needs ~85 kernels, r=0.2 ~195; the unit is the kernel — only kernels carry the axiom axis and construction pair; bounding r away from 1 is cheap and lands by Tier 2). Tier 4 threshold recalibration (OQ-48) ~700+ (old calibration used 691). e-statistics stay scoped to this one generation regime (OQ-26); never mix archived corpora into denominators.

**Pilot-increment log — never-generated reading-seeds recipe (2026-06-13, branch `corpus-rebuild-fresh`).**
The per-increment rebuild recipe is witnessed end-to-end on the fixed pipeline (commit
`2e3e1998`, cherry-picked onto `main` → cherry `dc12bf5a`; the five provenance/robustness defects).
`pilot_01` = **30 stories / 10 whole kernels** (`constitutional_text_authority`, `biblical_authority`,
`market_as_natural_default`, `acceptable_risk_for_energy`, `provincial_sovereignty_boundary`,
`naskh_principle`, `abrahamic_covenant`, `federation_membership_obligations`, `kodashim_obligation`,
`feud_obligation_kernel`), generated flat no-scope from `outputs/completion_seeds/chunks/pilot_01.json`.
Witnesses (all same-turn): ladder pending 10→30 after the ladder/fossil strip; provenance stamped from
`result_model` (all 30 read `claude-haiku-4-5-20251001`, no fabricated Sonnet/Opus); corpus load 5→35,
zero "Redefined static procedure" warnings (defect-3 multifile: story_provenance facts 1→5 across the
pick, 35 after generation); `run_pipeline` manifest `n_constraints` 5→35; 30/30 generated (7 attempt-1
failures all recovered in attempt 2, no `failures.json`); cost ~$0.87 (Haiku batch).
- **Seed-pool correction:** `build_never_generated_seeds.py` (byte-identical on `main` and the 304
  branch) now emits **1005 readings / 331 kernels**, NOT the plan's remembered 304/101 — the gap is
  pure manifest-pool growth in gitignored `outputs/` since `corpus-rebuild-304` was cut, not a logic
  change. So **remaining ≈ 975 readings / 321 kernels** (`pilot_02..`), NOT ~270. The seed pool is a
  derived artifact of the gitignored manifest pool; a fresh worktree off `main` has zero manifests and
  must regenerate the pool from a tree that has them (here: the `oq117-evidence-block` working tree's
  690-manifest pool), then carve chunks.
- **Quarantine (OQ-58, expected-and-caught, not failures):** the no-scope path SKIPS
  `validate_reading_relation_integrity` — run it manually (done here). 3 dangling edges caught & written
  to `prolog/cs_reading_relation_quarantine.json` (this overwrote 6 stale `employment_boundary` entries
  orphaned on the fresh corpus — last-run-scoped by design): `abrahamic_covenant → christian_supersessionist_reading`
  (the kernel's 4th reading is not in the seed pool — true closure gap, can't expand without authoring a
  seed), and 2× `naskh_principle → {classical_abrogation,contextual_harmonization}_reading` — a
  **generation-side naming drift**: the model appended `_reading` to sibling targets but the reading_ids
  / files carry no suffix, so within-kernel edges that should resolve land in quarantine. Watch this at
  scale; it inflates the dangling count for any kernel whose reading_ids lack a `_reading` suffix.
- **FULL COMPLETION RUN — DONE (operator ruled full completion, 2026-06-13).** The remaining pool was
  carved into 7 chunks (`pilot_02..08`, 50 kernels each except 08's 21) and generated serially (one batch
  at a time, OQ-77), each chunk: ladder-strip (before/after witnessed) → generate → manual OQ-58
  sweep → `run_pipeline` → commit. **Final corpus: n_constraints 5 → 993** (commits `e0a58643`,
  `5a541468`, `615c4445`, `647ba9f2`, `f4c7b13d`, `837723c8`, `441bdbad`, + pilot_08). Of 1005 readings
  attempted: **988 generated, 17 failed** (3-attempt retry exhausted, all NAMED — graceful, defect #1/#5
  holds at scale; ids in `outputs/no_scope_runs/cumulative_failures.json`, gitignored). At-scale load
  witness: 993 testsets load, 993 story_provenance facts (defect-3 multifile holds: one per story),
  **zero "Redefined static procedure" warnings**; provenance models 988× `claude-haiku-4-5-20251001`
  (generated) + 5× `claude-sonnet-4-5` (the main-baseline stories) — no fabrication. Total Haiku-batch
  cost ≈ $27.
- **Residual / forward items for this corpus (not blockers):**
  1. **17 failed readings** to re-attempt (fresh draws) — dominant cause is a **generation-side enum
     violation**: the model emits axiom `status: 'contested'` (valid set `holdable|overridden|foreclosed`)
     — 33 occurrences in pilots 02–03 alone, mostly retry-recovered but the residual failures cluster
     here, plus `not one of ['empirical...']` directionality-kind enums and JSON-parse truncations. A
     prompt/schema note constraining axiom-status to the enum would cut both the failures and the retry
     cost. The 17 partial-kernel gaps also leave their siblings' edges dangling (quarantined).
  2. **Naming-drift quarantine class** (≈6–17 edges/chunk, all CAUGHT not crashed): the model mangles
     sibling-edge targets — appends `_reading` where the file has none (`naskh_principle`), uses single-
     underscore (`us_constitution_interpretive_originalist`), or drops the kernel prefix
     (`bitcoin_electronic_cash_reading`). Within-kernel edges that should resolve instead land in
     `prolog/cs_reading_relation_quarantine.json` (which is last-run-scoped — it currently holds only
     pilot_08's 6 edges; the per-chunk sets are in each chunk's commit). Disposition is OQ-58 (reviewed,
     not auto-rewritten). A target-normalizer at emission (strip/normalize the suffix, re-prefix) would
     resolve most without a redraw.
  3. **Grid first-contact gate:** one firing across the whole run
     (`dueling_disappearance_mechanism__contraction_reading`, indicator `C-dir`, pilot_04) — REGENERATED
     per the increment-0 operator ruling (not waived); the fresh draw passed. Expect occasional firings on
     redraws/new chunks; regenerate, don't waive.
  - Throwaway driver `agent/_pilot_ladder_strip.py` (ladder strip + witness) is committed for reuse; not
    pipeline-wired. Branch `corpus-rebuild-fresh` is UNMERGED to `main` per operator ruling (2026-06-13).

**TWO-MODEL TWIN CORPUS — Haiku + Gemini Flash (2026-06-13, operator-ruled).** The same 1005-seed pool
was generated by a SECOND model (gemini-2.5-flash) via `agent/run_no_scope_gemini.py` — a faithful
kernel-aware port (reuses `build_cached_messages` + `process_batch_results` verbatim; only the batch
API/provider and destinations differ; `thinking_budget=0` for parity with Haiku). Flash run: **968/1002
+ 3 validation = 971 generated, 34 failed** (higher than Haiku's 17 — Flash is weaker at the strict
JSON schema, same enum-violation class). Measured tokens input=38.1M output=4.36M; the script's
`token_acc` print (~$22, interactive full-input rate) is an UPPER BOUND — real billed cost is far lower
(batch −50% + context-cache on the 31k-token prefix; Flash output is very concise, ~4.5k/story).
- **Final corpus layout (reconciled by filename):**
  - `prolog/testsets_haiku/` (960) and `prolog/testsets_flash/` (960) hold the **intersection** —
    stories BOTH models generated, paired by filename, byte-identical membership (verified
    `set(haiku)==set(flash)`, 0 mismatch either way). This is the controlled two-model comparison set.
    JSON specs mirror into `json_haiku/` and `json_flash/`.
  - `prolog/testsets/` (44) holds the unmatched + Sonnet: 28 Haiku-only (Flash failed) + 11 Flash-only
    (Haiku failed) + 5 Sonnet baseline. This is the STANDARD location, reserved for the c-orchestrator
    essay corpus the operator is building; specs in `json/`. run_pipeline on the default glob now
    classifies these 44 (n_constraints=44, clean, no grid-gate firing).
  - **Loading a twin for analysis needs a corpus_path overlay that RETRACTS the default first**
    (param/2's default `testsets` clause is found first): `retractall(config:param(corpus_path,_)),
    assertz(config:param(corpus_path,'testsets_flash'))` before `load_all_testsets`. Witnessed: each
    twin loads 960 testsets / 960 story_provenance facts, no errors. NOTE: run_pipeline's JSON_DIR is
    hardcoded to `json/`, so classifying a twin via overlay does not pick up `json_haiku/`/`json_flash/`
    specs — a twin-comparison harness must point the json source at the matching mirror.
- **Failure records:** Haiku `outputs/no_scope_runs/cumulative_failures.json`; Flash
  `outputs/no_scope_runs_flash/failures.json` (34) + run log. The Haiku-only/Flash-only split IS a
  finding: each model rescued ~half the other's failures (e.g. Flash got the jewish_sovereignty /
  historical_treaty stories Haiku failed; Haiku got bitcoin_whitepaper_purpose Flash failed).

## OQ-76 — Kernel/flat gate is stochastic on a real boundary band: routing noise lands in Stage-2's cross-axis correlation

**Ω-type:** Ω_E (the band is measured; the cause is K2-testable), with an Ω_P edge (which fix, and whether the interim hedge ships before Stage-2, are operator rulings).

**Status:** mitigated — generate-both (operator-promoted to PRIMARY fix, 2026-06-05) is BUILT and witnessed end-to-end; the **kernel-first router** (2026-06-06, OQ-79) further removes architectural RELIANCE on the free gate: the kernel question is now PRIMED (asked on every topic) and the verdict is kernel-liberal, so a coin-flip in free decomposition no longer decides routing. NOTE: the specific band topics (gig classification, platform moderation) have NOT been re-measured under priming — Phase 0 measured different topics; re-running the band under the primed prompt is the residual K2-adjacent check. Remaining: Stage-2 readout stratum (analysis-side) + optional K2 cause probe
**Origin:** 2026-06-05 SCOPE count-distribution probe side-observation, promoted by operator review:
T5 (gig economy) decomposed as a kernel in one arm and flat in the other — same topic, same temp.
Diffing the manifests showed the SAME contested substrate (the legal-classification contest) routed
onto the axiom axis (kernel: employee/contractor/hybrid readings) vs the observer axis (flat axis
`classification_test_collision`). This is not count noise: a flat take destroys the axiom axis
irrecoverably (no readings, no groundings to diff), while a kernel take still carries observer
perspectives — the miss direction is the unrecoverable one.

**Evidence (`audits/2026-06-05_kernel_gate_replication/`, 40/40 calls, pre-registered invalidation
conditions):** P(kernel) per topic at k=8 — drive-on-right 0/8 and personhood 8/8 (controls at
required extremes ⇒ instrument valid, gate NOT noisy-everywhere); affirmative action 8/8;
gig classification **5/8**; platform content moderation **3/8**. The band is real and topic-classed:
famous moral kernels stable, statutory/regulatory contests near coin-flip. Hand-adjudication
against §1.3-K: BOTH band topics pass all three criteria (X1: different victim sets per reading;
X3: `state_action_boundary` with formalist/entanglement/public_function readings) — the flat takes
are under-firing MISSES against explicit criteria, not definitional ambiguity. Noise is localized
to the binary gate: conditional on firing, reading counts are perfectly stable (4/3/3 per topic
across all runs).

**Specific question:** what makes the gate under-fire on §1.3-K-passing topics — salience-driven
recognition (fires on culturally famous contests, misses statutory/technical ones) vs phrasing
sensitivity vs something else? **K2 (licensed, not run):** hold a band topic's content fixed,
perturb topic phrasing (commitment-foregrounded / system-description / mechanism-only), k=5 each;
if P(kernel) tracks phrasing, normalize Stage-2 seed phrasing or move routing out of free
decomposition.

**Resolution (2026-06-05, operator-ruled then built — `audits/2026-06-05_flat_control_generate_both/`):**
generate-both promoted from fallback to PRIMARY (ruling rationale: both cheap mitigations —
P(kernel) stratification and the kernel-bias hedge — route through the same broken bit, a
salience-driven detector; generate-both makes the recognizer REDUNDANT instead of trusted, and
K1's localization says the expensive unreliable thing is exactly the binary decision while
everything downstream is stable). Implemented asymmetrically: flat control on EVERY kernel
(`<kernel_id>_flat_control` seed auto-emitted in `flatten_manifests`; substrate-only prompt —
the reading set is never shown; mechanical alignment key
`narrative_ontology:flat_control_of/2` emitted by `generate_constraint_pl.py` outside the
cs_structure gate; never `cs_kernel_id` — flat controls are NOT readings, so kernel statistics
and the OQ-58 sweep are untouched). Do NOT generalize to kernel-on-every-flat. Witnessed
end-to-end on run-tag `flatctl_probe`: first construction-pair diff = construction-robust at
the computed layer (tangled_rope ×4 seats, both constructions), divergent at the authored layer
(snare ε=0.65 vs tangled_rope ε=0.48) — the two-layer §7.1 datum the instrument exists to take.

**Remaining (keeps this OQ from closing):**
- **Stage-2 readout stratum (analysis-side):** report construction-pair diffs (computed-type
  agreement per seat; authored divergence) as their own stratum in the cross-axis correlation
  (OQ-75). The interim kernel-bias hedge is SUPERSEDED — the gate no longer determines whether
  the axiom axis exists.
- **Optional K2 cause probe** (phrasing sensitivity): still licensed, now diagnostic-only
  (informs whether the recognizer's salience bias also distorts WHICH topics get kernels at
  all — a corpus-composition question, not an axis-loss question).

## OQ-77 — giant_component_analysis SIGSEGV (rc=-11) under concurrent pipeline runs

**Ω-type:** Ω_E (closeable by a controlled serial-vs-concurrent repro).

**Status:** resolved — kill-condition run 2026-06-10: no serial recurrence at any tested size; ruled a concurrency artifact of the three-concurrent-pipelines regime. Operational rule recorded: **never run concurrent full pipelines / topic runs against the shared `prolog/testsets/` + `outputs/`** (promoted to CLAUDE.md Running the System).
**Origin:** 2026-06-06 slow-build session — three topic runs launched concurrently; `giant_comp` (`run_pipeline.py:416`) segfaulted (`rc=-11`) on exactly the maximally-overlapping run (leiden, n=39, manifest 00:10:49Z) and passed at n=43/n=33.
**Resolution evidence** (`audits/2026-06-10_oq77_serial_kill_condition/`): serial 10/10 rc=0 at n=39 — the exact crash size — with byte-identical outputs; 12 simultaneous co-resident invocations 12/12 rc=0 (pure co-residency ruled out); serial archive runs kernel_v1 n=1106 rc=0 and original_v6 n=3380 ×3 byte-identical complete reports (8,785-node component BFS — deterministic topology/stack-depth defect ruled out to 87× crash size). The exact crashing corpus is unreconstructible (transient mid-generation testsets state). Mechanism inside the concurrent regime remains unidentified (the mutating prep-phase interleave was not simulated); if a segfault ever recurs under SERIAL operation, reopen via the kill-condition's "recurs serially" branch with this audit as baseline.
**Carry-forward:** the triage-list premise stands independently of this close (build_discipline.md rung-4: never cite a corpus statistic from a concurrently-built run — the manifest must be from one coherent run). Side-finding filed as OQ-95 (phantom network nodes from dangling `affects_constraint/2` targets, witnessed during this audit's probe validation).

## OQ-78 — The ε authoring idiom: a 0.68 mode + 8/2 last-digit grid compresses the ε perturbation axis

**Ω-type:** Ω_E (a measurement caveat on the corpus, quantifiable and trackable as it grows).

**Status:** partial — three-fate split RATIFIED 2026-06-12 (one amendment: free-gate residual →
**Priority:** 1
OQ-117 at filing): quantization half CLOSED working-as-designed for SCOPE-stage statistics
(ε-bin is 4-level by construction THERE); idiom half OPEN, re-baselined on cohort zero;
independence circularity ESCALATED to OQ-117. **Probe HALTED pre-spend same day (greenlit, then
killed in pre-flight recon): the epsilon_bin channel is DEAD at the generation interface — no
production path feeds the bin to the authoring model, so the withheld arm ≡ production and the
contrast cannot run as designed** (witnesses W1–W3:
`audits/2026-06-12_oq78_dead_bin_channel/`). Fate-2 graduation RE-ROUTED to the zero-spend
cross-arm read over OQ-109 Phase C (see probe block). Original "NOT a leak" claim REVISED — the
historical leak was the PRE-de-leak prompt's numeric type-bands (scrubbed at b6c4e113,
2026-06-05); the SCOPE bin-boundary disclosure survives but reaches generation only indirectly
(via hypothesis co-authoring). Boundary-ancestry question ruled ARCHAEOLOGY — not chased.
**Origin:** n=91 build assessment (2026-06-06, commit `0fd22ea`, manifest run 03:50Z — coherent
snapshot, 91 .pl = 91 .json = manifest n). Flagged as a watch at n=20 (`0.58` interior anchor gone,
new `0.68` mode emerging); escalated to a finding at n=91.

**The observation:** authored `base_extractiveness` clusters hard. At n=91: **31/91 stories sit at
exactly ε=0.68**; the mode stack is 0.68(31), 0.58(12), 0.48(11), 0.28(10), 0.08(6) — only 13
distinct values over 91 stories; the last-digit grid is **8: 78, 5: 7, 2: 6** (86% end in 8). The
model authors ε on a coarse 0.1-spaced grid biased to the `.x8` rail, with a strong central mode.

**Why it matters (and why it is not a defect):** ε is one of the perturbation axes. A third of the
corpus sharing one ε value makes the ε axis **low-variance**, so claimed-vs-computed divergence and
χ are driven by directionality f(d) and scope σ(S) **more than by ε**. Any ε-binned Stage-2 statistic
(the digit-grid health check, ε-stratified divergence rates) inherits this compression — bins will be
sparse and lumpy. It is the AUTHORING regularity the perturbation principle expects to read off, not
an engine fault: the engine recomputes correctly on whatever ε is authored. But it is a standing
caveat on every ε-keyed denominator, and a candidate signal in its own right (does the model's ε mode
track topic, claimed-type, or nothing?).

**Distinguish from OQ-26 (ε reading-relative across runs):** OQ-26 is about ε varying for the SAME
topic across generation runs; OQ-78 is about ε CLUSTERING across DIFFERENT topics within one run.
Orthogonal.

**What would resolve / act on it:** (a) track the mode fraction and distinct-value count as the
corpus grows — does 0.68's share fall toward a spread (idiom diluting) or hold (idiom entrenched)?
(b) cross-tab ε-mode against claimed_type and topic to see whether the clustering is structural
(0.68 = "the model's default extractive value") or carries signal; (c) if entrenched and signal-free,
it bounds the usable resolution of the ε axis — report ε-based statistics at grid resolution, not
continuous. Do NOT "fix" it by disclosing target ε values to the author (that is the de-leak in
reverse — re-collapses the diff). If anything is steered, it is via topic/source-domain selection.

**Evidence update (2026-06-12, kernel_v2_test2 n=60 + live cohort-zero n=5; ruling PENDING — operator's call):**

- **(a) trajectory:** 0.68 share 34% (n=91) → 30% (n=60); distinct values 13 → 13; last-digit-8
  86% → 77%. Stable — but the n=60 set is the curated *survivor* of the n=91 corpus (dispositions/
  OQ-105 archivals), not growth, and the cohort-zero swap (2026-06-11) retired the whole series.
  No true growth datapoint exists; the watch needs RE-BASELINING on the cohort-zero regime (live
  n=5: 0.68×1, .x8 rail 4/5 — too small to read).
- **(b) cross-tab — ANSWERED, and it reframes the OQ:** ε tracks claimed_type in nearly separable
  bands: mountain (n=9) 0.02–0.15, 0/9 at 0.68; rope (n=10) 0.08–0.28 (3 exceptions at 0.48/0.68
  are kernel-reading stories — flat_control, techno_optimist_reading, post_1998_convergence —
  authored perspectival divergence, not noise); tangled_rope (n=23) 0.42–0.68; snare (n=17)
  0.68–0.78, only 3 distinct values; piton (n=1, regime_change_structural_break) 0.28. Denominator
  reconciled: 9+10+23+17+1 = 60. 0.68 = "the model's number for a high-extraction type," not a
  free default. The GENERATION prompt is clean of all threshold numbers (grep for
  0.68/0.55/0.56/0.45/0.46, positive control fired on a known number) — but see the leak re-check
  below: the SCOPE surface is not clean.
- **Mechanism found — the compression is two-layer:** (1) PIPELINE-DESIGNED quantization: SCOPE
  co-authors `epsilon_bin` ∈ {v_low, low, mod, high} (numeric boundaries disclosed at the SCOPE
  stage only, `prompts/uke_scope_v2_json.md:292`) alongside `hypothesis` (→ claimed_type); the
  generation prompt maps bin → `base_properties.extractiveness`
  (`prompts/constraint_story_generation_prompt_json.md:756`). Where a bin is recorded: conformance
  15/15; within "high", 8/13 at exactly 0.68. (2) MODEL IDIOM on top: the in-bin point mass and the
  .x8 just-under-round-number rail (0.08|0.10, 0.28|0.30, 0.48|0.50, 0.78|0.80) — no surface
  discloses these. CAVEAT: 41/60 stories carry NO recorded bin yet show the same grid (0.68 at
  22%); the `uke_scope` block is informational/optional, so bin-conditioning is unrecoverable for
  them (absence of record ≠ absence of conditioning — the provenance bit is missing). Side finding:
  4 stories carry free-text bin tokens (`moderate_high`, `moderate`, `negligible`) outside the
  schema enum — the informational block is unvalidated.
- **Implications:** ε-stratified statistics are claimed_type statistics in disguise (within-type ε
  variance is 2–5 values); claimed-vs-computed divergence is f(d)/σ(S)-driven — now with mechanism,
  not just observation. The generation prompt's "Claim/Metric Independence" checklist item is
  structurally undermined whenever SCOPE feeds both `hypothesis` and `epsilon_bin` (co-authored
  upstream, both received by the generator). Never cite the ε↔claimed_type correlation as a
  detection result — it is authoring convention (OQ-70-analog).

**Leak status REVISED (re-check 2026-06-12, against the new mechanism):** "NOT a leak" no longer
holds as stated. Two of the three interior bin boundaries disclosed at
`prompts/uke_scope_v2_json.md:292` coincide EXACTLY with config classifier thresholds: **0.10 =
`piton_epsilon_floor`** (Rule Z, `config.pl:293`) and **0.30 = `tangled_rope_epsilon_floor`**
(`config.pl:270`). The third boundary (0.55) matches nothing — `grep 0.55 config.pl` is empty
(control: the same grep family found 0.45/0.46/0.30/0.10 lines), so the rope/snare split
(`rope_epsilon_ceiling` 0.45 / `snare_epsilon_floor` 0.46) is NOT transmitted: the "mod" bin
(0.31–0.55) straddles it and that decision boundary remains free. Consequence for bin-conformant
stories: a "low" assignment guarantees ε > 0.10 (Rule Z pre-satisfied); "mod"/"high" guarantees
ε > 0.30 (tangled floor pre-satisfied) — concordance on those two gates is manufactured by
construction (→ OQ-117). Derivation direction (config-copying vs shared ancestry with the
logic.md zone structure) ruled ARCHAEOLOGY 2026-06-12 — not chased: the disclosure's effect is
identical either way, and it reaches the SCOPE bin-assigner only — generation still sees just the
token.

**Ruling (RATIFIED 2026-06-12, operator; one amendment: free-gate residual added to OQ-117 at
filing) — a SPLIT, not one status:**

1. **Designed-quantization half — close as working-as-designed.** ε resolution is ~4 levels by
   construction wherever the manifest path feeds a bin (and ~2 levels conditional on
   claimed_type). Report ε-keyed statistics at bin resolution; sub-bin precision is unsupported.
   Provenance hardening rides along: `uke_scope.epsilon_bin` mandatory-when-manifest-fed +
   schema-validate its enum (4 free-text tokens in the wild).
2. **Idiom half — stays OPEN, re-baselined on the cohort-zero regime.** The within-bin point mass
   at 0.68 and the .x8 rail are disclosed nowhere and persist into the new regime UNTESTED (live
   n=5: rail 4/5). The old regime's tight bands do NOT carry forward automatically — the live
   corpus already band-breaks (institutional_trust_erosion_c0: claim=mountain, ε=0.68 — the same
   story behind OQ-116's MOUNTAIN_METRIC_CONFLICT firing; the old regime had mountain 0/9 at
   0.68). Graduation step = the bin-withdrawal probe below.
3. **Independence circularity — ESCALATED to OQ-117** (own entry, not a paragraph here): SCOPE
   co-authoring `hypothesis` + `epsilon_bin` is a validity condition on the divergence machinery,
   not a measurement caveat on this axis.

**Bin-withdrawal probe: HALTED PRE-SPEND 2026-06-12 (greenlit at 15/arm, killed in pre-flight
recon; zero API calls).** The pinned design assumed production generation feeds `epsilon_bin`
and the withheld arm isolates it. Pre-flight witness: NO generation path passes the bin — the
unified backend (`story_generator_base.py:203` axis_source_desc), the gkc kernel path
(`generate_kernel_corpus.py:380` build_cached_messages), and the c-orchestrator inline path
(`c-orchestrator.py:607`) all feed `Hypothesis type` (the CLAIM side) and never the bin; the
only `epsilon_bin` consumers are two streamlit display lines; the prompt's UKE_SCOPE mapping
table is instruction-without-data. The historical numeric channel was the PRE-de-leak prompt's
type-band table (ε ≤ 0.25 mountain / ≥ 0.46 snare / ≥ 0.30 tangled — the config thresholds
verbatim), scrubbed at commit b6c4e113 (2026-06-05); every post-reset story was authored with
NO numeric ε instruction and NO bin token. The recorded uke_scope blocks are MODEL-FABRICATED
(no code writes them; free-text bin tokens; fabricated generated_date values incl. a future
date), so the earlier 15/15 bin-conformance is self-labeling, not instruction compliance.
Full witnesses W1–W3: `audits/2026-06-12_oq78_dead_bin_channel/README.md`. Halt-and-escalate
applied per the pre-registration discipline: a wrongly-premised pinned probe is halted, never
inline-amended into a different experiment.

**Fate-2 graduation, RE-ROUTED (zero marginal spend): cross-arm read over OQ-109 Phase C.**
The cohort-zero regen re-authors the SAME 60 archive seeds from title/domain/summary ONLY (no
hypothesis, no metrics — the declared seed spec in `agent/cohort_zero_regen.py`). That is a
withheld arm on matched seeds BY CONSTRUCTION; the kernel_v2_test2 archive (n=60,
hypothesis-fed) is the fed arm and the COMPARATOR (per the operator's correction: the archive
shares are context for magnitude — labeled: rail 86% at n=91 / 77% at n=60; exact-0.68 ~30% at
both — the test is cross-arm on matched seeds). Endpoints unchanged: rail share, exact-0.68
share. Outcome semantics carry over with one widening, stated honestly: Phase C withdraws MORE
than hypothesis (structural_delta, beneficiary/victim, CSR too — full-manifest withdrawal), so
PERSISTENCE there establishes idiom a fortiori; COLLAPSE leaves hypothesis-vs-rest unresolved
and would motivate a finer hypothesis-only arm (new design, new spend, operator call then —
that narrower contrast is what the halted probe should have been). Pre-noted partial signature
unchanged: institutional_trust_erosion_c0 (claim=mountain, ε=0.68; OQ-116's lint firing) breaks
the type-coupling while LANDING on the rail — n=1 of 5, hypothesis-pointer, recorded before the
Phase C read so it cannot be promoted to confirmation after. OQ-117's design call remains
SEQUENCED AFTER this read.

**Direction-of-fix discipline (carried + extended):** still no disclosure of target ε values to
the author; ADDITIONALLY no tightening of bin boundaries toward config thresholds (e.g. moving
0.55 to 0.45/0.46 would sharpen the very transmission channel the leak re-check interrogates).

## OQ-79 — c-orchestrator recognizes kernels but silently drops their readings; flat-entry topics never engage the kernel question

**Ω-type:** Ω_E (the loss is witnessed and counted) + an Ω_P design call (should the flat-entry path hold the kernel-vs-flat perturbation at all, or delegate).

**Status:** resolved — both mechanisms closed. mechanism-1 (recognized-then-dropped): c-orchestrator generates recognized kernel readings via the unified backend (P3, commit a7d56a14; witnessed: Zionism's 3 readings land with cs_kernel_id). mechanism-2 (never-engaged / flat-miss): the **kernel-first router** (2026-06-06) makes `_step_decompose` use the PRIMED scope prompt — reused from gkc `_scope_user_prompt` (single source, both front-ends share uke_scope_v2_json.md §1.3-K) — so EVERY topic now engages the kernel question, replacing the old unprimed §3-independence prompt that never asked. Witnessed end-to-end via the front-end (`--dry-run --skip-search`): magnifica → `is_contested_kernel=true`, 3 readings, where the unprimed path flattened it to 12 axes; flat topic → `is_contested_kernel=false` (reasoned rejection: "kernel hypothesis tested and correctly rejected"). Phase 0 (`outputs/kernel_first_phase0/PHASE0_READOUT.md`) characterized the primed verdict as **KERNEL-LIBERAL**: routes to kernel whenever a foundational reading is *constructible* (= the topic is contentful, `docs/seat-theorem-v1.md` Coupling Theorem), flat only when the situation settles it (ISO-8601 / drive-on-right held flat by reasoning; loud means-disputes reading-wars/nuclear routed kernel). Operator ruling **LIBERAL** (2026-06-06), two conditions: (1) kernel-positive label **DEMOTED** to "admits a foundational construction" — dominance UNJUDGED, never "certified dominant kernel" (a downstream count/headline/essay reading it as dominant commits the seat-theorem no-seat pose, since no seat-free dominance ranking exists — §6); (2) promotion policy = **ACCRUE UNCURATED** (a *seated* dominance stage is permitted but DEFERRED — design it against a witnessed pile, not blind). A3 grounding-leg **DROPPED**: Phase 0 showed it's the wrong instrument (over-routed readings have real constituencies → grounding confirms, not flags). The eyeball (`coherence_eyeball`) is the **liveness** backstop (catch a contentless topic fabricated as contentful), NOT a dominance gate. gkc `--scope` unification still partial (OQ-82).
**Origin:** 2026-06-06 n=91 corpus review. Operator observed that fed topics with obvious kernel
structure (Zionism, germline editing) came out flat. The persisted flat-run manifests
(`outputs/kernel_manifests/flat/`) split this into TWO mechanisms:

1. **Recognized-then-dropped (a bug).** Zionism: SCOPE returned `is_contested_kernel=True`,
   `kernel_id=zionist_legitimacy_basis`, 3 readings (national_liberation / settler_colonial /
   religious_restoration), and emitted all three into `generation_sequence`. But kernel readings
   live in `commitment_system_recognition.readings`, NOT `manifest["axes"]`, and
   `_step_generate` resolves only `axes` → the 3 readings hit "Axis … not found, skipping" and
   were **silently discarded**. The 6 flat supplementary axes landed (`british_mandate_scaffolding`,
   `transfer_doctrine`, … are in the corpus); the contested readings — the point of reading Zionism
   structurally — evaporated while the run reported success. Absence-presenting-as-presence at the
   generate seam.
2. **Never-engaged (OQ-76 in the wild).** 13/15 flat manifests have NO `csr` block at all — the
   kernel question never fired. germline editing, debt limit, healthcare, embryo: all plausibly
   contestable, all flattened without the gate engaging. This is OQ-76's measured salience-driven
   under-firing, now observed on real topics.

**Design frame:** kernel-vs-flat is itself a seat — the same topic admits both constructions and the
diff between them is signal (`the_perturbation_principle.md` §7.1, one level up from the within-kernel
flat control). The flat-entry path (c-orchestrator) currently neither holds that perturbation (it
under-engages it, mechanism 2) nor handles the kernel branch when it does engage (mechanism 1).
Generate-both (OQ-76) covers the within-kernel direction only; the flat-miss / dropped-kernel
direction — the unrecoverable one — is uncovered.

**Mitigation landed:** mechanism-1 silent loss → a LOUD, distinct, ledgered signal
(`KERNEL_RECOGNIZED_BUT_UNDELIVERABLE` in `generation_failures.jsonl`; the persisted manifest makes
it recoverable via `generate_kernel_corpus --scope`). The "skipping" line no longer reads as benign.

**Open (the operator's design call):**
- **Mechanism 1 fix — deliver vs delegate.** Either (a) c-orchestrator generates kernel readings
  itself (ports gkc's KERNEL CONTEXT prompt + flat-control + integrity machinery — risks the
  silent-fork, Pattern 2), or (b) on `is_contested_kernel`, c-orchestrator auto-delegates the
  topic to gkc's `--scope` flow (reuse, no fork; needs the free-text topic → kernel-seed bridge).
  Recommend (b).
- **Mechanism 2 — curate vs improve the gate.** Near-term: for topics the operator judges to be
  kernels, assert via `kernel_seeds.json` + `--scope` rather than trusting the gate (it is
  measured-unreliable, K1). Longer: K2 (phrasing-sensitivity, OQ-76) diagnoses why it under-fires.
- **Recoverable now:** the dropped Zionism kernel's readings are in its persisted manifest — re-run
  that topic through gkc to capture them; the 6 flat Zionism axes already in the corpus can stay
  (they are a legitimate flat construction — keeping both IS the topic-level construction pair).

## OQ-80 — generate-step token totals are unthreaded through the unified backend: reports 0, meaning "not measured"

**Ω-type:** Ω_E (a measured quantity that is currently unmeasured-but-presented-as-zero).

**Status:** resolved — (2026-06-09, the deferred fix built as specified). `process_batch_results`
takes an optional `token_acc` mutable out-param (None = NOT measured, never 0; 4-tuple signature
intact for gkc CLI callers); usage summed at receipt (spend is real even if the save fails);
`generate_from_manifests` forwards it per wave; `_step_generate` reads it onto the StepResult and
the "unthreaded — reported as 0" runtime note is replaced with actual counts. Witness:
`python/tests/test_token_acc_threading.py` (3 cases: summed-at-receipt incl. parse-failures;
errored-only leaves 0; `token_acc=None` path unchanged) — all pass 2026-06-09.
*(body compressed at close per footer rule)*
**Origin:** 2026-06-06 backend-merge P3. The old `_step_generate_batch` summed batch-result `usage`
into the StepResult; the unified backend delegated result-processing to `process_batch_results`,
which consumed the iterator without returning usage, so `_step_generate` reported a hard 0 —
absence presenting as a measured zero (Build-Discipline spine).

## OQ-81 — Do kernel readings make sound wave-upstreams for supplementary axes? (no — suppressed)

**Ω-type:** Ω_E (generation-quality; answered by a pre-registered A/B).

**Status:** resolved — operator-ruled SUPPRESS (2026-06-10); wired + payload-witnessed in the same commit.
**Origin:** 2026-06-06 backend merge (P4, commit `ed2ec212`): the unified backend wave-partitions kernel manifests, so a supplementary axis `downstream_of` a READING received that reading story's claimed_type as upstream context — an input the §5.1 wave logic (designed for flat upstreams) was never validated against.
**Resolution (evidence: `audits/2026-06-10_oq81_reading_upstream_recon/` — RECON, AB_PLAN, AB_RESULTS, WIREUP, raw runs):** Recon first established zero exposure to date (no story in any corpus was ever generated under reading-verdict injection) and that the reading-edge population is archive-format only. The A/B (3 arms × 3 reps, exact pipeline params, injected verdict deliberately ≠ axis hypothesis) found **verdict import occurred in the gradable channel and was absorbed before the categorical one**: claimed_type held (9/9 snare — a real null, positive-controlled by R-arm prose explicitly reasoning about the injected verdict) while the three-line verdict block pulled authored theater_ratio 0.690→0.513 with zero range overlap (K≈N), toward the injected verdict's profile. Discovered en route: `axis_source_desc` already injects the verdict-free kernel CSR into every supplementary-axis prompt, so kernel substrate needed no new wire (fix (a)≡(b)) and the decision reduced to one bit.
**Wire:** reading-typed deps suppressed at seed build (`_flat_seeds_from_manifest` — both the seed's wave deps and the axis copy `upstream_context` reads) + the same predicate in the serial escape hatch. Witness: germline (8 flat edges, 5 waves) byte-identical pre/post — flat §5.1 injection preserved; dutch+supp kernel manifest 4/5 payloads identical, the 5th losing exactly the three verdict lines (`wireup/diff_dutchsupp.txt`). Serial-path edit predicate-synced by code-read, NOT payload-witnessed; legacy app orchestrators untouched/out of scope. Kernel-CONCEPT-typed deps (current SCOPE format, 21/21 dangling) stay inert BY DESIGN — their substrate already arrives via CSR.
**Standing cautions:** (1) channel asymmetry as a general injection finding — categorical authored fields anchored by an explicit hypothesis are sticky; continuous fields absorb the pull (n=3, one axis/kernel/verdict — an instance, not a corpus effect size). (2) The CSR line poisons vocabulary-based leakage probes in ALL arms (the A/B's metric-4 confound, recorded designed-and-invalid) — future leakage probes must key on tokens present ONLY via the injected block.

## OQ-82 — gkc --scope entry point not yet routed through the unified backend (partial unification)

**Ω-type:** Ω_P (cleanliness/one-path design) + Ω_E (the gkc-kernel wave change is witnessable).

**Status:** open — the BUG (c-orch dropping kernels) is fixed (OQ-79 mech-1); this is the remaining
**Priority:** 1
cleanliness/enhancement step, deliberately NOT rushed at the end of the merge session.
**Origin:** 2026-06-06 backend merge. P3 routed c-orchestrator through `generate_from_manifests`
(the unified backend). gkc's `--scope` flow (`main()`, the legacy run-tagged path) still runs its
OWN generation: `flatten_manifests` → `build_batch_requests` (ONE batch, NO waves) →
`process_batch_results` → grouping/stamp/integrity/contradictions. So two generation
implementations still coexist; the silent-fork BUG is gone (both handle kernels), but the literal
one-path goal is not yet met.

**What the rewire does (a real behavior change to a WORKING path — witness, don't assume parity):**
route gkc `--scope` through `generate_from_manifests(model=GEN_MODEL, system=<gkc list>, ...)`. Three
deltas: (1) gkc kernel runs GAIN waves (supplementary axes with reading-deps move to a later wave —
the 176/166 finding; the P4 mechanism is already witnessed deterministically, commit ed2ec212);
(2) supplementary-axis framing shifts from gkc's `_axis_summary` to c-orch's `axis_source_desc`
(intended consistency — flat axes get flat framing regardless of entry); (3) re-route through one
backend.

**Integration seams to handle cleanly (why it's its own pass, not a tail-of-session wire):**
- `generate_from_manifests` already emits `emit_axiom_contradiction_facts`; the `--scope` flow emits
  them separately AFTER `coherence_eyeball` → must remove the duplicate or double-emit.
- `kernel_grouping.json` + `coherence_eyeball` wrap the generation block → keep them around the
  `generate_from_manifests` call.
- Witness: a small live kernel-seed run (1 kernel) → readings + flat controls + integrity sweep
  produced, AND the wave change visible (supplementary axes after readings); framed as a CHANGE
  from kernel_v1 (which had no waves), never "as before." Safe now: live corpus has zero kernels.

**Why deferred (operator-relevant):** the c-orchestrator fork — the actual defect — is healed and
witnessed. gkc `--scope` was never broken (flatctl_probe/stage1_probe used it successfully). This
step is cleanliness + an enhancement on a working path, which deserves a focused witnessed pass.

**Priority raised by the OQ-81 merge (operator, 2026-06-10):** the OQ-81 suppression of
reading-typed wave-upstreams lives in the unified backend (`_flat_seeds_from_manifest`), which
makes the unified backend the place where INJECTION POLICY lives — and gkc `--scope` is now the
only generation route not through it, i.e. the only route the suppression predicate does not
govern. Its wave-free design probably never injected verdicts, but "probably never" is a
code-read away from "confirmed never"; the rewire closes the cleanliness goal AND the last
injection-governance gap in one move. One ungoverned path is worth more than its size — take
this next over other open items. (The serial escape hatch in c-orchestrator got the predicate
directly, code-read-synced; the Streamlit-era app orchestrators remain out of scope and could
re-inject if resurrected — the suppression lives in the backend, not the corpus.)

---

## OQ-83 — Stakeholder-layer migration: replace the four-tuple authoring surface via the five/six-questions interview (the engine's first framing-perturbation)

**Ω-type:** Ω_C (role-set-as-declared-frame — A4-settled with residue ledger) + Ω_E (cross-framing
independence) + Ω_P (Type C vs B — transferred, see close note).

**Status:** resolved — measurement close-out (operator-scoped, 2026-06-11). All three Ω-types
answered or relocated; engineering follow-through spun off: **OQ-109** (Phase B cutover + Phase C
regen/retire), **OQ-110** (offline residual join + author-vs-derive D-fork). Evidence:
`audits/2026-06-07_stakeholder_layer_migration/` (Phase A steps 1–4, AUDIT.md, MIGRATION_PLAN.md,
STEP4C_PARTITION.md, TWO_AXIS_NOTE.md) + `audits/2026-06-11_oq83_close/` (Step-1 classifier-sync
witness; close session). Full investigation narrative: git history of this entry.

**Close note (per Ω):**
- **Ω_C SETTLED:** role-set-as-declared-frame ruled at A4 (85.0% alignment, middle band,
  proceed + residue ledger; contender 6.3% stays in the ledger as declared evidence that the
  frame treats contention relationally — contention is DERIVED, never authored).
- **Ω_E MEASURED:** the straitjacket WAS escaped for everything `in_contention` exists to detect
  (the 4b gate fired on a non-problem; OQ-85 silence-is-correct). Computed classification
  survived every same-grain comparison in the 4c pilot (n=6, ε-pinned, bin-blind: 2 survived /
  2 flipped / 2 unevaluable; both flips dissolved per-flip into victim-count ×
  `critical_mass_threshold` resolution, not framing). Robust separate signal: claim-layer
  framing effect 3/3 (stakeholder arm claims rope, four-tuple claims tangled_rope; the engine
  corrects both to snare) — model-stable (3/3 under Sonnet, identical to Gemini).
- **Ω_P TRANSFERRED, not answered:** observer-axis Type-B is architecturally foreclosed
  (`TWO_AXIS_NOTE.md`, `two_axis_witness.py`: byte-identical observer orbit, different committer
  verdict). The genuine C/B detection-independence question lives on the committer axis →
  **OQ-87**. The deferred Type-C/B ruling did not evaporate; it relocated.

**R4 ruling (operator, 2026-06-11): SATISFIED.** The n=6 pilot diff satisfies "produced and
preserved" — preservation witness: 18 tracked pilot-arm JSONs + `STEP4C_PARTITION.md`
(the plan's earlier "20" reconciled: grep over `pilot` also caught 2 `phase_a_pilot_*` demo
files). **Corpus-scale census: DECLINED-WITH-REASON** — TWO_AXIS gutted its payoff to
generation-resolution characterization with no current consumer. What a re-open would buy is
the untested STRUCTURE PASS (bin membership predicted by ε/signature/type; the pilot had no
evaluable low-ε topic). Any re-opened census is governed by the pre-registered extended (b)
criterion: same `(HasB,HasV)` boolean profile AND victim-count same-side-of-
`critical_mass_threshold`, ε-pinned, bin-blind, two clean separately-generated arms (the live
dual-surface prompt cannot serve as the framing perturbation).

**Classifier-sync (entry item 5) RESOLVED 2026-06-11**
(`audits/2026-06-11_oq83_close/STEP1_REPORT.md`): the nb_setval hypothesis CONFIRMED at
clinical_deskilling_automation T=0 (set-manipulation closes the mismatch; controls pass);
milblogger_legitimacy_erosion T=18 no longer mismatches on current code (OQ-90/OQ-44 moved the
piton path) → graduates CLEAN; new ε-sourcing mismatch `challenge_as_commons_maintenance` T=5
(grid-misalignment class, touches no counted flip, unflagged). Operator-ruled determinism fix
landed (`transition_paths.pl`: snapshot_type clears the globals at entry; order-dependence
witnessed before/after; the mismatch persists honestly — full snapshot_type ≡ classify_at_time
remains FALSE with the semantic ε-sourcing divergence documented, by design). Flag inheritance
→ OQ-110: clinical 0→2 documented exclusion; milblogger 12→18 clean.

**Where each surviving obligation went (cold-reader index):**
- **Consumer-side `Backed` verification** (the OQ-33 and OQ-46 closes both deposit it "with
  OQ-83") → **OQ-110** — the offline residual join is the consumer that exercises the bit.
- Phase B cutover, Phase C regen/retire, CLAUDE.md mandatrophy-note retirement, AUDIT OPEN-1/2
  → **OQ-109**.
- Author-vs-derive D-fork (time-varying role/d; C1/C2 pair) → **OQ-110** (operator ruling).
- Type-C/B → **OQ-87** (committer axis). v7 §4.5 bridge census (item 4): LANDED 2026-06-11,
  seams grep-witnessed (commit `5822c242`).

**Rulings of record (still operative — kept per the compress-on-close exception; OQ-109 builds
against these):** R1 drop authored per-seat perception (KEEP the computed perceived-vs-real
gap). R2 dial-set {agenda_setter, beneficiary, payer, excluded, observer} DECLARED/SWAPPABLE,
bundling stated. R3 `excluded` commentary-grade only; consumer = consensus-provenance check.
R4 preserve the A/B perturbation pair — ruled satisfied above; the pair stays preserved (audit
dir now; Phase C archives the arm corpora with manifests). R5 sixth question
(genealogy/obsolescence); mismatch consumer only; rewires the mandatrophy apparatus per A7
(recovery, not net-new scope — abandonment was a dropped seam at the format migration,
witnessed against git history). Contender: NO sixth role — contention is a structural RELATION
between seats, derived from opposed roles, never authored. Static roles now; time-varying
role/d is OQ-110's fork.

**Phase A landed surface (one line each; witnesses in the audit dir):** step 1 schema (7/7
witness suite incl. negative controls); step 2 compiler (0/100 additivity diff; stakeholder/7 +
secondary_role/3 + non_agent/2 + disappearance_verdict/2 + founding_problem_status/2;
role-derived beneficiary/victim, excluded derives NOTHING); step 3 engine layer
(`stakeholder_seats.pl`: per-(C,Name) d, `in_contention/3`, `seat_perceived_vs_real/4`,
`consensus_provenance/2`, `zombie_piton_crosscheck/2` — all commentary-grade; name-keyed
0.12/0.85 d-split causally located in the role params); 4b RENAMED-NOT-ESCAPED → OQ-85
resolved silence-is-correct → 4c pilot partition (above). OQ-84 resolved in-pass. Stage C
(OQ-92) made the live prompt dual-surface additively; the R4 control arm stayed intact.

**Follow-through landing (2026-06-16): R5 Q6 synchronic crosscheck COMPLETED.** The A7-recovered
`zombie_piton_crosscheck/2` (dead × piton only, single confrontation) was replaced by
`stakeholder_seats:q6_crosscheck/3` — the full status×computed-signature matrix, commentary-grade
(NEVER overrides dr_type; sole caller `report_generator.pl:r5_zombie_crosscheck_line/1`). Four
distinct non-verdict buckets kept separate (`q6_unmeasured` authored-absent / `q6_signature_unknown`
computed-absent / `q6_unclassified` present-fell-through / out-of-domain → lint fail-loud). Cell
names commit only to the synchronic mismatch — no trajectory or orientation vocabulary (the
tier cannot witness Ω_P). Daylight qualifier axis (`founding_problem_corroboration_class/2`,
authored atom; lint-gated) SHIPS INERT — all with-block stories read `daylight(unstated)` until a
bounded R5 backfill authors the class; **the backfill is the OPEN graduation step for the daylight
axis** (the status×signature matrix is fully live without it). Load-bearing witness settled:
`dr_type/2` resolves at `default_context` = analytical perspective (constraint_indexing.pl:156–161),
closing the proxy gap — `q6_unclassified` is WITNESSED 0 on the live corpus (no mountain/scaffold/
naturalized at analytical) but corpus-REACHABLE on the twins (haiku=1, flash=5, all `live × mountain`)
so the catch-all is real, not synthetic-only. Determinism (1 solution/story) + mode-robustness
(bound-Cell census returns the genuine set, not all stories) witnessed on all 3 corpora.
Audit + witnesses: `audits/2026-06-16_q6_crosscheck_completion/`. Deferred diachronic tier → **OQ-133**.

**Origin:** 2026-06-06/07 (feasibility → five-model review → operator mountain-and-frame
re-read → audit-then-plan). Closed 2026-06-11.

---

## OQ-84 — Intra-kernel shared_agent_link guard exists at the purity-network site but not at inferred_coupling_protocol (latent same-kernel contamination path)

**Ω-type:** Ω_E (engine-hygiene; the edge-set difference is directly witnessable).

**Status:** resolved — guard added at `inferred_coupling_protocol.pl` `compute_baseline_edges`
(2026-06-07, OQ-83 step 3, same pass as the projection per the operator pin). Branch settled by
git history BEFORE the edit (operator-required read): the coupling module was frozen 2026-02-18,
three months before kernels existed; the guard arrived 2026-05-25 (`622d8ece`) at only the
purity-network consumer; the coupling file never contained `cs_kernel_id` in its entire history —
never-updated-when-kernels-arrived (bug), NOT deliberate divergence. Witnesses (audit dir
`step3_guard_zombie.txt`): live corpus guarded=72 = unguarded=72 (zero kernels live — guard is a
no-op today, fail-closed for kernel-bearing corpora); synthetic same-kernel control (consulted
multifile facts; note `cs_kernel_id/2` is STATIC — assert throws, consult is the overlay tool):
unguarded sees the pair (1), guarded filters it (0). Sole consumer `run_coupling_protocol` →
`outputs/coupling_protocol.md` (run_pipeline.py:430).
**Origin:** 2026-06-07, `audits/2026-06-07_stakeholder_layer_migration/AUDIT.md` §A6.

The intra-kernel filter on `shared_agent_link/4` (`\+ (cs_kernel_id(C,K), cs_kernel_id(Other,K))`)
exists at the contamination-network consumer (`drl_purity_network.pl:96–98`) but NOT at the second
consumer: `inferred_coupling_protocol.pl:218–222` consumes `shared_agent_link(C1, C2, _, _)` raw
(only `C2 \= C1`). Same-kernel shared agent names are filtered from contamination but DO enter
`run_coupling_protocol`'s edge set. Live-corpus baseline (witnessed): 504 distinct agent atoms, 25
shared across ≥2 constraints, 38 cross-constraint pairs; live corpus currently has zero kernels,
so the path is latent, not firing — it goes live with the first same-kernel sibling readings
sharing agent names (kernel generation re-enabled, or OQ-83 Phase A).

**Resolution shape:** either add the guard at the second site (mirroring :96–98), or rule the
asymmetry intentional (components analysis may WANT same-kernel edges) — operator call. Either
way: enumerate `run_coupling_protocol` output consumers first (= AUDIT OPEN-3) and witness the
edge-set diff with/without the guard on a kernel-bearing corpus overlay. Sequencing: resolve or
rule BEFORE OQ-83 Phase A lands sibling readings, but do not gate it on the migration.

---

## OQ-85 — Generated contention arrives as same-role co-administrators, which derived-contention cannot see (the 4b renamed-not-escaped finding)

**Ω-type:** Ω_P (author-vs-derive contention — operator's to rule) + Ω_E (the generation
behaviour is measurable and model-conditional).

**Status:** resolved — silence-is-correct (operator-ruled 2026-06-07, against the read-only
decomposition audit). `in_contention`'s scope was already right; the asymmetric-only escape is
correct, not a limitation. Reports: `STEP4_4b_RENAMED_NOT_ESCAPED.md` (origin) +
`OQ85_DECOMPOSITION_AUDIT.md` (audit) in `audits/2026-06-07_stakeholder_layer_migration/`.
**Origin:** 2026-06-07, OQ-83 step-4 4b gate.

**THE WITNESS (load-bearing, leads the record): `in_contention` feeds NO classifier.** Grep:
zero consumers outside its definition site; `dr_type` / `classify_from_metrics` /
`signature_detection` read neither `in_contention` nor `constraint_stakeholder`. It is a
relational ANNOTATION — so its silence on co-equal antagonists cannot be a classification blind
spot, regardless of any other argument. This is the fact that resolves OQ-85; everything below
corroborates.

**THE GENERAL RESULT (not corpus-contingent): the type is metric-driven and stays correct even
with no anchor.** The constructed worst case `oq85_blindspot` — two co-equals both agenda_setter,
ZERO victims, no powerless anchor — STILL computes `dr_type=snare` from ε/supp alone. So the
silence is correct because classification doesn't need the pairwise relation *even when the
anchor is absent* — a general property, not "correct on this corpus because real stories happen
to carry a powerless payer."

**CORROBORATION (the anchor argument — defensible, but not load-bearing):** both real stories DO
carry a powerless structured anchor (streaming `independent_artists`, hospital `insured_patients`),
making the between-setter rivalry second-order. This explains *why it's also defensible* but would
hold even if false, because the two facts above already settle it.

**Supporting (audit detail):** 1a — the only structured per-agent directional referent is
role-derived `constraint_beneficiary/victim` (/2, per-constraint-global); multi-constraint,
per-metric, pairwise `(C,A,B)` ABSENT. 1b dual-level — (TYPE) both stories compute `snare`
(ε=0.7/supp=0.8), not claimed `rope`, engine already sees it; (PAIR) between-antagonist asymmetry
is prose-only. Positive controls: W1 clean asymmetric pair → `in_contention` + d-split 0.25/0.85;
blind-spot located+real yet type correct. A6 multi-constraint branch did not land → coupling
recount not due.

**Right-sizing (honest accounting):** the 4b gate did its job (stopped a run, surfaced a
structural fact) but the fact was smaller than the gate's framing — an annotation predicate's
scope matching what it's for, NOT "the migration re-imposed the straitjacket for half the cases."
The cheap probe that would have right-sized this at the 4b gate, before the disentanglement
reasoning, was the consumer grep ("does anything read `in_contention`") — it came three turns
late. The excursion still banked the 1a inventory ceiling and the dual-level distinction, but the
fastest path to the verdict was a consumer grep, not a feasibility audit.

**Residual (filed standalone, NOT folded into the migration): OQ-86** — surface the pairwise
who-extracts-from-whom as commentary for the rare no-anchor case. Unblocked, do-whenever; the
migration neither needs nor carries it.

The cross-framing experiment's gate revealed that, under the neutral stakeholder prompt,
gemini-2.5-pro authors the canonical contention (two co-equal institutional players over a
split) as **two `agenda_setter`s at the same power** — the opposition lives in the situation
prose, not in any role-difference. `in_contention` and the per-(C,Name) d-split (OQ-83 step 3)
only separate **agenda_setter/beneficiary vs payer**, so opposed co-administrators are
structurally invisible; the one contention topic that did author opposed roles drifted the
antagonists' power atoms apart instead. Net: the straitjacket is escaped for asymmetric-role
opposition (step-3 mechanism, re-witnessed firing) and **re-imposed for co-equal-administrator
opposition**, which is the shape the contention topics actually contained. This is the A4
contender-residue (6.3%, "the dial-set backgrounds contention between co-equals") resurfacing at
the generation layer with evidence — directly bearing on the operator's A4 ruling (no contender
role; derive contention from opposed roles), whose premise (opposition shows up as
role-difference) this evidence challenges.

**Operator decision (candidates; none self-resolved):** (a) re-examine the derive-don't-author
ruling against this evidence; (b) add an opposed-co-administrator case to `in_contention`/the
d-model — but two agenda_setters share a role and nothing structured marks them opposed, so this
needs a NEW authored opposed-pair signal, reopening author-vs-derive; (c) scope the migration's
straitjacket-escape claim to asymmetric-role opposition only. Prompt-craft is NOT a default
(would manufacture the finding); if pursued it must avoid contention-directive language.
Gemini-conditional (single-model pilot; the 2×2 model×framing Ω under OQ-83 is unmeasured).
**Phase B and 4c remain gated until this is ruled.**

---

## OQ-86 — Surface pairwise who-extracts-from-whom as report commentary (standalone, unblocked)

**Ω-type:** Ω_C (a reporting/annotation feature; commentary-grade, never classification).

**Status:** resolved — (2026-06-16) shipped as R3 commentary: `stakeholder_seats:extraction_reading/2`
**Priority:** 1
(+ `extractive_type/1`, `authored_victim/1`), the CID-anchored `report_generator:extraction_reading_line/1`
(Section 7, beside the q6 crosscheck line), and the `extraction_reading` sidecar field
(`enhanced_report:extract_extraction_reading`). Mirrors the q6 anchored-line→extractor→sidecar shape.
Guard = constraint-level `dr_type(C) ∈ {snare,tangled_rope}` ∧ no **authored** victim ∧ ≥1
beneficiary-side agent seat; names the beneficiary-side seats, flags the cost-bearer as prose-only.
Per-seat-extractive deliberately NOT the key (it names the victim, not the extractor — plan W1/W2).
**Origin:** 2026-06-07, OQ-85 decomposition audit.

**Witnesses & findings (plunit + cross-corpus census this session):**
- **Calibration measured, not inherited:** ε=0.82/supp=0.91 + blindspot structure → `dr_type=snare`
  (derived, not stubbed). Controls in `prolog/tests/test_oq86_extraction_commentary.pl` (24/24): positive
  fires with both co-equals; channel-line witness; three single-variable negatives (type/victim/non-vacuity).
- **Plan correction #1 — the data-repair bridge defeats a naive guard.** `data_repair.pl:153` (OQ-93
  shim-family) FABRICATES `constraint_victim(C, inferred_subject)` whenever E>0.46 ∧ S>0.40 and no victim
  is authored — i.e. on the EXACT blindspot metric profile. A literal `\+ constraint_victim(C,_)` guard is
  therefore INERT on every real report (Build Discipline P5/P6: a success-shaped fabricated token fills the
  no-victim hole). Fix: `authored_victim/1` excludes the fixed sentinel `inferred_subject`. Locked by a
  regression test + an end-to-end channel witness (real `enhanced_report.run_prolog_report` path emits the
  line + sidecar only WITH the exclusion).
- **Plan correction #2 — NOT silent on the live corpus (plan predicted 0).** Fires on **3 live** constraints
  (all `tangled_rope`, all no authored victim) — genuine blindspots, the feature working. Cross-corpus census:
  testsets_haiku 10/960, testsets_flash 34/960 (all `tangled_rope`, all `[]`); **0** across kernel_v1/v5/v6/sotu
  (~5,377 stories) — guard C correctly fail-closes there (those pre-stakeholder archives have 0 seat facts; 62
  kernel_v1 constraints pass guards A+B but cannot name extractors → silence, not a fabricated value). EVERY
  real firing is `tangled_rope`; omitting it from `extractive_type/1` would make the feature 100% inert on real
  corpora.
- **OQ-134 table-setting:** emitted through the same anchored-line→extractor→sidecar shape as q6_crosscheck,
  so a future corpus-wide census (its `bundled_with` sibling) can aggregate it with one generic
  per-constraint-commentary-sidecar exporter (coverage/denominator; buckets separate per P6), not a special one.

---

## OQ-87 — Committer-axis experiments (detection-independence / Type-B): BANKED, existence proof UNPROVEN

**Ω-type:** Ω_E (detection-independence is measurable; on kernel_v1 it is saturation-confounded, not
proven) + Ω_P (whether to fund a larger de-leaked study — operator's, fresh decision).

**Status:** partial — **BANKED/PARKED 2026-06-08.** Cold-read entry point:
**Priority:** 1
`audits/2026-06-07_stakeholder_layer_migration/COMMITTER_THREAD_HANDOFF.md`. Sub-questions answered;
the overarching detection-independence existence proof is **UNPROVEN** and its next move is a
fresh-decision larger study (NOT a parameter change). **Banked verdicts:** two-axis architecture
real, observer-axis Type-B architecturally foreclosed, committer axis is the separate
structure-sensitive surface (TWO_AXIS_NOTE); CA-1 field partition confirmed (cross-check);
CA-3 kernel_v1 diverge-A is ~89% drift-convention saturation, NOT load-bearing (cause-witness);
Step 0 observer claim-drift MODEL-STABLE (reproduces under Sonnet); pilot — husk-saturation is
reading-set + magnitude-authoring, NOT a Haiku prior, NOT removable by the Haiku→Sonnet bump
(matched run overturned the unmatched Step 1b). **Next move (fresh decision):** a much-LARGER
de-leaked kernel study to average out the dominant reading-set variance, then CA-3 + diverge-A
cause-composition vs the pinned 3-conjunct criterion — rebuild-scale, NOT a model swap. Independent
second experiment: **CA-2** (committer C/B / framing-dependence; archive cannot substitute — varies
content not framing). Reports: TWO_AXIS_NOTE, CA_COMMITTER_AXIS, PILOT_STEP01_REPORT. Run-tagged
pilot stories untracked/glob-isolated/disposable. Cost spent ≈284 calls.
**Origin:** 2026-06-07, the two-axis correction (TWO_AXIS_NOTE) → "more experiments before a corpus."

The committer axis is a real, content-driven classification surface separate from the observer
orbit (two-axis note). This pass measured it.
- **CA-1 (synthetic field-partition cross-check):** committer verdicts framing-INVARIANT
  (reference_frame / story_uid varied → identical) and content-SENSITIVE (grounding flip stopped
  `cs_axiom_foreclosed`; probe live). Confirms the field partition by a 2nd method (perturbation on
  top of the static read) — NOT a fresh architectural finding; no mis-binned field (the only
  interesting surprise) occurred.
- **CA-3 (detection-independence on kernel_v1, READ-ONLY, kernel_v1-REGIME):** per-axis verdicts
  pinned before the run (observer coherent=H0=1; committer dead=cs_axiom_foreclosed OR drift terminal
  ∈ {axiom_foreclosure,husk,extinction,repudiation}); H0 pre-check passed (non-degenerate, no
  stale-site collapse). N=906. 2×2: agree-live 44, agree-dead 648, **diverge-A (coherent+dead) 74 =
  8.2%** (clean Theorem-7, spread across 68 kernels), **diverge-B (incoherent+live) 140 = 15.5%**
  (artifact-prone, reported separately). **Both axes SATURATED** (observer 87% incoherent — plausibly
  real for contested kernels, staleness component unquantified; committer 80% dead — kernel_v1
  drift-authoring convention, **NOT OQ-70 FNL** which is observer-axis), so the summed density (23.6%)
  is NOT a trustworthy quantitative gate; the agree-dead majority is two saturated axes coinciding.

**Verdict (REVISED by the diverge-A cause-of-death per-item witness — the 74 count overclaimed):**
diverge-A's deaths are PREDOMINANTLY one drift-authoring convention, not heterogeneous cause. Of 74:
66 died via drift-trajectory only (reads drift_state alone); `cs_axiom_foreclosed` (only verdict
reading axiom grounding) fired 8; drift magnitude substantial 88%, acknowledged=false 92%; single
profile (practice_drift|authority_erosion, substantial, false)→husk = 50/74 (68%). So ~89% is the
same convention firing in the observer-coherent slice = **saturation wearing Theorem-7's clothes**,
NOT real orthogonality; the clean content-driven core shrinks to ~8 (the axiom-grounding-gated
cases). **The detection-independence existence proof is NOT load-bearing on kernel_v1** — it needs a
de-leaked re-measure before it stands. (The per-item witness caught the count standing in for the
substrate, again.)

**Two corpora, two gates (operator):** (a) a **detection-independence corpus** — CA-3 qualitative
gate is **NOT green after the cause-of-death witness**: the de-leaked kernel pilot is now a
PREREQUISITE to a clean existence proof (not just to a precise density), because kernel_v1's
diverge-A is ~89% drift-convention saturation; the clean core (~8) is too thin to authorize a
corpus on its own. (b) a **C/B (framing-dependence) corpus** — gated on **CA-2** (generation: same
commitment, two framings); the archive CANNOT substitute (it varies content, not framing); CA-3
says nothing about it. A rebuild must name which corpus it builds for; do not let diverge-A
authorize a C/B corpus.

**Next moves (operator's to sequence):** a small de-leaked kernel pilot is now the PREREQUISITE for
the detection-independence existence proof (kernel_v1 is too saturated — diverge-A ~89% convention);
CA-2 (committer generation-variance / the real C/B test); the 2×2 model×framing Ω (observer
claim-drift model-stability, filed under OQ-83).

**Pilot in progress (2026-06-07/08); plan in `audits/.../` + report `CA_COMMITTER_AXIS.md`-adjacent.**
- **Step 0 DONE — 2×2 model Ω: the 4c observer claim-drift is MODEL-STABLE, not a Gemini artifact.**
  Re-ran both arms under Sonnet (`claude-sonnet-4-5-20250929`) on the 3 contention topics:
  stakeholder→`rope`, four-tuple→`tangled_rope`, **3/3**, identical to Gemini (engine corrects both
  to snare). The claim-layer framing effect reproduces across model families → the cleanest
  framing result of the thread; observer-axis loose end closed. (artifacts:
  `*.sonnet_{stake,four}.json`, `step0_model_omega.py`.)
- **Key pilot finding (reshapes it):** the de-leak fixed type/metric leakage but NEVER touched the
  drift-example bait — the generation prompt's Rule 5 drift worked-example is
  `authority_erosion/substantial/false` (the kernel_v1 saturation profile), single-source
  (verified: the one-shot example JSON and SCOPE prompt carry no drift). The distribution-check
  counter-instruction was present in kernel_v1 and FAILED. So a naive de-leaked pilot reproduces the
  saturation. Restructured (operator-approved): Step 1 = drift example-neutralization gate (example
  REMOVED not rebalanced; current-vs-neutral arms, Haiku GEN_MODEL both, compare drift
  distribution); Step 2 (existence proof) only if Step 1 = artifact; if it persists, the
  second-model arm (≠Haiku) separates "real property of contested kernels" from "Haiku authoring
  prior" before any "true property" claim. Cost ceiling 500 calls total; composition-in-same-pass.
- **Step 1 + 1b DONE (2026-06-08); report `PILOT_STEP01_REPORT.md`.** Step 1 (Haiku, example
  removed vs kept): husk-driving fields PERSIST (substantial 84%→79%, ack-false 77%→72%, neither
  >15pts) but the example DID anchor DIRECTION (authority_erosion 14→4 on removal). Step 1b
  (Sonnet GEN, neutral prompt — the operator's model-confound guard): substantial drops 79%→**59%**
  (severe+minor appear) → **the husk-saturation is substantially a HAIKU GEN_MODEL authoring prior,
  NOT a true property of contested commitments** (kernel_v1 was Haiku-generated; its 89%-husk
  diverge-A is that prior + the direction example, not detection-independence ground truth). The
  guard caught the exact misread it was for (Step 1 alone read "persists→real"; the second model
  revealed "Haiku prior"). **HONEST LIMITATION:** arms were separate full gkc runs → each
  re-SCOPEd a different reading-set, so comparisons co-vary prompt/model with reading-set on small
  N (~35/arm); direction-anchoring survives this (selective depletion of the example's own
  direction), husk-as-Haiku-prior is suggestive-but-confounded. **Disposition:** detection-
  independence existence proof remains UNPROVEN but the saturation is plausibly REDUCIBLE (not
  ground truth); a clean proof needs a MATCHED run (SCOPE-once / same manifests, vary only GEN
  model; ideally Sonnet) + CA-3 + diverge-A cause-composition — a fresh-decision Step 2 (~250 calls,
  approaches the 500 ceiling). CLEAN STOP taken here (over-descend watch). Cost so far ≈198/500.
- **Step 2 MATCHED run DONE (2026-06-08) — the clean control OVERTURNS Step 1b; the Haiku→Sonnet
  bump is NOT confirmed.** Generated both arms from the SAME 12 manifests via the SAME backend
  (`generate_from_manifests`) + neutral prompt, varying ONLY GEN model (removes the reading-set +
  path confounds). substantial: Haiku 76% → Sonnet 62% (Δ13pts, below threshold); ack-false: Haiku
  49% → Sonnet **80%** (WRONG direction). So Sonnet does NOT cleanly de-saturate. Step 1b's "Sonnet
  de-saturates" was a reading-set artifact (unmatched re-SCOPE): `ack-false` ranges 49–92% across
  arms = **reading-set-dominated, not model**; `substantial` is robustly 62–88% (persistent
  authoring feature). **Corrected disposition:** husk-saturation is NOT cleanly a Haiku prior and
  NOT removable by model swap — the easy path is closed; components separate (substantial =
  robust; acknowledged = reading-set-dominated). Detection-independence existence proof remains
  UNPROVEN; a clean proof needs a MUCH larger run to average out reading-set variance (not a model
  swap), or accepting substantial-magnitude drift as the characteristic committer death-mode. The
  matched control was worth its cost — it caught a wrong conclusion (Step 1b) before it became a
  GEN_MODEL change. Report `PILOT_STEP01_REPORT.md`. Cost ≈284/500.

## OQ-88 — Flat router under-routes COUPLED methodological kernels; a false-mountain (authored mountain → computed rope) on a flat-entry topic is a candidate missed-kernel signal

**Ω-type:** Ω_E (the under-routing is witnessed and counted — World3, 2 stable dry-run samples; the
proposed detector is corpus-measurable) + Ω_P (design call: should the SCOPE gate treat an authored
inevitability/"mountain"/"irreducible" framing as a kernel trigger — seat necessity-vs-contingency —
operator's, fresh decision).

**Status:** open — candidate detector, N=2 witnessed instances; the positive/negative control sweep
**Priority:** 1
that would license it as a gate signal has NOT been run.

**Origin:** 2026-06-08, World3 essay comparison. A thesis-driven critique of the Nebel et al. 2024
World3 recalibration ("The Robustness Is the Tell", web Claude) landed a spine the pipeline essay
(`agent/analysis/essays/world3_recalibration_2024.md`) does not have: the **robustness inversion** —
parameter-insensitivity is evidence the collapse is architecture-determined and therefore evidence
*against* the calibrated forecast (date/mechanism), resolved by **seating the policy regime**
(collapse is mountain-within-BAU, rope-across-regimes; Stabilized-World scenario = the positive
control that the model is not a pure collapse engine). That regime is a contested SEAT.

**The under-routing, witnessed.** World3 routed FLAT under the kernel-liberal primed prompt (OQ-79),
stably across two `--dry-run` samples: `outputs/kernel_manifests/flat/world3_recalibration_2024_20260608_171605.manifest.json`
and `outputs/kernel_manifests/world3_kernel_probe/world3_recalibration_2024_20260608_183123.manifest.json`
(no `csr`/readings block; 0 readings, 4 axes). The second sample is the revealing one: it **emitted
the robustness fact itself as a standalone flat axis** (`parameter_sensitivity_structural_robustness`:
"different parameter sets yield different trajectories vs preservation of overshoot-collapse pattern
across parameters") AND `collapse_timing_credibility` as a *separate* flat axis — but never coupled
them (robustness ⊥ forecast) and never seated the regime, filing it instead as epistemic omegas
(`omega_earth4all_paradigm_shift`: "extending a dying paradigm or validating a still-useful one?";
`omega_belief_system_change_mechanism`). The gate decomposes a *coupled* methodological kernel into
its component axes + omegas, **dropping the coupling/foreclosure relation that makes it a kernel.**
This is the OQ-79 kernel-liberal gate's blind spot in the wild: it fires on a *constructible
foundational reading* but misses the necessity-vs-contingency seat when the topic dresses it as
measurement/epistemic uncertainty (both samples' F03 note reads the ambiguity as proxy/optimization
underdetermination, not a reading contest).

**The engine, however, DID flag it — in its own vocabulary.** `collapse_mechanism_ambiguity` (today's
report) classified authored=mountain → computed=**rope**, conf 0.01, `type_1_false_summit` severe,
boundary mountain→rope, all four observers agree (H¹=0). The **mountain↔rope type-divergence IS the
necessity-vs-contingency kernel question** — "is this an immutable necessity or a policy/coordination-
contingent arrangement?" The engine adjudicated the seat (rope) that the SCOPE gate never built.

**Candidate detector (the actionable claim):** a flat-routed topic the engine flags as FALSE MOUNTAIN
(authored mountain → computed rope, `type_1_false_summit`) is a **kernel false-negative candidate** —
the unseated seat is necessity-vs-contingency. Not World3-specific: same run, `demographic_skill_mismatch`
(china, flat) is authored=mountain → rope, conf 0.01 (aging-as-inevitability vs policy-contingent —
retirement age / immigration / automation). Two witnessed instances, one run.

**Positive/negative control REQUIRED before this becomes a gate (Build Discipline — every diagnostic
needs a positive control).** Sweep the live corpus for `flat-routed ∧ (authored mountain → computed
rope ∨ type_1_false_summit)`; verify (a) it FIRES on the known instances (World3 collapse, demographic)
— positive control — and (b) it does NOT blanket-fire on genuine false-mountains that are just an
authoring flinch with no contestable regime seat — negative control. Without (b) the detector
over-routes, which is exactly the OQ-79 kernel-liberal failure mode one level up.

**What resolution would change:** if the detector survives the control sweep, the SCOPE gate gains a
cheap, engine-grounded kernel trigger (an authored inevitability/"mountain"/"irreducible" framing
routes to seat necessity-vs-contingency as competing readings rather than flattening). If it
over-fires, the finding narrows to "the engine's false-mountain flag is a *prompt* for an operator
kernel-vs-flat review, not an auto-route." Cross-refs: OQ-79 (kernel-liberal gate / flat-entry path —
this is its measured blind spot), OQ-76 (salience-driven under-firing), `docs/seat-theorem-v1.md`
(Coupling Theorem), `docs/the_perturbation_principle.md` §7.1 (kernel-vs-flat is itself a seat).
Provenance: this analysis + KNOWN_STATE 2026-06-08.

---

## OQ-89 — `accessibility_collapse`/`resistance` under-authoring + the 0.5 neutral-default-crosses-threshold class

**Ω-type:** Ω_E (design-relevant — closes a fabricated-default path; generalization deferred).

**Status:** mitigated — core resolved 2026-06-09; residuals open (orphan cleanup, class generalization).

**Origin.** Audit `audits/2026-06-08_coordination_washing_clean_pass/` found `get_metric_average/3`
defaulted missing metric vectors to **0.5**, which exceeds `snare_epsilon_floor` (0.46), so a
constraint with no authored extraction fabricated `constructed_high_extraction` from no data.
Deeper cause: generation never authored `accessibility_collapse`/`resistance` for non-mountain
constraints (schema marked them mountain-only; prompt tied them to `emerges_naturally`). Removing
the 0.5 default surfaced the gap as `type_error` throws on 14/18 live constraints.

**Resolved (witnessed; KNOWN_STATE 2026-06-09, `rebuild_evidence/`).** (1) Schema requires both
metrics for ALL types (rejects each independently; `_basic_validate` fallback made consistent).
(2) Prompt promotes both to required-for-all with honest non-mountain guidance. (3) Engine fail-closes:
`get_metric_average` empty→`unknown`, abstain clause + `number/1` guards on the 4 profile-signature
predicates + `profile_numeric` gate on the confidence path — absence abstains to `unknown`, never
throws/fabricates (0 throws across corpus+probes; fully-vectored verdicts byte-identical). (4) The 3
articles regenerated; all 16 regenerated stories author both metrics; V5 substitution (B[metrics→0.5]==C
for 16/16) shows the formerly-defaulted metrics do not move these extraction-driven verdicts — fix is
structural, not a verdict change. This is the `get_metric_average:160` instance of OQ-43/OQ-44's
satisfy-on-absence class, now fail-closed.

**Open residuals.**
1. **Orphan cleanup.** Full re-run re-decomposes into different axes (world3 3→4, magnifica 11→6,
   china →5; minimal name overlap), orphaning the original testsets — left in place (operator ruling
   2026-06-09). 9 corpus members abstain to `unknown`: 2 are `*_contradictions` axiom meta-files
   (correct — not stories), 7 are orphaned originals superseded by fully-vectored replacements. n=34
   carries duplicate coverage until a cleanup decides which to retire.
2. **Legacy corpus** (~94/116 `json/` lack the metrics) is not retro-fixed; schema binds future
   generation only.
3. **Class generalization (narrow-scope ruling 2026-06-09 — flag, do not fix here):** any other
   `get_metric_average`-style default or metric/threshold pair where a neutral midpoint (e.g. 0.5)
   lands on the wrong side of a floor/ceiling has the same pathology. Audit candidate; cross-ref
   OQ-43/OQ-44 (satisfy-on-absence) — this is the *value-crosses-boundary* cousin, distinct from
   *absence-satisfies-quantifier*.

**What resolution would change:** residual 1 cleared → corpus denominator stops double-counting
orphan+replacement; residual 3 audited → confirms whether the fail-closed fix needs to extend to
other default/threshold pairs. Provenance: `audits/2026-06-08_coordination_washing_clean_pass/`,
KNOWN_STATE 2026-06-09.

**Evidence — the fail-closed fix made cross-corpus liveness sweeps runnable (2026-06-10).** Ran the
current `constraint_signature/2` across four corpora via `corpus_path` overlay; **0 throws** on all
(live 34, kernel_v1 1106, original_v5 702, original_v6 3380) — pre-fix these would have thrown on
under-vectored stories. Of the 12 signatures, **7 fire somewhere ⇒ LIVE** (natural_law,
false_summit_mountain, false_natural_law — all zero on live n=34 — are live-but-narrow); **5 are
dark across all ~5,222 stories** (coordination_scaffold, piton_signature, constructed_low_extraction,
constructed_constraint, ambiguous) — cruft-candidates pending the reference-exemplar control + the
value question (build_discipline *"Unwired ≠ worthless"*: firing-anywhere is evidence, not verdict).
Consistency: natural_law=404 on original_v6 reproduces the OQ-43 "404 NL on testsets_3000" count;
FNL=0 on kernel_v1 corroborates the OQ-70 bait-removal. Matrix:
`audits/2026-06-10_signature_liveness_crosscorpus/MATRIX.md`; KNOWN_STATE 2026-06-10.

---

## OQ-90 — Piton: build as a computed `false_ci_rope` refinement keyed on the stakeholder layer

**Ω-type:** Ω_C (design choice — operationalize the piton type; rulings recorded, build pending).

**Status:** resolved — capture-keyed piton refinement built, wired, and witnessed end-to-end (2026-06-11). Audit: `audits/2026-06-11_oq90_piton_refinement/`. Commits: `f2368073` (substrate: `uncaptured/1`, `piton_candidate/1`, `transient_neglect/1` in `narrative_ontology.pl`; `fcr_evidence/6→/7` capture-disposition), `64448411` (output-changing: the `resolve_with_perspectival_check/4` clause + `config:param(piton_refinement_enabled, 1)`), `fc724ab2` (retire the `Supp≤0.2` `piton_signature` dispatch + helper), `3a4e0209` (prompt).

**Origin.** Cross-corpus liveness sweep left `piton` dark across ~5,222 stories: a piton's real distributed extraction trips `appears_as_rope`, a Boltzmann failure fires FCR (priority 2) before the profile fallback (priority 6), so every piton was subsumed as `false_ci_rope`. The old profile gate keyed on `theater_ratio`/`Supp≤0.2` — lossy symptom proxies, both witnessed wrong (2026-06-10 controls).

**Resolution (the executed rulings).** Piton ⊂ `false_ci_rope`, refined IN-BRANCH (no cascade reorder): once FCR fires, `piton_candidate/1` (= authored-`diffuse` gain_flow ∧ `prohibitive` fixing_cost, both POSITIVE-authored, never NAF) relabels `dr_type` → `piton` while `dr_signature` stays `false_ci_rope`. The snare/piton split turns on *computed* capture (`constraint_captured/1`); absence fails closed (stays FCR-subsumed, never promoted). `transient_neglect/1` is a diagnostic + `fcr_evidence` disposition field only — no new type (operator ruling 2026-06-11).

**Witnesses (audit dir).** Phase-0 K=0 diffuse hand-audit 0/1 on `institutional_trust_erosion` (`diffuse_audit_institutional_trust_erosion.md`); Phase-2 four-shape control battery through the production `dr_type/3` (as-is→piton; captured/cheap/absent twins fall through; param=0 kill-switch→tangled_rope); Phase-3 `piton_refinement_enabled` 0↔1 pipeline diff = **exactly two rows** `tangled_rope→piton` (`regulatory_measurement_gap`, `institutional_trust_erosion`), signatures unchanged, leak controls held; Phase-4 two-sided retirement (positive control fires-before/falls-through-after + 0-row corpus diff).

**One-row vs two-row note (substrate drift).** The plan pre-registered ONE flip on a 48-testset snapshot; the live corpus is 52 (4 untracked working-tree testsets feed the pipeline, incl. `institutional_trust_erosion`). Re-registered to 2 rows after the hand-audit gate was extended to the new diffuse claim (operator-ruled). Reproducibility flag: a fresh clone at HEAD sees only 48 testsets and would reproduce a 1-row delta — the 4 untracked testsets must be committed for the 2-row result to reproduce (`corpus_drift_provenance_and_reregistration.md`).

**Carry-forward (operative, not blockers):**
- **Named next step:** post-prompt-change diffuse-eliciting batch (OQ-92 prompt-iterate loop), gated behind the K=0 diffuse hand-audit for that batch. Prompt change (`3a4e0209`) is OPEN-as-to-effect until that batch runs.
- `transient_neglect` cell is corpus-EMPTY (all 4 live diffuse claims are `prohibitive`); only witness is prototype control 5 + Phase-2 shape 3. Read "piton sparse" only WITH the upstream-shadow caveat: 4 piton_candidates exist, 2 are CI_Rope-shadowed upstream of FCR (not a refinement bug).
- **Superseded-pending (not removed, ruled scope):** the two `drl_core` theater-based piton clauses (`drl_core.pl:344`, `:403`); the maxent piton `default_profile` (theater-keyed, `maxent_classifier.pl:153–155`) is now stale vs the capture definition; `python/axiom_reachability.py:171,207` is a cascade replica modeling the removed `piton_signature` clause. New tracked item if/when these are reconciled.

---

## OQ-91 — The observer axis encodes decay but not repair: the missing upgrade/scaffold-success transition

**Ω-type:** Ω_C (design choice — what the type dynamics is for; whether to model repair as well as decay).

**Status:** open — finding recorded (verified asymmetry); design/build not started. Theory note: `docs/repair_dynamics.md`.
**Priority:** 1

**Finding (verified this session).** The type space is a dynamics, not a static taxonomy
(coordination is maintained, not achieved — same content as commitment-systems' "drift is
intrinsic"). `transition_paths.pl:transition_path/4` encodes **eight transitions, all downward / lateral-into-worse,
none upward** (rope→tangled_rope, tangled_rope→snare, rope→piton, scaffold→{piton,snare,tangled_rope},
snare→{piton,false_mountain}) — verified by full enumeration; positive control is that the eight decay
paths are present, so an upward head would have surfaced beside them. The predicate is consumed by
`drift_report.pl` (lifecycle) and **0 times in `run_pipeline.py`** — the live classification path
carries no transition concept at all (static per-constraint). So the engine registers how a constraint
falls, not how a scaffold lifts it. Mirrors (analogy) the five-questions-without-the-sixth bias: a
decay/cost-finding apparatus that structurally cannot see repair.

**Three separations to hold when building (constraints, not preferences):**
1. **Metaphors unwelded.** Repair ops on the rope (maintain / splice / replace) are the rigging
   metaphor; the **scaffold** is the construction metaphor — the temporary alternative load-path
   required to take a *load-bearing* constraint offline for repair (logic.md Thm 3, Supp ≥ 0.70),
   struck on success / ossifies to piton if not. They are not the same operation; the type
   vocabulary is multi-metaphor and does not compose.
2. **Axes decoupled.** The repair dynamics rhymes with the committer/CS axis (drift / acknowledgment
   / atrophy) but v7 Theorem 7 (Detection Independence) makes non-unification mandatory. Build the
   observer-axis repair in observer-axis terms; do NOT import committer-axis machinery (analogy, not
   bridge; only v7 §4.5 couples, citation at fixed-ε only).
3. **Persistence mechanisms distinct.** Scaffold-cost (load-bearing, high-Supp — snare/rope register)
   ≠ piton persistence (diffuse benefit / rational inaction, low-Supp). Do not collapse; piton
   `fixing_cost` is OQ-90's open question, not scaffold-cost.

**Candidate home (operator to rule the structure).** The type-trajectory *reporter*
`degradation_chain`/`snapshot_type` is **direction-neutral** (it reports the `snapshot_type`
sequence over the measurement series — would surface an upward run if one occurred); it is dormant,
off the live path. The transition *detector* `transition_path/4` is decay-only and would need upward
heads. So whether to extend a decay-*named* detector or build a dedicated upward structure is a
design decision, not a settled home — unfinished, not cruft (build_discipline "Unwired ≠ worthless").
Sub-question: do `maintain`/`splice`/`replace` warrant named engine operations or stay descriptive?

**What resolution would change:** the engine could register repair (a constraint lifted back up the
ordering), not only decay — and the asymmetry that currently makes the type dynamics a one-way
ratchet would close. Cross-refs: OQ-90 (piton fixing_cost), OQ-83 (committer axis), v7 Theorem 7,
`docs/repair_dynamics.md`, `docs/six_questions.md` (Q6), build_discipline "Unwired ≠ worthless".

## OQ-92 — No authored gain-flow / receipt surface: capture is not computed-representable (gates OQ-90)

**Ω-type:** Ω_C (design choice — whether to add an authored surface; the operator's ruling, not the
engine's to settle).

**Status:** resolved — all four stages LANDED + witnessed 2026-06-10 (one day, prototype to
wiring; evidence: `audits/2026-06-10_gain_flow_prototype/` + `audits/2026-06-10_oq92_step3_
preregistration/`). Stage A schema (8/8 witness, two-sided additivity); Stage B compiler
(0-diff 134/134, ghost-seat REJECTED both paths, swipl fact queryability); Stage C prompt
(stakeholders + receipt surface promoted additively into the live prompt; 6-story gfbatch1
generated — 6/6 author both fields, 0 diffuse, referential integrity clean end-to-end);
diffuse audit at K=0 against the pre-ruled criterion: **0/0 observed — vacuous pass, stated
as vacuous** (`diffuse_audit_batch1.md`; 6/6 named-capture prevalence flagged as
authoring-convention-until-checked, matters for OQ-90's piton side, not these gates); Stage D
wiring: `narrative_ontology:constraint_captured/1` (computed positively; absent/diffuse never
block) + the three OQ-94 benignity gates (drl_core scaffold clause, signature CI_Rope +
pure_coordination) + maxent scaffold spec same-commit. Two-sided controls
(`stage_d_controls.out`): uncaptured→scaffold vs captured-twin→rope; captured→pure_scaffold
not pure_coordination; CI_Rope deterministic intervention with verified restore
(certified → constructed_low_extraction under asserted capture → certified again).
Fabrication-ban grep witness: data_repair.pl returns only the ban comment. Suite green;
warning gate green (one deliberate allowlist line-drift update, gate fired on it correctly).
**Consequences: OQ-90 Steps 2–4 UNBLOCKED on a built surface; GAP-10 CLOSED. The Rulings
block below is kept intact per compress-on-close (operative rulings: tri-valued provenance,
fabrication ban, diffuse-audit criterion + K=0 parameters, benignity-gate semantics).**

**Origin.** OQ-90 ruled the snare/piton split turns on *capture* (snare = a seat captures the
extraction; piton = uncaptured) and that the no-capture test must be **computed, not authored-absence**
(OQ-83 R3 / Pattern 5). The proposed computed proxy `has_computed_capturer/1` (beneficiary-side seat
with favorable `dr_type_for_stakeholder`) was put to its pre-registered discriminating control
(plan `jaunty-juggling-wozniak`, build_discipline rule #3). **It HALTED at Outcome 2.**

**The finding (witnessed).** The cut **false-positives on a mild-favorable non-capturer.** χ
(`extractiveness_for_agent_d/4`, `ε·sigmoid_f(d)·σ(S)`) is **extraction-from-seat, not gain-to-seat**;
every beneficiary-side role gets low `d` (`config.pl:156–160`), so any beneficiary-side seat reads
favorable **whether or not the extraction accrues to it.** The cut degenerates into "does C have a
beneficiary-side-*role* seat at all." Two-part witness on the adversarial case (b): candidate-set
membership TRUE *and* cut TRUE on a seat with no `constraint_beneficiary` (so the false-positive is
real, not a no-candidate artifact). It also fires on an uncaptured designed DMV's agenda_setter.
Bonus: the only authored fact in the vicinity (`constraint_beneficiary/2`) feeds
`has_coordination_function/1` (`narrative_ontology.pl:303`) → pushes a capturer toward *scaffold*, the
wrong direction (promoted to OQ-94, 2026-06-10 — known-interference item for step 3). Witnesses:
`audits/2026-06-09_capture_axis_cut_control/` (PREREGISTRATION.md,
FINDINGS.md, step1_capturer_cut_control.out). Under-claim: witnessed on the constructed cases — the
false-positive is structural (upstream in the d-derivation, insensitive to the favorable-type set),
stated as a reading, not "unrepresentable across the whole range."

**What's needed.** An **authored gain-flow / receipt surface** — a per-(C, seat) or per-constraint
fact recording who *receives* the extraction (not who is unharmed), consumed *positively* (capturer =
gain authored to a seat), never by authored-absence. See GAP-10 (`docs/design/design_gaps.md`).

**Proposed unification (design proposal — SUPERSEDED 2026-06-10 by ruling (b) in the Rulings block
below; kept in place per supersede-don't-delete: this paragraph's "distinct scalars" caution is what
the ruling upholds).** OQ-90's other open term,
`fixing_cost`/benefit-of-fixing (the piton's `extraction < fixing_cost` second term), is also a
missing authored scalar in the receipt/accrual family. It is plausible that one authored-receipt
surface answers both — "who does the extraction accrue to, and at what cost to whom" — unifying the
capture split and the fixing_cost question. But accrual-of-gain and cost-to-fix are **distinct
scalars**; whether one surface covers both is a design ruling, and merging them owes a distinction-
check first (build_discipline rule #2). Flagged here for the operator, not folded.

**Rulings (operator 2026-06-10).**

- **(a) YES — build the authored gain-flow surface, prototype-first** (the OQ-93 precedent:
  hand-author the surface on control stories and run the discriminating battery BEFORE any
  schema/prompt/compiler change). Ruling (a) is "yes to prototype-first" and stands regardless of
  the prototype outcome; the outcome gates step 3, not the ruling.
- **(b) ONE authoring surface, TWO distinct fields (gain-flow + fixing_cost) — never one scalar or
  merged enum.** Justification (design grounds): (i) accrual-of-gain and cost-to-fix are
  semantically independent axes authored about the same seats; (ii) the fourth cell — captured +
  cheap fix — is live and the engine must be able to say so (witnessed by battery control 6, so
  this justification cites no cell the battery never witnesses); (iii) a merged enum would weld
  provenance to value, re-introducing exactly what the tri-valued design below unwelds. A rejected
  draft justification is recorded here to prevent re-citation: "one scalar can't encode two
  independent cuts" is FALSE as an information claim (a four-valued scalar encodes two bits; a
  three-valued enum covers three cells) — it establishes only that one *binary* bit can't. The
  design grounds, not the information argument, carry the ruling.
- **Tri-valued provenance design (the R3/Pattern-5 reconciliation).** `gain_flow` is authored as:
  gain-to-a-NAMED-seat (capturer; the named seat must exist in `stakeholders[]` — a checkable
  witness) | explicit `diffuse` (the author affirmatively asserts no seat captures — piton
  candidate) | ABSENT (surface not authored → fail-closed: no capture verdict, no piton refinement,
  constraint stays FCR-subsumed). Trap named at ruling time: once the surface is authored, NAF over
  "any seat has authored gain" is authored-absence in disguise — uncaptured must be authored
  positively (`diffuse`), never inferred from a missing capturer entry.
- **Malformed-gain default (DECIDED 2026-06-10, not left to be discovered).** `gain_flow` naming a
  NONEXISTENT seat fails the capture read (no `role_of` join) and is not `diffuse`, so at runtime
  it absorbs into the fail-closed register — a fourth condition collapsing into the third. Decided
  as the runtime default (fail-closed is the safe absorption) and witnessed by battery control 8.
  Step-3 validation item: the schema/compiler must REJECT a `gain_flow` value naming a seat not in
  `stakeholders[]` at authoring time, so the runtime absorption never silently hides a data error.
- **Fabrication ban (settled by the OQ-90 ruling; recorded 2026-06-10 — names the door, asks no
  question).** `gain_flow` is NEVER synthesized: absent stays absent, fail-closed — no repair,
  bridge, or imputation clause may infer it from metrics. The named door is `data_repair.pl`
  (`:124-131` already fabricates `constraint_beneficiary(C, inferred_institutional)` from
  E>0.46 ∧ S>0.40 on the DR-AUDIT path): a repair that "fixes" missing gain_flow from extraction
  metrics is structurally the `has_computed_capturer` counterfeit re-entering through a side door
  — a capture-adjacent fact synthesized from the metrics it feeds back into, on the path where it
  is least visible. OQ-90's HALT already ruled that route witnessed-broken. Step-3 precondition
  alongside the schema rejection and the diffuse gate; `data_repair.pl` is the enforcement site.
- **Step-3 gate (generated-diffuse audit), recorded now.** Authored-`diffuse` is an authored
  universal negative with no checkable witness (gain-to-seat verifies against `stakeholders[]`;
  diffuse verifies against nothing) and is the cheapest token for a generation model to emit
  without checking the seats. Hand-authored prototypes cannot surface this risk — controls pass by
  construction. GATE, not suggestion: before `gain_flow` drives classification on corpus data,
  hand-audit a pre-stated-size sample of generated `diffuse` claims for obvious capturing seats,
  with a pre-stated tolerance (size + tolerance pinned in the step-3 preregistration). OQ-70 is
  the precedent for a one-shot example value becoming a template convention; prevalence statistics
  on this field are authoring-convention until this audit passes. **Tolerance and sample size are
  RESERVED as an operator ruling at preregistration time** — the number arrives as a question to
  the operator, not a drafted value approved by momentum: it is a judgment about how much
  generation-side dishonesty the corpus can carry, with no evidence-settled default.

**What resolution would change.** With a gain-flow surface, the capture axis becomes computed
(positively, Pattern-5-safe), OQ-90's piton refinement (Steps 2–4 of the plan) can be built on it
rather than on the broken proxy, and the `Supp ≤ 0.2` piton gate can be superseded by capture. Until
then OQ-90's piton stays subsumed under false_ci_rope (not refined on a broken proxy). Graduation:
step-2 eight-control prototype (`audits/2026-06-10_gain_flow_prototype/`, preregistration committed
before the run) → on pass, step 3 = schema field + compiler emission + prompt change (the OQ-83
Phase-A playbook), with the diffuse-audit gate and the malformed-gain schema rejection as step-3
preconditions before any classification wiring. Step-3 sequencing (operator 2026-06-10): the
preregistration must name OQ-94 (`constraint_beneficiary` coordination-read interference —
homed first, per ruling) as a known-interference item, and the diffuse-gate tolerance is
reserved as an operator ruling at preregistration time. With OQ-94's per-site decision rule
RULED and the read-site pass complete (2026-06-10), classification-path handling is
rule-application over the pass's output. **Step-3 preregistration DRAFTED
(`audits/2026-06-10_oq92_step3_preregistration/PREREGISTRATION.md`), carrying two OPEN operator
questions — Q1 diffuse-audit sample/tolerance, Q2 the benignity-certification family as three
evidenced rows (split ruling permitted) — plus the settled preconditions: schema rejection,
fabrication ban (gain_flow never synthesized; `data_repair.pl` the named door), maxent
congruence, and the diffuse gate before any classification wiring. RULED 2026-06-10 (operator,
Q2 sequenced before Q1): Q2 row 1 GATE (P≠NP check dissolved — the gate reads authored
gain_flow, not beneficiary presence), row 3 GATE riding with row 1, row 2 DEFERRED pending the
CI_Rope reachability control (last pre-build item; gates only its own Stage-D clause); Q1: K=0
on the observable (detectability-limited probe — observed rate lower-bounds true rate by an
unknown multiple), halt = Stage D only never regeneration (regeneration IS the repair path),
N = whole-batch-if-small else ≥ 30 (labeled convention), obviousness criterion written BEFORE
reading the batch, findings say "0/N observed" never "clean". STAGES A–C UNBLOCKED. Row-2
control RUN same day (`audits/2026-06-10_oq94_row2_cirope_reachability/`): constructed vector
blocked at `boltzmann_compliant = inconclusive(insufficient_classifications)` (synthetic vectors
can't feed the Boltzmann test — diagnosed, not an interception proof); live-corpus existence
check: **CI_Rope ∧ beneficiary = 7 of 7** — the gate runs entirely on beneficiary-bearing
constraints today, interception hypothesis dead; whether any is CAPTURED is unknowable until
Stage C authors gain_flow. Row 2 RULED GATE (operator, same day): the family resolves
GATE-UNIFORM — with the evidence-shape distinction preserved (row 1 misfire-witnessed; row 2
reachability-witnessed, misfire pending the Stage-D two-sided control because unobtainable
earlier, not skipped); deferral would have inverted fail-closed (ungated CI_Rope certifying on
the first K=0-audited batch). Gate-uniform also answers Q1's coupling note in the conservative
direction: K=0 was priced against "a yes on any row" and got yes on all three. **Stage A LANDED
(2026-06-10): `gain_flow` + `fixing_cost` schema fields with tri-valued semantics, fabrication
ban, and compiler-enforcement notes in the field descriptions; two stakeholders-dependency
riders. Witness: 8/8 cases (three provenance shapes valid; four negatives biting at intended
guards; ghost-seat schema-valid BY DESIGN — compiler's job, Stage B) + two-sided additivity
sweep (91 pre-existing json/ invalids identical pre/post, ZERO new failures —
`stage_a_schema_witness.{py,txt}` in the prereg dir). Stage B LANDED (same day): compiler
emission + fail-loud referential integrity (`--no-validate` does not bypass) +
`narrative_ontology.pl` declarations; witnesses (`stage_b_compiler_witness.txt`): 0-diff
keystone 134/134 byte-identical old-vs-new, pilot branches (absent emits zero against two
firing twins), ghost seat REJECTED loud on both paths — closing Stage A's documented-open case
8 — and swipl-load fact queryability. Next: Stage C (prompt); the diffuse-audit criterion is
the next HUMAN gate — written before the first batch is read (operator-in-loop, prereg Q1).**
Cross-refs:
OQ-90 (piton/capture split + fixing_cost), OQ-83 (stakeholder layer / R3 authored-absence rule),
GAP-10, build_discipline rule #3 (axis introduction owes a pre-registered discriminating control) +
Pattern 5.

## OQ-93 — Imputation shim hides an unmigrated v3.4 measurement-grid contract (DR-AUDIT path)

**Ω-type:** Ω_C (ruled 2026-06-10: keep-and-migrate; intent top verdict excluded by ruling (a)).

**Status:** resolved — migration LANDED end-to-end 2026-06-11 (stages A–D + coverage-carrying
read + shim retirement), every stage same-commit witnessed:
`audits/2026-06-11_oq93_grid_migration/` (PREREGISTRATION.md incl. the operator-ruled split
κ gate + per-stage witness scripts/outputs); KNOWN_STATE 2026-06-11; branch `oq93-grid-migration`.
Executed: authored `coercion_grid` schema block (enums disjoint from MeasurementMetric;
stakes_inflation grid-side only) → compiler emission with fail-loud integrity (endpoints ==
interval, duplicate-slot REJECT = the once/1 contract; not bypassed by --no-validate) →
coverage-carrying read (`system_gradient/4`; []→0.0 KILLED; 8/32 one-level grid flips
increasing_coercion→OPEN while all five probe stories hold exact pins) → κ-gated N=10 generated
batch (PASS 0/10 excluded; bug rider found+fixed: scalar-series times poisoned gradient
next-points — compound(Metric) guard) → Stage-D wiring of `level_gradient_divergence/2` into
FCR/FSM/extraction-blindness omega (two-sided control witnessed; kappa CONDITIONAL 16/32 tag
FIRED; moderate-cap why-not recorded: 0 correction-grade carriers); injection/imputation killed
permanently (shim + flag retired → closes OQ-96). Ruling (a) stands: `structural_coercive_intent`
unwired — sub-fork filed as OQ-106. **FLIP RULED (operator,
2026-06-11): flip now, gate converted one-time → FIRST-CONTACT** — every grid-authoring story
gets the three-indicator audit once before any consumer read
(`python/grid_first_contact_gate.py` + tracked ledger, wired into run_pipeline; C-echo in any
new story halts and reverts the flip); live prompt carries the opt-in grid section; the 10
gate-passed stories PROMOTED to the corpus (n_constraints 48→58; every standing 0-diff baseline
re-pins to the post-promotion corpus). Promotion found and fixed two latent defects with
witnesses (flip_promotion_witness.txt, flip_promotion_suite.txt): grid_provenance read
interval-ANONYMOUS (56/58 cross-constraint leakage — build-unit-1 class, latent while only one
loaded interval ever had grid facts) and the suite generator's unanchored interval regex matched
prose (three phantom test_case IDs '18'/'0'/'from' ran green against injected anchors while the
real intervals never got their suite pass).
Cross-refs: OQ-44 (Pattern-5 instance — the completeness gate now reads authored-or-absent),
OQ-96, OQ-102 (riders landed with the stages), OQ-105 (the grid block shares t0/tn by
construction, so it cannot create that misalignment class), OQ-106.


## OQ-94 — `constraint_beneficiary/2` reads as benign coordination engine-wide; once the gain-flow surface lands, the same fact-family makes opposite-direction calls on captured constraints

**Ω-type:** Ω_C (design choice — what the legacy beneficiary fact may evidence once an authored
receipt surface exists; operator's ruling).

**Status:** open — homed 2026-06-10; known-interference item for the OQ-92 step-3
**Priority:** 1
**Deps:** blocked_on_human operator-benignity-certification
preregistration. Per-site decision rule RULED 2026-06-10 (Read-site pass block below).
**Read-site pass COMPLETE 2026-06-10** (`audits/2026-06-10_oq94_readsite_pass/`): 12 consumer
files (the prior "seven-consumer list" was `head -15`-TRUNCATED — concealed `drl_core.pl:346,373`,
the classification cascade itself, plus `maxent_classifier` and `omega1_audit`); rule sorted the
live surface into SOUND (4 NL/FSM mountain-likeness sites) and FORBIDDEN (tangled_rope cell,
decay detection, separability, NAF-voids) plus raw surfaces, and isolated ONE unsorted family →
**ESCALATED to operator: benignity certification** (`drl_core:346` scaffold clause + maxent
scaffold spec + `signature_detection:1019` CI_Rope gate + `:1122` subtype) — contains the
prototype's witnessed wrong-direction mechanism; gate-on-not-captured there is plausibly correct
but is a third question the rule doesn't decide. Step-3 preregistration therefore carries TWO
operator questions: diffuse-gate tolerance + the benignity-family ruling — **presented as THREE
evidenced rows permitting a split ruling (format note, operator 2026-06-10): the family is
unified by its question, not its evidence — `drl_core:346` arrives with the prototype's witnessed
misfire on the gate's side; the CI_Rope gate and pure_coordination subtype have no equivalent
witness yet; the format must not prejudge family-atomicity.** Also found:
`constraint_bridge.pl:96` is the first gain_flow-migration candidate (collects extraction actors
via benefits-from); `data_repair.pl:124-168` FABRICATES beneficiary facts from metrics on the
DR-AUDIT path (OQ-93 circularity hazard — high extraction manufactures a beneficiary which feeds
the coordination read).

**The behavior (witnessed twice).** `has_coordination_function/1` is one clause:
`constraint_beneficiary(C, _)` (`narrative_ontology.pl:303-304`) — the fact that *names a
beneficiary* is consumed as evidence of *benign coordination*. Witnessed pushing capturer seats
the wrong direction: (1) the 06-09 capture control ("the only authored fact in the vicinity
actively points the wrong direction for capture,"
`audits/2026-06-09_capture_axis_cut_control/FINDINGS.md:83`); (2) the 2026-06-10 step-2 prototype:
both capturer seats (cap_a/capturer_a, captured_cheap_f/capturer_f) computed
final_type=**scaffold** (`audits/2026-06-10_gain_flow_prototype/gain_flow_prototype.out`) — the
only production-engine behavior the prototype touched, and it opposes the capture surface's
purpose (keeping captured constraints snare-flavored).

**Why this goes live as a fight at step 3, not later.** The consumer surface is wide:
`drl_boltzmann_analysis.pl:111-123` (`separability_factor` inside `reformability_score` —
reform-value scoring of coordination/extraction CO-OCCURRENCE; MISLABELED in this entry's first
version as "the CI/coordination side of the Boltzmann test — the FCR axis," corrected 2026-06-10
on read-site inspection — the true Boltzmann factorization module `boltzmann_compliance.pl`
consumes NEITHER fact of this family, a meaningful absence: the probe positive-fired on 7 other
files), `transition_paths.pl:35,82,164`, `drift_events.pl:125,203`,
`logical_fingerprint.pl:176,226,444`, `invertibility_analysis.pl:120`, `gap_diagnostic.pl:135`,
`cs_pattern_detection.pl:360`. And the collision is structural, not incidental: the OQ-83 Phase-A
compiler derives `constraint_beneficiary/2` from stakeholder role `beneficiary`, so every
generated capturer seat (role beneficiary + authored gain_flow) automatically also feeds the
coordination read. Once step 3 wires `seat_captures` into classification, the same constraint
carries a snare-direction capture call and a scaffold-direction coordination call from the same
authored signal family — two live engine behaviors making opposite calls on the same constraints.

**Probe scope (2026-06-10 — so the per-consumer-diffs obligation has a list it can be complete
against; CORRECTED same day).** The seven-consumer list above = one grep for
`has_coordination_function` over top-level `prolog/*.pl` (non-recursive; subdirs excluded) —
**and `| head -15`, which this scope statement originally failed to name: the list was
truncation-bounded, concealing `drl_core.pl:346,373` (the classification cascade), `maxent_
classifier`, `omega1_audit`, and further sites. Untruncated census = 12 files / 33 sites
(`audits/2026-06-10_oq94_readsite_pass/census_raw.txt`). Lesson: a probe-scope statement must
name its output limits (head/-m/pagination), not only predicate and glob.** The deriving fact's own direct
consumers are a SECOND, WIDER surface, enumerated separately: `constraint_beneficiary` hits 15
engine files (`data_repair` 10, `tangled_rope_examples` 8, `signature_detection` 5 — the FSM
agent-beneficiary gate, `constraint_indexing` 4, `logical_fingerprint` 3, `drl_purity_network` 3,
`global_delta_report` 2, `cs_pattern_detection` 2, plus 1 each: `report_generator`, `json_report`,
`invertibility_analysis`, `drl_core`, `drl_boltzmann_analysis`, `constraint_instances`,
`constraint_bridge`) and two test files (`tests/test_agent_beneficiary.pl`,
`tests/test_cs_pattern_detection.pl`). These are file-level hit counts including declarations and
comments, NOT verified read-sites — read-site verification is owed at adjudication time. Any
predicate derived from either surface beyond `has_coordination_function` is unenumerated.
Breadth option (operator in-session note, 2026-06-10 CC session — NOT a review-thread item; the
review flagged this provenance distinction explicitly): if adjudication evidence needs more than
the live corpus (n per manifest, currently small), `prolog/archives/datasets/` can be overlaid
via `corpus_path` for cross-corpus sweeps (the OQ-89 liveness sweep pattern, ~5,200 stories) —
fit for measuring coordination-read liveness/scaffold-push prevalence and for per-consumer
behavioral diffs on the LEGACY side, and for a mass absent-register witness (archive stories
predate the stakeholder layer: no `stakeholders[]`, no `gain_flow`, so the entire archive must
land fail-closed). NOT fit for exercising the new surface itself — pre-reset regime, OQ-70
bait-confound caveats apply to any signature-prevalence read. Spike requirement (the case-7-via-
case-2 rule at scale): a mass-silence sweep witnesses fail-closed ONLY if something in the SAME
run demonstrably fires through the IDENTICAL read — a spiked story with an injected `gain_flow`
fact, or live-corpus constraints run through the same harness; without the in-run positive,
"entire archive lands absent" is byte-identical to a sweep that never looked.

**Read-site pass — first results (2026-06-10, witnessed; provoked by a review premise that
inverted on inspection).** A per-site decision rule was proposed in review: sort each read-site by
*which question the read asks* — mountain-likeness ("holds without enforcement") → capture-gating
sound, arguably correct semantics; coordination-despite-extraction (the tangled_rope/snare-adjacent
cell) → gating forbidden, split signal required. The rule is OPERATIVE — the first site sorted by
its intent-label flipped when the clauses were read:
- `drl_boltzmann_analysis.pl:110-125` (`separability_factor`): asks whether coordination and
  extraction CO-OCCUR (reform value). Clause 1 fires exactly on the captured-coordination cell
  (coordination + extraction + beneficiary + victim → 0.9 "separable, worth reform"); gating
  `has_coordination_function` on not-captured would demote captured constraints to clause 2
  ("only extraction, nothing to preserve," 0.3). Sorts **FORBIDDEN** — the opposite of what the
  module name suggests. (Off the dr_type path: reformability is consumed at scaffold-need /
  surgical-reform sites and `dirac_classification.pl`.)
- `drl_core.pl:285-288` (`natural_law_without_beneficiary`): literally `emerges_naturally` +
  `\+ requires_active_enforcement` + `\+ constraint_beneficiary` — the holds-without-enforcement
  read; beneficiary-existence already disqualifies mountain certification. Strengthening to
  capture sorts **SOUND**.
- `signature_detection.pl` NL gates (`count_power_beneficiaries` :211-223, the OQ-43 fail-close):
  mountain-likeness, same sort. And :102-108 is an IN-ENGINE witness that benefits-from ≠
  receives-extraction: P≠NP carries incidental `constraint_beneficiary` facts (authored for
  perspectival analysis) and needed a special natural-law-via-emergence interception to avoid
  CI_Rope misclassification — the engine already met the conflation and coded around it ad hoc.
- The CLASSIFICATION-PATH consumer is witnessed live in the step-2 prototype output
  (`gain_flow_prototype.out`): seats identical in metrics, role, and power split **scaffold**
  (capturer_a, capturer_f — `constraint_beneficiary` present) vs **naturalized** (bystander_b,
  bystander_g, capturer_h — absent). That scaffold-direction read on the dr_type path is the
  fight's live site and the preregistration's classification-path slice.
**RULED (2026-06-10; recording delegated by operator to CC after the review's discharge
argument, and exercised):** the per-site decision rule IS the adjudication frame — no global
exclusion between capture and coordination; per-site gating permitted (and may be correct) where
the read's question is mountain-likeness, forbidden where the read needs
coordination-despite-extraction. The hold was discharged because the rule's justification never
rested on the inverted Boltzmann anchor: it derives from tangled_rope's definition (capture and
coordination co-occur by construction), which is untouched, and the anchor's inversion was
produced BY the rule operating on corrected facts — a site's bucket flipped, not the buckets.
Positive control passed: the rule has sorted real sites into BOTH buckets (separability_factor →
forbidden; natural_law_without_beneficiary + the NL gates → sound) — a criterion sorting
everything one way would be doing no work. Consequence: the classification-path adjudication
becomes RULE-APPLICATION over the read-site pass's output, not open judgment; the step-3
preregistration carries ONE question-to-operator (the diffuse-gate tolerance), plus escalation of
only those sites the rule does not cleanly sort. Under the rule the
two resolution options below stop being rivals: per-site sorting likely yields the gate at
mountain-likeness sites and the split where coordination-despite-extraction is at stake, which
shrinks the `gain_flow` load-bearing surface relative to all-split — relevant to how tight the
diffuse tolerance must be.

**Resolution question (unruled).** What may `constraint_beneficiary/2` evidence once `gain_flow`
exists? Candidate shapes (sketched, not asserted): gate the coordination read on not-captured (a
beneficiary that *receives the extraction* is not coordination evidence); or split
benefits-from-coordination vs receives-the-extraction into distinct reads. Adjudication is the
operator's; the distinction-check discipline (build_discipline rule #2) applies before any fold,
and the wide consumer surface means any change owes per-consumer diffs (rule #3).
**Cost asymmetry the step-3 preregistration drafter must know (operator note, 2026-06-10): the
two options do not cost the same and do not expose the same surface area.** The split option is
half-built — beneficiary-role already encodes benefits-from-coordination and authored `gain_flow`
encodes receives-the-extraction, so the authored fact exists and the question reduces to which
consumers read which. That makes it cheaper than it looks AND higher-stakes: choosing it makes
`gain_flow` load-bearing across the consumer surface above instead of one capture read, raising
the diffuse-audit gate's stakes proportionally. The gate-on-not-captured option touches one
clause but folds capture into a coordination read.

**Sequencing (operator ruling, 2026-06-10):** homed BEFORE the OQ-92 step-3 build proceeds; the
step-3 preregistration must name this OQ as a known-interference item so the first corpus run
does not "discover" a fight the prototype already witnessed. Cross-refs: OQ-92 (Rulings block +
step-3 preconditions), OQ-90 (snare/piton capture split), GAP-10, OQ-83 (role-derived
beneficiary emission).

**Cross-ref note (2026-06-10, second external-review batch).** The XPrize reviewer's "snare,
right verdict wrong wire" critique is the who-bears-vs-who-benefits axis homed here: the engine
computed `competition_timeline_pressure` snare from authored `prize_pressure`/`no_exit`, but the
$101M flows to teams while trial subjects bear the risk — coercion-borne and benefit-flow point
at different parties. The critique is otherwise authoring/synthesis fidelity (the essay itself
surfaced the better "right to try" mechanism the engine did not compute); the engine-relevant
residue is exactly this who-bears/who-benefits separation. See
`audits/2026-06-10_external_review_xprize/`.

## OQ-95 — giant_component network counts phantom nodes: dangling `affects_constraint/2` targets enter the component BFS (118.9% of network on the live corpus; 259.9% on original_v6)

**Ω-type:** Ω_E (defect witnessed and counted; the fix is a scoping decision plus a generation-time validation question).

**Status:** resolved — 2026-06-10, option (a) at the shared source; evidence in `audits/2026-06-10_oq95_phantom_node_fix/`.
**Origin:** witnessed 2026-06-10 during the OQ-77 kill-condition audit (`audits/2026-06-10_oq77_serial_kill_condition/`, probe in `evidence/phantom_node_probe.txt`, writeup §5).
**Resolution:** the gating census showed all five `constraint_neighbors/3` consumers (giant_comp, drl_fpn, network_dynamics, json_report, and drl_purity_network's own `bfs_path`/cascade walks) inherited the phantom endpoints, so the fix landed in `drl_purity_network.pl`: `phantom_subject/1` (no `constraint_claim` AND no `constraint_metric`) makes `constraint_neighbors/3` symmetric fail-closed — phantom endpoints excluded, phantom subjects return `[]` (pre-fix the reverse-edge clause made phantoms traversable in both directions). Defense in depth: `giant_component_analysis.pl` `precompute_edges_loop` scopes edges to the enumerated node set (`ord_memberchk`), so component > node-count is impossible by construction. The existence test is claim-OR-metric, NOT corpus membership — demos/probsets pass; synthetic test fixtures must now author a claim (contract change; `test_forecloses_fpn_injection` fixtures updated).
**Witnesses:** live corpus 118.9% → 56.8% (44→21 nodes, 37 total unchanged); original_v6 259.9% → 89.2% (8,785→3,014 of 3,380); edges 75→49 = exactly the 26 dangling facts; post-fix phantom endpoint count 0 with firing positive control; new suite `prolog/tests/test_phantom_neighbor_filter.pl` 4/4; `fpn_injection` 6/6; validation suite 39/39 exit 0.
**Options (b)/(c) rejected with reasons** (writeup §4): dangling refs are an expected, separately-censused property of generated corpora (`dangle_curve.py` OQ-58, `reading_reference_linter.py`); contamination *values* were already phantom-inert via the `purity_score/2` `-1.0` sentinel, so once topology is closed no provenance bit is needed.

---

## OQ-96 — Validation suite RED on main: `domain_priors:category_of/2` calls a module deleted in February; first unguarded walk by a Polaris story

**Ω-type:** Ω_E (defect witnessed and attributed; fix choice interacted with the OQ-93 fork).

**Status:** resolved — closed 2026-06-11 by the OQ-93 shim retirement (exactly this entry's
close condition): `grid_shim_enabled` + injection/imputation/gate arms removed
(authored-or-absent is the only behavior); `run_pipeline` `domain_registry.pl` regeneration +
`.gitignore` fossil line retired; `python/domain_priors.py --output` made repo-relative;
`data_repair:source_class/2` injected/imputed buckets KEPT for archive replays. Witness:
before/after full-suite diff 0 unclassified lines (only the two retirement-message rewordings +
[ELAPSED] noise; per-class counts identical FAIL 0/0, OPEN 513/513, SHIM 48/48); residue grep 0
code refs to the flag with a positive control on HEAD~1. Interim fix history (throw-only clause
removal, load-warning gate + allowlist) in git and KNOWN_STATE 2026-06-10/11.
`audits/2026-06-11_oq93_grid_migration/` (phase6_*).


## OQ-97 — Success-shaped-absorption census: bounded grep of live code for Pattern-6 candidate sites

**Ω-type:** Ω_E (bounded audit task; verdicts per site are sort-on-encounter until it runs).

**Status:** resolved — 2026-06-11; census executed (`audits/2026-06-11_oq97_pattern6_census/`),
106 top-level files, three shape greps (160/227/210 raw lines), all 7 pinned positive controls
fired; class-based triage (19 classes), file-don't-fix.

**Origin:** filed 2026-06-10 (operator-directed) against `build_discipline.md` Pattern 6 after
three same-day instances (`[] → 0.0` gradient fallback, `grep -v Warning`,
findall-over-partial-levels). **Resolution:** 8 confirmed-candidate classes filed as OQ-112
(prioritized per the OQ-44 common-law ruling); none on the `dr_type` classification path
(drl_core Shape-A-clean — the census itself witnesses the OQ-44 commit-C fix). Accepted
limitations named in WRITEUP §7: static-load-directives-only denominator; 3-line Shape-B
coupling window (split idioms out of scope); sampled class adjudication. Cross-refs: OQ-93,
OQ-96, OQ-44, OQ-98 (banner site struck as witnessed join), OQ-112 (successor).

## OQ-98 — Report verdict banner is not a join over the report's own evidence: GREEN prints over 0%-authored grids and alongside severe alerts

**Ω-type:** Ω_E (defect witnessed and attributed; resolution interacted with OQ-93 provenance + operator rulings 2026-06-11).

**Status:** resolved — 2026-06-11; commits `e8ab707b` (plumbing) / `170db693` (histogram gate) / `ce9a26ec` (output-changing, schema_version 1→2); evidence `audits/2026-06-11_oq98_verdict_join/`; KNOWN_STATE 2026-06-11.

**Origin:** filed 2026-06-10, first external-review batch (`audits/2026-06-10_external_review_vote_market/`): `scale_ceiling_report.md` printed `VERDICT: GREEN / 12/12 subsystems — no tensions` over a 0%-authored grid and beside `! ALERT [severe]: type_1_false_summit`; correction-grade signature findings carried no severity tag.

**Resolution.** Headline verdict is now a join computed at the Prolog producer
(`diagnostic_summary:verdict_join/3`) and serialized WITH its raw inputs as a sibling of
`diagnostic_verdict`: joined = max-badness(base, severity floors severe→red, moderate→yellow,
informational→no floor) with a `cap_applied` token; correction-grade signatures alert at
moderate (`signature_detection:signature_grade/2` — correction iff an override signature
actually rewired the type at default context; commentary never alerts). **Operative rulings:**
(1) grid provenance does NOT gate the headline — probe P1 witnessed BRANCH A (0/48 summaries
changed under full synthetic 32-slot grids; positive control 46/46 `classify_interval` shifts),
so grid-fed findings (Pattern/Confidence, kappa) carry `[CONDITIONAL: grid authored A/T]` tags
instead, the banner ALWAYS prints the grid line, and `enhanced_report.py` headlines
`verdict_join.verdict` with stale artifacts rendering `[UNJOINED verdict — regenerate pipeline
(OQ-98)]`; **if any of the 12 subsystems ever becomes grid-fed, revert to strict fail-closed**
(authored<total ⇒ headline conditional). (2) severity=moderate for correction grade, confirmed
at the pre-output histogram gate: 8/48 headlines changed (6 green→red, 2 yellow→red, all via
severe claim-mismatch), zero moderate caps (all 13 correction carriers already base ≥ yellow).
Witnesses W1–W4 + 2 falsifiers pasted in the audit dir (RED-capped scale_ceiling banner;
CONDITIONAL tags firing on the live 0-authored regime; clean-green A/B additive-only; 4/4
sidecar==banner; injected mountain claim caps yellow→red, restore byte-verified; 13/13
correction-grade carry the join alert). Closes resolution sketch (a)-as-conditional-tags, (b),
(c) = reviewer items 1, 2, and the conditional-marking half of 5. Known edge: the kappa
CONDITIONAL branch is unreachable on the live all-absent-grid regime (kappa prints
DATA_INSUFFICIENT); it fires when a partially-authored grid first exists. Cross-refs: OQ-97
(banner site now a witnessed join; census stays open), OQ-102 (banner-side inversion fixed;
per-time-point provenance open), OQ-101 (ledger gains `verdict_join` as a ready-made source).

**Post-close substrate check (2026-06-11, operator-directed).** This OQ's motivating example
for sketch (c) was misdescribed by the report's own template prose: "an extraction mechanism
that **metrics failed to classify** as snare" (agenda_conditioning) is UNCONDITIONAL text in
`explain_signature(C, constructed_high_extraction, …)` (`signature_detection.pl:667`), printed
whenever that signature fires regardless of what the metric layer returned. Probed against the
code: `metric_based_type_indexed` == `dr_type` for agenda_conditioning at ALL four standard
contexts (powerless tangled_rope, moderate snare, institutional rope, analytical snare — the
signature never rewired anywhere), so metrics DID classify it snare and its commentary grade
is correct; possibility (b) — grade predicate excluding the divergence by context choice — is
ruled out at every altitude the report uses. Witness:
`audits/2026-06-11_oq98_verdict_join/agenda_conditioning_grade_probe.txt`; a code comment at
the template clause now flags the prose as an unchecked assertion (`signature_grade/2` is the
checked rewire fact).

## OQ-99 — Omega resolution-scenario generator: unbound constraint name prints `Constraint: unknown`; identical N=30 five-step template across all empirical omegas

**Ω-type:** Ω_E (witnessed, mechanical, output-changing).

**Status:** resolved — 2026-06-11, commit `6b1092c0` (worktree oq99-omega-scenarios).

**Resolution.** `generate_omega_resolution_scenarios/0→/1` (takes the report subject);
`resolve_omega_source/3→/4` (omega_source path, then subject binding, then fail-loud
`unresolved_source` — never `Constraint = unknown`; dead `omega_from_gap` fallback tombstoned).
Authored 5-arity `omega_variable` protocols (251 facts, 60/62 testsets) now render per omega.
The facts live in module `constraint_<id>` (NOT `user` as initially diagnosed — testsets declare
a module); the lookup keys on that module, which also disambiguates the 7 cross-file OID
collisions. Catch-all clause prevents mid-report abort. Witnesses: scale_ceiling before/after
diff (4× unknown → 0, per-omega authored protocols); gap-omega routing unchanged
(snare_masked_as_rope via omega_source); probes A/B/C (unresolved [OPEN], catch-all, typed
fallback) in KNOWN_STATE 2026-06-11. Line-drift note: filed against :572-583; at fix time the
fallback sat at :600-610 (content drift between filing and execution, same predicate).

## OQ-100 — Report register incoherence: three quantities named "confidence", "HARD DISAGREEMENT" at rival P=0.95, "ONTOLOGICAL FRAUD DETECTION" overclaims the forensic register

**Ω-type:** Ω_E (witnessed; (a)-(c) register-work, (d) structure-work — different sizes).

**Status:** resolved — 2026-06-11, commit `e9872538` (worktree oq99-omega-scenarios).

**Resolution.** (a) labels now name their quantities: `Pattern confidence (categorical):`
(Prolog), `MaxEnt P(claimed):` (MaxEnt shadow + convergence sections — a 4th bare label found
by the inventory sweep), `MaxEnt bands (corpus):` (histogram); legacy `agent/orchestrator.py`
regex + echo updated. (b) disagreement header graded by rival P with cuts extracted to
`enrich_pipeline_json.py` constants (BAND_DEEP/BAND_MODERATE, imported by enhanced_report.py):
≥0.8 REJECTED, ≥0.5 FAVORS RIVAL, None/<0.5 plurality split (explicit None guard — bare compare
would TypeError); plurality + None branches witnessed via crafted entries (zero live <0.5 cases).
(c) header → `DECLARED-TYPE vs OWN-ASSIGNED-METRICS SELF-CONSISTENCY`; code-wide grep zero
outside archives. Witnesses in KNOWN_STATE 2026-06-11. **(d) partial-closure note:** (d) is
subsumed by the OQ-101 ledger (resolved 2026-06-11) — not silently riding this resolved status.

## OQ-101 — Replace the auto-essay synthesis (orchestrator step 6) with a deterministic tensions ledger

**Ω-type:** Ω_P (design ruled 2026-06-10; built 2026-06-11).

**Status:** resolved — `python/tensions_ledger.py` LANDED 2026-06-11: non-generative extractor
(no LLM call — cannot over-state by construction) over `outputs/pipeline_output.json` + the
per-constraint reports; one bulleted block per constraint (verdict_join headline + alerts +
provenance buckets, per-position types + index mismatches, signature + grade, omegas, drift
events with the basis-projected tail (OQ-102(a)) and the report's joined severity|confidence
lines (OQ-102(b)), contamination edges with the OQ-103 provenance gap LABELED, grid coverage);
manifest stamped in the header. Orchestrator `_step_essay` REMOVED (form-not-implementation:
the essay collapses plurality under any synthesizer); step 6 = `_step_ledger`. Witness 8/8:
48/48 blocks on real pipeline output; fidelity spot-check vs two regenerated reports clean;
orchestrator step path success. The live-synthesis checklist stays the operator's instrument
(`audits/2026-06-10_external_review_xprize/README.md`). KNOWN_STATE 2026-06-11;
`audits/2026-06-11_oq93_grid_migration/phase7_*`. Cross-refs: OQ-102 (closed), OQ-103 (resolved
2026-06-12 — read-site now carries `edge_type` provenance + a `shared_agent_count` salience floor;
the ledger's contamination block can consume those fields directly instead of hand-labeling the
gap. Follow-up, not blocking: have `tensions_ledger.py` read the salience bit so floored
single-shared-agent edges drop out of the ledger too).


## OQ-102 — Drift/temporal subsystem carries no authored-vs-imputed provenance on its time series; "critical" outranks its own "confidence: low" at the read site

**Ω-type:** Ω_E (defect witnessed and attributed).

**Status:** resolved — both defects closed 2026-06-11 as riders on the OQ-93 stages, each with
its firing witness (landed-as-code-never-fired explicitly ruled insufficient at prereg):
**(a) per-time-point provenance:** optional `basis` (observed|projected) on Measurement + grid
points → compiler emits `narrative_ontology:measurement_basis/2` beside the same MID →
`measurement_provenance` grew a per-bucket `projected` count (meas_prov/5; json_report +
shared/schemas carry it). Firing chain witnessed end-to-end on a constructed fixture (nothing
live authors basis): fixture JSON → 3 emitted facts → swipl meas_prov(39,0,0,2,39) → 'projected
2/39' rendered on the tensions-ledger drift line. **(b) read-site inversion:** drift severity
tokens now join the constraint's own terminal confidence at the same line
(`[warning | confidence: low]` witnessed live on agenda_conditioning, both drift_report render
paths); enhanced_report's drift-events line carries per-event severity + series-provenance
caveat, and the TEMPORAL TRAJECTORY CONDITIONAL line surfaces the projected bucket
(before/after diff in `audits/2026-06-11_oq93_grid_migration/rider_b_drift_join_witness.txt`).
KNOWN_STATE 2026-06-11. Cross-refs: OQ-98 (join pattern), OQ-93 (provenance spine), OQ-101
(ledger surfaces both), OQ-105 (option (b) would extend the basis spine with an `interpolated`
bucket).


## OQ-103 — Contamination/purity-network edges: provenance bit serialized but inert at the read site, and no salience floor — RESOLVED

**Ω-type:** Ω_E (defect witnessed and attributed; title/scope corrected at close — see Scope correction).

**Status:** resolved — read-site fix landed 2026-06-12 (worktree `oq103-readsite-provenance`); the
synthesis-step enforcement half stays with OQ-101.

**The witness (filed 2026-06-10, `audits/2026-06-10_external_review_xprize/`).** The
`digital_colonialism_data_extraction | snare | shared_beneficiary | 0.30` edge
(`reprogramming_safety_toxicity_report.md`, Δpurity −0.0554) is a corpus-topology
`shared_beneficiary` computation, **not** authored in
`prolog/testsets/reprogramming_safety_toxicity.pl`. The article says nothing about biobanks; the
auto-essay spun a whole section off the edge (`sinclair_xprize_reprogramming_2026.md:176-180`).
Load-bearing escalation (2026-06-11, Pew review exchange): the trust↔representation
`shared_victim | 0.30` edge IS the relocation thesis of the parent essay in graph form
(`institutional_trust_erosion_report.md`, reciprocal at `representation_legitimacy_gap_report.md`)
— a corpus-derived edge carrying an essay's central claim, where authored-vs-derived is the
difference between "the source material asserts this connection" and "our corpus does."

**Scope correction at close (2026-06-12).** The filed title ("carry no provenance bit") was
wrong: the bit was already there. `constraint_neighbors/3` tags every edge with a `Source`
(`explicit | shared_beneficiary | shared_victim | inferred_coupling`); `json_report.pl`
serializes it as `edge_type`; `enhanced_report.py` already printed it in the Edge column.
`edge_type == explicit` IS the story-authored-vs-corpus-derived bit. The real defects were
(1) the bit was **inert** — renderer and synthesis step gave all edge types equal interpretive
weight, with no legend telling a consumer that `shared_beneficiary` means "the corpus computed
this, the source did not"; (2) **no salience floor** — a strength-0.30 single-shared-agent edge
headlined contamination identically to a strength-1.0 authored one. A theorized dedup-mislabel
(an `explicit` edge surfacing relabeled `shared_*` because `deduplicate_neighbors` keeps `MaxS`
strength but the first-sorted source) was CHECKED on the one live overlap pair
(`hybrid_security_reading ↔ formalist_employment_reading`, both name `platform_companies`) and
**NOT witnessed** — it surfaces correctly as `explicit 1.0`. So `edge_type` is a reliable
provenance bit in practice and no separate `authored` flag was added.

**Resolution (read-site; no engine classification change).**
- `json_report.pl`: each neighbor now serializes `shared_agent_count` — distinct agents shared on
  the edge's link type (null for `explicit`/`inferred_coupling`). This is the principled salience
  input: `edge_strength = 0.3 × count` capped at 1.0, so reading the count (not back-deriving from
  a literal 0.3) survives any edge-strength recalibration. Witnessed across all 106 live edges:
  explicit→null (22), shared_*→count 1 (82) or 2 (2); `strength == 0.3 × count` exact.
- `enhanced_report.py`: neighbor table gains **Provenance** (`authored`/`corpus-derived`) and
  **Salience** (`salient`/`low`) columns + a legend paragraph; the "primarily X" sentence now
  ranks over SALIENT edges only. Floor: authored always salient; corpus-derived agent edge salient
  iff `shared_agent_count ≥ 2`; `inferred_coupling` (zero live coverage) falls back to
  `strength ≥ 0.6`. **Empty-above-floor is explicit**: when a real negative Δ is carried entirely
  by floored edges, the sentence says "contamination is carried entirely by low-salience
  corpus-derived edges … No connection here is asserted by this case's source material" rather
  than promoting a weak edge.
- Floor demotes **82/106 (77%)** of live edges to low-salience — the network IS mostly weak
  corpus scaffolding. Both filed witnesses now render `corpus-derived | low` with the no-headline
  sentence (pipeline run `2026-06-12T04:29:38Z`, n=62).
- Unit fixture `python/tests/test_contamination_provenance_salience.py` (5/5 pass) pins the
  partition, the count-based floor, empty-above-floor, the still-headlines-when-salient path, and
  the live-uncovered `inferred_coupling` branch.

**Back-propagation declined (operator, 2026-06-11).** Post-fix the relocation-thesis edge renders
low-salience, retroactively weakening the parent essay's graph support — but no re-audit of
existing essays is owed: this corpus exists to fix these defects and is then rebuilt from scratch;
no looking backward.

**Remaining (OQ-101).** The synthesis step reading past the now-load-bearing provenance/salience
tags is enforced by the live-synthesis checklist (OQ-101), not here. Distinct from OQ-95 (phantom
BFS nodes = node membership; this is edge provenance + salience). Cross-refs: OQ-95, OQ-101.

## OQ-104 — Audit-citation integrity has no checker: paths cited from audit writeups can be untracked, gitignored, or nonexistent with nothing failing

**Status:** open — filed 2026-06-11 (OQ-33 close session; operator-flagged class)
**Priority:** 1
**Origin:** Two same-class instances in one session: `tripwire_fabricated_defaults_results.json`
cited by its 2026-05-30 audit from gitignored `outputs/` (relocated at the OQ-33 close), and
`audits/2026-02-25_spectral_laplacian/outputs/` — 25 evidence files cited by that audit's
writeup but swallowed by the unanchored `.gitignore` `outputs/` rule since creation (fixed by
the 2026-06-11 anchor, commit `09390f0f`). The anchor fixed the *mechanism* of these two
instances, not the invariant: typo'd paths, evidence left in a worktree, or the next ignore
rule arrive by different routes and read identically (writeup cites it, fresh clone lacks it).
**Candidate fix:** a cheap checker — every path cited from `audits/*/writeup.md` (and the
audit-dir READMEs) exists AND is tracked (`git ls-files --error-unmatch`); sibling of
`issues_status.py` / `known_state_status.py`, runnable standalone and at the location-mandate
seam. Open design point: citation-extraction grammar (backtick-quoted repo-relative paths is
probably enough; measure false-positive rate on the existing audits/ corpus before gating
anything). Not wired into run_pipeline until its false-positive rate is witnessed.

## OQ-105 — Suppression grid-misalignment rows: the scalar substitution sets flip TIMING in classify_at_time timelines

**Ω-type:** Ω_C (design choice — grid alignment at generation vs labeled interpolation at read; a narrow representation ruling spun off the OQ-46 close).

**Status:** resolved — 2026-06-12, operator ruled fix-fork option (a) ALONE (grid alignment at
generation; no read-side interpolation machinery); implementation landed same day
(`audits/2026-06-12_oq105_alignment_gate/`). Filed 2026-06-11 (OQ-46 Backed-reconciliation
session).

**Resolution note.** Evidence chain: mechanism + 23-row/11-constraint census
(`audits/2026-06-11_oq46_backed_reconciliation/`, OQ-110 cross-read); per-row interpolation
counterfactual over ALL misaligned rows (`audits/2026-06-11_oq105_row_sweep/`) — 4/23 rows
type-diverge (181/3588 cells across 156 contexts), every divergent cell the one snare-floor
mechanism (endpoint scalar ≥ 0.60 while the local series interpolates below → snare dated early);
19/23 robust at every context — *robust relative to LINEAR INTERPOLATION: the sweep enumerated
(b)'s payoff under (b)'s own semantics, not ground truth about suppression at those times.*
Containment held throughout: misaligned rows are `Backed=false`, OQ-110 witnessed 0 counted flips
on/adjacent. The OQ-109 Phase C cohort-zero swap (`7ca48e0b`, 2026-06-12) then retired ALL 11 host
constraints to `kernel_v2_test2` — live misaligned rows = 0 on the 5-story cohort-zero corpus
(every `_c0` story authors one shared grid; the ratified time-bound "regen the 11 within Phase C
or by a named date" was discharged by that retirement; its successor clock — alignment rule lands
BEFORE cohort-one generation — is met by this close).

**Implementation (landed 2026-06-12, witnesses in the audit dir):** prompt rule "One time grid
per story" (`prompts/constraint_story_generation_prompt_json.md`, Temporal Measurements) + the
fail-closed compiler gate `_grid_alignment_errors` in `python/generate_constraint_pl.py`'s
`validate_json` (both jsonschema and fallback paths; every generation driver imports it).
Witnesses: W1 synthetic misalignment fires; W2 all 5 live `_c0` JSONs clean; W3 the gate over the
60 archived pre-cohort-zero JSONs flags EXACTLY the 11 known hosts, zero false positives.

**Operative ruling + densification trade (recorded so it cannot resurface as a discovered
defect):** (a) trades (b)'s labeled read-side interpolation for unlabeled generation-side value
assertion at shared time-points the model didn't organically choose. Defense: model-authored-at-
generation carries the same epistemics as every other authored point — the defect was *code*
injecting endpoints post hoc, which (a) does not reproduce. The prompt rule therefore frames the
union grid as a first-class authoring requirement (assert each value, or thin to a sparser shared
grid, or drop the series — never backfill); the OQ-46 scalar-only-static rule stays orthogonal.

**Reopen conditions for (b)** (`interpolated` bucket on the `measurement_basis/2` spine — the
extension point stays alive via OQ-107's `witnessed` bucket): (1) a misaligned story reaches the
live corpus despite the gate; (2) the densification cost turns real on cohort-one generation
(grids thinned below drift-detection usefulness, or evidence of fabricated rather than thinned
values); (3) a Backed-blind consumer of raw `classify_at_time`/`constraint_history` timelines
becomes load-bearing over a corpus still containing misaligned rows (archive overlays qualify).
Cross-refs: OQ-46, OQ-44, OQ-102, OQ-107, OQ-109 (Phase C swap), OQ-110.


## OQ-106 — `structural_coercive_intent`: redesign-vs-retire sub-fork (range-dead threshold + producerless evidence tables)

**Ω-type:** Ω_C (design choice deferred by OQ-93 ruling (a), operator 2026-06-10 — must not evaporate).

**Status:** resolved — RETIRE ruled and landed (operator, 2026-06-12). Top verdict deleted
from `intent_engine.pl` (clause + `collect_intent_evidence/1` + dead helpers) with its five
config params/specs (`system_gradient_strong_threshold`, `beneficiary_gain_min`,
`structural_suppression_min`, `structural_resistance_min`, docs-only `loser_loss_max_gain`);
lower verdicts and the OQ-93 open() passthrough untouched. Intent_* tables and all
non-intent_engine readers (incl. the OQ-43 fail-closed NL gate) preserved as GAP-08
substrate. Witness: `audits/2026-06-12_oq106_retire/` (suite diff byte-identical on
substantive lines; warning-attribution residue positive-controlled as same-code run-noise).
Evidence basis: `audits/2026-06-10_oq93_grid_viability_probe/FINDINGS.md` §C3 (1.00 strict
vs 0.98 max reachable), GAP-08 (producerless tables), plus the deciding-pass finding that
the verdict token had NO consumer even if it fired (`report_generator.pl:22` imports
intent_engine `except([classify_interval/3])`).

**Still-operative ruling block (kept per compress-on-close exception).** The fork closed on
ruling (i) of the 2026-06-12 web-review exchange: **capture-as-design ratified as the piton
intension** — the snare/piton split's computed `constraint_captured/1` gate (present-tense
"maintained by a beneficiary now") carries the designed/decayed axis; origin-intent is not
type-constitutive, and Condition 2 of the dead verdict (alternatives seen and rejected) was
its only checkable form. **Kill condition (commit-plus-falsifier):** a corpus case where
proxy and intuition split — capture holds but the extraction is plainly emergent with no
design even ex post, or design is documented (authored alternative-rejection evidence) but
capture is absent, AND the operator wants those cases sorted differently. Such a case
falsifies the proxy and becomes the consuming research question that arms GAP-08's revival
condition (until then revival stays GENERIC: armed-not-scheduled was explicitly declined —
naming piton as standing candidate consumer was option (ii), not taken, to avoid an OQ-36
"build mid-baseline" license misread). Any revival = fresh preregistration (schema →
producer → threshold re-derivation → wiring), OQ-44 fail-closed on the tables.

## OQ-107 — Survey-wave witness adapter: no path from external instrument data to metrics, so every time series stays authored-or-derived and drift events are self-consistency checks, not measurements

**Ω-type:** Ω_C (design choice — a new ingestion surface; ranked first of the three engine
improvements in the operator's 2026-06-11 Pew-typology review exchange).

**Status:** future — operator ruling 2026-06-15: a real ingestion gap, but the
external-instrument adapter is work the operator does not see ever getting done; closed as
`future` (closed-but-searchable, revive if the substrate changes — full body kept below for
that reviver). Originally filed 2026-06-11 (Pew political-typology review exchange; source instrument
**Priority:** 1
`agent/analysis/originals/Pew_2026.5.10_political-typology_topline.txt`; the four story files it
grounded — `institutional_trust_erosion`, `representation_legitimacy_gap`,
`intra_party_fragmentation`, `generational_value_divergence` — untracked as of filing).

**The gap.** Story omegas carry `resolution_mechanism` prose (cross-national comparisons,
longitudinal tracking, meta-analyses — generic future-evidence text; witnessed in
`json/institutional_trust_erosion.json` omegas[0..2]), but the engine has no machinery to ingest
any of it. Meanwhile the 2026-06-11 exchange demonstrated what omega resolution actually looks
like in practice: **a survey wave IS the instance collection.** The Pew topline already contains
items that map onto engine metrics — ELITEUND-class items → directionality (the powerless-position
d), the social-trust series (SOCTRUST-class, with USASOLVE as the divergent pair) →
suppression/purity trajectories, ATTPOLRLLY-class affective-polarization items → coordination
capacity. Nothing can carry that mapping today.

**Why it outranks confidence-math improvements.** The OQ-102 `measurement_basis/2` spine (landed
2026-06-11) distinguishes authored/injected/imputed/projected — but every bucket is still
*internal*: the best case is "the author said so consistently." Low-confidence drift events
computed from authored series are consistency checks on the story's own telling, and the essays
correctly decline to cite them. A thin adapter mapping instrument items to metric time-points
would add a **`witnessed` basis bucket** — converting some authored-as-projected series into
externally measured ones, which changes drift events from self-consistency checks into
measurements. That is a provenance-kind upgrade no amount of confidence-model refinement can buy.

**Shape of the build (not ruled; sequencing only).** (a) A mapping fact schema (instrument item →
constraint metric × position × time-point, with instrument/wave provenance); (b) extend the
`measurement_basis/2` bucket set with `witnessed` (the OQ-105 fix-fork note already anticipates
extending this spine with `interpolated` — same extension point); (c) fail-closed rule inherited
from Pattern 5: an unmapped item contributes nothing, a mapped item carries its wave id. Gate
class: OQ-44 (the adapter must not let absence-of-mapping read as measured-zero). Cross-refs:
OQ-102 (the spine being extended), OQ-105 (parallel bucket extension), OQ-108 (the coverage
report this adapter would feed), OQ-83 (drift-event consumers).

## OQ-108 — Per-position witness coverage: the perspectival grid cannot report which positions are evidence-dense and which are inference-only

**Ω-type:** Ω_C (design choice — a reporting surface over existing grid data; second of the three
2026-06-11 review-exchange improvements).

**Status:** resolved — 2026-06-15: shipped the standalone authored-witness version (the
"full" OQ-107-dependent version is moot now that OQ-107 is `future`; the witness is the authored
stakeholder, not a survey wave, so OQ-107 dependence was wrong — dep dropped). `stakeholder_seats:
power_witness_count/3` + `power_witness_map/2` count named stakeholders authored at each power
atom; serialized as `perspective_witness` in `json_report.pl` (pipeline output, 64/64) and
rendered in `python/tensions_ledger.py`. A 0 = that perspective is inference-only, NOT
measured-absent — zeros are SHOWN with coverage carried to the read site (Pattern 6). Witnessed:
`geopolitical_settlement_competition` reports `powerless=tangled_rope moderate=snare` with
`powerless=0 moderate=0` authored — the argued-not-evidenced legs made visible. Originally filed
2026-06-11 (Pew political-typology review exchange). Evidence: KNOWN_STATE 2026-06-15.

**Key finding (why coverage is over 6, not the 4 perspective columns).** The witness axis is the
full 6-atom power vocabulary (powerless/moderate/powerful/organized/institutional/analytical,
docs/logic.md:293) — the AUTHORING vocabulary — which is DISTINCT from the 4-position observer
fingerprint (`logical_fingerprint:fingerprint_shift/2` probes 4 canonical observer seats;
`powerful`/`organized` have π and canonical-d but no `standard_context_for_power`, so no
perspective column). Reporting witness over the 6 needs no seat-decision collapse and loses no
stakeholders. Cross-ref: OQ-101 (the ledger surface), OQ-107 (the dropped dep, now `future`).

---

## OQ-109 — Stakeholder-layer migration completion: Phase B cutover + Phase C regen/retire (spun off the OQ-83 measurement close)

**Ω-type:** Ω_E (engineering follow-through with witnessable cutover effects; one embedded
re-witnessing obligation — the FNL regime resets at the example cutover).

**Status:** resolved — 2026-06-13 (operator ruling). The Phase C MIGRATION is complete and
witnessed (archive / schema removal / cohort-zero swap / battery / analytical-tail instruments;
population corrected to n=5). The σ/seat residual was **DISCHARGED TO A SUCCESSOR (OQ-118), not
answered**: the gated replicate spend RAN (15 draws, batch `msgbatch_01UbfPq13BcHgJKxcsqK549i`,
commit `dcfaea97`), the partition test executed, and the frozen σ/seat prediction (`5f2a626c`) was
**falsified-as-tested on exact-match fields** (Fisher two-sided p=0.649) — the structured finding
and its two graded re-test conditions live in OQ-118. The `reading_diff` re-point remains
COHORT-ONE-gated (no live stakeholder-cell story; `constraint_stakeholder/7` Unknown procedure) —
forward dependency carried into OQ-118's cohort-one scope. Filed 2026-06-11 at the OQ-83 close; R4
ruled SATISFIED (operator, 2026-06-11); corpus-scale census recorded declined-with-reason in the
OQ-83 close.

**CLOSE-OUT (2026-06-12/13, branch `oq109-phasec-closeout`; `audits/2026-06-12_cohort_zero/WRITEUP.md`):**
- **Step 0 population correction (RESOLVED):** two untracked Iran-essay stories
  (`proxy_integration_narrative`, `strategic_victory_narrative`) were loading the live corpus at
  n=7. Different generation regime than cohort zero (sonnet-4 / temp 1.0 / `seeded_from=none` vs
  `_c0`'s sonnet-4-5 / temp 0.2 / archive-seeded) ⇒ NOT cohort-zero-homogeneous. Iran-count fork
  CLOSED positive-controlled (genuine 2-story essay, not a fragment: `tensions_ledger.md` +
  `grep -l strategic_communications_geopolitical_narrative json/*.json` both return exactly two).
  Disposition = separate cohort, archived to `prolog/archives/datasets/iran_essay_2026-06-11/`
  (commit `d26d04a2`, byte-identity proven before live removal); corpus restored to clean n=5
  (pipeline manifest `2026-06-13T03:01:15Z`, `1f517a0`). NEVER mix into cohort-zero denominators.
- **Step 1 instruments (LANDED, wire-only, commit `1f517a08`):** `python/cohort_stability.py`
  (per-field draw-stability + within-vs-between distance; Pattern-5 absence-split; witnessed on
  `organization_floor`×3 + `--selftest` PASS) and `python/cohort_sigma_seat_eval.py` (parse-check
  reproduces the frozen prediction with zero drift; population gate REFUSES a verdict below 3×2,
  returns NO TEST at n=1 — never a degenerate "insufficient power" number).
- **σ/seat residual → DISCHARGED TO OQ-118 (spend ran, prediction falsified-as-tested):** the
  gated replicate spend executed via batch (`agent/cohort_replicate_batch.py`, 15 draws = 5
  contested kernels × 3, seeded from `kernel_seeds.json` through the FROZEN seed-spec so `5f2a626c`
  applies; commit `dcfaea97`). The σ/seat partition test (`cohort_sigma_seat_eval.py`, Fisher
  validated vs scipy) returned **NO SEPARATION, p=0.649** — the noise hypothesis the prediction
  pre-registered as its own falsifier was NOT rejected. The structured result (draw-stability tracks
  field-construction-type, not the σ/seat line) + two graded re-test conditions are OQ-118's subject.
  Per the frozen prediction's clause: mismatch = finding, NOT a redraw — honored (the frozen file is
  untouched).
- **`reading_diff` re-point → COHORT-ONE (carried to OQ-118 scope):** `constraint_stakeholder/7` is
  Unknown procedure on the live corpus and the `_c0` stories are perspectives-free, so the re-point
  has no live fireable positive control. Deferred to when a stakeholder-cell-bearing story lands; do
  not ship a re-point whose only "witness" is that nothing fired (inert-proving-inert).

**Inheritance (explicit):** the four-tuple surface is retirable — the cross-framing diff is
produced and preserved (18 tracked pilot-arm JSONs + `STEP4C_PARTITION.md`,
`audits/2026-06-07_stakeholder_layer_migration/`). No census gates Phase C. If the declined
census is ever re-opened (its payoff: the structure pass), OQ-83's pre-registered extended (b)
criterion governs — see the OQ-83 close note. All OQ-83 rulings of record (R1–R5, derived
contention) remain operative build specs here.

**Phase B scope (MIGRATION_PLAN.md §Phase B, same audit dir):**
- Prompt rewrite: five-questions + R5 interview REPLACES the P/T/E/S + per-seat-type sections.
  Stage C (OQ-92, 2026-06-10) already made the live prompt dual-surface additively, so cutover
  = DROPPING the four-tuple sections from the live prompt, not adding the stakeholder ones.
- NEW one-shot example replacing `agent/verification_bottleneck.json` — kills the OQ-70
  mountain-template bait convention; **cutover RE-OPENS FNL-regime re-witnessing**
  (signature-prevalence statistics reset again).
- Consumer migration per AUDIT A3 table: (a)-class Prolog (perspectival_gap →
  computed-over-seats; mountain-unanimity retire-or-recompute; validation/repair gates →
  stakeholder presence; report_generator mandatrophy gaps → rewired R5 consumer) and (a)-class
  Python; linter rules (powerless/institutional-required → role-coverage rules; Rule 18 →
  per-(C,Name) overrides).
  **SEAM DEPENDENCY (operator, 2026-06-12 — Pattern-5 gate, not a note):** mountain-claimed
  perspectives-free stories emit `invariance_check` over an empty authored table (compiler
  `_generate_tests`). Unanimity adjudication lands BEFORE or JOINTLY WITH the
  validation/repair presence gates; the empty-table emission must be WITNESSED CLOSED
  (positive control: a mountain-claimed perspectives-free story, shown gated or shown
  computing over seats) by whichever item lands second — recompute-over-seats closes it as
  a side effect; retire leaves it to the presence-gate item's scope. Two individually clean
  Pattern-3 diffs do NOT discharge this; the seam lives between them.
  **Unanimity recon (2026-06-12, read before adjudication):** the site is
  `only_mountain_classifications/1` (`signature_detection.pl:1345–1347`), guarding FCR's
  `appears_as_rope(low_extraction_profile)`. (i) Post-Phase-C failure mode is fail-OPEN:
  on an empty authored table the negated guard succeeds, so FCR loses its mountain
  protection on perspectives-free stories — second instance of the same seam class.
  (ii) Adjudication leans RECOMPUTE (the guard's product — pure natural laws are not
  "rope-appearing" — has no duplicate). (iii) **Reentrancy hazard, the reason this was not
  done in-session:** the guard runs INSIDE the signature layer; recomputing it via
  `dr_type/3` recurses (dr_type → integrate_signature_with_modal → FCR → this guard).
  The recompute must use a PRE-SIGNATURE computed type — `classify_from_metrics/6`-level
  unanimity over the canonical contexts, or the `natural_law_signature/1` profile check —
  never `dr_type/3`. Whoever lands it owes the seam positive control (mountain-claimed
  perspectives-free story) plus a reentrancy witness (signature evaluation terminates on a
  story whose guard path executes the recompute).
  **RULED + LANDED (2026-06-12; spec-corrected same day):** both named candidates FAILED
  the pinned gauntlet (the criterion working, per operator — the pin rejected everything
  offered; not grounds to loosen leg (1)). Option-4 witness failed (1/6). Final form is
  CONDITIONAL DISPATCH (the first disjunction form accidentally had C's extension — old ∨ C
  where C ⊇ old IS C; corrected same day, byte-identical revert witnessed):
  `(authored cells present -> old unanimity verbatim ; nl_certification_chain/1)`.
  Seam A1 closed (control passes via the C arm); live extension = old guard's exactly
  through Phase B; dispatch collapses to the C arm at Phase C re-witness. Extension
  question → OQ-114 (with exposure-window note). Full package:
  `audits/2026-06-11_oq109_phase_b/UNANIMITY_ADJUDICATION.md`.
  **Census (2026-06-12, operator-triggered — the class is NOT {2}):**
  `audits/2026-06-11_oq109_phase_b/b3_empty_table_census.md`. Six SILENT fail-open members
  (A1 FCR guard; A2 `test_harness:validate_per_index` vacuous-forall; A3
  `check_indexical_relativity`; A4 mandatrophy omega detectors; A5 `detect_gap_pattern` →
  `gaps:[]` collapse; A6 report PERSPECTIVAL_GAPS section) + three LOUD false-alarm members
  (B1 compiler invariance_check — the second known instance, reclassified loud; B2
  data_validation gates; B3 linter rules). A3–A5 are the existing A3-table items with an
  added explicit requirement: each migration must distinguish measured-no-gap from no-data
  (null/coverage bit, never bare `[]`). A2 and A6 are NEW seam-gate members. Every Class-A
  migration owes the seam positive control.
  **PRESENCE GATES + EMISSION SEAM LANDED (2026-06-12, commit `8e71faa4`):**
  `data_validation:agent_surface_present/1` dispatch (legacy cells → stakeholder seats →
  sanctioned authored-empty world_unchanged; fail-closed on no-surface; 5 two-sided
  controls + pipeline identity); census-B1 emission seam CLOSED two-sided (compiler gates
  `invariance_check` on perspectives presence; perspectives-free mountain story SHOWN
  GATED, metric tests kept; existing-corpus compile byte-identical); `data_repair`
  discharged no-migration-needed (`b3_data_repair_discharge.md` — scaffold bridge is
  v3.4-legacy-only, claim chain already falls through to computed). The seam-gate
  lands-second obligation is DISCHARGED by this unit. Gotcha: `data_validation` is NOT
  loaded by `[stack]` — `use_module(data_validation)` in probes.
  **R5 ZOMBIE CONSUMER LANDED (2026-06-12; pre-registered witness shape
  `b3_r5_consumer_preregistration.md` — predictions exact):** Section-7 mandatrophy
  surface EXTENDED (not forked) with `r5_zombie_crosscheck_line/1` consuming the Phase-A
  primitive `stakeholder_seats:zombie_piton_crosscheck/2` (its FIRST consumer — the A7
  dropped seam recovered). Census: 6 live firings (4 authored_zombie_uncorroborated, 2
  computed_piton_unflagged); **corroborated_zombie = 0 on the corpus — that bucket's
  semantics are witnessed ONLY by the overlay control (OPEN-2 scope-read template); do not
  cite the live diff for it.** Report diff: exactly one additive line per firing report,
  quiet control clean. CLAUDE.md mandatrophy note RETIRED per its own condition. Residual
  kept legible: `mandatrophy_resolved` is STILL a dangling schema field (the rewire
  replaced the apparatus, not the field) — Phase C retires it from the schema alongside
  `perspectives[]` (provenance: KNOWN_STATE 2026-06-07, OQ-83 A7).
- AUDIT OPEN-1/2 RESOLVED (2026-06-12, `audits/2026-06-11_oq109_phase_b/`): OPEN-1
  no-migration-needed — `cross_context_analysis/2` already computes via `dr_type/3` over
  `standard_context/1`, never reads the authored table (b3_open1_discharge.md); OPEN-2
  `epistemic_access_check/2` extended to count `constraint_stakeholder/7` seats alongside
  authored classifications (b3_open2_*.out). **Scope-read (2026-06-12):** the byte-identical
  pipeline diff witnesses NO-REGRESSION-TODAY only — the live corpus exercises no case where
  seat-counting changes an outcome (corpus is SILENT on the new semantics); the three
  positive controls are the ONLY witnesses the new semantics has. Do not cite the clean diff
  as behavior-preservation evidence AND the controls as works-evidence for the same claim —
  they witness different claims.
- **The CLAUDE.md Critical-Distinctions mandatrophy note retires here**, when the R5
  report_generator consumer lands (the note says so explicitly).

**Phase C scope (never before B):** D-fork side of the Phase C gate is SATISFIED — OQ-110
ruled branch b no-open (2026-06-11), so Phase C carries no C1/C2 schema additions and the
remaining gate is "Phase B complete" alone. **ORDERING PINNED (operator, 2026-06-12):
OQ-114 ruling FIRST → C-arm extension confirmed-or-amended → THEN the four-tuple-only
regen. RULED 2026-06-12 (OQ-114 resolved, split): C-arm extension CONFIRMED-AS-AMENDED —
Phase C builds the fail-closed per-story exclusion (institutional_trust_erosion OUT with
kill conditions; witnesses owed: excluded + two-sided control) and the re-witness inspects
organization_floor + demographic_skill_mismatch BY NAME (the C arm's first live
decisions).** Regen-first would put stakeholder-only stories under the C arm's guard decisions
before OQ-114 rules its extension — an extension later ruled wrong would have been live on
fresh corpus (same once-not-twice logic that put the D-fork ruling before Phase C). No
dependency forces regen-first: the OQ-114 archive probe runs on `corpus_path` overlays.
**Regen cross-check disposition rule (operator, 2026-06-12):** the 12 pre-existing
schema-failing JSONs (witnessed at B2, `b2_schema_failset_diff.out`) get a DISPOSITION PER
JSON at Phase C — regen-resolves (also four-tuple-only) / separately-fixed /
accepted-with-reason — never a bare "checked"; otherwise the set shrinks by inattention
and survives Phase C as permanent residue.
**The unanimity guard's C arm (`nl_certification_chain/1`) ENTERS LIVE SERVICE at this phase** — through Phase B it is
live-unwitnessed by design (every live story routes through the authored-cells arm; its
only witness is the seam control), so the Phase-C re-witnessing point is NOT skippable on
"the guard already passed B4" grounds: re-witness the guard on the post-retirement corpus,
with OQ-114's ruling governing its extension.
**HARD GATE (operator, 2026-06-12 — gate-shaped, not a list position): NO retirement or
regen commit may land before the fail-closed per-story exclusion mechanism is LANDED WITH
ITS WITNESSES PASTED** (trust_erosion shown excluded + the two-sided control of a
non-listed story reaching the C arm + the list-absent branch shown failing closed).
Schema retirement and regen both create perspectives-free stories; the moment the first
one exists the C arm decides live, with the exclusion either enforced or not — auto mode
may not reorder this on dependency convenience.
**Falsifier pinned for the trio-filters-nothing held-open (operator, 2026-06-12):** the
NL trio's filtering power is RE-MEASURED on the post-regen corpus at the Phase C
re-witness — does the trio separate certified from non-certified among the regenerated
stakeholder-only stories? Either outcome feeds the general-mechanism item; the archive
finding (C ≡ claim-mountain) may not be cited as the live corpus's state without the
re-measure. **RESOLVED at the cohort-zero battery (2026-06-12,
`audits/2026-06-12_cohort_zero/battery_witnesses.out`): the trio FILTERS on the new
regime** — of 4 mountain-claimed cohort-zero stories, exactly 1 certifies
(demographic_skill_mismatch_c0, AC=0.88/R=0.08); institutional_trust_erosion_c0
(0.71/0.58), organization_floor_c0 (0.78/0.31), scale_ceiling_c0 (0.82/0.15 — AC under
the 0.85 floor) all fail. The archive finding was an old-authoring-regime artifact, as
the stricter-authoring hypothesis predicted. **Battery outcomes (n=5, denominators are
the corpus):** named pair inspected individually — demographic_skill_mismatch_c0
PROTECTED on its own authored evidence (ruling vindicated by the redraw);
organization_floor_c0 EXAMINED because its redraw (ε=0.42, AC=0.78) is not NL-certified —
ruled-IN means the chain decides and we inspect, not unconditional protection; the
seat-theorem reading applies (new draw, new shape). trust_erosion_c0: excluded AND
chain-false — the exclusion witness holds (listed → examined); its bite is latent unless
a future draw certifies. Its redraw independently authored claim-mountain + ε=0.68 — the
substantive-dissent shape the exclusion preserved for examination, reproduced from
topic+summary alone. corroborated_zombie: none on cohort zero (flag stays armed);
adjunctification_c0 carries authored_zombie_uncorroborated (R5 surface lives in the
redraw). **The 12 pre-existing schema-failing JSONs: ALL dispositioned
archived-with-reason (kernel_v2_test2; the whole pre-cohort-zero set retired per the
cost-flag ruling).** Replicate data: organization_floor ×3 draws (ε identical at 0.42
across draws — first stability datum, AGAINST the OQ-26-contaminated expectation, WITH
the frozen σ prediction; one story, table stays OPEN pending cohort-one draws).
**PHASE C REFRAMED (operator ruling, 2026-06-12 — retirement = option 2 reframed as
cohort-zero seeding; rationale: meta-analysis requires a corpus homogeneous in generation
era and pinned in snapshot):**
1. **Archive-before-removal (hard edge):** the live corpus (62 .pl + paired .json) is
   archived under `prolog/archives/datasets/` with a SCHEMA-PINNED manifest (schema commit,
   code commit, n, date) BEFORE the removal commit — the archived corpus validates against
   its archived schema; the live schema validates only what's live; no window where the 47
   are live-invalid. The A/B pilot pair corpora archive likewise (control arm permanent,
   R4).
2. **Remove `perspectives[]` + `mandatrophy_resolved` OUTRIGHT** from the live schema;
   `additionalProperties:false` stays strict; the compiler's perspectives emission retires
   in the SAME commit (its reason — live legacy JSONs recompiling — leaves with the
   corpus). Witness: post-archival full compile of what remains live, clean. Precondition
   witnessed, not assumed: nothing from the 47 remains on a live recompile path
   post-archival (fresh positive-controlled probe; stragglers join the archive or get
   individual deprecation).
3. **Regen ALL ~60 as COHORT ZERO** under the new prompt — seeding the single-surface
   corpus, not modernizing a leaving one. The 12-failing-JSON dispositions mostly collapse
   to "regenerated"; the C-arm re-witness + the NAMED PAIR (organization_floor,
   demographic_skill_mismatch) + the trio re-measure run against cohort zero.
4. **Growth rule (schema-REQUIRED provenance, fail-closed):** every story carries prompt
   version (commit), schema commit, generation date, source-essay identifier, one-shot
   example in effect, **plus MODEL STRING and SAMPLING PARAMS (determinism-frontier
   ruling, 2026-06-12)** — the FNL inherited-signature machinery generalized per-cohort.
   An unprovenanced story is un-meta-analyzable by construction.
   **Plus, for cohort zero (witness-settled, 2026-06-12): `seeded_from: <archived
   story_uid>`** on every regenerated story — provenance/ground-truth, NOT an identity
   mechanism — and a **`draw` index** on every replicate-probe draw (three draws of one
   story otherwise share the full provenance tuple; the one that joins the corpus vs the
   two that remain probe artifacts must be distinguishable records).
   **Replicate probe rides cohort zero (same ruling):** 3–5 stories × 3 draws while the
   API is warm; field-level stability table (which fields are draw-stable — types? metric
   values? stakeholder structure? — and which swing). That table DEFINES the claims
   n=1-per-story meta-analysis may make: cross-story comparison on stable fields is
   signal; on unstable fields it is sampling noise wearing a story ID. Each regenerated
   story is ONE DRAW from a distribution (CLAUDE.md: generation is stochastic; the
   committed story is the determinism frontier — backchecking a generation tells nothing
   about the next run). **Probe amendment (2026-06-12): also compute pairwise
   fingerprint/signature distances within-draws vs between-stories** — within-vs-between
   separation is the identity-tolerance question made a number. **The distance metric
   reports positive-agreement and agreement-in-absence SEPARATELY (Pattern-5 guard,
   2026-06-12):** the witness's draw 2 matched its siblings 3/7 entirely on absence
   (voids []=[], zone negligible, coupling independent) — a metric that scores
   absence-match as match imports Pattern 5 into the calibration. **Gating relation,
   pinned: the stability table gates CLAIMS, not generation** — cohort zero does not
   wait on it; no cross-story comparison lands in any analysis until the table says
   which fields are draw-stable.
   **σ/SEAT FALSIFIER, PRE-REGISTERED (2026-06-12 — pin the prediction BEFORE any draw
   exists; freezes at its first commit, the OQ-114 pattern):** the σ/seat reading of the
   stability table (RULING block below) is the most consequential untested claim riding
   cohort zero, and once the table exists any partition reads as post-hoc confirmation.
   So: BEFORE the replicate draws run, classify every schema field into THREE buckets —
   **predicted-σ** (situation-fixed per the theorem), **predicted-seat**
   (seat-expressive: shift, reading-level classifications, perspectival fields), and
   **seed-supplied** (fields the regen driver hands the model from the archived
   material — the three draws of a story SHARE their seed, so seed-echo stability is
   input-echo, NOT evidence of σ-fixedness; mark these from the driver's prompt spec,
   not from the data). The synthesis survives iff the observed stability partition
   matches the predicted σ/seat split beyond chance ON THE NON-SEED-SUPPLIED FIELDS;
   the noise hypothesis predicts instability tracks boring covariates (field entropy,
   prose-proximity) and smears across the σ/seat line. A mismatch is a FINDING about
   where the theory's seat boundary sits — never a license to redraw predictions; a
   field the theorem underdetermines is halt-and-escalate at classification time, not
   inline-resolved. (The prediction pass occupies its seat before the table can —
   Corollary 3 applied to the probe itself.)
   **IDENTITY-ACROSS-REGEN (witness-settled, 2026-06-12;
   `audits/2026-06-12_signature_identity_witness/`): the engine's typing machinery
   carries KIND-level identity only — story-level identity does not survive a redraw and
   cannot be recovered backward by signature matching.** On the kernel_v1 naming-drift
   triple (the free natural experiment, old-prompt regime = upper bound on drift): draws
   1&3 matched 6/7 fingerprint dims with an identical shift pattern, but draw 2 shared
   NOTHING positive with its siblings (its 3/7 was all agreement-in-absence — no
   tolerance re-links it), while a topic-distinct control pair also hit 6/7
   (kind-overlap across topics is by design, `logical_fingerprint.pl` header).
   Consequence for every name-/identity-keyed Phase C item — the fail-closed exclusion
   list (institutional_trust_erosion), the NAMED PAIR re-witness (organization_floor,
   demographic_skill_mismatch), the per-JSON dispositions: anchor through `seeded_from`
   at regen time, then key on the cohort-zero story's own id; a name-keyed list
   evaluated against regenerated stories silently stops matching (the empty-table seam
   class, pre-empted here). Signature-keying the exclusion list is RULED OUT by the
   witness. What survives generation stochasticity is KIND-level meta-analysis
   (fingerprint/orbit/Boltzmann distributions over draw-stable fields) — the corpus's
   analysis contract; which fields qualify is exactly what the stability table rules.
   **RULING APPENDED (operator, 2026-06-12, citing the seat series —
   `docs/seat-theorem-v1.md`): a category shift on redraw is the mechanism working
   CORRECTLY, not identity failing.** The exercise's point is better analysis; things
   should shift with new information. A redraw occupies a new seat (different axioms
   held, different position in possibility space); verdicts are seat-indexed (Coupling
   Theorem: seat-free iff contentless — a classification that could not shift across
   draws would be measuring nothing open), and an earlier seat cannot pre-occupy the
   later one (Corollary 3). Determinism-as-desideratum is itself part of the problem.
   The analysis product is the SHAPE — do the connections, clusters, and shifts tell us
   something interesting, generate hypotheses we would not otherwise form — not
   draw-invariant truth. Corollary reading for the stability table (INFERRED, licensed
   by §6 Q5's liveness test = the σ/seat distinction asked in the world): draw-stable
   fields are situation-fixed (σ-side); draw-unstable fields are seat-expressive — the
   table is an empirical σ/seat partition, not a noise filter. "Sampling noise wearing a
   story ID" (item 4 above) indicts citing an unstable field as a story-level
   re-measurement; the shift itself, as a distribution, is analysis substrate. The
   witness's mechanical halves stand unchanged (no name/signature keying across regen;
   `seeded_from`/`draw` are provenance plumbing with no identity semantics); what is
   corrected is the "identity does not survive" valence — there was no seat-free
   identity to lose. Corollary 3 keeps the witness discipline intact: pre-committed
   confrontations still bite (reabsorption is costly and visible). **SCOPE (one line,
   so no one cites this ruling against a pipeline-determinism witness):
   "determinism-as-desideratum is part of the problem" is scoped to GENERATION — forward
   of the frontier, where seats live; behind the frontier (committed JSON onward),
   byte-identical reruns, gauntlet reconciliations, and the OQ-112 order-dependency
   class remain bugs-if-violated, exactly as before.**
5. **Snapshot rule:** corpus-relative statistics make the live corpus a moving target —
   pipeline runs that feed analysis get a snapshot ID; cross-story claims are stable only
   WITHIN a snapshot; cross-time claims cite both endpoint manifests. (Extends the
   existing manifest discipline; witnessed basis: 3 signature changes refit 57 stories.)
6. **Homogeneity falsifier (checkable at cohort two):** if cohort zero + two essay cohorts
   show cross-cohort signature drift DESPITE identical prompt/schema/example provenance,
   era homogeneity was not the binding constraint — engine stochasticity is — and the rule
   shifts to n-per-cohort large enough to average over it.
`reading_diff` re-pointed (stakeholder cells / computed seats); Build
Discipline Pattern-3 old-vs-new diffs before any retirement. **Post-reset calculus (witnessed
2026-06-11):** live corpus = 62 testsets, of which 47 carry compiled stakeholder facts and 49
carry six-questions atoms — the regen scope is the four-tuple-only minority (~13–15 stories),
far below the original full-regen estimate.

**Origin:** 2026-06-11, OQ-83 measurement close-out (this entry is the Phase B/C half of the
spin-off; the join half is OQ-110).

---

## OQ-110 — Offline residual join (observer flip-events × committer stages) + the author-vs-derive D-fork, terminating in an operator ruling

**Ω-type:** Ω_E (join) + Ω_P (D-fork ruling).

**Status:** resolved — 2026-06-11; operator ruled D-fork branch b does NOT open (derived-d stands).

**Resolution.** Join + pinned counterfactuals on the live corpus (manifest 2026-06-12T00:59:49Z,
`c22ec561`, clean, n=62): 91 backed flips → 82 ε-explained / 9 supp-explained residual (all
snare-suppression-floor crossings at the analytical seat — the bucket the pre-registered
criterion declared not-evidence-for-time-varying-d) / **0 genuinely unexplained**. OQ-105
cross-read: 0 counted flips on or adjacent to misaligned rows. The inherited consumer-side
`Backed` verification is DISCHARGED here — the OQ-33 → OQ-46 → OQ-83 → OQ-110 deposit chain
terminates (1.1 controls A/B/C + full-corpus in-process identity diff, comparator
positive-controlled). Nothing foreclosed: the C1/C2 stubs persist
(`constraint_indexing.pl:426–441`), so pair-or-nothing stays satisfiable later.
**Known signature (named so no one re-derives it):** the entire residual set was ONE
mechanism — the authored suppression series crossing the snare suppression floor (0.60)
between T1 and T2 at the analytical seat ("snare-floor mechanism"); a future join may
pre-classify floor-crossing residuals into this bucket. **Reopen condition (the ruling's
falsifier, mechanism-aware):** ≥1 backed flip on a future residual join (post-regen
substrate, manifest cited) surviving BOTH ε-pinning and supp-pinning AND not attributable
to the snare-floor mechanism. Coverage caveat for that evaluation: this close's
stage-alignment evidence was UNDERPOWERED by coverage (both-surfaces = 11/62), not null.
Evidence: `audits/2026-06-11_oq110_residual_join/` (WRITEUP.md), KNOWN_STATE 2026-06-11.

**Origin:** 2026-06-11, OQ-83 measurement close-out (this entry is the join half of the
spin-off; the Phase B/C half is OQ-109).

## OQ-111 — data_repair omega bridge queries module `IntervalID`; testsets declare `constraint_<id>` — guard fails silently, bridge imports zero (OQ-99's wrong-premise twin)

**Ω-type:** Ω_E (witnessed, mechanical; zero output impact on the live corpus today).

**Status:** open — filed 2026-06-11 by the OQ-99 close-out sweep (the wrong-module premise that
**Priority:** 1
OQ-99 hit and fixed in `report_generator.pl`, searched for elsewhere).

**The witness (probe, 2026-06-11).** After `user:consult('testsets/scale_ceiling.pl')`:
`current_module(scale_ceiling)` FALSE / `current_module(constraint_scale_ceiling)` TRUE;
`data_repair:bridge_omega_variables_pure(scale_ceiling, [], R)` imports **0** omegas while
**4** five-arity facts sit in `constraint_scale_ceiling`. Both arity branches
(`data_repair.pl:227–257`) guard on `current_module(IntervalID)` +
`predicate_property(IntervalID:omega_variable(...), defined)` and fall to `Results = []` on
miss — success-shaped absence (Build Discipline Pattern 6: measured-empty and didn't-look
collapse to one token). Live on every report run: `scenario_manager.pl:114` →
`repair_interval/1` → `bridge_v34_data/2` → `bridge_omega_variables_pure/3`.

**Why zero bite today — fact vs prediction.** The pairing census is FACT: every 5-arity OID
in the live corpus has a same-file `narrative_ontology:omega_variable/3` sibling (251/251,
census 2026-06-11). "A working bridge would be duplicate-guarded into a no-op" is a
PREDICTION about code that has never executed its success path — the duplicate guards
(`acc_has/2` in the bridge; check-before-assert in `persist_bridge_results/1`) are as
unexercised as the lookup they guard. Fine as triage justification; not a fix-time witness.
The bridge exists for UNPAIRED (v3.4-legacy) testsets — exactly where it would silently
fail. Secondary defect to not preserve blindly: a working /5 branch imports every omega as
hardcoded type `empirical` (`data_repair.pl:232`), fabricating the type.

**Resolution shape.** Either key the lookup on `constraint_<id>` (`atom_concat/3`, as
`report_generator.pl:661–664` now does — that fix is the template) with a positive-control
probe, or retire the bridge with a tombstone if v3.4-unpaired testsets are out of scope
(adjudicate by contribution, not wiring — Unwired ≠ worthless). **Fix-time witness set must
include BOTH cases:** (a) an unpaired case showing new imports (the bridge's purpose), AND
(b) a PAIRED case showing the duplicate guard actually fires on the now-working bridge — if
the guard has its own bug, the fix turns a silent no-op into duplicated omegas, a worse
defect than the one being fixed.

**Output-changing — lands alone, with this baseline.** A working bridge changes which
3-arity `narrative_ontology:omega_variable/3` facts exist at report time, and the OQ-99
authored-protocol path (commit `6b1092c0`) joins 5-arity facts against exactly that
enumeration — so this fix can change which omegas render and how, on every report run.
Same discipline as OQ-99/OQ-100: output-changing commit lands alone. **Diff baseline for
the fixer:** reports regenerated 2026-06-11 at code `04bf88ac` (post-OQ-99/100) —
`scale_ceiling` (4 omegas, all AUTHORED RESOLUTION PROTOCOL boxes),
`ai_governance_accountability` (authored boxes + 1 gap-derived `snare_masked_as_rope`),
`employment_boundary_contradictions` (no omegas). Reports are gitignored; regenerate the
baseline at the pre-fix commit via `python3 python/enhanced_report.py <ids>` before
applying the fix, then diff. **Sweep closure note:** the
census found `omega_variable/5` is the ONLY unqualified predicate testsets author (everything
else is `narrative_ontology:`/`constraint_indexing:`-qualified or plunit `test/1`);
`user:omega_variable` = zero hits in non-archive code (grep positive-controlled);
all five `user:`-qualified goals in engine code are loaders (`user:consult` ×4) or the
`user:file_search_path` hook; `current_predicate(user:` = zero. This entry is the sweep's
single finding.

## OQ-112 — Pattern-6 confirmed-candidate batch from the OQ-97 census: 8 classes, member-level sort and per-class disposition

**Ω-type:** Ω_E (sites enumerated with read witnesses; per-member verdicts and fixes pending).

**Status:** open — filed 2026-06-11 at OQ-97 close. Full class tables with member lists:
**Priority:** 1
`audits/2026-06-11_oq97_pattern6_census/WRITEUP.md` §4–§5.

**The batch (priority order, per OQ-44's common-law prioritize-by-success-shapedness;
re-ranked 2026-06-11 after the item-4 trace — see update below):**

1. **C4a — `; Signal = agrees` on absent probe input** (`diagnostic_summary.pl`, 13 sites; read
   witness `probe_abductive` :190–199: no `abd_triggers` fact → `agrees`). Vacuous agreement
   feeds the OQ-98 verdict join as absence-of-alert — green-over-absence one level below the
   fixed banner. Honest tokens (`inconclusive`/`unavailable`) already in the file's vocabulary.
   First job: sort each else-branch as data-absence (defect) vs conflict-absence (sound).
2. **A10 widened — exception/failure absorbed at consumer boundary** (elevated from old item 7
   by the item-4 trace). Stage-level `catch(_, true)` (`json_report.pl:72,76`;
   `trajectory_mining.pl:912`) absorbs EVERY maxent failure mode — witnessed vacuously
   succeeding over a live `type_error` (W16) and over `maxent_indexed_run`'s standalone
   quiet-failure (W14: hidden order dependency on `maxent_run`, runs 60/62 constraints when it
   runs at all); `catch(_, fail)` row drops (`maxent_report.pl:211`; `maxent_diagnostic.pl:395`
   — W12b); original `catch(error) → 0.0` members (`json_report.pl:415–418`, wasserstein).
   Channel-level Pattern 6: the boundary, not the site, is where measured and didn't-look
   collapse.
3. **A6 — absence-certifies-cleanliness** (`purity_scoring.pl:71,80,88`;
   `drl_boltzmann_analysis.pl:302`; `drl_fpn.pl:206`; `covering_analysis.pl:137`;
   `signature_detection.pl:1090`) — absence passes the clean gate (OQ-43 semantics). The three
   tripwired same-family predicates were NEUTRAL (unreachable on 194-row corpus); these five are
   unmeasured.
4. **A4 — `BaseEps = 0.5` / `Supp = 0` copied pair** (`boltzmann_compliance.pl:251,257`;
   `covering_analysis.pl:490,497`; `gap_diagnostic.pl:120,127`; `omega1_audit.pl:102`) — the
   drl_composition.pl:238 fabrication, flagged at 1 of 5 sites only (OQ-110 lineage).
5. **A3 — metric-fallback 0.0 on absent authored metric** (23 sites: constraint_indexing,
   maxent_classifier, invertibility_analysis, omega1_audit, genuine_findings_query,
   constraint_bridge) — the idiom OQ-44 commit C fixed in drl_core, surviving downstream.
   The sentinel interaction sub-item is TRACED (update below): dead branches confirmed, sink
   loud in isolation, live firing set empty; remaining scope here is the idiom cleanup.
6. **C4b — blind=stable trend family** (`drift_events.pl:92,437`; `intent_engine.pl:80`;
   `pattern_analysis.pl:37`; `logical_fingerprint.pl:338`) — no-data reads as measured-stable.
7. **A2 — statistic-on-empty → 0.0** (~40 sites, list in WRITEUP §4) — mean/median/slope/
   fraction on empty emits measured-flat (system_gradient twin); members co-printing N resort
   to SOUND.
8. **Low tier:** C4c `pass(no_*_data)`/`no_scaffold_needed` defaults (provenance arg present
   but collapsed by `pass(_)` reads); A7 zero-contamination-on-untyped (needs a design ruling:
   intended semantics vs absorption); B2 collectors over build-stage caches (defect only if the
   report path runs without the build step).

**Update 2026-06-11 — item-4 sentinel trace executed; verdict SILENT; re-rank applied.**
Evidence: `audits/2026-06-11_oq112_item4_sentinel_trace/` (probes v1–v3b + raw outputs).
Witness chain: (W3) `get_constraint_metrics` returns `supp=unknown` — the `; Supp = 0.0`
branches at `maxent_classifier.pl:255/:761` are DEAD; (W10/W15) with profiles present, both
standard and indexed LL paths THROW `type_error(evaluable, unknown/0)` at `is/2`
(`gaussian_log_likelihood`, `maxent_classifier.pl:123`) — loud in isolation, numeric positive
control LL=7.31; (W8) but both absent-suppression constraints (the OQ-44
`cs_axiom_contradiction` pair) lack `constraint_claim`, so the drivers enumerate 60/62 and the
atom never reaches the sink on the live corpus; (W12a) `maxent_threshold_proximity` absorbs
UNCAUGHT — clause failure before the arithmetic, the catch-free third sink a catch-grep cannot
see; (W16/W12b) every production boundary is silent (catch-true vacuous success / catch-fail
row drop). **Hazard is latent, not live:** the first claim-bearing story missing
`suppression_requirement` voids the entire maxent stage silently through the item-2 boundary.
Method note kept in the writeup: two probe iterations returned success-shaped non-witnesses
(LL=-10.0 with the dynamic profile table empty) — profile-present must be witnessed before
trusting any sink result.

Disposition per class follows the OQ-44 statute for any site touched (fail-closed on absence,
pass carries witness); output-changing fixes land alone per the established commit discipline.
Cross-refs: OQ-97 (census), OQ-44 (statute + common-law queue), OQ-98 (C4a feeds its join),
OQ-43 (A6 semantics), OQ-93/OQ-96 (pattern provenance), OQ-110 (A4 lineage).

## OQ-113 — natural_law_signature/1 is unsatisfiable by construction on the live corpus: has_viable_alternatives/2 never returns `false`; pure_natural_law subtype unreachable

**Ω-type:** Ω_E (witnessed, mechanical; Pattern-5 absence-gate stack).

**Status:** open — filed 2026-06-12 by the OQ-109 unanimity adjudication (candidate-B gauntlet).
**Priority:** 1
**Deps:** bundled_with OQ-43, bundled_with OQ-44

**The witness.** `signature_detection:has_viable_alternatives/2` has exactly two clauses:
`true` (an `intent_viable_alternative` fact exists) and `unknown` (catch-all) — `false` is
never produced. `natural_law_signature/1` requires `HasAlternatives == false`, so the
conjunction is unsatisfiable on any corpus authoring zero `intent_viable_alternative` facts
(live corpus: zero). Live-62 probe: true-set = 0 including six authored-unanimous-mountain
physics stories (`audits/2026-06-11_oq109_phase_b/unanimity_adjudication_probe.out`). This
STACKS on OQ-43's leg (`BeneficiaryCount == 0` reads empty `intent_power_change` —
vacuously true): one conjunct passes on absence, its sibling FAILS on absence, so the
predicate is dead while looking profile-gated. Corollary:
`determine_pure_subtype/2`'s `pure_natural_law` branch is UNREACHABLE — purity subtyping
never emits it on the live corpus.

**Resolution shape.** Adjudicate by contribution (Unwired ≠ worthless): what should
NL-profile certification require — an authored absence-of-alternatives datum (then
fail-closed needs an authoring surface for it), or the OQ-43-class fix (gate on
authored-presence before comparing)? Cross-refs: OQ-43 (sibling leg), OQ-44 (gate class
audit), OQ-37 (`unknown` sentinel discipline — the sentinel here works correctly; the
consumer's `== false` test is what can never see it).

## OQ-114 — Are the 3 bridge-extension stories genuine mountains? (FCR un-fire adjudication + archive divergence-rate probe)

**Ω-type:** Ω_E (measurable divergence rate) with an Ω_P edge (the per-story mountain ruling).

**Status:** resolved — 2026-06-12; probe ran under the frozen criterion, OUTCOME 3 (mixed),
operator ruled the live 3 SPLIT: `organization_floor` + `demographic_skill_mismatch` IN
(benign/artifact-shaped dissent; the C arm's FIRST LIVE DECISIONS — flagged BY NAME for
individual inspection at the Phase C re-witness, never absorbed into the aggregate diff);
`institutional_trust_erosion` OUT (substantive snare/piton dissent converging with the live
FCR firing — two independent instruments agreeing in the fail-open direction).
**Exclusion kill conditions (both directions, pinned at ruling):** flips IN if the
snare/piton dissent resolves (seat-dissent traced to a mechanism the instruments discount
— e.g. its duplicate-seat pairs shown to be authoring noise — or the FCR firing shown
spurious by its own witness); becomes PERMANENT-WITH-REASON if the dissent confirms as a
genuine non-mountain reading (at which point "why does C certify it" feeds the
general-mechanism build item). **Exclusion mechanism (Phase C build item): per-story
exclusion list read at dispatch time, FAIL-CLOSED — list present + story listed → old-guard
semantics; list absent/unreadable → halt or old behavior, NEVER silent C (the seam class
does not get instance three built into the fix for instance one). Phase C witnesses owed:
trust_erosion shown excluded + two-sided control (a non-listed story shown reaching the C
arm).** **Rider verdict (recorded so the conjunct is not re-proposed): option 4's
no-beneficiary conjunct was WRONG, not merely over-restrictive — unanimous (both-cell)
mountains declare beneficiaries too (7/32 kernel_v1, 12/411 v6), and beneficiary-carrying
C-only stories read as mountains under every named instrument; beneficiary-presence is an
FSM routing signal, not a mountain disqualifier (it correlates with seat-disagreement —
56%/32% vs 22%/3% — but does not separate substantive from artifact dissent).**
Evidence: `audits/2026-06-12_oq114_archive_probe/` (WRITEUP.md; four-cell tables kernel_v1
41→32/0/9/0, v6 430→411/0/19/0; all 28 C-only inspected; NL trio filters NOTHING on
archives — C ≡ claim-mountain there). Filed 2026-06-12 at the OQ-109 unanimity ruling. **Exposure-window correction:**
the first bridge (disjunction, commit `790bb009`) accidentally made C's extension LIVE on
main until the same-day dispatch commit — the un-fire and downstream effects (3 signature/
pool changes, 3 maxent_top_type flips, regulatory_measurement_gap verdict_join yellow→red,
57-story ensemble shift) WERE in pipeline output during that window; enumerated in
UNANIMITY_ADJUDICATION.md §Spec correction for tracing any consumer. The dispatch commit
reverted main to the old extension byte-identically; this question is NOT pre-answered.

**The question.** The bridge guard (authored-cells ∨ nl_certification_chain) extends FCR's
mountain-protection to 3 mountain-claimed NL-certified stories whose authored seats were NOT
unanimous: `demographic_skill_mismatch`, `organization_floor` (were coupling_invariant_rope),
`institutional_trust_erosion` (was false_ci_rope — a LIVE FCR un-fired at the bridge commit).
Are they genuinely mountains, and should that un-fire stand? All 3 declare beneficiaries, so
FSM scrutiny is untouched either way.

**Method (precondition checked 2026-06-12).** Archive divergence-rate probe: on
`archives/datasets/kernel_v1` (1,106) and `original_v6` (3,380), compare authored-cell
unanimity vs `nl_certification_chain/1` per story; the divergence rate + per-story
inspectables turn the live-62's 3-story anecdote into a measurement. Atoms witnessed present:
constraint_claim 1106/1106 and 3380/3380; NL-profile trio SPARSE (emerges_naturally 42 / 433,
accessibility_collapse 51 / 519, resistance 45 / 466) — so the comparison denominator is the
mountain-claimed subset; stories lacking the trio fail the chain closed (correct, and must be
reported as not-certifiable, never lumped with certified-divergent). Overlay `corpus_path`
per the OQ-89 pattern; chimera-era caveats (OQ-25) apply to v6 counts. Fallback if the probe
disappoints: live-62 per-story inspection of the 3 (their text, beneficiary structure, and
whether the FCR firing was OQ-70-class bait or detection).

**PINNED CRITERION (operator, 2026-06-12 — written before any archive number exists;
freezes at the probe's first commit; halt-and-escalate if wrongly specified mid-run,
never inline-amend):**

*Population & comparator (two traps pinned shut).* Per archive: stories with
`constraint_claim = mountain` AND authored perspectives PRESENT — both conditions, because
the comparator (old authored-cell unanimity) is only computable where seats exist. Trap 1
(the house failure mode): an archived perspectives-free story makes forall-unanimity
VACUOUSLY true — those stories are bucketed OUT with a count, never folded in; the
comparator carries its own positive control (shown finding the old-6 analogues or a known
seat-disagreeing story) before any divergence is counted. Trap 2: `nl_certification_chain`
reads LIVE config params (natural_law_collapse_min / resistance_max) — archived stories
were authored under earlier regimes; the probe uses the live params (the question is the
live guard's extension, not era-faithful reconstruction) and SAYS SO in its output.

*The four-cell table, direction asymmetry pre-stated.* Per story: unanimity-verdict ×
C-verdict. The divergence cells are NOT symmetric: **C-only** (C protects, unanimity
doesn't) is the FAIL-OPEN direction — FCR suppressed where the old guard would let it fire
(the live-3 shape); **unanimity-only** is fail-closed — noisy but safe. Both reported with
denominators, kernel_v1 and original_v6 SEPARATELY (chimera caveat standing): a direction
holding in both archives is evidence; a v6-only pattern is discounted.

*Inspection instruments, named in advance (derived from NEITHER candidate):*
classify_from_metrics-level type, snare-floor position, FSM/theater signals. Sample rule:
ALL C-only cases if ≤25 per archive, else a seeded random 25, seed recorded.

*Outcome meanings, written before any number exists:*
1. **Old extension CONFIRMED** iff the C-only divergences inspect as a coherent
   over-extension MECHANISM — seats disagreeing for substantive reasons the metric trio
   doesn't see (standing candidate: the beneficiary pattern — 5 of the old 6 declare
   beneficiaries; the archives test whether beneficiary-declaration co-occurs with
   seat-disagreement among C-certified stories). Consequence pinned: the C arm is
   RESTRICTED before Phase C live service; the live 3 stay excluded; the restriction's
   design becomes a build item with the mechanism as its spec.
2. **C's extension LICENSED** iff the C-only divergences inspect as comparator ARTIFACT —
   seat disagreement tracing to authoring noise, bait-era templates, or vacuity-adjacent
   authoring — while the certified stories read as mountains under the named instruments,
   **in BOTH archives (operator-ratified: the change is fail-open-direction and the
   chimera caveat disqualifies v6 as sole support — either-archive does NOT suffice)**.
   Consequence: the live 3 are inspected under the same instruments and ruled in; FCR
   un-fires on institutional_trust_erosion BY RULING this time, with the exposure-window
   entry cross-referenced.
3. **ESCALATE** if mixed or no coherent mechanism: the four-cell table plus per-story
   inspectables go to the operator; the live-3 ruling is operator judgment on that
   package. This is the honest default — pre-stated so the probe cannot be tortured into
   outcome 1 or 2.

*Zero-divergence case (amendment before freeze, operator 2026-06-12).* Outcomes 1 and 2
presuppose C-only divergences exist to inspect. **Near-empty C-only cells (< 3 per
archive) route to OUTCOME 3**, with the live-3 direct inspection under the named
instruments as the escalation package — zero divergence is neither mixed nor incoherent;
it is *C tracks unanimity at scale*, which makes the live 3 an anomaly relative to both
archives and reframes the package (why do they diverge where the archives don't?). **And
per the empty-result rule: an empty C-only cell counts as evidence of tracking ONLY after
the comparator's positive control has shown the probe can find divergence** — an empty
cell from a broken comparator is byte-identical to one from genuine agreement. Control
satisfiability is verified EARLY: if the archives contain no identifiable seat-disagreeing
story to serve as the control, that HALTS under the wrongly-specified clause; it is never
quietly waived.

*Riders.* (a) Whatever the outcome, the no-beneficiary conjunct's failure is RE-READ
against the archive evidence: if beneficiary-declaration does not anti-correlate with
genuine mountains at scale, option 4's failure was the conjunct being WRONG, not merely
over-restrictive — recorded in one sentence so it is not re-proposed. (b) Standard text:
halt-and-escalate if the criterion proves wrongly specified mid-run; the pin freezes at
the probe's first commit.

**Resolution shape.** Per-story ruling on the 3 (operator) informed by the rate read under
the pinned criterion; if ruled not-mountains, the chain needs a discriminating conjunct
(witnessed, not beneficiary-presence — that one failed extension-preservation 1/6) or the
un-fire reverts at Phase C re-witness.

## OQ-115 — signature_grade/2 references abductive_helpers under [stack] where the module is a PHANTOM (load-path-dependent; check_stack regression vs the 2026-06-04 baseline; pre-dates Phase B)

**Ω-type:** Ω_E (witnessed, mechanical, OQ-57 class).

**Status:** open — filed 2026-06-12 by the B4 gauntlet reconciliation (the one check_stack
**Priority:** 1
divergence not on the expected-divergence manifest; investigated to attribution).

**The witness.** check_stack reports `abductive_helpers:known_override_signature/1`
undefined, referenced by `signature_detection:signature_grade/2` (:1568). Under bare
`[stack]`: `current_module(abductive_helpers)` TRUE but `module_property(_, file(_))` FAILS
— a phantom module created by the qualified references; the call throws existence_error.
The predicate IS defined+exported (`abductive_helpers.pl:22,60`). Three use_module sites
exist (abductive_engine, abductive_triggers, diagnostic_summary) — NONE loaded by [stack].
The PIPELINE chain loads it via json_report → diagnostic_summary, so signature_grade and
the verdict_join alert path (diagnostic_summary:650) work where the pipeline runs —
witnessed by the green B4 gauntlet. Present at pre-Phase-B `c22ec561` (temp-worktree
witness, B4 reconciliation); NOT on the 2026-06-04 check_stack baseline (the OQ-98-era
alert path created the reference after it). The in-file comment
(signature_detection:1555–1559) claims the load is guaranteed — falsified for the [stack]
chain.

**Bite.** None in the pipeline today; real for any [stack]-only consumer of
signature_grade/2 (probes, REPL diagnostics) — exactly the OQ-57 lesson: diagnose on the
consumer's exact load chain. **Resolution shape:** import abductive_helpers in
signature_detection (the referencing module owns its imports), or add it to stack.pl;
either way update the check_stack baseline note and the :1555 comment. Engine-touching —
lands outside B4.

## OQ-116 — Two linter rules unmigrated for the cohort-zero authoring regime: SCAFFOLD_DANGER_ZONE calibration; MOUNTAIN_METRIC_CONFLICT contradicts the claim/metric-independence doctrine

**Ω-type:** Ω_E (advisory-lint calibration; witnessed on the cohort-zero pilot).

**Status:** resolved — MOUNTAIN_METRIC_CONFLICT half split-closed 2026-06-14; the
SCAFFOLD_DANGER_ZONE calibration half is refiled as **OQ-127** (open; status honesty — a
resolved+promotable principle and a live calibration call cannot share one status field).
Original filing 2026-06-12 at the pilot witness (`audits/2026-06-12_cohort_zero/pilot_witness.out`):
5/7 fired SDZ, 1/7 fired MMC (claim=mountain, ε=0.68); driver-owned pilot checks were 7/7 PASS.

**Ruling (operator, 2026-06-14): the linter is for the operator, not the engine — linting
stories would be orchestrated bias; lint never gates generation; threshold-coupled codes never
reach the authoring LLM (de-leak-in-reverse; the OQ-74 mirror).** Landed as a single chokepoint
(`linter.build_author_feedback`, the SSOT for `THRESHOLD_COUPLED_LINT`) through which every
feedback→prompt path is routed, and as a principle in `docs/design/design_discipline.md`.

**MMC resolves** because the claim-vs-metric divergence it flags is authored signal the engine
measures by computing a different SEATED reading — and per **OQ-74 / the seat theorem these
readings need not collapse to one true type** (the mountain claim and the metric reading both
stand; the divergence is the signal). Its only defect was message-text leakage (the authoring
imperative "reduce extractiveness / reclassify"); reworded this session to an operator readout
framed as a non-collapsing seat divergence, kept as an offline diagnostic, stripped from author
prompts.

**Correction to THIS entry's own premise (audit discipline — doc-contradicting, treated):** the
original text said "the engine MEASURES that divergence (FSM exists for it)." **FSM is the wrong
signal** — `false_summit_mountain/2` (`signature_detection.pl:1503–1517`) requires metrics WITHIN
mountain range (ε ≤ 0.25) plus a beneficiary; the firing had ε=0.68 ABOVE it. The engine analog
is the **metric classifier** (high ε → snare/rope/tangled_rope) with **FNL** a Boltzmann-gated
signature override. Engine-witnessed over **all 9** live-corpus MMC firings
(`audits/2026-06-14_oq116_mmc_engine_witness/`): the metric seat diverges from the mountain claim
9/9 (snare ×4, rope ×3, tangled_rope ×1, unknown ×1); FNL fires only **1/9** (Boltzmann-gated).
The named cohort-zero story `institutional_trust_erosion_c0` → snare, FNL=no. So: metric-seat
primary, FNL secondary; "FNL is the analog" is right in kind but FNL is the minority route.

**Bar discrepancy (was "recorded, not amended") — AMENDED 2026-06-14:** `pilot_witness.py` now
filters threshold-coupled codes through the centralized set before the "lint clean" assertion, so
operator-readout codes no longer flip PASS→FAIL.

**Evidence/build (commit `3587782b` + 2026-06-14 turn):** `linter.py` MMC reword + de-leak SSOT
chokepoint; `regenerate_stories.py`/`agent/cohort_zero_regen.py` routed through it;
`python/tests/test_deleak_chokepoint.py` (non-circular end-to-end + census tripwire guard);
`pilot_witness.py` bar fix; `docs/design/design_discipline.md` linter principle;
`audits/2026-06-14_oq116_mmc_engine_witness/`. SDZ half → **OQ-127**.

## OQ-117 — Claim/metric co-authoring at SCOPE manufactures claimed-vs-computed concordance: a validity condition on the divergence machinery wherever the manifest path feeds both sides

**Ω-type:** Ω_E (witnessed, quantifiable concordance manufacture) + an Ω_P design call (decouple
at SCOPE vs document-as-condition).

**Status:** resolved — (c) RULED **document-as-condition**, seat-framed (operator, 2026-06-13);
decouple withdrawn as an ε-layer artifact; two forward items filed (OQ-119 join-diff, OQ-120
rope/snare). Ruling block at the foot of this entry. Originally escalated 2026-06-12 out of OQ-78's evidence pass (operator-directed: own
entry, not a paragraph inside OQ-78's resolution). Same-day rulings: free-gate re-weighting
principle added at filing (below); the (c) design call is SEQUENCED AFTER OQ-78's fate-2 read
(now the OQ-109 Phase C cross-arm read — the bin-withdrawal probe was HALTED pre-spend).
**Same-day mechanism CORRECTION (see block below): the live co-authoring channel is
HYPOTHESIS-feeding, not bin-feeding — epsilon_bin never reaches the generation prompt on any
production path** (`audits/2026-06-12_oq78_dead_bin_channel/`).

**The problem:** SCOPE co-authors `hypothesis` (→ `claimed_type`, the CLAIM side) and
`epsilon_bin` (→ `base_properties.extractiveness`, the METRIC side) in the same upstream act
(`prompts/uke_scope_v2_json.md`; mapping table `prompts/constraint_story_generation_prompt_json.md:756`).
The generation prompt's "Claim/Metric Independence" checklist item asks the generator to author
the two sides independently — but both arrive co-authored from upstream, so independence at the
generation seat cannot be assumed for any manifest-fed story. Witnessed consequence (OQ-78
cross-tab, kernel_v2_test2 n=60): ε tracks claimed_type in nearly separable bands (snare
0.68–0.78, mountain 0.02–0.15); recorded-bin conformance 15/15.

**The sharpener (boundary alignment, OQ-78 leak re-check):** two bin boundaries coincide exactly
with classifier thresholds — 0.10 = `piton_epsilon_floor` (Rule Z), 0.30 =
`tangled_rope_epsilon_floor`. For bin-conformant stories ε cannot fall on the disqualifying side
of those gates: "low" pre-satisfies Rule Z; "mod"/"high" with a tangled hypothesis pre-satisfies
the tangled ε floor. Concordance on those gates is manufactured, not measured. The rope/snare
split (0.45/0.46) sits inside the "mod" bin and remains a free decision boundary.

**The free-gate residual (re-weighting principle — operator amendment at filing, 2026-06-12):**
the UN-transmitted gate is where the evidence lives. The rope/snare split is the only ε decision
boundary no bin edge touches — and it is exactly where the old corpus separates cleanly without
instruction (claimed ropes ≤0.28, tangled ≥0.42) AND where the only genuine divergences live
(the three kernel-reading ropes at 0.48/0.68, failing the rope ceiling by authored intent). The
free boundary is the one place the divergence machinery was exercising judgment rather than
reading back instruction. The consumer inventory in (a) is therefore a WEIGHTING, not a list:
divergence statistics are evidential in proportion to how free the gate is — currently exactly
one free ε gate. Corollary: OQ-78's earlier "bands consistent with config thresholds"
observation is retroactively sharper at this boundary — consistency at the untransmitted gate is
either idiom or shared ancestry, and the OQ-78 probe arbitrates (the ancestry question itself
ruled archaeology, not chased).

**Mechanism CORRECTED (2026-06-12, same day — pre-flight recon for the OQ-78 probe;
witnesses W1–W3 in `audits/2026-06-12_oq78_dead_bin_channel/`):** `epsilon_bin` NEVER reaches
the generation prompt — all three production paths (unified backend, gkc kernel path,
c-orchestrator inline) feed `Hypothesis type` and not the bin; the prompt's mapping table is
instruction-without-data; the recorded uke_scope blocks are model-fabricated (free-text
tokens, fabricated dates), so the 15/15 bin-conformance was SELF-LABELING. What this
re-scopes, honestly stated:

- **The live circularity channel is the HYPOTHESIS token** — the claim side is instructed on
  every manifest-fed story (`Hypothesis type: snare` → claimed_type echo), and ε is the
  model's prior GIVEN the instructed claim. The validity condition on divergence machinery
  STANDS, with this mechanism: low claimed-vs-computed divergence on manifest-fed stories
  partly measures hypothesis-echo, not authoring honesty.
- **Manufactured concordance via bin-conformance does NOT operate in production.** The
  boundary-alignment sharpener survives as a SCOPE-internal fact with one indirect path:
  bin and hypothesis are co-authored at SCOPE, so the disclosed boundaries (0.10/0.30) can
  tip the HYPOTHESIS choice near a threshold; the hypothesis is what travels downstream.
- **The free-gate residual generalizes:** no ε gate receives numeric instruction in the live
  pipeline — on the ε side ALL gates are now free; the fed side is the claim, uniformly. The
  re-weighting principle survives restated: divergence evidence is discounted by what the
  CLAIM side was fed, not by ε-gate geometry. (The original one-free-gate version was
  premised on the bin mechanism and is superseded with it.)
- **`epsilon_bin` is a Pattern-1 dangling wire:** produced by SCOPE, consumed only by two
  streamlit display lines and a mapping table whose data never arrives. Disposition (wire /
  retire / gap) folds into (c) — NOTE: wiring it would CREATE the per-story numeric-adjacent
  instruction channel this OQ interrogates, so the default is NOT re-wire.

**Why a validity condition, not a caveat:** the divergence machinery (FNL/FCR/FSM firings,
claimed-vs-computed concordance rates, Boltzmann compliance) reads claim-metric divergence as
authored signal. Where claim and metric are co-authored upstream, LOW divergence measures
pipeline construction, not authoring honesty — OQ-70's shape (authoring convention read back as
detection). The witnessed counterexamples — three kernel-reading ropes at ε 0.48–0.68
(techno_optimist_reading, flat_control, post_1998_convergence) — are deliberately authored
exceptions: they prove the channel can be opened on purpose, not that it is open in routine
generation.

**What would resolve (sequencing ruled 2026-06-12: (b) before (c); (b) re-routed same day):**
(a) consumer inventory AS A WEIGHTING (per the corrected residual) — which divergence consumers
read manifest-fed stories, and how much of each one's evidence rides the instructed claim side
(hypothesis-echo) vs authored structure;
(b) the OQ-78 fate-2 read over OQ-109 Phase C (zero marginal spend; archive = fed arm /
comparator, cohort-zero regen = withheld arm on matched seeds) — compare divergence rates and
the ε-grid endpoints across arms; a collapse there leaves hypothesis-vs-rest unresolved and
would motivate the finer hypothesis-only arm (new design, new spend);
(c) operator design call, GATED ON (b) — DECOUPLE at SCOPE (author the hypothesis without
feeding it downstream, or feed it flagged) vs DOCUMENT-AS-CONDITION (divergence statistics
over manifest-fed stories carry a "claim side instructed" provenance bit); plus the
`epsilon_bin` dangling-wire disposition (wire / retire / gap — default NOT re-wire, see the
correction block). Decision logic, recorded ahead: if the Phase C read shows the grid/banding
mostly INSTRUCTION (collapses without the manifest), decoupling buys back real authoring
variance and is worth the surgery; if mostly IDIOM (persists), decoupling buys almost nothing
and document-as-condition is the honest fix — the perturbation principle already frames ε as
authored, so documenting the coupling does not betray the design; pretending independence
would. Direction-of-fix discipline from OQ-78 applies: no numeric disclosure to any authoring
stage; no tightening of bin boundaries toward config thresholds.

**Cross-refs:** OQ-78 (mechanism + evidence + probe), OQ-116 (MOUNTAIN_METRIC_CONFLICT lint
contradicts the same independence doctrine, lint side), OQ-70 (convention-read-as-detection).

**EVIDENCE (2026-06-13, NOT a ruling — `audits/2026-06-13_oq117_within_arm_proxy/`):** the (b)
read was attempted over the now-landed Phase C cohort. Two findings, sequenced as the writeup
pins them:

1. **(b)-as-specified is NOT computable without funding a fed arm — corrects the prior "near-free"
   framing.** Comparator positive control (`comparator_positive_control.txt`): the probe is live
   (exact-name search FOUND the withheld replicates), and on that live probe **the matched fed arm
   does not exist** — `kernel_v2_test2` holds only `manifest.json` (0 stories), no fed-arm story
   was ever generated for the 5 σ/seat kernels (only `dcfaea97`, the withheld spend), and kernel_v1
   supplies only topic-adjacent siblings under different kernel identity, reading-split, pre-de-leak
   schema with **no `claimed_type` field** (PERSPECTIVE-prose, OQ-70 regime) — three stacked
   confounds, not a comparator. So the literal cross-arm divergence-rate read needs a **matched
   fed-arm spend** (Spend B), it is not free.

2. **Within-arm proxy — computable, run, MECHANISM-ESTABLISHED / DIRECTION-CONFOUNDED.**
   (`within_arm_proxy.txt`: manifest hypothesis = would-be-fed claim vs withheld replicate's
   freely-authored claim.) `claimed_type` = mountain **15/15 draws, draw-stable** → the claim is
   reconstructed from title+domain+summary alone (the hypothesis token is not what selects it):
   **mechanism established.** But all 5 kernels were *selected as contested-naturalization kernels*
   and naturalization IS the mountain claim, so "mountain 15/15" cannot separate idiom-default
   (→ document-as-condition) from these-kernels-are-mountains (→ hypothesis did no work): **direction
   confounded.** It does NOT lean document-as-condition — reading it so is the confound leaking in.
   Clean cells, untouched by the confound: **`free_market_naturalization`** claimed mountain @ ε=0.68
   stable across 3 draws → claimed-vs-computed divergence **persists with the hypothesis withheld**
   (within-story gap; summary smuggles the claim, ε authored honestly) — the witnessed shape (b)
   exists to find; and **`printing_press_reformation`** the lone ε-unstable kernel (0.38/0.42/0.68),
   an OQ-118 cast-instability echo on the metric side.

**The (c) fork, well-posed (operator's floor, two distinct spends — DO NOT conflate):** (i) accept
the mechanism + free_market's ε-divergence as sufficient for **document-as-condition** — defensible,
but the close note MUST state the *direction* was not established, only the mechanism, and MUST NOT
claim the proxy showed mountain-is-the-idiom; OR (ii) fund **Spend A — non-naturalization withheld
draws** (~5 non-naturalization contested kernels × 3 via `cohort_replicate_batch.py --ids …
--draws 3`; 42 candidates in `kernel_seeds.json`; no code change), which kills the selection confound
and converts "mechanism-established" into a rulable direction. **Spend B — matched fed-arm** (same 5
σ/seat kernels × 3, hypothesis fed; needs a seed-spec change to feed the hypothesis) is the
*separate, optional* buy for the literal rate (b) names; it does NOT touch the selection confound.
Cost (batch API, sonnet-4-5, priced off `dcfaea97`): **≈ $1 per 15-draw batch** (output-dominated;
≈ $0.77 cached / ≈ $1.11 cold), **≈ $2 for both** — dollars negligible; real cost is Spend B's code
change + analysis/witness time. Direction-of-fix discipline from OQ-78 still binds: no numeric
disclosure to any authoring stage.

**SPEND RESULTS (2026-06-13, both arms run — `audits/2026-06-13_oq117_within_arm_proxy/RESULTS.md`;
freeze `9aa1c90e` preceded both batch IDs). Still NOT a ruling — escalated.**

- **Spend A — Outcome A2 (STRUCTURE-TRACKING), conclusive. mountain 0/15.** Five non-naturalization
  kernels (impositions/coerced-reversal/transition/decline) all authored their honest *non-mountain*
  claim withheld (turkish/meiji→scaffold, gold_to_fiat→tangled_rope, mormon→snare/tangled_rope/rope,
  dueling→rope). **The within-arm proxy's selection confound is KILLED:** "mountain 15/15" on the
  naturalization kernels was **correct-read, not idiom** — the model authors `claimed_type` as a
  structure-tracking read of the summary. (c)-implication, claim channel: honest/summary-reconstructed
  → feeding the claim is redundant → **document-as-condition suffices on the claim side.**
- **Spend B — Outcome B3 (AMBIGUOUS by the frozen headline), informative structure.** Fed claim =
  mountain; ε is the measurement. **Headline free_market fed-arm mean ε = 0.427** (withheld was 0.68
  dead-stable) — lands in the frozen 0.40–0.55 dead band → **B3, reported as ambiguous, NOT narrated
  into B1 (≤0.40).** The structure: feeding moves ε **only at the divergent cell** — free_market
  −0.25 and destabilizes 0.68→[0.42,0.18,0.68] (one draw collapses to the mountain floor), total_war
  −0.10 to floor; inert where ε was already mountain-consistent (qwerty/zero Δ=0); printing_press
  perversely +0.10. Set-mean Δ −0.05. **Partial, divergent-cell-localized manufactured concordance —
  neither uniform (B1) nor inert (B2).**
- **Combined (c) read, escalated (operator's floor):** document-as-condition is right for the
  **claim** side (A2 conclusive); the **metric** side shows a **real but bounded** decouple rationale,
  localized to cells where claim and metric honestly diverge, that did NOT clear the frozen B1 bar.
  The call is genuinely **intermediate** — not a clean decouple, not a clean document. Per the freeze:
  escalated, not narrated to a verdict. Discipline honored: freeze-before-draw, ambiguous-reported-as-
  ambiguous, escalate-don't-redraw, no numeric disclosure to authoring.

**ENGINE ADJUDICATION (2026-06-13, operator redirect — supersedes the hand-ε layer for the mountain
case; `audits/2026-06-13_oq117_within_arm_proxy/ENGINE_ADJUDICATION.md`).** The spends measured
authored `claimed_type`+ε by hand; the engine's *purpose* is to adjudicate the mountain claim. Ran
the full pipeline on all 30 mountain-claiming stories (15 withheld + 15 fed) in an isolated worktree
(live n=5 corpus untouched). **0/30 ratified as mountain** (`true_mountain_report`: Total Validated
0; computed mountain 0/30). Three gates by ε band: false_natural_law + **Boltzmann non_compliant** +
`! ALERT [severe]: type_1_false_summit` → tangled_rope (free_market, printing_press); false_ci_rope →
tangled_rope/piton (qwerty); **coupling_invariant_rope** certified-genuine → rope but claim corrected
mountain→rope (total_war, zero_as_number). **Feeding the claim does not change the verdict** —
signature tally withheld {FNL 6,FCR 3,CIR 6} ≈ fed {FNL 5,FCR 4,CIR 6}, 0/30 both arms; the one mobile
cell (free_market_fed_d2, ε wobbled to 0.18) moved between two *rejecting* gates (FNL→FCR), never to
ratification. **Consequence for (c):** the manufactured-concordance worry does NOT occur for mountain
claims at the layer the engine decides types → **strengthens document-as-condition, weakens the
decouple rationale** I'd called real-but-bounded (it was an ε-layer artifact the engine catches on
both arms). **Scope bound:** tested the fed claim=mountain only; the rope/snare free boundary is
untested, so "robust to feeding" holds for the mountain case, not all claim types. Caveats:
grid 32/32 absent (OQ-93, verdict rests on ε/χ/signature/Boltzmann not grid); FNL is OQ-70-lineage
but the rejection is independent of it (Boltzmann + false_summit + computed-type are separate gates);
homogeneous probe corpus inflates the borderline/confidence stats (per-constraint verdicts are the
robust part).

**(c) RULING — document-as-condition, seat-framed (operator, 2026-06-13).** The ε-layer spends and
the engine run together resolve (c):

- **Seat-theoretic re-register (operator correction, `docs/seat-theorem-v1.md`).** The engine does
  not adjudicate whether a claim is "really" a mountain — by the Coupling Theorem a contentful
  verdict cannot be seat-free. The multi-perspective × multi-axiom × multi-temporal machinery's
  purpose is to **show the seat** (Q5 liveness = the σ/seat test: maintained-with-a-beneficiary = a
  live parameter = a seat; removal-rearranges-nothing-no-for-whom = mountain). So "0/30 ratified" is
  NOT "30 false mountains caught"; it is: from the engine's seats these structures read non-mountain
  because they carry the liveness flags (maintained extraction, beneficiary, coupling) a mountain
  claim conceals. `false_natural_law` = concealment-detection (Corollary 2a), not truth-rejection.
- **Why feeding can't manufacture concordance (the (c) core worry, answered).** The authored claim
  **label** is not a live parameter of the seat-constituted type — the constituting parameters are
  ε/coupling/beneficiary/timeline. The engine reads the seat off the **structure**, not the label;
  feeding moved ε only between *rejecting* gates (free_market_fed_d2 FNL→FCR), never to a mountain
  reading. 0/30 both arms. **Decouple is WITHDRAWN** — it would do surgery (sever SCOPE→hypothesis→
  claim) to fix a manufactured-concordance that does not occur at the layer the engine decides types;
  the Spend-B "real-but-bounded decouple rationale" was an ε-layer artifact the engine catches on
  both arms.
- **The documented condition (sharper than the ε version).** Manifest-fed stories have the claim
  instructed, but that does not bias the seat-constituted classification — the claim-coupling lives
  at the authored-ε layer and is *shown* downstream (Corollary 2a concealment-detection), not
  ratified. No provenance-bit code change is forced (the worry it would address does not materialize
  at the type layer); the condition is the record itself, here.
- **Scope bound.** Established for the **mountain / false-foundational** case — exactly the case (c)
  named (manufactured concordance = a concealed seat ratified as σ-fixed). The rope/snare ε boundary
  is a separate, lower-stakes question (both non-mountain; the dangerous direction is false
  foundational-ness) → OQ-120.
- **NOT funded (positive-control instinct, same as the matched-fed-arm gap).** The join-structure
  fed-vs-withheld diff is the proper *engine-characterization* test, but it is **not cleanly
  computable on this corpus**: the axiom axis no-opped (`cs_kernel_id` absent), the temporal axis is
  thin (grid 32/32 absent OQ-93; `classify_at_time` OQ-33 fabrication; confidence low) — only the
  observer axis is non-vacuously instrumented. A diff over a join measured on ~1.5/3 axes is
  grid-absent vacuity in a richer outfit. Filed → OQ-119, blocked on a three-axis-instrumented
  corpus (witness all three non-vacuous before running).

**Cross-refs:** OQ-119 (join-diff, engine-characterization, blocked), OQ-120 (rope/snare boundary),
OQ-118 (cast-instability — printing_press's metric-side ε wobble), `docs/seat-theorem-v1.md`,
`audits/2026-06-13_oq117_within_arm_proxy/ENGINE_ADJUDICATION.md`.

---

## OQ-119 — Join-structure fed-vs-withheld diff: does feeding move the cross-examination (not just the type)? — blocked on a three-axis-instrumented corpus

**Ω-type:** Ω_E (engine-characterization; measurable once the substrate instruments all three axes).

**Status:** open — blocked on substrate. Filed 2026-06-13 from the OQ-117 (c) ruling.
**Priority:** 1
**Deps:** blocked_on_human substrate-instrumentation

**The question (engine-characterization, NOT a (c) gate — (c) is resolved).** OQ-117 showed feeding
the mountain claim does not change the engine's *type* verdict (0/30 both arms). A sharper question
remains: does feeding move the **join structure** — which observer seats disagree, which axioms
dissent, how the drift trajectory runs — even when the final type holds? That is whether feeding
shifts the cross-examination the engine performs (perspective × axiom × temporal), per the seat
theorem (`docs/seat-theorem-v1.md`).

**Why it is blocked (positive-control before spend).** On the OQ-117 probe corpus the join is
instrumented on only ~1.5 of its 3 axes: **observer** axis live (four-seat χ spread witnessed,
e.g. free_market institutional χ=−0.029 vs analytical 0.932); **axiom** axis partial — the
committer/CS vantage no-opped (`cs_kernel_id` absent from the probe testsets); **temporal** axis
thin-to-vacuous (grid 32/32 absent OQ-93; `classify_at_time` carries the OQ-33 fabricated-suppression
issue; drift confidence "low"). A fed-vs-withheld join diff over that substrate would measure the one
live axis with two underpowered axes adding noise — the grid-absent vacuity failure (a richer
instrument computed over substrate that cannot support it is not a richer measurement). **Graduation:
a corpus where observer + axiom + temporal are each witnessed non-vacuous** (authored grid, present
`cs_kernel_id`, non-thin timelines) — then the diff is clean. Until then: do not run.

---

## OQ-120 — Rope/snare ε boundary under feeding: the free decision boundary OQ-117 left untested

**Ω-type:** Ω_E (measurable; lower-stakes than the mountain case).

**Status:** open — filed 2026-06-13 from the OQ-117 (c) ruling (scope bound).
**Priority:** 1

OQ-117's engine run tested the fed claim = **mountain** only and ruled (c) document-as-condition for
the false-foundational case. OQ-78's actual free ε decision boundary is the **rope/snare** split
(0.45/0.46), the one ε gate no SCOPE bin edge touches. Feeding a rope or snare claim and reading
whether the engine's seat-constituted type moves across that boundary is the remaining test — but it
is **lower-stakes**: both rope and snare are non-mountain, and the dangerous concealment (a seat
asserted as σ-fixed) is the mountain/false-foundational shape OQ-117 already covered. Pick up if a
rope/snare concordance question arises; not gating anything. Direction-of-fix from OQ-78 binds (no
numeric disclosure to authoring).

---

## OQ-118 — Draw-stability tracks field-construction-type, not the σ/seat line: the cohort's analysis contract (successor to OQ-109's σ/seat residual)

**Ω-type:** Ω_E (the re-tests are measurable) + Ω_P (where the seat boundary sits is a theory ruling).

**Status:** open — filed 2026-06-13, successor to OQ-109's σ/seat residual (discharged here, not
**Priority:** 1
answered). The frozen σ/seat prediction (`audits/2026-06-12_cohort_zero/SIGMA_SEAT_PREDICTION.md`,
committed `5f2a626c`) was tested on the replicate spend and **falsified-as-tested**; this entry
carries the structured finding and its open graduation conditions.

**The replicate spend (settled substrate; `audits/2026-06-12_cohort_zero/`, commit `dcfaea97`):**
15 draws = 5 contested kernels (`qwerty_path_naturalization`, `free_market_naturalization`,
`total_war_unthinkability`, `printing_press_reformation`, `zero_as_number`) × 3, batch
`msgbatch_01UbfPq13BcHgJKxcsqK549i`, `claude-sonnet-4-5-20250929` @ temp 0.2, seeded from
`prolog/kernel_seeds.json` through the FROZEN seed-spec (title+domain+summary) so the prediction
applies. Instruments: `python/cohort_stability.py` + `python/cohort_sigma_seat_eval.py` (Fisher
exact validated vs scipy to 6 sig figs).

**Settled now (robust, recorded):**
- **Apparatus-presence mis-bucketing (firmest — no naming confound):** `boltzmann`/`network`/
  `interval` presence are draw-STABLE 6/6, but the frozen prediction bucketed them **seat**. They
  are prompt/schema-determined — a fourth bucket the prediction folded into seat. This is a
  classification error IN THE PREDICTION, witnessed (presence is binary; nothing to conflate).
- **Null partition (scoped):** the σ/seat split does NOT beat chance on exact-match over these
  schema fields — predicted-σ 58 stable / 62 unstable, predicted-seat 36 / 32, 47.87% consistent,
  **Fisher two-sided p=0.649**. The noise hypothesis the prediction pre-registered as its own
  falsifier was NOT rejected.

**The actual subject (meta-finding):** draw-stability is an **artifact of field-construction-type**
— free-authored "cast" fields (stakeholder multisets, beneficiaries/victims, vindicated_propositions)
are draw-unstable; schema-mandated/computed fields (apparatus presence, omega count, verdicts) are
draw-stable — **not** the theorem's σ/seat line. This is a property the meta-analysis corpus needs
characterized BEFORE any cross-story claim trusts a field's stability (it gates the analysis
contract: which fields are comparable across n=1-per-story draws). NOT a vindication of the noise
model over the seat model — the two confounded halves below cannot adjudicate between them.

**Two confounded halves — the OQ's opening work (graduation conditions):**
1. **Cast/σ re-test with GRADED similarity (no new instrument):** exact set-match scores "invented a
   fresh cast" identically to "renamed the same cast" — the naming-drift mechanism the
   signature-identity witness documented (`audits/2026-06-12_signature_identity_witness/`). Re-test
   the cast fields with the within-vs-between DISTANCE metric (already built + witnessed this
   session, `cohort_stability.pair_distance`) applied at field level, splitting positive-agreement
   from agreement-in-absence. This is a NEW pre-registered test, NOT a redraw of `5f2a626c`.
2. **Verdict-stability disentangled from temperature:** `disappearance_verdict` (4 stable/2),
   `founding_problem_status` (3/3) look more draw-stable than "seat-expressive" predicts, but at
   n=6 and temp 0.2 this is suggestive-confounded, not a finding — temp 0.2 + a strong situation
   summary may over-determine the verdict (a generation-regime cause, not σ). Graduation: a temp
   sweep, or an explicit accept-as-confounded ruling.

**Discipline carried forward:** the escalate-don't-redraw clause is inherited — a graded re-test is
a new pre-registered test with its own frozen prediction, never a retrofit of the prediction that
just failed. Mismatch = finding about where the seat boundary sits, escalated, never an inline
redraw.

**Related cohort-one item (carried from OQ-109):** the `reading_diff` re-point is COHORT-ONE-gated
— `constraint_stakeholder/7` is Unknown procedure on the live corpus, so it has no live positive
control; pick up when a stakeholder-cell-bearing story lands (inert-proving-inert otherwise).

**Cross-refs:** OQ-109 (parent; σ/seat residual discharged here), OQ-26 (ε generated-not-invariant
— the determinism-frontier context), OQ-75 (Stage-2 cross-story claims that this analysis contract
gates), the signature-identity witness (KIND-not-story identity; the graded-metric template).

**Origin:** 2026-06-13, OQ-109 Phase C σ/seat falsifier result + operator ruling (split, with
verdict-stability demoted to confounded-half; discharge-to-successor).

---

## OQ-121 — Totalization convention for the commentary family + domain-relative census coverage (the `extraction_silent` collapse)

**Ω-type:** Ω_E (the fix is mostly MECHANICAL — the domain gate `extractive_type(dr_type)` is already computed; totalizing the predicate just stops throwing it away on non-fire) + a small Ω_C residue (two declared seats: report prevalence alongside coverage; classify the unnameable-blindspot case as covered).

**Status:** resolved — 2026-06-16. The census-honesty core is built: `extraction_reading` totalized to the never-fail family shape, census coverage made domain-relative, prevalence separated from coverage. Witnesses below. Residual (remaining family members) folded into the extension point, not a standing defect.

**Priority:** 4

**Deps:** bundled_with OQ-134 (the generic census this hardens); bundled_with OQ-86 (the `extraction_reading` source totalized here).

**Origin:** 2026-06-16, building OQ-134 + a closer look the operator asked for. The census shipped `extraction_reading` coverage as `null` because its silence was unruled — but the closer look found a bigger, structural issue than a missing ruling.

**The bigger issue (two layers).**
1. **A totalization debt in the commentary family.** The engine already encodes the discipline "no verdict ⇒ an EXPLICIT token, never a silent failure" where it matters most: `signature_detection.pl:136` makes correction-grade `constraint_signature/2` never fail (explicit `unknown` fallback, "instead of a default-fabricated verdict"); `q6_crosscheck/3` carries explicit absence buckets. But the rest of the R3 family never got it — `extraction_reading/2` **failed silently**, and `consensus_provenance/2` is silent on `Ns=[]`. A silently-failing predicate destroys the provenance bit AT THE SOURCE, so no downstream aggregate can reconstruct it (Build Discipline Pattern 6 in its purest form: the absence is total). The fix is mostly mechanical — guard A (`extractive_type(dr_type)`) already computes the domain; the predicate just discarded it on failure.
2. **Census coverage was corpus-relative, but domains aren't.** `coverage = (n_corpus − Σ absence) / n_corpus` is only correct when a reading's domain == the corpus (true for q6, false for extraction, whose domain is the 50/72 extractive-typed constraints). With no absence buckets that formula gives 1.0 over ALL 72 — falsely claiming coverage of the 22 out-of-domain constraints. And coverage ≠ prevalence ≠ corpus-fraction — one silent bucket collapsed all three.

**What was built.**
- `stakeholder_seats:extraction_state/2` — TOTAL (mirrors `q6_cell/2`): every constraint reaches exactly one of `out_of_domain` / `extraction_clear` / `extraction_unnameable` / `extraction_fired(Es)`. `extraction_reading/2` now rides on `extraction_fired` so its fire-or-silent report contract is UNCHANGED (oq86 14/14 still green).
- `extraction_unnameable` (extractive ∧ no victim ∧ no nameable extractor) is its OWN bucket — the starkest blindspot, previously swallowed whole by the silent failure. **5 such constraints surfaced on the live corpus** that were entirely dark before. Counts as MEASURED/covered (the question was answered) — a declared operator seat, revisable.
- `commentary_census.pl` gains three bucket KINDS (out-of-domain / absence / measured) and computes `coverage = (n_in_domain − Σ absence)/n_in_domain` (domain-relative) + `prevalence = fired/n_in_domain` as a DISTINCT number. q6 unchanged (universal domain → 0.611); extraction now `coverage 1.0` over 50 in-domain, `prevalence 0.06`.

**The general form (the convention, now the gate).** Any new `commentary_cell/3` source must be TOTAL and declare its out-of-domain / absence / prevalence buckets; the census refuses a coverage RATIO unless `commentary_coverage_decidable/1` flags the bucket sets ruled-complete. **Follow-up (2026-06-16): the two partial-silent family members `consensus_provenance/2` and `seat_perceived_vs_real/4` were totalized** to the same never-fail shape — `consensus_provenance` now always returns an explicit verdict (`no_agent_seats` out-of-domain / `seats_untyped` absence added; 21 live constraints that silently failed now report `no_agent_seats`), and `seat_perceived_vs_real` returns `Computed = untyped` instead of failing on an existing-but-underivable seat (total over 370 live seats; the `untyped` branch is a defensive guard with 0 live triggers). Regression: `prolog/tests/test_seat_totality.pl` (8/8). `mandatrophy_gap` is the last unconverted member — convert if/when censused.

**RESOLUTION witnesses (2026-06-16).** `audits/2026-06-16_oq121_totalization/`. plunit `test_commentary_census.pl` **40/40** (per-state positive controls for all four `extraction_state` outcomes; contract-preserved silence on clear/unnameable/ood; domain-split `n_in_domain = n_corpus − Σood`; coverage decidable for both sources). oq86 contract **14/14** (extraction_reading firing unchanged). Pipeline `commentary_census` + `json_report` ok; classification untouched (extraction_state/extraction_reading are not on the dr_type path — structural witness). Census: extraction `{clear 42, out_of_domain 22, unnameable 5, fired 3}`, coverage 1.0/50, prevalence 0.06.

**Open seats (declared, revisable).** (a) report both coverage and prevalence — taken; (b) `extraction_unnameable` = covered — taken. Override either by re-declaring the bucket (e.g. make `extraction_unnameable` an absence bucket) and re-running. Cross-ref: OQ-134, OQ-86, `prolog/stakeholder_seats.pl` (`extraction_state/2`), `prolog/commentary_census.pl`.

---

## OQ-122 — Control inversion: `type_1_false_summit` RED-caps on the type-CLAIM alone, independent of extraction magnitude — does RED track concealment or tax every authored hypothesis?

**Ω-type:** Ω_E (the discriminating test is a measurable single-field intervention) + Ω_C (whether
claim-only RED-capping is the intended semantics is a design ruling).

**Status:** investigating — filed 2026-06-13 from the Przybylski's-star stress run
**Priority:** 1
**Deps:** bundled_with OQ-128
(`agent/analysis/originals/przybylskis_star.md`; testsets `actinide_replenishment_mechanism_*`,
`neutron_star_bombardment_reading`, `radiative_levitation_stratification`, `superheavy_decay_reading`,
`performance_legitimacy_flat_control`; reports in `outputs/constraint_reports/`). Surfaced by a
web-Claude read of the five reports; structural claims independently witnessed against code/reports
below. **Empirical half settled 2026-06-13** by the discriminating probe
(`audits/2026-06-13_oq122_retype_discriminator/`, FINDINGS.md + probe.pl + probe_output.txt);
**the remaining open item is the Ω_C design ruling** (concealment-semantics vs over-broad
author-tax), the operator's call — see "Witnessed result" below.

**Witnessed result (2026-06-13 probe — supersedes the pre-registered control's framing):**
The re-type control proposed below (and by web-Claude as "the sharpest single next step") is
**confirmatory, not discriminating** — `type_1_false_summit` is gated on `constraint_claim(C, mountain)`
by construction (drl_core.pl:614), so re-typing mountain→tangled_rope removes the cap *a priori* and
the outcome is consistent with BOTH readings (Intervention B: `dr_type` and the `false_summit_mountain`
signature both UNCHANGED, only the claim-precondition vanished). The **actual** discriminator is the
beneficiary toggle (hold `claim=mountain`, retract `constraint_beneficiary`):
1. **RED is beneficiary-driven, NOT extraction-driven.** `false_summit_mountain` REQUIRES
   `ε ≤ mountain_extractiveness_max` (0.25); measured ε = **0.03** — high extraction would *fail* the
   gate. Near-zero extraction is a precondition; named **agent** beneficiaries (institutions, via the
   agency gate) are what trip it. Confirms the artifact reading at metric level.
2. **The RED is OVERDETERMINED** — removing beneficiaries does NOT clear it: even beneficiary-free,
   moderate & institutional seats classify `rope` → 2 `type_1` firings persist (the claim-independent
   power-scaling residue, χ=ε·f(d)·σ(S), OQ-50 comment drl_core.pl:605–612). **Only not-claiming-mountain
   clears the cap; neither the physics nor the beneficiaries alone does.** So web-Claude's "it's the
   type-claim" instinct is *more* right than "it's the beneficiaries," for a second reason neither read
   isolated.

**Core diagnosis RULED false-positive (2026-06-13)** by the cell sweep + fix-witness run
(`audits/2026-06-13_oq122_retype_discriminator/`: cell_sweep + fix_witness; FINDINGS.md RULING+REVISION).
FSM's RED-capping footprint over the whole corpus is exactly **2** (`radiative_levitation`,
`actinide_flat_control`), both pristine astrophysics with **zero `constraint_victim`** — benefit is
external (studying). A concealment detector firing where no agent victim can exist is a false positive
by definition (no victim ⇒ no extraction ⇒ nothing to conceal). The RED does NOT accurately report
"shaped by coordination, not physics" for these.

**Two over-claims from the first pass were corrected, then the FIX was settled by signature-ID +
a breadth sweep (do not cite superseded versions; full trail in FINDINGS.md REVISION + REVISION 2):**
- **"Discriminating power is INVERTED" — RETRACTED.** The 3 social cell-members are already RED-capped
  (`coupling_invariant_rope` + base `type_1`), NOT by FSM; FSM abstaining on them (supp>0.05) loses no
  signal. FSM is correctly scoped as a pristine-false-summit detector.
- **neutron_star is NOT a third FSM/victim false-positive — RETRACTED.** Signature = `false_ci_rope`
  (vic=0, ε=0.12). The FCR path also carries `superheavy` (FCR, vic=0, ε=0.68 — the genuine case), so
  the no-victim principle does NOT reach FCR (it would falsely condemn superheavy). The victim-
  discriminator is **FSM-local** (witnessed via the superheavy positive control). neutron_star's RED
  is OPEN under the **FCR/OQ-70 bait-confound** thread, not the FSM author-tax. (Label was applied
  before the signature was known — corrected.)

**SWAP (victim-gate) DECISIVELY beats remove — breadth-witnessed (`breadth_sweep_results.txt`).**
The turn-1/2 "swap ≡ remove, lean remove" was a measured-empty-vs-didn't-look artifact: the 57-story
`testsets/` happens to contain **zero** `vic>0` FSM-firings, so the gates looked equivalent. Two reads
overturn it: (1) authoring-ontology check — the linter/validation machinery couples other fields
(SCAFFOLD_DANGER_ZONE, tangled_rope-requires-victim, CS-ε) but **nothing couples `suppression` to
`constraint_victim`** ⇒ `supp≤0.05 ∧ victim≠∅` is **authorable** (witnessed absence, positive control
present); (2) breadth sweep over 5 corpora (overlay-witnessed) — the clean twin **`testsets_flash` has
22 `vic>0` FSM-firings** (naturalization-concealment shapes: `market_as_natural_default`,
`war_winnability_post1945__deterrence_unthinkable`, `statutory_debt_ceiling__constitutional_nullity`,
the `technological_determinism` readings), each a live case where the victim-gate RETAINS and removal
FORFEITS; the adversarial cell is non-empty in 4/5 corpora (Triffin-inevitability the cleanest). **The
load-bearing partition is EXACT (the discriminator's value = the cardinality):** `testsets_flash` =
40 FSM-firings = **18 `vic=0` + 22 `vic>0`** (all 22 enumerated in `breadth_sweep_results.txt`). The
cross-corpus aggregate (~55 `vic=0` / ~26 `vic>0`) is an exact sum of per-corpus integers carrying a
tilde for **cross-corpus identity only** (ID reuse in chimera archives + same-kernel readings across
twins ⇒ firings ≠ distinct stories), NOT un-run computation — corroboration, not the witness. Archives
(kernel_v1/original_v6) are chimera-confounded corroboration; the ruling rests on the clean twin's exact
22. (No over-claim: `vic>0` = "no-victim exemption does not apply / concealment possible," not per-case
"confirmed concealment.")

**Open graduation steps (all output-changing):** (a) **FSM victim-gate — IMPLEMENTED on branch
`oq122-fsm-victim-gate` (`ab1e9b26`), NOT merged.** Gate diff on `testsets_flash` is a clean partition
(FSM-firing 40→22: 18 `vic=0` exempted, 22 `vic>0` retained, 0 leakage, 22 untouched) ⇒ `constraint_victim`
is the sufficient FSM filter, `agent_victim/2` is a later refactor not a blocker. **Merge is BLOCKED on two
findings (FINDINGS.md GATE IMPLEMENTATION):** (i) breaks **36 unit-test fixtures** (`test_agent_beneficiary`
31, `test_contradiction_signatures` 5) that encode the **pre-victim FSM contract** — 0 `constraint_victim`
facts in either; they author the very mountain+beneficiary-no-victim author-tax the ruling repudiates.
Migration is per-fixture intent judgment (add-victim vs flip-expectation) = **operator's call**;
`agent_victim/2` does NOT help (no victims of any kind in the fixtures). (ii) **near-zero standalone verdict
impact on flash** — 16/18 exempted stay `t1=4` (RED) via `type_1`-over-`coupling_invariant_rope`
overdetermination; the FSM author-tag was seldom the SOLE cap. The CIR residual is a SEPARATE, plausibly
legitimate phenomenon (vic=0 contested-concept-as-mountain → rope; the no-victim principle does NOT extend
to the cap level — FCR/superheavy control). Gate's verdict benefit is concentrated on the physics cases
whose residual is OQ-50, not CIR. **Operator to rule: migrate fixtures + merge, or hold.** (b) **OQ-50
power-scaling residue** (moderate/institutional mountain-claimers → `rope`; still needed for
radiative/actinide to reach GREEN — and it, not the FSM gate, is the binding verdict-mover for the physics
cases); (c) **neutron_star / FCR** — adjudicate the low-ε FCR firing under **OQ-70** (bait-confound),
separate from this OQ.

**The witnessed inversion (two calibration controls land backwards):**
- `actinide_replenishment_mechanism_flat_control` — purity **0.996** (pristine), independent,
  Boltzmann-compliant, the *clean* baseline — comes back **RED**.
- `performance_legitimacy_flat_control` — purity **0.354** (contaminated), coupling 1.0,
  Boltzmann non-compliant, the *dirty* baseline — comes back **YELLOW**.
  (Caveat: the two controls ran in **different frames** — actinide family on the 57-constraint
  corpus, performance_legitimacy on a **13**-constraint corpus; report headers `CORPUS CONTEXT:`.
  Cross-control comparison inherits this. The ledger is scoped to the 57-run, so its omission of
  performance_legitimacy is correct, not a gap.)

**The mechanism (verified at code level, drl_core.pl:614):**
`dr_claim_mismatch(C, Context, type_1_false_summit, severe) :- narrative_ontology:constraint_claim(C, mountain), standard_context(Context), dr_type(C,Context,Actual), Actual \= mountain.`
The severe alert caps BASE: YELLOW → RED. It fires **only** on a constraint that *claims* mountain
yet whose authoritative `dr_type` departs from mountain. Three of the four reds in the 57-run
(`actinide_…_control`, `radiative_levitation`, `neutron_star`) are `BASE: YELLOW … CAPPED TO RED`
on this one alert (report verdict boxes). The dirty control claims `tangled_rope`
(`constraint_claim(performance_legitimacy_flat_control, tangled_rope)`), so the rule is
**structurally unable to fire on it** and nothing caps it → YELLOW. So RED here is tracking
*naturalization-concealment* (claiming law-of-nature status while having named beneficiaries), **not
extraction magnitude.** Only `superheavy_decay_reading` is **base-RED on its own metrics** — it
claims `rope`, not mountain, so the false-summit rule cannot fire; its RED is uncapped (verdict box
has no `CAPPED TO RED` line). "All four red" is one artifact firing three times + one genuine signal.

**The open question (two readings; commit via the pre-registered control, do not hold open):**
- *Charitable (intended):* concealment is more dangerous than honest-but-dirty coordination, so a
  low-extraction thing masquerading as physics SHOULD outrank a high-extraction thing that admits
  what it is. RED-capping on claim is the design working.
- *Uncharitable (artifact):* "has named beneficiaries" is a condition **every real scientific
  hypothesis** satisfies (it has researchers/funders), so the rule red-flags physics for *existing*.

**Pre-registered discriminating control (cheapest falsifier in the set — run before trusting any
red/yellow ranking):** hand-edit `radiative_levitation_stratification.pl` `constraint_claim` from
`mountain` → `rope` (or `tangled_rope`), **holding every metric fixed**, and re-run the pipeline.
This is a **deterministic single-field intervention on the committed `.pl`, NOT a stochastic
regeneration** — it is legitimate under the determinism-frontier ruling (committed JSON/PL onward is
deterministic) and a future agent must not dismiss it as a re-draw. **Pre-registered outcomes:**
verdict drops off RED ⇒ the RED was a function of the type-CLAIM, not the physics or the extraction
(artifact reading gains support, and claim-only capping is over-broad); verdict stays RED ⇒ the cap
is metric-driven and the claim is incidental (charitable reading). The base-YELLOW metrics should
not move; only the cap should.

**Relation to existing OQs (this is not a duplicate):** OQ-50 repaired the false-summit *detector*
(it now negates `dr_type`, not the vacuous `is_mountain(_,_,fail)`) and OQ-50 OPEN-1 asks the
design-intent question "what false summit explains *against*" — neither raises the empirical
control-inversion or the claim-vs-extraction discrimination. OQ-43/OQ-44 are the **inverse** failure
(a natural-law gate passing because the beneficiary table is *empty* — authored-zero vs absent);
here the gate *fires* on a populated beneficiary list. OQ-70 (FNL bait-confound) is adjacent but is
about `claimed_natural/2` source-2 reading any mountain perspective as a naturality claim, not about
the YELLOW→RED severe cap.

**Secondary observation (operator to rule; not yet its own OQ):** the cross-constraint *convergence*
block reports `neutron_star` and `superheavy` as sharing the `false_ci_rope` signature
("coordination-washed"), but flattens a real magnitude gap — `neutron_star` is `confidence: low`
with a barely-over-floor Boltzmann failure (`excess_above_floor` ≈ 0.10) while `superheavy` carries
coupling 0.75. The convergence aggregate (enhanced_report.py) does not carry confidence/magnitude to
the read site, so a 0.10-vs-0.75 gap renders as an equivalence. Report-presentation, not engine
classification — fold into a report-layer refinement OQ if the operator wants it tracked.

**NOT in scope here (already tracked):** the vacuous temporal/structural-gradient layer ("grid diet:
authored 0/32", INTENT `OPEN(no_gradient_data)`) and the "12/12 subsystems" base count that includes
OPEN subsystems — that is OQ-93 (grid vacuity); the reports are scrupulous (mark OPEN, not passed).

**Provenance caveat (cannot be settled from reports alone):** whether the engine *found* the
recognizable sociological shape (mainstream radiative-levitation penalized only for being dressed as
settled physics; exotic superheavy-decay carrying genuine foreclosure-plus-rising-investment) or the
stories were *authored* to have it is not separable at report level — the pre-registered control
above is the first thing that bears on it.

**RULING (2026-06-14, operator-aligned) — author-tax is real but narrow; FSM victim-gate HELD,
bundled with OQ-128, NOT standalone. OQ-122 stays OPEN.** The author-tax repudiation is scoped to
signature-layer honesty only (a no-victim mountain-claim has nothing to conceal). The FSM victim-gate
stays IMPLEMENTED-NOT-MERGED on `oq122-fsm-victim-gate` (`ab1e9b26`); land it **bundled with whatever
resolves the physics-RED Ω_C — now tracked as OQ-128 — not standalone.** The residual is **OQ-128**
(mid-power-mountain→rope power-scaling, `drl_core.pl:605-613` — the binding verdict-mover for the
physics false-REDs; do NOT attribute this to "the OQ-50 power-scaling fix," which names a settled
artifact that does not exist — the direction is an open Ω_C); neutron_star/FCR stays under **OQ-70**.

**Fixture-blocker RE-MEASURED 2026-06-14 — blocker (i) is STALE on the live corpus
(`audits/2026-06-14_oq122_fixture_triage/`).** The "breaks 36 unit-test fixtures (test_agent_beneficiary
31, test_contradiction_signatures 5)" was measured 2026-06-13, before corpus drift. On HEAD `da0e88e2`
the gate's diff over both suites introduces **zero new failures** (test_agent_beneficiary baseline 20
unique ≡ gate 20, delta ∅; test_contradiction_signatures baseline 5 ≡ gate 5, delta ∅ — id-sets in the
audit dir). The 20+5 failures are **pre-existing corpus drift**: 0 of 11 `fsm_agent_mountains` and
`maxwell_demon_impossibility` survive the 2026-06-05 reset, so the agent-beneficiary fixtures fail
regardless of the gate. The gate's ACTUAL live effect is a clean **2→0** on the two vic=0 physics
false-positives (`actinide_…_flat_control`, `radiative_levitation`) with no regressions. **Consequence:**
the fixture-cost half of the hold rationale no longer applies to the live corpus; the hold now rests
solely on the OQ-128 physics-RED leg (FSM-exemption alone may not move those 2 verdicts to GREEN, their
RED being overdetermined by power-scaling). A 36-row add-victim/flip-expectation triage is moot until the
fixtures are rebuilt against the post-reset corpus (separate fixture-health task). The operator may
re-rule the hold given the corrected (zero) cost — recorded as available, not actioned (engine merge =
human call). Cross-refs: **OQ-128** (residual, minted this session), OQ-50 (false-summit detector this
rides — OPEN-1/OPEN-2 closed 2026-06-14), OQ-70 (neutron_star/FCR), OQ-117 (false-foundational gates).

**Origin:** 2026-06-13, Przybylski's-star engine stress test + web-Claude report read; structural
claims re-witnessed against drl_core.pl:614, the five testsets' `constraint_claim/2`, and the report
verdict boxes (this session).

---

## OQ-123 — Powerless-seat χ-typing is the most model-sensitive surface in the twin comparison — is it noise, a real seat-localized model divergence, or a generation artifact?

**Ω-type:** Ω_E (a measurable per-seat agreement-rate gap, testable on any twin pair).

**Status:** open — filed 2026-06-13 from the twin cross-model comparison
**Priority:** 1
(`audits/2026-06-13_twin_comparison/`, n=960, haiku vs gemini-flash at commit 8126231). H1
(structural type model-invariance) HELD per-field for all 7 fields, but with a steep **seat
gradient**: `persp:powerless` agreement-rate 0.397 (the two models disagree on the powerless
reading more often than they agree; just above its 0.308 chance band), vs `persp:institutional`
0.672 and `verdict` 0.749. The powerless seat is where model choice moves classification most.
Open question: is this (a) the seat with the least authored signal so it floats, (b) a real
divergence in how the two models render powerless-perspective extraction/coercion, or (c) a
generation-template artifact? Discriminating handle: condition the powerless disparity on
authored-vs-imputed powerless metrics; compare against a third model when available. Under-claim:
one twin pair earns "model-sensitive here," not "the powerless seat is model-dependent in general."

**Origin:** 2026-06-13 twin-comparison audit (FINDINGS.md "seat gradient"); RESULTS.md H1 table.

---

## OQ-124 — Systematic signature lean `constructed_high_extraction` (haiku) ↔ `false_ci_rope` (gemini-flash): two models coding the same substrate's structure differently

**Ω-type:** Ω_E (a measurable directional disparity in the `signature` field across a twin pair).

**Status:** open — filed 2026-06-13 from the twin comparison
**Priority:** 1
(`audits/2026-06-13_twin_comparison/`). The recurring `signature` disparity over matched ids is
`constructed_high_extraction` (haiku side) ↔ `false_ci_rope` (flash side), in BOTH directions
across many constraints — the two models systematically foreground different structural codings of
the same authored substrate. Per OQ-70 this is STRUCTURAL-coding disagreement, NOT a detection
claim (signature prevalence is bait-confounded). Open question: which authored fields drive the
fork (extractiveness magnitude? the CI-rope gate inputs?), and is the lean a stable model
fingerprint (re-test on a third model / a fresh twin draw). Cross-ref OQ-70.

**Origin:** 2026-06-13 twin-comparison audit (FINDINGS.md "model-characteristic signature lean";
RESULTS.md `signature` disparity exemplars).

---

## OQ-125 — H2 continuous-invariance tail: is the below-band |Δχ| real value-invariance, or threshold-colocation entailed by H1? (pre-registered colocation test)

**Ω-type:** Ω_E (a pre-registered conditional-permutation test with a named decision rule).

**Status:** open — filed 2026-06-13 from the twin comparison. The pre-registered H2 DRIFT test
**Priority:** 1
(observed mean|Δχ| > permute band95) FAILED for all 5 continuous fields (H2-drift FALSIFIED). The
opposite tail fired instead — observed mean|Δχ| BELOW band5 for all 5 (true pairs more similar than
chance re-pairing) — but that tail was pre-registered only to be REPORTED, carries no committed
falsifier, and is **exploratory**. The load-bearing concern: the below-band tail may be **entailed
by H1**, not separate evidence about models. The binding H1 constraint forces the haiku/flash χ pair
onto the **same side of the decision threshold**, not to the **same value**; below-band similarity is
a stronger value-proximity claim. **Pre-registered discriminating test:** condition on
same-side-of-threshold pairs and ask whether the below-band |Δχ| SURVIVES within that conditioned
set — survives ⇒ real continuous invariance beyond H1; relaxes to chance ⇒ the tail was
threshold-colocation (H1-entailed). REJECTED alternative: conditioning on H1-disagreeing ids
(underpowered, not independent, if H1 holds strongly). Until this runs on a fresh corpus, the
invariance tail must not inherit H1's credibility.

**Precondition — `abductive_data.json` provenance (settled 2026-06-14):** it is
**committed-derivable**, not a carried-with-no-generator artifact — generator is
`abductive_report.pl` (`run_abductive_report`), run as the `_prolog_abductive` pipeline step
(`run_pipeline.py:469`), computed from the corpus. CAVEAT for "fresh corpus": `classify_corpus`
runs only `_json_report`, which READS `abductive_data.json` on-disk and does NOT regenerate it
for the target corpus — so a classify-only reproduce carries whatever corpus last ran the full
pipeline. **This does NOT affect OQ-125:** the file's `abd_triggers/2` are read only by
`diagnostic_summary.pl:191` (→ `verdict`) and `post_synthesis.pl:37` (→ flags); χ
(`perspective_chi`), signature, perspectives, and claimed_type are computed independently of it
(witnessed: the `drl_core`/`signature_detection` "abductive" hits are comments + a *different*
static helper `abductive_helpers:known_override_signature`, not `abd_triggers`). Since OQ-125
conditions on χ thresholds, the carried file is irrelevant to this test. (It can perturb the
`verdict` field, but both twins read the SAME on-disk copy, so that comparison stays symmetric.)
If a future fresh-corpus run wants `verdict` to be target-matched too, regenerate
`abductive_data.json` against the target corpus first (full pipeline with `corpus_path` overlaid).

**Origin:** 2026-06-13 twin-comparison audit (FINDINGS.md "[EDGE]" H2 disposition); operator review
sharpening (colocation decomposition over residualization; abductive-provenance precondition).

---

## OQ-126 — Temporal accountability (commitment-systems T0→T1→T2→T3 / seat-theorem Cor 3): how much of the theory the engine can carry is UNKNOWN

**Ω-type:** Ω_C (design — what an accountability surface should be) + Ω_E (the engine's current
coverage is witnessable, and was witnessed below) + an **Ω_P core that does not reduce** (the
honor/reabsorb verdict is seated, never engine-certifiable — Cor 2a/Cor 3). The entry's own claim
is deliberately under-scoped: theory mapped, partial coverage witnessed, **full coverage not yet
known and not settled here.**

**Status:** open — filed 2026-06-13 (CC session; doctrine thread with web-Claude, two essays as
**Priority:** 1
worked instances). Theory↔engine map below is witnessed. **CORRECTED same day — see below;
"Gap 2" is retracted.**

**CORRECTION (2026-06-13, same day — operator confrontation honored, not silently rewritten).**
The original entry located the Cor-3 *bite* inside the engine and framed its absence as a
deficiency ("Gap 2 — no external-confrontation path; OQ-107 must build it"). **That is a category
error**, corrected against `docs/design/design_discipline.md` §9: **the engine sits in the context
of DISCOVERY, not justification** — "nothing in discovery has to be calibrated, because nothing in
discovery is load-bearing; it only has to point somewhere worth digging." Miscalibration is nearly
free *because of where the engine sits in the pipeline*; the engine is the corpus's immune system,
not its court (§4: essay → constraint hypothesis → formalization → engine resistance → sharper next
essay). The Cor-3 bite — a real later seat confronting a pre-committed t0 — does **not** belong in
the engine and is **not missing** from it. **It lives in publication:** every essay goes to a
public, dated, commentable record (**cafebedouin.org**, operator-confirmed 2026-06-13) that readers
refer back to years later. The later seat is the **public** — commenters, future readers, the world
itself (columbia's *actual election results* confronting the dated matrix; the bloc-leverage clock
running to 2028/2032). So **"Gap 2" is retracted as a gap**: engine = discovery (self-consistency,
appropriately, cheaply); accountability = the **published artifact**, confronted by external
temporal seats the engine neither contains nor should. **OQ-107 is a discovery-side *input*
enhancement** (ingest external data so the engine points better), **not** the missing bite — re-scope
it there; do not gate Cor-3 accountability on it. The gate that makes this hold is §9's: **no verdict
skips adjudication** — the engine is a hypothesis generator; the author, then the public, are the
adjudication stages, and "the engine works" is a property of the pipeline *shape*, not the engine
alone. (**Gap 1 survives, demoted:** acknowledgment-bit-as-verdict is a discovery-layer cleanliness
item — scope it witness-not-verdict — not an accountability hole, since the engine was never the
accountability layer.) This correction is itself the thread's worked instance: the operator is the
later seat that confronted this entry; correcting it legibly rather than rewriting it silently is
§8's t3 honor-vs-reabsorb distinction applied to my own artifact — and cafebedouin.org is where that
same distinction bites for the essays.

**The theory (already present in the substrate, not invented here).** `commitment_systems_sketch_v5_2.md`
§3 (the acknowledged-drift event) is a temporal-accountability structure:
- **T0** — a kernel/commitment is issued from a seat (the prediction, the codification, the staked
  claim).
- **T1** — an authority interprets/operates it.
- **T2** — drift: what actually happened, the kernel and its practice diverge over time.
- **T3** — acknowledgment: does the seat **update** (honor the confrontation) or not (concealment /
  reabsorption-as-retreat).

This is seat-theorem **Corollary 3** (seats are non-interchangeable across time; the confronting
seat is a *different*, later seat the t0-seat "could not author"; the discipline "secures that the
price exists and is visible; it does not secure that anyone pays it"). It is also OQ-55's
**B-historical** channel (= A-transition + Cor-3 accountability), distinct from B-structural
(atemporal axiom-clash, ∥ A/C). The two essays are worked instances at opposite **external-anchoring**
tiers: `essays/2026-05_or_before/columbia_falsification_matrix.md` (dated/numeric thresholds
confronted by third-party market data — near-seat-invariant; honored RED at lines 81/89, and when a
confrontation was ambiguous it staked a *further* dated test, lines 97–104 = the discipline working);
`essays/2026-06/the_vote_market/the_vote_market_draft3.md` (a retained draft-vs-draft disposition log,
lines 143–164 — record-membership is the author's seat to draw, hence Π-exposed: line 162 files a
failed load-bearing test, "Branham rebuttal unfulfillable as posed," as "Accepted but modified").

**Engine coverage — WITNESSED this session (grep, file:line pasted):**
- **T2 is IN.** `cs_drift_trajectory/3` "computes t2 regardless of acknowledgment status"
  (`cs_pattern_detection.pl:404–405`).
- **T3 is IN, as routing.** `cs_drift_unacknowledged/2` reads the **authored** `Acknowledged` flag in
  `narrative_ontology:cs_drift_state(UID,_,gap(Type,Mag,Ack))` (`cs_pattern_detection.pl:412`);
  `cs_terminal_attractor/4` routes **acknowledged → revival/stable_pattern/framework-revision** vs
  **unacknowledged → husk/extinction** (`cs_drift_engine.pl:70–84`). The honor/forget split is
  already wired to outcomes.

So the engine carries the full T0→T3 **vocabulary**. Two gaps remain, both familiar shapes:

**Gap 1 — the acknowledgment bit is treated as the verdict (the cyclopean shape; same as
OQ-49/50/116/122).** The `Acknowledged` flag is **authored**, and the engine consumes it as if it
SETTLED honor-vs-reabsorb (terminal attractor). But honor/reabsorb is **seated and floats**:
vote_market 162's "Accepted but modified" = the author's own call that the drift was acknowledged;
a hostile reader reads the same edit as reabsorption-as-retreat. Reading an authored "acknowledged:
true" as settling the seat-update is the same seated-verdict-scoped-as-engine-fact error the
cyclopean cluster names. **Witness-not-verdict fix (UNKNOWN if buildable):** carry acknowledgment as
a *witness* (drift authored? ack-bit set? by whom? confrontable against what?) and leave the
honor/reabsorb verdict seated.

**Gap 2 — RETRACTED by the CORRECTION above (retained per supersede-don't-delete; the engine is
the discovery layer, so the absence of external confrontation is the correct division of labor, not
a gap — the bite is in publication).** ~~no external confrontation, so it is self-consistency, not accountability (the bite; =
OQ-107).~~ The acknowledgment is **self-reported**, not confronted against an external later state.
`measurement/5` is authored/imputed (`data_repair.pl` is "an imputation engine"); there is no
external-instrument adapter. The thing that makes Cor-3 *bite* — a pre-committed t0 confronted by an
external later witness (the dated falsification matrix; columbia's third-party data) — has **no
engine path.** That path is **OQ-107** (survey-wave witness adapter), UNBUILT. Until it exists, the
engine's drift/acknowledgment is a within-story self-consistency check and **must not be cited as
Cor-3 accountability.**

**External-anchoring grade (the live correction this thread produced; the inert tail below it is
named and declined).** B-historical accountability is **graded**, not binary-on-falsifier:
- **Tier 1 — external/dated/numeric/third-party** (columbia): the staked confrontation reads
  near-identically from any seat; strongest accountability. *A dated falsification matrix confronted
  by external data is the gold standard — there is no "better"; the demand for one was a flinch.*
- **Tier 2 — author's retained record** (vote_market Branham): Cor-2a-declared and legible, but the
  **record-boundary Π** (which commitments count as live vs. superseded) is the author's seat to
  draw, and it conceals a beneficiary (editorial privilege). Spec consequence: B-historical owes a
  **declared record-boundary field** alongside the witness triple.
- **Tier 3 — held-open** (no committed t0 / no falsifier): **no temporal handle**; honest router
  output is "seated, no temporal handle."
- **Stop, declared:** there is no σ-settled floor even at Tier 1 (columbia 97–104 shows residual Π in
  what-the-data-meant), but descending to "therefore no floor in principle, add another declared-Π"
  is **distributively inert** (it conceals no who-pays) — the halting-problem tail. Floor = Q1
  (descend while a seat hides a distribution; stop when the next is inert). The record-boundary
  (Tier 2) is the last live seat; below it is not written.

**Why this is an artifact, not a self-correcting engine behavior.** Cor-3 accountability cannot live
inside the generation step: no later seat confronts the earlier one within a single pass (the model
authors prediction and confrontation together; it has no over-time). Accountability is therefore
**installed in the artifact** — dated falsifiers, pasted witnesses, the disposition log — by the
party that persists (`build_discipline.md`), and its accountability is *realized on publication*
(cafebedouin.org), where external later seats (commenters, future readers, the world) actually
confront it. So the engine's part is discovery; the bite is the published artifact, not the engine.

**What resolution would change / graduation (all OPEN, none scheduled):** (a) refactor the
acknowledgment consumer to witness-not-verdict (Gap 1) — a discovery-layer cleanliness item,
buildability unknown, low-stakes (the engine is not the accountability layer); (b) ~~build the
OQ-107 external-witness adapter as the missing bite~~ **RETRACTED** — OQ-107 is a discovery-side
*input* enhancement, not an accountability gate; the bite is publication, which already exists;
(c) grade the *published artifact's* accountability by external-anchoring tier and record a declared
record-boundary (OQ-55 B-historical spec; this is a property of the essay/venue, not an engine
build). The honest unknown stated as such: **whether (a) is worth building is open; there is no (b)
to build — the accountability layer is the public record, not a future engine feature.**

**Cross-refs:** `commitment_systems_sketch_v5_2.md` §3 (the T0→T3 source); `seat-theorem-v1.md`
Cor 3 (non-interchangeable temporal seats), Cor 2a (declared-not-σ-settled), §6 Q1/Q5 (the floor),
§8 (record-boundary Π); OQ-55 (B-historical channel = this; B-structural ∥ A/C); OQ-74 (the
reading-relative declaration is the operator's to make — declared seat, not foreclosed; **pending
operator ruling**, not authored here); OQ-107 (Gap 2 adapter); OQ-49/50/116/122 (Gap 1 = the same
seated-verdict-as-engine-fact error); OQ-107 (discovery-side external-data *input*, NOT the
accountability bite — see CORRECTION); OQ-83/OQ-102 (drift-series provenance);
`design_discipline.md` §9 (engine = context of discovery), §8 (the t3/honoring gap). Worked
instances: `columbia_falsification_matrix.md`, `the_vote_market/the_vote_market_draft3.md`;
publication venue: cafebedouin.org.

**Provenance:** 2026-06-13 CC session; engine coverage witnessed by grep this session
(`cs_pattern_detection.pl:404–412`, `cs_drift_engine.pl:70–84`); the gold-standard-floor and
stop-at-the-record-boundary rulings are the operator's (this thread).

## OQ-127 — SCAFFOLD_DANGER_ZONE lint calibration: 5/7 fire rate on a shape the de-leaked prompt authors legitimately

**Ω-type:** Ω_E (advisory-lint calibration; witnessed on the cohort-zero pilot).

**Status:** open — split-refiled from OQ-116 on 2026-06-14 when the MMC half resolved. The
**Priority:** 1
**Deps:** splits_from OQ-116
operator-only linter ruling (OQ-116: the linter is an operator diagnostic, lint never gates
generation, threshold-coupled codes never reach the authoring LLM) **was applied here and
explicitly did NOT settle SDZ** — do not re-derive the MMC reasoning and extend it to SDZ; the
cuts differ (below). Status honesty is the deciding reason for the split: OQ-116 would otherwise
carry a resolved+promotable principle and a live calibration question under one status field.

**Witness (`audits/2026-06-12_cohort_zero/pilot_witness.out`):** 5/7 pilot stories fired
SCAFFOLD_DANGER_ZONE (mid-ε + beneficiaries + no enforcement/sunset + low theater — a shape the
de-leaked prompt authors legitimately). Both lint rules fired in the same pilot run, so this OQ
inherits that audit artifact directly (not by pointer-to-OQ-116).

**Why SDZ does NOT close as MMC did (the two genuine cuts):**
1. **No correction-grade analog of ANY family.** `prolog/signature_detection.pl` has
   `coordination_scaffold` only as a POSITIVE classifier (`coordination_scaffold_signature/_`,
   line 412); there is **no `false_scaffold` signature** (grep-confirmed; positive control:
   `false_natural_law`/`false_summit_mountain` DO exist, so the grep fires). Unlike MMC's FNL,
   the scaffold "danger" SDZ flags has nothing the engine *overrides classification* for — it is
   not a claimed-vs-structural divergence at all, just a warning that a metric profile might
   produce an UNINTENDED positive classification. The OQ-74 mirror ("engine measures the
   divergence; lint just notices it") therefore **does not transfer**. (Not "no FSM" — that
   baseline is stale; MMC's analog is FNL, see the OQ-116 correction.)
2. **Perspective mismatch.** The scaffold gate is context/Chi-dependent (`drl_core.pl:145`
   `is_scaffold(C, Context, scaffold)`; `classify_from_metrics(C, BaseEps, Chi, …)`), but SDZ
   fires **perspective-independently** at authoring time on base authored ε (`linter.py:233`,
   `ext_val <= scaffold_extraction_ceil`). So rewording SDZ as "readout of the scaffold gate"
   would be **inaccurate**, not merely gentler.

**De-leak membership is correct (orthogonal to calibration):** SDZ discloses/predicts an engine
boundary, so it **stays in `THRESHOLD_COUPLED_LINT`** (stripped from author prompts via
`linter.build_author_feedback`) regardless of the calibration call.

**What would resolve (operator's call — remedy FRAMED, not picked):** (a) narrow the trigger so
legitimate de-leaked ropes don't fire; (b) demote severity / mark coarse heuristic; (c) reword to
"authoring-time predictor of a perspective-dependent gate" (accurate, but does not address the
5/7 rate). No trigger surgery pending the ruling.

**Cross-refs:** OQ-116 (MMC half resolved; the shared operator-only linter ruling), OQ-74 (seat
divergence / de-leak-in-reverse — transfers to MMC, NOT to SDZ), OQ-117 (claim/metric
co-authoring doctrine), `docs/design/design_discipline.md` (linter operator-vs-engine principle).

**Provenance:** 2026-06-14 CC session implementing the OQ-116 close plan; the operator-only linter
ruling and the split decision are the operator's.

---

## OQ-128 — mid-power-mountain→`rope`: power-scaling artifact or correct refusal to certify a false foundational claim? (Ω_C)

**Ω-type:** Ω_C (design ruling — whether the mid-power decline is intended semantics).

**Status:** open — minted 2026-06-14 (split out of OQ-122's residual so it stops floating in prose)
**Priority:** 1
**Deps:** splits_from OQ-122, bundled_with OQ-122
**Origin:** OQ-122 fixture/verdict work, 2026-06-14. `classify_from_metrics` (pre-signature) returns
**non-mountain at the moderate/institutional power seats for ALL mountain-claimers** — a power-scaling
effect of χ = ε·f(d)·σ(S) shifting the mid-power contexts off the mountain band
(`drl_core.pl:605-613`, the comment above the `type_1_false_summit` clause). Witnesses: the false-RED
physics controls `radiative_levitation_stratification`, `actinide_replenishment_mechanism_flat_control`
(the two live `false_summit_mountain` firings, both vic=0).

**Specific question:** is mid-power-mountain→`rope` an **artifact** to fix, or the engine **correctly
declining** to certify a foundational claim that does not hold at mid-power seats? **The standing fact
that makes it possibly-correct, not a bug:** the signature layer *restores* mountain at mid-power seats
for **genuine** mountains in `dr_type`, so this only bites **non-restored** mountain-claimers — exactly
the population whose foundational claim the engine has reason to refuse. The Ω_C is whether that
refusal is intended. It is a **general engine phenomenon** (every mid-power mountain-claimer), broader
than the physics-control story — hence its own number, not an OQ-122 sub-bullet.

**Why it matters / what resolution changes:** it, not the FSM victim-gate, is the **binding
verdict-mover** for the physics false-REDs to reach GREEN — so the FSM gate is held to land **bundled
with this Ω_C's resolution** (OQ-122 ruling 2026-06-14). Do NOT call this "the OQ-50 power-scaling fix":
the direction is unruled; no settled artifact exists.

**Cross-refs:** OQ-122 (residual this splits from; the held FSM gate bundles here), OQ-50 (the
false-summit detector this rides — both OPEN items closed 2026-06-14), OQ-117 (false-foundational
rejection gates — Boltzmann non-compliance + type_1 + computed type), OQ-70 (neutron_star/FCR, separate).

## OQ-129 — perspectival-gap feeder rewired onto authored seats; labeling partition + coverage calls OPEN (Ω_C)

**Ω-type:** Ω_C (design ruling — the Ω labeling partition and the deliberate-vs-hole coverage calls).

**Status:** partial — rewire RESOLVED + witnessed 2026-06-14; OPEN-A..D carried below.
**Priority:** 1
**Origin:** `omega_from_gap/5` stopped firing after the 2026-06-05 rebuild. Root cause: its feeder
`report_generator:detect_gap_pattern/2` queried the **retired** `constraint_indexing:constraint_classification/3`
(the pre-rebuild per-power-seat stored-type surface). The rebuild moved that idea to
`narrative_ontology:constraint_stakeholder/7` (seat carries `(Power,Time,Exit,Scope)`; type computed
on demand). Live corpus: 0 `constraint_classification` facts → 0 gaps → 0 omegas. One dead wire.

**Resolved (the rewire):** `detect_gap_pattern/2` now computes each authored seat's type via the
canonical seat path `stakeholder_seats:dr_type_for_stakeholder/3` (per-`(C,Name)` d — escapes the
same-power atom collapse; chosen over the plan's inline `dr_type/3`, witnessed verdict-equivalent).
Gap = ≥2 distinct non-`unknown` types; fail-closed on <2. `omega_from_gap/5` is now **labeling**:
`extraction_blindness` (extractive seat lower-power than functional seat → `omega_extraction_blindness_<C>`,
critical) and `general_type_mismatch` (→ `omega_perspectival_<C>`); label computed into fresh vars then
unified so a pre-bound pattern cannot bypass the priority. `json_report.pl` gaps-array guard moved from
the dead `constraint_classification` to `report_generator:gap_coverage/1` (keeps null/`[]` distinct).
**Witnesses:** `audits/2026-06-14_omega_gap_reconstruction/` (per-item dump 20 GAP/17 no_gap/20 abstain;
5/5 label controls incl. both-label positive + power-order negative; d-path equivalence; twin breadth
flash 481/267/212, haiku 369/125/466; pipeline 20 `omega_extraction_blindness_*` serialized; check_stack
clean; validation 0 errors).

**OPEN-A (labeling partition).** extraction_blindness/general grounded (docs/logic.md §B.7 `naturalized`=
cover side; v7 Theorem-1 raw orbit `[naturalized,snare,rope,snare]`). **Finer labels `cut_safety`
(mountain/rope) and `learned_helplessness` (snare/mountain) are NOT yet grounded** — they currently fold
into general/extraction_blindness. Resolution = ground or drop them.
**OPEN-A advance (2026-06-14, witnessed): `extraction_blindness` is substantially an existential-labeling
artifact.** `label_gap/4` (report_generator.pl:275–279) fires `extraction_blindness` whenever *one*
(extractive seat lower-power, functional seat higher-power) pair exists (`member/member`, `De>Df`, cut) —
it never checks the relationship is monotone or dominant. So the same constraint's seats frequently
co-license the **mirror** reading (extractive seat at *higher* power than a functional seat) at once,
making the templated headline false to its own data. Witness: `audits/2026-06-14_extraction_blindness_existential_label/`
(`mirror_metric.txt`) — live **16/20 (80%)** mirror (corpus-independent positive control), `testsets_haiku`
**258/358 (72.1%)**, avg **2.73–2.85** distinct types/constraint (of ~6: non-monotone). Resolution = gate
the label on a monotonicity/dominance check, or demote the headline to "type-divergence across power,
direction non-monotone" when the mirror also holds. **Until ruled, gap-omega `extraction_blindness`
instances stay instances of this open Ω_C — never promoted to an asserted per-record field** (a clean
authority-controlled value can be uniformly false-to-its-data; the contested bit is sourced from this
record's open status).
**OPEN-B (coverage map).** `coverage_map.md`: deliberate-vs-hole call per abstainer needs the PROSE (not
done). `catastrophe_memory_kernel__boundary_maintenance_reading` is a strong HOLE candidate (six_questions
authored, `stakeholders[]` empty). The two `*_contradictions` entries are pl-only (NO_JSON).
**OPEN-C (`unknown` seats).** `livelihood_security_reading`: 8 authored seats all compute `unknown` (seat
path) → `gaps:null`. Missing-metric hole vs genuinely untyped — unresolved.
**OPEN-D (dedup).** Moot on live (20 gap-omega constraints, 0 authored-omega constraints; distinct ID
prefixes; `collect_omegas` dedups by ID). Re-check when authored omegas co-occur with gaps.

**Why it matters / what resolution changes:** the engine's whole perspectival-gap → Ω surface
(`assert_omegas_from_gaps`, `collect_omegas`, validation `omega_count`) was silently empty corpus-wide;
it now fires. The OPENs decide how many Ω *flavors* the surface carries and which abstentions are real
holes to backfill in generation.

**Cross-refs:** OQ-70 (FNL prevalence is bait-confounded — gap-Ω prevalence inherits the same authoring-
convention caveat, do not cite gap counts as a detection result without ruling this), OQ-83/OQ-109
(stakeholder-seat observer layer this feeder now rides), Build Discipline Pattern 1 (the dangling wire
this was) + Pattern 6 (the null/`[]` coverage bit).

---

## OQ-130 — omega-resolver: pilot validated on ISSUES.md; scale to the corpus omega set (Ω_E)

**Ω-type:** Ω_E (the scale claim — "computing over the access points routes the right few from the many" — is measurable; pilot done, corpus arm pending).

**Status:** open — pilot RESOLVED + witnessed 2026-06-14; corpus scale arm carried below.
**Priority:** 1
**Origin:** the omega-resolver memo (`~/.claude/plans/brief-the-omega-glittery-wozniak.md`): one
defect (absence-as-presence) read at four layers (wiring/evaluation/summary/relationship), at two
scales — ISSUES.md (128 typed OQs) and the corpus (4,430 authored omegas, `outputs/dump_omega_haiku.md`).
The resolver is the apparatus carrying the provenance/coverage/confidence bit to the read site.

**Pilot (RESOLVED 2026-06-14, `audits/2026-06-14_omega_resolver_pilot/`):** read-only catalog views
over ISSUES.md prose + one authored `Deps:` field (no `issues/` migration — Option 1). Apparatus:
`python/omega_resolver.py` (loader / authority control / SCC-condensation frontier view §D / checker
/ planted-fixture selftest). The only-claim-in-doubt test (§E): frontier view vs an independent
naive cold-reader baseline → **57 confirm, 7 contradict, 0 standoff**, each contradict settled by an
EXTERNAL fact (resolved-blocker status for OQ-37/41; own Ω_P type for OQ-03/56/58/69/82). Selftest
8/8 (incl. §D 2-cycle→one standoff, over-fire negative control). Criterion met: checkable verdict +
≥1 §A defect-OQ advanced (OQ-129 evaluation, OQ-37 wiring).

**Refinement the pilot surfaced:** the OQ→OQ + Ω_P model mis-buckets active Ω_E entries blocked on
an operator-spend-go / substrate gate (OQ-71/75/119) as workable_now; fixed with one authored
relator `blocked_on_human <freetext>`. Add to the §2 access-point families if/when the catalog
serialization is built (scale-time).

**Scale-arm POC (DONE 2026-06-14, `audits/2026-06-14_corpus_omega_soundness_poc/`).** Ran the §C
soundness gate as a proof-of-concept under a two-party independence protocol (sealed adjudicator
held-sample key committed `acc27d22` BEFORE a blind executor subagent ran probes 1–4), read-only over
`prolog/testsets_haiku/*.pl`. **The blocking precondition is discharged: the authored omegas are NOT
§8-class.** Measured results:
- **Soundness 24/30 = 80%** (Ω_E 86.7% / Ω_C 75% / Ω_P 66.7%); held-sample blind agreement 9/10.
  Content-templating LOW (probe 2: combined q+approach prefix max-dup=1) ⇒ the corpus artifact is
  **identity-overstatement, not fabrication.** The unsound omegas are a *class* — the kernel-contest
  family (`kernel_reading_contest`/`contestation_space`/`committer_frame__*`) restating authored
  ε/victim deltas (fails Irreducibility), not random noise.
- **Identity is three orthogonal axes, MEASURED (probe 1, converting the memo's assertion to result):
  KIND ⊥ topic** (signature/orbit ARI≈0 vs the `cs_kernel_id` partition; same-kernel→same-cluster
  7.65%≈chance — and `gauge_orbit`==`fingerprint-shift`, ONE KIND organ not two) **and frontier ⊥
  topic** (the suppression frontier family spans 225–264 kernels, far above its ~85 top-name count).
  The engine's existing organs **cannot** serve as the frontier-identity organ.
- **The two defects coincide:** the unsound kernel-contest family IS the semantic near-dup family that
  drives the frontier collapse, so a semantic-frontier dedup organ would simultaneously dedup AND
  quarantine the unsound class — dedup and the soundness gate are not independent passes.
- **Aspirational agenda shape (probe 4):** ~2,901 distinct frontiers (semantic-dedup lower bound,
  from 3,755 name-keyed), 61.9% Ω_E / 35.8% Ω_C / 2.3% Ω_P — measurement-dominated.
- **One disagreement unsettleable by the external authority:** id 20 (`messianic_timeline_indeterminacy`)
  is a frame-question with an empirical-historical resolution — an apparent *hybrid*, which
  `omega_variables.md` itself lists as an open question. Ω-type scoring reliability is limited exactly
  at the hybrid boundary.

**Fold-backs (the actual POC deliverable):** (#1) the frontier-identity organ neither signature nor
kernel provides is logged as **GAP-11**; at ISSUES.md scale the missing axis is *kind made computable*
(§A is hand-authored — a `kind` field for the resolver). (#2) ranking among same-bucket frontiers is
still unproven and the corpus is where it bites; probe 3's soundness share is the first ranking
ingredient (down-rank kernel-contest-restatement omegas before ranking). (#3) the §1b freshness key
must stamp a *source content-hash*, not git HEAD (carried below).

**Scale arm (still OPEN — the build, now de-risked by the POC):** point the loader at a corpus omega
record built from `dump_omega_haiku.md` (+ gap omegas), run §C's two gates before trusting an agenda —
(1) dedup by frontier-identity (positive control: a planted known-duplicate pair must merge, a known-
distinct sibling pair must stay apart; the POC's lexical clustering is a LOWER BOUND — embeddings the
real organ, see GAP-11), (2) carry each omega's `confidence_without_resolution` bit to the read site —
then per-Ω-type finding aids. The POC validated the *gate*; the *build* (serialized catalog + ranking)
is the remaining work.

**Why it matters / what resolution changes:** turns the 5.5 MB `dump_omega_haiku.md` from a dump into
a deduplicated, confidence-carried, reachability-ranked research agenda — but only after the soundness
gate, else it launders artifactual omegas into a clean-looking catalog (the §1g hole).

**Cross-refs:** OQ-129 (the engine gap-omega surface this generalizes; OPEN-A is the §8 evaluation-
layer defect-zero), OQ-70 (gap-Ω prevalence is bait-confounded — the agenda inherits the caveat),
GAP-11 (the frontier-identity organ the POC's probe 1 showed is missing), Build Discipline Patterns
1/5/6 (the four layers are instances of these).
**External adjudication addendum (2026-06-14, separate instance — `adjudication_external.md`).**
Probe-3 independence was *within-instance* (the executor sealed its own held key); the first
genuinely-separate read corrects id-20 (`messianic_timeline_indeterminacy`) SOUND→TYPE_INCORRECT, so
**≈77% sound (23/30) — and the only external look moved the rate DOWN**, i.e. the 80% was
optimistically biased, not merely imprecise. Three caveats stand: (a) probe 1b came back ≡ 1a, so
kind⊥topic is proven for **ONE** KIND surface — not "the KIND organs are orthogonal"; (b) the
independent check *lowered* the number; (c) the unsound mass = the kernel-contest family (= probe-4
dedup mass = probe-1c frontier family — three probes triangulate one culprit). **Interpretation
CONTESTED / OPEN (operator):** whether that family is convention-noise to gate at the generator, OR a
*legitimate committer-frame Ω_P frontier the design intends* — the Seat Theorem Cor 2b (the ruling
that types OQ-56 Ω_P) holds "which reading/seat" to be a **declared, contestable premise, not a
theorem**, i.e. an Ω_P frontier, and the generator+schema are built to have the LLM take an authorial
stand on a reading. Under that reading the family is **sound-but-MISTYPED** (conceptual→Ω_P), not
restatement, and its cross-kernel recurrence is the invariant-probe pattern, NOT duplication. **So
the prior bullet's "restating ε/victim deltas (fails Irreducibility)" is the executor's reading, not
a ruling.** Forward step (OQ-130 child, NOT a generator gate): (1) *uncontested* — retype the
kernel-contest family Ω_P; (2) *settle first* — **local frontier vs template stamp** before any
generation change. Push pre-condition (not §8-class) holds regardless.

**Ω-type diagnostic POC addendum (2026-06-14, separate-instance cross-read —
`audits/2026-06-14_omega_type_diagnostic_poc/`).** Ported `debugging_philosophy.md` §6.1 ("the fix
that works reveals the type") to Ω-vars: type each omega by **which resolution operation discharges
it** (define→Ω_C / decide→Ω_P / measure→Ω_E, external-at-own-locus else **restatement**). Adjudicator
sealed a 14-omega held key (`94c7346e`, BEFORE the blind executor ran), anchored to the substrate
question not §6.1. **The settle-first ruling above is now answered, and step (1) is REFUTED: the
kernel_reading/committer family does NOT retype uniformly Ω_P — it SPLITS per-omega by mechanism**:
Ω_C (genuine criterion frontiers), **Ω_P** (committer-position frontiers, e.g.
`reading_committer_frame_dependence` — the Seat-Theorem Cor-2b case, real-and-mistyped), **Ω_E**
(observable suppression, e.g. the Hanafi `kernel_reading_contest`), **restatement** (generate-and-
compare artifact, e.g. the Nicene `kernel_reading_alternative_framing`). Two omegas named
`kernel_reading_contest` discharge differently (Ω_P+Ω_E vs Ω_E) ⇒ **mechanism ≠ name; any name-keyed
retype is unsound.** Cross-instance: 9/14 held agree, 5/14 the adjudicator types differently — **all 5
in the family**, because the executor's LLM-judge **collapsed decide/measure into define and re-stamped
the authored `conceptual` label** (so its 82.5% agree-with-authored is inflated on the family;
adjudicated ≈72.5%, mislabeling moved UP under independence). Self-label unreliable **both directions**
(conceptual→empirical off-family too: cover-story/historical-necessity/counterfactual omegas) ⇒
**diagnostic is load-bearing.** Two-sided gate control **fired**: executor's restatement limb MISSED
both seeded restatements (false-neg) ⇒ `restatement-rate=0` is partly gate-no-op (Pattern 5/6), not
measured-empty — **sharpen the restatement gate before any corpus-wide restatement count.** "Ω_E is a
status" **HOLDS** (1/14 falsifier, rare, directional). Deterministic baseline `unknown`=12.5% but
agrees with judge only 47.5% ⇒ true determinism boundary ≈53% (commits-wrong 40%) — typing needs
judgment at the omega layer. **Forward (per-omega retype + gate-sharpen) folded into OPEN below;
ESCALATED to operator** (genuine framework calls, not adjudicable from evidence): (i) the **decider-
locus vs define-dominant cut** — the whole family split rides it, and two instances split on it; (ii)
the **ε-invariance define-vs-restatement** call (the `kernel_reading_distinction` subfamily, contested,
not ruled); (iii) whether confirmed Ω_E-as-status **edits `omega_variables.md`** (an Ω_E status-line);
(iv) wiring **diagnose-then-stamp into the generator** is a deferred generator change gated on (i).

**Restatement-gate no-op FIXED (2026-06-14, `…/restatement_gate_fix.md`).** The build defect behind
metric (c) — the gate missed 2/2 seeded restatements — is repaired: the gate now consults the entry's
`declared_fields`, so re-deriving the constraint's own authored fields (ε-invariance / comparing
authored ε·base-properties·victim-sets across **declared** readings) is RESTATEMENT, on whichever
signature fired. **Two witnesses:** (1) `deterministic_baseline.py` has a runnable `seed_control()`
that is now **GREEN** (id20/id27 caught; KNOWN_EXTERNAL pass; UNDER_DECLARATION external; two-sided
commit control holds; exit 1 on RED); (2) a **blind LLM** under the fixed protocol independently typed
id20/id27 restatement and id1/id30 external — refuting regex-overfit. Both prior gates on (iv) are now
clear (#1 declared 2026-06-14; gate works), **but (iv) stays deferred for a different reason:**
deterministic typing alone is ~half judgment (`unknown_rate` 0.10; the family split rides the
operation-locus seat), so a generator stamp would need the LLM-judge in the loop — a cost decision,
not a build-blocker. The historical `judge_results.json` is kept as the POC's as-run artifact.

**Evidence follow-up (2026-06-14, adjudicator, read-only — discharges #2, advances #1/#3).**
- **#2 RULED → restatement.** `metrics_as_routing.md` + `when_metrics_arent_measurement.md` (foundation
  docs) establish **ε is ROUTING, not measurement**. So the ε-invariance test (`kernel_reading_distinction`
  27/31) re-derives a *routed* value the constraint already carries ⇒ **restatement, not Ω_E**. Corollary
  that sharpens the gate: the diagnostic's "measure" = a **world** observation, never computing/comparing
  an engine metric (ε/π/purity).
- **#2 seam (one-line flag).** This makes the define/measure boundary depend on the ε-is-routing claim: if
  an omega's "measure" genuinely needs observing something **ε only proxies** (routed value and world value
  diverge), it is mis-routed to restatement. Probably rare; the seam #2 introduces, not a blocker.
- **#3 — under-powered objection removed; doc-edit WAITS on #1.** Ω_E stratum re-run, **N=24 over-sampled
  authored-Ω_E** (the POC's "1/14" was mis-framed — falsifier lives over authored-Ω_E, not all 14): **status
  holds, ~22–23/24 adjudicator-typed** (a hand-read band, directional — not a measured point; don't quote a
  false-precision %). All resolve by genuine world-observation (ethnography / longitudinal / historical-textual
  / survey), **none by engine-metric**; falsifier ~1–2/24 (`documentation_sufficiency` = define-a-standard).
  The omega_variables.md status-line edit is now **supported but still a framework-doc change** — operator's
  design call, **revisit after #1 is ruled** ("supported" ≠ "do it"). **#3 LANDED 2026-06-14** (operator
  approved): `omega_variables.md` Ω_E block now carries a first-person **declared-reading** status-line —
  Ω_E = type assigned when the *named resolution* is a world-observation (status: awaits-external-input),
  evidence marked **directional (N=24, ±~18pp), not a census**, with the ε-routing boundary and an explicit
  operation-locus **seat** caveat. Declared as a governance stand, revisable with the seat — not a metaphysical
  claim that Ω_E "is" a status.
- **Number hygiene — quarantine the 40-sample judge numbers.** 82.5% agree / 47.5% det-vs-judge / "≈53%
  boundary" all ride the **disqualified no-op judge**. Trustworthy figures are on the 14-omega hand key:
  judge-vs-handkey **9/14**; the deterministic baseline (re-checked vs the hand key, define→C/decide→P/
  measure→E) fails the **same committer-frame decider-locus cases (19/28/38) + restatement (20)** the judge
  does ⇒ both automated layers fail exactly where judgment is required; boundary is partial-credit-soft
  (~36–71%), not a clean 53%.
- **#1 — HELD OPEN (operator ruling 2026-06-14); the read-back is NOT independent confirmation.** Correction
  to an earlier overstatement: the held key was sealed before the **executor** ran (independence from *it*),
  but the read-back was performed by the **same adjudicator who proposes decider-locus**, asking the
  decider-locus question of their own sealed types — *"one mind, twice,"* not the text independently grounding
  the cut. And **define-dominant was never faithfully tested**: the executor's gate that emitted "uniform Ω_C"
  was a **no-op** (missed 2/2 seeded restatements), gate-silence not a faithful define-dominant read. So the
  cut was ratified by its proponent against a broken opponent. **Forward (the unspent unit of independence):**
  a **second blind instance** types the contested committer-frame omegas (19/28/38) + ε-invariance (27/31)
  from resolution text + **both** locus rules stated neutrally, **without** the held key; ratify only on two
  independent reads. (Decider-locus is probably right — the resolution text does say *values decide* — but
  "evidence-supported by independent read-back" overstated it by one full instance.)
  **Second blind read DONE (2026-06-14, `…/second_blind_read.md`): define-dominant REJECTED on two
  independent reads.** Fresh instance, define-dominant a live neutral option, no held key — typed NONE of the
  5 as Ω_C: id19 Ω_P, 27/31 restatement, 38 Ω_E, 28 Ω_E(Ω_P-shadow); **3/3 exact** with the hand key on the
  overlap, id28 a fine Ω_E-vs-Ω_P split (both agree the decider is external/committer). It **sharpens** the
  cut: rule = **"type by the named resolution operation, not option-locus"** (decider-locus→Ω_P is the special
  case; Ω_E where the operation is a world-observation; ε-invariance 27/31 = restatement under both rules).
  **#1 ratifiable on two independent reads — operator's signature is the only remaining step;** on signing,
  the rule lands as *operation-locus* and the family retypes per-omega (Ω_P/Ω_E/restatement, NOT wholesale).
  **#1 DECLARED (operator, 2026-06-14) — as a SEAT, not a truth claim.** The ruling is not "operation-locus
  is correct" (no seat-free adjudication exists — Hume: reason serves the selection; Seat Theorem Cor 2b: a
  declared, contestable premise; `metrics_as_routing.md`: a governance stand, not a truth claim). The ruling
  is a **declaration of seat**: *we occupy operation-locus.* define-dominant is **one** alternative seat,
  named and **declined — not refuted** (a seat cannot be refuted, only declined; the two blind reads showed
  our seat is *stable/reproducible*, not that the rival is false). Per `docs/altar-to-the-unknown-reading.md`:
  there are **other seats we could have occupied and did not, including a sixth-seat class we cannot name**
  (typing-rules that would not register as typing-rules to us); we **reserve that seat unfilled** — "naming
  the sixth seat reserved-but-unfilled is what prevents the map from claiming to be the territory." **We are
  obligated to declare OURS, not to enumerate theirs** (a completeness-census would be the cyclopean move one
  level up). Carried risk, declared (altar §86 Ω_P): the reservation itself may be acknowledgment *or*
  sophisticated status — undecidable from inside; left reserved because pretending the two named seats are
  exhaustive is the visible trap. **Recursion noted:** the resolver, asked how it types omegas, declares a
  seat and builds an altar to the typing-rule it cannot name — it instantiates the structure it routes; the
  determinism boundary is the seat, and the seat is irreducibly declared, never computed. Family retypes
  per-omega **under this declared seat, revisable** if the seat is ever re-declared.

**Discharge:** (soundness-spot-check discharged 2026-06-14 — POC ran, ≈77% sound external-adjudicated / 80%
executor, not §8-class. Ω-type diagnostic POC discharged 2026-06-14 — family splits per-omega, wholesale-
Ω_P-retype refuted, self-label load-bearing-to-fix; 4 operator escalations open above.)

## OQ-131 — Six observer positions vs four: what a 6-point observer site does to the cohomology, and whether a future corpus should adopt it

**Ω-type:** Ω_C (design choice — whether a future corpus widens the observer fingerprint to 6
positions) with an Ω_E measurement arm (what the widening does to H⁰/H¹ on the existing corpus).

**Status:** future — operator ruling 2026-06-15: a real question worth recording, not slated for
work now; closed `future` (searchable, full-bodied, revive if a future corpus is built). Arose
from OQ-108's witness-coverage finding (the 6-vs-4 split below). **Q1 (Ω_E) MEASURED 2026-06-15**
(operator confirmed arm (a)): consonant-suppressing on live + both twins, see the Q1-measured
block below and `audits/2026-06-15_oq131_six_observer/`. Entry **stays `future`** because **Q2
(Ω_C corpus-adoption) is the deferred part** — the Ω_E arm is resolved, the Ω_C arm is not.

**The setup (from OQ-108).** Authoring uses the **6-atom power vocabulary** (powerless, moderate,
powerful, organized, institutional, analytical; `docs/logic.md:293`), but the observer fingerprint
is a **discrete 4-point site**: `logical_fingerprint:fingerprint_shift/2` probes only the four
power atoms that have a `standard_context_for_power/2` (powerless/moderate/institutional/
analytical). `powerful` and `organized` carry π (`config.pl:57-62`) and canonical-d
(`:142-143`) but have **no standard observer context**, so they never appear as a perspective
column or a site point. The cohomology rests on that 4-point cover: `grothendieck_cohomology.pl:10,17`
defines U = {U₁..U₄}; **H¹ = count of disagreeing context-pairs across the observer site**
(`:115`); the hub-conflict trigger fires at H¹ ≥ 4 (`abductive_hub_conflict_h1_threshold`,
`abductive_triggers.pl:816-820`); the Python proxy `compute_h1` is distinct-types / total-
perspectives over the 4 (`python/boolean_independence.py:370`).

**Q1 (Ω_E — measurable NOW, no new corpus).** Give `powerful` and `organized` standard contexts →
a **6-point site**. H¹ (disagreeing pairs) now ranges over C(6,2)=15 pairs vs C(4,2)=6; the
`shift/4` fingerprint becomes a 6-tuple, re-identifying orbits and H⁰ (singleton-orbit count)
corpus-wide; the h1_band thresholds (hub_conflict ≥4) need recalibration against the larger pair
space; the subobject-classifier picture (currently trivially Boolean on the 4-point discrete site)
changes. The mechanism already exists: `constraint_indexing:site_contexts/1` defaults to the 4
canonical contexts and is switchable (it is how the product site is selected — `grothendieck_-
cohomology.pl:111-113`), and the engine can already classify the existing constraints at
powerful/organized seats (π and canonical-d exist). So this arm is a **cheap pre-registered probe
on the current corpus** if ever revived: add the 2 contexts, switch `site_contexts/1`, recompute
H⁰/H¹, diff the bands — pre-register what a band shift vs no-shift would mean before running.

**Q1 — MEASURED 2026-06-15 (Ω_E arm RESOLVED; `audits/2026-06-15_oq131_six_observer/`).** Ran the
pre-registered probe across the live corpus + both committed twins (`testsets_haiku`,
`testsets_flash`, 960 each, fixed substrate ⇒ deterministic) under additive six-observer site
modes `canonical_6` / `power_only_4` / `power_only_6` (engine commit `a06b5c7f`; new
`site_contexts_for_mode/2` clauses, first-arg indexed, no catch-all; default `canonical` witnessed
unperturbed; new seats appended AFTER the canonical four so the 6 canonical pairs stay positional).
Headline = mean over constraints of **(H¹₆−H¹₄)/9** (the 9 = C(6,2)−C(4,2) movable new pairs,
conditioned on the fixed 4-seat config; **9-pair basis PASS for every constraint** — the byte-
identity is witnessed, not assumed). Gates all PASS: zero len-6 fallbacks, exchangeability gate
PASS (re-run orbit-identical ⇒ `dr_type` pure fn of C ⇒ permutation valid), seat-marginal entropy
non-degenerate, liveness PASS.

**Finding (all three corpora, same sign): observed (H¹₆−H¹₄)/9 falls BELOW the pre-registered
permutation band → `consonant_suppressing`.** live 0.446 vs [0.741, 0.825]; haiku 0.562 vs
[0.738, 0.755]; flash 0.550 vs [0.754, 0.775] (N=1000, seed=20260615). **Two distinct facts, two
witnesses (the design's permutation-vs-redundancy decomposition, kept separate):** (1) *permutation
(load-bearing null)* — the new seats disagree with the fixed 4-seat config **below chance**: less
than random reassignment of the *same* seat-type marginal would, so they are anti-correlated with
disagreement (this is the `consonant_suppressing` label; it speaks to disagreement-vs-chance, **not**
redundancy). (2) *co-classification (arm 6, NOT the null)* — `echoes_both` = **neither new seat
introduces a type absent from the 4-seat orbit** (witnessed defn, `oq131_six_observer_probe.py:335-342`:
`s5 not in four`/`s6 not in four`), holding in 82/69/62% (`echoes_both + new_type_either == n`
exactly). `echoes_both` is a **type-novelty** measure (membership in the 4-type set) — it is **not**
per-seat redundancy (echoing one *specific* canonical seat, which the probe does not measure) and
**not** modal consonance; do not read the echo % as the cause of the permutation result. The two
reinforce here but remain two facts. Net for the resolution question: the new seats add a novel type
in only 18/31/38% of constraints **and** their disagreement is below chance — modest added
resolution, fracture suppressed not manufactured. The combinatorial artifact (mechanical H¹ inflation
from more pairs) is **FALSIFIED** — it would have read within/above band. Co-outputs: orbit splits 10/293/367; H⁰ singletons can only drop (haiku 139→117, the
comparability-with-the-4-position-corpus boundary). The effect is **power-atom-driven, not bundle-
driven** (`power_only` control ≈ realistic headline) and **bundle-robust within the §3 sweep
envelope** (every per-seat ladder point still below band; highest 0.62 on flash, still < 0.754).
Twin model gap **0.012** on the **873 non-grid matched stratum** (basename key; grid census 87/0
reproduces the plan exactly) ⇒ composition, not model. Full writeup + raw cells + `analysis.json`:
`audits/2026-06-15_oq131_six_observer/FINDINGS.md`.

**Two scope walls on this Q1 result (no silent narrowing):** (i) **H⁰/H¹ ONLY** — the subobject-
classifier / topos-structure effects this entry records on a larger site remain **OUT OF SCOPE and
OPEN**; (ii) the result is **"at these declared seat-bundles, within this sensitivity envelope,"
NOT "the 6-point cohomology of this corpus"** — the sweep *bounds* robustness, it does not make the
finding bundle-independent, and does not eliminate the seat-bundle as a parameter. This measures
*this* corpus under a wider probe, **not** a forecast of a natively-6-authored corpus (that is Q2).

**Q2 (Ω_C — the genuinely deferred part).** Should a *future* corpus adopt 6 observer positions —
aligning the observer axis with the 6-atom authoring axis (so witness coverage and fingerprint
share a vocabulary)? Trade-off: finer observer resolution and a witness/observer axis that lines
up, against recalibrating every H¹-keyed threshold, re-identifying all fingerprints/orbits, and
losing comparability with the current 4-position corpus. Generation is the determinism frontier
(CLAUDE.md Critical Distinctions): a 6-position corpus is a NEW corpus, not a re-measurement of
this one, so Q2 cannot be answered by back-fitting — it is a build-the-corpus decision.

**What resolution would change.** `standard_context_for_power/2` gains 2 clauses (powerful,
organized context bundles); `site_contexts/1` enumerates 6; `fingerprint_shift/2` becomes 6-wide;
`config:abductive_hub_conflict_h1_threshold` and the h1_band cuts recalibrate; downstream: orbit
identity, MaxEnt over the type vector, the abductive hub_conflict gate. Cross-refs: OQ-108 (the
witness-coverage feature and the 4-vs-6 finding), OQ-107 (`future`, sibling deferral), the
subobject-classifier audit (4-point discrete site is trivially Boolean; non-linear/larger site is
the open question), `constraint_indexing:site_contexts_product/1` (the existing site-switch
precedent and its scope-exclusion calibration).

## OQ-132 — Python path consolidation: finish the migration + settle package-vs-`paths.py`

**Ω-type:** Ω_C (design choice — import ergonomics / repo structure).

**Status:** open — filed 2026-06-16. Phase 1 LANDED: `python/paths.py` is the canonical
source of truth (depth-agnostic `pyproject.toml`-marker root finder), the 3 hardcoded-`/home/scott`
absolute-path files fixed onto it, AGENTS.md §3 documents it + the byte-identical nested-script
bootstrap. Witnessed: paths.py resolves == the old hardcoded values; bootstrap finds the same root
from python/, audits/, sweeps/, tests/, a/b/c/, agent/.

**Priority:** 4

**The remaining work (held pending a ruling).** ~69 scripts still re-derive the root inline under
4 names (`REPO_ROOT`/`ROOT`/`REPO`/`BASE_DIR`) × 4 depth-dependent expressions. **Do not migrate
them yet** — the migration TARGET depends on an unsettled decision:
- **(A) Keep `paths.py` + the sentinel bootstrap** (current Phase-1 path). The bootstrap is
  depth-agnostic and copy-safe, so the fork is already killed for new code; migration is mechanical.
- **(B) Go full-package** (`pyproject.toml` currently has `[tool.poetry] package-mode = false`;
  flip it, add a package layout, `pip install -e .`) → `from rootpkg.paths import …` with **no
  bootstrap anywhere**. Dissolves the trap instead of relocating it, but changes how every script is
  invoked. The reviewer's load-bearing flag (2026-06-16): migrating 69 files onto a bootstrap you'd
  later delete under (B) is throwaway churn — settle A-vs-B FIRST.

**What resolution would change:** the ~69 inline derivations migrate to one import; the 4-name/4-depth
fork is fully retired (not just for new code). Cross-ref: AGENTS.md §3 (the documented canonical),
`python/paths.py`.

---

## OQ-133 — Confrontation-response signature: the diachronic tier that gives orientation (a deferred Ω_E) its only traction

**Ω-type:** Ω_E — deferred (orientation: cover vs survival vs defense). **Corrected 2026-06-16 from a
prior Ω_P mislabel** (resolved against `docs/omega_variables.md`'s own definitions, not the loose
uke_think Ω_P). Orientation is NOT Ω_P: Ω_P is a value judgment that differs *legitimately across
stakeholders* (resolved by those bearing the cost deciding), whereas orientation is a **fact about the
concealer's actual stance** — observers differ in *access*, not legitimately in *values*. Its named
resolution operation is *world-observation* (the longitudinal confrontation-response signature below =
the paradigm Ω_E operation), so it is a deferred Ω_E, status: awaits the t0-anchor tier. **The
mislabel was load-bearing:** routing orientation to Ω_P makes its resolution "someone bearing the cost
declares it" — which licenses the encloser to self-certify as a defender by fiat (the concealment move
blessed by the routing); the Ω_E routing withholds that license, forcing the verdict to be earned from
the honor/reabsorb pattern. **Boundary (the Ω_E claim's falsifier):** the signature tracks orientation
only absent strategic gaming — a sophisticated encloser can *perform* honoring (forge the longitudinal
witness as a deepfake forges the index); under gaming the operation fails and orientation falls
**outside the framework entirely** (`omega_variables.md` Mechanism Boundaries exclude strategic gaming)
— Ω_E in the non-gaming regime, out-of-framework under gaming, **never Ω_P.** Full typing rationale:
`docs/technical/build_discipline.md` → *When to stop verifying*.

**Status:** future — filed 2026-06-16. Deferred tier; gated behind the decay-only / off-live-path
problems in `docs/repair_dynamics.md` §6 and the temporal-series threads (OQ-83/OQ-109/OQ-110). Not
slated for near-term work, kept searchable + full-bodied so it can be revived when the t0-anchor
machinery is ready.

**Priority:** 5

**Deps:** blocked_on OQ-109, blocked_on OQ-110 (the temporal series / snapshot_type / transition_paths
machinery this tier would wire `founding_problem_status` into as the `t0` anchor); bundled_with OQ-83
(the synchronic `q6_crosscheck/3` establishes the `live × snare` structural footprint this tier would
then try to orient).

**The question.** The synchronic Q6 crosscheck (OQ-83 follow-through, landed 2026-06-16) sees
status × present-type but NOT the path and NOT why the mismatch exists. The tempting next tier is the
**trajectory** (origin→present movement) — but trajectory STILL underdetermines orientation: drift +
reabsorption is enclosure; drift + honoring is a seat correcting itself — same trajectory, opposite
orientation. The signal that actually discriminates orientation is the **response to confrontation**
(Corollary 3 — the t3 honor-vs-reabsorb move), one layer PAST trajectory. So the deferred tier's
target is the **confrontation-response signature**, NOT the trajectory. Naming it this way makes the
deferral principled (it is the only tier where orientation — the deferred Ω_E, see Ω-type above —
gets any traction: the tier where it is *witnessed* rather than *declared*) and tells the t0-anchor /
`transition_paths` machinery what it is FOR. A trajectory tier built without this would reach movement
and falsely believe it reached orientation.

**What resolution would change.** Gives the engine a witnessed handle on orientation (cover vs survival
vs defense) that the synchronic tier is forbidden by construction from delivering — converting
`live_claim_vs_snare_present` from "structural mismatch, orientation unwitnessed" into a tier that can
actually orient it via the confrontation response. Cross-ref: `docs/repair_dynamics.md` §6, the OQ-83
follow-through landing note, `audits/2026-06-16_q6_crosscheck_completion/`.

---

## OQ-134 — Corpus-wide Q6 crosscheck census (cell distribution as report commentary)

**Ω-type:** Ω_C (a reporting/annotation feature; commentary-grade, never classification — same genre as OQ-86).

**Status:** resolved — 2026-06-16. Built as a GENERIC commentary census (operator ruling:
build the generic exporter + wire it into `run_pipeline.py`, not a q6-special one).
`prolog/commentary_census.pl` (module + multifile `commentary_cell/3` hook + absence-bucket +
coverage-decidability declarations + `run_commentary_census/0`) computes per-source bucket
histograms over `corpus_loader:corpus_constraint/1`; `python/run_pipeline.py`
(`_prolog_commentary_census`, registered in the Phase-2 `tasks`) transports the `CENSUS*` lines
into `outputs/commentary_census.{json,md}` with a corpus-identity manifest. q6 is source #1; the
OQ-86 `extraction_reading` census is source #2 (delivers OQ-86's noted census follow-on in the
same mechanism). Witnesses: see RESOLUTION below.

**Priority:** 3

**Deps:** bundled_with OQ-86 (sibling commentary-reporting feature: surface an existing engine
computation as report commentary, never a classifier input); bundled_with OQ-83 (the `q6_crosscheck/3`
source this would aggregate).

**RESOLUTION (2026-06-16, witnesses).** Audit + raw output:
`audits/2026-06-16_oq134_commentary_census/`.
- **Hand-census cross-check (multiset, not byte):** automated q6 histogram matches the by-hand
  census in `audits/2026-06-16_q6_crosscheck_completion/WRITEUP.md` on EVERY named cell; the only
  diff is `q6_unmeasured` 19→20 and `n_corpus` 71→72 — exactly the +1 corpus growth since
  2026-06-16 (the new story authors no `founding_problem_status` → unmeasured; verified
  corpus_constraint=72, with_founding_status=52, without=20). `q6_unclassified=0` confirmed-empty
  on live.
- **plunit `tests/test_commentary_census.pl`: 20/20 pass** (`run_tests(commentary_census)`) — sum
  invariant Σ buckets == n_corpus AND n_corpus>0 (both sources); `q6_unclassified` ≠
  `q6_signature_unknown` (distinct keys, side-absent precedence); per-cell positive controls
  (fixtures land in dead/live/contested cells + both absence buckets); `commentary_cell` is
  deterministic; absence-bucket + coverage-decidability declarations; extraction_reading bivalues.
- **Cross-corpus self-labeling (fresh swipl/process per corpus):** twins reach `q6_unclassified`
  (haiku=1, flash=5, n=960 each) — the live 0 is corpus-specific, not universal; pre-stakeholder
  archives are the fail-closed control: `kernel_v1`(1106), `original_v5`(702), `original_v6`(3380),
  `testsets_sotu`(189) route 100% to `q6_unmeasured`, **zero named cells** — the census fabricates
  no verdict from absence (Pattern 6 honored).
- **Pipeline integration:** `python3 python/run_pipeline.py` → `commentary_census` task `ok`,
  `outputs/commentary_census.{json,md}` written; q6 coverage=0.611 (44/72), extraction_reading
  coverage=`null` at OQ-134 close (N/A — absence semantics UNRULED → spun out as **OQ-121**, now RESOLVED: extraction totalized, coverage 1.0 over its 50-constraint domain, prevalence 0.06). Classification
  byte-identical by construction: the census runs as its own swipl process, reads only, asserts
  nothing — not on the `dr_type`/json_report path (structural witness, same grade as the q6 audit).

**Extension point (for the OQ-86 census sibling and beyond).** A new commentary source is a
one-clause add to multifile `commentary_cell/3` (+ `commentary_source/1`, optionally
`commentary_absence_bucket/2` and `commentary_coverage_decidable/1`). The rest of the R3
commentary-grade family (`consensus_provenance/2`, `seat_perceived_vs_real/4`,
`mandatrophy_gap`) is future-cheap but no open OQ requests aggregating them today.

**Origin:** 2026-06-16, the Q6 sidecar wiring (`extract_q6_crosscheck` landed in `enhanced_report.py`
the same day).

**The question.** Per-constraint `q6_crosscheck` now reaches the report as a structured sidecar field
(`extract_q6_crosscheck`, `python/enhanced_report.py`). What is still missing is a **corpus-wide
census**: counts per named cell (`live_claim_vs_snare_present`, `dead_claim_vs_piton_present`, …) plus
the four non-verdict buckets kept SEPARATE (`q6_unmeasured` / `q6_signature_unknown` / `q6_unclassified`
/ out-of-domain), each with its denominator (N authored / with-block / corpus). This is the synchronic
plan's step-4 "coverage to the read site" at corpus scale; `report_generator` only does a per-constraint
`forall`, so a corpus-level aggregator/exporter is needed. **Hard constraints:** commentary-grade only
(never a classification input); report `q6_unclassified` and `q6_signature_unknown` as separate counts
(do not collapse — Build Discipline Pattern 6); `q6_unclassified: 0` on the live corpus is witnessed-0
(dr_type default_context = analytical surfaces no mountain/scaffold/naturalized) but corpus-reachable on
the twins — label it confirmed-empty for live only. Cross-ref: OQ-86 (sibling), OQ-83, the q6 sidecar
commit, `audits/2026-06-16_q6_crosscheck_completion/`.

---

## OQ-135 — Adopt v8 (seat/gauge/orientation) + machine-enforce the one-seat invariant

**Ω-type:** Ω_C (design-boundary / declared-seat — what the engine's ontology *is*; the adoption call and the spec's Q4 vocabulary are the operator's seat, not computed).

**Status:** open — blocked on operator (v8 adoption + spec Q4 vocabulary ruling) and the in-flight review of the design spec. Not workable until unblocked.

**Priority:** 3

**Deps:** blocked_on_human (operator must adopt v8 and rule the spec Q4 vocabulary question); bundled_with OQ-15

**Origin:** 2026-06-16, the seat/orientation invariant audit + R3 presentation-vs-structure probe (`audits/2026-06-16_seat_invariant_vs_prolog/`); design spec drafted rev1–rev3 (`docs/design/v8_seat_gauge_orientation_design_spec.md`).

**The work.** The audit concluded the engine **votes one seat** (R3 probe: `cs_pattern` tracks authored presentation, blind to binding structure; the `cs_verdict` false-X layer audits presentation against the metric reality, one-directionally). The v8 design spec states the resulting ontology (seat / gauge / orientation), draws the seat/face line by **audit direction**, and gives the standing invariant as a **transitive cross-axis taint property** (no committer field reaches observer computation by any path except entailment-typed payload on the single forward `influences` bridge). Spec §8 scopes the implementation; **priority-1 there is the one new artifact**: promote that invariant to a checkable **dataflow taint guard** with two positive controls (payload-injection on `influences`; (B)-seam-promotion off `influences`). The rest is low-stakes vocabulary migration. **NOT an engine rebuild — behavior-preserving** (the seat, gauge, and orientation machinery already exist; only the guard is new). See GAP-12 for the declared absence this closes (the invariant is prose-only today).

**What resolution changes.** The one-seat invariant goes from a v7 *prose* invariant (v7 §4.5; recorded as a decision in `docs/design/two_axis_architecture_v7.md`, OQ-14 resolved) to *machine-enforced*; and the v7→v8 "seat"="gauge" vocabulary becomes canonical (the spec §4 bridge table). Vocabulary migration must reconcile with **OQ-27** (H¹ signature-resolved-vs-raw phrasing) and **OQ-28** (seat-theorem-v1 honesty edits not all witnessed). Cross-ref: `audits/2026-06-16_seat_invariant_vs_prolog/REPORT.md`, KNOWN_STATE 2026-06-16 (seat/orientation audit + v8 spec).

---

## OQ-136 — What the commentary census measures: corpus authoring gaps vs genuine structural categories

**Ω-type:** Ω_E (measurable — cross-reference the census buckets against generation provenance and a bounded hand-read; a clustering test discriminates the two hypotheses) + a small Ω_C tail (per-bucket disposition is a ruling once the evidence is in).

**Status:** open — standalone, unblocked, do-whenever. The substantive payoff of the census (OQ-134/OQ-121): now that it reports honestly, its numbers are the first corpus measurements to interpret.

**Priority:** 3

**Deps:** bundled_with OQ-121 (the totalized census that surfaces these buckets); bundled_with OQ-134 (the census mechanism); bundled_with OQ-83 (q6 source), OQ-86 (extraction source).

**Origin:** 2026-06-16, the totalized commentary census. Once the absence / out-of-domain / unnameable buckets stopped collapsing into silence, several became visible as findings ABOUT THE CORPUS, not census defects. Live-corpus measurements (n=72, manifest-dated — re-witness, do not cite frozen):
- **`extraction_unnameable` = 5** — extractive constraint-level type, NO authored victim, AND no nameable extractor seat: BOTH sides of an extractive relation unnamed (the starkest blindspot, previously swallowed whole).
- **`q6_unmeasured` = 20 (28%)** — no `founding_problem_status` authored (R5 authoring gap).
- **`q6_signature_unknown` = 8** — a founding-problem block present but `dr_type` = unknown (computed side absent despite authored origin).
- **`consensus_provenance: no_agent_seats` = 21 (29%)** — no non-excluded agent seat exists.
- **`manufactured_consensus_candidate` = 8** — unanimity with a NAMED excluded seat (e.g. `demographic_resource_allocation` excludes `migrant_worker_households`; `refugee_convention_text__expansive_humanitarian_reading` excludes `restrictive_sovereigntist_governments`, `procedural_integrity_advocates`).

**The question.** Are these systematic **authoring gaps** (the de-leak generation pipeline under-specifies victim / founding-problem / agent-seat fields on certain topics) or **genuine structural categories** (situations where extraction really is diffuse, origin really is contested-into-unknown, no agent seat really exists)? The two have opposite dispositions: a pipeline artifact is fixed at generation (and may mint a generation OQ); a genuine category is kept and possibly named/reported as a first-class corpus statistic.

**Discriminating test (pre-registered).** For each absence/out-of-domain/unnameable bucket, cross-reference its member constraints against `story_provenance` (model, sampling params, prompt/schema/example commits) + run-tag + topic family. **Clustering by model / run-tag / topic ⇒ authoring artifact** (a generation-side gap, fixable); **spread roughly uniformly and corresponding on a hand-read to genuinely diffuse/contested situations ⇒ real category** (keep + report). Bounded hand-read of the 5 `extraction_unnameable` + the 8 `manufactured_consensus_candidate` (each names its excluded/unnamed party — check whether that party is genuinely absent from the situation or an authoring oversight). Caveat: generation is stochastic and the corpus is a small post-reset rebuild — re-witness the counts at run time against the manifest, and treat per-story provenance as the join key (never names across a regen boundary).

**DENOMINATOR CAVEAT (witnessed, `census_sweep.py` 2026-06-16).** Before reading any census *rate* (prevalence) or *coverage* across a config / schema-refit / corpus change: a rate can move PURELY by domain-shrink with NO change in the underlying finding. Witnessed: `tangled_rope_chi_floor` 0.35→0.85 raised extraction `prevalence` 0.060→0.067 (+12%) while `extraction_blindspot_fired` held at **3** — 5 extractive constraints just fell out of the domain (`n_in_domain` 50→45). Always report the raw `fired` count and `n_in_domain` ALONGSIDE the rate, or hold the domain fixed. Likewise q6 `coverage` decomposes into a config-INVARIANT authoring component (`q6_unmeasured`, fixed) and a config-VARIANT computational one (`q6_signature_unknown`, moves with thresholds) — do not read q6 coverage as a single authoring-completeness figure. The clustering test above must use raw counts per (model/run-tag/topic), never rates. Sweep tool + full findings: `audits/2026-06-16_census_sweep/`.

**What resolution changes.** Either a generation-pipeline authoring fix lands (if the gaps cluster — likely a new generation OQ), or a ruling that one or more buckets are genuine corpus categories worth first-class reporting (a seated dominance/statistic stage on the census). Either way the census's absence rates become INTERPRETABLE rather than raw counts. Cross-ref: OQ-121, OQ-134, `outputs/commentary_census.json`, `audits/2026-06-16_oq121_totalization/`, `python/sweeps/census_sweep.py` + `audits/2026-06-16_census_sweep/`.

---

*Last updated: 2026-06-16. Add new items with sequential OQ-NN labels. Mark
resolved items with a status change and a resolution note rather than deleting —
provenance matters.*

*Compress-on-close (added 2026-06-04): when an entry's status transitions to
resolved/disposed, compress its body in place — keep the header, Ω-type, the canonical
Status line, Origin, and a short resolution note with evidence pointers (commit hash,
KNOWN_STATE.md entry date, `audits/<date>_<slug>/`, witness files); drop the
investigation narrative (full history stays in git). Target ≤ 10 lines; exception: a
closed entry carrying a still-operative ruling or build spec (e.g. OQ-51's ruled-not-
yet-built 4th sheaf value) keeps that block intact. Entries are compressed in place,
never moved to a second file (single-tracker rule, Build Discipline Pattern 2), so
`grep OQ-NN ISSUES.md` keeps resolving every cross-reference. mitigated/partial
entries stay full-bodied — they are semi-live. `future` entries also stay full-bodied:
there is no resolution narrative to drop, and the full problem statement is exactly what a
reviver would need.*

*Status grammar (normalized 2026-06-04, machine-readable): each entry's first
Status line is exactly `**Status:** <token>` optionally followed by ` — <detail>`,
with token ∈ {open, investigating, mitigated, partial, resolved, disposed, future}.
`future` (operator ruling 2026-06-15) is a closing token for a REAL question that is
deliberately not slated for work — realistically won't get done — but is kept searchable
(`grep OQ-NN ISSUES.md` still resolves) and full-bodied so it can be revived if the
substrate changes. It is NOT in `omega_resolver.py`'s ACTIVE set, so a `future` entry drops
out of the workable frontier and the active counts; unlike resolved/disposed it carries no
resolution witness, so the rotted-witness check skips it.
Census: `python3 python/issues_status.py` (table + counts; pass a token to filter);
`--check` exits 1 on any malformed entry — run it after editing this file, and ALWAYS after
merging a worktree branch that touches it: since 2026-06-10 it also fails on duplicate OQ
labels, the artifact two parallel worktrees produce when both claim the same next OQ-NN and
automerge clean (pre-fix, the duplicate entry was silently invisible to census and checker).
One-liner equivalent: `awk '/^## OQ-/{oq=$2} /^\*\*Status:\*\* /{print oq, $2}' ISSUES.md`.*
