# Open Questions and Issue Tracker

Persistent tracker for unresolved questions surfaced by audits and correctness
work. Each entry records: origin, the specific question whose answer would close
the item, evidence so far, and what would change once resolved.

Statuses: **open** | **investigating** | **mitigated** | **resolved**

---

## OQ-01 — Rope gate Chi ≤ 0 bypass: intentional modeling or artifact?

**Ω-type:** Ω_C (design choice — modeling decision to ratify, guard, or record in logic.md).

**Status:** open
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

**Status:** open
**Origin:** Tranche 2 correctness pass, Phase 1 audit, May 2026.  
**File:** `prolog/testsets/conceptual_emergence_reading.pl`  
**UID:** `72c8aa61-6909-40a1-83ef-a460510f3b82` (verified present in file, 13 occurrences)

**Specific question:** Does `cs_is_metric_stable(conceptual_emergence_reading)` actually
succeed at runtime, making UID 72c8aa61 a live `cs_drift_mismatch` case?

**Evidence so far:** Architecturally, the conditions for mismatch are met —
`gap(axiom_overriding, substantial, false)` routes to axiom_foreclosure via the
attractor table. Whether DR sees the constraint as metric-stable at analytical context
has not been verified by running the engine and observing `cs_drift_mismatch/2` output
for this UID.

**What resolution changes:** Either confirms `cs_drift_mismatch/2` fires on real corpus
data for this UID (the predicate has been exercised end-to-end with a concrete case),
or reveals it as architecturally-possible-but-empirically-empty so far. The simplest
check: run `cs_drift_mismatch('72c8aa61-6909-40a1-83ef-a460510f3b82', Source)` in the
Prolog REPL after loading the CS schema and conceptual_emergence_reading testset.

---

## OQ-08 — DR/CS context asymmetry not surfaced in mismatch reports

**Status:** open
**Origin:** Tranche 2 audit Item 2.  
**File:** `prolog/cs_drift_mismatch.pl` (~line 52, "by design" comment)

**Specific question:** When `cs_drift_mismatch/2` fires, should the mismatch report
annotate that DR and CS are computing over different framing Π — DR at the fixed
analytical context, CS over context-free authored facts?

**Evidence so far:** The predicate's comment (~line 52) documents the asymmetry as by
design: "DR is instance-blind; two instances sharing C both see the same DR stability
result — by design." Reports that emit mismatch findings currently do not surface this
— a reader could interpret it as "CS and DR disagree about the same thing" when the
correct reading is "CS and DR are answering structurally different questions."

**What resolution changes:** One annotation line in the mismatch report section makes
the Π-difference visible at the report level, consistent with the declaration
discipline the schema runs on. No logic change required — this is a report-layer
change in `prolog/json_report.pl` or the Python report formatter. Low cost, high
clarity value for any external reader of mismatch output.

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

**Status:** open
**Origin:** Cross-axis comparison layer design pass, May 2026.  
**File:** `docs/two_axis_architecture_v7.md` (lines 41, 109, 116, 139, 143)

**Specific question:** The doc asserts in multiple places that the two axes are
"joined only by the `influences`-entailment bridge through `drl_composition`."
That claim was load-bearing under the prior architecture. The design decision to
unbless the bridge and route `influences` through the not-yet-built comparison
layer (alongside every other cross-axis edge) makes the doc's central claim
false. When does the doc get updated to reflect the new topology?

**Evidence so far:** The cross-axis inventory (OQ-15) found 16 predicates
crossing the axis boundary, only one of which is the blessed `influences`
bridge. Routing all 16 through a single named comparison/mediator layer was
agreed; the bridge stops being privileged and becomes one cross-axis read among
others. The doc's own thesis (line 17, "mark the drift") prescribes that
architectural decisions get recorded *in* the doc when they're made. Currently
the decision exists in conversation only.

**What resolution changes:** The doc either accurately describes the live
architecture (comparison layer is the sole sanctioned join, `influences` enters
it like any other cross-axis read) or remains stale on its central
architectural claim. The "Why they must not be unified" section needs the
rewrite; the "Open by design" section (line 109) needs the bridge demoted from
sanctioned-singleton to one-of-many. Until the doc is updated, the next reader
trusts a document that describes a topology the code no longer matches —
exactly the unmarked-drift failure the doc was written to prevent.

---

## OQ-15 — Cross-axis comparison/mediator layer: designed but not built

**Ω-type:** Ω_C (design choice — architecture decided, build deferred).

**Status:** open
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
**Origin:** Theorem 7 anchor verification, May 2026 (the precision note that changed v7's H¹=0 phrasing).  
**Files:** `prolog/cohomological_obstruction.pl` (or wherever orbit_vector is constructed); `prolog/drl_core.pl` (dr_type, integrate_signature_with_modal); `docs/deferential_realism_paper_v6.13.md` (theorem 2 statement)

**Specific question:** H¹ in this framework is computed over the signature-resolved dr_type orbit — the path cohomological_obstruction → orbit_vector → type_at_context → dr_type applies the structural signature before H¹ counts disagreement. Raw classify_from_metrics types may be — and for the prohibition anchor are — maximally heterogeneous ([naturalized, snare, rope, snare]) while signature-resolved types are uniform ([tangled_rope × 4]), yielding H¹=0. The v6.13 paper describes H¹ as "a partition functional over the classifications assigned by the observer positions" without specifying which classification — raw or signature-resolved. The v7 draft was loose at exactly the same point and had to be corrected. Does the v6.13 statement need a precision amendment to make the signature-resolution explicit?

**Evidence so far:** The verification run for the prohibition anchor produced the precision note: H¹ measures coherence of the signature-resolved orbit, not raw-type uniformity. v7 was revised to state both orbits explicitly (raw and signature-resolved), report them together as the corpus confirmation, and tie the result to Theorem 1 (the signature is the cover story). v6.13's Theorem 2 statement and the surrounding orientation prose use "classification orbit" without disambiguation; a careful reader running classify_at_time directly would get raw types and not know they don't match what H¹ counts. This is the same kind of silent precondition the FPN convergence proof carried before the run (an assumption stated in prose, never made code-visible).

**What resolution changes:** Either (a) v6.13 Theorem 2 is amended to specify "signature-resolved classification orbit" wherever "classification orbit" currently appears, and the engine carries a comment at cohomological_obstruction confirming the path goes through dr_type (signature-resolved), not classify_from_metrics (raw); or (b) the paper stands with the looser phrasing and v7 carries the precision as a v7-specific clarification. Option (a) eliminates the ambiguity at the source; option (b) leaves the source loose and patches it downstream. The latter is the pattern that produced the hanbali misattribution (v7 inherited v6.13's loose phrasing, then I wrote a theorem on top of it).

---

## OQ-28 — Seat Theorem v1.1 honesty edits batched but not all witnessed by runs

**Status:** open
**Origin:** Seat Theorem amendment work, May 2026.  
**File:** `docs/seat-theorem-v1.md` (now v1.1 with three pending edits)

**Specific question:** The Seat Theorem v1.1 batch contains three edits: (1) §3 correction against the FPN injection run — witnessed by test_forecloses_fpn_injection.pl, fully grounded; (2) §5 incompleteness-downgrade from claimed structural parallel to acknowledged analogy — not witnessed by any run, justified by the observation that the parallel to Gödel was never formally derived; (3) P3-locality clause appended to every self-sealing claim — also not witnessed by a run, justified by the observation that P3 is contestable and the theorem cannot defeat someone who rejects it. Two of three edits are arguments-from-the-chair while the third was an argument-against-the-chair-from-a-run. Is the asymmetry acceptable, or should edits (2) and (3) carry their own falsifiability discipline?

**Evidence so far:** Edit (1) is the model the build has been honing for months — abstract theorem corrected by a Prolog run, citation direction from engine to theorem. Edits (2) and (3) are corrections the theorem could in principle make to itself by inspection: §5 never claimed a formal incompleteness derivation, so downgrading it to analogy is closing a gap the prose opened; P3 is explicitly contestable by direct realists, so localizing the theorem's reach is making explicit what was already true. Neither requires a run because neither claims a measurable property — they're scope clarifications, not result claims. But the asymmetry matters for how the document is read: a reader who sees one witnessed edit and two unwitnessed ones may wonder why the unwitnessed ones aren't also conditional on a test.

**What resolution changes:** Either (a) edits (2) and (3) are clearly marked as scope-clarifications rather than result-claims, so the asymmetry is intentional and on the record (the witnessed edit corrects a claim; the unwitnessed edits narrow scopes and don't need run-grounding for that reason); or (b) edits (2) and (3) acquire their own falsifiability — what would falsify the analogy-not-derivation framing of §5, what would refute the conditionality on P3 — and the document carries those alongside the §3 witness. Option (a) is the honest distinction; option (b) is overkill but would make the document fully symmetric. The asymmetry is acceptable as long as it's named, which it currently isn't.

---

## OQ-29 — Results stale against a moved corpus (general form of OQ-25 §5.11 scope)

**Status:** open
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

**Status:** investigating — 2026-05-30
**Origin:** Fabricated-default inventory session, 2026-05-30. Tripwire graduated 2026-05-30.  
**Files:** `prolog/drl_composition.pl:179` (temporal fallback), `prolog/drl_core.pl:96`
(static fallback — DORMANT, see below), `prolog/testsets/*.pl` (223 testsets,
suppression_requirement measurement absent in all).

### Witnessed (execution-witnessed — tripwire run 2026-05-30)

**Fabricated-default on temporal path (D1a, LOAD-BEARING-WRONG):**  
`suppression_requirement` measurement is absent in all 223 testsets (grep confirms 0 facts).
`classify_at_time` (`drl_composition.pl:179`) has no authored value to read and falls back
unconditionally to `Supp=0.5` — fires on 100% of the temporal path (647/647 measurement rows).

**Tripwire confirmation (execution-witnessed, 2026-05-30):**
Source-patch `Supp=0.5` → `Supp=999.9`, run `constraint_history` over full corpus:
- 279/647 temporal rows changed
- 219 tangled_rope → snare
- 60 unknown → snare
- **0 → unknown** (plan's instance-reported claim of 443 unknown flips was WRONG)

The mechanism: `snare_suppression_floor=0.60`. Fabricated `Supp=0.5` falls below this
floor. Every temporal row that would otherwise be `snare` is demoted to `tangled_rope`
(if `tangled_rope_suppression_floor=0.40 ≤ 0.5`) or `unknown`. 50.4% of non-unknown
temporal classifications (279/553) are misclassified — systematically too low, not random.

**Correction to prior instance-reported claim:** The "443/519 non-unknown classifications
flipped to unknown" (in prior session writeup and original OQ-33 text) was INCORRECT.
That claim assumed poisoning would push unknown-ward; actual behavior is the opposite —
higher Supp enables snare. The tripwire run supersedes the instance-reported estimate.

**Cross-surface fabrication (D2, DORMANT — corrected from LOAD-BEARING-WRONG):**  
The static path (`get_raw_suppression`, `drl_core.pl:96`) fabricates the same missing
metric as `0`, not `0.5`. BUT: the 32 testsets lacking `suppression_requirement` are the
`_contradictions.pl` stubs — not classified constraints. `all_corpus_constraints/1`
excludes them (requires `constraint_metric(C, extractiveness, _)`). Tripwire run:
`Value=0` → `999.9` produced **0 changes** across 191 classified constraints.

| Surface | Fallback | fires-now (actual corpus) | verdict |
|---------|----------|--------------------------|---------|
| Temporal (D1a) | Supp = 0.5 | 647/647 temporal rows | LOAD-BEARING-WRONG |
| Static (D2) | Supp = 0 | 0 classified constraints | DORMANT |

The cross-surface asymmetry in OQ-33 prior text described a real difference in fallback
values but overstated D2's impact: D2 fires on contradiction stubs only, not on the
classified corpus.

**D20/D21 Boltzmann sites (DORMANT — corrected from UNSURE):**  
Tripwire run: `BaseEps=0.5` → `999.9` and `Supp=0` → `999.9` in `classify_at_context_impl`
produced **0 changes** across 191 classified constraints. Same mechanism as D2 — the 32
affected testsets are contradiction stubs, excluded by `all_corpus_constraints/1`.

Audit writeup: `audits/2026-05-30_authoring_closure_fabricated_defaults/audit_authoring_closure_fabricated_defaults.md`

### Resolution options (design decision — no verdict asserted here)

**(a) Author temporal suppression_requirement measurements into testsets.** Fills the
missing data at source; both surfaces would then read the same authored value. Cost:
authoring burden on 190+ testsets; requires schema guidance on what a correct
suppression_requirement measurement looks like.

**(b) Align `classify_at_time` to `get_raw_suppression` fallback (Supp=0).** Eliminates
the cross-surface asymmetry. **Cost: collapses the Surface-1 / Surface-3 distinction that
the three-surface model exists to hold.** If the temporal surface uses static-surface
fallback values, the two surfaces are no longer observationally independent on
missing-data constraints. The three-surface model's cross-surface divergence signal
becomes uninterpretable for any constraint where suppression_requirement is absent —
which is currently all of them. Flag this cost explicitly before choosing (b).

**(c) Formalize "temporal surface returns indeterminate when it lacks its own data."**
Accept that `classify_at_time` returns `unknown` for constraints without authored temporal
suppression. The 443 extant `tangled_rope` emissions are reclassified as either policy
decisions (if the engine should treat unmeasured suppression as 0.5) or repair targets
(if they are simply wrong). Makes the fabrication visible and opt-in rather than silent.

### Blocks

1. **Surface-3 perturbation primitive** (`constraint_history/3`) — premature until
   resolved. Perturbing a surface whose baselines are fabricated measures noise against
   noise: the primitive would compare fabricated-Supp classifications against
   perturbed-param fabricated-Supp classifications. No clean signal until the fabrication
   is either authored away (a) or formalized (c).
2. **Validity of the 443 extant temporal classifications** already in the corpus and any
   report that cites temporal-surface outputs as independent evidence.
3. **Cross-surface divergence interpretation** — the static-0/temporal-0.5 sub-finding
   means any existing analysis that attributes static/temporal divergence to observational
   difference rather than filler asymmetry is compromised for suppression-absent
   constraints (currently all constraints).

See `docs/technical/build_discipline.md` Pattern 4 (fabricated default) for the
defect class.

---

---

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
`internalization_depth` (`psych_bridge.pl:19`), `resistance_to_change`
(`data_validation`/`json_report`/`utils`), `accumulation_speed` (`utils.pl:211`, explicit 0.0
default) — all 0/0 both corpora, none compiler-emitted. Plus compound measurement metrics
`accessibility_collapse(Level)`/`stakes_inflation(Level)`/`suppression(Level)` read by
`coercion_projection.pl:15` but never emitted (compiler emits `measurement/5` only for
theater_ratio/base_extractiveness/suppression_requirement). **Decision per metric:** author it,
or remove the dead read. **Low-stakes each.**

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
- Row 14 scaffold "suppression must decline over time": **no trajectory check** exists; scaffold
  uses scalar `Chi` + `has_sunset_clause`. **Decision:** add a trajectory gate or drop the rule.
- Row 15 "final measurement = base extractiveness": unenforced (no validator). **Low-stakes.**
- Rows 16–18 (piton atrophy / Goodhart / perspective-min): narrative-only, committer-only, or
  schema/linter-enforced respectively — likely no engine action.

**Resolution would change:** whether the prompt's temporal rules are real engine constraints.

## OQ-40 — G5: scalar-vs-temporal representation splits

**Ω-type:** Ω_C (design choice — authoritative representation per metric, or document the axis split as intended).

**Status:** open — Census rows 19–22. `extractiveness`, `base_extractiveness`,
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
scalar-fallback fix **converges them** (both now: temporal → authored scalar → floor). NB this
convergence depends on the row-23 **stopgap**; if OQ-46 removes the scalar fallback without the
generation template first authoring a temporal series, the split reopens. The `extractiveness`/
`base_extractiveness` sub-splits (rows 19–20) remain open.

## OQ-41 — G6: fabricated defaults for absent data (fail-closed vs impute)

**Ω-type:** Ω_C (design choice — fail-closed vs impute; subsumed by the OQ-44 satisfy-on-absence policy).

**Status:** partial — row 23 MITIGATED (2026-05-31, Commit A); rows 24–27 open (row 26 NEUTRAL for 3 of 6 sites — 4 OPEN, see coverage correction below). Census rows 23–27. A silent
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
  **The scalar fallback is a labeled STOPGAP** — see **OQ-46** (generation-template fix retires it).
- Rows 24–25 `BaseX=0.5` / extractiveness→0.5: latent (tripwire artifact: 0 changes; extractiveness
  required-authored).
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

**Status:** open — Policy decision should be made once across the class, not per site.

A recurring structural pattern, now seen in four places: an engine gate is **satisfied by the
absence of data**, so it passes vacuously and the result reads as a positive finding when it is
really an unauthored blank. The gate looks like it discriminates; on the current corpus it cannot,
because its input is empty or defaulted. Naming the class so the adjudication happens once:

- **G6 fabricated defaults (OQ-41).** `Supp=0.5` / `BaseX=0.5` substitute for absent authored
  data; the engine computes on a value nobody authored. Fail-closed vs impute.
- **Empty `intent_*` tables (OQ-36, OQ-37).** The `intent_*` family is genuinely empty corpus-wide;
  `forall(...)` and findall-driven consumers succeed vacuously.
- **`get_metric_average:160` default `0.5`** for metrics with no `constraint_metric` rows.
- **NL beneficiary gate (this item, 2026-05-31 gap check).** `natural_law_signature`'s
  `BeneficiaryCount == 0` (`signature_detection.pl:295`) reads `count_power_beneficiaries`, which
  joins `affects_constraint` x `intent_power_change`. `intent_power_change` is empty corpus-wide
  (**0 facts** measured on testsets_3000), so `BeneficiaryCount == 0` holds for **every** constraint
  by absence, not by checking. The gate is dormant-over-empty-table, not discriminating.

**NL-specific evidence (gap check, testsets_3000, 3380 constraints):**
- 404 constraints carry the `natural_law` engine signature (Pattern-3 unbound query + post-filter).
- **0/404** carry a `constraint_beneficiary/2` fact (the corpus holds **6739** such facts, none on
  the 404). FSM coverage of the NL population is therefore empirically **0/404 by cascade
  construction**: `false_summit_mountain` (`:87`) is checked before `natural_law` (`:97`) and
  requires `Beneficiaries \= []`, so it catches every beneficiary-bearing mountain first — the NL
  residue is exactly the beneficiary-blind constraints. FSM is **not** belt-and-suspenders backup
  for the NL gate; the `:84-86` source comment claiming so was corrected on 2026-05-31.
- **0/404** carry an `intent_power_change` beneficiary (the gate's own source is empty).

**What the 404 NL certifications currently mean:** "no beneficiary **authored**," not "no
beneficiary **exists**." The engine-insensitivity result stands (T.1: all 404 are bucket-A
metric-real mountains, eps 0.00-0.22, supp 0.00-0.04) — the engine will not manufacture beneficiaries.

**Decision (one policy across the class):** for each satisfy-on-absence gate, choose
fail-closed (require the datum before the gate may pass) vs keep-vacuous-pass (accept that the gate
is inert on the current corpus and document it as such). **Activating the NL beneficiary gate is a
content re-audit of the 404** (do any actually have asymmetric winners hidden behind an emergence
claim?), **not engine maintenance** — populating `intent_power_change` faithfully for genuine
natural laws yields zero beneficiaries and zero flips; it only bites a mis-authored false-natural-law.
**Connects to OQ-41 (G6), OQ-36/OQ-37 (empty `intent_*`).**

**Fifth instance (verified 2026-06-05, triage Item 2):** `signature_detection:has_viable_alternatives/2` defaults `false` on the empty `intent_viable_alternative/3` table, and `natural_law_signature` requires `HasAlternatives == false` — the absence SUPPORTS NL certification (pass-open). The sibling BeneficiaryCount gate was re-sourced (D3 fail-close); this one was not. Fail-closing it would un-certify every NL constraint until the intent layer is fed or the gate re-sourced — an output-changing design call not yet made. See `design_gaps.md` GAP-08.

## OQ-44 — Engine-wide audit: no gate may be satisfied by absence (authored-zero vs absent)

**Ω-type:** Ω_C (design choice — the engine-wide fail-closed-on-absence policy; decided once for OQ-41/36/37/43).

**Status:** open — (audit task). Generalizes OQ-43; gating policy for the whole satisfy-on-absence
class (OQ-41, OQ-36/OQ-37, OQ-43).

**Premise:** the engine must distinguish "authored to be zero" from "absent" everywhere, and
never let absence satisfy a gate. Zero-because-measured and zero-because-missing collapse to the
same value at a comparison site, so a gate that cannot tell them apart is testing nothing whenever
its source table is empty. The NL beneficiary gate (OQ-43) is one confirmed instance; this OQ asks
whether there are others, and fixes the policy once.

**What to audit.** Enumerate every engine gate of these shapes and, for each, record whether its
driving data is non-empty in the active corpus:
1. Equality/threshold over a `findall` count — `Count == 0`, `Count =< K`
   (e.g. `count_power_beneficiaries`, `signature_detection.pl:165`).
2. Threshold over a metric that defaults on absence — `V =< Ceil` where `V` comes from a
   `( ... -> V = Measured ; V = Default )` fallback (cross-link Pattern 4 / OQ-41 sites:
   `get_metric_average:160` `0.5`; `classify_at_time` `Supp=0.5`; `get_raw_suppression` `Supp=0`).
3. Universal quantifier / negation-as-failure over a possibly-empty table — `forall(P, Q)`,
   `\+ disqualifier(C)` (e.g. `data_verification:verify_interval_completeness`;
   `natural_law_without_beneficiary/1` guards in `drl_core.pl`).

**Method (per gate):** measure the source-predicate fact count on the active corpus (the Pattern 5
diagnostic in `build_discipline.md`). Verdict per gate:
- **DISCRIMINATING** — source non-empty, gate distinguishes pass from fail on real data.
- **VACUOUS-PASS** — source empty / always-defaulted, gate passes by absence for all constraints
  (record the count, as OQ-43 did: `intent_power_change` = 0 facts → `BeneficiaryCount == 0`
  universal).
- **DEFAULT-PASS** — source absent but a fabricated default clears the gate (Pattern 4 overlap).

**Decision (one policy across the class):** for each VACUOUS-PASS / DEFAULT-PASS gate, choose
**fail-closed** (the gate may not pass unless the datum was authored — distinguish `\+ exists`
from `authored(0)`, e.g. via an explicit authored-zero marker or a non-empty-source guard) vs
**keep-vacuous-pass** (accept the gate is inert on the current corpus and document it at the gate).
The choice is the same adjudication OQ-41 frames for fabricated defaults; make it once for both
fabricated-value (Pattern 4) and satisfied-by-absence (Pattern 5) gates rather than per site.

**What resolution changes:** turns "no disqualifier authored" certifications (e.g. the 404 NL
mountains) into either honestly-conditional findings (gate documents its own vacuity) or
genuinely-checked findings (gate fail-closed, requires the table populated). Connects to
OQ-43, OQ-41, OQ-36/OQ-37. Build-discipline Pattern 5 records the pattern and diagnostic.

## OQ-45 — Content audit: do any of the 404 NL constraints hide asymmetric winners?

**Status:** open — (corpus-quality audit, NOT engine maintenance). Spun off from the D3 ruling so the
wiring fix (NL beneficiary gate fail-close, Commit B1) and the content question stay separate. The
gate fail-close makes "no beneficiary authored" honestly-conditional rather than a vacuous pass; it
does **not** decide whether any of the 404 natural-law certifications are *mis-authored* false-naturals
with a real winner hidden behind an emergence claim. Populating `intent_power_change` faithfully for a
genuine natural law yields 0 beneficiaries / 0 flips (OQ-43), so this audit only bites mis-authoring.
Audit the 404 on their own merits; do **not** populate `intent_*` as maintenance. Connects to OQ-43.

## OQ-46 — D4-for-suppression is a GENERATION-TEMPLATE requirement; it retires the row-23 stopgap

**Status:** open — Sequencing constraint for the row-23 fix (OQ-41). The row-23 fix
(`classify_at_time`) currently bridges absent temporal `suppression_requirement` to the authored
**scalar** value — a **labeled stopgap**, not a sanctioned second representation. The real fix is
**upstream, in generation**: the story template must author a *temporal* `suppression_requirement`
series. **Partially progressed by the 2026-06 rebuild:** engine-measured on the live corpus, the
template now authors a temporal series for **471/562** constraints; **91/562** remain scalar-only and
still hit the stopgap (0/562 reach `unknown`). (Pre-rebuild this was inverted — 650/656 rows had only
the scalar.) This is a **generation-template requirement, not an engine representation ruling** — i.e.
D4 (G5 scalar-vs-temporal) for suppression is resolved by authoring the series, not by the engine
choosing a representation. **Once the template authors the series for the remaining 91: delete the
scalar-fallback clause in `classify_at_time` and let the temporal path stand alone** (it is still
load-bearing for those 91 — do not delete early). Do **not** build a scalar/temporal equivalence check on the bridge — skip it; the
bridge is temporary. Sequenced: this rides the regeneration arc (OQ-47), not Commit B.

**Post-reset check (2026-06-05):** the live corpus' first 20 stories author a temporal `suppression_requirement` series **20/20** — the generation-template requirement lands universally under the de-leaked prompt. Once the live corpus accumulates with this holding, the row-23 stopgap (`classify_at_time` scalar bridge) is retirable + fail-closed `unknown` added (output-changing engine edit; needs its own witnessed pass).

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

**Status:** open — escalated for (a)/(b) ruling. Read-only audit; no clause removed.

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

**Status:** partial — open (two follow-ups). The core detector bug is RESOLVED — see KNOWN_STATE.md
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

---

## OQ-58 — Dangling cs_reading_relation targets: edges naming a reading that does not exist

**Ω-type:** Ω_P (content decision). **Disposition policy ruled 2026-06-02 (below); the per-edge
missing-vs-typo-vs-noise sort is a later narrative-read pass, not a mechanical rule.**

**Status:** partial — policy ruled; narrative-read pass run on the 13 forecloses (6 repaired); the residue is a
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

---

## OQ-70 — FNL fires on template-authored bait perspectives: FNL prevalence measures authoring convention, not detection

**Ω-type:** Ω_E (corpus measurement), with an Ω_C committer-axis edge (same family as OQ-65).

**Status:** resolved — operator ruled option A as the CLASS (2026-06-05): no signature may read a single authored perspective as a story-level claim. claimed_natural source 2 AND appears_as_rope's sibling clause removed (commit `72ec2cdd`). Witness on the live 20: FCR 16→5 (remaining are low-ε profile-driven), FNL 3→1; POSITIVE CONTROL manpower_exhaustion_trap (explicit claim-mountain, non-compliant) still fires FNL via source 1 — wrong path removed, detector intact. Signature prevalence is citable as a claims statistic from the rebuild's story 1.
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

**Status:** open — evidence re-witnessed 2026-06-05; ruling deliberately not made
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

## OQ-75 — Stage-2 corpus rebuild under the de-leaked pipeline: diff distribution + cross-axis invariance correlation

**Ω-type:** Ω_E (the staked prediction is measurable).

**Status:** open — gated on operator go; Stage-1 single-example gate passed 2026-06-05 (KNOWN_STATE.md 2026-06-05)
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
correlation readout (OQ-76 Remaining).** OQ-70's bait confound
is still live — FNL prevalence in a rebuilt corpus is not a detection result until OQ-70 is ruled.

**Power tiers for the readouts (recorded 2026-06-05, before the data — pre-register criteria at each tier BEFORE looking):** Tier 1 machinery shakedown ~100 stories (~15 topics; pipeline-holds criterion only, no verdicts; e-digit grid needs >=5 expected/cell). Tier 2 diff-distribution verdicts ~250-300 stories (proportions to +-4-5pp; per-claimed-type cells need ~30+ each). Tier 3 cross-axis correlation ~100-150 KERNELS (~500-600 stories; r=0.3 at 80% power needs ~85 kernels, r=0.2 ~195; the unit is the kernel — only kernels carry the axiom axis and construction pair; bounding r away from 1 is cheap and lands by Tier 2). Tier 4 threshold recalibration (OQ-48) ~700+ (old calibration used 691). e-statistics stay scoped to this one generation regime (OQ-26); never mix archived corpora into denominators.

## OQ-76 — Kernel/flat gate is stochastic on a real boundary band: routing noise lands in Stage-2's cross-axis correlation

**Ω-type:** Ω_E (the band is measured; the cause is K2-testable), with an Ω_P edge (which fix, and whether the interim hedge ships before Stage-2, are operator rulings).

**Status:** mitigated — generate-both (operator-promoted to PRIMARY fix, 2026-06-05) is BUILT and witnessed end-to-end; remaining: Stage-2 readout stratum (analysis-side) + optional K2 cause probe
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

**Status:** open — caught and non-blocking (pipeline completes 40/41); hypothesis is a concurrency race, not a deterministic graph-topology bug
**Origin:** 2026-06-06 slow-build session. Three topic runs launched CONCURRENTLY against the
single flat `prolog/testsets/` + shared `outputs/`. The `giant_comp` step (`_prolog_giant_comp`,
`run_pipeline.py:416`; swipl on `giant_component_analysis.pl`) segfaulted (`rc=-11`) on exactly
ONE of the three — the leiden run (n=39), whose pipeline ran at manifest 00:10:49Z, BETWEEN the
other two (embryo 00:08:36Z, debt 00:12:46Z), i.e. maximally overlapping — and succeeded at n=43
and n=33. The non-fatal warning `Local definition of drl_core:base_extractiveness/2 overrides
weak import from constraint_data` precedes the crash but is present on passing runs too (red
herring).

**Hypothesis (to be tested, not asserted):** the crash is a SYMPTOM of concurrent swipl
invocations racing on shared `outputs/` artifacts (orbit data, temp files), not an intrinsic
defect in `giant_component_analysis.pl`. Evidence for: appeared only on the maximally-overlapping
run; intermittent; a 0-exit-code segfault on otherwise-valid corpora is the shape of a
file/resource race, not a logic bug.

**Kill-condition (closes the OQ):** run the same N topics SERIALLY (or generate-only then one
`run_pipeline`). If the segfault does NOT recur across several serial runs at comparable corpus
sizes, it is a concurrency artifact → resolution is the operational rule "do not run concurrent
full pipelines against shared testsets/+outputs/" (the slow-build workflow), not a Prolog fix.
If it DOES recur serially, it is a real defect in `giant_component_analysis.pl` (likely stack
depth / unbounded recursion on a particular graph topology) → investigate the analysis itself.

**Triage-list membership (build_discipline.md citation-time rule):** this OQ is the rung-4 premise *"the live corpus / pipeline_output denominator is current"* failing — concurrent runs race the shared denominator, so any single run's pipeline_output is not a coherent snapshot. The same-run guard catches the orbit/pipeline mismatch; it does not catch two full pipelines interleaving. Do not cite a corpus statistic from a concurrently-built run as settled.

**Why it can wait:** `run_prolog` catches it (`[WARN] giant_comp ... non-critical, continuing`)
and the pipeline finishes 40/41; `giant_component_analysis.md` is simply not refreshed that run.
But a SIGSEGV is never "fine" — logged so the kill-condition is run before trusting the
giant-component output on any concurrently-built corpus.

## OQ-78 — The ε authoring idiom: a 0.68 mode + 8/2 last-digit grid compresses the ε perturbation axis

**Ω-type:** Ω_E (a measurement caveat on the corpus, quantifiable and trackable as it grows).

**Status:** open — measurement caveat, not a defect; NOT a leak (no author-facing surface discloses
0.68; the de-leak's assembled-payload grep is clean). Watch as the corpus grows.
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

## OQ-79 — c-orchestrator recognizes kernels but silently drops their readings; flat-entry topics never engage the kernel question

**Ω-type:** Ω_E (the loss is witnessed and counted) + an Ω_P design call (should the flat-entry path hold the kernel-vs-flat perturbation at all, or delegate).

**Status:** mitigated — silent loss is now loud + ledgered; the full fix (deliver or delegate recognized kernels) is an open design call
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

---

*Last updated: 2026-06-05. Add new items with sequential OQ-NN labels. Mark
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
entries stay full-bodied — they are semi-live.*

*Status grammar (normalized 2026-06-04, machine-readable): each entry's first
Status line is exactly `**Status:** <token>` optionally followed by ` — <detail>`,
with token ∈ {open, investigating, mitigated, partial, resolved, disposed}.
Census: `python3 python/issues_status.py` (table + counts; pass a token to filter);
`--check` exits 1 on any malformed entry — run it after editing this file.
One-liner equivalent: `awk '/^## OQ-/{oq=$2} /^\*\*Status:\*\* /{print oq, $2}' ISSUES.md`.*
