# Open Questions and Issue Tracker

Persistent tracker for unresolved questions surfaced by audits and correctness
work. Each entry records: origin, the specific question whose answer would close
the item, evidence so far, and what would change once resolved.

Statuses: **open** | **investigating** | **mitigated** | **resolved**

---

## OQ-01 — Rope gate Chi ≤ 0 bypass: intentional modeling or artifact?

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
**Resolution:** v6 authored 2026-05-28. See `docs/observers_not_humans_v6.md`.  
**Witness files:** `outputs/alt_power_transform_results.json`, `outputs/range_sweep_results.json`

**Evidence resolved:**

- H0 conditionally confirmed: sign-flip is load-bearing when Hub 1 spans the snare
  gate AND rope-gate bypass behavior is treated as given. Tangled_rope shows T4 gap
  +0.21 vs snare+rope +0.014 (14.6× concentration).
- H1 eliminated: Arm B is range-inert (max drop 0.099, non-monotone across span
  1.70→1.20→0.85 with Hub 1 live throughout).
- H2 rejected: T3 smoothness gradient non-monotone (0.886→0.782→0.827→0.145;
  gradient erasure, not smoothness, is the variable).
- H3 resolved: existing controls (piecewise_no_flip, sigmoid_shifted) ARE confounded
  as diagnosed; T1 pair is the clean test.

**What was changed:** v6 corrects v5's interpretation of the alternative-functions
robustness test. The empirical numbers reproduce (Jaccard 0.685–0.828 within the
full-corpus rerun 0.697–0.833), but sign-flip is load-bearing only in tangled_rope,
not corpus-wide. The universality-class claim is localized to the constraint family
where institutional beneficiaries sit below d_zero. §2.3 and §3.3 are resolved as one
finding (institutional sign-flip mechanism producing both the robustness and the
[+,+,−,+] chi pattern), not two independent findings. The rope-gate bypass (OQ-01)
is flagged as a conditional assumption whose resolution would clarify the mechanism's
theoretical status. Witnesses cited per seat-theorem §3 Correction style.

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
**Origin:** alt_power_transform full-corpus run (testsets_3000, 3380 constraints), May
2026.  
**Resolution:** v6 corrected the claimed range to 0.697–0.833 (2026-05-28).

**What was changed:** V5 §2.3 claimed Jaccard 0.685–0.828. The full-corpus rerun
(3,380 constraints, testsets_3000) produced 0.697–0.833. Four of six original variants
fall within the corrected range; sqrt_flip (0.833) and quadratic_flip (0.830) now sit
at the upper end rather than above the ceiling. The shift reflects corpus-snapshot drift
(the original range was computed on an earlier snapshot; the full 3,380-constraint run
was the first test on complete testsets_3000). Within acceptable tolerance. V6
accordingly uses the full-corpus range (0.697–0.833) as the empirical witness.

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

**Precondition:** Gap A (OQ-04 / PRIORITIES.md item 2) must be closed first — the
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

**Status:** open  
**Origin:** AUDIT.md finding W3, 2026-02-28.  
**File:** `prolog/config.pl`; `prolog/config_schema.pl`

**Specific question:** `logic_engine` and `version` are declared as `param/2` facts in
`config.pl` but are referenced nowhere in executable Prolog code and have no entry in
`config_schema.pl`. Are they safe to remove?

**Evidence so far:** Audit confirmed both are unreferenced in executable `.pl` code
(`audit_data/config_params_unused.txt`). They are non-numeric atoms, invisible to the
±25% sensitivity sweep by construction. 38 other unused params are in `config_schema.pl`
(documentation-only); these two are not even there. Audit verdict: "Confirmed removable."
Removal requires coordinating `config.pl` and `config_schema.pl`; no Python scripts
reference them by name.

**What resolution changes:** Reduces config noise. Establishes a precedent for cleaning
up future dead params. Low risk, ~5 minutes of work.

---

## OQ-12 — `.env` not in `.gitignore`

**Status:** open  
**Origin:** AUDIT.md finding §6 security review, 2026-02-28.  
**File:** `.gitignore` (repo root)

**Specific question:** `.gitignore` does not contain a `.env` entry. No `.env` file
currently exists, but if one is created (e.g., for local API key storage) it would be
committed silently.

**Evidence so far:** Audit confirmed the omission. All current API keys are sourced from
`os.environ` or `st.secrets` — no hardcoded credentials exist. The risk is latent, not
active. Audit recommended adding `.env` to `.gitignore` as a one-minute preventive fix.

**What resolution changes:** Eliminates a credential-leak risk if a `.env` file is ever
created locally. One-line fix.

---

## OQ-13 — Four pylint E-level errors in Python code

**Status:** open  
**Origin:** AUDIT.md §5 pylint summary, 2026-02-28.  
**Files:** `python/classification_confidence.py:370`;
`agent/orchestrator.py:1052`; `agent/perspective_experiment.py:796` (and one other)

**Specific question:** Are the four E-level pylint errors genuine bugs or safe dead code?

**Evidence so far:** Audit identified:
1. `classification_confidence.py:370` — `all_metrics_by_id` undefined variable. Line 370
   uses it in `if 'all_metrics_by_id' in dir()`, immediately overwritten at 372. Dead code
   from a refactor — harmless.
2. `agent/orchestrator.py:1052` — `topic` possibly used before assignment. Safe in
   practice: `parser.error()` at line 1043 exits if no topic source, so line 1052 is only
   reached when `topic` is bound.
3. `agent/perspective_experiment.py:796` — `constraints` possibly used before assignment.
   Needs inspection to confirm safety.
4. One additional possibly-used-before-assignment (file not recorded in audit notes).

Overall pylint score at audit time: 9.07/10.

**What resolution changes:** Confirms or eliminates latent bugs. Items 1 and 2 appear
safe to close with a comment or minor refactor. Item 3 needs a read before closing.
None are blocking; all are low priority.

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

**Status:** open  
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

**Status:** open  
**Origin:** FPN convergence-test run, Branch E verdict, May 2026.  
**Files:** `prolog/drl_purity_network.pl` (constraint_neighbors/3, compute_edge_contamination/7); `prolog/test_forecloses_fpn_injection.pl` (Case 2 — coexists_with_label_blindness)

**Specific question:** The architecture note's claim that coexists_with's contamination weight is "zero by definition" is — per the run — an unimplemented design intent, not a mathematical property. The FPN is label-blind; injecting coexists_with as an affects_constraint fact produces non-zero contamination identical to any other edge with the same purity delta. Unlike forecloses (which is structurally inert in its semantically correct direction, gradient-orthogonal to the network — genuinely unrepresentable regardless of code), coexists_with is structurally admissible (just a scalar) and excluded only by the fact that nothing currently routes it into constraint_neighbors/3. Should the exclusion remain documented-only, or should an edge-type guard be added so the decoupling is enforced rather than incidental?

**Evidence so far:** The FPN injection test (Case 2) ran an identical injection to forecloses_1b and got identical scalar flow. The architecture note has been rewritten to claim only what each edge earned: forecloses excluded structurally (gradient-orthogonality demonstrated), coexists_with excluded by unenforced design intent with constraint_neighbors/3 named as the actual gap. No live code path currently routes a coexists_with edge into the network, so the gap is latent — but the next code change that does will inject as a label-blind scalar and nothing will catch it. This is the same shape as the chimera latency (OQ-related — pre-cleanup, the chimera was inert until the next cs_kernel_id addition would have made it live): documented-and-inert versus enforced-by-code.

**What resolution changes:** Either (a) the architecture note's open-item clause is made loud enough that any future edit routing coexists_with into constraint_neighbors/3 cannot proceed without first reading the warning (self-flagging documented gap), or (b) an edge-type guard is added in compute_edge_contamination/7 that returns zero contamination when the underlying cs_reading_relation is coexists_with, converting "currently unenforced" to "mechanically enforced." Option (a) is consistent with the build's mark-drift-rather-than-armor discipline; option (b) closes the latent leak. The decision is "loud documentation versus actual guard," and either should be on the record rather than left as ambient.

---

## OQ-24 — forecloses requires no FPN representation, but the absence is undocumented in the engine

**Status:** open  
**Origin:** FPN convergence-test run, May 2026.  
**Files:** `prolog/cs_pattern_detection.pl`, `prolog/cs_axiom_engine.pl` (where forecloses is consumed on the committer axis); `prolog/drl_purity_network.pl` (where it is correctly absent)

**Specific question:** The run established that forecloses is gradient-orthogonal to the contamination model — points up the purity gradient (high-purity foreclosing reading → low-purity foreclosed reading), the network flows only down it, so the correct-direction edge is inert (Delta = 0) and any active injection inverts causation. This is a structural property of the relation, not a property of any specific test corpus. The engine currently has no record that this property is the reason forecloses is absent from constraint_neighbors/3 — the absence is enforced by the fact that nothing was ever wired to do otherwise. Should the engine carry a comment or assertion documenting why forecloses is structurally excluded, so a future contributor doesn't add a routing on the assumption that it's just an unimplemented feature?

**Evidence so far:** The architecture note (post-rewrite) carries the gradient-orthogonality finding. The engine itself does not. compute_edge_contamination/7 has no comment explaining why no cs_reading_relation(_, _, forecloses) ever reaches it. A future contributor reading the code would see: edge types exist, only one (the bridge via influences → detect_necessity_inheritance) is wired, no comment explains why the other two are absent. The asymmetry between document-level explanation and code-level silence is the same documentation/enforcement gap as OQ-23, but inverted — there the question is whether to enforce; here the question is whether to explain.

**What resolution changes:** One comment at compute_edge_contamination/7 (or at the top of drl_purity_network.pl) citing the FPN injection test and stating that forecloses is structurally excluded by gradient-orthogonality, with a pointer to test_forecloses_fpn_injection.pl for the witness, would prevent a future contributor from "fixing" an apparent gap. Without it, the architectural decision lives only in the architecture note, and the next person editing the network module would not encounter it. Low cost, prevents an unmarked-drift re-litigation.

---

## OQ-25 — testset directory chimera: resolved-inert but resolution mechanism is latent

**Status:** resolved (2026-05-28)  
**Origin:** Corpus cleanup, May 2026 (in response to ε-variability investigation).  
**Files:** `prolog/testsets/` (working tree); `testsets_archive_20260525/` (archive)

**Resolution:** Both options implemented.
- **(a) Documentation:** `docs/cs_load_discipline.md` — states the invariant, grouping-key decision (ConstraintAtom, not KernelAtom; OQ-26 evidence), regeneration protocol, pointer to the guard.
- **(b) Enforcement:** `prolog/config_validation.pl` — new `config_violation/1` clause wired into `validate_config_postcorpus/0` (called at end of `corpus_loader:load_all_testsets/0`). Fires when any ConstraintAtom carrying `cs_story_uid/2` has two or more distinct `constraint_metric(C, extractiveness, E)` values in the DB. Halts with exit 1 before any CS-layer predicate can run. Verified: clean load passes; injected conflict (`synthetic_conflict_test`, ε={0.42, 0.58}) correctly rejected with `CS ERROR (OQ-25)`. Divergence count post-guard: 79 reading-pair divergences across 34 kernels (unchanged from §5.11).

**Grouping-key decision:** ConstraintAtom (reading name), NOT KernelAtom. Rationale: OQ-26 confirmed ε is reading-relative; grouping by KernelAtom would false-positive on legitimate multi-reading kernels. The chimera failure mode (same ConstraintAtom from two runs, conflicting ε) is caught by ConstraintAtom grouping.

**Specific question:** The testset directory passed through three exploratory generation runs (during schema stabilization) that reused constraint IDs, leaving the directory a chimera of multiple runs with substantially different authored ε per ID. The cleanup resolved it: collisions triaged (stale duplicates dropped, genuinely-distinct readings archived rather than silently merged), directory reduced to the single committer-axis run as canonical. The §5.11 trifurcation numbers were verified byte-identical before and after, confirming the chimera was computationally inert because every CS-layer predicate gates on cs_kernel_id, which only CS-run stories carry. The mitigation works by construction — but the construction is implicit. What documents the load-discipline that any future multi-run testset directory must follow to remain inert?

**Evidence so far:** Cleanup performed; archive retained at testsets_archive_20260525/. All §5.11 numbers (axiom_foreclosure 25, husk 57, stable_pattern 19, repudiation 2; cs_axiom_foreclosed 30; cs_drift_unacknowledged 84; conflicts 39/15/35/1; kernel divergence 79/34) identical pre/post. The hermetic sealing — non-CS stories invisible to CS predicates — is a property of the current state, not a guarantee. If any future regeneration adds cs_kernel_id to a non-CS-run story (or merges runs in a way that produces inconsistent ε for a single cs_kernel_id), the chimera becomes live and the divergence count (the one ε-dependent number in §5.11, via classify_at_time) silently contaminates.

---

## OQ-26 — ε is generated, not observer-invariant in the sense Axiom 2 assumes

**Status:** resolved  
**Origin:** ε-variability investigation, May 2026; cleanup of testset chimera surfaced the underlying mechanism.  
**Resolution:** Option (a) implemented 2026-05-28. See `docs/deferential_realism_paper_v6.13.1.md` Axiom 2 (lines 66–91).

**What was changed:** Axiom 2 amended to clarify that ε-invariance holds **across observer positions** but **not across generation runs**. New **Generation-dependence note** explicitly scopes all ε-dependent statistics (H¹ distributions, classification proportions, divergence counts) to "one coherent generation" rather than treating them as population estimates. This makes v6.13.1 consistent with v7 §6's caveat and prevents the published record from asserting an invariance it doesn't have.

**Evidence resolved:**
- Birth ε=0.58 ("tangled hybrid") vs ε=0.12 ("coordination mechanism") under different SCOPE decompositions
- Mourning: 0.18 vs 0.58; Domain_partition: 0.08 vs 0.35
- These are genuinely different readings (confirmed by narrative inspection), not generation noise
- ε is a property of a reading, not of a topic; topic has no fixed ε
- The prohibition anchor for Theorem 7 validated as ε-stable (0.68 in both states) only *because* ε is generation-dependent
- Every corpus statistic the framework cites downstream is now scoped durably

**Option (b) deferred:** Constraining generation for run-reproducibility (same seed, same SCOPE, same prompt → reproducible ε) is a separate architectural item, deferred as future work. The present resolution addresses the published record: option (a) is honest about what the framework has; option (b) would buy an invariance not currently provided.

---

## OQ-27 — H¹ definition: signature-resolved vs raw-type orbit is not stated in the engine

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
**Resolved:** 2026-05-29 (same session as origin; see commit 7af6b945)  
**Origin:** Phase 2 restructure, 2026-05-29.

**What happened:** Five section-building functions were replaced with `return ""` stubs
rather than deleted, because the plan required getting the line count below 2836 without
time for a full deletion pass. The stubs pass `wc -l < 2836` but leave dead code that a
future instance could un-stub without knowing why each section was cut.

**Resolution:** All five definitions were deleted in commit 7af6b945 (not left as stubs).
Confirmed by: `git show 7af6b945 -- python/enhanced_report.py | grep '^[-+].*def build_'`
shows five `-def` removals. Zero `return ""` bodies remain among the five. Current file:
2670 lines. Render verified: `python3 python/enhanced_report.py autonomy_reading`
completes with COMPLETED SUCCESSFULLY and E5 stability band section rendered.
The "how to apply" / "why each was cut" rationale below remains valid for anyone
considering re-adding these sections.

**Stubbed functions and why each was cut:**
- `build_level3_distribution`: corpus statistics display (type distributions, constraint
  percentiles). Cut because: value-stack that gives the model a false sense of corpus
  positioning without actionable signal. H¹ and orbit data already in Level 1; distribution
  statistics add overhead without feeding the iteration-prompt contract.
- `build_structural_section`: pattern mining output (structural twins, covering analysis,
  Erdős-Selfridge table). Cut because: reads from `pattern_mining.md` and
  `covering_analysis.md` which are rarely current and never consumed by the iteration loop.
  Pattern data is available from `corpus_data.json` directly if needed.
- `build_wasserstein_section`: Wasserstein transport between perspective types. Cut because:
  highly technical metric that feeds neither the iteration prompt nor the essay synthesis
  significantly; the inter-lens divergence it measures is better captured by the verdict
  banner and hard_disagreement fields already in the sidecar.
- `build_cohomology_section`: H¹ contextuality / monotonicity display. Cut because: H¹ is
  already rendered in `build_level1_identity` and the sidecar; the full sectional cohomology
  details are redundant for the model reader.
- `build_game_theory_section`: Nash profiles and institutional observer vulnerability. Cut
  because: the game-theoretic analysis depends on pre-computed sweep data that may be stale
  (OQ-29 population), and the stability band (Section E5) now provides a more rigorous
  perturbation-based stability measurement.

**What resolution changes:** Either (a) delete the function definitions entirely and remove
the function names from this OQ entry (cleaner), or (b) promote them to explicit "archived"
status with a comment block stating why they were cut and the evidence that informed the
decision, so the record is in the code not just in ISSUES.md. Option (b) is safer: the
code documents its own provenance.

**How to apply:** Before un-stubbing any of the above, read this OQ entry. The sections were
cut because they feed neither the iteration-prompt contract (orchestrator.py:785–807) nor
the model-flinch layer (c-orchestrator.py:694). Un-stubbing requires a reason that these
criteria have changed, not just that the section "might be useful."

---

## OQ-30 — Stability band witness set incomplete (one confirmed pair only)

**Status:** mitigated (2026-05-30); Surface-2 lock front WITNESSED (2026-05-31). 24 params
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
**Files:** `python/sweeps/bifurcation_sweep.py` and 5 others (see below)

**Fix applied 2026-05-29:** Changed `Path(__file__).resolve().parent.parent` to
`Path(__file__).resolve().parents[2]` in all 6 affected sweep scripts:
`bifurcation_sweep.py` (line 380), `cognitive_displacement_sweep.py` (line 30),
`persistence_sweep.py` (lines 32, 720), `product_site_delta_sweep.py` (line 33),
`representation_robustness_sweep.py` (line 26), `structural_config_sensitivity.py` (line 42).
Output path in `bifurcation_sweep.py` left as `python/bifurcation_results.json` (existing file
location). The 6 stale result files (python/*.json) are still HELD pending re-run confirmation.

**Remaining:** A current-corpus bifurcation witness for no-kernel readings is still UNWITNESSED
on the live 223-testset corpus. The old witness (14 flips, `snare_chi_floor=0.655`, testsets_3000)
is a different corpus. OQ-32 scripts can now be run against the live corpus; whether to re-run
is a backlog decision, not a path-bug blocker.

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

Audit writeup: `outputs/audit_authoring_closure_fabricated_defaults.md`

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

**Status: open**

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

Source: `outputs/wiring_gap_census.md` (read-only census; git HEAD `220739b8`, live corpus
226 / archive 3380). The census **characterizes** every prompt↔schema↔engine disagreement and
**routes** each for adjudication — it resolves none. Stubs below are grouped by adjudication
decision; the row references map all 27 census rows so nothing is unrouted.
**Adjudication (cruft-vs-wire) is a separate session.**

## OQ-35 — G1: authored fields the engine never consumes (or consumes only inertly)

**Status: open.** Census rows 1–6.
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

**Status: open.** Census row 7. `intent_power_change`, `intent_beneficiary_class`,
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

**Status: open.** Census rows 8–12. `inevitability` (`constraint_bridge.pl:22`),
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

**Status: open.** Census rows 13, 4 + §5. **Confirmed dead:** `predict_transformation/3`
(`drl_composition.pl`, 0 callers anywhere), `cs_reference_frame/2`. The exhaustive sweep yields
528 exports → 422 zero-external-caller → {65 `/0` CLI, 114 meta-called, 26 ext-only, **217 candidate**}.
The 217 is an **upper bound**, not an orphan list — it conflates genuinely-dead with
over-exported-but-internally-called; separating them needs clause-head-vs-body parsing per
predicate. **Decision:** scope a clause-level dead-code pass to convert 217 → a real orphan list.
Do **not** strip from the 217 directly — that is the false-orphan trap (cf. the `mandatrophy_resolved`
read-vs-declare canary).

## OQ-39 — G4: prompt rules with no engine enforcer

**Status: open.** Census rows 14–18.
- Row 14 scaffold "suppression must decline over time": **no trajectory check** exists; scaffold
  uses scalar `Chi` + `has_sunset_clause`. **Decision:** add a trajectory gate or drop the rule.
- Row 15 "final measurement = base extractiveness": unenforced (no validator). **Low-stakes.**
- Rows 16–18 (piton atrophy / Goodhart / perspective-min): narrative-only, committer-only, or
  schema/linter-enforced respectively — likely no engine action.

**Resolution would change:** whether the prompt's temporal rules are real engine constraints.

## OQ-40 — G5: scalar-vs-temporal representation splits

**Status: open.** Census rows 19–22. `extractiveness`, `base_extractiveness`,
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

**Status: row 23 MITIGATED (2026-05-31, Commit A); rows 24–27 open (row 26 NEUTRAL for 3 of 6 sites — 4 OPEN, see coverage correction below).** Census rows 23–27. A silent
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

**Status: open (doc fix).** CLAUDE.md's 2026-05-31 note states `affects_constraint` is "empty
across all of testsets_3000." The census grep finds **9305 emitted facts** in the archive (520
live). The note conflated `affects_constraint/2` (a populated network edge, read at arity 2 and
matching arity-2 facts) with the genuinely-empty `intent_*` tables. The empty-table finding holds
only for the `intent_*` family (OQ-36). **No corpus-coverage divergence exists** — every predicate
agrees across both corpora. **Resolution:** correct the CLAUDE.md `Known State` note.

## OQ-43 — Satisfy-on-absence gate class: the NL beneficiary gate is the fourth instance

**Status: open. Policy decision should be made once across the class, not per site.**

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

## OQ-44 — Engine-wide audit: no gate may be satisfied by absence (authored-zero vs absent)

**Status: open (audit task). Generalizes OQ-43; gating policy for the whole satisfy-on-absence
class (OQ-41, OQ-36/OQ-37, OQ-43).**

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

**Status: open (corpus-quality audit, NOT engine maintenance).** Spun off from the D3 ruling so the
wiring fix (NL beneficiary gate fail-close, Commit B1) and the content question stay separate. The
gate fail-close makes "no beneficiary authored" honestly-conditional rather than a vacuous pass; it
does **not** decide whether any of the 404 natural-law certifications are *mis-authored* false-naturals
with a real winner hidden behind an emergence claim. Populating `intent_power_change` faithfully for a
genuine natural law yields 0 beneficiaries / 0 flips (OQ-43), so this audit only bites mis-authoring.
Audit the 404 on their own merits; do **not** populate `intent_*` as maintenance. Connects to OQ-43.

## OQ-46 — D4-for-suppression is a GENERATION-TEMPLATE requirement; it retires the row-23 stopgap

**Status: open. Sequencing constraint for the row-23 fix (OQ-41).** The row-23 fix
(`classify_at_time`) currently bridges absent temporal `suppression_requirement` to the authored
**scalar** value — a **labeled stopgap**, not a sanctioned second representation. The real fix is
**upstream, in generation**: the story template must author a *temporal* `suppression_requirement`
series (today 650/656 timeline rows have only the scalar). This is a **generation-template
requirement, not an engine representation ruling** — i.e. D4 (G5 scalar-vs-temporal) for suppression
is resolved by authoring the series, not by the engine choosing a representation. **Once the template
authors the series: delete the scalar-fallback clause in `classify_at_time` and let the temporal path
stand alone.** Do **not** build a scalar/temporal equivalence check on the bridge — skip it; the
bridge is temporary. Sequenced: this rides the regeneration arc (OQ-47), not Commit B.

## OQ-47 — Audit the SCOPE→seed seam BEFORE the de-stamp regeneration batch

**Status: open. Gating constraint on the regeneration arc.** The behavior-preserving prompt/schema
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

## OQ-48 — Classification thresholds never recalibrated against the live (post-rebuild) corpus

**Status: open (table-setting for the rebuild).** The χ / ε / suppression classification
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

**Status: open — escalated for (a)/(b) ruling. Read-only audit; no clause removed.**

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
- **:749 (1661, dominant).** Metric said snare (pure extraction; the reading *failed* the
  tangled_rope gate — lacked coordination-function / asymmetric-extraction). FNL forces tangled_rope
  (hybrid). Tension: tangled_rope is *less* extractive than snare, and the metric already determined
  the reading is not structurally tangled. Is FNL correctly catching physics-washed construction the
  metric misses (b), or laundering pure-extraction-claiming-naturality into mere hybrid, losing the
  snare signal (a)? The whole override layer turns on this.
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

---

*Last updated: 2026-06-01. Add new items with sequential OQ-NN labels. Mark
resolved items with **Status: resolved** and a resolution note rather than
deleting — provenance matters.*
