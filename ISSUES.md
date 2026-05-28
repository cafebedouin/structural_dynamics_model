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

*Last updated: 2026-05-28. Add new items with sequential OQ-NN labels. Mark
resolved items with **Status: resolved** and a resolution note rather than
deleting — provenance matters.*
