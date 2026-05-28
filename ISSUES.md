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

**Status:** open  
**Origin:** alt_power_transform full-corpus run (testsets_3000, 3380 constraints), May
2026.  
**Witness file:** `outputs/alt3k_sigmoid.json` and per-variant alt3k_*.json files

**Specific question:** Two variants (sqrt_flip: 0.833, quadratic_flip: 0.830) exceed
the paper v5 §2.3 claimed ceiling (0.828) by 0.002–0.005. Is this acceptable rounding
tolerance, or does it reflect a corpus-version skew that should be noted in v6?

**Evidence so far:** 4 of 6 original variants fall within the claimed range
(0.685–0.828). The two above-ceiling variants exceed by a margin consistent with
corpus growth from the snapshot used to compute the original range. The claimed range
was computed on an earlier corpus snapshot; the full 3,380-constraint run was the
first test on the complete testsets_3000 corpus.

**What resolution changes:** Trivial as a standalone item — within plausible
corpus-snapshot drift. Relevant only in the v6 authoring context (OQ-05). Either
correct the claimed range to 0.685–0.833 in v6, or note the corpus snapshot
(testsets_3000, 3380 constraints, run 2026-05-28) used to compute the full-corpus
figure. The choice between correcting and noting affects how the claim is presented
but not whether it is reproducible.

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

*Last updated: 2026-05-28. Add new items with sequential OQ-NN labels. Mark
resolved items with **Status: resolved** and a resolution note rather than
deleting — provenance matters.*
