# Open Questions and Issue Tracker

Persistent tracker for unresolved questions surfaced by audits and correctness
work. Each entry records: origin, the specific question whose answer would close
the item, evidence so far, and what would change once resolved.

Statuses: **open** | **investigating** | **mitigated** | **resolved**

---

## OQ-01 — Rope gate Chi ≤ 0 bypass: intentional modeling or artifact?

**Ω-type:** Ω_C (design choice — modeling decision to ratify, guard, or record in logic.md).

**Status:** resolved — bypass ratified as intentional modeling content; no guard. Operator ruling 2026-06-18.
**Priority:** 1
**Origin:** alt_power_transform corrected T2 run, Arm A/B range sweep, May 2026.
**File:** `prolog/drl_core.pl:378–387` (rope clause; bypass at :384, `(Chi =< 0 -> true ; ... BaseEps =< EpsCeil)`).

**Question:** Is `Chi =< 0 → true` intentional modeling content (χ ≤ 0 ⇒ net beneficiary ⇒
ε-magnitude no longer distinguishes rope from extraction, ceiling irrelevant) or an
implementation artifact?

**Resolution (2026-06-18):** Intentional. (1) Intent is recorded in `docs/logic.md` §rope
("Negative-chi epsilon bypass (v6.0)") and `drl_core.pl:381`, both predating the sweep; the
net-beneficiary theory is coherent. (2) The motivating worry — the Arm A3 presheaf collapse
(Jaccard 0.864→0.319, ~1,417 spurious presheaves) — was **falsified as a property of the
clause** by re-grounding on the post-reset live twins: A3 does NOT collapse on
`testsets_haiku` (0.904) or `testsets_flash` (0.897), and on flash the no-sign-flip B3
(0.820) drifts MORE than the sign-flip A3 — ruling out sign-flip as the causal variable. The
collapse was a `prolog_v5` ε/d-distribution artifact. No guard added (path b rejected on
evidence). Boundary note added to `logic.md`. Witness:
`audits/2026-06-18_oq01_rope_bypass_twins/` (WRITEUP.md, evidence/summary.json,
ground_rope_bypass.py). Residual ("which ε/d distributions re-enter the collapse band") →
OQ-22.

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

**Status:** partial — constitutive limb (03a) RESOLVED by operator seat-declaration (2026-06-18); empirical limb (03b) OPEN, **unblocked 2026-07-02** (v8 adopted, OQ-135 resolved), and possibly unnecessary.
**Priority:** 5
**Deps:** blocked_on OQ-135
**Origin:** `docs/deferential_realism_paper_v7.md` §6 open questions; reinforced by
`docs/seat-theorem-v1.md` §7–§8.
**File:** `prolog/tests/test_forecloses_fpn_injection.pl` (closest precedent)

**The underspecification (the finding that reframed this OQ, 2026-06-18).** The original
"specific question" — *run the engine on DR-as-constraint, does it return
Mountain/Rope/Snare/Tangled Rope?* — is a **Type C paradox** in the operator's own
taxonomy (`docs/debugging_philosophy.md` §5): a grammatically singular question packaging
several distinct queries (which DR — the law, the v7 framework, or the engine, per
`docs/design/v8_seat_gauge_orientation_design_spec.md` §0? authored under which seat? the
*constitutive* "where does DR sit" vs. the *empirical* "what type does one authored draw
return?"). Its fix is **index specification**, not a run. And a clean self-applied verdict
reported as DR's *location* would **commit the no-seat pose** the framework names as its
unique inconsistency (`seat-theorem-v1.md` §5, Cor 2a; the "present a face as the seat"
fraud of `docs/one_seat_audited.md`): a seated verdict posed as seat-free. So the question
as posed cannot be answered in the form it asks — and the seat corpus says exactly why.

**03a — constitutive limb: RESOLVED by declaration (operator, 2026-06-18).** Where DR sits
in its own ontology has no seat-free answer (Coupling Theorem: DR is contentful ⇒ seated;
§7–§8: it cannot self-certify its framing Π). The operator's declared seat: **base-layer
skepticism** — we cannot know Truth as a base layer — *and yet a seat must be taken*, which
means axioms, readings, and stakeholders/observers carrying time-evolving qualities (exit,
power, …) relative to the question (`docs/debugging_philosophy.md`). DR's purpose is to make
the effort to **expose those seats**, and beneath the seat, one's **orientation toward it** —
toward oneself and others (`docs/one_seat_audited.md`) — which routinely runs on limited
vision or an intentional cover story that presents a *choice* as an invariant, often a choice
someone else pays for, or one we'd rather not acknowledge has a price. The human practice DR
instantiates: `docs/litany_of_the_real.md` (orientation) and `docs/six_questions.md` (the
Cor 2b interrogative seat-exposure battery). DR therefore refuses the no-seat pose about
itself precisely *by* declaring this seat; its inability to world-anchor ε is not a defect
but the only law-consistent rest point (`v8_seat_gauge_orientation_design_spec.md` §7). The
residual *work* — building ε's declaration discipline — is now tracked at **OQ-205** (minted
2026-07-02 after the OQ-135 close left it homeless; was "the same task tracked at OQ-135 /
v8 §7"), not a separate open question here.

**03b — empirical limb: OPEN, gated, possibly unnecessary.** If one authors a
DR-constraint-story under *pre-registered, declared* choices (referent + readings + ε) and
runs the pipeline, the type returned is a fact **seated on those authoring choices**
("DR-authored-as-X classifies Y, seated on X"), never "DR is Y" — and must be reported with
its replicate-stability (generation is a draw, not a measurement — see Critical
Distinctions). Was deferred behind v8 adoption; **unblocked 2026-07-02** (OQ-135 resolved —
the v8 vocabulary any 03b writeup would be seated in is now canonical). The operator notes it
**may not be necessary**: 03a already delivers what self-application was meant to reveal
(DR's seat is declared, its orientation-discipline named), so 03b would add an illustrative
seated datum, not a resolution. Next forward move if worked: pre-register referent + readings
+ ε, run the pipeline, report type + replicate-stability seated on those choices (v8 §7.2's
redraw doctrine governs).

---

## OQ-04 — Cyclopean-point kernel: 1:N reading structure not encodable in current schema

**Ω-type:** Ω_C (design choice — schema-expressiveness cut: add a 1:N predicate or accept 1:1).

**Status:** resolved — (2026-06-23, design-cut ruling)
**Priority:** 1
**Origin:** `docs/unknown_reading_review.md` audit; `docs/altar-to-the-unknown-reading.md`;
`agent/analysis/essays/cyclopean_point_epistemic_synthesis.md` [path corrected — item
originally cited docs/; file is in agent/analysis/essays/].
**Files (archive-only — corrected 2026-06-23):**
`prolog/archives/datasets/kernel_test/disparity_as_depth_signal.pl`,
`prolog/archives/datasets/kernel_test/cyclopean_point_as_manufactured_center.pl`,
`prolog/archives/datasets/kernel_test/power_asymmetry_in_legibility.pl`;
`prolog/archives/datasets/kernel_test/autonomy_reading.pl` (template). All four are
kernel_v1-regime and were dropped from the live `prolog/testsets/` by the 2026-06-05 reset
(witness: `ls prolog/testsets/<each>` → no such file; `git ls-files | grep` → only
`archives/` paths). The original `prolog/testsets/...` `Files:` paths were stale.

**Resolution (schema cut — defer 1:N, accept 1:1):** Decline the 1:N reading-object
predicate (`cs_reading_covers/2` and `cs_reading_enumeration_status/2` — both ABSENT,
`grep -rn` empty; positive control: `cs_reading_relation` found in 5 files, so the
absence is real). The schema stays 1:1. Grounded on **reversibility + N=1 + asymmetry**:
a 1:N predicate justified by a single archived case is a predicate with exactly one user
woven through four surfaces (ontology decls, generator emission, registry validation,
report rendering) that cannot be cheaply un-shipped; the cost is asymmetric — 1:1→1:N is a
clean promotion when a *second* case appears, but un-weaving schema is not. The existing
`cs_kernel_id/2` + `cs_reading_relation/3` apparatus already expresses "N constraints of
one kernel" as sibling readings, adequate for any case the live (singleton) corpus carries.
The deferred 1:N capability is logged as a **trigger** in `docs/design/design_gaps.md`
(promote to a gap when a SECOND kernel needs it).

**Left open by design (NOT adjudicated):** the one-reading-vs-three *ontology*. The two
source docs contradict — `docs/altar-to-the-unknown-reading.md` reads the three constraints
as ONE analytical-observer reading (→ would want 1:N); `docs/unknown_reading_review.md`
Gap A reads them as THREE sibling readings (→ 1:1, already expressible). This closure rules
only the *schema* cut; it does not pick a side on the ontology, which stays explicitly open.

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

**Status:** resolved — (2026-06-23, off-case absence witness + matched-pair matrix). All
four conjuncts now witnessed in BOTH directions. Phase A searched all four real corpuses
(`testsets`, `testsets_haiku`, `testsets_flash`, `archives/datasets/kernel_v1`), each
off-bucket gated by a two-sided planted control (sensitivity+specificity, all PASS) and a
per-corpus overlay fingerprint. Result: drift-C4, axiom-C2, axiom-C4 have **live** corpus
off-cases (closed on real UIDs `0b5146c6…`, `e0fb873f…`, `b65e1d35…`); drift-C3
(stable+non-minor+unacknowledged) is a **structural absence** in all four corpuses
(unacknowledged stable drifts are always minor; non-minor stable drifts are always
acknowledged) and is closed via a transient `with_asserted` single-conjunct probe — no
synthetic fixture written to `testsets/`. Phase C matched-pair matrix: all 8 SILENT/FIRED
rows PASS, plus real-corpus fire controls (drift 46, axiom 12/182). Evidence:
`audits/2026-06-23_oq06_offcase_fixtures/` (search.pl, probe.pl, raw outputs, WRITEUP.md).
Scoped to the four named conjuncts; the presumed off-cases for the remaining conjuncts
(drift C1/C2, axiom C1/C3) are tracked as **OQ-177** (`splits_from` OQ-06). drift-C3 re-open
trigger: a future authored unacknowledged stable non-minor drift.
**Priority:** 1
**Origin:** Tranche 2 correctness pass, Phase 1 audit, May 2026.
**Files:** `prolog/cs_pattern_detection.pl:412–416` (`cs_drift_unacknowledged/2`) and
`prolog/cs_axiom_engine.pl:137–141` (`cs_axiom_foreclosed/2`) — predicates (the original
`cs_drift_engine.pl` pointer was **stale**: that file only references the predicate in a
comment at lines 34–35); `prolog/cs_corpus_analysis.pl:158–162` (callers);
`prolog/json_report.pl:531–533` (report emission).

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

**Status:** resolved — (2026-06-23) reading-robustness is now a first-class report output on
any ≥2-reading kernel; demonstrated on the live twin `end_of_life_decision_authority`.
**Priority:** 1
**Origin:** User-identified capability gap, May 2026; `docs/unknown_reading_review.md` §4–5.
**Files:** `prolog/cs_kernel_registry.pl` (`compare_kernel_readings/3`), `prolog/json_report.pl`
(`reading_robustness` object), `python/enhanced_report.py` (`build_kernel_reading_section`),
`prolog/tests/test_cs_kernel_registry.pl`.

**Resolution.** The "no predicate, script, or report section performs this comparison" premise
was STALE. The comparison *engine* already fired live: `cs_kernel_divergence/4` runs
`classify_at_time/4` across the 156 product-site contexts per reading pair, and
`write_kernel_comparison_entry` / `build_kernel_reading_section` already rendered a per-kernel
comparison block (reading_count, diverging_pair_count, axiom conflicts, trifurcation verdict,
per-reading CS metadata). Step-1 evidence pass (full-pipeline load via
`classify_corpus('testsets_haiku',…)`): the engine yields **166 context-level divergences** on
this kernel, matching the direct probe; each of the 3 readings carries its **own** `h1_band` in
`per_constraint`, so H¹-across-readings is a data join, not an engine change (Step-1(b)
falsifier did not fire; H¹ instance-blindness UNCHANGED).

What was MISSING — and what this change ADDED — is the summary/verdict layer:
- `compare_kernel_readings/3` (generalizes `cs_kernel_divergence/4`, kept as-is): a per-context
  `agree(Type)` / `diverge(TypeMap)` profile over the SAME `classify_at_time/4` evaluations
  (join, not new compute) + per-pair stats. Positive control (also a corpus-independent unit
  test `compare_join_consistency_with_divergence_engine`): Σ per-pair DivergeN ==
  #`cs_kernel_divergence` solutions — witnessed 166==166 on the twin, and again on the archived
  `state_execution_authority` triplet.
- JSON `reading_robustness` object: robust/specific context counts; per-pair **context-aligned
  Jaccard** `AgreeN/(2·NCtx−AgreeN)` over presheaf section graphs (global-vocabulary Jaccard was
  rejected — it scores ~1 when two readings merely permute which type lands where, mislabeling
  divergence as robustness); per-reading H¹ + headline `h1_band_robust` (fail-closed to `null`
  on a missing H¹).
- `enhanced_report.py`: robustness summary + Jaccard table in the existing kernel section.

Witness (twin `end_of_life_decision_authority`, 3 readings): 156 contexts → **73 reading-robust
/ 83 reading-specific**; H¹ robust (all band 5); Jaccard sanctity↔autonomy 0.63, autonomy↔vuln
0.53, sanctity↔vuln 0.31. Two-sided control passed (known-divergence ctx → diverge bucket; agree
ctx → 0 divergence solutions). Spawned **OQ-176** (`cohomological_obstruction/3` returns H¹=0 for
an absent constraint — Pattern-5 measured-flat-vs-didn't-look; out of scope here, readings are
always real).

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

**Status:** resolved — load-bearing core, 2026-06-24: Phase 2 ruled **policed-in-place (v8)**; the taint guard is the structure. Synthesis (v7 named mediator) preserved as a triggered upgrade (see ruling note). Residual vocabulary migration lives in OQ-135 (human-gated).
**Priority:** 1
**Deps:** bundled_with OQ-135 (machine-enforced one-seat invariant; the v8-§8 reading of this layer)
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
also where `classify_at_time` would split (OQ-40) — the keystone-within-the-
keystone is the mediator design itself.

**Phase 0a substrate witnesses (2026-06-23, `audits/2026-06-23_oq15_crossaxis_witnesses/`).**
Read-only census of the cross-axis surface, every find line-witnessed:
- **Two live target architectures, reconciled by an operator value ruling — not recency.**
  `docs/design/two_axis_architecture_v7.md` (mediator layer = *relocate* all cross-axis reads into a
  sole-reader third layer; `influences`→`detect_necessity_inheritance` unblessed) vs.
  `v8_seat_gauge_orientation_design_spec.md` §8 (*keep* `influences` as the one sanctioned forward
  bridge, *police in place* with a transitive dataflow taint guard). `bundled_with OQ-135`. "Structural"
  = relocated (v7) vs. mechanically-policed (v8) is the operator's named-reading call (Phase 2).
- **Call-site form is MIXED:** `cs_drift_mismatch/2` reaches observer machinery *transitively* (via
  `cs_is_metric_stable/1`→`network_dynamics`); `detect_necessity_inheritance`, `cs_kernel_divergence`,
  `compare_kernel_readings`, `constraint_neighbors/3`, and the `json_report` aggregators are static. ⇒
  grep/import checks are blind under either architecture; the **taint guard is load-bearing**.
- **`detect_necessity_inheritance` is a committer→observer ENTAILMENT-DERIVATION** (reads the *typed*
  `influences` edge → derives an observer relation); `cs_drift_mismatch` is an observer→committer
  COMPARISON (consumes an observer verdict). Different direction AND kind. **Single-bridge is principled
  in KIND, guard-enforced in CARDINALITY** (corrected from an earlier "principled" that conflated the
  two): the relation-atom type system axis-segregates — `influences` (entailment, 38) is read ONLY at
  the observer derivation; `forecloses` (47) / `coexists_with` (104) are committer-modal, never crossing.
  So the bridge is the structurally privileged entailment carrier, not "only one authored." But "exactly
  one forever" is convention-not-theorem — a future second entailment-routing is legitimate-in-kind yet
  breaks the count; the guard makes that crossing LOUD (fail-closed), which is the warrant for keeping it
  under any architecture. Informs but does not decide Phase 2.
- **The guard is corpus-INDEPENDENT (witnessed):** it walks the engine call graph; re-running with the
  live `testsets/`, `testsets_haiku/`, and `testsets_flash/` twins loaded all yield the SAME 8 edges,
  byte-identical sets. The boundary is a code property — loading a corpus adds facts, not call edges — so
  the twins need no separate guard run.
- **`constraint_bridge.pl` is NOT a cross-axis surface** — `compute_veto_actors` reads `dr_type` +
  authored `constraint_beneficiary` (a substrate/observer-input field), **no `cs_` read**. The earlier
  "reverse DR→CS read" hypothesis is *false*; it is correctly absent from `Files:` and must not enter the
  guard whitelist. (The "two DR→CS reads" above = `detect_necessity_inheritance` + `constraint_neighbors/3`.)
- **No runtime back-channel — re-witnessed engine-wide 2026-06-24 (`bc_rewitness.txt`), corrected from
  the original inspection-only read** (which swept only `cs_*.pl`/`drl_*.pl`): a non-vacuous engine-wide
  grep (flags a planted `cs_` assert) finds NONE, and the complete enumeration of every assert target
  shows zero `cs_` committer facts written at runtime (only observer substrate + observer-internal
  caches; the one `=..` site, `data_repair.pl:107`, calls a metric prior, not an assert). **Honest
  residual:** this is a STATIC witness over source ("found none," not a runtime snapshot-diff proof), and
  it is a separate surface from the guard — the guard covers static READS (observer clause calling `cs_`),
  runtime WRITES (`assertz` of a `cs_` term) are covered by this enumeration, not by the guard. The
  "(OQ-17)" pointer above was a misattribution → repointed to **OQ-40** (the scalar-vs-temporal
  `classify_at_time` split; OQ-17 is the disposed testsets_3000 quarantine).

**Phase 1 — transitive taint guard LANDED (2026-06-23, commit `fd1ee561`).** The architecture-neutral,
load-bearing half (v8 §8 item 1) is built and gate-wired: `prolog/check_axis_boundary.pl` walks the
*loaded call graph* (clause/2 over every engine predicate, descending control constructs + meta-calls +
nested module qualifiers) and emits each committer→observer boundary edge; `python/check_axis_boundary.py`
diffs them against `prolog/axis_boundary_allowlist.txt` (load_warning_gate pattern, fail-closed on any
un-allowlisted edge) and `--selftest` runs the negative case + two required positive controls (path-b
payload widening, path-c non-influences seam — both fire; they caught a real nested-qualifier blindness in
the guard before it landed). Wired into `scripts/gate.sh`; GATE GREEN; behavior-preserving (no engine
predicate touched; guard absent from the live load path).
- **The reachability census beats OQ-15's hand inventory:** 8 boundary edges, of which only **two are
  observer-VERDICT reads** — the sanctioned `influences` bridge and the bucket-3 `cs_kernel_id` exclusion
  — so **v8's "exactly one forward bridge" is confirmed in place**. The other 6 are comparison/validation
  *tooling* (`axiom_diff`, `reading_diff`, `config_validation`), modules **the original `Files:` inventory
  omitted**. (cs_drift_mismatch/cs_kernel_registry are observer→committer — the *other* direction — and
  correctly do not appear as guarded-direction edges.)
- **What remains = Phase 2, the operator's value ruling (NOT recency):** *structural = mechanically
  enforced* (the guard IS the resolution; record v8 governing, close this OQ's load-bearing core, leave
  vocabulary migration to OQ-135) **vs** *structural = relocated* (schedule the v7 code-move: extract a
  named mediator, relocate the comparison reads, unbless `influences`; the taint machinery is reusable but
  the into-mediator whitelist is gated behind the move) **vs** *synthesis* (v7's named layer enforced by
  v8's guard). W2 (the bridge is categorically the unique committer→observer dataflow) *informs* but does
  not decide it. Until ruled, shipping the guard makes policed-in-place the de-facto interim state.

**Phase 2 — RULED policed-in-place (v8); load-bearing core CLOSED (2026-06-24).** Operator ruling on the
named reading of "structural": **a green gate is sufficient — the boundary need not be legible in the
source tree today.** The taint guard *is* the resolution; v8 (policed-in-place) governs. W1 made the guard
mandatory under *every* architecture (transitive reads ⇒ relocation buys legibility, NOT
grep-checkability), so building the architecture-neutral guard first was correct regardless; that is
banked. The W2 kind-witness (entailment-typed `influences` is the structurally privileged carrier;
cardinality "exactly one" is convention-not-theorem) is why the guard polices the *contingent* half.
- **Synthesis (v7 named mediator enforced by the guard) is PRESERVED, not foreclosed — v7 is
  unbuilt-but-available.** The upgrade trigger is **falsifiable and witness-tied: a SECOND
  committer→observer bridge is proposed** (the cardinality-convention breaking — the one event under which
  the type-segregation principle must be re-argued and a named layer earns its cost). NOT the
  unfalsifiable "first legibility failure." The trigger is **mechanically wired**: such a bridge makes the
  guard fire RED (a new un-allowlisted edge), routing the contributor to `axis_boundary_allowlist.txt` →
  this OQ → the synthesis decision. The guard firing IS the trigger event.
- **The guard is now the SOLE enforcement of a convention-not-theorem, so its positive controls are
  load-bearing and run in BOTH recurring gates** (shown-firing, not assumed): `scripts/gate.sh`
  (`--selftest`, ritual) AND `python/run_pipeline.py` (axis-boundary gate beside the load-warning gate,
  automatic every pipeline). A guard that silently stopped discriminating turns both RED.
- Vocabulary/seat-gauge migration (v8 §8 item 4) remains human-gated under OQ-135.

---

## OQ-16 — Temporal vocabulary rename pass deferred

**Ω-type:** Ω_C (design choice — naming-convention cut: self-identifying `metric_*`/`context_profile_*` names over `dr_*`/status-quo; ruled this session 2026-06-25).

**Status:** resolved — 2026-06-25: all five renames executed name-only across three commits (predicate `0a204af1`; file/module + pipeline/dashboard `1d861cee`; doc references `1bcc07c5`). No logic, algorithm, or threshold moved.
**Priority:** 1
**Deps:** splits_from OQ-15 (sequenced out of the May 2026 temporal excavation as a separate name-only pass; independent of the human-gated OQ-135 v8 vocabulary wave)
**Origin:** Temporal excavation audit, May 2026.  
**Files:** `prolog/metric_drift_events.pl`, `prolog/metric_drift_report.pl`,
`prolog/context_profile_mining.pl`, `prolog/context_profile_report.pl`,
`prolog/network_dynamics.pl` (`detect_network_contamination/3`)

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

**Resolution (2026-06-25):** All five renames landed (name-only):

| Old | New | Kind |
|-----|-----|------|
| `drift_events.pl` (mod `drift_events`) | `metric_drift_events.pl` (mod `metric_drift_events`) | file + module |
| `drift_report.pl` (mod `drift_report`) | `metric_drift_report.pl` (mod `metric_drift_report`) | file + module |
| `trajectory_mining.pl` (mod `trajectory_mining`) | `context_profile_mining.pl` (mod `context_profile_mining`) | file + module |
| `trajectory_report.pl` (no module decl) | `context_profile_report.pl` | file only |
| `network_dynamics:detect_network_drift/3` | `network_dynamics:detect_network_contamination/3` | predicate only (file unchanged) |

Operator rulings folded in: `metric_*` over `dr_*` (no `dr_` scheme exists today; `cs_` is a
concept marker, not a general file-prefix convention); one complete pass (`.pl` sources +
generated `.md` output + genuine doc references) to avoid manufacturing a fresh half-renamed
mismatch. Witness: `[stack]` loads ok; `current_predicate(detect_network_contamination/3)` true
and `detect_network_drift/3` absent; `[abductive_triggers]` loads through the `drl_lifecycle`
reexport facade; `check_stack.pl` clean (no renamed-module qualifier in its undefined-predicate
set — positive control for a missed call site); full `run_pipeline.py` exit 0 writing
`outputs/context_profile_report.md` (old `trajectory_report.md` gone); dashboard reads the
renamed path. Left untouched (out of scope, logged not missed): JSON output field `drift_events`
(`json_report.pl`, python schemas), internal predicate `run_trajectory_report`, doc *filenames*
(e.g. `trajectory_implementation_notes.md` keeps its name though its body now describes
`context_profile_mining`), and dated recon/essay docs where the old filename is the subject of a
historical narrative (`recon_2_scope*.md`, `when_frame_isnt_foreground.md`). The plan
under-enumerated both the predicate call sites (completed in `1d861cee`) and the doc set
(completed in `1bcc07c5`). Pre-existing latent `dirac_classification:standard_context/1` error in
the production-disabled (`trajectory_enabled=0`) report path is rename-independent (usage
byte-identical pre/post) — tracked separately if it surfaces.

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

**Status:** resolved — behavior-preserving annotate-with-falsifier + delete dead predicate + route the fix. 2026-06-25.
**Priority:** 1
**Origin:** Temporal wiring spike, faithfulness audit, May 2026.  
**File:** `prolog/metric_drift_events.pl:72-79` (path corrected — original cited `drift_events.pl`, which exists only in a worktree).
**Deps:** bundled_with OQ-184 (faithful least-squares `drift_velocity` rebuild — the deferred output-changing fix); see also OQ-183 (`metric_trend` net-change-vs-trend seat).

**Premise correction (the OQ's "safe because output is only a boolean gate" was *partly false*, witnessed):** of the four collapsing predicates, `metric_delta/5` is genuinely private/gate-only, but two reach a SERIALIZED verdict. `drift_velocity/3` (endpoint rate) → `network_dynamics:network_drift_velocity/4` (sum over `Rate>0` contributors) → `cs_drift_mismatch:cs_is_metric_stable/1` (gate `V >= network_drift_velocity_threshold`) → `cs_drift_mismatch/2` → `json_report.pl:2015` → `pipeline_output.json`. Magnitude reads are render-only (`metric_drift_report.pl:160/:174`, `cascade_prediction`); the gate is machine-consumed. `metric_trend/3` (net-change bucket) → `cs_verdict(scaffold_suppression_escalating)` (`json_report.pl:570`).

**Settled empirical finding (re-witnessed 2026-06-25, all three live legs; probes in `audits/2026-06-25_oq18_temporal_reduction/` — `oq18_flipped_probe.pl` + `oq18_divergence_probe.pl` + `oq18_metric_trend_flip.pl` + `oq18_realized_probe3.pl`, live positive control on every leg):** the endpoint reduction genuinely diverges from the faithful least-squares slope (86/97 · 890/954 · 639/949 series; per-neighbor max |Δrate| 0.0011/0.0057/0.0067), and the `cs_drift_mismatch` gate consults non-monotone contributors (gate-live 3/56/10; realized-flippable 0/29/6) — **but 0 serialized `cs_drift_mismatch` verdicts actually flip** under the faithful velocity on any leg (max faithful SUM 0.006745/0.007851/0.004333 < Thresh 0.01; closest headroom = `0.01 − 0.007851 = 0.00215` on `testsets_haiku`). `metric_trend`→`scaffold_suppression_escalating` diverges on 0/1/17 serialized verdicts (haiku: `nicene_creed` Δ=0.08 vs fit 0.0207). So the gates are **exposed but not currently corrupting**.

**Resolution (behavior-preserving):** (1) `metric_delta/5`, `metric_trend/3`, `drift_velocity/3` annotated at their definitions with reduction-kind + faithful-source pointer + the witnessed flip-status/falsifier (NOT "safe-as-gate" — that label was falsified). (2) `drift_acceleration/3` + `compute_acceleration/2` deleted (zero callers, first-3-points reduction, misleading name); faithful full-series acceleration logged as a declared absence in `docs/design/design_gaps.md`. (3) Deferred items routed: OQ-184 (faithful-velocity rebuild, output-changing, carries the sum-level kill-condition tripwire) and OQ-183 (the net-change-vs-sustained-trend semantic seat). **Falsifier (carried, not closed-over):** the first serialized `cs_drift_mismatch` verdict whose faithful `network_drift_velocity` sum crosses `network_drift_velocity_threshold` — at which point OQ-184 must land; the 0.00215 haiku headroom is the reason to prioritize it. Witness: load+positive-control (broken-edit fails load), pipeline clean-vs-edited byte-identical modulo `pipeline_run_at` (faithful Pattern-3 diff — literal cross-run byte-identity is impossible, the manifest stamps a run timestamp), report renders, probe re-runs + raw output in `audits/2026-06-25_oq18_temporal_reduction/`.

---

## OQ-19 — Temporal-shape trigger thresholds are corpus-specific magic numbers

**Status:** resolved — thresholds hoisted to named `_DRIFT_*` constants keyed to
`_DRIFT_MEASUREMENT_GRANULARITY`; runtime guard `_series_granularity` flags
finer-than-floor series at the read site; positive-control test added
(`python/tests/test_drift_trajectory_granularity.py`). Witness below (2026-06-25).
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

**Resolution (2026-06-25, `python/enhanced_report.py`):** The defect was never
the *values* (calibration witnessed sound for this corpus) — it was that the
granularity assumption was silent. Fix, single-file, behavior-preserving:
1. Hoisted the 6 thresholds (7 occurrences) into a documented module-constant
   block above `build_drift_trajectory_section`, keyed to
   `_DRIFT_MEASUREMENT_GRANULARITY = 0.01`. Trigger A is encoded *derived*
   (`_DRIFT_REVERSAL_FLOOR = 4 * _DRIFT_MEASUREMENT_GRANULARITY`) so it auto-
   rescales; B/C stay literals with rationale (they are empirically tuned, not
   granularity-derived) and the guard is their backstop.
2. Added `_series_granularity(dt)` (str-repr decimal-count, not arithmetic) and a
   `[CALIBRATION WARNING]` line prepended to the section when the actual series
   are finer than the floor. Prose commits only to "re-run the floor sweep" (a
   premise moved, not a measured miscalibration).

**Premise correction (the finding that contradicts the original entry):** the live
corpus is *no longer uniformly 2-decimal*. 4 constraints carry **authored**
3-decimal values (`longevity_mismatch` 0.115, `propagation_speed_asymmetry`,
`protein_anabolic_resistance`, `validation_judgment_separation`; measurement_
provenance authored, not projected). **None of the 4 currently fire a trigger**, so
the section never renders for them and the guard stays **inert on rendered output**
— but the guarded-against finer-granularity regime is already partly present in
authored data, which strengthens, not weakens, the case for making the assumption
loud. If a 3-decimal constraint ever fires a trigger, the warning fires with it.

**Witnesses (2026-06-25):**
- Float kill-condition: `4*0.01==0.04` and `5*0.01==0.05` both `True` (IEEE-754) —
  derived form byte-identical to the literal, behavior-preserving to the last ULP.
- Grep completeness: pre-refactor function body holds all 7 bare literals (positive
  control); post-refactor holds **0** (every site migrated, not just exercised ones).
- Behavior-preserving per-trigger diff (HEAD vs working tree, live pipeline data):
  Trigger A `doomsday_clock_metric__hybrid_legitimacy_reading`, B
  `jewish_sovereignty_palestine__settler_colonial_reading`, C
  `animal_status_kernel__property_reading` — all three render and are **byte-identical**.
- Guard alive + inert: positive-control test renders `[CALIBRATION WARNING]` on a
  synthetic 3-decimal firing section and none on the 2-decimal one; full live corpus
  = 29 sections rendered, **0** warnings.
- Test: `python3 python/tests/test_drift_trajectory_granularity.py` → ALL PASS.

**Out of scope (stays open):** OQ-18/OQ-183 (`metric_drift_events:metric_trend/3`
net-change-vs-trend seat, the ±0.05 cut) is `bundled_with` OQ-19 only by magic-number
adjacency; it is a Prolog-engine seat (operator ruling, output-changing) and this
close does not touch it.

---

## OQ-20 — DR-regression baseline diff against `v3-dev-baseline` tag never run

**Status:** resolved — PERTURBED; priority-cascade classification stable, MaxEnt
top-type moved. Audit `audits/2026-06-22_oq20_dr_baseline_diff/` (2026-06-22).
**Priority:** 1
**Origin:** Tranche 2 correctness pass, May 2026.
**Files:** git tag `v3-dev-baseline` (`3e75f90b`); `prolog/json_report.pl`

**Question (answered):** Did CS-era code perturb DR output vs the pre-CS tag?
The literal mechanism in the original entry (checkout tag, byte-diff) is
confounded — the tag carries a different corpus (corpus reset 2026-06-05). The
audit instead held the **corpus fixed** and varied **only code** (cells A/B on
`original_json`, C/D on `original_v6_csfree`; cross-code within corpus), with an
empty noise floor (all cells byte-identical across repeats).

**Verdict: PERTURBED — and replicated across both pre-CS corpora.** CS-era code
*did* change DR output, with a sharp structure. (Code-vs-noise attribution is
witnessed: noise floor empty AND positive-controlled — repeats are fresh
processes that independently recompute, and a warm in-process 2nd run is
byte-identical to cold, so the empty floor is real, not a cache shadow; #5's
non-determinism is session-overlay/Python-phase, both bypassed by the
`run_json_report`-only path. Buckets exhaust the 30-field intersection.)
- **Priority-cascade classification BYTE-STABLE** (identical 13-field zero-diff
  set on both corpora; this is the cascade surface, NOT the MaxEnt top-type
  below): `claimed_type`, `classifications`, `base_extractiveness`,
  `suppression`, `theater_ratio`, `victims`, `beneficiaries`, `topic_domain`,
  `human_readable`, `emerges_naturally`, `requires_active_enforcement`,
  `resistance`, `resolution_strategy`. The χ/ε/d/f_d metric *values* are also
  identical (`perspective_chi` "changes" only by adding `f1_d`/`f2_d` sub-keys).
- **Per-constraint `id` relabeled** (132/1151), commit `801390a5` swapped
  enumeration `known_constraint/1` (in-file id) → `corpus_constraint/1`
  (filename base) — near-bijective relabel + two correctness gains
  (digit-leading-atom recovery `8k_tv_limit_2026`; demo exclusion
  `catholic_church_1200`). NOT the UUID migration.
- **Genuinely changed**: `signature` (~85%); MaxEnt distribution recalibrated
  (`raw_maxent_probs`/`maxent_probs`). The MaxEnt argmax (`maxent_top_type`) is a
  separate classification surface and is NOT mostly stable — flips 297/1017 (29%)
  on original_json, **2448/3373 (73%)** on original_v6, dominated by
  `tangled_rope→snare` (enumerated in `analysis.json`); the priority-cascade
  verdict (`claimed_type`) stays stable while the MaxEnt top-type recalibrates.
- **Intentional/added (not regressions)**: `domain` null→computed, `coupling`
  +violations, and **`gaps` list→null = OQ-109 B3 coverage-bit (2026-06-12) +
  the 2026-06-14 `detect_gap_pattern` rebuild** (probed: `gap_coverage/1` emits
  null=didn't-look vs []=examined-no-gap; the predicate still fires — Pattern-6
  fix, not dark code). Other downstream fields (`diagnostic_verdict`, `omegas`,
  `perspectives`, `h1_band`, `purity_*`) trail the signature/maxent changes.
  Mint a child-OQ only if a specific field's change is later questioned.

Witness/evidence: `audits/2026-06-22_oq20_dr_baseline_diff/{WRITEUP.md,analysis.json,corpus_hashes.json}`;
raw cell outputs in `outputs/oq20/` (gitignored, reproducible from pinned corpora + commit).

---

## OQ-21 — A12 multi-instance render branch never exercised on pipeline data

**Status:** mitigated — (a) correctness RESOLVED: an executed positive control found and fixed a real defect (the "latest by `cs_created_at`" path was dead — `aggregate_all(max(T-U))` evaluated `T-U` arithmetically, threw on atom UIDs, was swallowed by `catch/3`, and always fell to the `@<` fallback). Dead clause removed; `@<` ruled canonical; pinned by `prolog/tests/test_a12_multi_instance_render.pl`. (b) CLOSED as a recorded design absence (2026-06-26): A12's render branch is correct (shipped test) but its trigger — shared-ε / multi-committer-UID replicate sets — has **no demonstrated populator** (stochastic generation yields conflicting-ε draws DP-001 rightly rejects); reopens only if an ε-canonicalizing-per-reading generation mode is named. 2026-06-26.
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

**Update (2026-06-25) — the dual-consult claim was wrong; (a) resolved with a fix.**
The "verified by manual dual-consult" above read the comment's *intent*, not the
code's behavior. A positive control (`prolog/tests/test_a12_multi_instance_render.pl`)
drove the real `write_per_constraint_entry/4` render path with a synthetic
multi-instance constraint and witnessed that the documented "pick latest by
`cs_created_at`" selection **never executes**: `aggregate_all(max(T-U), …, max(_-UID))`
evaluates `T-U` as *arithmetic*, and UIDs are atoms (UUIDs), so it throws
`type_error(evaluable, …)`, is swallowed by the surrounding `catch(_, fail)`, and
*always* falls through to the `msort/last` `@<` UID-ordering fallback. The timestamp
comparison was dead for the branch's entire life.

**Ruling (operator, 2026-06-25): `@<` is canonical; recency is the WRONG selector.**
Instances of one constraint name are *parallel draws, not versions* (determinism
frontier, CLAUDE.md Critical Distinctions) — there is no canonical-latest, so
selecting by recency is incoherent with the model's own semantics. The only live
correctness-bearing consumer of a selected instance's fields is `orbit_operator.py`'s
committer terminal-projection orbit (via `cs_drift_terminal`); it needs a
*deterministic, stable* canonical, which standard order of UID atoms supplies — it
never reads timestamps. (Grep: the other selected fields have no live consumer —
`cs_reference_frame` is emitted-but-never-read; `cs_drift_moment`/`cs_drift_gap` feed
only the one-off `oq110_residual_join.py` audit.) **Fix:** the dead `aggregate_all`
clause is removed; `@<`-maximal-UID is the sole multi-instance selector; the in-code
comment now states the parallel-draws reason so the bug can't grow back (a
mechanism-only comment would invite a "helpful" revert toward timestamps).

**(a) is therefore RESOLVED** (witnessed defect → fixed → pinned), not merely
verified-by-reading. The test pins `@<` selection with **bundle coherence** (≥2
distinguishable fields track the same winner) and a **deadness/recency pin** (the
`@<`-max UID is given the *older* timestamp and must still win); a positive control
confirmed t1 goes RED if recency selection is reintroduced. **(b) — does the branch
fire on real pipeline data — remains open**, gated on a future multi-instance load
(a deliberate two-run co-load or a multi-run-merge feature). NOTE: the original
"natural unblock" pointer to OQ-17 (`testsets_3000` wiring) is stale — OQ-17 is
`disposed` (that corpus was archived 2026-06-05); (b) has no active gating OQ today.

**The colliding DATA already exists; the gate is the MERGE MECHANISM, not the data
(2026-06-26).** The same reading name drawn across runs is precisely why the
`cs_story_uid` UUID layer was introduced — `(kernel_id, reading_name)` is a *type
label, not a key*, and the UUID is the per-draw surrogate. The pre-reset multi-run
archives carry real collisions (e.g. `archives/datasets/kernel_test/`: 147 names
appearing in `kernel_run_01/02/03` with distinct UIDs; `abolition_reading.pl` lives
in two run-dirs). But name == filename and `corpus_loader` globs ONE dir
non-recursively, so any single flat `corpus_path` load collapses each name to one
file = one UID (witnessed: every live CS-layer corpus is single-instance-per-name —
`testsets/` 81/81, `testsets_haiku` & `testsets_flash` 960 facts / 0 collisions each,
`kernel_v1` 906/0). Firing A12 needs co-loading multiple draws of one name into a
single image — a multi-run merge the loader structurally lacks (flattening can't do
it: the draws share a basename). Archives without the CS layer (`original_v6`/ex-
`testsets_3000`, `original_v5`, `testsets_sotu`) have 0 `cs_story_uid` and cannot fire
it regardless. The multi-UID-per-name case is N parallel draws of one reading type —
exactly the "parallel draws, not versions" the `@<` ruling rests on.

**Closing (b) — the operative barrier is WITNESSED to be the module-collision, not a
merge abstraction nor DP-001; A12's trigger has no populator (2026-06-26).** The prior
"the gate is the MERGE MECHANISM" framing was directionally right but unwitnessed and
under-specified; a session hypothesis that "DP-001 is the single-instance barrier" was
*falsified* by running both. Two real `abolition_reading` draws (ε=0.88 and ε=0.68, from
`archives/datasets/kernel_test/{,kernel_run_02/}abolition_reading.pl`) co-loaded through
`corpus_loader`:
  - **The operative single-instance barrier is the per-story `:- module(constraint_<name>,[])`
    collision.** With both files carrying the same module declaration, the second file throws
    `permission_error(redefine,module,constraint_abolition_reading)`, is SKIPPED by
    `load_testset_list`, and the load survives: **1 testset loaded, only ε=0.88 present, one
    `corpus_constraint`, DP-001 silent, exit 0.** The collision — not DP-001 — is what keeps any
    single flat `corpus_path` load single-instance-per-name.
  - **DP-001 is the correct *complementary* backstop on the observer axis, not an obstacle.**
    Renaming only the second file's module (`constraint_abolition_reading_b`) so both files
    actually load yields a fact-level chimera; DP-001 fires exactly as designed:
    `CS ERROR (OQ-25): reading abolition_reading has conflicting ε values [0.68,0.88] … chimera
    load detected`, **exit 1**. A12 (committer multi-UID) and DP-001 (observer one-ε) are the two
    halves of the intended two-axis model, not a tension.
  - **No populator exists for A12's trigger.** A legitimate multi-instance set (one name → N UIDs)
    requires **shared-ε, committer-varied** draws; stochastic generation gives each draw a
    *different* ε (OQ-26 / Axiom 2), i.e. exactly the conflicting-ε chimera DP-001 rejects. So (b)
    is not a pending witness — it is a **declared design absence**: the branch is correct code for
    a load the discipline does not produce.
  - **Reopen condition.** A generation mode that *canonicalizes ε per reading* (making committer
    variation the only multi-instance axis) would produce the shared-ε replicate set A12 needs.
    The operator flagged that reproduced kernels exist / could be made. If such a populator is
    named, OQ-21(b) reopens and Option 2 (replicate multi-instance loader — UUID-ending-key
    filenames + decouple `register_corpus_constraint` from the basename + per-name-ε handling)
    becomes the build. Witnesses pasted in KNOWN_STATE 2026-06-26 (correction-key entry).

---

## OQ-22 — Hub 1 / Hub 2 fall-through behavior at threshold starvation

**Status:** resolved — Verdict B (2026-06-28): starvation DOES occur under the default sigmoid; the
starved subset is Hub-2 decisions reported as two-hub. Follow-up provenance field minted as OQ-192.
**Priority:** 1
**Deps:** bundled_with OQ-01
**Ω-type:** Ω_E (empirical corpus-distribution question)
**Origin:** alt_power_transform first T2 run (compressed variants), May 2026 — diagnosed during the
H1 investigation, deferred once the corrected Arm A/B sweep resolved H1. Original question: under what
conditions does Hub-1 (χ-gates) disengage and classification fall through to Hub-2 (effective
immutability), for which constraints/contexts, under the default transform; is there a boundary?

**Resolution (audit `audits/2026-06-28_oq22_hub_starvation/`, FINDINGS.md; read-only, no engine
behavior change).** 4-leg census + (observer×immutability) grid under the DEFAULT sigmoid.
**Verdict B — starvation IS present:** a constraint is starved when its observer-χ span sits within
ONE same-type band of its realized per-constraint χ→type map, so Hub-1 cannot move the type by
changing observer and any cross-observer type variation is Hub-2-sourced. Grid-confirmed Hub-2-sourced
(subset a — every member carries a grid witness; the immutability pin collapses its cross-observer
variation; **zero** reclassified to Hub-1): **testsets 5/109, haiku 23/960, flash 100/960, kernel_v1
49/1106** (band-screen starved 26/148/233/145; the larger non-starved type-varying "contrast" set is
normal two-hub with Hub-1 range, and matches the grid's persists-under-pin count exactly — the
false-halt guard routed persistence to Hub-1, never halt).
- **Scope:** the stamped per-leg CONTENT hashes (`content_hash_*.txt`: testsets f50cf9b, haiku
  b9cf33c, flash 74bf36b, kernel_v1 a5e9bae — the id-set `corpus_hash` collides across the matched
  twins, so cite content_hash). Counts are per-generation, NOT claimed as expected prevalence.
- **B kill condition:** a re-generation in which a flagged member STOPS starving falsifies that member
  (B is an existence claim, discharged by ≥1 witnessed instance; four legs witnessed — re-generation
  cannot unmake the measured counts). The symmetric Verdict-A falsifier ("a clean corpus flips to
  starved") is moot — A did not hold.
- **Boundary (gate geometry).** Min non-degenerate single-type χ-band over swept configs ≈ 0.099 (the
  piton band (0.35,0.45]); realized bands run far wider per constraint, and although band cutpoints sit
  AT the config values {0.35,0.45,0.66,0.90}, a single threshold owns 4–5 different types across
  constraints — so the χ→type map is per-constraint, never the config partition. This floor is
  **sigmoid-driven Hub-1 disengagement only**: χ = ε·f(d)·σ(scope) has a SECOND, compression-immune
  span source σ(scope) (observers span local 0.8 / national 1.0 / global 1.2) — sigmoid-only
  compression starved 46/109, +σ-flat 101/109 (Phase-3). Positive controls both fired (global
  compression → widespread; single injected ε=0.02 member individually resolved). The originally-
  witnessed extreme regime (χ ceiling 0.15) is now VALIDATOR-FORBIDDEN (`config_schema.pl:74`
  `sigmoid_upper∈[0.5,3.0]` + `L<midpoint<U`).
- **Engine doc fix (comment-only):** mountain is a PURE Hub-2 type (no χ gate) — code + canonical spec
  `docs/logic.md:644` agree; the stale inline comment `drl_core.pl:205` ("requires BOTH low χ AND
  immutability") was corrected to match. No other engine change (audit pre-committed to none).
- **OQ-01 link:** the A3 collapse was a `prolog_v5` artifact (resolved 2026-06-18, does not reproduce
  on live twins); this resolves the sharpened residual — *which ε/d distributions enter the band* — as
  Verdict B with the subset enumerated above.

---

## OQ-23 — coexists_with exclusion is unenforced design intent, not structural

**Ω-type:** Ω_C (design choice — loud documentation vs mechanical guard).

**Status:** resolved — (2026-06-29). Narrow contamination-local fix: a same-kernel-donor guard added as the first clause of `compute_edge_contamination/7` (`drl_purity_network.pl`) — a same-kernel sibling edge contributes ZERO contamination. Sibling readings are linked by `affects_constraint` only to document ε-distinctness (DP-001), not as a contamination conduit; the FPN was label-blind and treated them as downward purity contamination. **Witness:** canary census `leaked` 2→0 on testsets/ (and 1→0 forecloses); `effective_purity` for the two testsets leaking pairs returns to intrinsic (press 0.851→0.928, jewish 0.793→0.972, totalContam→0); cross-leg post-fix census `leaked=0` (`census_postfix_*.log`); plunit regression gate `no_coexists_or_forecloses_leak_on_loaded_corpus` GREEN. Sited at the contamination computation (NOT `constraint_neighbors_existing/2`) deliberately, so giant_comp topology is UNCHANGED (zero-change control: testsets 66/12 baseline unchanged) — the giant-component 334→70 reinterpretation is **deferred to the OQ-193 child** (operator ruling option 3, 2026-06-29: narrow now, giant_comp later). Discriminant `same-kernel` (every same-kernel edge connects two readings empirically; 0 non-sibling). Provenance: `audits/2026-06-29_oq23_coexists_fpn_canary/` (WRITEUP + HOLD_FINDINGS); KNOWN_STATE 2026-06-29.
**Priority:** 1
**Origin:** FPN convergence-test run, Branch E verdict, May 2026.  
**Files:** `prolog/drl_purity_network.pl` (`constraint_neighbors_existing/2` — the real side-channel admission point, line 91–110; tripwire comment 59–67); `prolog/tests/test_forecloses_fpn_injection.pl` (Case 2 — coexists_with_label_blindness); `prolog/tests/test_coexists_fpn_canary.pl` (the OQ-23 canary + controls, 2026-06-29)

**Specific question:** The architecture note's claim that coexists_with's contamination weight is "zero by definition" is an unimplemented design intent, not a mathematical property. The FPN is label-blind. ~~excluded only by the fact that nothing currently routes it into constraint_neighbors/3~~ — **FALSE premise, retired 2026-06-29 (see Evidence).** The exclusion is *not* latent: the generation template authors an `affects_constraint` edge between sibling readings, that edge IS routed into `constraint_neighbors_existing/2`, and the intra-kernel filter (`drl_purity_network.pl:105`) covers only shared-agent edges — so coexists_with siblings already contaminate each other via the parallel `affects_constraint` side channel. The decision is no longer "loud documentation vs guard" but "**close the active leak vs accept dual-channel authoring and retire the claim**."

**Evidence:**
- (May 2026) The FPN injection test (Case 2) ran an identical injection to forecloses_1b and got identical scalar flow — label-blindness confirmed at the predicate level.
- **(2026-06-29) Positive-controlled canary measures the leak as ACTIVE on the corpus, not latent** (`audits/2026-06-29_oq23_coexists_fpn_canary/`). Census of co-present coexists_with sibling pairs, leak = `effective_purity` attributes Contam>0 via the `affects_constraint` edge:

  | leg | denom | eligible (purity≥0, Δ>0) | coupled (affects_constraint) | **leaked** |
  |---|---|---|---|---|
  | `testsets/` (live) | 3 | 2 | 3 | **2** |
  | `testsets_haiku/` | 770 | 181 | 762 | **178** |
  | `testsets_flash/` | 776 | 461 | 590 | **361** |
  | `kernel_v1` | 695 | 676 | 680 | **662** |

  Every populated leg leaks. testsets/ leaking pairs: `press_reformation_causation` (mutual_shaping↔strategic_deployment, Contam=0.256) and `jewish_sovereignty_palestine` (cultural_zionist↔settler_colonial, Contam=0.199) — both same-kernel siblings, contaminating edge `source=explicit` (the authored `affects_constraint` side channel; shared-agent edges correctly stripped by the line-105 intra-kernel filter). Positive control (fact-level injection), two negative controls (equal-purity; sentinel-donor short-circuit), and a direct-typed-edge tripwire all green — the probe SEES leaks (Pattern 5 discharged).
- The OQ-23 entry's earlier "held on testsets/ by sparsity" recon sampled ONE kernel (`basic_law…parliamentary_sovereignty`, sibling ungenerated); other live kernels DO have co-present siblings and leak. The exclusion held only by two Pattern-5 absences (singleton sparsity, `-1.0` purity sentinel) — neither is a coexists_with filter.

**What resolution changes (operator's Ω_C ruling):** Either **(1)** extend the intra-kernel filter in `constraint_neighbors_existing/2` to strip `affects_constraint` edges between same-kernel typed-relation siblings (a real engine change with its own old-vs-new pipeline diff; makes "zero by definition" true and flips the canary green), or **(2)** accept dual-channel authoring — siblings DO contaminate via authored `affects_constraint` — and retire the "zero contamination weight by definition" language from the architecture as wrong. The canary (`run_coexists_census/0` + the saved census logs) is the standing witness either way. Ruling (a+) [documented-only] is NOT available: the gap is active, not latent, so "documented-only" would assert a false world-fact.

**Investigation outcome (2026-06-29, operator ruled HOLD then option-4 investigate; see `audits/2026-06-29_oq23_coexists_fpn_canary/HOLD_FINDINGS.md`):** the sibling `affects_constraint` edges are the LLM faithfully following the DP-001 ε-invariance instruction ("link ε-distinct constraints via affects_constraint/2"); the relation is OVERLOADED (ε-linkage / UKE dependency / generic). A reversible load-time strip + per-consumer diff **falsified the "redundant / no info loss" premise**: stripping same-kernel sibling edges changes **4 of 5** reachable consumers — FPN (the leak), composition `detect_extraction_dominance` (737 sibling pairs flagged as `embedded_snare` on kernel_v1), counterfactual `dependency_chain` (1516 sibling "dependencies"), inferred_coupling; only `has_viable_alternatives` is inert (`constraint_bridge`/`uke_dr_bridge` are recommendation-source-gated → unreachable by sibling edges). So an **FPN-local filter would leave the same sibling-as-component/dependency conflation live in composition + counterfactual** — it is not a localized FPN issue. Discriminant precision is corpus-dependent: kernel_v1 1516/1516 same-kernel edges are typed-sibling, but testsets only 6/64 (58 are sibling pairs whose `cs_reading_relation` dangles, OQ-58) — so `cs_reading_relation` is NOT a complete substitute on the live leg. Re-escalated; operator ruled per-consumer (correction-vs-loss), first step = reachability witness. **Reachability table (does each sibling-edge read SHIP?):** FPN `effective_purity` → YES (`pipeline_output.json contamination_network`) = ships+wrong → **fix**; coupling `inferred_coupling` baseline → YES (`coupling_protocol.md` edge count) = wrong by the module's own OQ-84 logic → **fix candidate**; composition `detect_extraction_dominance` → NO (zero callers) and counterfactual `dependency_chain` → NO (`simulate_cut` has no live caller) = **inert-wrong → log, no engine change**; signature inert → drop. So only 2 of 4 ship. Fix = consumer-local `same-kernel` guard in `constraint_neighbors_existing/2` (mirrors line-105 shared-agent guard; same-kernel==sibling empirically, 0 non-sibling found) — but that site is shared by 5 contamination-topology consumers (fpn/network_dynamics/giant_comp/json_report), so it is contamination-topology-local (giant_comp ripple), output-changing → needs old-vs-new pipeline diff + manual go. OQ-58 DECOUPLED (typed edge is a label, consumers read structure). OQ-24/forecloses rides the FPN fix. STILL OPEN pending the go on the output-changing fix.

**Cross-link (OQ-24 reopening candidate, flag-don't-fix):** the same `affects_constraint` side channel is label-blind to `forecloses` too — on `testsets/` the forecloses census is denom=1/eligible=1/coupled=1/**leaked=1**. OQ-24's doc-only close ("forecloses excluded by gradient-orthogonality") protects only the *typed* channel; the authored side door carries contamination in the causation-inverted direction the OQ-24 argument relied on being inert. Logged as an OQ-24 reopening candidate; not folded into the OQ-23 change.

---

## OQ-193 — does giant_comp connectivity legitimately count same-kernel sibling edges? (deferred from OQ-23)

**Ω-type:** Ω_C (design choice — is within-kernel sibling connectivity inflation a correction to make or legitimate topology).

**Status:** open
**Priority:** 3
**Origin:** OQ-23 giant-comp ripple witness, 2026-06-29 (operator ruling option 3: narrow-now-giant_comp-later).
**Deps:** splits_from OQ-23, blocked_on_human operator-correction-vs-loss-ruling
**Files:** `prolog/giant_component_analysis.pl`; `prolog/drl_purity_network.pl` (`constraint_neighbors_existing/2` lines 91–110); `audits/2026-06-29_oq23_coexists_fpn_canary/giant_ripple_*.log`

**Specific question:** `giant_component_analysis` builds its contamination/coupling graph from `constraint_neighbors/3`, which admits same-kernel sibling `affects_constraint` edges (the DP-001 ε-linkage edges — see OQ-23). **Witnessed fact:** those sibling edges are heavily load-bearing for connectivity — stripping them collapses the giant component **334→70** and triples components **276→789** on kernel_v1 (testsets 66→87 / 12→9); positive control passed (raw `affects_constraint` dropped by exactly the strip count). So giant_comp's headline "the constraint network forms a giant component of N" currently counts within-kernel reading-plurality as cross-kernel coupling. Is that a **correction** to make (the true cross-kernel giant is ~70; same-kernel siblings are not a coupling signal — the engine already says so for shared-agent edges at `drl_purity_network.pl:105` and the `inferred_coupling` OQ-84 guard) or a **loss** to avoid (authored sibling edges are legitimate topology and 334 is intended)?

**Why deferred, not folded into the OQ-23 fix:** the OQ-23 FPN purity leak was *witnessed wrong and shipping*; this giant_comp change is *not witnessed either way* (genuine correction-vs-loss). Resolving it by precedent-analogy (OQ-84 shared-agent → explicit) is the same analogy-across-consumers move that the OQ-23 per-consumer diff already falsified once. The OQ-23 fix was sited at `compute_edge_contamination/7` (contamination-local) precisely so giant_comp topology stays UNCHANGED (zero-change control witnessed) and this question can be ruled on its own evidence rather than as a side effect. A 334→70 reinterpretation of a shipped headline metric deserves its own witness + ruling.

**What resolution changes:** if **correction** — extend the intra-kernel guard to the explicit-edge channel in the topology path (e.g. `constraint_neighbors_existing/2`, mirroring line 105), with an old-vs-new diff of every `constraint_neighbors` consumer (5 sites: fpn/network_dynamics/giant_comp/json_report) and the giant_comp headline restated to the cross-kernel value. If **loss** — document that giant_comp counts authored sibling edges as topology by design, and the OQ-84 shared-agent guard is NOT extended to explicit edges (record the asymmetry rationale).

---

## OQ-194 — 21 embedded validation tests fail on the tracked corpus (meant-to-pass or WIP?)

**Ω-type:** Ω_E (empirical — what is the actual intended-green state of these validation units; witnessable by inspecting validation intent + history).

**Status:** resolved — meant-to-pass-vs-WIP answered (operator ruling, 2026-06-30): the 20
embedded failures are **neither** — they are **correct apparatus commentary**, conditional on the
embedded tests staying **non-gating** (not in `gate.sh`). The 21st failure was an unrelated
fixture-rot defect (fixed, separate commit).
**Priority:** 3
**Origin:** surfaced incidentally during the OQ-23 fix (2026-06-29), while regression-checking `tests/test_phantom_neighbor_filter`.
**Files:** `prolog/tests/test_phantom_neighbor_filter.pl`; the 16 tracked claim=mountain testsets carrying the embedded `mountain_threshold_validation` / `nl_profile_validation` units (e.g. `animal_status_kernel__property_reading.pl`).

**Resolution.** Live run (verbatim, 2026-06-29): `cd prolog && swipl -g "[stack], [tests/test_phantom_neighbor_filter], run_tests, halt"` → **21 fail / 93 pass**. The 21 are two unrelated things:

- **20 embedded failures** = 13 `mountain_threshold_validation` + 7 `nl_profile_validation` (the 7 files are a strict subset of the 13). These are the apparatus **correctly commenting** that stories which *claim* mountain (`constraint_claim(C, mountain)` — the SURFACE claim; `data_verification.pl:178` labels it "Claimed type") do **not** have true-mountain metrics. DR's core is claim ≠ actual, so a mountain-claim with extractive/contested metrics is the disguised-constraint phenomenon working as designed. **Not regressions, not WIP**; the tight bars are intentional (failure marks contention). **Conditional on staying non-gating:** a failing *assertion* conventionally reads as defect, so the red-as-signal interpretation holds only because these are not wired into `gate.sh` — it breaks the moment anything gates on them.
- **1 `phantom_neighbor_filter:real_target_edge_fires` failure** — an unrelated **fixture-rot defect** (the OQ-95 harness's own positive control); fixed in a separate commit (self-selecting fixture + loud throw).

**Structural evidence (self-contained; holds even if no OQ existed).** The generator (`python/generate_constraint_pl.py`, `_generate_tests`, the `if bp["claimed_type"]=="mountain"` block) emits both embedded tests **only** on claim=mountain, with **hardcoded** bars (E≤0.25, S≤0.05, AC≥0.85, R≤0.15) — not derived from the story. 12 of the 32 embedded assertions PASS (3 mountain + 9 nl; protein/radiative/actinide are clean mountains) — green-by-design probes, not all-fail WIP. **OQ-116 corroborates** with a prior ruling on the same shape: its `MOUNTAIN_METRIC_CONFLICT` linter's claim-vs-metric divergence "is authored signal the engine measures by computing a different SEATED reading — and per OQ-74 / the seat theorem these readings need not collapse to one true type (the mountain claim and the metric reading both stand; the divergence is the signal)." The embedded `mountain_threshold_validation` test is the test-form analog. (Quoting OQ-116's body, re-read this session; OQ-74 / the seat theorem are cited only as OQ-116 invokes them, not independently.)

**3-bucket triage (evidence):**
- **A — claim=mountain but extractive:** animal_status (E=0.88), jewish (0.78), institutional_trust (0.68), secession (0.62), organization_floor (0.42); 4 of 5 also carry the contradictory `extraction_signature` test (E≥0.46). → engine correctly says "not a mountain."
- **B — low-E but contested/near-miss:** neutron_star, demographic_resource, architectural, longevity, validation_judgment, scale_ceiling, propagation_speed, demographic_skill (E≤0.25 but S≈0.08–0.22, or AC just under 0.85). → tight thresholds intentionally fail contested stories.
- **C — phantom:** the rotted fixture, split out (the code fix).

**Per-item commitment witness (live run, 2026-06-29 / re-confirmed 2026-06-30).** All 20 embedded failures are plunit `: failed` assertions, **zero `: error`** exceptions (no crash hiding in the bucket); and all 13 failing units' files declare `constraint_claim(_, mountain)` (verified by grep against the live run's failing-unit list).

**Landing (so the failures don't re-read as defects).** Explanatory one-line comment emitted from the generator (every future mountain testset carries it) **and** backfilled into all **16** current claim=mountain testsets (`grep -lE 'constraint_claim\([a-z0-9_]+, mountain\)' testsets/*.pl` → 16, all carry the embedded test; empty `comm -3` diff); a signpost note in the `test_phantom_neighbor_filter.pl` header; a dated KNOWN_STATE.md entry.

**Deferred calibration → OQ-48 (no new OQ minted).** The hardcoded embedded-test bars (E≤0.25, S≤0.05, AC≥0.85, R≤0.15) are added to **OQ-48** (the standing post-rebuild recalibration ledger) as additional deferred targets — the Bucket-B near-misses are genuine low-E mountains failing only the tight `S≤0.05`/`AC≥0.85` bars. Revisit lands on the queryable frontier at regen time, not in a human's memory.

**Evidence:** live `run_tests` output (2026-06-29; re-run 2026-06-30); `python/generate_constraint_pl.py` `_generate_tests`; KNOWN_STATE 2026-06-30; OQ-116 (corroborating authority); OQ-48 (calibration deferral); phantom fix in `prolog/tests/test_phantom_neighbor_filter.pl` (code commit).
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

**Reopened then re-resolved via the OQ-23 narrow fix (2026-06-29):** the OQ-23 canary found
`forecloses` siblings ALSO leaked via the authored `affects_constraint` side channel (testsets/
forecloses census: denom=1/eligible=1/coupled=1/leaked=1; kernel_v1 leaked 198/212).
Gradient-orthogonality protects only the *typed* channel; the original close did not consider the
parallel side door (causation-inverted contamination the OQ-24 argument relied on being inert).
**Now fixed:** the same-kernel-donor guard in `compute_edge_contamination/7` is relation-agnostic, so
it zeroes the forecloses side-channel leak too (forecloses census `leaked` 1→0 on testsets/ post-fix).
OQ-24 stays resolved on this stronger basis (typed channel inert by gradient-orthogonality + side
channel closed by the same-kernel guard). Evidence: `audits/2026-06-29_oq23_coexists_fpn_canary/`.

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

**Status:** resolved — 2026-06-30: **DISCLOSURE, not redefinition.** The engine already
computes H¹ over the signature-resolved `dr_type` orbit; the only gap was that no doc/comment
said so. **Append-versioning ruling:** `v6.13.md` + v6.8–v6.12 left frozen; precision landed in
`docs/deferential_realism_paper_v6.13.1.md` (dated OQ-27 amendment + two inline "signature-resolved"
qualifications at the intro and §5.1) and an engine comment at `prolog/grothendieck_cohomology.pl`
(`orbit_vector/2` + `type_at_context/3`). v7 §Thm 7 already carried the precision (no-op). Path
disclosed: `cohomological_obstruction → orbit_vector → type_at_context → dr_type`; inside `dr_type`,
`metric_based_type_indexed` (raw `classify_from_metrics`) **then** `integrate_signature_with_modal`.
**Witnesses (manifest `2026-06-30T00:08:22Z`, n=116), distinct denominators:** 65 of 86 four-real-seat
constraints at H¹>0 (discrimination witness); *separately* 116/116 reproduction of stored `h1_band`
from the serialized `perspectives` orbit (orbit-reproduction control). The unproven **general-n** gap
spectrum (W4 surfaced it) split out as **OQ-195** so it does not gate this close.
**Origin:** Theorem 7 anchor verification, May 2026 (the precision note that changed v7's H¹=0 phrasing).
**Files:** `prolog/grothendieck_cohomology.pl` (orbit_vector/type_at_context comment + :158 stale-range
flag → OQ-195); `docs/deferential_realism_paper_v6.13.1.md` (OQ-27 amendment); `docs/deferential_realism_paper_v7.md` (already correct). Successor: **OQ-195**.

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

**Status:** resolved — 2026-06-18: `corpus_hash` single-sourced (`python/corpus_hash.py`); 14 live producers self-stamp; consumers fail-closed (orbits match-guard, persistence + Fisher report guards); existing data dispositioned (2 deleted, test-artifacts excluded-as-class, pre-reset citations annotated). Threads A–E + the 4-producer residual, all witnessed. The audit-script ruling was settled by probe — `metric_audit`/`sheaf_audit` load live `pipeline_output.json`/`orbit_data.json`, so they stamp like any live producer (not the archive wrong-identity case). **Follow-up 2026-06-20:** orbit regeneration wired into the pipeline as Phase-1b (`regenerate_orbits` step) so the orbits the match-guard reads are always fresh — the fail-closed `check_orbits_corpus_hash` is kept as the backstop, not removed (KNOWN_STATE 2026-06-20).
**Priority:** 1
**Origin:** Sweep-consolidation audit, 2026-05-29.
**Files:** `python/corpus_hash.py` (single-source fingerprint, 2026-06-18); `python/run_pipeline.py` (match guard); `python/enhanced_report.py` (persistence consumer guard); the 10 stamped producers; `python/bifurcation_results.json` (confirmed stale instance); 19 `*_results.json` files total (full list below).

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

*Disposition update (2026-06-18): reconciled 19→14 on disk (2 in `outputs/` gitignored,
2 relocated to `audits/`, `alt_power_transform_results.json` plain already deleted). Of the
14, `config_sensitivity_results_test.json` and `structural_config_sensitivity_results_original.json`
were **deleted** (dead orphans). The 12 in-tree result files now carry `corpus_hash` on next
producer run; the 4 `outputs/`+`audits/` producers are the RESIDUAL above.*

Confirmed stale: `bifurcation_results.json` (constraint-level grep). Others unverified — they may be stale (if run against testsets_3000/) or current, but the file itself cannot tell you which.

**What resolution changes:** Every `*_results.json` producer stamps a `corpus_hash` field (sha256 of the sorted list of loaded `.pl` filenames, or the git HEAD of `prolog/testsets/` at run time). The new `perturb()` primitive (`python/sweeps/perturb.py`) stamps `baseline_hash` at the orbit level — extend this pattern to all producers. Consumers check staleness: if current testsets hash ≠ stored hash, flag stale rather than read as authoritative. This is the file-level analogue of the Step-2 coverage gate in the sweep-consolidation primitive: a result computed against a dead corpus is the file-level cousin of a verdict computed with zero coverage.

**Resolution (2026-06-18) — Threads A–D (all witnessed; commits `b6aefb5a`, `4ab980ff`, `7b016978`):**
- **Thread A (single-source):** the corpus fingerprint lived in **four** byte-identical copies
  (`perturb.py`, `run_pipeline.py`, `census_sweep.py` + the perturb-imported copies in
  `regenerate_orbits.py`/`demotion_pass.py`) — the Pattern-2 silent fork that lets stamp and check
  drift. Consolidated into **`python/corpus_hash.py`** (`compute_corpus_hash` +
  fail-closed `assert_corpus_current`); all callers repointed (private name kept as alias). Identity
  witness: every path = `d2b3ec9429f1` on current `testsets/`, byte-identical to the pre-merge
  baseline. (Plan said 2 copies; grep found the 3rd — `census_sweep.py`.)
- **Thread B (producers self-stamp at write time):** `config_sensitivity_sweep`,
  `directionality_sensitivity_sweep`, `cognitive_displacement_sweep`, `oracle_gap_analysis`,
  `game_theory_stability`, `product_site_delta_sweep`, `representation_robustness_sweep`,
  `structural_config_sensitivity`, `bifurcation_sweep` — **plus `persistence_sweep`** (a 10th the
  plan's 9-list missed: it produces `persistence_results.json`, consumed by `enhanced_report`).
  `perturb`/`demotion_pass`/`regenerate_orbits` already stamped. Witness: 10/10 carry the stamp
  (static); runtime control `game_theory_stability.json` freshly written with `corpus_hash`.
  (Also fixed `persistence_sweep.py:32` standalone-import crash — `parents[2]`→`parents[1]`.)
- **Thread C (consumers fail-closed):** `assert_corpus_current` requires the field AND matches it;
  raises on mismatch/absence (never pass-open on a possibly-dead corpus). The OLD known weakness —
  `run_pipeline.check_orbits_corpus_hash` was **presence-only**, so a stale-but-stamped orbits file
  passed — is **closed**: upgraded to a match-check (the `_stamp_orbits_corpus_hash` post-hoc stamp
  is already gone; `regenerate_orbits.py` is atomic — swipl export + stamp in one invocation).
  `enhanced_report.build_persistence_section` now surfaces STALE/WARNING instead of silently
  rendering a dead-corpus persistence file; `persistence_sweep` warns when its bifurcation input is
  stale. Three-sided witness: match=pass, mismatch=raise, absent=raise, no-file=pass.
- **Thread D (existing data):** a **set-level** doc-citation probe (positive control: flags v3 +
  bifurcation) corrected the plan's "5 dead orphans, none cited":
  - **Deleted** (genuinely producerless + uncited beyond this list): `config_sensitivity_results_test.json`,
    `structural_config_sensitivity_results_original.json`.
  - **Kept, excluded as a class** — `alt_power_transform_results_3k.json`, `test_battery_results.json`
    have LIVE test producers in `python/tests/` (write-only; no reader anywhere). They are test
    artifacts, not authoritative analysis results. `alt_power_transform_test_3k` runs vs the **3k
    archive**, so a testsets-keyed stamp would be **affirmatively wrong** — the named reason
    blanket-stamping is unsound (a producer must stamp ITS OWN corpus, archive or live).
  - **Kept + annotated** — `config_sensitivity_results_v3.json` is doc-cited (`config_sensitivity_v3.md`).
  - **Pre-reset annotations** on the live-framed citations: `project_orientation.md`,
    `config_sensitivity_v3.md`, `CONFIG_SENSITIVITY.md`, and **`AGENTS.md`** (the set-probe caught
    this third live-framed site the plan's "only two" missed).
- content hash (not filename-only): detects in-place edits; does NOT detect changes in
  `testsets/<run_tag>/` subdirs (unchanged limit).

**Residual CLEARED (2026-06-18, commit pending):** the four producers scoped out as "reconciled
away" now stamp — `axiom_reachability.py`, `sweeps/epsilon_sensitivity.py`, `audits/metric_audit.py`,
`audits/sheaf_audit.py` (runtime control: `sheaf_audit_results.json` freshly written with
`corpus_hash=d2b3ec9429f1`). The **Fisher consumer** at `enhanced_report.py:1903` is now guarded —
a stale/absent-hash `epsilon_sensitivity_results.json` surfaces STALE instead of rendering pre-reset
Fisher numbers (four-sided witness: current+present→render, current+absent→"not computed",
mismatch→STALE, absent-hash→STALE). The **audit-script ruling** was settled by probe, not defaulted:
`metric_audit.py:115` and `sheaf_audit.py:146–147` load the LIVE `pipeline_output.json` /
`orbit_data.json` (corpus-derived), so a `compute_corpus_hash(testsets)` stamp is the correct
identity — they are live producers, not the archive wrong-identity case. (Surfaced separately while
exercising: `sheaf_audit.py:515` ZeroDivisionError when the working set is empty — first read as
"small post-reset corpus" but later WITNESSED (2026-06-18) to be the `classifications` producer
regression, NOT corpus size: OQ-147 (crash floor, resolved) / OQ-148 (regression, open) — and
`oracle_gap_analysis.py:143` `entry["contexts"]` on a string — pre-existing bugs, not OQ-29.) Test-artifact class (`alt_power_transform_results_3k`, `test_battery_results`)
stays excluded-as-class.

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

**Status:** resolved — adjudicated 2026-06-21 (`audits/2026-06-21_oq35_field_counterfactual/`).
All 6 rows ruled with witnesses below; row 1's fact-strip executed on the operator's go
(2026-06-21, output-neutral diff-proven).
**Priority:** 1

- **Rows 2–3 `accessibility_collapse`/`resistance` — RETAIN (load-bearing router inputs), committed.**
  The census's "cosmetic (T.1)" was NL-override-specific and was superseded by the routing-sink
  conversion (OQ-128/OQ-138): these fields feed `false_summit_mountain`/`false_ci_rope`@routed/
  `constructed_high_extraction` (`signature_detection.pl:155,157,180,182,1462,1466`), which post-OQ-138
  **route** — they revert `dr_type` to the metric type and carry their effect in
  `verdict_join.{verdict,alerts,signature_grade}`. So `dr_type` alone is the WRONG diff variable (it
  shows a false 0-diff). Counterfactual probe (`probe_oq35_field_counterfactual.pl`, full observation
  tuple `obs(dr_type, signatures, verdict, alerts, signature_grade)`, three controls — presence /
  routing-aware positive / null) over 5 corpora: **load-bearing in every presence>0 corpus** (treatment
  diff: testsets 55/92, haiku 691/960, flash 537/960, kernel_v1 26/44, original_v6 421/3380; null
  control clean=0 everywhere; positive control passes everywhere). Witness example (live): a constraint
  whose `dr_type` stays `snare` in both arms while its signature flips `constructed_high_extraction →
  unknown` and verdict drops `yellow → green`. **Kill condition (single statement):** a clean 0-diff on
  the full observation tuple in *every corpus where presence>0 AND the positive control passes there*
  (presence==0 corpora cannot witness "cosmetic" — recorded "field absent here"). Not met anywhere.

- **Row 1 `is_mandatrophy_resolved/1` — dead facts → STRIPPED (operator go, 2026-06-21).** The 2 facts
  + their comment were removed from `narrative_ontology.pl` (retirement note left in place); the strip is
  output-neutral, **diff-proven** (validation-suite output byte-identical bar `[ELAPSED]` timing jitter;
  pre-existing lycurgan interval warning unchanged). Zero goal-body/meta-call readers in non-archive code
  (grep witness) made this safe. The only
  mandatrophy analytical surface, `format_mandatrophy_gap/3` → `compute_chi_v6/6`
  (`report_generator.pl:476`), computes `delta_chi` from base_extractiveness·f(d)·scope and is
  **independent** of the facts (code-read) — so the strip is output-neutral by construction. That surface
  is *itself* dead on the live corpus (0 `MANDATROPHY GAP` lines; its gate needs powerless≠institutional
  via `constraint_classification/3`, which holds 0 powerless facts live) → logged as a dangling consumer
  in `design_gaps.md`, separate from the fact strip. D6's escape hatch partly collapsed: OQ-109 retired
  `detect_omega`, so revival now also requires rebuilding the consumer — higher revival cost favors
  strip-now. (The strip was the operator's seat — executed on the 2026-06-21 go.)

- **Row 4 `cs_reference_frame/2` — RETAIN on the OQ-133 bet, kill condition attached.** NOT a clean
  RETAIN: it is **inert consumption** — serialized to committer JSON at `json_report.pl:590` but **no
  join is computed** (offline t0→t1→t2 reconciliation deferred to OQ-133). Retain as the authored t0
  anchor the deferred tier needs; **kill condition:** when OQ-133 ships, the join either materializes
  (vindicates retain) or is cut (then strip the emission). Stripping now would destroy the t0 anchor and
  remove it from the serialized committer output. Cross-link: OQ-133.

- **Rows 5–6 `uke_scope.*`, `commentary.*` — by-design, no action.** `uke_scope.*` is schema-only
  provenance (`schema:719-737`, not emitted/read); `commentary.*` is `.pl` comment text + a
  `perspectival_gap` plunit (no facts read). Confirmed; closed.

**Resolution changed:** rows 2–3 are now recorded **load-bearing** (not cosmetic — census reversed);
row 4 is **inert-consumption-retained-on-bet** (not "zero readers" — `json_report.pl:590` is a real
read site); row 1 facts are **dead** with the strip staged on the operator. See OQ-38 correction below.

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

**Status:** resolved — Census re-disposition 2026-06-30 (`audits/2026-06-30_oq37_census_redispose/`); every read-but-unauthored name dispositioned, no OQ-37-local engine work remains. Deferred capability livens → GAP-23; supp/ε-floor recalibration → OQ-48; dead-code orphan → OQ-38.
**Priority:** 1
**Deps:** bundled_with OQ-48

**Original census (rows 8–12).** `inevitability`, `internalization_depth`, `resistance_to_change`,
`accumulation_speed` + compound `accessibility_collapse(Level)`/`stakes_inflation(Level)`/
`suppression(Level)` — read by the engine but 0/0 authored, none compiler-emitted. Decision per
metric: author or remove. **Re-disposition settled the root:** all trace to the fixed compiler emit
set (`generate_constraint_pl.py:608-635`); "author" = add to compiler+schema+validator+prompt,
"remove" = strip a consumer of a never-emitted name. The confound (forgot-to-wire vs
deliberately-scoped-out) lives at the generation front, not the read site.

**RESOLUTION — per-name disposition (2026-06-30).** Authoritative cross-corpus census
(`constraint_metric(_, Name, _)` FACT pattern, not bare name): all 6 target names **0 facts** on
testsets/haiku/flash/kernel_v1 = 3,142 stories; positive controls `resistance`/`extractiveness`
fire on every leg. Witness: `audits/2026-06-30_oq37_census_redispose/recon_evidence.md`.
- `inevitability` — **read ALREADY REMOVED** (D2 strip, `constraint_bridge.pl:20-25`; the
  `constraint_status/3` `binding_limit` consumer is gone). Capability (inevitability/no-alternative
  fraud) **superseded structurally by `false_natural_law`** (`signature_detection.pl:1018,1040`;
  gate `claimed_natural + boltzmann_compliant(non_compliant)`) — scalar metric off the FNL path,
  unneeded. → GAP-23 (not-a-gap note).
- `resistance_to_change` — never emitted; the live sibling `resistance` is a **distinct referent**
  (OQ-64: NL/coercion-grid metric vs drift-domain resistance-to-abolition — shared stem, not shared
  meaning). 8 reads dispositioned: the vacuous `validate_edge_cases` piton check **already removed**
  (`1eacd2fc`, per OQ-38 — the prior "can now be removed" line was stale); the `data_validation.pl:320`
  extreme-value member **dropped** 2026-06-30 (behavior-preserving, commit `5b7a8b95`); honest
  print/null/label sites (`report_generator.pl:650,818`, `json_report.pl:265`, `utils.pl:346`) **ruled
  KEEP** (honest by design); `utils.pl:205,213` sit in zero-caller helpers (→ OQ-38);
  `metric_drift_events.pl:174,247` (`function_obsolescence`) → GAP-23 liven-unit. **Repoint
  resistance_to_change→resistance DECLINED** (OQ-64 morphology trap; the detector is already dead at
  its first goal `alternatives_available`, so the repoint buys zero behavior while baking a latent
  wrong-metric identification).
- `accumulation_speed` — sole read inside zero-caller `safe_get_profile_components/2`
  (`utils.pl:210-211`) → **OQ-38 dead-code** (not author/strip; false-orphan discipline — do not blind-strip).
- `sunset_time` — never emitted; `detect_sunset_violation` (`metric_drift_events.pl:182,250`) cannot
  fire. The self-supplied falsification tell (declared expiry, then violated); **non-redundant** with
  `scaffold_suppression_escalating` (a metric-TREND verdict, orthogonal to a declared-EXPIRY
  violation — probe b). → **GAP-23 liven (highest priority)**, operator seat.
- `internalization_depth` — two breaks: home module `psych_bridge.pl` **never loaded** (absent from
  `stack.pl`; `drl_core.pl:129`/`data_repair.pl:69` refs are comments) + input never emitted.
  Manufactured-consent quadrant `suppression_requirement` cannot separate. → **GAP-23 liven (highest
  cost)**, operator seat; kill-condition: dies if `suppression_requirement` alone separates
  manufactured from genuine consent.
- compound `accessibility_collapse(Level)`/`stakes_inflation(Level)`/`suppression(Level)` —
  **RESOLVED by OQ-93** (2026-06-11): the leveled grid was made authorable (32 facts testsets, 696
  haiku via `measurement/5`); `coercion_projection.pl:25-27` reads them live. The census "never
  emitted" was **stale**.
- **Anchor drift corrected:** the census's `drift_events.pl:141,214` is now
  `metric_drift_events.pl:174,247`; `constraint_bridge.pl:22` read is removed (comment `:20-25`).

**Opened-by-this-work residuals (routed, not OQ-37-local):**
1. χ-partition gap — **CLOSED** (`3ab3ace4`, `tangled_rope_chi_floor` 0.40→0.35). supp/ε-floor
   recalibration residual → **OQ-48**.
2. Part D masked-unknowns (`constitutional_supremacy_reading`, `hybrid_atrophy_reading`,
   `relational_autonomy`) — **MOOT post-reset**: absent from live `testsets/` (reset 2026-06-05);
   twin occurrences are independent redraws (OQ-26), not the pre-reset instances. Cannot re-witness;
   a current-corpus masked-unknown sweep would be fresh work, not this diagnosis.

---

**Historical body (pre-resolution; retained searchable).**

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

**Ω-type:** Ω_E (whether a predicate is statically dead is empirically witnessable by the tool).

**Status:** resolved — reproducible orphan-xref tool built and the four calibration orphans
stripped (2026-06-30; `audits/2026-06-30_oq38_orphan_xref/`). The discredited ad-hoc grep is
replaced by a tool-native funnel; the held-out remainder is routed to **OQ-196**.
**Priority:** 1
**Deps:** splits_from OQ-37, gates OQ-196

**What was built (commits `c9be12ca`/`736783e4`/`6a3acf1d`):**
- `prolog/orphan_xref.pl` — `library(prolog_xref)` clause-head-vs-body separator, mirrors
  `check_stack.pl` (load-path-independent diagnostic, NOT a pipeline gate). Per defined
  `Name/Arity`: file, exported?, static-caller set (module-stripped), class (`LIVE` /
  `ENTRYPOINT_CLI` / `STATIC_ORPHAN`). Caller matching is conservative (global `Name/Arity`,
  biases LIVE — the only dangerous error for an orphan tool is a false orphan).
- `python/audits/oq38_orphan_sweep.py` — runs the core, builds the dynamic-reachability surface
  (Python/shell goal-strings + Prolog name-construction prefixes), masks static orphans, emits the
  funnel.

**Tool-native funnel** (`outputs/oq38_orphan_funnel.json`, 121 sources, pre-strip): tool exports
**614** (prior grep claim 528, delta **+86 — a FINDING: the grep undercounted exports**) →
zero-static-caller 255 (201 `STATIC_ORPHAN` + 54 `ENTRYPOINT_CLI`) → N = **201 `STATIC_ORPHAN`**
(prior grep candidate 217, delta **−16**) → dynamic-masked 29 → **M = 172** real-orphan upper bound.
**[EDGE]** M is still an upper bound: "statically uncalled" ≠ "dead" for anything reachable via a
Python goal-string or Prolog name-construction (the axis `prolog_xref` is blind to).

**Stage-1 hard gate PASSED** (the tool earned trust before any positive flag): `cs_reference_frame/2`
reported `LIVE` (caller `json_report.pl:write_per_constraint_entry/4` — the adversarial OQ-35 case
the stale grep blew); `non_monotonic_trajectory/2` reported `LIVE` (caller
**`metric_drift_report.pl:generate_drift_report/1`** — confirming the prior cite `drift_report.pl:164`
was stale; that file is absent). All five name-construction positive controls fire
(`=..`, `atom_concat`, `atomic_list_concat`, `format(atom(...))`, `term_to_atom`).

**The four calibration orphans stripped** (each `STATIC_ORPHAN` & absent from the dynamic surface,
re-witnessed via the trusted tool; behavior-preserving witnesses — load gate exit 0, validation
suite byte-identical timing-normalized, pipeline `per_constraint` sha256 unchanged `d9c85bec…` with
mtime advanced):
- **Commit A** `736783e4` — slope-pair `linear_slope/2` + `slope_accum/3` (`drl_composition.pl`);
  cascade tail of the `1eacd2fc` `predict_transformation/3` strip. xref settled `slope_accum/3`'s
  only caller was `linear_slope/2` itself.
- **Commit B** `6a3acf1d` — safe_get-pair `safe_get_all_metrics/2` + `safe_get_profile_components/2`
  (`utils.pl`). The "harvester invokes safe_get by name" worry was a labeled risk-flag, not a fact —
  downgraded by witness: zero literal refs, no `safe_get_` construction across any class.
- **Cascade finding:** Commit B newly orphaned **one** predicate — `safe_get_category/3` (its sole
  caller `safe_get_all_metrics/2` is gone). Per the scope ruling it is NOT stripped here; it routes
  to OQ-196 (with the pre-existing `safe_get_extractiveness/2` / `safe_get_suppression/2` orphans).

**OQ-35 context retained:** `cs_reference_frame/2` is NOT dead — `json_report.pl:590` is a real
read site (inert *consumption*, serialized not joined — OQ-133); the tool independently confirms it
`LIVE`. Same `1eacd2fc` removed the vacuous `resistance_to_change`-keyed piton sub-check in
`validate_edge_cases/0` (`data_validation.pl`), superseded by OQ-90.

## OQ-39 — G4: prompt rules with no engine enforcer

**Ω-type:** Ω_C (design choice — where the prompt/engine enforcement boundary sits).

**Status:** resolved — Census rows 14–18 all disposed (2026-06-25). Row 14 via a commentary verdict
(NOT a gate/drop); rows 15–18 closed with witnessed dispositions.
**Priority:** 1
- **Row 14 scaffold "suppression must decline over time" — RESOLVED via COMMENTARY (operator ruling,
  2026-06-25; NOT gate-vs-drop).** Reclassifying a rising-suppression scaffold to rope/tangled_rope
  would assert *coercion* the evidence does not show — it only shows the decline rule is violated. So
  the engine annotates instead: new clause `cs_verdict(C, scaffold_suppression_escalating)` in
  `cs_pattern_detection.pl` (commentary-grade, annotate-only — flows to the `cs_verdicts` output field,
  touches no classification/override path). Fires when a constraint certifies `scaffold` at any standard
  context AND its authored `suppression_requirement` *series* is rising (`drift_events:metric_trend`).
  **14 live constraints fire.** **Cross-leg finding:** rising:falling ≈ 5–6:1 in every leg
  (testsets/ 13:2, haiku 53:7, flash 43:9 @ institutional) — the two reconciled legs share one
  generation prompt, so this rules out one model's idiosyncrasy (not prompt-independence). Since the
  rule *is* a generation-prompt rule, the sharp reading is: **the prompt's own "suppression declines"
  instruction is systematically not honored by generation**, which strengthens the commentary case
  (the engine annotates exactly where the prompt contradicts its own output). A strict "require
  decline" gate would deny 18/20 institutional scaffolds; "deny on rising" 13–14/20 — both large
  reclassifications the ruling rejects. **Doc-conflict resolved:** `metric_trend/3` reads the
  `measurement/5` series directly via `metric_delta` (earliest→latest); its consumers
  (`drift_events`, `transition_paths`, `logical_fingerprint`) do **not** route through
  `classify_at_time`, so the commentary check is time-independent and **moot to OQ-178's off-grid
  Time=0 wrinkle**. Cut/placement gotcha: the clause MUST be the first `cs_verdict/2` clause and use
  `once/1` (no trailing `!`) — it is gated on `dr_type=scaffold`, orthogonal to the `cs_pattern`-gated
  siblings, so a trailing cut or below-placement would silently prune a co-firing verdict (cut-regression
  control in `tests/test_oq39_scaffold_escalation.pl` proves a dual-verdict constraint carries BOTH).
- **Row 15 "final measurement = base extractiveness" — CLOSE, no engine action.** No validator checks
  latest-time extractiveness == authored `base_extractiveness` scalar (`drl_composition.pl:235–237`
  reads temporal-else-flagged-0.5; no equality check). Negative claim positive-controlled: grep for an
  equality validator surfaced `linter.py:227` (metric *extraction* for mandatrophy `ext_val > 0.7`, not
  an equality validator) and DID match real validators — absence witnessed, not a blind miss. The scalar
  is the authored baseline (OQ-46 sanctioned). **Low-stakes.**
- **Row 16 "piton atrophy" — CLOSE, enforcer exists.** `coordination_dead/1` (`drl_core.pl:317–320`)
  wired into the piton pre-check of `classify_from_metrics/6` (`:344–351`), fired by authored
  `coordination_vitality(C, dead|degrading)`.
- **Row 17 "Goodhart / metric substitution" — CLOSE, leave diagnostic-only** (operator confirmed).
  `detect_metric_substitution/1` (`drift_events.pl:103–110`) is report-path only (`drift_report.pl`,
  `transition_paths.pl`), not a classification gate — by design.
- **Row 18 "perspective-min" — CLOSE, no engine action.** Linter `MISSING_PERSPECTIVE`/`ROLE_COVERAGE`
  (`linter.py:144–181`). Per operator: the linter is an *operator-evaluation* tool, not an authoring
  gate constraints must pass and not an engine enforcer — the rule lives correctly at the eval surface.

**Resolution:** the prompt's temporal/structural rules are dispositioned per row — row 14 gets engine
*commentary* (not enforcement, not a drop); rows 15–18 need no engine action (the enforcer already
exists, or the rule correctly lives at the linter/report surface). Code: `cs_pattern_detection.pl`
(new `scaffold_suppression_escalating` clause), `tests/test_oq39_scaffold_escalation.pl` (3-case
positive control). See KNOWN_STATE 2026-06-25.

## OQ-40 — G5: scalar-vs-temporal representation splits

**Ω-type:** Ω_C (design choice — authoritative representation per metric, or document the axis split as intended).

**Status:** resolved — (2026-07-01) representation split RULED INTENDED (2026-06-24) and now
LIFTED into a canonical design doc (`docs/design/two_axis_architecture_v7.md` §"Representation
grounding: which store is authoritative per metric (OQ-40)"): `constraint_metric/3` = authoritative
scalar/observer store (read by `drl_core`, e.g. `base_extractiveness/2` at `drl_core.pl:85`);
`measurement/5` = temporal/committer store (read by `classify_at_time`/`drift_events`); split
intended per metric, with the temporal-only-authoring off-grid correctness obligation recorded
(OQ-83/OQ-195). Census row-22 (`compute_temporal_stability` reads the scalar store, not
`measurement/5`) SPUN OUT to **OQ-201** with its coverage witness
(`audits/2026-07-01_oq41_row26_expansion/probe_row22_coverage.pl`), per operator ruling (close on the
doc lift; do NOT repoint row-22 now — that would repeat the rows 24–25 off-grid trap inverted).
Rows 19–20 origin below.
**Priority:** 1
Census rows 19–22. `extractiveness`, `base_extractiveness`,
`suppression_requirement` each read as scalar `constraint_metric` (observer `drl_core`) **and** as
`measurement/5` (committer `drl_composition`/`drift_events`) — the two representations can carry
different values per axis. Plus `compute_temporal_stability` (`signature_detection`) folds scalar
`constraint_metric` as a pseudo-time-series instead of `measurement/5` (→ OQ-201). **Decision (taken):**
document the axis split as intended.
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

**Rows 19–20 `base_extractiveness` — split RULED INTENDED, with a live correctness edge
(2026-06-24).** The scalar-vs-temporal split is the v7 two-axis design: the observer axis
(`drl_core`) reads scalar `constraint_metric`; the committer/temporal axis (`classify_at_time`,
`drift_events`) reads `measurement/5`. NOT a defect — document as intended. **But the same
audit (`audits/2026-06-24_oq41_basex_t0/`) showed the edge has teeth:** 15 live constraints
author `base_extractiveness` TEMPORALLY ONLY (no scalar), so the temporal series is their *only*
authoritative ε. Off-grid temporal queries are therefore not an edge case but the **main path**
for the whole temporal-only family — a live correctness surface. (Its original instance, the
`cs_kernel_registry` Time=0 probe, is now moot: that comparator reverted to static `dr_type/3`,
commit `5b069ae1`. The off-grid obligation persists for genuine DR-temporal callers —
`drift_trajectory`/`degradation_chain`, OQ-83 family.) So the doc records both halves:
split is intended AND temporal-only authoring makes off-grid reads a correctness obligation.

## OQ-41 — G6: fabricated defaults for absent data (fail-closed vs impute)

**Ω-type:** Ω_C (design choice — fail-closed vs impute; subsumed by the OQ-44 satisfy-on-absence policy).

**Status:** resolved — (2026-07-01, row-26 expansion, `audits/2026-07-01_oq41_row26_expansion/`) row 23 FIXED (Commit A); rows 24–25 SUPERSEDED (commit `5b069ae1`); **row 26 CLOSED** with the five sites' per-site verdicts (below); row 27 by-design. All census rows 23–27 disposed. (Prior `partial`: row 26 NEUTRAL for 3 of 6 sites, 4 OPEN.)
**Priority:** 1
A silent
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
- Rows 24–25 `BaseX=0.5` (`drl_composition.pl` `classify_at_time_with_supp`): **the prior
  "REACHABLE-BUT-LOCKED / dormant / 0 fires at t=0" characterization is FALSIFIED on the live
  corpus (2026-06-24, n=97; audit `audits/2026-06-24_oq41_basex_t0/`).** It is LIVE at t=0:
  `cs_kernel_registry` calls `classify_at_time/4` at **Time=0** (lines 67,68,101) → /5 →
  `classify_at_time_with_supp` → the `BaseX=0.5` branch (same predicate path, no dormant
  `constraint_history` between). **15 constraints** hit the t0 default; fail-closing them
  (the OQ-44 reflex, attempted Pass-1B then REVERTED) is **output-changing**
  (`cs_kernel_divergence_count` 17→16; `jewish_sovereignty_palestine` flips
  `diverging_pair_count` 1→0). **OQ-44 does not apply: there is no absence.** All 15 author
  `base_extractiveness` as a temporal SERIES at real historical years (1450, 1700, 480 BC…),
  none at the synthetic Time=0 — **0/15 genuinely ε-absent**. So the t0 default is **off-grid
  probing**, and fail-closing ERASES real signal: `settler_colonial`=snare vs
  `cultural_zionist`=scaffold at every authored time (a true divergence) collapse to both-
  `unknown`/false-agreement (`robust_context_count` 0→156, a success-shaped absorption). The
  real fix is a probe/classifier off-grid ruling → **OQ-178** (gated falsifier resolved: the
  `cs_kernel_registry` Time=0 is a synthetic "baseline comparison" sentinel, not a shared-origin
  semantic — code comment line 61). **RESOLVED, then SUPERSEDED (commit `5b069ae1`, 2026-06-25):**
  `cs_kernel_divergence` now classifies with static `dr_type/3` and calls `classify_at_time` not at
  all, so the `BaseX=0.5` off-grid arm is no longer reached from `cs_kernel_registry` — more cleanly
  than the interim latest-snapshot fix (`9fde36c9`). NOT an OQ-44 fail-closed site. OQ-178 and OQ-179
  both dissolved at the root (the off-grid question persists only for genuine DR-temporal callers).
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
  - **Sites VERIFIED at HEAD (2026-06-24, re-witnessed for the expansion build):**
    `covering_analysis.pl:490` (`BaseEps=0.5` in `classify_at_interpolated/4`; sibling `:497`
    `Supp=0`), `gap_diagnostic.pl:120` (`BaseEps=0.5`; sibling `:127` `Supp=0`),
    `omega1_audit.pl:102` (`BaseEps=0.5`; sibling `:115` `Theater=0.0`), `drl_fpn.pl:206`
    (`Immunity=0.5`, a NEW site distinct from the already-classified `:197` LIVE-COSMETIC).
    **Build note:** these enclosing predicates have DIFFERENT signatures than the 3 covered
    `(C,V)` sites (e.g. `classify_at_interpolated/4` takes `(C,D,Sigma,Type)`), so the expansion
    needs a per-site query, not a `ROW26_SITES` list-append — plus the planned synthetic-no-data
    positive control that MUST fire. Produce-then-stop instrument; no apply.
- **Row 26 CLOSED — five-site expansion (2026-07-01, `audits/2026-07-01_oq41_row26_expansion/`,
  HEAD `27afde7a`).** Step-0 grep confirmed every 2026-06-24 line cite is exact at HEAD (no drift),
  and resolved the two flagged unknowns against substrate. Five sites, none LIVE-CLASSIFYING ⇒ **no
  fail-close landed** (behavior-preserving):
  - `covering_analysis:490` (+`:497` `Supp` sib), `gap_diagnostic:120` (+`:127`), `omega1_audit:102`
    (+`:115` `Theater`): **DORMANT/LOCKED**. All three are metric-absence guards
    (`constraint_metric(...) -> true ; X=Default`); reject-guard confirmed; must-fire control fires
    (`classify_at_interpolated` succeeds on a metric-absent atom ⇒ 0.5 default fired, returns
    `unknown`). Enclosing predicates have **0 pipeline callers** (`classify_at_interpolated` reachable
    only via dormant `gap_diagnostic`; `cache_gap_profile`/`compute_one_profile` have zero callers).
    Would-fire (genuine scalar absence — no rows-24/25 off-grid confound, `constraint_metric` has no
    grid): **9/119 testsets/, 0/1106 kernel_v1** (the 9 is a testsets/ sparsity artifact). Covered by
    the **OQ-44 once-for-class** dormant ruling (same as rows 24–25), not fixed per-site.
    **[NB: the plan's step-3 "interpolation off-grid" class turned out to have zero members — the
    `covering:490` 0.5 branch is gated on `constraint_metric` presence, not `D`/`Sigma`, so it takes
    the metric-absence bogus-atom pre-test like the others. Corrected against substrate.]**
  - `drl_fpn:206` `Immunity=0.5`: **NEUTRAL-by-corpus (cosmetic-if-fired)**. Compute-failed fallback
    (fires only when `dr_type` fails in precompute ⇒ no `fpn_type_cache`). Firing-marker patch:
    **0 natural fires** over testsets/(119) AND kernel_v1(1106), both converged; positive control
    (forced type-cache miss on a neighboured IP≥0 constraint) **FIRES** both runs ⇒ 0 is
    measured-empty, not didn't-look. `dr_type` is total on both corpora. Sink: `fpn_ep` feeds only
    diagnostic/report/abductive-evidence, **never `dr_type`** ⇒ cosmetic even if it fired.
  - `drl_fpn:197` `NewEP=IP`: **CARVED OUT of row-26 — not a fabricated default, out of OQ-41 scope**
    (no row-26 verdict assigned). Grep resolved the contested mechanism: branch is `IP < 0.0 ->
    NewEP = IP`, `IP` = `-1.0` sentinel set upstream when `fpn_intrinsic` absent, commented
    `% Sentinel: no purity data` — it propagates a negative sentinel, it does not fabricate a
    mid-value. **This also corrects the prior row-26 note (lines above), which described `:206`'s
    trigger ("fires when `dr_type` fails in precompute") under the `:197` label — a conflation:
    `:197` = intrinsic-purity-absent sentinel, `:206` = type-cache-absent Immunity default.**

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
resolved 2026-06-11, D-fork ruled NO-OPEN at OQ-110; revive-or-gap of the dormant
`transition_paths`/`snapshot_type` repair-direction classifier is OQ-91's candidate-home
question — the *separate* dormant HAC structural-family / cross-domain-twin module
`context_profile_mining.pl` is owned by **OQ-182** (revive + validate as commentary-grade), bundled
with this thread but not gated by it).

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

**Ω-type:** Ω_E (empirical content question — closed by the witnessed content read).
**Status:** resolved — 2026-07-01 — **YES.** The 404 (ARCHIVAL: measured on original_v6/testsets_3000;
live NL count is 0 and the signature is dead-by-range on HEAD per OQ-113) were re-derived by a
pre-`8b5a34b8` overlay probe (aggregate control PASS: exactly 404; member list now on substrate) and
screened exhaustively (0 false-mountain-shaped, 0/404 authored beneficiaries — member-level control on
the cascade-construction claim). Content read: 35/404 (adversarial-primary, pre-registered strata;
rubric v1 pre-flight FAILED 0/3 on the OQ-52 anchors, amended v2 passed 3/3) → **6 hidden-winner / 4
ambiguous / 25 genuine-natural**, all six quote-pairs verified verbatim. Adjudicated split (declared
seat): (i) extraction wearing the mountain frame — `repeat_player_structural_advantage` ("16.6% award
decline… real extraction" beside "emerges naturally… No policy created it"),
`demographic_elimination_imperative`, `attention_as_capturable_resource` (random-stratum hit — a BOUND-BREAKER: the pre-registered
suspicious criterion did not bound the phenomenon; unquantifiable under the chimera caveat),
`capability_compulsion_gradient` (borderline); (ii) genuine natural core with an unauthored ecosystem
winner — `gilgamesh_mortality_limit`, `ecological_carrying_capacity`. Bucket (ii) is a design note for
any future NL re-powering (GAP-08 §7): gain-AROUND-persistence ≠ gain-from-authoring; the beneficiary
leg must distinguish them — and bucket (ii) is simultaneously the content rubric's known
FALSE-POSITIVE mode (it fires on non-mis-authorings), the symmetric label to B2's known
metrics-blindness; automation inherits both or inherits an unlabeled error class. No engine change follows (NL dark on HEAD; FSM cascade + fail-close already
landed; live corpus post-de-leak). NO prevalence claim (chimera corpus, OQ-70/OQ-25). NOT exhaustive
with OQ-52: a hidden-winner neither false-mountain-shaped nor NL-certified falls through both audits.
Full evidence: `audits/2026-07-01_oq45_oq52_hidden_winners/` (WRITEUP.md).
**Cross-leg check (2026-07-02):** same instruments over kernel_v1 + twins + live (`b5_*` artifacts, same audit dir). NL populations 26/8/5/0; all 39 members content-read EXHAUSTIVELY: kernel_v1 **4 hidden-winner** (honor_satisfaction, press_reformation, state_killing_authority__abolition_reading, tsunami_stone catastrophe_validation), haiku 0 (2 reader calls downgraded on adjudication), flash **1** (`temple_sacrifice_commitment__performance_only` — a LIVE-leg instance). kernel_v1=26 matched the recorded matrix (aggregate control). Two more NL-gate coarseness data: victim-bearing stories certify (gate checks beneficiaries only); the 404 h1=4 uniformity is a v6 template artifact.
**Priority:** 1
**Deps (dropped on close):** none were authored; cross-refs OQ-43 (origin), OQ-52 (partition sibling).
**Origin:** spun off the D3 ruling (OQ-43), 2026-05-31.
*(Investigation narrative compressed on close per footer rule; full history in git.)*


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

### MEASUREMENT (2026-06-18, twins + confounded kernel_v1 arm — `audits/2026-06-18_oq48_recalibration/`)

Audit + proposal only (no `config.pl` edit). Pre-registered distribution-break verdict rule
(KDE antimode + bandwidth-robustness + lobe-mass + Hartigan Dip; cross-twin agreement as the
validity gate) run on the twins (`testsets_haiku`=960, `testsets_flash`=960; the live `testsets`
is below the bar). **Result: 0 thresholds recalibratable — all seven in-scope cuts MODEL-CONFOUNDED,
zero proposed values.** Every metric is multimodal on both twins (Dip p=0), but the flash corpus's
antimodes fail the pinned bandwidth-robustness gate *where their locations track haiku's* ("soft
agreement, hard disagreement"), so no DRIFTED candidate arises and the validity gate licenses no
move. Twin-swap falsification and the POSSIBLY-INDUCED cross-metric flag never fired (no candidate
to filter). Two 691-era cuts are corroborated by haiku: `snare_chi_floor` 0.66 ≈ haiku trough 0.666,
`snare_epsilon_floor` 0.46 ≈ haiku trough 0.484 — i.e. *not stale on haiku*, only unconfirmable on
flash.

**Confounded third arm (kernel_v1, 1106, pre-reset/pre-de-leak; corroboration-only, never pooled —
OQ-26):** corroborates `snare_epsilon_floor` (0.46) across BOTH regimes (haiku break 0.484,
kernel_v1 0.442) — external validity stronger than the seed-sharing twins; `snare_chi_floor` is a
near-miss (kernel_v1 χ break 0.606, dist 0.054, just outside ±0.05). No verdict changes.

Positive controls all pass: LOADCOUNT 960/960/1106 (`asserta` overlay; hard-stop armed at the
on-disk file count), 0-unknown metric columns, byte-identical probe re-run, planted-gap break-finder
recovers 0.4506. Provenance: twin TSV content sha256 haiku `7039d37b…` / flash `3c24b1d2…`;
metric-code commit `0a629077`. Probe/analysis: `python/audits/oq48_threshold_distributions.py`,
`oq48_analyze.py`, `oq48_triangulate_kernel_v1.py`.

**Status unchanged: open.** No floor moved → not mitigated/resolved. **Closure waits on corpus
regeneration beyond the twins** — either (recalibrate) a *same-regime* post-de-leak third corpus
that breaks the haiku/flash tie, or the live `testsets/` rebuild reaching the ~700-story Tier-4 bar
with a single-corpus break; or (vindicate-and-keep) accumulating trusted post-reset corroboration
that the 691-era cuts still cleave, mitigating cut-by-cut (`snare_epsilon_floor` is the first such
candidate). kernel_v1 is confounded and cannot promote a value.

### Additional deferred recalibration targets: embedded mountain-test bars (from OQ-194, 2026-06-30)

The generator (`python/generate_constraint_pl.py`, `_generate_tests`) emits, for every
claim=mountain story, two embedded validation tests with **hardcoded** bars:
`mountain_threshold_validation` (E≤0.25, S≤0.05) and `nl_profile_validation` (AC≥0.85, R≤0.15).
These bars are not derived from the corpus and were never recalibrated against the post-rebuild
corpus, so they belong on this ledger. OQ-194 ruled the resulting failures **correct apparatus
commentary** (claim ≠ actual), but flagged a calibration question for regen: the Bucket-B
near-misses (`neutron_star_bombardment`, `demographic_resource_allocation`, `architectural_pattern_validity`,
`longevity_mismatch`, `validation_judgment_separation`, `scale_ceiling_c0`, `propagation_speed_asymmetry`,
`demographic_skill_mismatch_c0`) are genuine low-E mountains failing **only** the tight `S≤0.05` /
`AC≥0.85` cuts — exactly the "is this bar stale?" question this OQ exists to answer at rebuild time.
Recalibrate these alongside the `config.pl` cuts; do not move them now to rescue specific stories
(curve-fitting to the holdout, the same error this OQ warns against for `tangled_rope_*`).

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

**Build-extension — RESOLVED 2026-06-25 (cs_kernel_comparison site).** `cs_kernel_registry`'s
`compare_kernel_readings/3` (`ctx_reading_verdict`, `pair_reading_agreement`) counted
`unknown==unknown` as agreement/robustness (an all-`unknown` context scored `agree`, inflating
`robust_context_count`) — the SAME family as the ruled `count_disagreeing_pairs` defect, at a
site the original OQ-51 build did not enumerate. **Applied the ruling:** `unknown` ⇒ N/A;
verdict trichotomy `agree(Type,NUnk)`/`diverge(TypeMap,NUnk)`/`undetermined(NReal,NUnk)` (lenient
— ≥2 real ⇒ verdict over the real readings); `cs_kernel_divergence/4` + `pair_reading_agreement/7`
require both types real (join invariant preserved); Jaccard `null` on zero-comparable; JSON
`specific_context_count`→`divergent_context_count` + new `undetermined`/`abstaining`/
`divergence_patterns` (deliverable ii — the report now ENUMERATES the disagreement, not just
counts). Two null-path footguns fixed (HOLE A `~6f`-on-null abort; HOLE B `:.3f`-on-None).
**Witness:** unit 20/20 (6 synthetic N/A controls + join invariant), dynamic 0 errors, pipeline
exit 0, partition invariant 9/9, `cs_kernel_divergence_count` 20→16, JSP report enumerates
settler=snare/cultural=scaffold. Commit on branch `oq51-unknown-na-cs-kernel-comparison`;
KNOWN_STATE 2026-06-25. **Scope:** ONLY `cs_kernel_comparison` — the original OQ-51 ruled-not-built
spec for `count_disagreeing_pairs`/`sheaf_status` 4th value/H1-emits-null (lines above) was the
main build item, **now BUILT 2026-06-25** (next paragraph).

**Main build — RESOLVED 2026-06-25 (canonical sheaf/H1 path + product-site H1).** The ruled spec
(lines above) is built. `grothendieck_cohomology`: `is_real_type/1`; `count_disagreeing_pairs`
counts only both-real-different pairs; `obstruction_from_vector/3` (pure, synthetic-testable) emits
**H0=null, H1=null** on <2 real seats; `contextuality_*` exclude undetermined from numerator AND
denominator (`number/1` guards stop `sum_list` throwing on the null atom); `descent_status` gains an
`undetermined` clause. `sheaf_analysis:sheaf_status/2` gains the 4th regime `undetermined` via BOTH
ruled routes — route 1 (<2 real seats) and route 2 (H1=0 but `arakelov_height/2` uncomputable, the
second absence route) — and a sibling `sheaf_undetermined_reason/2` carries
`insufficient_seats|uncomputable_height` (the provenance bit; the two routes resolve under different
added data — do not collapse). JSON/schema: `h1_band` nullable, `sheaf_status` enum + `undetermined`,
new `sheaf_undetermined_reason` field with the present-IFF-undetermined cross-field invariant.
Containment (operator-ruled loud, in core commit): per-site `h1_band_or_raise` guard
(`shared/loader.py`) on the 13 live `h1_band` readers — a null read silently-as-0 now fails LOUD,
distinguishing key-absent (stale artifact) from null (undetermined). Commit 2 applied the same N/A
rule to `product_site_export.pl`'s own H1 (separate file `product_site_orbits.json`, disjoint
consumers `range_sweep`/`product_site_delta_sweep` made null-aware). **Witness:** `test_sheaf_na`
10/10 synthetic controls + live route-1=15; dynamic suite 0 errors; pipeline exit 0 + mtime advanced;
schema gate green; per-constraint diff 26 h1_band / 22 sheaf_status moves, 15 undetermined
(insufficient_seats), 0 null-h1-not-undetermined; w1 `partition_ok=True`; containment trips loud on
`run_drift_mismatch`+`game_theory_nash`; None-aware readers stay green; cold==warm cache null. **Cross-
corpus (engine change):** 0 partition violations on `testsets_haiku`(960), `testsets_flash`(960),
`kernel_v1`(1106). Route 2 is **live-dormant in the pipeline** (witnessed synthetically; `arakelov`
needs MaxEnt absent in a bare load). Commits `f8ae0c9c` (core) + `15cca7ed` (product) on branch
`oq51-sheaf-na-canonical`; KNOWN_STATE 2026-06-25. **Priority:** 1. Residual surfaces minted as
**OQ-180** (sibling bare-`\=` diagnostics + 3 audit-dir silent h1_band sites) and **OQ-181** (per-site
undetermined *semantics* for the 13 readers — containment makes them loud, OQ-181 makes them correct).
*(Investigation narratives compressed 2026-06-04 per footer rule; full probes in git history.)*
## OQ-52 — False-mountain cross: do the naturalized→snare manifest rows have an authored beneficiary?

**Ω-type:** Ω_C (design choice — what the false-mountain read is *for*, and whether the beneficiary
channel is the right disqualifier). Related: OQ-43/OQ-45 (partition: OQ-52 is the beneficiary-AUTHORED
presents-as-natural side; OQ-45's 404 NL certs are the beneficiary-SILENT side. NOT exhaustive: a
hidden-winner neither false-mountain-shaped nor NL-certified falls through both).
**Status:** resolved — 2026-07-01 — both legs closed. Core (2026-06-02, n=772 engine): all 16
false-mountain manifest rows carried authored beneficiary+victim → authored extraction visible only at
the analytical seat. W1-magnitude leg (2026-07-01): `w1_sheaf_join.py` gained the
`wasserstein_incomparable_mass` join + PROVISIONAL ≥0.05 materiality label (commit `e8189d10`);
kernel_v1 classified same-run via `classify_corpus` (n=1106) and the CURRENT-ENGINE selection
(N=293: strict=235 + loose=58) ranked —
`audits/2026-07-01_oq45_oq52_hidden_winners/a3_false_mountain_w1_ranking.{md,json}`. Control outcome (the recover-the-16 control was RETIRED AS IMPOSSIBLE — list never saved — not passed):
all 5 recorded member names recover with member-level H1 EXACT (quran=4/article_9=5/abrahamic=6), but
the population count does NOT reproduce (HEAD ENGINE on kernel_v1 — same corpus, new code:
strict=235+loose=58 of manifest=944; live n=119 leg for contrast: manifest=71, strict=4, loose=4 —
vs 16 of 98 on the 2026-06-02 engine; 944 cross-witnessed by OQ-197 acceptance controls, commit
`34ff919f`) — engine drift. The ranking is anchored by the three named members, not a recovery of the sixteen. HEAD re-measure: 289/293 both-authored,
**4 victim-only** (first partial-disqualifier cases; repair sentinels screened 0/1106), 0 with neither
— the corrective-grade reading holds. H1 stays the stable ordering; W1 magnitude is valid only for the
pinned manifest.
**Cross-leg check (2026-07-02):** the authored-channel finding replicates at 100% on every live leg — haiku 113/113 both channels, flash 83/83, live testsets 8/8 (kernel_v1 289/293, the 4 victim-only rows remain the only exceptions). `b5_screen_twins.json` in the audit dir.
**Priority:** 1
**Deps (dropped on close):** was prose-gated on "the OQ-51 W1 rebuild"; OQ-51 resolved via the
h1_band-N/A route — of its W1 spec only snapshot-pinning pre-existed (`980f0224`); the join+threshold
landed here.
**Origin:** W1 × sheaf_status join, 2026-06-02.
*(Investigation narrative compressed on close per footer rule; full history in git.)*


---

## OQ-53 — Observer and reading axes are conflated in the classifier (no kernel-fixed reading comparison)

**Ω-type:** Ω_C (design choice — is the kernel/reading axis first-class?).

**Status:** resolved — 2026-06-20 — both legs closed. Within-kernel SATISFIED (OQ-55 router);
transpose WITNESSED-LIVE on the OQ-56 canonical vocabulary. See resolution note.
**Priority:** 1
**Deps (dropped on close):** was `blocked_on OQ-56`; OQ-56 ruled 2026-06-20 — transpose runs on the
canonical `observer_signature`/`obstruction_class` vocabulary.
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

**Report-path witness (2026-06-20, `audits/2026-06-20_kernel_reading_orbits/RECON.md`) — surprises
the earlier assumption.** Code-read of the three conflation-locus files shows a SPLIT, not a flat
prefix-opacity: the kernel is a **first-class queryable object** in `cs_kernel_registry.pl`
(`cs_readings_for_kernel/2`, `cs_kernel_obstruction/4`) AND in `json_report.pl` (enumerates
`cs_kernel_id(_,K)`, emits the `cs_kernel_comparison` C3 array + `cs_kernel_divergence_count` B3);
it is **prefix-opaque only in `logical_fingerprint.pl`** (0 kernel mentions — a per-reading
fingerprint is kernel-blind by design). Firing witnessed on the haiku twin: 328 multi-reading
kernels feed `cs_kernel_comparison` (non-empty array; via the Phase-1b probe's 331
`cs_readings_for_kernel` firings). So the same-kernel leg is best closed **(a-restricted):
first-class in the registry/operator AND report layers; prefix-opaque only in the per-reading
fingerprint** — not the flat "(b) report prefix-opaque" earlier expected.

**What resolution changes:** The same-kernel leg is effectively built (registry + report
first-class; same-kernel diff = `reading_diff`/`axiom_diff`, OQ-59). The **transpose leg**
("hold a reading-stance fixed, sweep across kernels") is gated on the OQ-56 vocabulary pick. The
OQ-150 measurement (2026-06-20) shows only the Tier-1 keys (observer-signature, obstruction-class)
are draw-robust; a transpose over a Tier-2 key would be model-relative. **Reserved (operator's
call, NOT pre-closed here):** whether to build a knowingly-model-relative committer-axis transpose
or restrict the transpose to the draw-robust observer axis. The empty-menu kill did not fire
(Tier 1 is non-empty), so this is present-and-escalate, not foreclosed-by-finding.

**Transpose surface BUILT (operator ruling 2026-06-20; commit `0c488468`).** The cross-kernel
orbit operator (`python/orbit_operator.py` + `prolog/kernel_orbit_export.pl`, wired into
`run_pipeline.py`) materializes `outputs/{reading,kernel}_orbits.json`: readings grouped across
kernels by each declared key, kernels grouped by structure/obstruction. Tier-1
(observer-signature, obstruction-class) is the declared transpose surface; Tier-2 committer-side
keys are emitted as report-only with their twin-agreement numbers inline. The transpose query
("hold a reading-orbit fixed, sweep across kernels") is now answerable over these artifacts. Live
orbits are sparse (3 multi-reading kernels) — the discovery substrate is the twins. Same-kernel
leg = (a)-for-json_report (witnessed).

**Resolution (2026-06-20) — both legs closed.**
- **Within-kernel leg SATISFIED.** Kernel is first-class in `cs_kernel_registry.pl` + `json_report.pl`;
  readings compared as a set; *why* they disagree routed by `cs_trifurcation:cs_reading_trifurcation/3`
  (live in `cs_kernel_comparison` as `reading_trifurcation`, `scope:within_kernel`, commentary-grade,
  all four A/B/C/unknown branches firing — OQ-55 resolved).
- **Transpose leg WITNESSED-LIVE (Branch 1).** With OQ-56 ruled (canonical vocabulary =
  `observer_signature` + `obstruction_class`), the transpose query — hold `observer_signature` fixed,
  sweep across kernels — runs on the live corpus and finds multi-kernel orbits:
  `constructed_high_extraction` spans **25 genuine multi-reading kernels**, `false_ci_rope` spans **11**
  (positive control: 89 distinct kernels present across members, query detects 5 multi-kernel orbits —
  not byte-identical to an empty read). The semantic-stance transpose (`seat_role_vector`) is
  **model-relative only** (OQ-56 Ω_E foreclosure), not part of this close.
- `logical_fingerprint.pl` stays prefix-opaque by design (per-reading fingerprint is kernel-blind), so
  the close is **(a-restricted)**, not flat.

**Evidence pointers:** `python/orbit_operator.py` + `prolog/kernel_orbit_export.pl` →
`outputs/{reading,kernel}_orbits.json`; OQ-55 (`cs_trifurcation.pl`);
`audits/2026-06-20_kernel_reading_orbits/`; KNOWN_STATE 2026-06-20; orbit canonical-fact commit (this session).

**Addendum (2026-06-26) — the RESERVED semantic-stance transpose is now built, with its
model-relativity surfaced (not claimed away).** The 2026-06-20 close ran the transpose on the
draw-robust *observer-signature* key and explicitly RESERVED the semantic-stance/seat-role
transpose as "model-relative only." That reserved surface is now built as a GAP-04 increment:
`cs_kernel_registry:cross_kernel_stance_profile/2` (+ `reading_stance/2`, `stance_cohort/2` over
the `declared_stance/2` seat) holds a declared reading-STANCE fixed and sweeps it across kernels,
keyed on the kernel-independent `logical_fingerprint:fingerprint_shift/2` (now serialized into
`pipeline_output.json`). Consumer: `python/cross_kernel_stance_report.py` →
`outputs/cross_kernel_stance.{json,md}` over the live twins. It does NOT violate the model-relativity
ruling — it READS the result as a σ/seat partition (draw-stable vs draw-variant), not a fixed label,
and surfaces it: abolition converges on `shift(*,snare,rope,snare)` 5/7 on BOTH twins (draw-stable),
deterrence flips convergent(haiku 4/1)↔divergent(flash 2/3) (draw-variant), originalist is
kernel-divergent. The cohort is a DECLARED seat (Seat-Theorem Cor 2b): morphology only suggests
candidates (witnessed: exact-stem catches 4/7 of the abolition cohort; a substring rule over-admits
the anti-abolition `dharmasastra_corpus__abolitionist_rejection`), so each member + the verdict carry
morphology-suggested-vs-hand-declared provenance. **Remaining under GAP-04/here:** kernel as a
first-class queryable object, the kernel-fixed↔transpose pair as a formal paired operation, and the
FULL curated stance vocabulary (this increment seeds an initial declared table for the exercised
stances only). Pins: `prolog/tests/test_cs_kernel_registry.pl` (transpose_* consensus-spine tests);
KNOWN_STATE 2026-06-26.

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

**Status:** resolved — within-kernel A/B/C router built + wired 2026-06-20; controls 8/8 green; all
four branches fire on the live corpus. Re-scoped off OQ-56 (within-kernel router needs no cross-kernel
vocabulary). See resolution note.
**Priority:** 1
**Deps (dropped on close):** was `blocked_on OQ-56`, dropped on the 2026-06-20 re-scope; OQ-56 now
gates only OQ-53's cross-kernel transpose leg, not this within-kernel router. No remaining dependency.
**Origin:** Kernel/reading review, 2026-06-02.
**Files:** `prolog/cs_trifurcation.pl` (`cs_reading_trifurcation/3`), `prolog/json_report.pl`
(`write_kernel_comparison_entry` → `reading_trifurcation` field), `prolog/tests/test_cs_trifurcation.pl`;
spec `docs/debugging_philosophy.md` §6.

**Resolution (2026-06-20).** New module `cs_trifurcation.pl` maps a kernel's reading-set onto
{Type A drift, Type B structure, Type C ambiguity, unknown} by dispatching on the authored obstruction
edge (`cs_kernel_obstruction_status/2`) refined by two computed within-kernel diagnostics:
- `real_closure` (a reading `forecloses` another) → **Type B**; `cs_axiom_foreclosed` on a member
  confirms (`confirmed`) but does not gate (`edge_only`).
- `licensed_plurality` (`coexists_with`) → **Type C**.
- `untyped` + `cs_drift_unacknowledged` on a member → **Type A** (sole computed branch).
- `untyped` + no drift → **`unknown`** — Pattern-5 fail-closed, an emitted verdict, NOT a default type.
- `singleton` → no verdict (predicate fails → JSON `null`).

§6 mapping confirmed against the *definitions* (not the table paraphrase): Type B = "impossible by
definition"/axiomatic inconsistency = `forecloses`; Type C = stable coexisting frames = `coexists_with`;
Type A = unmarked mutation treated-as-stable = the `false` (unacknowledged) drift gap. **Commentary-grade**
(annotates `cs_kernel_comparison`; never overrides `classify_from_metrics/6`); `scope:within_kernel`
stamped inline so it cannot be misread cross-kernel.

**Re-scope (operator ruling 2026-06-20).** OQ-55's operational core is the *within-kernel* router; every
input is per-kernel/per-member-reading and draw-robust within a kernel. The former `blocked_on OQ-56` was
a soft block — OQ-56 vocabulary only gates *cross-kernel* disagreement-labeling (OQ-53's transpose leg).
Re-scope witness = input-boundary trace: every input is gated by `cs_kernel_id(_,K)`, no cross-kernel fact
enters the verdict (`audits`/KNOWN_STATE 2026-06-20).

**Witness.** Controls `test_cs_trifurcation.pl` 9/9 green, incl. the Type-A two-twin (obstruction held at
`untyped`, drift proven the discriminator), a **single-bit** twin (`tk_drift` vs `tk_drift_ack`: direction
+ magnitude held identical via in-test unification, only the `acknowledged` flag flips false→true →
verdict flips `type_a_drift`→`unknown` — isolates the unacknowledged bit, not direction/magnitude), and a
cross-kernel-leak negative control. Live corpus: all four
branches fire (`type_a_drift`×5, `type_b_structure`×1, `type_c_ambiguity`×2, `unknown`×1 =
`polaris_document_status`) — the fail-closed `unknown` fires on real data, not a synthetic case.

**Canonical question (provenance):** When two readings of a kernel disagree, can the engine classify
*why* (Type C index ambiguity / Type A frame drift / Type B structure)? **Answered: yes, within-kernel,
commentary-grade.**

---

## OQ-56 — Reading-stance taxonomy: the selection-seat is blocked on cross-kernel clustering

**Ω-type:** Ω_P (preference/stakeholder — a declared, contestable selection premise, not derivable).

**Status:** resolved — 2026-06-20 — operator Ω_P ruling given; vocabulary pick made. Compress-on-close;
the kill condition below is a recorded reopen condition (NOT an armed tripwire — re-evaluated by a human,
not auto-detected).
**Priority:** 1
**Deps (dropped on close):** was `blocked_on_human oq56-vocabulary-pick-from-OQ-150-menu`; the pick is made.
**Origin:** Kernel/reading review, 2026-06-02.
**Files:** `python/orbit_operator.py` (`CANONICAL_VOCABULARY`, `canonical` stamp),
`docs/design/design_discipline.md` §0.1; readings (`kernel__reading_name`), `cs_kernel_registry.pl`.

**The Ω_P ruling (operator, 2026-06-20).** The canonical cross-kernel reading-stance vocabulary =
the two Tier-1 draw-robust keys: `observer_signature` (reading-unit, twin-agreement 0.722) +
`obstruction_class` (kernel-unit, 0.734). The six Tier-2 keys (incl. `seat_role_vector`, 0.245) are
carried **report-only, model-relative**, twin-agreement inline — NOT canonical. Recorded as a **checked
fact, not a memory** (Build Discipline Pattern 2): `CANONICAL_VOCABULARY` in `orbit_operator.py`,
surfaced as `canonical` on every orbit record (witness: `canonical=true` on exactly the 2 Tier-1 keys,
false on the 6 others). Declared first-person, decline-not-refute the seat-role-vector rival, in
`design_discipline.md` §0.1.

**The Ω_E finding (recorded as the real result, not buried under the ruling).** OQ-56's motivating
question — name the semantic stances (naturalizing / coordination / power-revealing) comparably across
kernels — has **NO draw-robust answer on this corpus**: the reproducible keys are structural/coarse, and
the one semantically-aligned key (`seat_role_vector`) is draw-fragile (0.245). The semantic-stance
transpose is **foreclosed-as-draw-robust**, available model-relative only. This is an Ω_E
(corpus-conditional), reopenable by a more reproducible extraction — not a permanent engine fact.

**Kill condition (reopen condition RECORDED, not automatically detected — reopens as Option 2).** If a
live downstream consumer ever *requires* the semantic-stance label (`seat_role_vector`) inside the
*canonical* vocabulary to **function** (not merely to display it), the ruling flips to canonizing it
despite its fragility ("a model-relative answer to the real question beats a robust answer to a different
one"). As of 2026-06-20 no such consumer exists — witnessed two-pronged grep (named-key +
generic-`canonical`, run both pre- and post-`canonical`-stamp), each with an `observer_signature`
positive control proving the search fires. **Detection is manual:** nothing arms this — a future session
that builds such a consumer trips no check, so the condition must be re-evaluated by hand (re-run the
Step 0 grep) when the orbit-artifact consumer surface changes. It is a documented reopen trigger, not a
live tripwire; do not trust an automatic guard that was never built.

**Evidence pointers:** OQ-150 measurement `audits/2026-06-18_oq56_twin_within_kernel_perturbation/` +
`audits/2026-06-20_kernel_reading_orbits/`; orbit_operator canonical-fact commit (this session);
KNOWN_STATE 2026-06-20. *(Body compressed 2026-06-20 per footer rule; menu/clustering history in git.)*

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

**Status:** mitigated — policy ruled (2026-06-02); cross-corpus census re-measured + the instrument
wired as a non-gating run_pipeline step + backlog right-sized and generation deferred (2026-06-20,
"CROSS-CORPUS CHARACTERIZATION" below). Residual is a deferred CONTENT decision + a standing-linter
monitor (it's a kernel-completeness problem, not edge-patching), no longer awaiting a ruling.
**Priority:** 3
**Deps (dropped on close):** was `blocked_on_human oq58-corpus-identity-ruling`; ruled 2026-06-20 (three-leg / beta posture, below).
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
items graduate into the defensible backlog. *(DONE 2026-06-20: wired as the non-gating `reading_linter`
step in `run_pipeline.py` `_phase_post_prolog`; writes `outputs/reading_reference_census.json` with
manifest+corpus_hash. Witness: pipeline 47/47 OK, "163 dangling → 158 missing / 66 kernels (5 id≥2
defensible) — NON-GATING".)*

**CROSS-CORPUS CHARACTERIZATION + DEFER RULING (2026-06-20).** Full audit:
`audits/2026-06-20_oq58_cross_corpus_incompleteness/` (read-only `census_driver.py` + saved output;
linter selftest PASS). The 2026-06-05 reset stale-ified every count above; re-measured live + twin +
archived corpora.

- **Census (rate% = distinct-missing / cs_reading_relation edges):** LIVE testsets 92 files / 169
  edges / 163 dangling / 158 missing / **93.5%**; testsets_haiku 960 / 2004 / 127 / 75 / **3.7%**;
  testsets_flash 960 / 2008 / 101 / 47 / **2.3%**; archives kernel_v1 1106 / 1774 / 94 / 86 /
  **4.8%**. (original_v5/v6/sotu author 0 committer edges — observer-only regime.)
- **Why LIVE is 93.5% — sparsity, not a frontier:** the live corpus is 97% singleton kernels (89
  kernels / 92 files, **1.03 readings/kernel**); each lone reading dangles edges to its 2–5
  ungenerated declared siblings. Mature corpora run 2.90 r/kern (~3% dangling). Concrete witness:
  `jewish_sovereignty_palestine__cultural_zionist_reading` authors 4 `coexists_with` edges — 1
  resolves (`settler_colonial`), 3 absent.
- **Regime swap (git, witnessed — corrects the earlier "kernel-completeness in testsets/" framing):**
  the 06-13 rebuild pilots BUILT `testsets/` to **1000 files / 2.92 r/kern / ~3% dangling**
  (steady-state); commit `0ccc03cf` then moved that reconciled corpus OUT into the twins
  (`testsets_haiku`/`testsets_flash`, byte-intact at 960/960 today) and `testsets/` reverted to a
  singleton topical working set (51 → 92). So LIVE is NOT mid-convergence — the reconciled ~3% corpus
  exists, preserved in the twins; the live dir is a different regime that won't converge by time.
- **GAP-07 / bounded-attractor answer (split):** dangling RATE is bounded ~2–5% across independent
  lineages (haiku/flash/kernel_v1); defensible (id≥2) COUNT ~40 is reproducible WITHIN a lineage
  (haiku 39 ≈ flash 41, haiku∩flash = 39) but NOT universal (kernel_v1 = 8, different population;
  tri-lineage common core = 1). The dangling space is bounded, not an open frontier; the census
  right-sizes the work (158 panic → ~40 real) without dissolving it.

**RULING: instrument + characterize, DEFER content generation** (downgrade is NOT permanent parking —
the ~40 are real; the linter's "tail graduates as corroborated" must be read, not filed). Quarantine
surface = leave + document (no new writer). Two generate-backlogs recorded, **generation deferred**
(independently of the corpus-identity flag below):
1. **Durable defensible set = twin-reproducible id≥2 = 39 readings** (haiku ∩ flash; same kernel
   population) — the backlog the census proved real; lives in the reconciled archive (twins).
2. **Stream-relative set = live id≥2 = 5 readings / 3 kernels**, explicitly stream-relative:
   `jewish_sovereignty_palestine` (liberal_nationalist / post_zionist / religious_zionist),
   `press_reformation_causation` (technological_determinism), `zero_mathematical_status`
   (number_reading). For deliberate kernel-completion on the live corpus.

**Quarantine-surface honesty:** `prolog/cs_reading_relation_quarantine.json` is a PER-GENERATION-RUN
artifact (last batch only), NOT the live backlog. The live, corpus-wide backlog is the linter census
(`outputs/reading_reference_census.json`) / `cs_kernel_registry:cs_reading_relation_unresolved/4`.
(Note also added at the writer, `agent/generate_kernel_corpus.py:validate_reading_relation_integrity`.)

**CORPUS-IDENTITY — RULED (operator, 2026-06-20): three live legs, beta posture.** `testsets/` is
the LIVE leg ON PURPOSE — a deliberately singleton topical working set that lets the operator
exercise the engine while building it (and surface more live issues); `testsets_haiku/` +
`testsets_flash/` are the reconciled multi-reading twins, kept as the comparison baseline. All three
are live; the singleton sparsity is intended, not a clobber or a half-finished rebuild (the reconcile
*preserved* the multi-reading corpus in the twins, byte-intact). **Currently ALPHA, working toward
BETA:** extract maximum value from the current corpus so the work earns its way to beta before any
rebuild; a fresh `testsets_*`-style rebuild comes only after schema/wiring/enough-of-ISSUES.md are
worked out — with many OQs open, a ways off. A
future instance may SUGGEST a rebuild when accumulated changes warrant it, not propose one lightly.
Promoted to CLAUDE.md Critical Distinctions ("THREE LIVE LEGS, and the beta posture"). So for OQ-58:
the deferred generation targets the live leg deliberately when kernel-completion is run there; there
is no "wrong corpus" to resolve.

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

**Ω-type:** Ω_P (research program — was a backlog ledger; items have graduated to their own OQs).

**Status:** resolved — ledger drained 2026-06-20; all still-live items now tracked individually as OQ-154–170 (plus the prior check_stack graduation OQ-142–145). A backlog ledger resolves by being *drained*, not by executing its contents.

**Priority:** 1

**Origin:** Tracking-surface consolidation 2026-06-04: AGENDA.md, AUDIT.md, TODO.md reviewed
item-by-item against the substrate and deleted (Pattern 2: ISSUES.md is the single tracker). Items
were verified STILL UNTRACKED and still live at consolidation; everything else in those files was
verified shipped (maxent_profile/4; OQ-59 #1–#4; never-generated #1), already tracked (regen-polish
backlog + 4 hard-fails in OQ-58), or moot (UNRESOLVED_MANDATROPHY count from the pre-rebuild corpus;
"scope has zero classification effect" — σ(S) is now in the canonical χ).

**Resolution — provenance map (each ledger item → its individually-tracked OQ).** Drain executed
2026-06-20 with three pre-write witnesses: (1) δ load-bearing **perturbation probe** (no-op negative
control + δ:=0.3 flip on δ's own sink → δ is live-but-zeroed, correcting OQ-162's inherited text);
(2) close-vs-keep-open **ruled from `omega_resolver.py`'s dangling-detection source** (authority set
is all parsed OQs incl. resolved, and no inbound Deps edge points at OQ-69 → safe to close, not keep
open as a thin parent); (3) the four PARTIAL **scope floors re-witnessed against the files** (157,
160, 163, 164). Splitting rule applied: split iff a real Deps edge exists between an item's parts.

| Ledger item | Becomes | Note |
|---|---|---|
| Engine-hardening pair, leg a — scope-design validator on `site_contexts/N` | **OQ-154** | independent of b/c |
| Engine-hardening pair, leg b — MaxEnt parameterization for arbitrary sites | **OQ-155** | gates OQ-156 |
| Engine-hardening pair, leg c — Arakelov fragility on 10-slice contexts | **OQ-156** | blocked_on OQ-155 |
| Spec-encoding D-1 tests | **OQ-157** | PARTIAL: 1/5 shipped (`test_maxent_profile_indexing.pl`); scopes remaining 4 |
| Cover-story detector enrichments (Pkg B) | **OQ-158** | |
| Scaffold/renewal audit (Pkg D) | **OQ-159** | |
| Cluster-level analysis → report (Pkg F) | **OQ-160** | PARTIAL: computed in `cluster_space_phase*.py`, not wired into `enhanced_report.py`; gates OQ-170 |
| Cluster systematic exploration (Pkg G) | **OQ-170** | blocked_on OQ-160 (the real F→G edge; split per operator ruling 2026-06-20) |
| Empirical 2nd/3rd cases (Pkg C) | **OQ-161** | |
| δ → baseline-deviation reframe (Pkg E) | **OQ-162** | description CORRECTED — δ is live-but-zeroed, not "not load-bearing" (witnessed) |
| Python toolset consolidation | **OQ-163** | RESOLVED: `cli.py` dispatcher shipped (1-prime, no moves); physical move → OQ-191 |
| Parameterize directionality constants | **OQ-164** | PARTIAL: 6 `canonical_d` done; `power_role_heuristic/4` + `exit_modulation/2` remain |
| framing_notes invitation calibration | **OQ-165** | |
| Incremental tabling (`as incremental`) | **OQ-166** | |
| Output write-path anchoring | **OQ-167** | |
| Author "the mint" testset | **OQ-168** | queued, not authored |
| T4 confirmed_liminal 2nd-case | **OQ-169** | dormant; trigger-deferred |
| check_stack baseline → pipeline gate | **OQ-142–145** | graduated earlier (2026-06-18) |

The 16 ledger bullets became **17 new OQs** (154–170): the engine-hardening pair is three legs and
the cluster item splits F/G. Provenance/witnesses: KNOWN_STATE 2026-06-20;
`audits/2026-06-20_oq69_ledger_drain/`. Priorities on all 17 are **provisional — operator to rule**
(the declared seat).

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

**Status:** mitigated — scale run complete 2026-06-04 (438/449); H1/H3 falsifiers fired beyond noise, H2 mixed. Phase A (zero-spend, 2026-06-20) closed the design question: the nesting NEVER reaches the generator (parent_kernel/level are sidecar-only, by deliberate design — A0), so the proposed breadth arm's strip-the-pointer reading is a provable no-op and branch 1 (depth-realized-at-generator) was never in the experiment. The 1.5× excess is the authorship-bundle (Opus identity and/or lineage-structured authoring, undistinguished); list-inflation closed across all 5 dims (A2). Mitigated not resolved: the graduation step (Opus-flat breadth, reading-(b)) splits author-identity from lineage-structure but needs spend, declined 2026-06-20.
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

**Phase A resolution (2026-06-20; zero-spend, read-only — closes the design question, defers the spend). Witnesses: `audits/2026-06-04_oq71_depth_lineage/a2_richness_alldims_results.json`, code citations below; full prose `docs/design/a_hypothesis_about_corpus_size.md` §10.1.** The disjunction (depth vs authorship) is a **trichotomy**: (1) depth-realization *reaching the generator*, (2) author-identity (Opus vs Haiku/SCOPE), (3) lineage-structured authoring (tree-thinking enriches seeds regardless of realized nesting).
- **A0 — the parent relationship never reaches the generator, by design.** `build_lineage_seeds.py:114–134` forks the generation `seeds` (→ `lineage_seeds.json`, fed to the model — **no `parent_kernel`/`level`**, summary = commitment + delta + same-kernel sibling block) from a separate `lineage.json` sidecar (`parent_kernel`/`level`, consumed post-hoc by the fingerprint join only). The generator prompt (`generate_kernel_corpus.py:430–486`) reads only flat seed fields; grep confirms it never touches `parent_kernel`/`lineage.json` (`:104` "kernel lineage is carried separately"). The origin plan `virtual-inventing-allen.md` shows this was DELIBERATE and STATED (not inferred-from-silence): it lists *"Untouched by design: generation prompt/schema/example, GEN_MODEL"* and *"Generator held fixed… Only seed authoring and output routing differ from control,"* framing the manipulation as *"depth-correlated authoring"* — the generation prompt was a frozen non-variable by design → **mitigated, not inconclusive-by-construction** (no wiring defect). **Consequence:** the proposed breadth arm reading-(a) ("strip `parent_kernel`, regenerate") is a provable no-op — nulling a field the generation seed never references changes zero bytes of generator input → `depth − breadth ≈ 0` by construction. Branch 1 was never instantiated; the instrument cannot isolate it by any seed manipulation.
- **Why no_scope is blind to nesting — by design (two-path architecture).** SCOPE path (`_scope_user_prompt`/orchestrator `_step_decompose`) hands the MODEL a topic and lets it CONSTRUCT the kernel (`is_contested_kernel` + readings array); no_scope renders PRE-DECOMPOSED readings. Batch generation forces decompose-FIRST (can't SCOPE-construct kernels inline across a batch), so the per-reading prompt is structurally blind to nesting — robust, not a field-placement accident, and inherited by any breadth arm (also batched). The CONTROL's structure was itself model-SCOPE-constructed then harvested (`build_never_generated_seeds.py` pulls `is_contested_kernel` SCOPE manifests → never-generated readings as flat no_scope seeds). So depth-vs-control at the structure level = **Opus-hand-designed nested tree vs the SCOPE model's flat decompositions**, both rendered identically at generation → "author-identity" (branch 2) = *who constructed the kernel structure*, not just who wrote prose.
- **Sibling co-channel (narrows the claim — sign cuts toward mitigated).** `sibling_reading_ids` DOES reach the prompt and its set-size covaries with tree level (Pearson r=−0.366; per-level means L0≈3.0, L1≈4.0 → L7–L9≈2.0). So the claim is "the generator never saw **parent-nesting**," not "never saw depth." **The negative sign disposes the hazard:** deeper nodes carry FEWER siblings → the channel transmits LESS contest-pressure at depth, cutting AGAINST sibling-mediated tree-position driving the excess. Scoped bounds: the sibling-SIZE effect on novelty is witnessed-flat for lengths 2–4 (control-stratification; only length-1 slopes) — bounds size, not composition; pointer-strip leaves the sibling block unchanged regardless. Sibling COMPOSITION is uncontrolled — a residual the sign argument dispositions, not a control.
- **A2 — list-inflation closed across all 5 dims.** Matched n=294, K=2000: JOINT distinct-class excess +38.7 vs every single dim's MARGINAL excess tiny (props −2.0, voids −1.7, actors −2.3, drift +2.7, zone +2.8; largest +2.8). Joint ~14× any one dim → new *combinations*, not cardinality proliferation; positive-controlled (inflated-props synthetic arm flagged). Closes the prior 2-of-5-dims caveat. (Control seed file drifted 2026-06-13, missing 26/300 ids → ran drift-immune on full frozen arms + current len2+ stratum; same verdict both views.)
- **Close, at true width:** the excess is **not attributable to generator-visible parent-nesting**; it is the authorship-bundle (identity and/or lineage-structure, undistinguished). **Graduation step (→ resolved):** Opus authors ~300 *flat* seeds, same frozen generator (origin plan reading-(b)) — splits branch 2 from branch 3, the only live question once branch 1 is out of scope. Needs spend; declined 2026-06-20; a future instance may revive it (the registered discriminator, not a new OQ). **Drift cross-link: key the control on `control_membership.json` (300 frozen ids), NOT the regenerated `never_generated_seeds.json` (drifted to 274 by 2026-06-13), or the breadth arm compares against a 274-story control believing it is 300.**
- **Construct-validity gap → OQ-171 (do not read mitigated as "§3 tested").** OQ-71 falsified only *substrate-level* boundedness (Opus/no_scope can express more classes than the live corpus holds); it never entered the **SCOPE construction path** §3's bounded-attractor claim is actually about, so §3 is left standing as within-regime. Re-engaging §3 on its own turf is **OQ-171** (context-controlled batch-of-one; the naive small-batch proxy is declined there for inheriting this exact disjunction). reading-(b) above answers a downstream "identity vs lineage-structure" cut, not §3's claim.

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

**Status:** resolved — injection-governance premise falsified by code-read (2026-06-21); the
remaining one-path/waves cleanliness is deferred-known, relocated to `design_gaps.md` GAP-15
under the alpha→beta posture. NOT a `--scope` rewire — none was done; the disposition is "this is
no longer an open question."

**Origin:** 2026-06-06 backend merge. The BUG (c-orch dropping kernels) was fixed (OQ-79 mech-1).
Two manifest-capable generation paths still coexist: the unified backend `generate_from_manifests`
(waves + the OQ-81 suppression `_flat_seeds_from_manifest`, called only by c-orchestrator) and the
legacy `--scope` flow (gkc `main()` → `build_batch_requests` → one batch, no waves →
`process_batch_results`). OQ-81 raised this to Priority 1 on the worry that `--scope` was the only
generation route the injection-suppression predicate did not govern ("probably never injected
verdicts… a code-read away from confirmed never").

**Resolution (the code-read, witnessed 2026-06-21):** the `--scope` path is **structurally
injection-free** — confirmed never, not probably. `build_batch_requests`→`build_cached_messages`
(gkc.py:419) never calls `upstream_context` (single call site :963, inside `_seed_messages` →
invoked only at :1034 within `generate_from_manifests`); the injection-carrier dict
`generated_by_id` is constructed only inside the unified backend (:1010, populated :1056) and the
`--scope` `main()` never builds it; indirection ruled out (no `globals()`/dispatch/alias to
`upstream_context`; the four `getattr` hits are jsonschema/usage/model/args). `build_cached_messages`
injects only the kernel's own authored substrate (sibling reading IDs, structural delta), never a
computed sibling verdict — exactly what OQ-81 rules safe ("kernel substrate reaches the prompt via
CSR"). So the last injection-governance gap was never a gap; the urgency that raised this to P1 is
gone.

**Disposition (operator ruling, 2026-06-21):** close. What remained after the premise died is not
an open *question* but a backlog task with a known disposition — pure one-path cleanliness + a
waves *enhancement* on a working, injection-safe, currently-dormant legacy path (live corpus has
zero kernels; the rewire can't be witnessed end-to-end now). That work folds into the eventual
rebuild and is recorded as a deferred-known design gap → **`design_gaps.md` GAP-15**. The genuinely
still-open sliver — the Streamlit-era app orchestrators could re-inject if resurrected, since the
suppression lives in the backend not the corpus — is carved out as its own entry → **OQ-172**.

**Deps:** splits_from OQ-79, splits_from OQ-81; spawned OQ-172.

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

**Status:** resolved — built + witnessed 2026-06-26 (close-state 1: repair observed in real corpus, commentary-grade detector + report surface landed, asymmetry closed). Theory note: `docs/repair_dynamics.md` §8.
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

**Resolution (operator-ruled + built + witnessed, 2026-06-26; evidence
`audits/2026-06-26_oq91_repair/`, theory `docs/repair_dynamics.md` §8).**
- **Close-branch = state 1** (operator-approved after the read-only B1-scan): real
  corpora DO contain upward runs surfaced by the existing direction-neutral
  `degradation_chain`/`snapshot_type` instrument — testsets/ 2, kernel_v1 30 (incl.
  multi-step homoousios/versailles snare→tangled_rope→rope); scan logic
  positive-controlled (`b1scan_finding.md`). **No new authored atom needed** ⇒
  repair value is NOT rebuild-deferred (close-state 3 did not fire).
- **Operative rulings (kept):** (1) structural home = dedicated repair-named
  `repair_transition/4` in `transition_paths.pl`, separate from decay-only
  `transition_path/4` (metaphors unwelded); reuses `degradation_chain/3` as the
  series source. (2) **commentary-grade, never correction-grade** — must not feed
  `classify_from_metrics/6`, the signature layer, or `verdict_join`. (3)
  `maintain/splice/replace` ARE named ops (4th arg), `scaffold_struck` the distinct
  construction-metaphor op; selection is a function of from/to + chain prefix.
- **Witnesses:** B2 standalone (lycurgan→replace, shinbutsu→replace; decay-only
  apoe4 yields none); op tally on kernel_v1 maintain-11/splice-1/replace-18 (a
  bound-Op clause-selection bug found + fixed — `repair_op` now a true function).
  B3 report surface (`enhanced_report.py:build_repair_section`, single data
  direction Prolog→field→Python). **B4 invariant PASS**: `pipeline_output.json`
  classification fields byte-identical with vs without the surface (only the new
  `repair_transitions` field added; `b4_invariant_diff.log`). Suite green (0 errors),
  snapshot-migration 10/10, warning gate 3/3 allowlisted.
- **What changed:** the engine now registers repair (a constraint lifted back up the
  ordering), not only decay — the one-way ratchet is closed. Cross-refs: OQ-90 (piton
  fixing_cost, still open), OQ-83 (committer axis), v7 Theorem 7, `docs/six_questions.md`
  (Q6). OQ-182 (bundled sibling) is independent — its A4 gate-flip remains the operator's
  call and is not affected by this close.

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

**Status:** resolved — 2026-06-10/11. The fight is defused: the per-site decision rule was RULED
(positive control passed — real sites sorted into BOTH buckets: `separability_factor` FORBIDDEN,
`natural_law_without_beneficiary` + NL gates SOUND), and the escalated benignity family (Q2, three
rows) was RULED **GATE** and the gates landed in live code with two-sided Stage-D controls. Closed
via the OQ-92 Stage-D landing (OQ-92 resolved). The former `blocked_on_human`
operator-benignity-certification gate is DISCHARGED — all three rows ruled. **Witnesses:**
`audits/2026-06-10_oq92_step3_preregistration/` (Q2 ruling in `PREREGISTRATION.md:152-180`;
`stage_d_controls.out`: CI_Rope certified → constructed_low_extraction under asserted capture →
certified, verified restore = the row-2 captured-stops-certifying witness),
`audits/2026-06-10_oq94_readsite_pass/` (12-file/33-site census + per-site sort),
`audits/2026-06-10_oq94_row2_cirope_reachability/` (row-2 reachability control, 7/7 on a fully
beneficiary-bearing live-corpus population — interception affirmatively killed). **Gates in code,
each tagged to its OQ-94 row:** `drl_core.pl:373` (scaffold `\+ constraint_captured`),
`signature_detection.pl:1112/1235/1334` (CI_Rope + `pure_coordination`),
`maxent_classifier.pl:175/194` (`boolean_spec(scaffold, constraint_captured, forbidden)` mirror).
The capture read consumes authored `gain_flow` (fabrication-banned, `data_repair.pl:122-131`,
fail-closed), NOT the fabricated `constraint_beneficiary` — so the no-op-on-current-corpus state is
the designed fail-closed behaviour (Pattern 5), not absence-satisfies-gate. Residual
`data_repair.pl:140` beneficiary fabrication is OQ-93/OQ-86 shim-family ("do not extend it"), NOT
OQ-94 debt. Compress-on-close: the operative per-site rule + benignity disposition are kept below;
the investigation narrative (read-site pass mechanics, probe-scope truncation lesson, the
witnessed-twice scaffold misfire) is in git history.
**Priority:** 1

**Operative ruling — the per-site decision rule (RULED 2026-06-10).** No global exclusion between
capture and coordination. `has_coordination_function/1` (one clause: `constraint_beneficiary(C,_)`,
`narrative_ontology.pl:303`) may be gated on not-captured WHERE the read's question is
mountain-likeness ("holds without enforcement"); gating is FORBIDDEN where the read needs
coordination-despite-extraction (the tangled_rope/snare-adjacent cell — `separability_factor`,
`drl_boltzmann_analysis.pl:110-125`). The rule derives from tangled_rope's definition (capture and
coordination co-occur by construction), independent of the inverted Boltzmann anchor that provoked
it. Classification-path adjudication is RULE-APPLICATION over the read-site pass output, not open
judgment.

**Operative disposition — the benignity family (Q2, three rows, all GATE).**
- Row 1 `drl_core.pl` scaffold clause: GATE — misfire-witnessed (the step-2 prototype split
  capturer seats to scaffold), two-sided control pinned, maxent mirror same-commit. The P≠NP
  precedent does NOT apply: the gate reads authored `gain_flow` capture, not beneficiary presence.
- Row 2 `signature_detection.pl:1019` CI_Rope: GATE after the reachability control — reachability
  witnessed (gate runs on an all-beneficiary population, interception killed) + the misfire witness
  produced in the Stage-D two-sided control. Fail-closed posture: gating now means the worst case is
  a no-op clause; NOT gating would fail-open on the highest-stakes read during the least-trusted
  data's first pass.
- Row 3 `signature_detection.pl:1122` `pure_coordination` subtype: GATE, riding with row 1
  (commentary-grade, consistency-with-row-1).

**Cross-refs:** OQ-92 (gain_flow build + Stage-D landing that closes this), OQ-90 (snare/piton
capture split, unblocked on the built surface), OQ-93/OQ-86 (`data_repair` beneficiary fabrication
— separate hazard), GAP-10 (closed), OQ-83 (role-derived beneficiary emission). XPrize who-bears-vs-
who-benefits residue: `audits/2026-06-10_external_review_xprize/`.

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

**Status:** resolved — 2026-06-26 (SCOPED), operator ruled **gate the danger class**; the
frozen-evidence route now gates in `scripts/gate.sh` (empty/GREEN today, RED on recurrence).
Two residual routes named below stay non-gating by ruling, each with a kill condition. Filed
2026-06-11 (OQ-33 close session; operator-flagged class); checker built 2026-06-18 (ungated).
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

**Built 2026-06-18 (stays open, ungated) — `python/audit_citation_status.py`.** Standing,
re-runnable, sibling of `issues_status.py`/`known_state_status.py`; **NOT in `scripts/gate.sh`**
(ungated until the FP rate is ruled). Invariant: a cited path **exists AND is git-tracked, OR is
allowlisted-ephemeral**; missing and untracked-not-allowlisted are one class. WARN stages by
three sublabels, three destinies: `untracked-pending` (real `outputs/*` evidence ref;
`--promote-untracked` lifts to ERROR when the set is empty-or-allowlisted on a fresh clone),
`missing-pending-M` (`--promote-missing` lifts when every survivor is classified, not when the
set is empty), `grammar-ambiguous` (globs/ellipsis/descriptive dir mentions — **never promotes**).
A gitignored path inside the repo root is **never allowlisted** (it IS the OQ-104 signature);
allowlist = `~/`, `/tmp/`, paths escaping repo root, decided AFTER normalization.

**Witnessed census (`audits/2026-06-18_oq104_citation_checker/`):** 1224 path-citations over 85
audit dirs. **untracked-pending = 35 distinct, ALL `outputs/*`** (plan projected ~32; +3 is
witness-corrects-projection); reading every context, all 35 are **descriptive references to
canonical regenerable `outputs/` artifacts** (schema docs, data-flow, CLI defaults, command
lines), **none the dangerous frozen-evidence origin class** (those were remediated at the
2026-06-11 anchor `09390f0f`). **Disposition (operator ruling 2026-06-18):** (i) copy-into-audit-dir
inapplicable (`outputs/` is regenerated → today's file ≠ historical snapshot → copying = Pattern-3
faith-merge); (ii) allowlist forbidden (OQ-104 signature); → **leave flagged, non-gating;
`--promote-untracked` deferred.** **missing-pending-M = 66 distinct** (drove 278 plan-upper-bound →
66 via the four-plus FP-class rules); every survivor classified — relocation/rename,
illustrative/proposed-structure, archive-directory shorthand, deleted-gitignored-output — **no live
broken citation hides in the bucket**, so `--promote-missing` stays deferred. Controls: `controls.py`
23/23 (one positive control per new exclusion + a `/etc/passwd` field-list bug it caught),
`controls_run.sh` idempotence + rot-sensitivity (tracked cited file → `git rm --cached` →
flips pass→flag). **Two promotion conditions and the brace/glob + descriptive-outputs seats are
recorded as the wiring triggers; `scripts/gate.sh` unchanged.**

**Resolved (scoped) 2026-06-26 — gate the danger class.** Operator ruling: the real OQ-104
danger (a frozen, unique evidence file a fresh clone needs and lacks — the spectral_laplacian
origin) is **distinguishable** from the benign class. An untracked cited path is dangerous **iff
it does not start with top-level `outputs/`** (repo-root `outputs/` is rebuilt by every
`run_pipeline`; an `audits/.../outputs/` evidence file or any other gitignored frozen ref is
*not*). Implementation: the single `untracked-pending` sublabel split in
`audit_citation_status.py:classify()` by `c.startswith("outputs/")` into
`untracked-frozen-evidence` (**GATING** — intrinsic ERROR, no flag) and `untracked-regenerable`
(non-gating WARN). `--check` exits 1 iff frozen-evidence non-empty OR parse `problems`; wired as
the 7th `run` line in `scripts/gate.sh` (`audit cites`). **Witnessed base rate zero today:** all
39 distinct untracked paths are under `outputs/` → `untracked-frozen-evidence:0` → gate GREEN;
fires only on a genuine recurrence. Controls now 25/25 (matched-pair frozen-vs-regenerable
isolating the prefix as the deciding variable + a dotted `./outputs/` post-normalization control);
end-to-end RED-on-frozen / GREEN-on-removal and rot-flip both witnessed. Evidence + the gate run:
KNOWN_STATE 2026-06-26; the amended `audits/2026-06-18_oq104_citation_checker/` (FINDINGS gating note).

**Scope of "resolved" — one of two origin routes mechanized; do not over-read.** The gate fires
iff a new untracked **non-`outputs/`** cited file appears; it does NOT automate the per-item
regenerability/benignity reading the 2026-06-18 hand audit did. Two residuals stay non-gating by
ruling (both defensible at base rate zero, both with kill conditions):
- **Route gated:** worktree/ignore-rule frozen evidence (exists-locally, gitignored, not under
  top-level `outputs/`) — the spectral_laplacian origin. **Now RED on recurrence.**
- **Residual — typo'd path (NOT gated):** a typo'd evidence path doesn't exist → lands in
  `missing-pending-M` → non-gating, because a typo is checker-indistinguishable from a benign
  relocation reference (gating `missing` would FP on all 70). *Kill condition:* a typo'd evidence
  citation lands in `missing-pending-M` and ships GREEN.
- **Residual — convention-not-invariant (NOT gated):** `startswith("outputs/")` encodes
  "regenerated by `run_pipeline`" but tests the *prefix*. A frozen artifact parked in top-level
  `outputs/` (gitignored → exactly where one parks gitignored things) reads `untracked-regenerable`
  → silent. *Open sub-decision:* if `run_pipeline`'s output set is enumerable, tighten the predicate
  to membership in that set (closes the hole); else accept the named residual. *Kill condition:* a
  non-`run_pipeline` file under top-level `outputs/`, cited and untracked, ships GREEN.

`untracked-regenerable` (39) + `missing-pending-M` (70) stay non-gating standing surfaces
(promotions deferred, by the 2026-06-18 ruling).

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

**Ω-type:** Ω_E (witnessed, mechanical; zero output impact on the live corpus).

**Status:** resolved — 2026-06-18. RETIRED (not fixed): operator ruled `prolog/archives/datasets/*`
out of scope (no backward-compat), which closes the bridge's only genuine consumer.

**Priority:** 1

**Diagnosis (confirmed).** `data_repair:bridge_omega_variables_pure/3` guarded its module lookup on
the BARE interval id, but testsets declare their facts in module `constraint_<id>`, so the guards
always missed and the predicate returned `[]` — it imported zero omegas on every report run (Build
Discipline Pattern 6: success-shaped absence; OQ-99's wrong-module twin). Live path:
`scenario_manager.pl:114` → `repair_interval/1` → `bridge_v34_data/2` → the bridge. Authored omegas
already reach reports WITHOUT it: testsets author `narrative_ontology:omega_variable/3` directly
(`report_generator.pl:709`) and the 5-arity protocol renders at `:776-794` (the OQ-99 fix). The
bridge's only genuine purpose — synthesizing the 3-arity fact for v3.4-legacy UNPAIRED testsets —
has no in-scope input (live corpus 100% paired; unpaired inputs are archive-only).

**Resolution.** Removed `bridge_omega_variables_pure/3` + its `bridge_v34_data/2` call + the
now-dead `persist_single(omega_variable(...))` dispatch clause, with tombstones in `data_repair.pl`.
The deferred capability is logged as **GAP-13** (`docs/design/design_gaps.md`) with the
re-introduction recipe (re-key on `constraint_<id>` per the OQ-99 template, both positive controls,
a non-fabricated type). A secondary defect retired with it: the /5 branch fabricated type
`empirical` for a typeless 5-arity fact.

**Witness (paste-or-untag).** Pre-removal probe on
`border_control_legitimacy__freedom_of_movement_primary`: `bare_module_FALSE` /
`constraint_module_TRUE` / `five_arity_in_constraint_module_TRUE` / `bridge imported 0 omegas` (the
no-op fired — not a didn't-run zero). Removal is behavior-preserving: ZERO DIFF on three
omega-authoring reports (border_control, catastrophe_memory_kernel__boundary_maintenance_reading,
animal_status_kernel__property_reading) across both raw `run_scenario` output and
`enhanced_report.py`. Dynamic suite GREEN (80 passed, 0 failed, 0 errors). Provenance:
KNOWN_STATE 2026-06-18.

## OQ-112 — Pattern-6 confirmed-candidate batch from the OQ-97 census: 8 classes, member-level sort and per-class disposition

**Ω-type:** Ω_E (sites enumerated with read witnesses; C4a + item 2 + item 7 + item 4 fixed; items 3,5,6,8 witnessed-latent across all three live legs, declared-not-landed).

**Status:** resolved — 2026-06-23 (close-out, witness `audits/2026-06-23_oq112_closeout/`).
Combined witness pass (Part A bite-check on 1/2/4/7 + Part B kill-conditions on 3/5/6/8 +
cross-corpus check on both twins) → **DECLARE-AND-STOP**: only item 1 touched live output
(13/92 abductive `agrees`→`unavailable`, **headline-neutral**); items 2/4/7 latent-hardened;
items 3/5/6/8 do NOT fire as live bites on `testsets` (92), `testsets_haiku` (960), or
`testsets_flash` (960) — the absence-gates are masked **upstream** (epistemic/sufficiency guards
require the same metric family the downstream gate needs; structural, not 92-sparsity). No headline
ever flipped on any live leg from any OQ-112 fix. Fix-shapes for 3/5/6/8 recorded in the writeup,
not landed (latent-hardening judged not to earn its spend pre-rebuild). **Declared scope boundary:**
the pre-de-leak archives (`kernel_v1`/`original_v6`/…) were not swept (retrospective-audit breadth,
OQ-89 pattern; re-runnable). Resolution history below.

**Status (historical):** open — filed 2026-06-11 at OQ-97 close; **item 1 (C4a) RESOLVED 2026-06-22**
(Round 1, witness `audits/2026-06-22_oq112_round1/`); **item 2 (A10-widened + completion gate)
RESOLVED 2026-06-23** (Round 2, commits `4ee4ce08`+`0ef5bf6d`, witness
`audits/2026-06-22_oq112_round2/`); **item 7 (wasserstein incomparable-mass provenance)
RESOLVED 2026-06-23** (Round 2, witness `audits/2026-06-22_oq112_round2/item7_*`); **ROUND 2
COMPLETE**.
**Dual-status (both true; the second is NOT subsumed by "COMPLETE"):** "Round 2 COMPLETE" is the
round level; at the gate level the item-2 maxent completion gate is **live-fire UNEXERCISED on the
92-corpus (0/92 latency), live trigger named as the falsifier** — without this line "COMPLETE"
misreads as "gate proven live," which the 0-of-92 line specifically denies. Item-7 is likewise
output-identical on the live 92 (absent/errored arms 0-firing; 344/344 cells genuine float incl.
measured 0.0) — the contract widening is forced-witnessed (4-state control), live-UNEXERCISED.
**item 4 (A3) RESOLVED 2026-06-23** (Round 3, witness `audits/2026-06-23_oq112_round3/`; Commit 1
landed alone — Commit 2 DROPPED and Commit 3 DISSOLVED into Commit 1, both premises falsified by
Round 0, see Round 3 update below); items 3,5,6,8 staged.
**Round-4 gate (the arc's own kill-question, 2026-06-23):** before spinning up Round 4 on items
3/5/6/8, point to **one verdict a user actually saw change** across everything OQ-112 shipped
(items 1/2/4/7) — or declare the arc **latent-hardening** (Pattern-6 hazards closed before they
went live) and stop. Items 2 and 4 both gate on an absence condition that does not occur on the 92
corpus (every claim-bearing constraint carries all metrics; maxent always completes), so the honest
preliminary read is latent-hardening pending that positive control. Naming the question, not
resolving it by producing more rounds.
**Priority:** 1
Full class tables with member lists: `audits/2026-06-11_oq97_pattern6_census/WRITEUP.md` §4–§5.

**The batch (priority order, per OQ-44's common-law prioritize-by-success-shapedness;
re-ranked 2026-06-11 after the item-4 trace — see update below):**

1. **C4a — `; Signal = agrees` on absent probe input** (`diagnostic_summary.pl`, 13 sites).
   **RESOLVED 2026-06-22 (Round 1).** Member sort (read witness, all 12 probes): 10 sound · 3
   defects. Discriminator: `agrees` is sound after the probe predicate *succeeded* with a positive
   no-tension result (`none`/`[]`/`H1=0`/no-override/good-zone); a defect when reached from the
   `catch(_,_,fail)` else (data-absence). Fixed (commit `4e6cf6e9`): `:198`/`:212`/`:163`
   `agrees`→`unavailable`. `:198` (`probe_abductive`) is the only LIVE site — 13/92 constraints
   have no `abd_triggers` fact (producer `abductive_report.pl:401–404` omits no-hypothesis
   constraints; loader asserts no fact); was counted as agreement, now dropped. Output-changing at
   the agreements list, **headline-neutral** (join verdict identical for all 92). `:212`
   (`constraint_signature` total → unreachable) and `:163` (`classify_disagreement` total over 5
   shapes → unreachable) fixed as fail-closed hardening. The 10 sound sites
   (`:154,:173,:179,:196,:210,:234,:237,:268,:272,:274`) are left as-is.
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

**Update 2026-06-22 — Round 1: corpus pinned, item 1 (C4a) resolved, items 2–8 re-staged.**
Evidence: `audits/2026-06-22_oq112_round1/WRITEUP.md` (+ `pinned_corpus.txt`, `probe_before.tsv`/
`probe_after.tsv`, `probe_controls.txt`, `diagnostic_summary_fix.diff`).

- **Corpus pin (self-witnessing): LIVE=92** (membership emitted, not just a count; manifest
  `pipeline_run_at=2026-06-22T02:03:39Z`, `ab8d1d7`). Negative control: bad `corpus_path` throws
  `corpus_empty`, `testsets_haiku` overlay → 960 (overlay-took ≠ default-fallback). Consumer-
  predicate check: the diagnostic path enumerates `corpus_constraint/1` (`json_report.pl:64`), not
  a sibling table. **The inherited 62-row (item-4 SILENT) and 194-row (item-3 NEUTRAL) verdicts
  are facts about THOSE sets — they re-anchor to 92 in their own rounds before being cited.**
- **Item 1 fix witness:** probe reproduces the real diagnostic path; deterministic within harness;
  85/92 join verdicts match the committed snapshot (the 7 that differ are bidirectional pre-existing
  non-determinism / stale `abductive_data.json`, not introduced here). Before→after: exactly the 13
  `has_abd=no` rows flip `agrees`→`unavailable`; join column identical for all 92.
- **`:198` producer-side follow-up (recorded, not in scope):** the fix also routes the
  *measured-empty* case (`abd_triggers(C,[])`, currently never produced) to `unavailable`. The fully
  Pattern-6-correct form carries the provenance bit at authoring time — producer emits `[]` for every
  corpus constraint + an `abductive_loaded` witness fact; consumer returns `agrees` on `[]` vs
  `unavailable` on missing-fact. Producer+loader+consumer, output-changing; folded into item-2's
  completion-fact design (same shape: positive completion witness + fail-closed on its absence).

**Corrected staged designs (each carries its 92-corpus re-witness obligation):**
- **Item 2 (A10-widened, channel absorbers) — CORRECTED.** Do NOT ship `catch(Goal,E,assert_failure)`
  alone: `catch/3` is blind to *failure* (W12a clause-failure before arithmetic, catch-free; W12b
  `catch(_,fail)` error→dropped row). Invert the default: emit `maxent_completed(N, witness)` on
  genuine completion and **fail-closed in `verdict_join` on its absence** (subsumes the loud-channel
  option). Item-2 positive control must **force a clause failure**, not just a `type_error`.
  Re-witness item-4 SILENT on 92 first. One deferred ruling (`blocked_on_human`): may a maxent stage
  ever legitimately emit zero constraints?
- **Item 3 (A6, 5 unmeasured sites)** `purity_scoring.pl:71,80,88`, `drl_boltzmann_analysis.pl:302`,
  `drl_fpn.pl:206`, `covering_analysis.pl:137`, `signature_detection.pl:1090`: tripwire measurement
  on the pinned 92 ("NEUTRAL on 194" is not standing), then fail-closed per statute on any live site.
- **Item 4 (A3 idiom cleanup):** dead branches confirmed / live firing empty on 62; re-witness on 92,
  then the idiom cleanup.
- **Items 5 (C4b blind=stable), 6 (A2 statistic-on-empty), 7 (A10 catch→0.0 — folds into item-2's
  completion-fact design), 8 (low: C4c/A7/B2):** report-grade; staged per success-shapedness order.

Disposition per class follows the OQ-44 statute for any site touched (fail-closed on absence,
pass carries witness); output-changing fixes land alone per the established commit discipline.
Cross-refs: OQ-97 (census), OQ-44 (statute + common-law queue), OQ-98 (C4a feeds its join),
OQ-43 (A6 semantics), OQ-93/OQ-96 (pattern provenance), OQ-110 (A4 lineage).

**Update 2026-06-23 — Round 2: Round 0 re-witness on 92 + item 2 RESOLVED + the zero-legal ruling.**
Evidence: `audits/2026-06-22_oq112_round2/` (`WRITEUP.md` Round 0, `INDEXED_ASSERT.md`, `GATE.md`,
probes + outputs). Three commits: `d69d5d39` (Round 0 + the witness-truth controls), `4ee4ce08`
(indexed completion assert), `0ef5bf6d` (the verdict_join gate + widened absorbers + invariant).

- **Round 0 re-witness on 92** (the inherited 62-row item-4 verdict re-anchored): the sink throw
  `type_error(evaluable, unknown/0)` is REAL — witnessed isolated AND profile-present (not the
  LL=−10.0 empty-table non-witness) — but LATENT: all 6 unknown-suppression constraints on 92 are
  claim-less, so maxent discovery drops them (N=86 enter) and the throw is unreachable via the live
  driver. Zero-with-witness count = 0 across `:555`/`:734`.

- **Deferred zero-legal ruling — RULED (B) defer; falsifier SUPERSEDED (two conditions, two probes).**
  The Round-1 wording above recorded ONE condition —
  > "One deferred ruling (`blocked_on_human`): may a maxent stage ever legitimately emit zero
  > constraints? … falsifier = any stage emitting `maxent_run_info(_,0,_)`."

  W2 proved that single condition INSUFFICIENT: the only path to a zero-output stage on 92 is
  upstream-pruned (the claim-less exclusion), and that pruning is *itself* the latent hazard item 2
  exists to gate. Ruling: **(B) defer** the zero-with-witness producer/loader/consumer handshake,
  under **TWO falsifiers**:
  1. any maxent stage emits `maxent_run_info(_,0,_)` on a live corpus → fork (A) forced; re-check
     via Round-0 **W3** (`probe_reachability_and_zero_92.pl`).
  2. any **claim-bearing** story lacking `suppression_requirement` appears (count 0, W2) → the sink
     fires, the stage voids, `run_info` is **absent** (NOT zero-with-witness, so the gate catches it
     by absence); re-check via the **item-4 reachability probe** (`probe_reachability_and_zero_92.pl`
     W2b), **NOT W3** — W3 counts zero-with-witness and is blind to the claim-less→claim-bearing
     transition.

  [EDGE] "defer-B safe" ≠ "hazard absent": defer-B rides on the upstream pruning that is the hazard;
  two different claims, two different falsifiers (above).

- **Item 2 (A10-widened + completion gate) — RESOLVED.** Distinct per-stage completion witnesses
  (`maxent_run_info` classical; the NEW `maxent_indexed_run_info` for the indexed stage, asserted
  strictly after `maxent_classify_all_indexed`); attempt markers `diagnostic_summary:maxent_attempted/1`
  set at the json_report stage boundary before the absorbing catch; `verdict_join` fails closed via
  `maxent_void_alerts/1` — **per-attempted-stage, at the consumed (default) context**, never "any
  completion present" (so classical's present fact cannot mask an indexed void — the cross-term);
  absorbers widened to `( catch(G,_,fail) -> true ; true )` so a stage FAILURE continues the run
  (catch/3 is blind to plain failure). Severity **moderate/yellow** (operator ruling 2026-06-23: a
  void is absence-of-measurement, not a measured-severe finding). AGENTS.md "completion-witness-or-
  fail-closed" invariant landed in the gate commit. The distinct-fact decision is load-bearing:
  the throw-arm witness shows classical `run_info` PRESENT while indexed ABSENT — a shared fact would
  read clean.

- **Gate status — forced-witnessed, live-UNEXERCISED (recorded at true strength).** The gate is
  witnessed against FORCED throw + FORCED plain-fail (the `:871-874` no-priors guard, the catch-blind
  channel the inverted default exists for) + N=0-legal-passes + the classical/indexed cross-term
  (`GATE.md` matrix). `LATENCY/92` = 0 of 92 voided under normal completion → the gate's LIVE-fire
  behaviour is unexercised **by construction** (the live corpus produces no void). This is "verified
  against forced controls", NOT "verified live on 92"; the live trigger is falsifier (2) — the item-4
  reachability probe — not a re-run on today's 92.

- **Item 4 (A3 idiom cleanup) — RESOLVED 2026-06-23 (Round 3, Commit 1 landed alone).** The
  maxent-local accessors (`get_constraint_metrics/4`, `metric_value/3`,
  `get_constraint_metrics_indexed/5`, `metric_value_indexed/4` in `maxent_classifier.pl`) now return
  the `unknown` sentinel (OQ-44 pattern) on absence of base_extractiveness / extractiveness_for_agent
  / theater, instead of a fabricated `0.0`; the two dead `;Supp=0.0` branches removed (Round-2 W3:
  `get_raw_suppression` always returns its sentinel). `maxent_threshold_proximity/4` gains a
  `number/1` fail-closed guard. **Blast radius contained to `maxent_classifier.pl`** (Round-0 recon:
  the local accessors have no cross-file consumers). Witnesses (`audits/2026-06-23_oq112_round3/`):
  WA — 0 sentinels produced over the 86 claim constraints on 92 (else-branches unreached → genuine
  values byte-identical, **live-unexercised on 92**); WC — constructed theater-absent claim
  constraint → `maxent_indexed_run` throws → `run_info` absent → **item-2's completion gate fires**
  (indexed void alert → verdict_join caps headline).
  - **Round 0 falsified two of the three pre-registered commits, so they did NOT land:**
    - **Commit 2 (findall silent-drop) DROPPED.** Its premise — a silent findall-drop that "item-2's
      gate structurally cannot catch" — is contradicted. `sum_list` is OUTSIDE the findall and
      *throws* on `unknown` (MECH witness); the throw aborts `maxent_compute_profiles_indexed`
      (`:897`) BEFORE `maxent_indexed_run_info` is asserted (`:905`), so item-2's gate already floors
      it (WC proves this end-to-end). The only genuinely-silent path is the benign ≥2-sample
      `default_profile` fallback. No separate fix warranted.
    - **Commit 3 (boundary external-crash) DISSOLVED into Commit 1.** Its premise — an un-wrapped
      external caller of `maxent_boundary_analysis` crashes — is false: that predicate has **zero
      callers**, and `maxent_threshold_proximity`'s only two live callers (`maxent_report.pl:211`,
      `maxent_diagnostic.pl:395`) are already `catch`-wrapped. The fail-closed `number/1` guard is
      folded into Commit 1 (the commit that introduces the `unknown`) as hardening-at-point-of-
      introduction; `boundary_analysis` adjudicated **unfinished value, not cruft** (per-constraint
      nearest-edge fragility view; the dual of the live per-boundary report) → wire-it opportunity
      logged as **GAP-19**, not retired.

- **Item 7 (wasserstein `catch→0.0`, `json_report.pl:438–442`) — RESOLVED 2026-06-23 (Round 2,
  landed alone).** Replaced the 4 `catch(_,0.0)->true;0.0` arms with `wm_token/3` (float | absent |
  errored) + `wm_emit/3` (serializes float | null | `"errored"`). The helper carries a fourth-state
  guard (succeed-with-unbound-M → `errored`, fail-closed against a malformed JSON hole); that state
  is unreachable through the STATIC producer, whose `extract_chain_probs` terminal `is/2` always
  binds Mass on success — the guard is defensive against a future producer change.
  `schemas.py:228` inner-value contract widened in-comment (the `(…, dict, True)` tuple is
  unchanged and still validates: 0 errors over the regenerated output). **Witnesses**
  (`audits/2026-06-22_oq112_round2/`): `item7_wm_token_controls.txt` — 4-state forced control, all
  PASS (genuine 0.0→`0.000000`; nonzero→`0.400000`; absent→`null`; errored→`"errored"`;
  unbound-M→`"errored"` via the guard, with the shipped clause pasted so the control goal is
  diff-able against the guard subterm). `item7_before_after_diff.txt` — item-7-isolated diff (clean
  BEFORE regenerated at HEAD `a5593f7` with item-7 reverted vs AFTER): **ZERO other top-level fields
  moved, ZERO wasserstein cell flips**; 344/344 live cells are genuine float (incl. measured 0.0,
  correctly NOT collapsed to null), absent/errored arms 0-firing on the live 92 → output-identical,
  contract widening **live-UNEXERCISED**. `item7_schema_validation.txt` — 0 schema errors.
  `0.0` stays a *legal measured value*. The realized in-repo numeric-reader set was empty (grep
  bounded to in-repo; out-of-repo/notebook float-readers are unwitnessed — a per-context value read
  as a float now gets `null`/`"errored"` where the state was absent/errored).

**Update 2026-06-23 — CLOSE-OUT (combined witness pass + cross-corpus check → DECLARE-AND-STOP).**
Evidence: `audits/2026-06-23_oq112_closeout/` (`BITE_RULING.md` pre-registered, `WRITEUP.md`,
`repin_92.txt`, `partA_*.txt`, `partB*.txt`, `crosscorpus_{twin,flash}.txt` + probes). Resolves the
Round-4 gate above: the demanded "one verdict a user actually saw change" does **not** exist on any
live leg → the arc is **latent-hardening**, confirmed (not asserted).

- **Re-pin:** LIVE=92, membership byte-identical to Round-1 pin, 86 claim subjects; negative
  control overlay-took (haiku→960).
- **Part A (bite-check, field-level ruling recorded pre-data):** item 1 = the only live-touching
  fix — **13/92** abductive `agrees`→`unavailable`, **HEADLINE-NEUTRAL** (join identical for all
  92); items 2 (0/92 void alerts under completed run), 4 (0 sentinels), 7 (344/344 wm_emit cells
  genuine float; the 18 top-level nulls are the PRE-EXISTING outer else `json_report.pl:452-454`,
  not item-7-produced) = **latent**. Positive control: forced indexed void on a green constraint →
  headline green→yellow, flagged by the comparison (the "no headline bite" claim is falsifiable).
- **Part B (kill-conditions, 4 items / 4 distinct controls):** 3/5/6/8 do **NOT** fire as live
  bites. The v1 guard-predicate sweep over-counts; **consumed-output reachability** is the witness:
  item 3 (A6) — the 6 excess-absent constraints short-circuit at `epistemic_access_check=false` →
  `purity_score: null` (value never reaches a consumer); item 5 (C4b) — corpus is NOT synchronic
  (1569 series) but all 14 `stable` trends are MEASURED small-deltas (absence FAILS, not reads
  stable); item 6 (A2) — canonical `system_gradient`→`open` (Pattern-6-fixed), cohomology empty-
  fallbacks don't fire (N=92>0), `contextuality_fraction`=0.0 is measured H¹=0; item 8 (C4c) — the
  6 are `boltzmann_compliant=inconclusive(insufficient_classifications)`, never false-clean. A7
  needs a design ruling; B2 benign in pipeline order.
- **Cross-corpus (the scope-widening that turned "latent on 92" into "structurally latent"):** the
  A6/C4c gates re-checked on both denser live twins — `testsets_haiku` (960: 494 epistemic-true, **0**
  excess-absent-reaching-purity, 129... 43 compliant, **0** rode pass(no_extraction_data)) and
  `testsets_flash` (960: 748 epistemic-true, **0**; 129 compliant, **0**). **0 live bites on all
  three live legs.** Masking is **structural** (epistemic/sufficiency guards require the metric
  family the downstream gate needs). Archives NOT swept (declared boundary, OQ-89-pattern breadth).
- **Disposition:** items 3/5/6/8 **witnessed-latent, fix-shapes recorded (WRITEUP §Disposition),
  NOT landed** — latent-hardening judged not to earn its spend pre-rebuild. No engine `.pl` edits
  this round. **Methodological yield:** a guard-predicate count over-reports a Pattern-6 firing;
  consumed-output reachability — and checking it on more than the sparsest corpus — is the witness.

## OQ-113 — natural_law_signature/1 is unsatisfiable by construction on the live corpus: has_viable_alternatives/2 never returns `false`; pure_natural_law subtype unreachable

**Ω-type:** Ω_E (witnessed, mechanical; Pattern-5 absence-gate stack).

**Status:** resolved — 2026-06-18. Fork (b) ruled (document builder-unreachability;
no logic change) — and (a)/(c) DISSOLVED by evidence, not preference (below). Limb-2
honesty defect FIXED (silent dead clause → loud throw tripwire). The residual capability
gap is routed to **GAP-08 §7 author-independent immovability signal**, not a standing
defect. Compressed on close per the footer rule.

**The witness (limb 1, detector dead-by-range).** `has_viable_alternatives/2`'s range is
exactly `{true, unknown}`; `false` is builder-unreachable. `natural_law_signature/1`
requires `HasAlternatives == false`, so the predicate is unsatisfiable on every corpus.
Probe (2026-06-18) across all three live-era corpora — live `testsets` (79) + twins
`testsets_haiku` (960) + `testsets_flash` (960) = 1,999 stories, each loaded with the
overlay-took-effect witness (`corpus_count` = 79/960/960, not silently the default):
positive control `natural_law_signature(profile(0.92, 0.02, 0.04, 0, false, stable, _))` →
`control_fires` on every corpus; live `natural_law_signature` → `live_firings=0` on every
corpus; `has_viable_alternatives` returning `false` → `hva_false_count=0` on every corpus.
Locked as regression: `prolog/tests/test_oq113_dead_natural_law.pl` (3/3 green).

**The witness (limb 2, subtype unreachable).** `determine_pure_subtype/2`'s
`pure_natural_law` branch gates on the dead detector → 0-firing. Converted to
`throw(unreachable_pure_natural_law(C))` — behavior-neutral today (witnessed:
`structural_purity` over all 1,999 live-era constraints — `testsets` 79 + `testsets_haiku`
960 + `testsets_flash` 960 — → `structural_purity_all_ok_no_throw` on every corpus),
loud the day a corpus/schema powers the detector (doubles as the §9b.2 KILL).

**Why (a) — extend the builder to emit `false` — is not available now (dissolves, by
absence of signal).** (1) The metrics are non-diagnostic: clean and contested mountains
degrade byte-identically `[mountain,rope,rope,mountain]`
(`audits/2026-06-17_mountain_authoring_sweep/universal_degradation_witness.txt`), so an
opened gate rides burned metrics (§9a(iii)). (2) The only reliable discriminator is absent
live: the strong leg is a non-mountain `constraint_classification` seat (caught 161/370
contested in v6); the **live corpus authors 0 `constraint_classification` facts**
(it authors the CS/committer-axis schema instead — `cs_axiom_contradiction`,
reading-relation edges), while kernel_v1 authors ~9,000. (3) The remaining `false`-sources
are falsified — prose/omega-`false` is the contaminated source GAP-08 rejects (370/627
engine-mountains carry "impossible alternative" prose AND a contested reading). So (a)
collapses into "first build the GAP-08 §7 author-independent immovability signal," which
does not exist. **(c) — retire the leg — is killed by §9a(iii):** removing the detector's
only discriminating leg makes it fire non-discriminatingly on the burned metrics. **(b) is
forced.**

**Cross-refs / closure.** OQ-43 (sibling `BeneficiaryCount==0` leg) and OQ-44 (gate-class
policy) are both **resolved** — the sibling/policy legs are closed. OQ-128 retired the
`resolve_modal_signature_conflict(_, natural_law, mountain)` override (0/3843 behavior-
neutral), demoting the detector to a wired-but-dark router socket. OQ-37 (`unknown` sentinel
discipline — the sentinel works correctly; the consumer's `== false` test is what can never
see it). Residual capability → GAP-08 §7 (author-independent immovability signal) +
`audits/2026-06-17_mountain_authoring_sweep/ROUTING_SINK_DESIGN.md` §9a(i)/§7.

**Deps:** blocked_on_human GAP-08 §7 author-independent immovability signal (a residual capability gap routed to a design gap, not an OQ edge)

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

**Status:** resolved — fixed 2026-06-18 by side-loading `abductive_helpers` in `stack.pl`;
check_stack returns to the 4-finding 2026-06-04 baseline (no abductive line). Filed
2026-06-12 by the B4 gauntlet reconciliation (the one check_stack divergence not on the
expected-divergence manifest).

**Priority:** 1

**The phantom.** Under bare `[stack]`, `signature_detection:signature_grade/2`
(`signature_detection.pl:1624`) calls `abductive_helpers:known_override_signature/1` but the
module was a phantom (`current_module` TRUE, `module_property(_, file(_))` FAILS) — the call
threw existence_error. Pipeline unaffected (loads it via json_report → diagnostic_summary),
so the green B4 gauntlet hid the gap.

**Resolution.** Option 1 (import in signature_detection) REJECTED by evidence — it cycles
tighter than the in-file comment documented: `abductive_helpers → maxent_classifier →
signature_detection:constraint_signature/2` (`maxent_classifier.pl:60`) back into
signature_detection, plus the grothendieck→drl_core arm. Option 2 (`stack.pl` side-load
`use_module(abductive_helpers, [])`) is the safe fix; the falsified `:1611-1617` comment was
corrected. **Witness (cold `[stack]`, corpus-free):** before → `THREW`
(`existence_error(procedure, abductive_helpers:known_override_signature/1)`); after →
`RETURNS`. check_stack before: 5 findings (incl. abductive); after: 4 (baseline). Provenance:
KNOWN_STATE 2026-06-18; commit at close. The other 4 baseline findings → **OQ-142** (class
sweep: one unguarded bite, OQ-115; the rest guarded/dead — phantom×guarded×reachable
discriminator). Lineage: OQ-57 (wrong-qualifier class), OQ-98 (the alert path that minted
the reference).

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

## OQ-118 — Draw-stability tracks field-construction-type, not the σ/seat line: the cohort's analysis contract (successor to OQ-109's σ/seat residual)

**Ω-type:** Ω_E (the re-tests are measurable) + Ω_P (where the seat boundary sits is a theory ruling).

**Status:** open — filed 2026-06-13 (successor to OQ-109's σ/seat residual, discharged here not
answered). The **2026-06-27 re-probe** (`audits/2026-06-27_oq118_reprobe/`, commit `fc57e833`,
re-runnable `oq118_reprobe.py`) **discharges narrow-A** (σ/seat is not the stability partition) and
re-scopes the rest into the four limbs below; **broad-A and the cast graded re-test remain open;
the verdict class is RULED accept-as-confounded (2026-06-27), its one temp sweep held in reserve.**
The frozen σ/seat prediction
(`audits/2026-06-12_cohort_zero/SIGMA_SEAT_PREDICTION.md`, `5f2a626c`) was **falsified-as-tested**.

**Priority:** 1

**The replicate spend (settled substrate; `audits/2026-06-12_cohort_zero/`, commit `dcfaea97`):**
15 draws = 5 contested kernels (`qwerty_path_naturalization`, `free_market_naturalization`,
`total_war_unthinkability`, `printing_press_reformation`, `zero_as_number`) × 3, batch
`msgbatch_01UbfPq13BcHgJKxcsqK549i`, `claude-sonnet-4-5-20250929` @ temp 0.2, seeded from
`prolog/kernel_seeds.json` through the FROZEN seed-spec (title+domain+summary) so the prediction
applies. Instruments: `python/cohort_stability.py` + `python/cohort_sigma_seat_eval.py` (Fisher
exact validated vs scipy to 6 sig figs).

**The finding, in four limbs (2026-06-27 re-probe; witness `audits/2026-06-27_oq118_reprobe/`):**

**Limb 1 — narrow-A: DISCHARGED. σ/seat falsified as a *stability predictor* (not as theory).**
Stripping presence-hollow fields (apparatus + prose-presence — the comparator sees only PRESENT vs
EMPTY) from both arms does not rescue the partition; it **inverts** it. Replication arm (hollow IN)
reproduces the instrument's original 58/62 | 36/32, 47.9%, p=0.6490 — validating the comparator map
by reproduction — while the content-only arm (hollow OUT) drops to 26/58 | 18/24, 39.7%, p=0.2348,
the σ side decontaminating *toward* the unstable cast multisets. The inversion, not the bare p, is
the witness. (Corrects the prior text: apparatus-presence's 6/6 is presence-trivial, not "firmest.")

**Limb 2 — broad-A: OPEN (hypothesis carried by a floor contrast).** Construction-type, not
generation-regime, governs the stability of *generated* fields. Positive witness: four generated
authored scalars — `suppression` 5/6, `accessibility_collapse` 4/6, `theater_ratio` 3/6,
`resistance` 3/6 — all non-degenerate (between-story variance > 0, Probe 4) and all clearing the
free-composed cast multisets at 0/6. The contrast is a **floor** (every authored scalar reproduces
above the cast's zero), **not** a within-class gradient — stability and spread do not co-vary
(`accessibility_collapse`, smallest spread 0.14, sits mid-stability). `extractiveness` is
known-flagged (`cohort_stability.py:186`) and excluded from the witness set (sanity row only). Not
discharged: three "stable" fields are degenerate constants witnessing nothing (`emerges_naturally`,
`claimed_type`, `has_sunset_clause`; `omegas.count` likewise degenerate — correcting the prior
"omega count … draw-stable"), and "authored scalars reproduce as a class" is an unrun positive test.
**Graduation (b):** a pre-registered construction-type partition whose positive-control burden
includes the variance/degeneracy sweep (Probe 4 is the template). The verdict class is **bracketed
as temp-confounded by the Limb-3 ruling** — broad-A's positive test rests on the authored-scalar
floor contrast, not on the verdicts as a clean member.

**Limb 3 — verdict class → RULED accept-as-confounded (2026-06-27, operator).**
`disappearance_verdict` (4/6, 2 distinct) and `founding_problem_status` (4/6, 3 distinct) are the
seat-predicted *content* class, non-degenerate (Probe 4) but confounded with temp 0.2 — a strong
summary may over-determine the verdict (generation-regime cause, not σ). **Ruling:** the verdict
class is treated as temperature-confounded **by declaration, not measured**; the one temp sweep
(~$1.5–2: 30–45 draws = the 5 cohort-zero kernels × 3 × {2–3 added temps} @ ~$0.045/draw Sonnet
batch, OQ-119 rate) is **held in reserve**, not spent. **Marked-unknown dependency:** *does any
live claim need the verdicts ACTUALLY stable, or only the confound acknowledged?* — a corpus
question cross-linked to the blast-radius enumeration (**OQ-190**). The verdict class is the content class
cross-story claims (OQ-75) lean on; **if that enumeration finds an OQ-75 claim resting on verdict
stability being real, the reserved sweep becomes load-bearing and runs then** — cheaper before the
claim hardens than after.

**Limb 4 — cast graded re-test: cheap on spend, NOT on instrument.** The 17 raw draws are on disk
(`audits/2026-06-12_cohort_zero/replicates/`), so re-analysis needs no spend — but the field-level
graded distance metric does not yet exist and owes its OWN positive control (renamed-same-cast near
/ fresh-cast far, τ calibrated against OBSERVED drift not synthetic permutation) before any read
counts; exact set-match conflates "fresh cast" with "renamed cast"
(`audits/2026-06-12_signature_identity_witness/`). Story-level ranges already overlap (within max
0.543 ≥ between min 0.500), so the field-level metric may return ambiguous *by construction* at n=6
— honest framing: "cheap re-analysis that will probably report it needs spend," not a guaranteed
cheap win. Frozen prediction + control spec: `audits/2026-06-27_oq118_reprobe/HALF1_PREDICTION.md`
(commit `e9efbacd`; a NEW test, never a retrofit of `5f2a626c`).

**Blast radius (the invalidation — named, not enumerated):** accepting Limbs 1–2 retroactively
suspects any claim resting on cast-field stability — the `reading_diff` re-point (cohort-one-gated),
OQ-75 cross-story claims, per-story roster/beneficiary/vindicated mechanisms. *Suspects named from
memory, not from a corpus sweep — the enumeration is itself an open item, now tracked as **OQ-190**.*
**This enumeration also prices the Limb-3 reserved spend** (it is the same corpus-grep wearing two hats: the agent who runs
it must check both whether a claim leans on cast-field stability *and* whether one needs verdict
stability real — the second answer is what un-reserves the temp sweep).

**Successor fork (parked):** a σ/seat partition with apparatus/presence pulled out as its own
declared bucket, re-run fresh, is a legitimate pre-registerable test (not a retrofit) — out of scope.

**Discipline carried forward:** escalate-don't-redraw — every re-probe this session was a fresh read
of the frozen draws, never a retrofit of the failed prediction.

**Related cohort-one item (carried from OQ-109):** the `reading_diff` re-point is COHORT-ONE-gated
— `constraint_stakeholder/7` is Unknown procedure on the live corpus, so it has no live positive
control; pick up when a stakeholder-cell-bearing story lands (inert-proving-inert otherwise).

**Cross-refs:** OQ-109 (parent; σ/seat residual discharged here), OQ-26 (ε generated-not-invariant
— the determinism-frontier context), OQ-75 (Stage-2 cross-story claims that this analysis contract
gates), the signature-identity witness (KIND-not-story identity; the graded-metric template).

**Origin:** 2026-06-13, OQ-109 Phase C σ/seat falsifier result + operator ruling (split, with
verdict-stability demoted to confounded-half; discharge-to-successor).

---

## OQ-119 — Join-structure fed-vs-withheld diff: does feeding move the cross-examination (not just the type)? — RESOLVED (2026-06-21): verdict layer moves, committer invariant (Theorem-7 holds)

**Ω-type:** Ω_E (engine-characterization — answered by the three-axis fed-vs-withheld spend).

**Status:** resolved — 2026-06-21 (`audits/2026-06-21_oq119/WRITEUP.md` + RESULTS.md; Gate-0
substrate witness `audits/2026-06-21_oq119_gate0/`). Filed 2026-06-13 from the OQ-117 (c) ruling.
**Priority:** 1
**Deps:** blocked_on_human spend-go

**ANSWER (96 Sonnet generations, 5 kernels × 16 readings × {withheld,fed} × 3 draws; per-axis rule
`median(D_A) > max(F_A)`, generation-stochasticity floor measured per axis, observer de-weighted):**
**Feeding moves the join at the DIAGNOSTIC VERDICT layer and leaves the COMMITTER structure invariant.**
- **verdict layer MOVES (4/5 kernels):** feeding the mountain claim — with parties held fixed, so the
  authored ε/suppression stay high — forces the `MOUNTAIN_METRIC_CONFLICT` seat divergence and
  escalates the `false_natural_law` signature from **commentary → correction** grade (+1 alert),
  witnessed e.g. acceptable_risk withheld `yellow/commentary/1` → fed `yellow/correction/2`. The
  cross-examination responds to the fed claim *without* moving the broad type (the sharper-than-OQ-117
  effect). Honest caveat: this is substantially the *claim-gated* FNL path, so semi-expected.
- **committer obstruction/divergence INVARIANT (0/5): Theorem-7 detection-independence HOLDS,
  measured not assumed.** Feeding an observer-side claim does not re-author which sibling readings
  foreclose. The honest form is "no effect beyond generation noise" — withheld redraws already flip
  `real_closure↔licensed_plurality` (committer floor nonzero on 4/5); on the one clean-floor kernel
  (ai_governance, F=[0,0,0]) feeding moves it exactly zero. NO violation.
- **observer + temporal-rate move SOFTLY (2/5, 3/5):** labile, low-information, as predicted; the
  de-weighting kept them from carrying the headline.

**Method hardening that made the answer trustworthy** (all pre-draw, committed):
- substrate re-witnessed open (Gate 0): 325 kernels clear all three axes on `testsets_haiku` — the
  2026-06-13 blocker text was pinned to the stale 5-kernel OQ-117 probe corpus.
- generator corrected: single-story `cohort_replicate_batch` authors NO `cs_` facts (committer dead);
  the **kernel-regen path** (`generate_kernel_corpus` no-scope) authors `cs_reading_relation` —
  committer axis live in both arms.
- confound closed: bare "mountain" overlaps the schema's no-parties exemption (OQ-149 `allOf[0]`);
  **fed framing holds parties fixed** so fed-arm coverage loss can't masquerade as the effect.
- Sonnet override (Haiku dropped schema-required `stakeholders[]` → OQ-149 gate fired loud, 2/5
  coverage; Sonnet → 5/5). Schema/prompt UNTOUCHED (OQ-149/OQ-83 exemption is deliberate).
- k frozen pre-draw (median-vs-max), observer de-weighted (the headline cannot ride observer wobble).

**Second-order thread (NOT resolved here):** the committer axis is **generation-noisy** (withheld
obstruction flips across redraws) — consistent with **OQ-149** (committer/CS the most model-divergent
layer). Whether a larger draw budget resolves a sub-noise committer effect is open; route to OQ-149.

**The question (engine-characterization, NOT a (c) gate — (c) is resolved).** OQ-117 showed feeding
the mountain claim does not change the engine's *type* verdict (0/30 both arms). A sharper question
remains: does feeding move the **join structure** — which observer seats disagree, which axioms
dissent, how the drift trajectory runs — even when the final type holds? That is whether feeding
shifts the cross-examination the engine performs (perspective × axiom × temporal), per the seat
theorem (`docs/seat-theorem-v1.md`).

**Substrate gate — WITNESSED OPEN on `testsets_haiku` (2026-06-21, `audits/2026-06-21_oq119_gate0/`).**
The original blocker text below was pinned to the stale 5-kernel OQ-117 probe corpus; the haiku twin
moved past it. Witnessed (engine queries, not greps):
- **observer** — live; 4-seat χ spreads non-degenerate; `extractiveness_for_agent` fails to
  `null`, no 0.5 fabrication path. *Caveat:* per-power stakeholder authoring is sparse
  (`power_witness_map` ≈ 0 on many roster stories) — the spread is a real engine computation
  (context-driven) but not per-seat authored; a moved seat = moved ε/directionality, not a
  re-authored stakeholder (recorded for the spend).
- **axiom** — DISCHARGED. The "`cs_kernel_id` absent" claim is stale: 327/328 multi-reading kernels
  carry a typed obstruction status (220 real_closure / 107 licensed_plurality / 1 untyped);
  `cs_kernel_divergence` fires (readings classify differently at some context); status is
  fail-closed on absence (no agreement fabrication).
- **temporal** — DISCHARGED. Its cited sub-blockers **OQ-93** (grid) and **OQ-33** (fabricated
  suppression) are both **resolved (2026-06-11)**. The `Backed` bit separates known-rich (pass) from
  known-thin (fail) and is reachable in both states (positive control). NB: the engine reads
  `measurement/5` series, NOT the prose token `coercion_grid` (not a predicate).
- **joint cell**: **325 kernels** clear all three axes simultaneously (kill condition was <3).

**Comparator built + validated (Phase 1, `python/audits/oq119_join_diff.py`).** Self-diff=0,
cross-kernel-diff>0 with named moved fields, within-kernel shift resolvable (≈0.27), numerical
resolution linear to ~5e-4. The binding floor is the **withheld-redraw generation-variance floor**
(OQ-26), measurable only with spend — the pre-registration mandates ≥3 withheld redraws/story to
establish it.

**Next move = the operator's spend-go.** The discriminating metric + per-axis prediction + spend
spec are FROZEN pre-draw in `audits/2026-06-21_oq119_gate0/PREDICTION.md` (≈30 draws, ~$2 — same
draw count as the OQ-117 spend that ran; reuses the `oq117_spend_driver.py` structure; draws are
probe artifacts, none join the live corpus).
A notable frozen sub-prediction: `obstruction_status` moving under feeding would be a
**detection-independence (Theorem 7) violation** and a finding in its own right. **Do not run the
fed arm without the ruling.** The Ω_E question itself stays OPEN — this session opened the substrate
gate and built the instrument; it did not answer whether feeding moves the join.

**Original blocker text (superseded 2026-06-21, kept for provenance):** *"On the OQ-117 probe corpus
the join is instrumented on only ~1.5 of its 3 axes … committer/CS vantage no-opped (`cs_kernel_id`
absent) … temporal thin-to-vacuous (grid 32/32 absent OQ-93; `classify_at_time` carries the OQ-33
fabricated-suppression issue). Graduation: a corpus where observer + axiom + temporal are each
witnessed non-vacuous — then the diff is clean."*

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

## OQ-121 — Totalization convention for the commentary family + domain-relative census coverage (the `extraction_silent` collapse)

**Ω-type:** Ω_E (the fix is mostly MECHANICAL — the domain gate `extractive_type(dr_type)` is already computed; totalizing the predicate just stops throwing it away on non-fire) + a small Ω_C residue (two declared seats: report prevalence alongside coverage; classify the unnameable-blindspot case as covered).

**Status:** resolved — 2026-06-16. The census-honesty core is built: `extraction_reading` totalized to the never-fail family shape, census coverage made domain-relative, prevalence separated from coverage. Witnesses below. Residual (remaining family members) folded into the extension point, not a standing defect.

**Priority:** 4

**Deps:** bundled_with OQ-134 (the generic census this hardens), bundled_with OQ-86 (the `extraction_reading` source totalized here)

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

**Status:** resolved — 2026-06-18 (physics-RED fixed by OQ-128; FSM victim-discriminant handed to OQ-138; the `oq122-fsm-victim-gate` branch is DROPPED, not merged)
**Priority:** 1
**Deps:** bundled_with OQ-128

**RESOLUTION (2026-06-18) — the bundled OQ-128 resolution discharged this entry; the FSM gate is superseded, NOT merged.**
- **Physics-RED FIXED by OQ-128, not by the FSM gate.** The type_1 cap was *discriminated*
  (degrade→snare = severe / degrade→other = informational; `drl_core.pl:629–638`). On live (commit
  `2172d55`, manifest 2026-06-18) both physics false-positives now read
  `verdict_join.verdict = yellow`, `cap_applied: none`, `type_1_false_summit = informational` at every
  seat. The RED that filed this OQ is gone. OQ-122's graduation step (b) named "OQ-50 power-scaling
  residue … the binding verdict-mover" — that became **OQ-128**, now resolved; the (b) text below is
  superseded.
- **FSM victim-gate (`oq122-fsm-victim-gate`, `ab1e9b26`) — DROPPED, superseded by the engine-ROUTES-
  never-RECLASSIFIES architecture (OQ-128/AGENTS.md).** The gate is a *suppress-the-detector*
  reclassification — exactly the shape OQ-128 removed. Its verdict benefit is now ≈0: the RED is
  already cleared, and `false_summit_mountain` only contributes a `signature_correction/moderate`
  alert while `base_verdict` is *independently* yellow (`cap_applied: none`), so suppressing FSM cannot
  recover GREEN here either. (The "36-fixture cost" was already shown stale corpus-drift,
  `audits/2026-06-14_oq122_fixture_triage/`.) The branch's single-clause diff lives at commit
  `ab1e9b26` (recoverable) and the gate's load-bearing witness — the clean `testsets_flash` partition
  **40 FSM-firings = 18 `vic=0` + 22 `vic>0`** (`audits/2026-06-13_oq122_retype_discriminator/breadth_sweep_results.txt`)
  — is **handed to OQ-138** as the discriminant for the FSM clause.
- **The victim INSIGHT survives, re-shaped for OQ-138.** "A mountain-claim with no victim has nothing
  to conceal" is correct; apply it the OQ-128 way — **discriminate the FSM signature's severity**
  (`false_summit_mountain ∧ vic=0 → informational/route`; `∧ vic>0 → moderate/floor`), the exact
  analogue of the type_1 ε-split, NOT a detector gate. Recorded as the proposed FSM disposition in **OQ-138**.
- **neutron_star / FCR** stays under **OQ-70** (bait-confound), as graduation step (c) already routed it.

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

**Open graduation steps (all output-changing) — SUPERSEDED 2026-06-18, see RESOLUTION above:** (a)
branch DROPPED (suppress-the-detector shape rejected by OQ-128; discriminant → OQ-138); (b) the
power-scaling residue became OQ-128, RESOLVED; (c) neutron_star/FCR → OQ-70. Original text retained for
provenance: (a) **FSM victim-gate — IMPLEMENTED on branch
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

**Status:** mitigated — dominant lean ruled SIGNAL against a pre-registered, positive-controlled
convention control (2026-06-27, `audits/2026-06-27_oq124_oq149_committer_convention_control/`);
minority counter-direction + the third-model spend remain open.
**Priority:** 1
(`audits/2026-06-13_twin_comparison/`). The recurring `signature` disparity over matched ids is
`constructed_high_extraction` (haiku side) ↔ `false_ci_rope` (flash side), in BOTH directions
across many constraints — the two models systematically foreground different structural codings of
the same authored substrate. Per OQ-70 this is STRUCTURAL-coding disagreement, NOT a detection
claim (signature prevalence is bait-confounded). Open question: which authored fields drive the
fork (extractiveness magnitude? the CI-rope gate inputs?), and is the lean a stable model
fingerprint (re-test on a third model / a fresh twin draw). Cross-ref OQ-70.

**RESOLUTION — Field A ruled SIGNAL (2026-06-27, convention control).** Both twins re-classified at
one commit (`bbf5c92`; the existing outputs straddled the OQ-138 ROUTE conversion of `false_ci_rope`
+ `constructed_high_extraction`, so were non-comparable). Positive controls held (claimed_type
0.7208, cs_kernel_id 1.000). The fork is **strongly asymmetric**, not "both directions" —
`haiku=CHE / flash=FCR` 157 vs `haiku=FCR / flash=CHE` 12 (~13:1). Decomposition + a two-sided
`with_retracted` control (`retraction_control.log`) show the dominant lean is **continuous
extraction-magnitude signal, not template convention**: 0/157 dominant forks ride the
`constraint_claim(rope)` template slot alone (all have min ε ≤ 0.45 rope ceiling → FCR via
`low_extraction_profile`), and on all 157 the haiku side authored higher `base_extractiveness`
(flash authors systematically lower ε: 0.508 vs 0.565, cross-twin Spearman 0.86). So the fork is a
threshold-crossing of a model-characteristic extraction calibration → **the signature lean carries a
model index** (flag for v8 §3 / OQ-72 invariant claims). The 12-slot minority direction carries a
real template component (5/12 source-1-only) and is **OPEN** (meets neither pinned bar). **Third-model
spend now WARRANTED** (the convention branch did NOT fire) — gated follow-up, spend-go stays the
operator's. Per-field detail in the audit `FINDINGS.md`.

**Instance (essay-review triage 2026-06-26, `moral_causation_locus` twin).** The
dispositional/situational reading pair both carry `structural_signature:
constructed_high_extraction` at borderline confidence (~0.0097) with
`override_mismatch(constructed_high_extraction, snare)` — a witnessed case of the haiku-side
extraction lean firing uniformly at near-zero confidence (the reviewer also reports
F006 / coordination_washing / T1–T6 uniform; signature + confidence witnessed here, the T-code
detail not re-verified). Reinforces the "detector disposed to produce extraction" reading; the
uniform institutional=rope "convergence" the report reads as coordination is partly a config
artifact — see OQ-188. Reports: `outputs/constraint_reports/{dispositional,situational}_reading_report.{md,json}`.

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

**Ω-type:** Ω_C (was the engine's mid-seat degradation of an authored mountain a diagnostic signal or a power-scaling artifact — a seat-indexed reading question).

**Status:** resolved — 2026-06-17 (ANSWERED = artifact; FIXED + BUILT via the routing architecture). Compressed on close per the footer rule; the full 538-line arc is in git history (commits cited below; `git log --oneline --grep OQ-128`).

**Origin:** 2026-06-14, split from OQ-122's residual (so it stopped floating in prose). The engine degrades authored mid-power mountains to `rope`; the question was whether that flags a false foundational claim (diagnostic) or is a χ = ε·f(d)·σ artifact.

**ANSWER (witnessed): ARTIFACT, non-diagnostic.** Mid-seat degradation is UNIVERSAL — it hits clean `thermodynamics_entropy` / `fermats_last_theorem` identically to contested `axiom_of_choice`. (The "confirmed artifact" was retracted mid-arc as over-claimed, then re-validated against converged controls.) Degrading an authored classification is the engine producing a DIFF — its job — not a verdict to be manufactured.

**FIX (operator architecture: the engine ROUTES disagreement, it NEVER reclassifies; only review reclassifies). Three builds, all witnessed:**
1. **Routing sink** — `prolog/routing_sink.pl`: per-SEAT author↔engine diff → seven typed MECE addresses; taps `dr_claim_mismatch/4` UNMODIFIED; wired into `run_pipeline.py` Phase 2; consumed by `enhanced_report.py`. Design: `audits/2026-06-17_mountain_authoring_sweep/ROUTING_SINK_DESIGN.md`. (commits `e2de9b7b` sink, `77364550` wire, `63f6b6b3` address split)
2. **natural_law overwrite RETIRED** — `:867` `resolve_modal_signature_conflict(_, natural_law, mountain)` removed; the detector lives as a socketed (unpowered) router input. Witnessed behavior-neutral: the resolver fired **0/3843** across six corpora, `dr_type` byte-identical. (commit `9211be01` build, `057b8adb` witness)
3. **type_1 cap DISCRIMINATED** — the `severe` floor was OVERLOADED (fired identically on the non-diagnostic artifact AND on real false summits). Split so degrade→snare = `severe` (withhold) and degrade→other = `informational` (routes, no headline floor). Discriminant validated: clean ε gap in the mountain-claimed population (snare ε≥0.50 / rope ε≤0.25, nothing between, KILL=0 across six corpora ~7,000). Acceptance: RED **389→102**, all 10 v5 mountain-claimed snare-at-analytical STAY RED, `dr_type` byte-identical. (commits `0a629077` build, `f6da1063` record). **KILL:** a mountain-claimed snare-at-analytical authored at 0.25<ε<0.50 breaks the gap — re-run the χ-decomposition when a new corpus lands.

**Residuals SPLIT OUT (so this entry CLOSES rather than floats):** **OQ-138** (the rest of the `resolve_modal_signature_conflict` overwrite family — route vs reclassify per clause), **OQ-139** (green-base reads "safe" but means "consistent" — a labeling thread), **OQ-140** (characterize author↔engine divergence — what a routed diff MEANS, not where it goes). **Socket-powering = GAP-08** (the disposed `intent_*` declared-absence; the §7 author-independent immovability signal does not exist — detector multiply-dead, fires 0/~7,000). **Update 2026-06-23:** GAP-08's intent-attribution predicates are now **declined on principle** (operator ruling — a logic engine must not impute a third party's intent; reads effect + self-declaration only, `design_discipline.md` §4). So the socket is not "awaiting an intent-layer revival" — that path is closed; any future socket signal comes from the CS committer axis, never the retired `intent_*` surface.

**Cross-refs:** OQ-122 (parent residual), OQ-50 (false-summit detector), OQ-98 (verdict_join base/floor split), OQ-70 (FNL bait — blocks OQ-138's `false_natural_law` member), GAP-08 (socket signal), OQ-138 / OQ-139 / OQ-140 (children).

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

**Cross-link (OQ-35, 2026-06-21):** `cs_reference_frame/2` is the authored committer t0 anchor this
tier needs. It is currently inert consumption (serialized at `json_report.pl:590`, no join). OQ-35
RETAINs it **on this OQ's bet**, with the kill condition: when this tier ships, the t0→t1→t2 join
either materializes (vindicates the retain) or is cut (then OQ-35 strips the emission).

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

**Status:** resolved — 2026-07-02. v8 adopted (operator plan-approval ruling; spec Q4 = **wholesale**); paper authored; invariant machine-enforced (pre-built); vocabulary migration executed.

**Priority:** 3

**Deps:** bundled_with OQ-15

**Origin:** 2026-06-16, the seat/orientation invariant audit + R3 presentation-vs-structure probe (`audits/2026-06-16_seat_invariant_vs_prolog/`); design spec rev1–rev3 (`docs/design/v8_seat_gauge_orientation_design_spec.md`).

**Resolution (2026-07-02; KNOWN_STATE 2026-07-02).** (1) **Paper:** `docs/deferential_realism_paper_v8.md` authored (commits `4ea2c2d5` + `16143c15` review-response Appendix) carrying every spec obligation: §5.4 bridge table verbatim, §5.7 kill-condition, §6.4 ε declaration discipline handed forward, Theorem 2 |real-seat| caveat in-body (→ OQ-195), two-seat retirement in prose. Verification witnessed at commit: obligations grep-checklist, docs-wide two-seat sweep (8 hits, all allowed senses), number-regime sweep, fresh-agent self-containedness control 7/7. (2) **Guard** (spec §8 item 1) was pre-built via OQ-15 Phase 1: `prolog/check_axis_boundary.pl` + `python/check_axis_boundary.py`, gate-wired (`scripts/gate.sh`), both positive controls fire; closed GAP-12; evidence `audits/2026-06-23_oq15_crossaxis_witnesses/`. **Dead-hash note:** the guard-landing commit recorded here and in GAP-12 as `fd1ee561` no longer resolves in repository history (likely rebase/squash casualty; the only main commit touching the guard file is `f6921ac1`) — cite the guard by artifact + gate wiring + audit dir, never that hash. (3) **Migration wave:** README rewrite (`7c4cca6f`), CLAUDE.md orientation + canonical-paper pointer (`64a44514`), cross-ref/vocabulary notes in `seat-theorem-v1.md`, `one_seat_audited.md`, `design_discipline.md`, `metrics_as_routing.md`, `paper_versioning.md` (v8 row), AGENTS.md pointers, memory sweep (2 files), foundations README link fix. **Residual risk (named, accepted):** the guard is *static* reachability over loaded clause bodies (descends into meta-call constructs); goals **constructed at runtime** (`=..`/`call` chains built from data) are invisible to it — same class as OQ-137's opt-in-registry residual; mint separately if the operator wants it closed. Unblocked: OQ-03 empirical limb (03b).

---

## OQ-136 — What the commentary census measures: corpus authoring gaps vs genuine structural categories

**Ω-type:** Ω_E (provenance-clustering test + bounded hand-read) + Ω_C tail (per-bucket disposition rulings) — both discharged.

**Status:** resolved — 2026-07-02. Evidence in (pre-registered) + all six dispositions ruled by the operator and executed same day.

**Priority:** 3

**Deps (dropped on close):** was `blocked_on_human oq136-bucket-disposition-rulings-R1-R6`; ruled 2026-07-02 (recommendations executed as given). Bundled: OQ-121/OQ-134 (census), OQ-83 (q6 source), OQ-86 (extraction source).

**Origin:** 2026-06-16, the totalized commentary census — once the absence buckets stopped collapsing into silence, they became findings about the corpus needing an artifact-vs-genuine discrimination.

**Resolution (2026-07-02; PROPOSAL frozen `0ba48b4c` BEFORE any join; execution `2b66dedc`; evidence `audits/2026-07-02_oq136_census_bucket_provenance/` — PROPOSAL/RECON/membership.tsv/contingency_tables/stats_output/HANDREAD/WRITEUP; all controls fired; Holm family BY RULE = 4 powered buckets × 2 artifact axes = 8).** Findings at n=119: **q6_unmeasured (26) + no_agent_seats (26) = authoring artifact** (clustered on model AND prompt_commit, p_holm=8e-4; strata: haiku 16/28 each + ALL 9 provenance-less `*_contradictions`; 25/26 overlap = ONE generation-path gap — the prose plans the seats/fields the fact layer never emits). **q6_signature_unknown (16) = kept as genuine config-variant** — **one-legged verdict** (statistical leg only; the pre-registered hand-read leg was not run for this bucket — operator-flagged, ruled knowing that). **manufactured_consensus_candidate (9) = genuine** (8/9 hand-read RULED with in-file witnesses; 1/9 radiative_levitation false-positive BY ITS OWN TEXT → excluded-role vocabulary gap). **extraction_unnameable (3, unpowered) = compound** (seat limb = the haiku artifact; victim limb genuine-to-the-reading 2/3 RULED).

**Rulings executed (operator, 2026-07-02):** R1/R2 → **OQ-202** minted (ONE generation OQ covering both artifact buckets, haiku + contradictions strata named; extraction seat limb included per R5's compound reading; **R6 folded in** — the `*_contradictions` provenance-stamping gap is the same path, one witness). R3 → q6_signature_unknown kept, one-legged caveat written into WRITEUP + this close. R4 → mcc first-class reporting GO → **OQ-204**; excluded-role vocabulary → **OQ-203** (standalone — different fix site than OQ-202). R5 → extraction_unnameable bucket kept (post-OQ-202 membership = the genuine both-sides-unnamed residue). The census `no_agent_seats` out-of-domain declaration is RATIFIED (was provisional pending these rulings — the bucket was ruled artifact, the mapping stands; `prolog/commentary_census.pl` comment updated).

**DENOMINATOR CAVEAT (still-operative citation rule — kept intact per the compress-on-close exception; witnessed `census_sweep.py` 2026-06-16).** Before reading any census *rate* (prevalence) or *coverage* across a config / schema-refit / corpus change: a rate can move PURELY by domain-shrink with NO change in the underlying finding. Witnessed: `tangled_rope_chi_floor` 0.35→0.85 raised extraction `prevalence` 0.060→0.067 (+12%) while `extraction_blindspot_fired` held at **3** — 5 extractive constraints just fell out of the domain (`n_in_domain` 50→45). Always report the raw `fired` count and `n_in_domain` ALONGSIDE the rate, or hold the domain fixed. Likewise q6 `coverage` decomposes into a config-INVARIANT authoring component (`q6_unmeasured`) and a config-VARIANT computational one (`q6_signature_unknown`) — never read q6 coverage as a single authoring-completeness figure. Clustering-style tests use raw counts per stratum, never rates. Tool + findings: `audits/2026-06-16_census_sweep/`.
---

## OQ-137 — Census the reading layer for the typed-absence convention (does every aggregatable predicate carry its absence as a type, not a silent failure?)

**Ω-type:** Ω_E (a mechanical audit — enumerate the reading/verdict predicates, test each for the never-fail-on-domain property with a positive control) + a small Ω_C tail (classifying a predicate as "aggregatable reading" vs "genuinely relational lookup" is a judgment, recorded per predicate).

**Status:** resolved — 2026-07-02. Census done (41 predicates classified), 2 silent defects + 1 doc-key trap fixed, standing guard landed (registry + suite + pipeline gate).

**Priority:** 3

**Deps:** bundled_with OQ-121, bundled_with OQ-134

**Origin:** 2026-06-16, generalizing the OQ-121 ad-hoc totalizations (3 predicates by hand) into a systematic reading-layer census + standing check.

**Resolution (2026-07-02, commits `a81d4c83`/`2453b922`/`486756fe`/`ed851eb7` + gate commit; evidence `audits/2026-07-02_oq137_reading_totality/`, KNOWN_STATE 2026-07-02):** `prolog/reading_registry.pl` (aggregatable_reading/3: 16 total_on_domain + 25 partial_by_design with stated reasons; 5 domain specs; census_source_backing/2 anti-fork bridge) + registry-driven `prolog/tests/test_reading_totality.pl` (exactly-one per total entry; both positive controls fire) + sequential fail-fast gate in `run_pipeline.py` `_phase_prolog` (deliberate-break control witnessed red / clean green). Defects fixed: `explain_signature/3` had no `unknown` clause and its consumer composition silently TRUNCATED the report signature section (planted-fixture witness 0/110 → 111/111); `cs_terminal_attractor/4` rows overlapped (duplicate + order-dependent terminals; made row-disjoint, first-solution preserved on all 42 combos); `cs_has_axioms/1`/`cs_axiom_inconsistent/2` doc key "+C"→"+UID" (constraint-name key never fires, silently). `test_cs_drift_engine.pl` rebuilt self-contained (was red since the corpus reset deleted its fixtures; 11/11 green). Scope note: "resolved" covers the four swept families (stakeholder_seats, signature_detection, cs_*, report_generator read sites) — classification_table.md records the out-of-scope remainder per family.

**Residual risk (named, accepted at close):** the registry is OPT-IN — a new reading predicate that never registers escapes the totality suite entirely (the anti-fork bridge covers census sources only). Proposed follow-up if the operator wants it closed: a registration-coverage lint sweeping module exports for corpus-keyed verdict predicates absent from `reading_registry.pl` (mint as its own OQ; not minted unilaterally).

## OQ-138 — `resolve_modal_signature_conflict` family: which signature OVERWRITES should route, not reclassify? (Ω_C + Ω_E)

**Ω-type:** Ω_C (per-clause ruling: is each overwrite a load-bearing correction or a manufacturing reclassification the router should replace?) over an Ω_E base (a mechanical before/after `dr_type`+headline diff per clause).

**Status:** partial — FSM sub-part RESOLVED 2026-06-21; **FCR-9 sub-part RESOLVED 2026-06-21** (seat-aware,
witnessed + 5-corpus generality sweep; `audits/2026-06-21_oq138_fsm_route_conversion/FCR9_FINDINGS.md`).
**constructed-3 sub-part RESOLVED 2026-06-21** (seat-aware + CLAIM discriminant; `CONSTRUCTED3_FINDINGS.md`).
Remaining: CI-rope (KEEP), CI-rope route-purity (OPEN), constructed_high mountain-input + constructed_low/
constraint (0 live changers, sub-item), piton-3 (OQ-90), false_natural_law (OQ-70). **Per-clause RULINGS recorded 2026-06-21**
(operator; see PER-CLAUSE RULINGS below): CI-rope KEEP (out of frame, evidence-settled); FCR-9 CONVERT on the
FSM template + piton-3 carved out (OQ-90); constructed CONVERT-conditional (owes a claim-discriminant witnessed
to keep seat #2's floor); CI-rope route-purity HOLD/OPEN (operator-merits); `false_natural_law` deferred (OQ-70).
The two CONVERT rulings owe their build (conversion commit + post-build surface diff) — partition-witnessed, not
yet conversion-witnessed. Split from OQ-128 (2026-06-17): `natural_law` overwrite (`:867`) retired, type_1 cap discriminated.

**Priority:** 3

**Deps:** splits_from OQ-128, blocked_on_human per-clause-route-vs-reclassify-ruling
*(Per-clause route-vs-reclassify is a declared seat. NB the `false_natural_law` MEMBER is gated on OQ-70 — see Origin — but that does not block FCR/FSM/constructed, so it is not an OQ-138-level dep.)*

**Gate-expiry annotation (2026-07-02, OQ-70 premise-rot correction pass).** The OQ-70 gate
named above and in the Origin/per-clause lines **expired before it was authored**: OQ-70 was
resolved 2026-06-05 (option A class ruling, `72ec2cdd`), the defers here were written
2026-06-17/21, and the OQ-70 body contains no residual limb a member could wait on (full-entry
read + repo census witnessed at the correction pass, KNOWN_STATE 2026-07-02). The
`false_natural_law` member is therefore **rulable-once-witnessed like its siblings** — witness
= before/after diff + routing-adequacy positive control per the FCR-9 precedent; NB only 1
live firing, so breadth comes from the twins/archives sweep. The route-vs-keep ruling itself
stays in this entry's existing `blocked_on_human` per-clause queue.

**Origin:** 2026-06-17, OQ-128 residual. The whole `resolve_modal_signature_conflict` table (lines 885–932) is OVERWRITES (rewrite `dr_type` by signature) — the same architecture as the retired `:867` and the discriminated type_1 cap. Unlike natural_law (0 fires → free retirement), these FIRE and CHANGE `dr_type`: witnessed on current main the signature layer changes the metric type at **66/288 live seats** (524/3840 haiku, 637/3840 flash); per-signature firing `constructed_high_extraction 41 / false_ci_rope 19 / coupling_invariant_rope 4 / false_summit_mountain 2 / false_natural_law 1`. So NONE retires free — each needs its own before/after diff and a routing-adequacy positive control. **SPLIT:** FCR / FSM / constructed are rulable-once-witnessed (mechanical gathering); `false_natural_law` is deferrable-pending-OQ-70.

**What resolution changes.** A per-clause ruling (route / discriminate / keep-as-correction) with each clause's before/after verdict diff as the witness, consistent with the engine-ROUTES-never-RECLASSIFIES architecture (AGENTS.md). The type_1 discriminated-severity build (`drl_core.pl` `dr_claim_mismatch`) is the precedent pattern. Cross-ref: OQ-128 (parent), OQ-70 (FNL gate), OQ-98.

**FSM clause — proposed disposition INHERITED from OQ-122 (closed 2026-06-18): DISCRIMINATE on victim presence, do not gate.** OQ-122's FSM victim-gate (suppress `false_summit_mountain` when `vic=0`) is superseded by this family's route-not-reclassify architecture, but its discriminant is sound and pre-witnessed. Apply the type_1 pattern: `false_summit_mountain ∧ vic=0 → informational` (route via the sink — a mountain-claim with no victim has nothing to conceal, the non-diagnostic case) / `∧ vic>0 → moderate` (concealment floor stays). Live now: FSM fires 2 seats, both `vic=0`, currently graded `signature_correction/moderate` (holds the two physics cases at YELLOW) — under the discriminant they would route as informational. **Pre-witnessed discriminant value (the gate's load-bearing partition):** `testsets_flash` 40 FSM-firings = **18 `vic=0` + 22 `vic>0`** (`audits/2026-06-13_oq122_retype_discriminator/breadth_sweep_results.txt`); the adversarial `vic>0` cell is non-empty in 4/5 corpora. Build = the discriminated-severity change + per-seat before/after `verdict_join` diff (expect the 2 physics YELLOWs to drop their signature floor; `vic>0` FSM-firings unchanged). Still `blocked_on_human` for the route-vs-keep go, but the discriminant and its witness are in hand. Cross-ref: OQ-122 (origin of the discriminant), OQ-50 (FSM detector).

**FSM BUILD RECORD (RESOLVED 2026-06-21, operator go).** Full witness:
`audits/2026-06-21_oq138_fsm_route_conversion/FINDINGS.md`. Shipped: (a) FSM stops overwriting —
`config:param(false_summit_override_target)` default `tangled_rope→mountain` (the mountain-input
clause's `Result=Target=mountain=ModalType`, so the existing hook neutralizes the overwrite and stays
an ablation lever); unknown-input clause `tangled_rope→unknown` (OQ-37 abstain, **0 live fires —
unverified-in-commit**). (b) Shared template: `signature_detection:converted_signature/1` +
`signature_diagnostic_severity/3`; `signature_grade/2`+`signature_severity/2` grade converted
signatures on the victim discriminant (`vic>0→moderate/correction`, `vic=0→informational/commentary`),
NOT on the (now-zero) type delta — the `dr_claim_mismatch/4` precedent. Legacy signatures
byte-identical. (c) **Consumer fix found by code-read (not in the plan):** FSM removed from
`abductive_helpers:known_override_signature/1`+`override_target/2`, else `probe_signature/3` emits a
spurious `override_mismatch` tension post-revert; P1/P7 then go vacuous.
**Two divergences from the plan, both material:** (1) the live corpus grew 57→**92**, so it now has
**3** FSM seats incl. one **vic>0** (`protein_anabolic_resistance`) — the kill condition is on live
main, not just flash. (2) At the report surface the 3 seats go **yellow→RED**, NOT the plan's expected
green: the override was MASKING dirac(`second_class`)+cohomology(`fails_descent`)+abductive tensions by
setting the type to tangled_rope (where they're "expected"); reverting to mountain unmasks them as
genuine contradictions. **Operator ruling 2026-06-21:** *the engine adds commentary, does not change
classifications, and it is OK for diagnostics to render different verdicts* — Position A (let the
subsystems speak; red is honest) over Position B (suppress dirac/cohomology to force green base).
`claimed_type=mountain` preserved (route ≠ reclassify); the victim discriminant lives in the
commentary layer (`signature_grade`/alert severity, serialized). Evidence for the ruling:
**82 FSM seats across 5 corpora** (≈6,500 stories) ALL carry cohomology/dirac → 0 where the discriminant
would be headline-visible; the structural tensions are invariants of false summits, so Position B would
blanket-suppress an always-firing signal. Witness: full-pipeline diff = **only the 3 FSM seats change,
89 byte-identical**; `severity_floor/2` two-sided control discharged; trap (silent green) averted
(headline red; protein keeps `correction` via the discriminant despite zero type-delta — a naive revert
would have dropped it to commentary). `validation_suite` 92/0/0; `check_stack` baseline-clean;
`test_contradiction_signatures` 5-fail is pre-existing CS-axis fixture (identical OLD vs NEW). Subtlety
recorded: `constraint_signature/2` BOUND-arg query bypasses the cascade cuts (the build uses the unbound
form, correct).

**DEFERRED-CLAUSE EVIDENCE PASS 2026-06-21 (read-only; no conversion). Full:
`audits/2026-06-21_oq138_fsm_route_conversion/DEFERRED_CLAUSE_EVIDENCE.md`.** True (unbound) cascade-winner
counts on live (92) — the "N fires" framing overstated blast radius; most overrides are INERT or
load-bearing-elsewhere. Each clause's diff is now IN HAND; the partition ruling is the operator's seat.
- **coupling_invariant_rope — KEEP on the merits; NOT a partition the evidence settles**
  (`audits/2026-06-21_oq138_fsm_route_conversion/cirope_genuine_vs_washed.md`). The genuine-vs-washed
  discriminant is the CI-rope detector ITSELF, already run: FCR (`appears_as_rope` ∧ fails ≥1 Boltzmann test
  = WASHED) fires first in the cascade; CI-rope (Boltzmann-compliant ∧ scope-invariant ∧
  has_coordination_function ∧ `\+ constraint_captured`, OQ-94 gate = GENUINE) certifies what survives. The 6
  winners are uniform genuine passes (all compliant/invariant/coord/uncaptured; excess 0.03–0.10, non-gating
  by design; **positive control: 52 live seats FAIL boltzmann** so the test discriminates). No sub-partition —
  forcing one would convert a test-backed cert on a distinction the engine already ruled. Companion gate
  (constructed-#2 lesson, consumer side): `dr_type='rope'` is load-bearing downstream — `dirac:287`
  rope⇒**first_class**, purity immunity 1.0, cohomology rank, boltzmann, maxent — so reverting `rope→scaffold`
  would make the engine read genuinely-coordinating seats as constructed scaffold. CI-rope is categorically
  unlike FSM/FCR/constructed (which MANUFACTURE a contested verdict): it is a CORRECT certification. **Operator's
  seat:** keep, or pay the consumer-rewire cost (5 consumers read `dr_type='rope'`, not the signature) for
  route-purity. Evidence frames it; the merits ruling is the operator's.
- **false_ci_rope — route + victim-discriminate, SAME unmask story as FSM (milder).** 25 winners, 12 CHANGE
  (scaffold→tangled_rope ×8, scaffold→piton ×3, snare→tangled_rope ×1), 13 inert. Hook ablation
  (`fcr_override_enabled` 0): changed seats revert to scaffold, base green→YELLOW — routing ALSO unmasks
  tensions (the "scaffold stays green" guess was refuted), so the discriminant lives in the commentary layer.
  Victim split present (6 vic=0 + 6 vic>0). **Piton sub-case (3, scaffold→piton, all vic>0):** OQ-90
  FCR-branch refinement — needs separate handling, cannot blind-revert to scaffold.
- **constructed-3 — CONVERTED (RESOLVED 2026-06-21, seat-aware + CLAIM discriminant).** Build record:
  `audits/2026-06-21_oq138_fsm_route_conversion/CONSTRUCTED3_FINDINGS.md`. Routed the 3 constructed_high
  unknown→snare seats to the honest abstain `unknown`; the NEW **claim discriminant** (mountain→severe,
  else→informational; victim doesn't distinguish — all 3 vic>0) keeps #2's floor by replacing the lost
  `type_1_false_summit(severe)` with the signature severe. **Kill condition MET (witnessed):** #2
  (institutional_trust_erosion, claimed mountain) keeps RED byte-identical; #1/#3 route to yellow/commentary;
  47 inert + all non-constructed byte-identical; 5-corpus `mountain-routed→severe` holds (mtn-no-severe=0
  everywhere). `constructed_routed/1` keyed on the UNBOUND cascade winner + dr_type=unknown outcome (a bound-arg
  query tripped on the detector and wrongly caught `superheavy_decay`, an FCR seat — §1 gotcha; **same
  bound→unbound fix applied to `fcr_routed/1`, behavior-preserving**). **Residual RESOLVED → OQ-173 (2026-06-21):**
  the maxent boost (`maxent_classifier:341`) was made seat-aware (skip at `constructed_routed/1`). NB the
  "flips #1/#3's maxent_top to tangled_rope" framing was imprecise: on the CLASSICAL path #1/#3's
  maxent_top was already the raw argmax (the ×3 boost never flips a classical top — positive control:
  only 2 corpus-wide flips, both non-converted unconditional overrides); the real boost-driven flip lived
  on the INDEXED path (#3 `shinbutsu` tangled_rope→snare, now corrected). Benign by outcome (headline
  yellow, subsumed; #2 red via severe) — verdict_join byte-identical. `audits/2026-06-21_maxent_seat_aware/`.
  Scope was constructed_high unknown-input only; mountain-input + constructed_low/constraint have 0 live changers (sub-item).
  ORIGINAL EVIDENCE — **constructed_* override INERT on 94%; the 3 real changes are NON-uniform (bare abstain was wrong).**
  constructed_high 50 winners but only **3** CHANGE (all `unknown→snare`); 47 inert (metric already snare).
  constructed_low 1 inert; constructed_constraint / coordination_scaffold 0 live. No config hook → ablated via
  a temporary one-clause edit + pipeline (floor-source decomposition, report surface incl. abductive;
  `audits/2026-06-21_oq138_fsm_route_conversion/constructed3_floor_source.md`). **Refuted "convert + abstain,
  no downstream change":** bare revert-to-unknown moves all 3 headlines in DIFFERENT directions — the 2
  tangled_rope-claimed seats (equal_protection, shinbutsu) ESCALATE yellow→RED (cohomology unmask, the FSM
  pattern); the mountain-claimed seat (institutional_trust_erosion_c0) DE-ESCALATES RED→yellow because its
  `type_1_false_summit` SEVERE alert was riding on the override-manufactured snare and drops to informational
  once dr_type=unknown. So a bare type-layer abstain silently drops #2's severe "mountain-claim over probable
  extraction" flag ⇒ constructed inherits the **FSM discriminated-severity template** (signature carries the
  diagnostic), NOT bare abstain. 47 inert seats byte-identical; behavior change is 3 (2 up, 1 down).
- **false_natural_law** — UNGATED 2026-07-02 (the OQ-70 defer expired before it was authored —
  see the Gate-expiry annotation above); rulable-once-witnessed, witness not yet gathered.

**PER-CLAUSE RULINGS (operator, 2026-06-21).** Verdict → witness → kill condition. Two are evidence-settled
(commitments); two are operator-merits seats (OPEN/conditional — the evidence frames but does not settle them,
so phrasing them as ruled would counterfeit a witness).

**STANDING GATE (record once; the arc's generalization).** Before converting ANY override, decompose TWO
things, not one: (1) where the seat's OWN floor is sourced after revert (FSM lesson — a dropped floor reads as
silent green); (2) what OTHER diagnostics consume the type the override manufactures (constructed-#2: a severe
`type_1_false_summit` rode the manufactured snare and vanished on revert; CI-rope: a certified type is
load-bearing for 5 downstream subsystems). An override's blast radius is everything downstream that reads the
type it writes, not just the seat's own verdict. (Reusable form in `docs/technical/signature_detection_wiring.md` §4.)

- **CI-rope — KEEP, out of conversion frame (evidence-settled).** CI-rope is the certification arm of the
  FCR/CI-rope Boltzmann battery, not a manufacturing override: FCR fires on washed (fails ≥1 structural test),
  CI-rope certifies what survives. Route-not-reclassify governs manufacturing; CI-rope writes a verdict the
  structural tests EARNED, so it is mis-grouped by the inherited "N fires" framing. Witness: 6 uniform genuine
  passes (excess 0.03–0.10, non-gating; positive control 52 live seats fail boltzmann, so the passes
  discriminate); no sub-partition forced. **Kill:** a downstream consumer found to treat `dr_type='rope'` as
  contested-rather-than-certified ⇒ CI-rope re-enters the frame.
- **CI-rope route-purity — HOLD, lean KEEP-as-written (OPEN, operator's seat).** The 5 consumers
  (dirac→first_class, purity immunity 1.0, cohomology, boltzmann, maxent) read `dr_type='rope'`; route-purity
  would repoint them at the CI-rope signature. Lean: do not rewire — `dr_type='rope'` is the more stable
  contract than a signature predicate's internal shape; the certification is correct either way. Convert only if
  route-purity is enforced engine-wide for a reason beyond this clause. Architecture-aesthetics, not diagnostic.
  *This limb also inherits the parked `neutron_star`/FCR RED adjudication (OQ-122/OQ-128 closes parked it
  "under OQ-70"; orphaned by the OQ-70 close — re-homed here 2026-07-02, it is an FCR case).*
- **constructed — CONVERT the 3, gated on a claim-discriminant that keeps seat #2's floor (conditional).**
  47/50 winners are snare→snare (inert; convert byte-identical). Behavior change is 3. Bare abstain-to-unknown
  is refuted: it drops a severe `type_1_false_summit` on #2 (institutional_trust_erosion, RED→yellow) that
  correctly rode the manufactured snare ⇒ constructed needs the FSM discriminated-severity template, not bare
  abstain. Candidate discriminant: authored claim (mountain/contested keeps a floor). **Conditional** — the
  discriminant ships only once WITNESSED to keep #2's floor while letting inert seats route free (the way FSM's
  victim-discriminant was witnessed on protein, not argued). **Kill:** if the claim-discriminant cannot hold
  #2's severe without also flooring genuinely-inert seats, it is the wrong discriminant and constructed
  re-opens. Owes: conversion commit + post-build surface diff. Partition-witnessed only, not conversion-witnessed.
- **FCR-9 — CONVERTED (RESOLVED 2026-06-21, seat-aware); piton-3 carved out.** Build record:
  `audits/2026-06-21_oq138_fsm_route_conversion/FCR9_FINDINGS.md`. The FSM template did NOT transfer directly:
  false_ci_rope is seat-split (9 routed / 3 piton / 13 inert, one signature), so signature-level keying would
  flip the inert seats' grade and disturb the piton-3. Built seat-aware: `fcr_routed/1` (keyed on the stable
  dispatch gates + the dr_type OUTCOME, NOT a metric proxy — an earlier proxy diverged from ModalType on 2
  haiku+4 flash seats, caught by the 5-corpus generality sweep), `converted_at_seat/2` (signature-level FSM,
  seat-level FCR), `seat_overrides/2` threaded through `probe_signature`/P1/P7. Witness: 9 seats route
  tangled_rope→scaffold/snare, 6 verdicts change (discriminated: vic>0 correction/moderate, vic=0
  commentary/informational, sig=AGREE, milder than FSM — mostly yellow), piton-3 TYPES unchanged + 13 inert +
  all non-FCR byte-identical; 5-corpus invariants pass; validation_suite 92/0/0. **Carve-out relaxed:**
  `statutory_debt` (piton) shifts yellow→red via the corpus-relative maxent ENSEMBLE (entropy_flag) — type
  unchanged, OQ-90 not relitigated (Position-A-acceptable). **Residual RESOLVED → OQ-173 (2026-06-21):** the maxent FCR boost
  (`maxent_classifier.pl:331`) was made seat-aware (skip at `fcr_routed/1`) — confirmed benign for the 9
  (all classical tops already == raw argmax; the indexed boost had no tangled_rope mass to amplify at these
  seats; verdict_join byte-identical). Same fix covers the constructed shape at :341.
  `audits/2026-06-21_maxent_seat_aware/`. piton-3 still held on OQ-90.

## OQ-139 — `green` base verdict reads "safe" but means "consistent": a labeling thread (Ω_C)

**Ω-type:** Ω_C (a labeling/semantics decision — relabel, annotate, or leave; no structural defect to fix).

**Status:** open — split from OQ-128 (2026-06-17); small, not urgent (by-design, not rot).

**Priority:** 4

**Deps:** splits_from OQ-128.

**Origin:** 2026-06-17, OQ-128 residual. `compute_verdict` aggregates SUBSYSTEM TENSION/disagreement (green = the subsystems AGREE), NOT extraction. So a cleanly-classified high-ε snare (e.g. `china_taiwan_reunification_mandate` ε=0.8) carries a GREEN *base* — correct BY DESIGN (verdict_join: base=consistency, alert floors=severity, OQ-98), with the type_1 floor supplying the severity that reddens the headline. Witnessed: the v5 snare-9 all green base at ε 0.5–0.8; the discriminant (`dr_type`) reads ε true, independently of the base. So this is NOT structural rot — it is that a reader may misread "green base" as "safe" when it means "consistent."

**What resolution changes.** A decision on whether to relabel/annotate the base verdict so a cold reader cannot read green-base as safe (the headline is `verdict_join`, which already floors on severity; this is purely about the base INPUT's presentation). Cross-ref: OQ-128 (parent), OQ-98 (verdict_join base/floor split).

## OQ-140 — Characterize author↔engine disagreement per diagnostic mode: what a routed diff MEANS, not where it goes (Ω_C + Ω_E)

**Ω-type:** Ω_C (what KIND of disagreement each routed diff is — a typing judgment) over an Ω_E base (sub-classify the population by claim type / degrading seat / diagnostic mode).

**Status:** open — split from OQ-128 (2026-06-17). The foundational open the routing only made LEGIBLE: the sink NAMES disagreement; nothing yet says what KIND each is.

**Priority:** 2

**Deps:** splits_from OQ-128
*(Its substrate, the routing sink, is already BUILT — so this is unblocked, not waiting on it.)*

**Origin:** 2026-06-17, OQ-128 residual. The routing sink's `author_engine_divergence` address is **148/288 live seats — the MAJORITY** — and "they disagree" is a named category, not a characterization. Distinct from the routing rulings (which decide WHERE a diff goes); this decides WHAT a diff means (sub-type by claim type? by which seat degrades? by diagnostic mode?). The largest live bucket is its evidence base.

**What resolution changes.** A sub-typing of the `author_engine_divergence` population that turns "they disagree" into named kinds, each with a witness — the substantive research the routing made possible. Cross-ref: OQ-128 (parent), `prolog/routing_sink.pl` (the substrate), OQ-133 (orientation signature, a sibling diachronic-tier question).

## OQ-141 — Does `mitigated` belong in `omega_resolver.ACTIVE`? (footer-vs-code status-token conflict) (Ω_C)

**Ω-type:** Ω_C (a tier/membership-boundary judgment: which status tokens count as "active" for the workable frontier — an undefined boundary between two sources, not a contested origin).

**Status:** open

**Priority:** 6

**Origin:** 2026-06-18, surfaced while building the derived router index (`issues/INDEX.md`). Two sources disagree on whether `mitigated` is a live status: `issues_status.TOKENS` (`python/issues_status.py:29`) and the ISSUES.md footer grammar list `mitigated` as a normal token alongside `open`/`partial`, and the footer narrates `mitigated`/`partial` as "semi-live", but `omega_resolver.ACTIVE` (`python/omega_resolver.py:58`) is exactly `{open, investigating, partial}` — it PARKS `mitigated`, so the 6 currently-`mitigated` entries drop out of the workable frontier, the `menu`/`activations` active counts, and the index's "current resolver-defined frontier" partition.

**What resolution changes.** Whether the derived router index (and `menu`) surfaces the 6 `mitigated` entries in the active frontier. The index mirrors whatever the resolver settles on — it imports `ACTIVE` rather than re-encoding the set — so this is upstream of the index, not a property of it. Kill condition (checkable in-substrate): trace the consumers of `omega_resolver.ACTIVE` (the `.active` property → `frontier()` bucketing, `cmd_menu`, `cmd_activations`, and now `cmd_index`'s partition). If `mitigated` entries are deliberately parked out of the workable frontier (a `mitigated` issue is semi-live but not something to PICK UP now), the resolver is right and the footer prose is the stale source (fix: footer wording). If they belong in the pick-up rotation, `omega_resolver.py:58` is the bug (fix: add `mitigated` to `ACTIVE`; the index follows automatically by import). Either fix is upstream of and invisible to the index code. Cross-ref: `python/omega_resolver.py:58` (the parked set), `python/issues_status.py:29` (the broader token set), `issues/INDEX.md` (the derived consumer).

## OQ-142 — check_stack baseline undefined-predicate references (the "baseline cleanup" deferred at KNOWN_STATE.md:3006)

**Ω-type:** Ω_E (mechanical, witnessed per finding).

**Status:** open — minted 2026-06-18 from the OQ-115 class sweep.

**Priority:** 8

**Deps:** bundled_with OQ-143

**Origin:** the OQ-115 fix returned check_stack to its 4-finding 2026-06-04 baseline. This
parent tracks those references so each is worked independently and records *why it doesn't bite
today* — a witnessed dead-or-guarded reaching path, NOT "baseline-documented." The class-sweep
discriminator is **phantom × guarded × reachable**: a reference bites only when its target is
absent at the call's load chain AND the call is unguarded AND the path is reachable; OQ-115 was
the one unguarded bite. Sub-findings split out as numeric OQs (the tracker's machine-readable
label grammar is `OQ-\d+`; lettered sub-IDs are invisible to `issues_status`/`omega_resolver`,
so the plan's `142a/b/c` became OQ-143/144/145): OQ-143 (validation_suite guarded phantom),
OQ-144 (data_repair xref mis-attribution), OQ-145 (drift_events:175 wrong-qualifier — **RESOLVED
2026-06-18**, the one code change; check_stack baseline now **3 findings**). Remaining annotate-only:
OQ-143, OQ-144. Lineage: OQ-115, OQ-57.

## OQ-143 — `validation_suite:test_case/4` ← test_harness:26 (guarded phantom; check_stack baseline finding)

**Ω-type:** Ω_E (mechanical, witnessed).

**Status:** open — minted 2026-06-18 from the OQ-115 class sweep.

**Priority:** 10

**Deps:** splits_from OQ-142

**Origin:** check_stack flags `validation_suite:test_case/4` (phantom under `[stack]`)
referenced by `test_harness.pl:26`. **Non-bite (read-pass witness):** `test_harness.pl:23-35`
calls `test_case/4` ONLY inside the `current_predicate(validation_suite:test_case/4) ->`
then-arm; the `;` else-arm runs `run_all_tests('tax_code_section_469')` and does not reach
`test_case/4`. Guarded — the negative control for the discriminator (phantom but guarded ⇒ no
bite). **Resolution:** annotate as intentional `current_predicate` fallback; no code change.
Re-witness the guard (re-paste the then/else arms) before closing.

## OQ-144 — `data_repair:constraint_beneficiary/2` & `constraint_victim/2` (xref mis-attribution of a clean dynamic call; check_stack baseline finding)

**Ω-type:** Ω_E (mechanical, witnessed).

**Status:** open — minted 2026-06-18 from the OQ-115 class sweep.

**Priority:** 10

**Deps:** splits_from OQ-142

**Origin:** check_stack flags `data_repair:constraint_beneficiary/2` and `constraint_victim/2`
undefined, referenced from `data_repair.pl` (`bridge_beneficiary_victim_pure/3`,
`bridge_scaffold_markers_pure/3`). **Non-bite (read-pass witness):** `acc_has/2`
(`data_repair.pl:60`) goal-calls `narrative_ontology:Fact` (an indirection static xref cannot
trace, so check_stack mis-attributes it to `data_repair:`). The real target is dynamic/multifile
(`narrative_ontology.pl:100`), so absence fails clean — cold `[stack]` call
`narrative_ontology:constraint_beneficiary(zzz_nonexistent,_)` → `fails_clean`, not a throw.
**Resolution:** annotate the check_stack baseline that these are xref mis-attributions of a
clean dynamic-predicate call; no code change. Re-witness the `fails_clean` call before closing.
Cross-ref OQ-86 (the data_repair sentinel-minting bridge).

## OQ-145 — `narrative_ontology:requires_active_enforcement/1` ← drift_events.pl:175 (latent wrong-qualifier; OQ-57-class the OQ-57 fix missed)

**Ω-type:** Ω_E (mechanical, latent existence_error).

**Status:** resolved — fixed 2026-06-18; one-token qualifier fix at `drift_events.pl:175`.

**Priority:** 5

**Deps:** splits_from OQ-142

**Origin:** `detect_is_piton/1` (`drift_events.pl:175`) called
`\+ narrative_ontology:requires_active_enforcement(C)` with the WRONG qualifier — the predicate
lives in `domain_priors` (`domain_priors.pl:30,139`). OQ-57 fixed the sibling `:236`, missed
`:175`; inside `\+` it THREW if `detect_is_piton/1` was reached. **Reachability (witnessed, with
positive control):** static external = 0 callers (control: `drift_event` → 19 refs, so the grep
DOES find callers — the 0 is real); static internal = export/comment/head only, no goal-position
call (file dispatches detectors elsewhere at `:401,:407`); runtime-constructed (`call/N`/meta-call)
= unverified by static grep. So unreached on every checkable path, latent on the rest — the fix is
correct regardless.

**Resolution.** `narrative_ontology:` → `domain_priors:` at `:175`, mirroring `:236`. **Witness
(cold `[stack]`, synthetic constraint with extractiveness 0.05 / theater_ratio 0.80 to reach
`:175`):** before → THREW
`error(existence_error(procedure, narrative_ontology:requires_active_enforcement/1), context(drift_events:detect_is_piton/1,_))`;
after → `SUCCEEDED_CLEAN` (prints "Drift: Internalized Piton"). check_stack baseline drops 4 → 3
(the `requires_active_enforcement` finding is gone). Provenance: KNOWN_STATE 2026-06-18; commit at
close. Lineage: OQ-57, OQ-115.

---

## OQ-146 — Orbits top-level metadata (`corpus_hash`) poisons consumers that iterate the file as constraints

**Ω-type:** Ω_E (mechanical; latent type_error armed by an upstream stamp).

**Status:** resolved — fixed 2026-06-18; single-source partition-and-assert loader + 6 consumers repointed.

**Priority:** 4

**Deps:** splits_from OQ-29

**Origin:** OQ-29 made `corpus_hash` stamping standard, including a **top-level `corpus_hash` key
inside `outputs/product_site_orbits.json`** (flat `{id: {h0,h1,contexts,…}}` dict, no namespace
separating metadata from constraints). Any consumer iterating the top-level keys *as if each were a
constraint* now hits `corpus_hash` (a `str`) and crashes — "worked before" because un-stamped orbits
had no such key. Surfaced as the `oracle_gap_analysis.py:143` crash (`entry["contexts"]` on a str),
first patched with an **inline** filter (`7b5801f0`). A set-level census found it systemic. **Census
+ positive control:** `git grep -ln product_site_orbits` → per file grep iteration idioms; the
control re-found all five known exposures **and** surfaced `structural_config_sensitivity.py:529`
(missed by an `.items()`-only grep) — so empty hits elsewhere are looked-and-absent, not never-looked.
Exposed: `product_site_delta_sweep.py:117`, `structural_config_sensitivity.py:363/529`,
`alt_power_transform_test{,_3k}.py:~96`, `game_theory_nash.py:158` (only `--input
product_site_orbits.json`), `oracle_gap_analysis.py:143`.

**Resolution.** One canonical, fail-loud predicate `shared.loader.load_orbits_constraints` —
**partition-and-assert**: keep dict-with-`contexts` as constraints, drop only affirmatively-named
metadata (`_ORBITS_METADATA_KEYS = {"corpus_hash"}`), **raise** on any unclassifiable top-level key
(a malformed constraint or a new metadata key fails loud, never silent-undercount). Rejected: silent
shape-filter (undercount reads as success), inline-per-consumer (forks the predicate — Pattern 2),
schema re-namespace (breaks every OQ-29 guard + the swipl exporter), sidecar (re-adds drift surface).
All 6 consumers repointed to the helper (the inline `7b5801f0` filter replaced too). **Crash-over-drop
is safe by producer construction** (RULED, not empty-grepped): `product_site_export.pl:80–96`
`write_one_entry` emits `"contexts"` **unconditionally** for every constraint, and the context key set
is a static Cartesian product (`constraint_indexing.pl:1052 site_contexts_product/1`) that never reads
the corpus → every entry in the live corpus **and every archive** carries `contexts`; a top-level
entry lacking it can only be post-export metadata or corruption — exactly what should raise.

**Witnesses (set-level, 2026-06-18).** (1) PRIMARY set-equality (RHS hardcodes the literal, NOT via
`_ORBITS_METADATA_KEYS`): `set(load_orbits_constraints(P)) == set(json.load(P)) − {"corpus_hash"}` →
True (75 vs raw 76). (2) Partition-assert RAISES naming `junk` on injected `{"junk":{"h1":0.5}}` (the
dict-without-`contexts` shape a silent filter would have eaten). (3) `orbit_data.json` no-op witnessed
directly: raw len 75, zero non-constraint-shaped keys, helper len 75. (4) Per-consumer two-sided: each
of the 5 accesses RAISES on raw, yields exactly **75** via helper; `oracle_gap_analysis.py` completes;
`game_theory_nash.py --input outputs/product_site_orbits.json` TypeError→completes (75). (5) All 6
files resolve `from shared.loader import load_orbits_constraints`. Provenance: KNOWN_STATE 2026-06-18;
commit at close. Out of scope (separate bug, not this class): `sheaf_audit.py:515` ZeroDivisionError
— first read as a corpus-size bug, actually the `classifications` producer regression (WITNESSED
2026-06-18, sparsity ruled out): OQ-147 (crash floor, resolved) / OQ-148 (regression, open).
Lineage: OQ-29.

---

## OQ-147 — sheaf_audit.py:515 ZeroDivisionError on empty working set; fail closed (null rates + explicit verdict) across md/console/JSON, not fabricated PRESERVED

**Ω-type:** Ω_E (mechanical; latent arithmetic on an empty table; the fabricated-PRESERVED /
false-flat-JSON risk is a Pattern-5/6 success-shaped-absorption instance).

**Status:** resolved — fixed 2026-06-18; fail-closed floor on all three surfaces, fixture pass pasted.

**Priority:** 5

**Deps:** splits_from OQ-29

**Origin.** `sheaf_audit.py:515` divides by `n_total` (working set = constraints with ≥2 of the 10
Tier-1 slice contexts), which is **0** on the current corpus because `classifications` is empty
corpus-wide (the *upstream* cause is OQ-148; the crash floor is correct regardless). A naive
`if n_total else 0.0` is wrong: `crossing_rate = 0.0` hits the `== 0.0 → "PRESERVED (zero
crossings)"` branch (sheaf_audit.py:464), making an empty set indistinguishable from measured-flat
(Build Discipline Pattern 5/6).

**Resolution.** One `insufficient = (working_set_size == 0)` predicate, reused on three surfaces:
(1) markdown early-returns `_insufficient_data_markdown` (zero corpus-count arithmetic); (2) console
prints a one-line INSUFFICIENT-DATA message instead of the rate block; (3) results JSON sets
`crossing_rate`/`preservation_rate` to **null** (not 0.0) and adds `verdict: insufficient_data`.
Verdict single-sourced via `_verdict(crossing_rate, insufficient)` (JSON + markdown can't drift;
happy-path bands byte-identical to the old 464–471 strings). Exit 0 (an empty working set is not an
error). **Witnesses (2026-06-18):** pre-fix `ZeroDivisionError` at :515; post-fix exit 0 with JSON
`crossing_rate: null` / `verdict: insufficient_data`; fixture `python/audits/tests/test_sheaf_audit.py`
4/4 PASS (covers the empty-case markdown + the non-self-witnessing `_verdict` string swap at line
483). Provenance: KNOWN_STATE 2026-06-18; commit at close. Lineage: OQ-29; upstream OQ-148.

---

## OQ-148 — pipeline stopped populating per_constraint.classifications corpus-wide (regression 2026-06-11→2026-06-18); consumer blast-radius — which absorbed `[]` as a measurement?

**Ω-type:** Ω_E (mechanical; producer dangling-wire, Pattern 1 + Pattern 5 blast radius).

**Status:** open — outcome witnessed = regression (field populated then empty); mechanism NOT yet
witnessed (see falsifier).

**Priority:** 4

**Deps:** splits_from OQ-29

**Origin (regression witnessed).** `outputs/pipeline_output.json` carries `classifications: []` for
**all 80** constraints (2026-06-18 run, `with >=1 classification: 0`). Committed post-reset snapshots
prove it used to populate: `audits/2026-06-11_oq98_verdict_join/pipeline_output.baseline.json` →
46/48 constraints, 287 entries (run 2026-06-11T07:13Z); `…oq90…refine1.json` → 50/52, 312 entries
(2026-06-11T21:03Z). So `classifications` populated on 2026-06-11, empty on 2026-06-18 = **producer
regression** in the intervening week. The data still exists in `orbit_data.json` (75 entries,
4-context `contexts` dicts) — the 10-slice `classifications` producer specifically regressed.
**Sparsity is ruled out** (data exists; this is a wiring break, not a small corpus).

**Spine (the work).** NOT merely "rewire the producer" but a **consumer blast-radius audit**:
`classifications` is a declared schema field (`python/shared/schemas.py:195`, default at :56)
referenced across ~40 python files (`grep -rln classifications python/`), incl. `orbit_characterization`,
`metric_audit`, `idea_site_exploration`, `sotu_mountain_decoupling`, `position_geometry_audit`,
`bc_coupling_audit`, `audit3_*`. `sheaf_audit` is the only one that **crashed loudly** (OQ-147); the
Pattern-5 risk is the quiet ones that absorbed `[]` into committed outputs that read as measurements.
The audit's first deliverable is the **true consumer set** (which of the ~40 references read the
field vs. name-match), then per consumer: did `[]` land in a committed output that reads as a
measurement?

**Mechanism — two candidates + falsifier (commit-plus-falsifier; don't presume code-commit).** The
same snapshots show the corpus grew 48/52 → 80 across the window, i.e. a **reset/regrow happened**,
so the break may live in the reset/regrow data path, not a code commit. Candidates: (1) a
producer-touching code change between 06-11 and 06-18; (2) the reset/regrow changed the data path so
the producer no longer receives its input. **Falsifier:** if `git log` over the producer wire finds
**no** producer-touching commit in the window, the cause is the reset/regrow path and bisect was the
wrong probe — pivot to the data path. Run the bisect, but pre-register that null result as the pivot
signal.

**[EDGE] the 75-vs-80 hole.** Exactly 5 `*_contradictions` ids in `pipeline_output` lack `orbit_data`
(actinide_replenishment_mechanism, digital_money_legitimacy, performance_legitimacy,
polaris_document_status [a freshly-untracked testset in git status], visual_evidentiary_authority).
A producer fix that rewires `orbit_data → classifications` silently inherits a 5-constraint hole —
surface it, don't assume a clean rewire covers the corpus. Lineage: OQ-29; OQ-147 ships independently.

---

## OQ-149 — The committer axis is the most model-divergent layer (reading_relation topology + axiom-override asymmetry)

**Ω-type:** Ω_E (empirical — measurable cross-model agreement rates; interpretation may route to the OQ-70 authoring-convention confound).

**Status:** partial — Field B (`reading_relation`) ruled CONVENTION; Field C (`overridden`)
OPEN-pending-instrumentation (2026-06-27, `audits/2026-06-27_oq124_oq149_committer_convention_control/`).
**Priority:** 2
**Deps:** bundled_with OQ-124
**Origin:** OQ-56 D1 recon census over the twin corpora, 2026-06-18
(`audits/2026-06-18_oq56_twin_within_kernel_perturbation/`).

**Specific question:** Why does the committer/CS axis disagree across models so much more than
the observer axis, and is the disagreement seat-expressive signal or an OQ-70-class authoring
convention? Two witnessed twin asymmetries (haiku vs flash, same 960 name-identical slots,
classified at `8126231`):
- **`cs_reading_relation` multiset agreement = 0.392** — far below `claimed_type` 0.721 and
  `cs_kernel_id` 1.000. The `forecloses` (real closure, Theorem 8) vs `coexists_with` (licensed
  plurality) call — the committer-axis state structure (v8 §5) — is where the two models split
  ~60% of the time. This is the *committer-side analog* of the powerless-seat model-sensitivity
  (OQ-123/124), on a different axis.
- **`cs_axiom_status` `overridden`: 51 (haiku) vs 4 (flash)** — a 13× model asymmetry in how
  often an axiom is authored as overridden (the keeping-face / `cs_drift` signal, v8 §5).

**Evidence so far:** authored-fact census, positive-controlled (the parser's `claimed_type`
agreement 0.7208 reproduces the engine-side 0.721 of `2026-06-13_twin_comparison`). Raw counts
in the audit dir. **Confound caveat (OQ-70):** signature/relation prevalence can track an
authoring-template convention rather than detected structure — so this is a *coding-divergence*
finding, not a detection claim, until the bait-confound counterfactual is run on these fields.

**Stakeholder-coverage half — forward remedy LANDED (2026-06-19, commit `becd0f87`).** The
stakeholder-authoring asymmetry (haiku 494 vs flash 748 readings) was a false-negative class, not
a constraint property: 423/466 haiku no-stakeholder stories had authored beneficiaries/victims;
flash wrote the field on 75% of those same slots. Root cause = the schema marked `stakeholders`
optional (and its description said so) while the prompt said required → weaker model dropped it.
Fixed with a conditional schema gate (require stakeholders iff parties present; gravity exempt) +
KNOWN_STATE 2026-06-19. **Forward-only** (takes effect on the next rebuild; existing twins
unchanged). The `reading_relation` 0.392 and `overridden` 51-vs-4 halves remain OPEN here.

**What resolution changes:** If the divergence survives the OQ-70 control, the committer axis
is the most seat-expressive layer the engine has (the place a model's "reading" of grounding/
closure is least situation-fixed) — which would make any committer-axis invariant claim
(v8 §3, OQ-72) carry a model index. If it is convention, the relation/override fields need a
provenance bucket before they can be read as structure. Either way it gates how much weight
v8's `forecloses`/`coexists_with` state structure can bear.

**RESOLUTION (2026-06-27, convention control — `audits/2026-06-27_oq124_oq149_committer_convention_control/`).**
Substrate pinned: twins re-classified at `bbf5c92` (Field A) + corpus content-hashes (B/C). Raw
divergences reproduced (B 0.3917, C 51-vs-4). Per the operator ruling, ruled per field:

- **Field B (`cs_reading_relation` 0.392) = CONVENTION.** With Field C pulled (below), B rested on
  its settled covariate alone (authored `base_extractiveness`, cross-twin Spearman 0.86, outside the
  OQ-70 slot set). Flash leans systematically more foreclosing (forecloses_fraction 0.239 vs 0.206,
  sign-test p=0.020), but the relation call **fails to covary with the closure-degree substrate on
  the disagreeing slots** (Spearman 0.156/0.162 < the pinned 0.20) while the agreeing-slot positive
  control reproduces it (0.256/0.258). So the ~60% disagreement is a model authoring **default**, not
  detected structure → the `forecloses`/`coexists_with` field needs a **provenance bucket** before
  per-slot values can be read as structure (precedent: stakeholder fix `becd0f87`). Not built this pass.
- **Field C (`overridden` 51-vs-4) = OPEN-pending-instrumentation.** The per-slot coercion witness
  (split flash `holdable` into authored vs coerced) does NOT survive the original flash generation
  run (no raw responses saved; `repair_stats` aggregate; `contested/foreclosed→holdable` remap
  silent; `json_flash/` post-repair). Recovering it ⇒ re-generation with instrumentation = the spend
  the operator ruled out. **Not ruled; no weaker proxy substituted.** Decidable enrichment:
  `overridden` is **coercion-invariant** (missing status → generation FAIL, not silent `holdable`),
  so the 51-vs-4 asymmetry is a **real authoring difference**, not the optional-drop artifact
  hypothesized — only flash's `holdable` fine-structure is undecidable. **Graduation step:**
  instrument `story_repair._normalize_axiom_status` to log `cid` in the next flash(-class) generation
  run, then re-run the C decision rule. Gated follow-up; spend stays the operator's.

Field-A half is on OQ-124 (ruled SIGNAL). Bundle (`bundled_with OQ-124`) stays: the three fields
were one census and the third-model spend is gated on A's signal verdict.

---

## OQ-150 — Kernel and reading orbits: a variety of orbit-keys over the same data

**Ω-type:** Ω_E (empirical grouping; feeds the Ω_P taxonomy selection of OQ-56).

**Status:** mitigated — the variety-of-orbit-keys discovery is measured (2026-06-20); the
two-tier menu is delivered AND the orbit operator is built + wired (`python/orbit_operator.py`,
`prolog/kernel_orbit_export.pl` → `outputs/{reading,kernel}_orbits.json`, in `run_pipeline.py`;
full run clean, gate GREEN, commit `0c488468`). Tier-1 keys are the declared surface; Tier-2 are
emitted model-relative with twin-agreement numbers inline per orbit (operator ruling 2026-06-20).
Residue: a model-ROBUST orbit *key* (a signature reproducible better than the observer baseline)
is still open research — the built operator declares the reproducible keys, it does not invent a
new more-robust one; needs OQ-72's committer-axis alignment.
**Priority:** 2
**Deps:** gates OQ-56
**Origin:** OQ-56 D1 (M3), 2026-06-18
(`audits/2026-06-18_oq56_twin_within_kernel_perturbation/`).

**RESULT (2026-06-20, `audits/2026-06-20_kernel_reading_orbits/`).** Eight orbit-keys measured
cross-twin (haiku/flash, n=960; per-id/per-kernel membership-agreement vs permutation band95,
seed 20260620; positive controls pass — `claimed_type` 0.7208, `cs_kernel_id` sets identical, K1
reproduces the 2026-06-18 M3 0.134 exactly). **All eight clear the chance floor (`lo>band95`),
so the band95-keyed empty-menu kill did NOT fire** — but that floor only tests beats-chance, a
weaker thing than the **reproducibility** filter the plan's Context elected. Judged against the
extraction baseline (~0.72 — the substrate's own reproducibility), the keys split into two tiers:
- **Tier 1 — membership-reproducible at baseline:** `kernel-obstruction-class` (0.734, coarse
  4-way verdict) and `observer-signature` (R1, 0.722).
- **Tier 2 — above-chance but membership-fragile (below baseline):** terminal-observer (0.566),
  apparatus `cs_pattern` (0.487), terminal-committer (0.300), axiom-grounding-profile (0.272),
  seat-role-vector (0.245), kernel structure-signature (0.134).

**Two substantive Ω_E findings:** (1) **the committer axis is fragile in its FINE signatures but
reproducible when COARSENED** — apparatus/grounding are model-relative, yet the 4-way obstruction
verdict (`real_closure` 66% vs `licensed_plurality` 32%) reproduces at baseline; granularity, not
axis, governs declarability. (2) **The apparatus orbit is gradient-orthogonal to the observer
orbit** (normalized MI 0.063; Theorem 7 distinction-check) — a genuine second axis, keep
separate, do not fold.

**Specific question:** Can the kernels be grouped into orbits by a reading-structure signature
(n_readings, within-kernel depth-vector, claimed_type multiset across readings, relation
topology), and how model-stable is that grouping? Distinct from the **observer-orbit**
machinery (`orbit_characterization.py` / H¹ / `logical_fingerprint.pl`), which orbits
*constraints by cross-slice stability*, not *kernels by reading structure*.

**Evidence so far (D1 M3, twin outputs n=960):** kernels DO form real orbits — 125 (haiku) /
111 (flash) distinct full signatures over 328 multi-reading kernels, largest single orbit = 29;
the depth-vector class is dominated by `(1,1,1,1)` (all seats see depth) = 140/328 (43%). But
**membership is draw-sensitive**: cross-model agreement is 0.134 on the full signature, 0.329
on the coarse depth-vector class. Orbit *structure* (the distribution shape) is robust; per-
kernel *membership* is largely model-relative.

**Two grouping targets, not one (operator, 2026-06-19):**
- **Kernel orbits** — group *kernels* by the structure of their reading-set (above).
- **Reading orbits** — group *readings across kernels* by their seat-signature (which positions
  see what). This is the unit OQ-56's reading-stance taxonomy actually needs: an orbit = a class
  of readings that "see the same way" across the seat-gauge, comparable across kernels.

**The variety of orbit-keys (surfaced by the seat-sweep, `SEAT_SWEEP.md`; pairs with OQ-151).**
The same readings/kernels admit several quotients; pick the key per question:
- **role-vector orbits** — the 4-seat reading `[beneficiary, payer, excluded, analytical]`. The
  dominant class `[naturalized, snare, snare, snare]` is the **cover-story baseline** (= the
  1-vs-4 geometry written as a vector); the *informative* orbits are the **departures**:
  `[snare,snare,snare,snare]` (beneficiary in the cluster → extraction with no cover story) and
  excluded-dissent (manufactured consensus).
- **gap orbits** — magnitude of the beneficiary↔payer divergence (cover-story *intensity*).
- **dissent-pattern orbits** — *which* seat breaks consensus (excluded-only / payer-only / all).
- **structure-signature orbits** — (n_readings, depth-vector, claimed_type multiset) — the kernel
  key above.

**What resolution changes:** Gives OQ-56 its prerequisite cross-kernel clustering on an honest
footing: the declarable invariants are the **seat gradient** (institutional flattens within-
kernel depth; powerless/moderate see it — D1 M1) and the **orbit-distribution shape**, NOT
per-kernel/per-reading membership (which is an Ω_P/Ω_E draw-property, OQ-118). A taxonomy keyed on
per-item orbit membership would be model-relative and must not be read as discovered. Building a
stable orbit key (model-robust signature) is the open work; needs the axiom-axis alignment of
OQ-72 to include the committer-axis dimensions honestly.

---

## OQ-151 — Role-projected ("six-questions") gauge + empty-chair detector — built on the honest primitive

**Ω-type:** Ω_C (design — what to build + the declared slot definitions; a diagnostic method, NOT a theorem of social reality).

**Status:** open
**Priority:** 2
**Deps:** bundled_with OQ-51
**Origin:** stakeholder/cohomology thread off OQ-56 (2026-06-19), sharpened by a three-model
review (Gemini / Perplexity / Claude Code web). Evidence: `outputs/pipeline_output.haiku.json`
(n=960, `8126231`) + `grothendieck_cohomology.pl`; witnessed probes this session.

**The instrument.** Re-base the per-constraint obstruction onto a **canonical 4-role site** —
`[beneficiary, victim, excluded, analytical]` — instead of (or alongside) the fixed power-gauge
`[powerless, moderate, institutional, analytical]`. The role site *is the six-questions made into
a base space* (Q1 who-benefits/who-pays → beneficiary+victim; Q4 who's-not-in-the-room →
excluded; the observer → analytical), so it is **cross-constraint comparable by construction** (same
semantic axes every time) while reading the *authored parties*. Witnessed value: the role-gauge
makes the cover story legible — `abrahamic_covenant__ishmael` reads
`[ben:naturalized, pay:snare, excl:snare, obs:snare]` (beneficiary sees natural law, everyone else
sees extraction); the power-gauge scrambles this across power positions.

**Build = Option-2 hybrid + Option-3 dual-gauge diff.**
- **Hybrid coverage:** synthesize `beneficiary`/`victim` slots from `base_properties` directionality
  (low-d/high-d), near-universal (haiku 942/960, 841/960); `analytical` = the canonical analytical
  context (100%); **`excluded` is stakeholder-only (~60%, coverage-carried).** The excluded seat is
  the *irreducible* contribution — base_properties has no who's-not-in-the-room array, so it cannot
  be synthesized. It is the entire information gain.
- **Dual-gauge diff:** keep power-gauge AND role-gauge; the diff is signal — `power-H¹>0 ∧
  role-H¹=0` = fracture is about authority/power-placement; `role-H¹>0 ∧ power-H¹=0` = fracture is
  about who-benefits, invisible to the power orbit.

**The headline value (the inversion):** the find is NOT visible disagreement (it documents itself).
It is **manufactured consensus** — apparent agreement that holds only because the dissenting seat
was never seated: `included-roles agree ∧ a genuinely-typed excluded seat dissents`. *Consensus is
more suspect than conflict; the ideological load of a constraint is measured by how empty the room
had to be for it to look agreed.*

**Corpus seat-sweep result (the OQ-56 question answered; `SEAT_SWEEP.md` in the audit dir,
2026-06-19).** Holding each position fixed and sweeping the corpus (% extractive): beneficiary
15.5/14.6 (haiku/flash) vs payer 90.0/81.0, excluded 80.9/72.2, observer 81.6/72.3, analytical
82.6/71.7 — a **1-vs-4 geometry** (beneficiary vs a tight not-beneficiary cluster), not a spectrum.
The benign↔extractive swing is **FORCED** by `role→d` (mechanism, not finding; twin-replication is
only a stability check since both runs share the config), and **excluded≈payer is forced** (config
+ coverage dilution). **Two residuals survive substrate kill-conditions:** (1) `naturalized` is
ε/χ-gated not d-fixed (`drl_core.pl:416`: ε>0.45 ∧ χ<0.35 — the cover-story type), so it is the
beneficiary seat's reading of high-ε constraints; (2) the **analytical seat sits at the payer's
pole** (82.6/71.7, exact, vs a ~53% midpoint) — it carries no `role→d`, only a declared
`context→(d,σ)`, so **"no view from nowhere": the declared analytical seat is positioned at the
extraction pole, beside the parties who pay.** That is the publishable claim (Seat Theorem as
measurement); the cover-story result is forced/old.

**FOUNDATIONAL CORRECTION — do not repeat the counterfeit rigor (witnessed 2026-06-19).** The
engine's "H1" is NOT a cohomology rank. `grothendieck_cohomology:count_disagreeing_pairs` is, by
its own comment, a *"Cech 1-cocycle proxy"* — a tally of disagreeing context-pairs (range 0..6).
Witness: Abrahamic role-gauge `[naturalized,snare,snare,snare]` gives tally **3**, but the genuine
first Betti number of that disagreement graph (star K₁,₃) is **E−V+C = 3−4+1 = 0**. Claude Code
web's kill condition resolves: replacing "H1" with "count of seats differing from the modal" changes
nothing downstream — because that IS what it computes. So **H⁰ (global section ⟺ all agree) is
legitimate; "H1" is a contextuality/disagreement count, not dim H¹.** Build this detector on the
honest primitive (H0 + disagreement tally + coverage); claim Grothendieck cohomology ONLY if a real
nerve+cochain (overlapping cover, e.g. reading_diff's vantage alignment) is constructed. This also
flags the *existing* engine naming (`cohomological_obstruction`, `contextuality_fraction`,
`sheaf_status`) as overclaiming — pairs with **OQ-51**.

**Set-valued aggregation (forced — never modal).** Modal-per-role silences the lone dissenter (it is
exactly the signal). Define each role-slot's reading as the **SET** of its seats' types.
`H0 = global section ⟺ the UNION of all seats' types (every role) is a singleton` — any single
dissenter in any role breaks it and can never be averaged away. A non-singleton role-set is *intra-
role fracture* (a lone dissenter inside a role), surfaced, not washed. Manufactured-consensus flag =
included-roles' union is low-disagreement AND the excluded set carries a **genuinely-typed** type
outside it.

**FAIL-OPEN on absence (witnessed-necessary).** Per-item check of the 5 raw manufactured-consensus
candidates found **4/5 were false positives**: the excluded seat was `unknown` (untyped), and
counting `unknown ∉ included-types` as dissent is the Pattern-5 absence-as-presence trap one level
up. Only `employment_boundary__substantive_employment_reading` was genuine (ben:naturalized /
pay:snare / excl:tangled_rope). ⇒ the detector MUST require the excluded seat **genuinely typed**;
absent/`unknown` excluded → `unknown`, never "consensus." And every count is per-item-verified
before it ships (count-as-witness is the failure mode).

**Failure modes to carry (method, not theorem — Perplexity):** (1) `unknown`-as-dissent (above);
(2) modal-washes-the-dissenter (→ set-valued); (3) Option-2's high coverage is *partly synthetic* —
beneficiary/victim are the analyst's directionality priors, not data density, so "comparable by
construction" restates a declared seat (Seat Theorem); carry that caveat to the read site; (4)
excluded coverage-gap (fail-open); (5) slot definitions must be defensible/stable, not post-hoc
labels. **agenda_setter** (the 5th role, ~604 facts) is omitted by "exactly four" — fold into
beneficiary (institutional-side) or keep as a 5th slot; dropping it loses the piton "administrator
who profits" signal (a declared choice).

**What resolution changes:** gives a comparable, party-aware obstruction with a built-in manufactured-
consensus / no-seat-pose detector (pairs with the v8 showing-face guard) — OR concludes the honest
instrument is "a four-seat empty-chair coverage check," with the cohomology dropped as decoration.
Either is a real outcome; the kill condition (cochain reconstruction) decides which. Relates OQ-51
(H1/W1 obstruction + the naming correction), OQ-150 (orbits could key on the role-vector), OQ-56.

---

## OQ-152 — Per-seat naturalization-collapse under suppression: a COMMENTARY diagnostic (beneficiary last to switch)

**Ω-type:** Ω_C (design — a new commentary-grade diagnostic; **annotates, never overrides** the type).

**Status:** open
**Priority:** 3
**Origin:** suppression/scaffold thread off OQ-56 (operator, 2026-06-19); the temporal half is
`design_gaps.md` GAP-14 (this OQ is its commentary-grade static cross-section).

**Specific question:** `naturalized` (`drl_core.pl:416`: `ε > rope_epsilon_ceiling(0.45) ∧ χ <
tangled_rope_chi_floor(0.35)`) is the engine's cover-story type — high real extraction compressed
below detection — and the seat-sweep (`SEAT_SWEEP.md`) shows it is structurally the *beneficiary*
seat's reading. The Litany says visible suppression contradicts naturalness ("heavy enforcement →
snare, not mountain"), and the sharp form (operator) is **per-seat and ordered**: as suppression
rises, the cover story breaks seat by seat — payer/excluded first (they feel it), observer/analytical
next, **beneficiary last, if ever.** The deliverable is a **diagnostic the engine runs and attaches
as commentary**: for each seat, at what suppression level would its `naturalized` reading flip to a
*visible* snare — and the verdict is the **ordering** (monotone, beneficiary highest/→∞).

**Why COMMENTARY-grade, NOT a classification gate (the load-bearing correction, operator
2026-06-19):** do **not** add a suppression term to `classify_from_metrics`. The de-leak of
2026-06-05 *deliberately removed* the extractiveness/suppression maxima "so the authored claim and
the authored metrics stay independent — the engine computes the type; **divergence is the signal**"
(schema `allOf` block 0 comment). Re-gating `naturalized` on suppression would re-introduce exactly
that removed maximum and suppress the signal. The right move is the opposite: leave the type alone
and **surface the divergence** (type=`naturalized` while suppression is high, seat-resolved) as
commentary — the cover-story-collapse ordering is the annotation. This joins the existing
commentary-grade stakeholder diagnostics (`seat_perceived_vs_real`, `consensus_provenance`,
`q6_crosscheck`, `cs_authority_masking`) — it does not touch `drl_core` classification (verdict-grade
distinction: correction-grade overrides, commentary-grade annotates).

**What resolution changes:** Adds the **per-seat suppression-collapse curve** as a reported
diagnostic — the static cross-section of GAP-14's temporal grip-decay (over time, seats abandon the
cover story one by one, beneficiary last). The finding *is* the ordering; the witness is a
constraint where the payer-seat collapse-threshold is below the beneficiary's. No params on the
classifier; a new commentary predicate + a per_constraint output field, with a positive control
(a high-ε/χ-compressed/high-suppression constraint whose payer-seat reading is flagged collapsed
while the beneficiary's is not) and the never-overrides invariant checked. The full temporal
feedback stays in GAP-14.

---

## OQ-153 — Scaffold↔husk as the paired temporal axis: a husk is a naturalized scaffold that locked instead of dissolving

**Ω-type:** Ω_C (design — whether to recognize a scaffold/husk temporal pairing as a first-class
axis, and where husk lives relative to the existing `piton`/`naturalized` types and the committer
attractor; a framework-direction question, not a code fix).

**Status:** open
**Priority:** 3
**Deps:** bundled_with OQ-152
**Origin:** operator framing, 2026-06-19 ("a husk is a naturalized scaffold that locked instead of
dissolving"; the clarifier: scaffold and husk are the two *temporal* categories — scaffold on the
way **up** (temporary, increasing coordination, meant to dissolve), husk on the way **down**
(terminal, coordination hollowing, should have dissolved and couldn't)). Pairs with
`design_gaps.md` GAP-02 (the removed observer-axis husk trajectory-shape read, born-vs-glide) and
GAP-14 (temporal grip-decay); the static seat cross-section is OQ-152. **Sharpened 2026-06-19**
(operator): the five-condition structural definition, the legitimacy-trap correction, the QWERTY/
canon discriminating pair, the de-naturalization death mechanism, and the falsifier below.

**The cut (corrects an earlier draft of this OQ): husk neighbors `naturalized`, not `piton`.** The
discriminator between a constraint that can husk and one that cannot is *not* the piton-ness they
may share — it is **naturalization plus a reading layer**. The canonical pair: QWERTY and a stale
canon are *both* suboptimal Lindy-locked **pitons** (extracting below the fix-threshold), but they
diverge precisely at the husk question. QWERTY **cannot husk** — a pure coordination lock with no
interpretive layer (kernel = "this key layout," practice = typing, no reading between them), its
suboptimality fully visible (everyone knows Dvorak exists), so it cannot naturalize and cannot rot;
it sits as a stable piton until switching-cost falls or a coordinated jump fires. A canon **can
husk**, because it has the reading layer QWERTY lacks — and that machinery is what kills it.

**Five necessary conditions for husking (the structural test / the discriminating control):** (1) a
**kernel**; (2) a **reading layer** thick enough to absorb drift; (3) **naturalization** staking
legitimacy on the kernel's fixity; (4) a **frozen update-authority**; (5) a **moving world**. QWERTY
has (1) and (5) only → stable piton, immune. A canon has all five → husks. The **reading layer is
the pivot**: it simultaneously *enables* naturalization (treat the text as fixed while interpretation
adapts), *absorbs* drift (practice changes, the reading reinterprets, the kernel is declared
untouched), and *hides* drift (where the rot accumulates). No reading layer → no husk.

**Sharpened definition of the husk:** *not* "a kernel that cannot be updated" (a logical lock) but
**a kernel that cannot be updated without destroying the legitimacy the update was meant to serve**
— a **legitimacy trap**. Internal structure is **three layers coming apart**, not two: the **kernel
freezes** (naturalization forbids editing it), the **reading drifts** (the world moves, so it must),
**practice rides the reading**. For a while the growing reading-layer bridges the kernel→practice
gap — this is how every living tradition adapts, by reinterpreting rather than editing scripture.
The husk is the **terminal state of that bridging**: when the kernel↔reading gap is total, the
kernel constrains practice in *nothing* and is retained only as a **legitimacy-token** — pointed at,
never load-bearing. A rope (or snare) wearing a mountain's costume, the costume now empty.

**Placement, restated with the cut:** **husk ≈ scaffold-origin ∩ naturalized ∩ failed-sunset**, but
the *discriminator from a plain piton* is the reading-layer-plus-naturalization, not the
sunset-failure (which piton and husk share). The temporal pairing holds: **scaffold** (ascending —
temporary, coordination rising, *meant* to dissolve) and **husk** (descending — terminal,
coordination hollowing, *should have* dissolved and couldn't) are the two temporal categories; the
husk is the **failed scaffold — the temporary that claimed to be eternal and then couldn't admit it
was temporary.** QWERTY was literally a scaffold (it solved typebar-jamming) that should have
dissolved when the constraint lifted and instead locked into a piton; it did *not* husk only because
it never naturalized. The engine surfaces are: the cover-story type `naturalized` (`drl_core.pl:416`,
high real ε compressed below the χ floor), FNL/`false_natural_law` (a constructed support reading a
naturality claim — the likely witness surface), and the committer husk attractor
(`cs_drift_engine.pl:64,70,80`). What the engine can *currently* witness of the five conditions is
**partial** and itself a finding to map: kernel (`cs_kernel_id` ✓), reading layer
(`cs_reading_relation` edges, multiple readings/kernel ✓ partial), naturalization (FNL / `naturalized`
✓), moving world (`founding_problem_status` dead/live, `disappearance_verdict` ✓ partial), **frozen
update-authority (no obvious authored field — a gap).**

**How a husk ends (two refinements to "eventually disappears"):** husks do **not** die from drift —
they can carry a total kernel↔reading gap for centuries looking healthy, because the reading absorbs
everything and the token still clears. They die from **de-naturalization**: a *phase change* when
enough observers stop reading the kernel as a mountain, at which point the empty costume is seen as
empty and the legitimacy-token stops clearing (gradual hidden drift, *sudden* collapse — why husks
look fine right up to the end). The second exit is not death but **schismatic re-anchoring**: the
Reformation as husk-reversal (`sola scriptura` — drag practice back to the kernel, burn the drifted
reading), reversible only by a new authority declaring the old one hollow, which de-naturalizes it
for everyone watching and produces a fork. So: **a husk ends only by de-naturalization — collapse
when belief is withdrawn, or re-anchoring by a schism that withdraws belief deliberately.**

**The update theorem (no comfortable exit):** there is **no costless update of a naturalized
kernel**. Three paths, two of which are the disease: **(i) override by authority fiat** reveals
updatability and fractures the naturalization (deuterocanon/schism); **(ii) silent reading-drift**
preserves the premium short-term and *is* the husking mechanism long-term; **(iii) relocate
legitimacy from the kernel's fixity to the update-process's integrity** — the only non-husking path,
but itself a one-time **de-naturalization** that costs the naturalization premium (the extra
coordination from people believing it couldn't change). Survivors did (iii) *early*, building a
**licensed, revisable interpretive authority into the kernel itself** so updating is canonical rather
than betrayal (rabbinic interpretation, common law, openly-revisable science). The cure feels like
the loss, so most systems keep the premium and husk.

**Authored falsifier (commit-plus-falsifier):** a naturalized constraint that **edited its kernel by
open fiat and *kept* its mountain-status** — observers watched it change and went on reading it as
invariant. Claim: this does not happen; watching a mountain be edited converts it to a rope in the
observer's eye, irreversibly. Test case to audit: **Vatican II** (open update, attempt to retain
mountain-legitimacy, result a traditionalist fracture — reads as partial husking, not retained
naturalization) — `[UNVERIFIED]`, asserted from memory by the operator, not yet audited.

**Candidate live instances (authored, NOT yet witnessed against the corpus):** **IQ** — all five
conditions, mid-husk now (kernel = fixed biological *g*; naturalized as culture-free natural measure;
reading drifting via renorming + accreting caveats; practice riding the reading; frozen authority
that cannot say "we are revising the construct" without forfeiting the natural-law claim sorting
institutions bank on — the per-generation renorming *is* the silent reading-drift). **AI handed
codifying authority while its outputs are presented as objective** = manufacturing naturalization at
machine speed (= building husks fast; model-collapse is the fast death, husking the slow one; the
non-husking path is the same — legitimacy in a transparent, Lindy-grounded, revisable *process*,
never in claimed objectivity). These are authored exemplars motivating the type, not corpus results.

**What resolution changes:** A ruling on whether the scaffold↔husk pairing is elevated to a named
temporal axis, and which axis carries husk. Three options in increasing cost: **(a) commentary-only**
— surface a `husk`-shaped *annotation* when the five-condition signature co-occurs (`naturalized`/FNL
∧ scaffold-origin ∧ reading-layer present ∧ failed-sunset), mirroring OQ-152's commentary-grade move;
**no** `classify_from_metrics` change, **no** re-introduction of the de-leaked suppression maximum.
**(b) revive the observer trajectory-shape read** (GAP-02 born-vs-glide), disciplined to the committer
axis per `two_axis_architecture_v7.md`'s no-cross-axis-reduction rule. **(c) decline** — rule that
piton + the committer husk attractor already cover the space (then close `future` with the ruling
recorded). The deliverable for (a) needs the **QWERTY/canon discriminating control made concrete**: a
positive witness (all five conditions, flagged husk) separated from (i) a plain **piton** with a
reading layer but no naturalization — the QWERTY case, must *not* flag — and (ii) a plain
**naturalized** cover story with no scaffold origin/reading layer — must *not* flag. All three must
come apart on real constraints before the annotation earns its name. **Do not** add a husk type to
the priority cascade without that control; an undischarged axis-relabel is the silent failure mode
(Build Discipline: unguarded axis-swap). A prerequisite sub-finding: **does the corpus author the
frozen-update-authority condition at all?** If not, the five-condition signature is unfingerprintable
as-is and (a) is itself blocked on a schema gap (route to `design_gaps.md`).

---

## OQ-154 — Scope-design validator on `site_contexts/N` predicates (engine-hardening pair, leg a)

**Ω-type:** Ω_E (mechanical guard — witnessed by constructing a mis-designed site that must trip it).

**Status:** open — minted 2026-06-20 from the OQ-69 ledger drain.

**Priority:** 1

**Origin:** First of the two engine extensions flagged "remain unimplemented" in
`when_apparatus_sharpens_taxonomy.md`: a validator over `site_contexts/N` predicates that catches
the **σ(universal)=1.0 class of site-design failure** (a scope atom whose `scope_modifier` is
unvalidated / collides with `national` → no differential χ effect) *before* the next site is added.
Cross-ref the calibration-based scope exclusion at `constraint_indexing.pl:954–955`
(`regional`/`continental`/`universal` excluded from the product site because their scope_modifier
values are unvalidated). **Nothing shipped** — no such validator exists. Independent of OQ-155/156
(the splitting rule: no real Deps edge to its siblings). Drained from OQ-69 (was the engine-hardening
pair, leg a). **Priority provisional — operator to rule** (Priority is the operator's declared seat).

## OQ-155 — MaxEnt parameterization for arbitrary sites (engine-hardening pair, leg b; gates OQ-156)

**Ω-type:** Ω_E (mechanical engine extension).

**Status:** open — minted 2026-06-20 from the OQ-69 ledger drain.

**Priority:** 4

**Origin:** Second engine extension from `when_apparatus_sharpens_taxonomy.md`: generalize the MaxEnt
parameterization (currently keyed to the canonical / 156-point product sites) so it accepts an
**arbitrary site** of N contexts. This is the prerequisite that **unlocks Arakelov fragility on
10-slice contexts** (OQ-156) — hence it `gates` OQ-156 (the edge is authored on OQ-156 as
`blocked_on OQ-155`, once, to avoid a redundant reverse edge). **Nothing shipped.** Drained from
OQ-69 (engine-hardening pair, leg b). **Priority provisional — operator to rule.**

## OQ-156 — Arakelov fragility on 10-slice contexts (engine-hardening pair, leg c)

**Ω-type:** Ω_E (measurement — fragility computed on a constructed 10-slice site).

**Status:** open — minted 2026-06-20 from the OQ-69 ledger drain.

**Priority:** 5

**Deps:** blocked_on OQ-155

**Origin:** Arakelov-height fragility analysis on a **10-slice context site** (`project_orientation.md`
§8.3, marked **Open**). Genuinely **blocked on** OQ-155: the fragility measurement needs the MaxEnt
parameterization to accept an arbitrary (10-context) site first — the real Deps edge the splitting
rule requires, and the one that lets `menu` surface this as BLOCKED until OQ-155 lands. **Nothing
shipped.** Drained from OQ-69 (engine-hardening pair, leg c). **Priority provisional — operator to
rule.**

## OQ-157 — Spec-encoding unit tests for load-bearing measurement primitives (AGENDA D-1; remaining 4)

**Ω-type:** Ω_E (tests — assertions over documented behavior).

**Status:** open — minted 2026-06-20 from the OQ-69 ledger drain.

**Priority:** 2

**Origin:** AGENDA D-1: encode paper-documented behavior of the load-bearing measurement primitives
as plunit assertions so spec-vs-code drift is caught at commit-time (two witnessed drifts motivated
it). **PARTIAL — 1/5 shipped:** `prolog/tests/test_maxent_profile_indexing.pl` (MaxEnt profile
context-independence) is the *only* spec-encoding test present (witnessed 2026-06-20: `ls
prolog/tests/` shows it and no χ-arg / entropy-norm / H¹ / purity test). **Scope = the remaining
four:** χ argument structure (d, σ), entropy normalization (H/log N), H¹ (signature-resolved orbit —
**pairs with OQ-27** in prose, not a blocking edge), purity propagation rate. Drained from OQ-69.
**Priority provisional — operator to rule.**

## OQ-158 — Cover-story detector enrichments (AGENDA Package B; drive-bys)

**Ω-type:** Ω_E (wiring — one-clause additions, each witnessed by a regression test).

**Status:** open — minted 2026-06-20 from the OQ-69 ledger drain.

**Priority:** 1

**Origin:** AGENDA Package B: wire `drift_event` predicates into `cs_pattern_detection.pl` verdict
clauses (e.g. `extraction_accumulation` + `coupling_drift` → `anchored_fixity_with_accretion`).
One-clause additions + a regression test each; ship as drive-bys. **Nothing shipped.** Drained from
OQ-69. **Priority provisional — operator to rule.**

## OQ-159 — Scaffold/renewal audit (AGENDA Package D)

**Ω-type:** Ω_E (measurement with existing predicates).

**Status:** open — minted 2026-06-20 from the OQ-69 ledger drain.

**Priority:** 3

**Origin:** AGENDA Package D: distinguish **exercised renewal** (= scaffold without drift) from
**performative renewal** (= scaffold + `extraction_accumulation` + `theater_rising`). Testable with
existing predicates. **Nothing shipped.** Drained from OQ-69. **Priority provisional — operator to
rule.**

## OQ-160 — Cluster-level analysis wired into the report (AGENDA Package F; gates OQ-170)

**Ω-type:** Ω_E (wiring — connect computed analytics to the report surface).

**Status:** open — minted 2026-06-20 from the OQ-69 ledger drain.

**Priority:** 2

**Origin:** AGENDA Package F: surface cluster-signature statistics + cluster-level CS inference in
`enhanced_report.py`. **PARTIAL — analytics computed, not wired:** the cluster computation lives in
`cluster_space_phase{3,4,5}.py` but `enhanced_report.py` has **no cluster section** (witnessed
2026-06-20: the only "cluster" hit at `enhanced_report.py:2186` is prose inside a string, not a
report section). **Scope = the wiring** (computation → `enhanced_report.py`), not re-deriving the
analytics. Gates OQ-170 (Package G builds on F; edge authored on OQ-170 as `blocked_on OQ-160`).
Drained from OQ-69. **Priority provisional — operator to rule.**

## OQ-161 — Empirical second/third cases (AGENDA Package C)

**Ω-type:** Ω_E (generation + measurement).

**Status:** open — minted 2026-06-20 from the OQ-69 ledger drain.

**Priority:** 6

**Origin:** AGENDA Package C: author empirical second/third cases — 2026 US midterm
constitutional-legitimacy axes, Colombia 2026; Roman Empire backtest queued for a dedicated session.
**Queued, not authored.** Drained from OQ-69. **Priority provisional — operator to rule.**

## OQ-162 — δ → baseline-deviation reframing (AGENDA Package E; description corrected)

**Ω-type:** Ω_C (conceptual reframe — specify what baseline-deviation semantics δ carries).

**Status:** open — minted 2026-06-20 from the OQ-69 ledger drain.

**Priority:** 4

**Origin:** AGENDA Package E: reframe δ (cognitive displacement) as a baseline-deviation quantity
(theory session first). **Description corrected 2026-06-20 against a witnessed perturbation probe** —
the ledger's "δ not load-bearing in current implementation" was the stale half. Witnessed: δ **IS
wired and load-bearing WHEN SET** — `resolve_displacement/2` (`constraint_indexing.pl:543`) feeds
`D_eff = clamp(D + δ, 0, 1)` (`:580`) into the sigmoid → χ; overlaying `cognitive_displacement :=
0.3` (`probe_harness:with_overlay/3`, caches cleared) flips χ on all 4 canonical contexts, with a
no-op (δ:=0.0) negative control byte-identical and a clean restore (the harness is positive-
controlled on δ's *own* sink, not a proxy path). But δ is **inert at the default config**
(`config.pl:171` cognitive_displacement=0.0; `:174` profile=uniform; all `positional_displacement`
facts = 0.0), so `D_eff = D + 0.0 = D` and δ contributes nothing to live pipeline output as shipped.
**So the reframe targets a parameter that is live-but-zeroed, not dead code.** Probe + witness:
`audits/2026-06-20_oq69_ledger_drain/`. Drained from OQ-69. **Priority provisional — operator to rule.**

## OQ-163 — Python toolset consolidation: single CLI entry point (TODO.md item 2)

**Ω-type:** Ω_E (maintainability — witnessed by the OQ-32 path-resolution regression class).

**Status:** resolved — 1-prime shipped 2026-06-27 (operator ruling); discoverability value
delivered, physical move deferred to OQ-191.

**Priority:** 1

**Resolution (1-prime — dispatcher, no file moves).** Shipped `python/cli.py`: one discoverable
entry point (`python3 python/cli.py list` to discover, `<group> <name> [args]` to run) that groups
every tool logically (grouping is a property of the command tree, not the directory layout) and
dispatches by subprocess everywhere — own interpreter, own `sys.path[0]`, forwarded argv, propagated
exit code, so it cannot change any script's behavior. Positive-controlled `cli selftest` wired into
`scripts/gate.sh` (N>0 per physical group, known-path resolution, synthetic ambiguity catch, misc-
count visibility); gate GREEN. Witness: this section + `cli.py` + gate row.

**Why the move was NOT needed (carry the paths.py finding).** OQ-163's Ω_E justification rested on
the **OQ-32 path-resolution regression class** (the last reorg silently broke 6 scripts' path
resolution). That risk class is **already dead at the root:** `paths.py` provides a depth-agnostic,
`pyproject.toml`-marker-based repo-root finder, ending the inline-root-derivation fork OQ-32 was
about. So moving ~73 files would re-incur that risk to buy only discoverability — which the
dispatcher already delivers. The move is therefore deferred (OQ-191), not abandoned.

**Kill condition (recorded in the weaker-true wording, NOT "genuine zero").** The operator condition
was: 1-prime substitutes for the move only if nothing consumes the directory structure itself. Honest
reading of the probe (2026-06-27): the condition strictly **fired** — one genuine consumer exists,
`verify_reorg.py`, which globs `python/{tests,sweeps,audits}/*.py`. It is **inert under deferral**:
1-prime moves no paths, so it stays green on the status quo and would only need its `DIRS` list
updated *if the physical move ever ran*. Probe sensitivity was shown **independently** (a synthetic
`glob.glob("python/audits/*.py")` line fires; a corpus-dir glob does not — specific, not a catch-all).
Every other glob/walk enumerates corpus/data dirs, not python *script* subdirs. So the discharge is
the sufficient weaker claim — the move buys nothing **now** — not a false "nothing consumes the
structure." Drained from OQ-69.

## OQ-164 — Parameterize the remaining directionality constants (AUDIT W2/E1)

**Ω-type:** Ω_E (mechanical — config-extraction, swept inert).

**Status:** open — minted 2026-06-20 from the OQ-69 ledger drain.

**Priority:** 3

**Origin:** AUDIT W2/E1: lift the hardcoded directionality constants in `constraint_indexing.pl`
into config. **PARTIAL — 6 done:** the `canonical_d_*` constants are already param-specced
(`config_schema.pl:83–88`, witnessed 2026-06-20). **Scope = the two predicates still hardcoded as
facts:** `power_role_heuristic/4` (`constraint_indexing.pl:477+`) and `exit_modulation/2` (`:469`).
Swept inert at ±25% → maintainability-only. **OQ-63's d-derivation work touches the same table** —
do them together if either is picked up (prose lineage, not a blocking edge). Drained from OQ-69.
**Priority provisional — operator to rule.**

## OQ-165 — framing_notes invitation calibration

**Ω-type:** Ω_C (calibration/design decision; fed by an Ω_E measurement input).

**Status:** open — minted 2026-06-20 from the OQ-69 ledger drain.

**Priority:** 5

**Origin:** Does the `framing_notes` invitation produce conceptual- or empirical-leaning omegas? A
calibration signal for generation. The named resolution is a **design/definition decision** about how
the invitation is calibrated (Ω_C), fed by an Ω_E measurement arm (classify the omega distribution the
current invitation produces). **Nothing shipped.** Drained from OQ-69. **Priority provisional —
operator to rule.**

## OQ-166 — Incremental tabling to replace hand-rolled memo caches

**Ω-type:** Ω_E (engine change — output-affecting; zero-diff-witnessed).

**Status:** open — minted 2026-06-20 from the OQ-69 ledger drain.

**Priority:** 2

**Origin:** Replace hand-rolled memo caches with SWI `:- table ... as incremental` — `as incremental`
dynamics auto-invalidate on retract/assert, retiring the manual `cache_registry:clear_all_caches/0`
discipline. **Output-affecting on the hottest path** (`classify_at_context`) — **OQ-02's LCO history
says zero-diff witness first** (prose caution, not a blocking edge). **Nothing shipped.** Drained from
OQ-69. **Priority provisional — operator to rule.**

## OQ-167 — Output write-path anchoring (complete swipl location-independence)

**Ω-type:** Ω_E (mechanical — anchor writes the way reads are anchored).

**Status:** open — minted 2026-06-20 from the OQ-69 ledger drain.

**Priority:** 3

**Origin:** Exporters / probe scripts still write cwd-relative `../outputs/...`; anchoring writes the
way corpus **reads** are now anchored (`resolve_corpus_dir/2`, 2026-06-04) would complete swipl
location-independence and retire the remaining `cd prolog/` requirement
(`swipl_load_path_and_probe_gotchas.md` §9). **Nothing shipped.** Drained from OQ-69. **Priority
provisional — operator to rule.**

## OQ-168 — Author "the mint" as a testset (the essay→engine loop, first deliberate instance)

**Ω-type:** Ω_P (decide-to-author program act) — **resolved as not-actionable**; the imagined Ω_E
discharge ("engine resistance") reduces to a consistency check, below.

**Status:** resolved — closed 2026-06-27 (operator ruling). Nothing to author. The "essay → engine
resistance → sharper essay" loop over-described what the engine does: it evaluates an LLM-authored
constraint story and reports whether the authored fields are internally consistent with the authored
type-claim. There is no oracle on whether a real-world regime "is" a mountain — only contradiction
detection. So "let the engine fight an honest prior" has no content beyond "the engine flags when
your numbers contradict your label," and there is no further work item here.

**Origin (kept for trace):** 2026-06-11 Pew-typology exchange proposed authoring the information
regime ("the mint") as its own constraint and letting the engine "fight" an honest `mountain` prior
the way it refused `institutional_trust_erosion`'s mountain claim (`design_discipline.md` §4).

**Witness that closes it (2026-06-27, `enhanced_report.py institutional_trust_erosion_c0`):** the
"refusal" is just a claim/data contradiction, and mountains are invariant by definition.
`institutional_trust_erosion_c0` claims `mountain` (`:118` + `emerges_naturally` `:122`) while
authoring non-invariant data: `base_extractiveness 0.68` (`:104`), `suppression_score 0.52` (`:105`,
vs the 0.05 mountain ceiling), and a **rising** ε-series 0.38→0.68 over t=0..60 (`:315–:327`). The
report's FALSE-MOUNTAINS forensic fires (`dr_type: unknown` at all 4 seats; signature
`constructed_high_extraction`; MaxEnt favors `snare` 0.64; `verdict_join=red`,
`type_1_false_summit`). The engine did not adjudicate the world — it caught a mislabeled story. That
is the whole mechanism, and it needs no new constraint to demonstrate. Corollary: the essay→engine
loop's value, if any, is bounded to coherence-auditing an authored encoding, never thesis-validation.

## OQ-169 — T4 (confirmed_liminal) one-case category (dormant; trigger-deferred)

**Ω-type:** Ω_E (categorization measurement; awaits a second live case).

**Status:** open — deferred (dormant): re-examine on a 2nd live `confirmed_liminal` case; trigger `abductive_triggers:trigger_confirmed_liminal/3`. Minted 2026-06-20 from the OQ-69 ledger drain.

**Priority:** 8

**Origin:** Re-examine the T4 (`confirmed_liminal`) category when a **second** live case appears;
currently 1 live case (`crypto_permissionless_reading`). Minted **dormant** — it stays in the active
frontier (edge-free) but at low Priority and the Status detail names its trigger, so a future instance
re-examines only when the substrate produces a 2nd case. Drained from OQ-69. **Priority provisional —
operator to rule.**

## OQ-170 — Cluster-level systematic exploration (AGENDA Package G; blocked_on OQ-160)

**Ω-type:** Ω_E (analysis — systematic clustering exploration).

**Status:** open — minted 2026-06-20 from the OQ-69 ledger drain.

**Priority:** 9

**Deps:** blocked_on OQ-160

**Origin:** AGENDA Package G: systematic clustering exploration, building on Package F (OQ-160). The
ledger sequenced it "then Package G … after" Package F, so the F→G dependency is **real** — by the
splitting rule the cluster item is two OQs, and this `blocked_on OQ-160` edge is what lets `menu`
surface G as **BLOCKED** until F's cluster stats + report wiring land. **Nothing shipped.** Drained
from OQ-69 (**operator ruling 2026-06-20:** the cluster item splits F/G; minted as OQ-170 because
154–169 were already assigned when the split was ruled). **Priority provisional — operator to rule.**

## OQ-171 — Does the SCOPE *construction path* saturate? (the construct-validity gap OQ-71 left standing)

**Ω-type:** Ω_E (corpus measurement — but possibly not cleanly constructible; see the obstruction).

**Status:** open — minted 2026-06-20 from the OQ-71 Phase A close; construct-validity gap named, experiment design specified, spend + pricing deferred to operator.

**Priority:** 2

**Deps:** splits_from OQ-71, blocked_on_human operator-spend-go

**Origin:** 2026-06-20, OQ-71 Phase A two-path-architecture finding (operator-surfaced). §3's bounded-attractor claim is about the **SCOPE construction path** — does the model, growing the corpus the way it is actually grown (SCOPE constructs each kernel from a topic), exhaust the structural-class vocabulary (generator-specific S_max). OQ-71 stressed boundedness with a regime the SCOPE path never enters — **Opus-hand-designed structure rendered through no_scope** — so its 1.5× falsifier killed only **substrate-level** boundedness (the rendering substrate can express more classes than the live corpus holds), NOT **path-level** boundedness. §3's own claim is therefore left standing as within-regime; OQ-71 does not re-engage it. (§10.1 close, `a_hypothesis_about_corpus_size.md`.)

**Specific question:** Does the SCOPE construction path keep discovering structural classes when allowed to condition on accumulated structure, or does it saturate? **Clean design (context-controlled batch-of-one):** same topic set, SCOPE-construct each kernel, vary ONLY whether construction sees prior structure — batch-of-one against a *growing* store (full inline context) vs batch-of-one against an *empty* store (context-blind, reproducing the no_scope condition through the SCOPE path). Holds topics and construction-model fixed; varies the one thing §3 is about. **Explicitly DECLINE the naive "just decompose in smaller batches and count" version:** batch size covaries with both (a) inline-vs-pre-decomposed construction and (b) how much accumulated context exists when a batch runs, so "smaller batches re-open discovery" is a disjunction by construction — it buys another *mitigated*, the exact OQ-71 failure one regime over.

**The obstruction (A0 recurs, name it before any spend):** the variable you want to turn — realized depth / accumulated-context in the construction path — is not a clean input the SCOPE path exposes. Forcing SCOPE deep means either (a) feed deeper *topics* and hope it constructs nested structure → conflates topic-domain richness with nesting depth, or (b) inject parent-context into the SCOPE prompt → a third regime, no longer the path §3 is about. The context-controlled batch-of-one design above is the one framing that varies inline-context cleanly; whether even it is fully constructible at the corpus's real scale is open. The honest alternative outcome is to **register §3 as path-untestable-as-stated** and accept the substrate-level falsification OQ-71 already has as the most that is reachable.

**What resolution changes:** this is the only experiment in the OQ-71 tree that re-engages §3 on its own turf with a clean falsifier. Either it shows the SCOPE path keeps discovering (boundedness genuinely weakens, not just within-regime) or it saturates (boundedness survives its own-path test) — or it proves non-constructible, which is itself a finding about §3's falsifiability. **Gated on:** an operator pricing confirmation (batch-of-one at corpus scale is many small API calls, a different cost shape than OQ-71's batch) + spend-go. Ranking if a graduation experiment is funded: SCOPE-depth-if-constructible (this) > OQ-71's Opus-flat reading-(b) > SCOPE-on-same-topics (cheapest, most confounded).

---

## OQ-172 — Injection-suppression lives in the backend, not the corpus: a resurrected orchestrator could re-inject

**Ω-type:** Ω_P (one-path / where-policy-lives design) + Ω_E (witnessable by code-read of any orchestrator that gets resurrected).

**Status:** resolved — retire-the-path executed (operator ruling, 2026-06-21). The live injection
site is deleted, not relocated.

**Origin:** OQ-81 placed injection-suppression in a *backend predicate*
(`generate_kernel_corpus._flat_seeds_from_manifest`), not a corpus/schema invariant. OQ-82's close
confirmed every live generation route is injection-safe; the sliver was the **legacy
`agent/orchestrator.py`** (`DRAuditOrchestrator`, driven by the dormant Streamlit apps
`agent/app.py` + `agent/c-app.py`): its `_step_generate` (:343–358) threaded the upstream story's
`claimed_type` into the downstream `build_prompt` over **all** `downstream_of` deps, with no
suppression filter — the OQ-81 channel, live in code, only its drivers dormant.

**Resolution (witnessed):** the operator chose **retire-the-path** over hoist-the-policy. The three
files were deleted (`git rm agent/orchestrator.py agent/app.py agent/c-app.py`) — a closed,
superseded cluster: `orchestrator.py` (the Gemini-era orchestrator, replaced by `c-orchestrator.py`)
was imported only by the two Streamlit apps; the apps were imported by nothing; no test / shell
script / packaging entry-point referenced any of the three (import grep cleared with a positive
control on `story_generator_base`'s 10+ importers). Orphaned `streamlit` dep removed from
`requirements.txt` + AGENTS.md (kept `google-*`, still used by ~8 live files); stale
`agent/orchestrator.py` usage lines in `c-orchestrator.py`'s docstring fixed. The injection site no
longer exists, so the policy-location question is moot for the retired path.

**Standing tripwire (the gate survives the close):** injection-suppression still lives in a backend
predicate, not a bypass-proof invariant. **Before adding or reviving ANY generation front-end,
code-read its generation step:** if it threads an upstream computed verdict (`claimed_type`) into a
downstream prompt over `downstream_of` deps AND does not route through `_flat_seeds_from_manifest`,
it re-opens the OQ-81 channel. Route every new generation path through the unified backend
`generate_from_manifests`, never a hand-rolled prompt-assembly loop.

**Deps:** splits_from OQ-82, blocked_on OQ-81 (the suppression predicate it governed).

**Deps:** splits_from OQ-82, blocked_on OQ-81 (the suppression predicate it governs).

---

## OQ-173 — MaxEnt signature-override boost made seat-aware (the OQ-138 maxent residual) (Ω_E)

**Ω-type:** Ω_E (mechanical before/after diff: does the MaxEnt boost still fire at routed seats?).

**Status:** resolved — 2026-06-21 (seat-aware skip shipped + witnessed; 21-corpus generality sweep;
`audits/2026-06-21_maxent_seat_aware/FINDINGS.md`). Repoints the two OQ-138 maxent residuals
(constructed-3 `:341`, FCR-9 `:331`).

**Origin:** OQ-138's type-layer conversions (FSM/FCR-9/constructed-3) made `dr_type` and the
diagnostic consumers seat-aware, but `maxent_classifier.pl`'s parallel override
(`apply_override_for_sig`, signature-level, no `C`) still boosted the **routed** seats toward the
override target — the MaxEnt layer manufacturing where the type layer stopped. Verdict-capable
(feeds `probe_maxent`), so treated as an output-changing conversion with its own witness.

**Resolution (witnessed).** Threaded `C` through `apply_override_for_sig/3→/4` (single call site,
maxent_classifier.pl:318); skip the boost at routed seats reusing the existing
`signature_detection:fcr_routed/1` + `constructed_routed/1` (verbatim, unbound-cascade keyed — no
new predicate, no bound-arg mis-key). Non-converted clauses ignore `C` (byte-identical). FSM has no
MaxEnt boost path (catch-all no-op — confirmed against substrate, scope holds). Witness
(`diff_witness.out`): exactly the **12 routed seats** (9 fcr + 3 constructed) revert to their raw
distribution; **0** non-routed seats move on any maxent surface (negative half byte-clean — raw-probs
discriminator); **1** categorical flip — `shinbutsu` indexed top tangled_rope→snare (the one
genuinely-manufactured verdict; classical tops unchanged because the conditional boost never flipped
a classical argmax — refines OQ-138's "flips maxent_top" framing); **0** `verdict_join` changes.
21-corpus sweep: `routed_STILL_boosted=0` everywhere; non-converted boosts intact;
`original_v5` PARTIAL (pre-existing `maxent_run` failure, NOT a regression — stash-confirmed).
`validation_suite` 92/0/0; `check_stack` baseline-clean; `gate.sh` GREEN. Incidental: pruned a stale
`maxent_classifier.pl:852 [C2]` load-warning allowlist entry (singleton renamed `_`).

**Priority:** 4

**Deps:** splits_from OQ-138
*(Workable-now at mint — `fcr_routed`/`constructed_routed` already existed; no human ruling owed,
the design seat was ruled with OQ-138. Resolved same day.)*

---

## OQ-174 — `cs_reading_relation` feeds `contamination_network` explicit edges: shared-input, Theorem-7 intact (Ω_C)

**Status:** resolved — benign carve-out (shared authored input, not
detection-dependence). OQ-20 audit Arm 2, 2026-06-22.
**Priority:** 3
**Files:** `prolog/drl_purity_network.pl` (`constraint_neighbors/3`, ~67/92/257);
`prolog/json_report.pl` (`write_contamination_network`)

**Finding:** Arm 2 (strip all `cs_*` from kernel_v1, HEAD code, E vs F, empty
floor) found the DR observer core fully detection-independent (`claimed_type`,
`perspective_chi`, `signature`, `maxent_*`, `classifications`, `purity` all
unchanged) — the sole moving DR field is `contamination_network` (180 stories:
152 cs-bearing + 28 cs-free neighbours, semantic neighbour-set changes, 0/180
ordering-only), because `constraint_neighbors/3` reads `cs_reading_relation` into
its `explicit` edges.

**Ruling (the crux was empirical, settled by substrate):** `cs_reading_relation`
is an **authored corpus fact** — `narrative_ontology:cs_reading_relation('uuid',
other_reading, coexists_with|forecloses)` written into the testset files at
generation time, **never asserted by any engine pass** (no `assert`/`dynamic`;
cs-analysis files use it only inside `once(...)`/`\+(...)` read guards). It exists
independent of whether anything was *classified* cs. So contamination_network
reading it is a **shared-input dependency**, the same pattern as its
`shared_victim`/`shared_beneficiary` edges — **not** detection output feeding
back into detection. Theorem 7 forbids the latter; the former is intact. The
plan's "200 cs-free byte-identical" negative control "fails" precisely because
the authored relational edge couples cs-free neighbours — a feature of the
finding, not a strip bug or a leak. Residual design note (optional): the FPN
could gate `cs_`-prefixed authored edges for axis tidiness; not a correctness
issue. Witness: `audits/2026-06-22_oq20_dr_baseline_diff/WRITEUP.md` §Arm 2.

**Reopen condition (kill-switch):** this resolution holds *because*
`cs_reading_relation` is authored, not detected. If any future commit makes a
pass **assert/asserta/assertz `cs_reading_relation`** (produce it from the
cs-detection pass rather than load it from testsets), the shared-input argument
collapses into detection→detection feedback — reopen and re-rule.

---

## OQ-175 — MaxEnt `maxent_top_type` shifted `tangled_rope → snare` on ~2261 constraints across the CS window (Ω_E)

**Status:** open
**Priority:** 3
**Origin:** OQ-20 audit Arm 1, 2026-06-22 (`audits/2026-06-22_oq20_dr_baseline_diff/`).
**Files:** `prolog/maxent_classifier.pl`, `prolog/config.pl` (snare/tangled_rope
MaxEnt thresholds/features); diff evidence
`audits/2026-06-22_oq20_dr_baseline_diff/analysis.json`

**Specific question:** OQ-20 found the MaxEnt argmax (`maxent_top_type`) flips
across the tag→HEAD CS window — 297/1017 (29%) on original_json, **2448/3373
(73%) on original_v6** — and the flips are **not scattered** (which recalibration
would be): **2261 of the 2448 original_v6 flips are the single ordered pair
`tangled_rope → snare`**. A flip that concentrated along one classification edge
is a *boundary that moved in one direction*, not diffuse noise — either the
snare/tangled_rope MaxEnt decision threshold shifted, or a feature feeding that
one contrast changed somewhere in the 707-commit window. This is a single
bisectable cause, the way the OQ-20 id change pinned to `801390a5`.

Note this is the **MaxEnt probabilistic** type surface, distinct from the
priority-cascade `claimed_type` (which is byte-stable across the same window —
OQ-20). The two classification surfaces disagree on these constraints; that
disagreement is itself the interest (does the recalibrated MaxEnt top-type still
track the cascade verdict, or has it drifted off it?).

**What resolution changes:** Bisect the `tangled_rope→snare` boundary move to a
commit/threshold change; decide whether the shift is an intended recalibration
(then document it) or an accidental drift in the snare/tangled_rope contrast
(then assess whether HEAD's MaxEnt top-type is better or worse calibrated than
the tag's). Measured on archive corpora (original_v6 chimera-era); re-witness on
the live corpus before citing magnitude.

---

## OQ-176 — `cohomological_obstruction/3` returns H¹=0 for an absent constraint (Pattern-5 measured-flat vs didn't-look) (Ω_E)

**Status:** open
**Priority:** 4
**Origin:** Witnessed 2026-06-23 during OQ-10 build. Calling
`grothendieck_cohomology:cohomological_obstruction(no_such_reading_xyz, H0, H1)` on a
non-existent constraint SUCCEEDS with `H0=1, H1=0` rather than failing: `orbit_vector/2`
returns a uniform all-`unknown` vector for the absent constraint → `UniqueTypes=[_]` →
global-section/no-obstruction verdict (`grothendieck_cohomology.pl:158–169`). So a genuinely
flat constraint (perspectives agree, H¹=0) and an absent/perspective-less one collapse to the
same `0` token — build_discipline Pattern 5/6 (absence satisfies the gate; measured-empty vs
didn't-look).

**Why it matters / scope:** does NOT affect OQ-10 — `reading_robustness` only ever calls this on
readings drawn from `cs_readings_for_kernel/2`, which are always registered, so H¹ is always a
real numeric (the `h1_band_robust=null` fail-closed branch is consequently exception-guard only,
never absence-triggered). The defect is latent for any future consumer that calls
`cohomological_obstruction/3` on an unvalidated constraint id and reads `H1=0` as "measured flat."

**What resolution changes:** a ruling on the return contract for an absent/perspective-less
constraint — fail, an `unknown` sentinel, or keep `0` with a documented "callers must
pre-validate membership" contract — plus a consumer sweep (6+ callsites: `json_report` `h1_band`,
`corpus_cohomology`, `descent_status`, `subobject_classifier`, the orbit reporters) confirming
none rely on absent→0. Engine-behavior change to a core widely-consumed predicate; above the
fix-on-sight threshold (needs the ruling + the sweep), hence logged not patched.

---

## OQ-177 — OQ-06's presumed off-cases for the NON-target drift/axiom conjuncts are unwitnessed (Ω_E)

**Status:** open
**Priority:** 4
**Deps:** splits_from OQ-06
**Origin:** Flagged during the OQ-06 resolution (2026-06-23,
`audits/2026-06-23_oq06_offcase_fixtures/`). OQ-06 scoped to four conjuncts
(`cs_drift_unacknowledged` C3/C4, `cs_axiom_foreclosed` C2/C4) and **presumed** the
remaining conjuncts already had live off-cases — asserted without a visible witness:

- `cs_drift_unacknowledged` C1 — `cs_drift_state(UID,_,_)` absent (UID has no drift state at all)
- `cs_drift_unacknowledged` C2 — `acknowledged=true` (an acknowledged gap, off the `false` slot)
- `cs_axiom_foreclosed` C1 — `cs_axiom(UID,_,Atom)` absent
- `cs_axiom_foreclosed` C3 — drift direction ≠ `axiom_overriding`

**Why it matters / scope:** same half-spec-tested logic OQ-06 closed for its four conjuncts —
a conjunct never observed to stay silent when it should has only half its spec exercised. These
were left out of OQ-06 deliberately (the operator's scope call) to keep what OQ-06 certified
crisp; they are a *separate* presumption, not a silent expansion of OQ-06. Low priority: the
OQ-06 Phase-A scan already passes over these same facts, so witnessing their off-cases is nearly
free (reuse `search.pl`'s bucket pattern + two-sided planted controls). The axiom-C1/drift-C1
"fact-absent" cases are Pattern-5-shaped (a gate over a possibly-empty table) and worth a
fail-closed check specifically.

**What resolution changes:** extends the OQ-06 both-directions witness to the full conjunct set
of both predicates — closes the last "asserted absent, never searched" gaps in
`cs_drift_unacknowledged/2` and `cs_axiom_foreclosed/2`. No engine change expected; a search +
matched-pair probe in the same audit style.

---

## OQ-178 — Off-grid Time=0 probing in cs_kernel_registry mis-classifies temporal-series readings

**Ω-type:** Ω_C (design choice — where to fix an ill-posed off-grid query: the probe or the classifier).

**Status:** resolved — SUPERSEDED. The latest-snapshot probe-fix (`9fde36c9`) was itself reverted
to static `dr_type/3` (operator ruling 2026-06-25, commit `5b069ae1`); the static path dissolves
both OQ-178 and OQ-179 at the root. See **Supersession** below; the interim resolution is kept for
provenance.
**Priority:** 1
**Deps (dropped on close):** was `blocked_on_human oq178-probe-fix-vs-classifier-semantics-ruling`;
ruled 2026-06-25 — first the latest-snapshot probe-fix, then superseded by static `dr_type/3`
(same seat, re-ruled the same day on the principle below).

**Supersession (2026-06-25, commit `5b069ae1`).** `cs_kernel_divergence/4` and the
`compare_kernel_readings/3` JOIN now classify with static `dr_type/3` (time-neutral), mirroring
`perspectival_incoherence`. **The principle (independent of any one constraint):** binding ANY DR
measurement time into a `cs_*` cross-reading comparator is a category intrusion — the moving axis is
reading/perspective, not time. And *latest*-snapshot specifically reads a COLLAPSING constraint at
its terminus, where the latest authored ε can be 0 → DR-type `unknown`; `unknown==unknown` then
reads as agreement and MASKS a real divergence. Static reads the representative authored ε. The
`reading_snapshot_time/2` helper is dropped (no external users); the OQ-51 `is_real_type` N/A filter
is preserved (load-bearing for the join invariant). **Output-changing, witnessed against regenerated
`json_report`** (probe == json: 16/8 before, 18/8 after): live corpus (n=97)
`cs_kernel_divergence_count` 16→18, kernels 8→8; the +2 recovered pairs are both
`visual_evidentiary_authority` (`post_evidentiary` × `indexical_realism` / × `distributed_verification`),
genuine type≠type (`snare ≠ tangled_rope`/`naturalized`), **zero unknown-pairings** (the OQ-37
artifact did NOT occur). Twin corpora corroborate the recovery direction at scale: `testsets_haiku`
861→893 pairs (+32, +3 kernels), `testsets_flash` 813→846 (+33, +4). **NB:** `shinbutsu` — the
motivating collapse exemplar that drove the interim audit's larger numbers — is now a SINGLETON
reading in the live corpus and produces no live pair; the reversal stands on the principle, not on
that constraint's ε=0. (The plan predicted 22/9 on an earlier corpus snapshot; corpus drift since
makes it 18/8.)

**Resolution — INTERIM, now superseded (2026-06-25).** Falsifier resolved against a shared probe time: `cs_kernel_divergence`'s
output carries **no time field** and **no consumer keys on time** (counts → `json_report` only;
OQ-119 exports key on `spatial_scope`; `cross_reading_diff`/`enhanced_report` on kernel/context) —
so the comparison is per-CONTEXT, not time-aligned, and each reading may be read at its OWN valid
time (no common-floor needed). New `reading_snapshot_time/2` = max authored `base_extractiveness`
time (fallback 0); applied to BOTH `cs_kernel_divergence/4` and the `compare_kernel_readings/3`
JOIN (invariant preserved 42/42). **Operator ruling: LATEST authored time** (current/most-developed
state; earliest under-detects divergence, which accretes along the trajectory). Witness:
`cs_kernel_divergence_count` 17→20, JSP divergence preserved (1→1, now correct types), all 15
off-grid readings on-grid (0 hit the 0.5 default), 32 readings re-based T=0→latest ALL from authored
ε (no fabrication by construction), validation 0/1/1. NB the single-snapshot is lossy (9/15 readings
change type across grids; `shinbutsu` de-differentiates) and `max(T)` is chronologically-earliest for
BC-encoded stories (`lycurgan`; OQ-105) — both carried into **OQ-179**.

**Finding (2026-06-24, audit `audits/2026-06-24_oq41_basex_t0/`).** `cs_kernel_registry`
(`cs_kernel_divergence/4`, `compare_kernel_readings/3`) classifies every reading at a fixed
**Time=0** to compare them ("baseline comparison across readings" — code comment line 61; a
synthetic SENTINEL, not a shared historical origin — falsifier resolved). But 15 live-corpus
constraints author `base_extractiveness` ONLY as a temporal `measurement/5` series at real years
(1450, 1700, 480 BC…), none at Time=0 — so the Time=0 probe is **off every story's grid**. There
`classify_at_time_with_supp` falls to the `BaseX=0.5` impute (OQ-41 rows 24–25) and classifies off
a value nobody authored. This is NOT OQ-44 absence (0/15 genuinely ε-absent) — it is an ill-posed
off-grid query. Witnessed harm: `jewish_sovereignty_palestine` reads `settler_colonial`=snare vs
`cultural_zionist`=scaffold at every authored time (true divergence), but the Time=0 probe rode the
0.5 accidentally-preserving it; fail-closing to `unknown` (OQ-44 reflex) instead ERASES it
(both→unknown, `robust_context_count` 0→156). So both impute-0.5 and fail-closed-unknown are wrong
at Time=0.

**The design seat (split, operator recommendation 2026-06-24):**
- **Probe fix (substrate correction, settle-now class):** `cs_kernel_registry` should probe at a
  story-VALID time, not the synthetic Time=0. Wrinkle to rule: readings in one kernel can have
  different grid starts (JSP: 1917 vs 1900), so no single on-grid time is shared — candidates are
  *latest-common* (JSP: both have 2024) or per-reading-nearest. This is the small choice to settle.
- **Classifier semantics (separate, genuine OQ-105 seat):** what should `classify_at_time(C, T, …)`
  return for an off-grid `T`? (nearest-authored / constant-extrapolate / `unknown`). Recurs anywhere
  off-grid times are queried, so do NOT fold it into the probe fix to paper over one caller.

**What resolution changes:** restores correct kernel-divergence verdicts for temporal-series
constraints; unblocks OQ-41 rows 24–25 (re-pointed here) and OQ-39 row 14 (trajectory gate shares
the off-grid wrinkle). The `unknown==unknown`-as-agreement absorption it exposed is tracked as an
OQ-51 build-extension (independent, can land in parallel).

---

## OQ-179 — cs_kernel_divergence is a single-snapshot comparison; a trajectory-aware measure is more faithful

**Ω-type:** Ω_C (design choice — point comparison vs trajectory comparison for kernel divergence).

**Status:** resolved — MIS-PREMISED (operator ruling 2026-06-25). Its "trajectory" conflated two
distinct temporal elements; `cs_kernel_divergence` reverted to static `dr_type/3` (commit
`5b069ae1`, the OQ-178 supersession), removing the snapshot entirely and dissolving the question at
the root.
**Priority:** 2
**Deps (dropped on close):** was `blocked_on OQ-178` (OQ-178 superseded; this OQ dissolved with it).

**Resolution — mis-premised (2026-06-25).** OQ-179 asked `cs_kernel_divergence` to become
trajectory-aware over the DR `measurement/5` series. That conflates **two distinct temporal
elements**: the DR measurement series (`classify_at_time`) and the CS lifecycle trajectory
(`cs_reference_frame`→`cs_drift_state`→`cs_drift_trajectory`, role-indexed). A `cs_*` cross-reading
comparator should be NEITHER — its moving axis is reading, not time (see OQ-178 Supersession). The
revert to static `dr_type/3` removes the single snapshot, so there is no chosen moment left to make
"trajectory-aware." **Re-homing the genuine signal:** the affirmative observation — sibling readings
can classify to different DR-types at different points of their OWN measurement grids (9/15 in the
old audit) — is a real question, but it belongs to the **DR temporal subsystem**
(`drift_trajectory`/`temporal_residual`/`degradation_chain`; OQ-110 family), measured on the DR
axis, NOT bolted into the CS-layer comparator. It is deferred there: the live corpus is
singleton-sparse (no kernel has two temporal-series sibling readings), so sibling-trajectory
divergence is only measurable on the twin corpora. **OQ-105 BC-encoding fold is now moot for this
path** — static `dr_type/3` never takes `max(T)`, so the numeric-vs-chronological-time mismatch does
not arise here.

**Origin (OQ-178 close, 2026-06-25).** OQ-178 fixed the off-grid bug by reading each kernel reading
at its LATEST authored time. But that is still a SINGLE snapshot, and the same audit witnessed that
the snapshot is lossy by construction: **9 of the 15 off-grid readings change type across their own
grids** (e.g. `settler_colonial` tangled_rope@1917→snare@2024; `shinbutsu` tangled_rope@1603→rope@1868
— **de-differentiating**, the opposite direction), and corpus-wide **32 readings shift type T=0→latest**.
Two readings identical at their latest time can diverge for the entire prior century; a latest-snapshot
reports them as agreeing. The honest signal is "do the readings' TRAJECTORIES ever diverge," not "do
they classify differently at one chosen moment."

**The design question.** Is `cs_kernel_divergence` a *point* comparison or a *trajectory* comparison?
The 9/15 (and 32 corpus-wide) type-instability is affirmative evidence for trajectory: a kernel could
diverge if its readings classify differently at any shared/overlapping authored time, or over the
union of their trajectories. Larger redesign of `cs_kernel_divergence`/`compare_kernel_readings` than
the OQ-178 probe-time swap — its own produce-then-gate.

**Also fold in here:** the `max(T)`-is-chronologically-earliest issue for **BC-encoded stories**
(`lycurgan` 480..330 positive-descending; OQ-105 encoding family) — a trajectory measure that reads
the authored series in order sidesteps the numeric-vs-chronological-time mismatch a single `max(T)`
snapshot inherits.

**What resolution changes:** replaces a lossy single-moment divergence verdict with one faithful to
the temporal model the corpus authors; removes the inherited BC-encoding snapshot artifact.

## OQ-180 — Sibling bare-`\=` diagnostic surfaces + audit-dir silent h1_band sites (residual OQ-51 N/A)

**Ω-type:** Ω_C (design choice — same N/A rule, separate diagnostic surfaces).

**Status:** open
**Priority:** 3
**Deps:** splits_from OQ-51

**Origin (2026-06-25, OQ-51 main build).** The OQ-51 main build (commits `f8ae0c9c`/`15cca7ed`)
applied the `unknown`-is-N/A rule to the canonical H1/sheaf path and the product-site H1, but two
sibling diagnostic surfaces use the **same bare-`\=`** pattern and were deliberately NOT folded in
(separate-surface ⇒ separate change):
- `drl_core.pl:586` `perspectival_incoherence` — bare `\=` over seat type pairs.
- `grothendieck_cohomology.pl:504` `find_boundaries`/`transition_boundaries` — `T1 \= T2` counts an
  `unknown`→real transition as a boundary.
Each computes a *diagnostic* (not a headline verdict), so the impact is annotation-level, but the rule
is the same: an `unknown`-involving pair is N/A, not a disagreement/boundary. **Also fold in here:** the
**3 audit-dir silent `h1_band` readers** the main build's containment deliberately did NOT guard (they
are retrospective tooling, not the live analysis surface, and one already has an `isinstance` null-guard):
`python/audits/cc_diagnostic.py:1226`, `python/audits/sheaf_audit.py:171`, `python/audits/audit3_synthesis.py:138`
(each `.get('h1_band', 0)` — silently coerces a null undetermined to 0). **Witness carried:** the grep
of the bare-`\=` at each Prolog site + the 3 audit-dir `.get('h1_band',0)` sites above.

**What resolution changes:** extends the N/A rule to the sibling diagnostics and makes the audit-dir
readers null-aware, closing the last silent `unknown`-as-disagreement / null-as-0 surfaces.

## OQ-181 — Per-site `undetermined` semantics for the 13 canonical h1_band readers (OQ-51 follow-up)

**Ω-type:** Ω_C (design choice — what `undetermined` MEANS at each consumer, per-site judgment).

**Status:** open
**Priority:** 2
**Deps:** splits_from OQ-51

**Origin (2026-06-25, OQ-51 main build).** OQ-51 made `h1_band` nullable (null = undetermined). The
main build's containment (`shared/loader.py:h1_band_or_raise`) makes a null read fail **loud** at the 13
live readers — but loud is a stopgap: each site needs a *per-site semantic* decision (exclude / bucket /
report) for what `undetermined` means there. That is per-site judgment, deliberately kept OUT of the
output-changing core commit. The 13 readers had **inconsistent null-defaults** (this table is the
witness):

| default pattern | files |
|---|---|
| `or 0` | `orbit_characterization.py` (×6) |
| `.get("h1_band", 0)` | `game_theory_nash.py`, `game_theory_cover_story.py`, `game_theory_stability.py`, `game_theory_mixed_strategy.py`, `run_drift_mismatch.py`, `tangled_rope_sign_flip.py`, `corpus_profile.py` |
| `.get("h1_band", -1)` | `sweeps/epsilon_sensitivity.py` |
| `.get("h1_band")` raw | `tangled_gradient.py` |
| already None-aware (correct) | `w1_sheaf_join.py`, `enhanced_report.py`, `h1_distribution_shape_test.py` (fixed in main build) |

**Also mint here (refactor, separate from semantics):** centralize the per-site guard into a
`load_per_constraint(path)` loader helper that materializes the sentinel once, and migrate the readers
to it (the main build chose per-site `h1_band_or_raise` calls over this refactor to keep the core commit
scoped).

**What resolution changes:** replaces 13 loud-stops with correct per-site `undetermined` handling, so
the H1-bearing analyses run (rather than crash) on an undetermined-bearing corpus.

## OQ-182 — Revive + validate the dormant HAC structural-family / cross-domain-twin commentary subsystem

**Ω-type:** Ω_C (design choice — what the diagnostic organ is for; commentary-grade annotation, never reclassification).

**Status:** mitigated — **family product SHIPPED: `trajectory_enabled=1` (2026-06-27).** Precondition (a) SATISFIED — the giant_comp/trajectory serialization fix landed and was witnessed at the mechanism level (pre-fix co-residency captured, cured arm disjoint), N=10 freshness battery 10/10 GREEN, C0 re-witnessed zero-diff. Validated two ways (C-null +5.01σ single-corpus; C-gen substrate TRACK=162/162=1.000 local-invariance); A1 C0 PASS, kernel_v1 C-prov PASS. Precondition (b) (kernel_v1 C-null breadth addendum) remains optional/non-gating. Twin product OPEN (deferred to rebuild).
**Priority:** 2
**Deps:** bundled_with OQ-91

**Recon verdict (2026-06-25).** `prolog/context_profile_mining.pl` (the HAC structural-family /
cross-domain-twin module, formerly `trajectory_mining.pl`) is dormant
(`config:param(trajectory_enabled, 0)`, `config.pl:571`). Its crash was fixed during the OQ-16 rename
(commit `fc9b4688`), but fixing the crash did not validate the subsystem. Adjudicated by *value* per
build_discipline's "Unwired ≠ worthless": it yields **UNIQUE** products no *live* subsystem produces —
metric-enriched structural families via hierarchical agglomerative clustering (the live `orbit_report.pl`
produces only type-only orbit families by exact signature match), plus integrated cross-domain structural
twins. Unique-but-unwired = *unfinished value* ⇒ ruling is **revive + validate**.

**Governing invariant.** Commentary-grade, never correction-grade. Enabling the subsystem may ADD
`outputs/context_profile_report.md` and nothing else — it must not change `dr_type`, purity, signature
verdicts, `verdict_join`, or any classification field in `pipeline_output.json`. The primary witness is
structural (C0 layers 1+2: encapsulation read-scan + write-scan, gathered this session); the corpus diff
is corroboration over tested inputs only. Nothing downstream consumes `context_profile_report.md` — the
nothing-reads-it property is part of the invariant, not a gap.

**Why `bundled_with OQ-91`, not `blocked_on OQ-91`.** OQ-91 is the sibling dormant observer-trajectory
thread (`transition_paths`/`snapshot_type` repair). The original plan speculated a `blocked_on OQ-91`
impute-site edge, but the C-prov static trace (this session) shows the four `trajectory_distance`
components do **not** consume OQ-91's impute sites (`snapshot_type`, `classify_at_time`,
`constraint_history`, `degradation_chain`): of 9 leaf-source modules, 7 are fully clean and the 2 hits
(`drl_core.pl:306`, `boltzmann_compliance.pl:510`) are passive `nb_getval` fallback reads of the
`classify_at_time_*` globals, which `trajectory_run` never sets. The dependency is not live; the threads
are siblings, not gated.

**Resolution criteria (gate controls).**
- **C-prov** — no imputed/fabricated value reaches the clustering. Primary witness is corpus-independent
  (the call-graph fact: nothing in `trajectory_run`'s tree calls `classify_at_time`); per-corpus
  confirmation is the runtime `classify_at_time_*` global-unset check, run on BOTH `testsets/` and
  `kernel_v1`, with a positive control showing a global *does* set when `classify_at_time/4` is called.
- **C0** — commentary-only. Structural primary (C0 layers 1+2) + corroborating overlay-0-vs-1
  `pipeline_output.json` diff with zero deltas in every classification field (exit 0 + mtime advanced),
  plus an adjacent-field positive control.
- **C1** — HAC families non-degenerate (neither all-singletons nor one giant cluster); cut-height
  escalation pre-registered, re-tuning surfaced not auto-absorbed.
- **C2** — cross-domain twin two-sided control, positive case identified by an *independent* signal
  (anti-circularity); absence-as-finding if no real pair exists; synthetic pair = mechanism-test only.
- **C3** — determinism + permutation-stable partition (ship-blocking; HAC tie-breaking can flip the
  partition under benign reordering).
- **C-null** — scope-setter (not flip-blocker): frozen protocol (mean silhouette from the precomputed
  distance matrix; per-component-independent shuffle; N=200; real mean `s` must exceed the 95th
  percentile of the null). Pass ⇒ close as "validated meaning-bearing product"; fail ⇒ "validated = safe
  + stable, NOT semantically verified; family meaning OPEN," with the shuffle test named as what closes it.

**Evidence dir:** `audits/2026-06-25_oq182_trajectory_revive/`. Cross-domain-twin fork verdict (Step 2):
`context_profile_mining:cross_domain_twins/3` is canonical; `isomorphism_engine.pl` is a
loaded-but-non-executing Pattern-2 fork (all 4 call sites dead) — log-only, deletion deferred as its own item.

**Progress (2026-06-25, testsets/ leg; operator chose testsets-first with a re-checkpoint before kernel_v1).**
- **Cheap tier DONE + committed** (`fc4cadbf`): C-prov PASS (`c_prov_runtime.log`), fork-log/GAP-20,
  kernel_v1 denominator = 1,106 (`kernel_v1_denominator.log`), frozen C-null protocol
  (`c_null_protocol_FROZEN.md`).
- **C2 domain ruling (operator, `c2_domain_finding.md`).** `cross_domain_twins/3`'s "domain" is a
  name-prefix heuristic (`constraint_domain/2` = id before first `_`; 86/104 distinct), NOT authored
  `topic_domain`. Ruling: KEEP name-prefix (do not edit mid-validation); **C2 = mechanism-test only**,
  **C2-value OPEN** (closer = rebuilt corpus with a real authored domain field). **Families and twins
  are DISTINCT products:** the 448 twins over a near-vacuous gate make the **TWIN product OPEN**
  (deferred to rebuild).
- **Family STABILITY witnessed — but NOT family MEANING (committed `e4eb7646`/`9833847e`).** C1 PASS
  (11 families, sizes [1,2,3,4,4,8,8,9,13,21,24], non-degenerate) and C3 PASS (identity byte-identical
  across processes + permutation invariant under reversed input, positive-controlled) establish that the
  partition is a **stable, well-defined, reproducible function of the data** — they do NOT establish it
  **tracks signal**. A fixed-seed clustering of pure noise clears C1+C3 (non-degenerate sizes,
  byte-identical, reorder-invariant). C1 is a RAW size distribution (`NFam>=2, MaxSz<NClustered`), not a
  null-relative non-degeneracy test. **The gate that separates "stable function of the data" from "real
  structure in the data" is C-null, which is UNRUN.** So the family-side scope is SYMMETRIC with the
  twin-side: do not let C1+C3 green read as "family product validated" over an unrun meaning-gate, exactly
  as the twin-vacuity refusal forbids reading 448 twins as "twin validated" over a vacuous gate. Banked =
  family **stability**; family **meaning** is OPEN pending C-null.
- **C-null PASS — family MEANING now validated (testsets/ leg; `c_null_harness.pl`, `c_null_results.log`,
  `c_null_distribution.json`).** Standalone control-first Prolog harness, no engine edits
  (`trajectory_enabled` stays 0; `git status` shows only the audit dir). **RealSil = 0.161119** (97
  clustered constraints, 11 families) vs **P95(null) = −0.026436** over **200 per-component draws**
  (0 degenerate). All four positive controls pasted BEFORE the verdict and gating it: INTERNAL-CHECK
  (Σ w_k·comp_k == engine `pair_dist`, max-diff 0.0), GROUPING-FIDELITY (`make_groups@identity` ==
  engine `group_by_shift`, 26 groups), FIDELITY (`P0 == RealPartition`, |S0−RealSil|=0, 11 families),
  JOINT-TOOTHLESS (S_joint = RealSil to 1e-16, relabel-match=yes — the false-PASS demonstrated),
  TIE-BREAK (overlay regime σ-pure). **TEETH PASS** (null_median −0.0945 < RealSil; standardized gap
  **+5.01σ**); **0/200 null draws reach RealSil** — real lies beyond the *entire* null. Null family-count
  centers at **15 (real 11)** — the frozen doc's predicted false-FAIL-leaning direction — and real still
  clears, so the PASS is conservative. Reproducible under pinned seed `20260625` (run-twice → identical
  P95; SWI 9.2.9; Python percentile cross-check matches every statistic). **⇒ OQ-182 family product
  VALIDATED as meaning-bearing.** Twin product remains OPEN (parallel report: 448 twins / 4656 pairs,
  gate near-vacuous) — unchanged by this leg.
- **MECHANISM CORRECTION (flagged; frozen *quantities* untouched).** The frozen "Chimera surgery map"
  was mechanically wrong: `group_by_shift/2` recomputes the shift pre-grouping via
  `logical_fingerprint:fingerprint_shift/2` from the **constraint identity**, ignoring `trajectory_cached`
  entirely — so a chimera `trajectory_cached` + `run_hierarchical_clustering/1` pins shift grouping to the
  *real* boundaries regardless of σ_shift (toothless / false-PASS; breaks the joint control). The harness
  instead builds shift-groups itself (`make_groups/4`, keyed on `fingerprint_shift(C[σ_shift(i)])`) and
  reuses only `cluster_all_groups/2` + `assign_families/1`. The per-component shuffle is a pure index
  recombination over precomputed real component matrices — no chimera trajectory needed. Erratum recorded
  in `c_null_protocol_FROZEN.md` (statistic / null model / N / threshold unchanged).
- **NEXT (gated, in order):** (1) ~~**C-null**~~ **DONE (PASS, 2026-06-25)** — family meaning validated;
  the family **meaning** gate is closed (control-first harness; per-component shuffle destroys, joint
  shuffle demonstrated toothless, real beyond the entire null; see the C-null PASS bullet above).
  (2) ~~**C0**~~ **DONE (PASS, 2026-06-26)** — flag 0→1 changes only `config.trajectory_enabled`
  in `pipeline_output.json`; all classification fields byte-identical, positive-controlled
  (`audits/2026-06-25_oq182_trajectory_revive/c0_finding.md`). (3) ~~**C-gen**~~ **DONE — FAILED at
  the locked bar, then characterized (2026-06-26).** haiku↔flash family-partition ARI=0.117 (< 0.50
  fail bar). Operator ruled option-2 (re-specify, no laundering): a freshly pre-registered,
  granularity-insensitive **substrate read** gives TRACK=162/162=1.000 — every inter-leg family split
  is backed by a real per-reading `fingerprint_shift` difference, zero cut-height artifact. **Dual
  finding (both stand):** global partition does NOT recover across generation (ARI fail) AND that
  failure is **generation-EXPRESSIVE, not clustering noise** (locally stable PRES=0.83 descriptive-only;
  globally expressive — consistent with the draw-stable/draw-expressive posture). Evidence:
  `c_gen_finding.md`, `c_gen_successor_PREREGISTRATION.md`, `c_gen_successor_finding.md`. (4) **kernel_v1
  re-checkpoint:** C-prov **PASS** (1106; classify_at_time globals unset post trajectory_run,
  positive-controlled, `c_prov_kernel_v1_finding.md`); **C-null DEFERRED** (cost; cannot unblock a flip
  the C-gen result already gates).
  (5) **A4 — CLOSED by operator ruling (2026-06-26): close the family product as a scoped
  finding, keep it DORMANT; do NOT flip.**

**CLOSE (operator ruling, 2026-06-26).** The family product is **validated** — meaning-bearing
(C-null +5.01σ, single corpus) and **locally generation-invariant** (C-gen substrate
TRACK=162/162=1.000: every same-kernel reading-pair that should co-track does, across the
haiku/flash boundary). Its global cross-generation partition is **generation-EXPRESSIVE, not
noise** (ARI 0.117 / TRACK 1.000 — the ARI measures the thing that is legitimately expressive).
**Validity and shippability are orthogonal, and only validity is witnessed-positive.** The flip
is **blocked by a witnessed-NEGATIVE freshness criterion, not by doubt:** A4 requires "the stage
runs and the report regenerates each pipeline run" (Pattern 1) — but the one flag=1 pipeline run
that tested it **stalled** (>10 min, likely `giant_comp` under the added parallel-clustering
pressure; a second run completed in 12.6s, so it is intermittent). A stage seen to hang cannot be
tagged freshness-witnessed this turn; flipping would ship a pipeline that intermittently hangs and
claim a witness that actually failed. So the product is kept **dormant** (`trajectory_enabled`
stays 0).
- **Two preconditions for any future flip (gated, in order):** (a) a `giant_comp` /
  trajectory-stage **serialization or isolation fix** so a flag=1 pipeline completes deterministically,
  with a green freshness run as the witness; (b) optional — a kernel_v1 **C-null breadth leg**
  appended for the record (does NOT gate the close). **Attempted 2026-06-26 — did NOT complete:**
  the frozen testsets/-leg harness (N≈97) ran setup (611k pairs, 53 shift groups) but produced no
  null distribution on the 1106-story corpus; adapting/sampling the harness for breadth is a separate
  optional task (`c_null_kernel_v1_addendum.md`).

**FLIP (2026-06-27) — precondition (a) SATISFIED; `trajectory_enabled` 0→1.** Root cause confirmed:
the stall was **concurrency memory pressure from the trajectory×giant_comp overlap** (both O(N²),
memory-heavy, co-resident in the 4-worker Phase-2 pool), NOT a giant_comp bug (OQ-77 established
giant_comp is serially fine at 87× the corpus). **Fix (surgical, Python-only):** `run_pipeline.py`
`_phase_prolog` pulls `trajectory` out of the parallel `tasks` list and runs it **sequentially after**
`_run_parallel` returns (the `with ThreadPoolExecutor` has joined giant_comp before returning).
Order is correctness-irrelevant (trajectory's only output `context_profile_report.md` has no
downstream consumer — the C0 invariant). **Witnessed (`audits/2026-06-27_oq182_trajectory_serialization/`):**
(1) **mechanism witness** (the licensing witness, not the run count) — a ~0.1s `ps`/RSS sampler over
flag=1 pipelines: PRE-FIX arm captures giant_comp's swipl window **co-resident** inside trajectory's
(0.64s overlap, deterministic on run 1 — the positive control proving the battery scale exercises the
failure path); CURED arm shows trajectory's swipl starts **0.79s after** giant_comp's swipl exits
(disjoint, zero RSS co-residency — the direct subprocess-level proof). (2) **N=10 liveness battery
10/10 GREEN** (`battery_n10.log`): exit 0, every Phase-2 stage ok, giant_comp 0.6–0.8s (serial band),
`context_profile_report.md` regenerated (mtime advanced + size>0 — the literal A4 criterion). (3)
**freshness-detector positive control PASS** (real seeded stale report flagged; non-vacuous). (4)
**C0 re-witness PASS** — zero classification-field diff flag=0 vs flag=1, planted-field positive
control caught. Trajectory measured alive-window 1.5s at n≈104 ⇒ the held 300s `run_prolog` timeout
is a ≥175× margin (NOT bumped to 900). `validate_config` PASS at flag=1, `trajectory_weights_sum`
gate active+satisfied (0.35+0.25+0.25+0.15=1.0). Revert path (revert-on-red) unused.
- **Twin product** remains OPEN (448 twins over a near-vacuous name-prefix gate; deferred to a rebuild
  with a real authored `topic_domain`). Evidence: `audits/2026-06-25_oq182_trajectory_revive/`
  (`c0_finding.md`, `c_gen_finding.md`, `c_gen_successor_finding.md`, `c_prov_kernel_v1_finding.md`).

---

## OQ-183 — `metric_trend/3`: cross-module name collision + serialized-verdict net-change-vs-trend seat

**Ω-type:** Ω_C (declared-seat — the "escalating" semantics is the operator's ruling, not a correctness fact).

**Status:** open
**Priority:** 2
**Origin:** OQ-18 close (sequenced out 2026-06-25); faithfulness audit of the temporal predicates.  
**File:** `prolog/metric_drift_events.pl:86-93`; `prolog/logical_fingerprint.pl:329`; gate at `prolog/json_report.pl:570`.
**Deps:** splits_from OQ-18, bundled_with OQ-19 (the ±0.05 magic-number adjacency this seat sits on)

**Specific question (two limbs on the same predicate):**
**(i) Name collision (Build Discipline Pattern 2).** `metric_drift_events:metric_trend/3` returns `{increasing, decreasing, stable}` (no `unknown`); `logical_fingerprint:metric_trend/3` returns `{rising, falling, stable, unknown}` — same predicate name, different output vocabulary, different modules. Cross-module clash with no queryable fact saying which vocabulary a caller expects.
**(ii) Semantic seat (operator's ruling).** `metric_drift_events:metric_trend/3` measures *net change* (`V_last − V_first` bucketed at ±0.05), which gates the serialized `cs_verdict(scaffold_suppression_escalating)`. Net change *is* correctly the endpoints, but it is NOT *sustained* trend: a spike-and-recede series with net Δ>0.05 reads `increasing` while a least-squares fit is flat. Should "escalating" mean *net-higher* (endpoints correct) or *sustained-trend* (LSQ more faithful)?

**Evidence so far (witnessed 2026-06-25, probe `audits/2026-06-25_oq18_temporal_reduction/oq18_metric_trend_flip.pl`, live flip control):** net-change and regression-trend diverge for **0 / 1 / 17** serialized `scaffold_suppression_escalating` verdicts on testsets/haiku/flash (haiku: `nicene_creed` Δ=0.08 vs fit 0.0207; most flash cases hairline at the ±0.05 cut → OQ-19 adjacency). Positive-control lesson (mandatory for any re-run): bind `C` from `corpus_loader:corpus_constraint/1` BEFORE `cs_verdict(C, scaffold_suppression_escalating)` — an unbound query returns a false 0 (`dr_type/3` cannot generate `C`).

**What resolution changes:** Limb (i) is a correctness/clarity fix (rename one of the two `metric_trend/3` or namespace the vocabulary). Limb (ii) is a declared seat: ruling *net-higher* keeps current behavior (annotation already in place); ruling *sustained-trend* makes OQ-184's least-squares slope the gate source for `scaffold_suppression_escalating` too (output-changing — fold into OQ-184's recalibration). Neither is a correctness bug today (0 flips reach a wrong verdict on testsets; the haiku/flash divergences are at the semantic boundary, not errors).

---

## OQ-184 — Faithful least-squares `drift_velocity` replacement (+ series-acceleration rebuild), with sum-level kill-condition tripwire

**Ω-type:** Ω_E (empirically/computationally resolvable — a faithful full-series velocity is a measurement question, not a seat).

**Status:** open
**Priority:** 2
**Origin:** OQ-18 close (the deferred output-changing fix; 2026-06-25).  
**File:** `prolog/metric_drift_events.pl` (`drift_velocity/3`); `prolog/network_dynamics.pl:126` (`network_drift_velocity/4`); `prolog/cs_drift_mismatch.pl:92-97` (`cs_is_metric_stable/1`).
**Deps:** splits_from OQ-18

**Specific question:** Replace the endpoint `drift_velocity/3` (`metric_delta`-based, first/last only) with a least-squares slope over the full series (the engine already has the slope primitive `drl_composition:linear_slope/2`). The deleted `drift_acceleration/3` faithful rebuild (full-series, `design_gaps.md` declared absence) folds in here.

**Why deferred / output-changing (witnessed):** the swap changes which constraints flag `"cs_drift_mismatch"` in `pipeline_output.json` (gate source changes) and shifts `cascade_prediction` timings — so it needs its own commit + manual approval and recalibration. **Migration list:** `cascade_prediction` + `metric_drift_report` prints (render), **and `cs_drift_mismatch:96` `cs_is_metric_stable` (machine-consumed).** Before the swap, **snapshot the current serialized `cs_drift_mismatch` set as the diff baseline** so the output-change is witnessed as a specific membership delta, not "numbers shifted."

**Kill condition (carry as a CHECKABLE tripwire, not prose):** a recurring assertion that, per endpoint-serialized `cs_drift_mismatch` verdict, recomputes the **faithful `network_drift_velocity` SUM** (`sum_list` over `Rate>0` contributors with `linear_slope` rates — mirror the gate's exact conjunct 2, `sum >= network_drift_velocity_threshold`, NOT a per-series `Vf` proxy) and fails on any crossing. **As of 2026-06-25 the witnessed headroom is sum-level `Thresh − max faithful sum = 0.01 − 0.007851 = 0.00215` on `testsets_haiku`** (`oq18_flipped_probe.pl`; `faithful_ndv` does `sum_list`). The thin margin means the tripwire catches BOTH likely failure modes: a new high-`Vf` series AND an existing verdict gaining a contributor as the corpus grows. Prototype + witness: `audits/2026-06-25_oq18_temporal_reduction/` (`oq18_flipped_probe.pl` + `flipped_probe.out`).

**What resolution changes:** lands the faithful velocity, recalibrates the `cs_drift_mismatch` membership and cascade timings against the snapshot baseline, retires the OQ-18 falsifier (the endpoint reduction stops being a latent corruption source), and wires the sum-level tripwire into the recurring gate so a future corpus crossing the 0.00215 headroom fails loud instead of silently emitting a wrong verdict.

---

## OQ-185 — Generation does not honor the scaffold "suppression declines over time" expectation (5–6:1 rising:falling, all three legs)

**Ω-type:** Ω_C (design choice — which generation-fidelity remedy: make the rule explicit, gate at authoring, accept-and-annotate, or drop the expectation).

**Status:** open
**Priority:** 2
**Origin:** OQ-39 row-14 close (2026-06-25). The engine-side disposition (a commentary verdict that annotates the rule-break post-hoc) surfaced the generation-side question OQ-39 did not own. KNOWN_STATE 2026-06-25.
**File:** `agent/uke_write_v2.1.md:173` (scaffold prompt definition — qualitative only); generation step `agent/c-orchestrator.py` `_step_generate`; post-hoc detector `prolog/cs_pattern_detection.pl` `cs_verdict(C, scaffold_suppression_escalating)` + control `prolog/tests/test_oq39_scaffold_escalation.pl`.
**Deps:** splits_from OQ-39

**Specific question.** Scaffold's analytical signature includes *suppression declines over time* (the "temporary framework / sunset" reading — census G4 row 14). Generation produces the opposite at scale: among scaffold-certified constraints with an authored `suppression_requirement` series, **rising beats falling ≈ 5–6:1 in every leg** (testsets/ 13:2, testsets_haiku 53:7, testsets_flash 43:9 @ institutional context). The two reconciled twins share one generation prompt, so this is not one model's idiosyncrasy. **Crux (witnessed):** the prompt never actually asks for a declining suppression series — `uke_write_v2.1.md:173` defines Scaffold only qualitatively ("transitional arrangement," "temporary framework"), with no `suppression_requirement`-series instruction anywhere in the prompt. So the "rule" is an analytical inference from the *type definition*, not an authored generation instruction generation could honor. The decision: which remedy, given the engine already annotates the gap (OQ-39)?

- **(a) Make it explicit** — add a series-level instruction to the scaffold generation guidance ("a scaffold's `suppression_requirement` series should trend down toward sunset"). Tests on `testsets/` per the test-bed posture; risks over-fitting generation to the engine's expectation.
- **(b) Gate at authoring** — a linter/validator check that flags a rising-suppression scaffold at generation time (operator-eval surface, like the row-18 perspective checks), not an engine enforcer.
- **(c) Accept-and-annotate (status quo)** — OQ-39's `scaffold_suppression_escalating` verdict already records the break post-hoc; do nothing at the generation surface and read the verdict as a fidelity signal.
- **(d) Drop the expectation** — rule that a scaffold may legitimately *tighten* before it sunsets (rising suppression is not anti-scaffold), retiring census row 14 as a non-rule. If so, the OQ-39 verdict becomes a neutral descriptor, not a rule-break flag.

**Evidence so far (witnessed 2026-06-25).** The cross-leg counts above (OQ-39 implementation; cross-checked against an independent inline probe — same 14 firings on `testsets/`). The prompt-locus negative (no series instruction) is a read-witness, not a memory: `grep suppression_requirement agent/uke_write_v2.1.md` is empty; the scaffold row is qualitative-only. Positive-control lesson carried from OQ-39/OQ-183: bind `C` from `corpus_loader:corpus_constraint/1` before any `cs_verdict(C, scaffold_suppression_escalating)` re-census (an unbound query returns a false 0).

**What resolution changes.** Picks the generation-fidelity remedy and either (a/b) tightens the generation surface so future corpora honor the rule, (c) ratifies post-hoc annotation as sufficient, or (d) reclassifies the finding as a non-violation — which would re-grade OQ-39's verdict from rule-break to descriptor. Not a live correctness bug (the engine annotates, does not mis-classify); a generation-quality + rule-realism decision best ruled before the next bulk rebuild, since (a)/(b) only bind corpora generated after the change.

---

## OQ-186 — Contamination-network convergence does not distinguish independent constraints from common-cause re-descriptions: a clique of co-authored slices reads as mutual corroboration

**Ω-type:** Ω_C (design choice — what an independence/common-cause caveat on convergence should be, and where it lands). The empirical leg (does the machinery in fact ignore node independence?) is Ω_E, witnessable by the positive control below.

**Status:** open
**Priority:** 2
**Origin:** essay-review triage 2026-06-26 (gray_divorce draft, comment #5). Sibling of the contamination-edge work OQ-103 (salience floor) / OQ-174 (cs-edge shared-input) — both addressed edge STRENGTH and PROVENANCE; neither addresses node INDEPENDENCE.
**File:** `prolog/drl_purity_network.pl` (`constraint_neighbors/3`, contamination cascade); `prolog/json_report.pl` (`write_contamination_network`); read site `python/tensions_ledger.py` / `python/enhanced_report.py`.

**Specific question.** When several constraints in one topic are re-descriptions of a single underlying fact (the reviewer's instance: "gray divorce costs the lower-earning spouse," sliced into ~four extractive constraints — third-act, gendered-outcome, etc.), the contamination network builds edges among them (`shared_beneficiary`/`shared_victim`/`inferred_coupling`) and the resulting clique reads as convergence — i.e. as mutual corroboration. But N descriptions of one common cause agreeing is NOT N independent witnesses. The machinery has no notion of node independence: it cannot tell a clique of co-authored slices from a clique of genuinely distinct constraints that happen to point the same way. Decision: should convergence carry a common-cause / independence caveat to the read site (cf. OQ-108's witness-coverage pattern — carry the provenance bit, do not collapse), and if so what is the discriminator?

**Evidence so far (review-raised, UNWITNESSED on substrate).** The gray_divorce instance is the reviewer's read, not yet a run. **Graduation / positive control (riskiest-shape test):** construct two synthetic topics — (A) N constraints that are deliberate slices of one authored fact (shared victim+beneficiary, near-duplicate ε), (B) N constraints with genuinely distinct victims/beneficiaries that nonetheless type extractive — run both through `constraint_neighbors/3` and confirm the network/convergence read treats A and B identically (the defect) vs differently (no defect). Only the run licenses "the machinery ignores independence." Cross-refs: OQ-103, OQ-174, OQ-101 (synthesis-read enforcement).

**What resolution changes.** Either (a) the read site gains an independence/common-cause caveat so a co-authored clique stops reading as corroboration, (b) the discriminator is ruled un-computable from authored fields and the caveat becomes a synthesis-checklist item (OQ-101 family), or (c) the run shows the machinery already separates A from B and the concern is essay-side only. Bears directly on any essay that cites contamination convergence as evidence.

**Witnessed instance (2026-06-26).** The `moral_causation_locus` reports' `convergent_institutional` reasoning (`outputs/constraint_reports/dispositional_reading_report.md:396,428`) rules a coordinated-cartel reading on the grounds that all readings "share the same institutional observer type [rope] … indicating coordinated rather than independent operation." But that uniform institutional=rope is a generic consequence of low-d institutional seats sitting below the f(d) root (**OQ-188**), not coordination — a concrete case of convergence-as-config-artifact, distinct from the shared-agent-edge route. Two independent artifact channels now feed the false-cartel read.

---

## OQ-187 — A directional extraction verdict (RED, extraction-masked-as-rope) may headline a contestable moral direction as seat-free: is verdict directionality surfaced as a declared, seat-indexed premise?

**Ω-type:** Ω_C (a reporting surface, like OQ-108). The Ω_P core (moral direction of a RED — trap vs cost-of-exit) is real but, per the resolution, is NOT per-constraint discriminable; the engine declares non-adjudication at type level rather than abstaining case-by-case.

**Status:** resolved — standing read-site legend note (thesis downgraded surface-not-abstain); render enrichment split to OQ-189.
**Priority:** 3
**Origin:** essay-review triage 2026-06-26 (gray_divorce draft, comment #4). Resolved 2026-06-27 (conversation; witnesses below).
**Deps:** bundled_with OQ-78 (Var_fd-dominance is OQ-78), bundled_with OQ-188 (authored-d fragility), bundled_with OQ-186 (convergence-as-artifact), splits_from OQ-189 (render enrichment OPEN)
**File:** read site `python/enhanced_report.py` (`build_verdict_banner` / `_red_direction_caveat`); verdict join `prolog/diagnostic_summary.pl` (`verdict_join/3`); deferral gate `prolog/signature_detection.pl:851` (`has_metric_perspectival_variance`).

**The one-output check, answered: UNMARKED.** The serialized verdict surface for `womens_financial_autonomy` headlines RED seat-free — `verdict_join.verdict=red`, the moral direction carried only by the `false_ci_rope` signature label ("hides extraction", confidence high). Witnesses: `verdict_join` red with `measurement_provenance.projected=0` (so the OQ-102 `basis=projected` caveat the filing hypothesized does NOT apply — direction is `authored`, the most authoritative basis, not projected); the authored Ω_P `coordination_vs_destabilization_framing` (type `preference`, confidence low) appears **0 times** in `pipeline_output.json` (serialized `omegas` field is a disjoint engine-detected `conceptual`-only population — positive control: 50/109 populated, all `conceptual`).

**No per-constraint discriminator exists — all three candidate triggers are dead** (witnessed base rate, live 109-corpus): (1) **honor-the-deferral** — `fcr_deferred_signature_mismatch` gates on `has_metric_perspectival_variance` = ≥2 distinct *types* across power seats (Axis 1); for this constraint it fires because analytical=`unknown`, NOT a direction split. (2) **Var_fd-dominance** — `dominant==directionality` in **97/101 (96%)** of constraints and **16/17 (94%)** of REDs; this is OQ-78 restated (f(d) structurally dominates χ-variance), so a caveat keyed on it is an always-on disclaimer = noise. (3) **authored preference-omega** — fires only **2/17** REDs (`llm_synthesis_capacity`, `womens_financial_autonomy`), blind on the 15/17 author-forgot cases (where self-certification does the damage), and serializes nowhere (would need new producer plumbing, not a join).

**Resolution (ruling 2026-06-27).** Thesis downgraded from "abstain on direction" to "**surface that direction is authored-not-adjudicated**" — OQ-108's precedent is **cosmetic** (`verdict_join/3` reads its Cap/Alerts from `join_alerts`+`maxent_void_alerts` only; `perspective_witness` gates nothing), so "abstain" exceeded the cited precedent. Because direction-dependence is universal (OQ-78) and d is always authored (victim/beneficiary/ε), the marker is **standing, not per-constraint**: a one-line type-level legend note on every RED — *"RED extraction = a statement about AUTHORED directionality d, not a seat-free moral verdict; the engine does not adjudicate the contested direction (trap vs cost-of-exit)."* This DEFINES RED so the over-read is impossible, discharging the path-4 essay-warning at the legend instead of as standing advisory. **No verdict-state change** (no substrate signal supports per-constraint RED→deferred; applying to all REDs would neuter RED). Witness: `_red_direction_caveat` in `enhanced_report.py`; two-sided positive control run 2026-06-27 — note present on RED (`womens_financial_autonomy`), absent on YELLOW/GREEN. Cross-refs: OQ-102, OQ-108, OQ-56, OQ-78, OQ-128 (routing-sink), OQ-186, OQ-188, OQ-189.

**Forward-note for the OQ-78/186/188 family (reusable finding from the OQ-189 read).** If a future rebuild ever makes direction-contests common enough to warrant a per-constraint direction-contestability surface, the trigger must key on **omega-direction-typing, not preference-typing**. The OQ-189 gate-(a) experiment showed those axes are orthogonal: the one corpus constraint with a genuine direction-contest (`llm_synthesis_capacity` / `synthesis_legitimacy_boundary`) had it typed `conceptual` and invisible to a `preference`-type filter, while the `preference`-omega that fired was off-topic. Selecting by omega-type does not select for direction-contestability — build the trigger on direction.

## OQ-188 — Low-d institutional seats sit just below the f(d) sign-change root, so the OQ-01 rope-gate bypass that makes them "see rope" is knife-edge on an authored stakeholder-role choice

**Ω-type:** Ω_C (read-site surface — whether root-proximity is flagged where a classification is read) over a witnessed Ω_E core (the straddle below is computed, not argued).

**Status:** open
**Priority:** 2
**Origin:** essay-review triage 2026-06-26 (`moral_causation_locus` twin; the reviewer's "softest load-bearing spot"). The graduation question raised at the OQ-187 filing (canonical-config vs story-authored `d`) is settled empirically here.
**File:** `prolog/config.pl` (`canonical_d_institutional` :144, `stakeholder_role_d_*` :156–160, sigmoid params :130–133); χ path `prolog/drl_core.pl` / `prolog/drl_composition.pl`; rope-gate bypass (OQ-01); read sites `python/enhanced_report.py` / `python/tensions_ledger.py`.

**The witnessed mechanism.** f(d) (sigmoid L=−0.20, U=1.50, d0=0.50, k=6.00) changes sign at **d=0.1642**. In `outputs/constraint_reports/dispositional_reading_report.md:80,243` the institutional seat sits at **d=0.120** — which is `stakeholder_role_d_agenda_setter` (a config constant **selected by an authored role**, NOT the canonical institutional fallback 0.00) — giving f=−0.042 → χ = 0.68 × −0.042 = **−0.029**. χ ≤ 0 trips the rope-gate bypass (OQ-01); that bypass IS "institution sees rope." The two commonest institutional roles **straddle the root**: agenda_setter d=0.12 → f=−0.042 (rope) vs beneficiary d=0.25 → f=+0.110 → χ=+0.075 (**not** rope). A SINGLE authored role change crosses the root and flips the institutional verdict. The report's own decomposition confirms f(d) is **101.2%** of cross-seat χ variance (ε constant 0.68 contributes ~0) — which is *why* root-proximity is load-bearing. Witness: config greps + `python3` sigmoid root/straddle computation (conversation 2026-06-26); report lines cited.

**Scope (graduation question answered): SYSTEMIC, with a story-level trigger.** The role→d mapping is a config constant, so ANY story authoring an institutional `agenda_setter` lands at d=0.12, generically ~0.044 below the root — institutional-seat rope verdicts are knife-edge corpus-wide, not a one-twin fluke. The trigger is which role got authored. Direction matters: moving toward canonical 0.00 makes it MORE robustly rope (f=−0.119); the flip requires moving UP past 0.164. The lever is the DISCRETE authored role, not a continuous "nudge the observer."

**Specific question / what resolution changes.** Should a read site flag when a seat's classification rests within ε of the f(d) root (so an authored-d-fragile verdict stops reading as solid), and/or should the agenda_setter/beneficiary straddle of the root carry a config note? Bears on every essay leaning on an institutional rope / extraction-blindness result, and partly explains the "uniform institutional=rope convergence" that reports read as coordination (OQ-186) — that uniformity is a generic consequence of this mechanism, not a cartel. Cross-refs: OQ-01 (the bypass), OQ-78 (f(d) dominates variance), OQ-123 (seat model-sensitivity), OQ-186 (convergence-as-artifact).

---

## OQ-189 — Per-report enrichment lifting the authored preference-omega beside a RED verdict: net-negative by default, gated on an undone render-read

**Ω-type:** Ω_C (read-site render surface). Splits from OQ-187 (the standing legend note shipped; this was the per-constraint enrichment OQ-187's ruling deferred).

**Status:** disposed — won't-build; the gating read (2026-06-27) collapsed the value while the standing note already covers the case.
**Priority:** 4
**Origin:** OQ-187 resolution 2026-06-27. The proposal: lift the authored `preference`-type direction-omega (e.g. `coordination_vs_destabilization_framing`) beside the verdict where it exists.
**Deps:** splits_from OQ-187
**File:** read site `python/enhanced_report.py`; authored omega source is the embedded-Prolog report path (authored omegas serialize nowhere in `pipeline_output.json`, OQ-187 witness).

**Disposition (witnessed read 2026-06-27): the trigger fires on the WRONG AXIS.** Gate (a) keys on `preference`-*typing*, which is orthogonal to direction-contestability — and the one case that could have vindicated it proves the orthogonality. `llm_synthesis_capacity` HAS a genuine direction-contest (`synthesis_legitimacy_boundary`: legitimate-knowledge vs speculative-extraction) but it is typed `conceptual`, so gate (a) is **blind to it**; the `preference`-omega that DOES fire there (`institutional_adaptation_pathway`) is off-topic — it contests a future institutional trajectory (adapt vs entrench), not the RED's extraction valence, failing the bar (`coordination_vs_destabilization_framing`: two named seats, opposed valence, same quantity). So on-topic direction-contest firing = **1/17** (`womens_financial_autonomy` alone), and that 1 is an accident of which omega the author happened to type `preference`, not the gate selecting *for* direction — the gate doesn't track the thing OQ-187 cared about. **Secondary** (would have disposed it anyway): even on that 1, the product is already delivered twice — the OQ-187 standing note (universal, no anti-signal) and the OMEGA RESOLUTION SCENARIO (same omega, in-report) — so the kill-condition label, though expressible in the report idiom, is moot. **Revival is TWO-clause, not one:** a future corpus must make on-topic direction-contests common AND the trigger must select them by **direction-typing, not omega-type** (the orthogonality this read exposed). A one-clause "if direction-omegas become common" revival would re-make the wrong-axis error. Cross-refs: OQ-187 (parent), OQ-108, OQ-101.

---

## OQ-190 — Blast-radius enumeration: which live claims rest on cast-field draw-stability (the OQ-118 corpus-grep that also prices the reserved verdict spend)

**Ω-type:** Ω_E (a corpus + codebase census — enumerable by query/grep/read; no theory ruling).

**Status:** open — filed 2026-06-27, split from OQ-118's blast-radius open item.
**Priority:** 2
**Deps:** splits_from OQ-118, gates OQ-75

**Origin:** OQ-118 (2026-06-27). The four-limb ruling named a blast radius "from memory, not from a
corpus sweep" — accepting OQ-118 Limbs 1–2 (σ/seat is not the stability partition; free-composed
cast fields are draw-UNSTABLE, 0/6) retroactively suspects any live claim that leans on cast-field
draw-stability. The enumeration was left as an open item; this OQ carries it. It is the unblocking
item for OQ-118's two remaining substantive limbs (broad-A's relevance, the reserved verdict spend).

**The question (two answers from one grep).** Sweep the live corpus + analysis code for claims,
report surfaces, and per-story mechanisms that depend on a field being draw-stable, bucketing each
by the field-construction-type it rests on:
1. **Cast-field dependents.** Anything keyed on the draw-stability of free-composed cast fields
   (stakeholder multisets, beneficiaries/victims, vindicated_propositions). OQ-118 named suspects
   to CONFIRM or CLEAR (not assume): the `reading_diff` re-point (cohort-one-gated), OQ-75
   cross-story claims, per-story roster/beneficiary/vindicated mechanisms. Each found dependent is
   SUSPECT — its stability premise is falsified by OQ-118 Limb 2 (cast = 0/6 draw-stable).
2. **Verdict-stability dependents (this prices the OQ-118 Limb-3 reserved spend).** Does any live
   claim need the verdict class (`disappearance_verdict`, `founding_problem_status`) ACTUALLY
   draw-stable, or only the temp-confound acknowledged? OQ-118 ruled the verdict class
   accept-as-confounded and held its ~$1.5–2 temp sweep in reserve; **a YES here un-reserves that
   spend** (it becomes load-bearing — cheaper before the dependent claim hardens than after). Same
   grep, two hats.

**What resolves it.** A census artifact (`audits/<date>_oq190_blast_radius/`) listing every
dependent found, tagged {cast-field | verdict | both} × {SUSPECT-confirmed | cleared}, each with a
per-finding disposition: re-witness the claim under the OQ-118 finding (does it survive cast/verdict
instability?) or mark it for repair. The verdict-bucket answer routes back to OQ-118 Limb 3 (spend
or stay-reserved).

**Discipline (the enumeration is itself a probe — it owes a positive control).** "I found no
dependents" is a fact about the search until the search is shown to find: grep a name you KNOW
depends on stability and confirm the sweep flags it BEFORE any "blast radius is empty/small" claim.
Fail-closed on absence — a not-yet-witnessed dependent stays SUSPECT, not cleared.

**Cross-refs:** OQ-118 (parent; the finding that creates the blast radius), OQ-75 (the cross-story
consumer this protects), OQ-109 (cohort lineage), re-probe witness
`audits/2026-06-27_oq118_reprobe/`.

## OQ-191 — Python toolset physical regrouping (deferred from OQ-163)

**Ω-type:** Ω_E (maintainability — directory layout; trigger-deferred).

**Status:** future — deferred from OQ-163 (2026-06-27); dormant, revival is trigger-gated.

**Priority:** 8

**Deps:** splits_from OQ-163.

**Origin.** OQ-163 shipped the discoverability half via `python/cli.py` (1-prime: logical command
groups, no file moves). The remaining half — physically moving ~73 top-level scripts into
subdirectories — is deferred here because **it buys nothing now and carries real risk:**

- **paths.py already drained the OQ-32 path-fragility class** (depth-agnostic, `pyproject.toml`-marker
  repo-root finder) — the original Ω_E justification for the move is dead at the root.
- **The CLI already delivers discoverability** (`python3 python/cli.py list`), so the move's only
  remaining payoff is already banked.
- Moving now would re-incur the OQ-32 risk class for ~73 files: 50+ `shared/` importers, 22
  `corpus_hash` importers, `run_pipeline.py`'s 20+ sibling imports, ~414 doc references, and a
  hardcoded `scripts/gate.sh`.

**Revival trigger (tightened — NOT "a directory-structure consumer appears").** A directory-structure
consumer *already* exists — `verify_reorg.py` globs `python/{tests,sweeps,audits}/*.py` — and is
**inert** under deferral (1-prime moves no paths). Revive only when a consumer appears that the move
would **break**, or when an independent reason to move the files (not discoverability, which is
solved) arises. Note: if the move ever runs, `verify_reorg.py`'s `DIRS` list must be extended to the
new subdirs. Probe = the OQ-163 kill-condition grep (one consumer found, sensitivity shown with an
independent synthetic control).

**Cross-refs:** OQ-163 (parent; discoverability half shipped), OQ-32 (the path-resolution regression
class, drained by `paths.py`).

---

## OQ-192 — Deciding-hub provenance field (carry the Hub-1/Hub-2 bit to the read site)

**Ω-type:** Ω_C (design choice — add a per-context provenance field vs leave the hub implicit).

**Status:** resolved — document-only (operator ruling, 2026-06-29). Option (b): no field built.
**Priority:** 3
**Deps:** splits_from OQ-22

**Resolution:** The OQ-22-witnessed distinction (Hub-2-decided vs genuine two-hub) is carried by
documentation, not a new engine field: OQ-22's resolution note + the corrected two-hub comment at
`drl_core.pl:205–211` ("a PURE Hub-2 type … does NOT check χ", cites `logic.md:644`). No current
consumer keys on the hub, and document-only is consistent with mark-drift-not-armor. The
discriminator stays available as a prototype (`python/audits/oq22_grid.py`) for any future audit.
**Reopen condition:** if a downstream consumer ever reads per-observer type variation AS two-hub
signal (a live mis-read at flash 100/960 ≈ 10%, kernel_v1 49/1106, testsets 5/109), revisit option
(a) — emit per-observer `deciding_hub ∈ {hub1_chi, hub2_immutability, both, neither}` as a
`perspective_chi` sibling, anchored to the audit's grid-confirmed per-leg counts.
**Evidence:** `audits/2026-06-28_oq22_hub_starvation/` (`grid_*.tsv`, `FINDINGS.md`). Declared-absence
twin: **GAP-22** (`docs/design/design_gaps.md`).

---

## OQ-195 — General-n H¹ gap spectrum under the OQ-51 variable-real-seat regime (the unwritten induction)

**Ω-type:** Ω_C (definitional completion — the general-n form of Theorem 2's gap, a proof obligation rather than a contested seat).

**Status:** resolved — 2026-07-02. Proven for every cardinality: `docs/h1_gap_spectrum_general_n.md`.
**Priority:** 3
**Origin:** OQ-27 resolution, 2026-06-30 (split out at W5 of the OQ-27 plan so OQ-27 could close clean on the signature-resolved *disclosure* without gating on an unproven general law).
**Deps:** splits_from OQ-27

**Resolution (2026-07-02; commits `5d052990` + close commit; evidence `audits/2026-07-02_oq195_general_n_gap/`).** The proof doc delivers: Lemma 1 (H¹ = C(n,2) − Σ C(nᵢ,2) = engine's pair count); **Theorem A** (min nonzero = n−1, so {1..n−2} forbidden at every n); **Theorem B** (exact band decomposition by largest agreement bloc, self-similar recursion); an unconditional band-floor lemma; **Theorem C** (inter-band gap iff n ≥ j+3+C(j+1,2), every value in the gap forbidden); **Theorem D** (type-token bound T=7, derived from code not assumed — top truncation live in the STAKEHOLDER frame, where authored seat counts reach 12; operator's pre-check reshaped the scope from caveat-closure to live law). Machine-verified n ≤ 40 under pre-registered BLOCKING criteria — **per-band architecture** (plan-review catch: the band union is invariant under dropping the parts-constraint, so a union check cannot verify the classification; the unconstrained classifier was run as a discriminating control: identical unions, mismatched bands at 38/39 n) — plus record-match, negative controls, Theorem-C iff (zero exceptions). Engine witness `prolog/tests/test_h1_spectrum.pl` 23/23 (exhaustive n=2–4; constructive all ≤7-bloc partitions n=5–12; OQ-51 filter at n=12; two negative controls). Propagation landed: v8 §3.4/§9.6/Appendix; v7 dated amendment note (band values seat-count-conditioned); v6.13.1 changelog item-6 pointer; `grothendieck_cohomology.pl` both range comments rewritten. **Line-drift note:** the flag everywhere cited as `grothendieck_cohomology.pl:158` actually lived at ll.167–182 — cite by predicate (`cohomological_obstruction/3` header), never that line. Successor build: **OQ-207** (stakeholder-frame H¹ wiring). The old 4-name band census (manifest `2026-06-30T00:08:22Z`) was evidence-so-far, superseded by the theorem; re-pin by manifest if ever reused.

---

## OQ-196 — Value-adjudicate the held-out static-orphan remainder (M=170)

**Ω-type:** Ω_C (design choice — each orphan is strip-as-cruft vs preserve-as-unfinished-value, a `design_gaps.md` GAP, not an empirical fact).

**Status:** open
**Priority:** 4
**Origin:** OQ-38 resolution, 2026-06-30 (split out at Stage 4 of the OQ-38 plan: Option 1 strips
only the four calibration orphans and routes the remainder, rather than mass-stripping a first-run
analyzer's full list — which would trust exactly the aggregate green-check OQ-38 exists to distrust).
**Deps:** splits_from OQ-38, blocked_on OQ-38
**Files:** `audits/2026-06-30_oq38_orphan_xref/` (tool funnel + orphan list); `prolog/orphan_xref.pl`
(re-run to refresh); `python/audits/oq38_orphan_sweep.py`; `docs/design/design_gaps.md` (GAP target
for unfinished-value orphans).

**Specific question:** OQ-38's tool reports **M = 170** static-orphan predicates (124 exported,
spread across `utils.pl` (17), `abductive_triggers.pl` (16), `metric_drift_events.pl` (9),
`narrative_ontology.pl` (9), …; `outputs/oq38_orphan_funnel.json`). This is an **upper bound**, not
a strip list — `[EDGE]` "statically uncalled" ≠ "dead" for anything reachable via a Python
goal-string or Prolog name-construction the static xref is blind to. Each of the 170 needs the
**value adjudication** (CLAUDE.md *Unwired ≠ worthless*): (1) what product does it yield? (2) does a
live subsystem already yield it → **duplicate** = strip; (3) else → **unfinished value** = wire it or
log a `design_gaps.md` GAP, never retire on wiring grounds. The asymmetry holds: retiring a
valuable-but-unwired predicate silently destroys a capability; keeping a duplicate is mild clutter —
when unsure, preserve and adjudicate.

**Known cascade seed (from OQ-38 Commit B):** the safe_get convenience wrappers
`safe_get_category/3` (newly orphaned by the strip), `safe_get_extractiveness/2`,
`safe_get_suppression/2`, `safe_get_metric/4` are in M — the now-dead tail of the
`safe_get_all_metrics/2` batch wrapper. Likely a clean duplicate-of-live-/N-arity strip, but
adjudicate per the rule, do not blind-strip (false-orphan discipline).

**What resolution changes:** converts the 170-entry upper bound into a partitioned ledger
(strip-list vs GAP-list), draining the last of the OQ-38 dead-code frontier. Re-run
`oq38_orphan_sweep.py` first — the corpus and engine drift, so M rots; re-pin before adjudicating.

---

## OQ-197 — Gap omegas read silently empty when stakeholder annotation is absent or insufficient (Pattern 6)

**Ω-type:** Ω_E (empirical — a witnessable substrate defect; the fix is a code change, verifiable by a re-run with an operability control).

**Status:** open
**Priority:** 2
**Origin:** detector_calibration (Slice-B) cross-corpus run, 2026-06-30 — surfaced while validating the
proposed omega's net-new firing against `extraction_blindness` across all legs
(`docs/design/detector_calibration_omega_proposal.md` §5/R4 finding + witness tables).
**Files:** `prolog/report_generator.pl` (`seat_type_reading/2`, `detect_gap_pattern/2`, `label_gap/4`,
`gap_coverage/1`); `docs/design/detector_calibration_omega_proposal.md` (R4 finding + cross-corpus witness).

**Specific defect:** `detect_gap_pattern/2` — the sole detector for the `extraction_blindness` and
`general_type_mismatch`/`omega_perspectival` gap omegas — reads its per-seat types from
`seat_type_reading/2`, which is keyed on authored `constraint_stakeholder/7` facts (via
`dr_type_for_stakeholder`), NOT on the canonical-seat `dr_type/3`. When stakeholder annotation is
**absent** (kernel_v1: 0 facts → 0/1106 gap omegas fire) or **present-but-insufficient** (the twins:
stakeholders don't span ≥2 power positions with ≥2 distinct computed types — 29/43 haiku + 41/53 flash
of the detector_calibration net-new set), the `Rs=[_,_|_]` / `Types=[_,_|_]` precondition fails and
`detect_gap_pattern` returns **nothing** — reading as "no cover-story / no gap" rather than "not
measurable here." Meanwhile `dr_type/3` DOES produce the structure at the canonical seats (944/1106
kernel_v1 constraints have ≥2 distinct cross-seat engine types; `isaac_covenant__boundary_maintenance_reading`
= `[naturalized, tangled_rope, rope, snare]`, a textbook extraction_blindness shape the omega never sees).
Build-Discipline Pattern 6: measured-empty and didn't-look collapse to one success-shaped token at the
gap-detection boundary.

**Why higher-priority than any single omega:** every gap-omega read on every corpus is currently
unfalsifiable between "checked, found nothing" and "the detector was inoperative here" unless
stakeholder-operability is verified per-constraint first. Any at-scale/legacy sweep that trusts
`extraction_blindness` counts (e.g. OQ-89-class breadth over kernel_v1) silently reads 0 cover-story where
the structure exists. It also corrupts downstream value estimates — it inflated detector_calibration's
apparent net-new ~3× (43/53 reported vs 14/12 genuine).

**Candidate fixes (adjudicate):** (a) carry operability provenance — `detect_gap_pattern` returns
`undetermined`/`insufficient_seats` rather than failing, so absence and didn't-look stop collapsing (the
OQ-51 `h1_band`-null pattern); and/or (b) source `seat_type_reading` from the stakeholder-independent
canonical-seat `dr_type/3` (which already produces the structure), deciding whether stakeholder-keying is
by-design or an unintended coupling. **Positive control for any fix — TWO cases, not one (the failure mode
has two flavors):** (i) kernel_v1's **total-absence** case — the 944 cross-seat-varying constraints each
flag or read `undetermined`, never silent 0; AND (ii) the twins' **present-but-insufficient** case — the
specific 29 haiku + 41 flash constraints (stakeholders present, 4–10, but not spanning ≥2 power positions
with ≥2 distinct types) must likewise flag or read `undetermined`, never silent 0. A fix that patches "zero
stakeholder facts" without patching "facts present but not the ones `detect_gap_pattern` needs" passes (i)
and silently fails (ii) — which would let this OQ resolve as fixed, R4 reopen on it, and the ~3× net-new
inflation reappear under a different flavor of insufficient annotation nobody tested against.

**Progress (2026-07-01, branch `oq197-three-valued-gap-operability`):** the three-valued CONTRACT landed
(commit `b616e625`), built source-agnostic per operator ruling so it survives whichever way (a)/(b) lands.
`report_generator:gap_status/2` returns `gap(...)` | `no_gap` | `undetermined(no_seats|single_seat|single_power_position)`;
`detect_gap_pattern/2` firing logic UNCHANGED (byte-identical firing set on testsets, 57=57) — the split is
additive. `gap_seat_source/1` (default `stakeholder`; `canonical` via `constraint_classification/3` already
written) is the one-line (a)/(b) seam feeding both firing and operability. `gap_coverage/1` lifted from the
≥1-seat proxy to the operability precondition (= case-(ii) fix at the `"gaps"` field: insufficient → null).
Witnessed: `gap_status` total/deterministic (119/119; dist gap=57 no_gap=32 **undetermined=30** — the
surfaced Pattern-6 population); `dataset_recycling_amplification → no_gap`; 9 two-sided plunit controls
(`prolog/tests/test_gap_operability.pl`) pass; 0 new corpus-suite failures.

**Consumer map — SIX sites, not five (blast radius, `Files:` for the wiring unit):** (1) `json_report.pl:343`
`"gaps"` field — now honest via lifted `gap_coverage` (null for undetermined); (2) `json_report.pl:1291`
`collect_omegas`, (3) `:1669` `constraints_with_gaps`, (4) `:1678` `omega_count` — all still **unguarded →
silent 0**, must consume `gap_status` and emit a companion undetermined/coverage count; (5)
`detector_calibration.pl:88` `covers_via_extraction_blindness` — unguarded; (6) **NEWLY FOUND —
`python/tensions_ledger.py:131`** computes its OWN index-mismatch from `perspectives`, NOT from the `gaps`
field, and has an independent Pattern-6-adjacent bug: `{v for v in persp.values() if v}` counts `unknown` as
a diverging value, so `dataset_recycling_amplification` reads "perspectives diverge (no gap pattern matched)"
purely because analytical is untyped (scaffold-vs-unknown, not a real type gap). The OQ-197 detector fix does
NOT touch this site; it needs its own repoint to `gap_status` (or at minimum an `unknown` filter). Python
null→[]/0 collapses also at `query.py:444`, `tensions_ledger.py:125`, `enhanced_report.py:336`.

**Remaining graduation steps (sequenced per operator ruling 2026-07-01):** (1) wire the 6 consumers to the
three-valued result + assembled-interface witness that `"gaps":null` emits for an undetermined constraint;
(2) run the two positive controls (kernel_v1 total-absence + twins present-but-insufficient) with their
negative controls; (3) the **h1_band cross-tab** — **DONE for testsets (2026-07-01, `audits/2026-07-01_oq197_source_h1_crosstab/`),
points toward (a):** on the both-sources-determinate subset (n=84) canonical (b) firing is EXACTLY coextensive
with `h1_band>0` (58/58 fire↔pos, 26/26 no_gap↔zero, zero off-diagonal — (b) is `h1_band`'s construction, a
redundant recomputation), while stakeholder (a) is DISTINCT from `h1_band` on 3/84 (authored-stakeholder
disagreement `h1_band` does not carry). Source-explicit `gap_status/3`/`detect_gap_pattern/3` landed to run it
(commit `6bda83ec`), which also caught+fixed a bug: the b616e625 canonical clause used
`constraint_classification/3` with an unbound context (mode `+Context`) → 0 seats; fixed to `dr_type/3` via
`standard_context_for_power/2`. **Twins extension DONE (2026-07-01), with a corpus-independence correction:**
on the both-determinate subsets (testsets 84 + haiku 452 + flash 661 = 1197) canonical (b) is coextensive with
`h1_band>0` at **0 off-diagonal, zero exceptions** (definitional — confirms the (b)-wiring, NOT new evidence);
stakeholder (a) distinct from `h1_band` on **36/1197** (3/19/14). Twin `h1` computed in Prolog
(`cohomological_obstruction/3`), positive-controlled vs pipeline `h1_band` on testsets (0/119) before use.
**NOT three independent corpora:** `testsets_haiku`+`testsets_flash` are TWINS (same seed, different backend →
correlated), so this is one independent corpus (testsets, 3 distinct) + one correlated pair (19+14), not triple
replication. Establishes **non-redundancy ONLY** — (a) is irreducible to `h1_band` as a construction; it does
NOT establish the divergences are (a)-correct rather than authoring noise (stakeholder facts are single-pass
authored input). Evidence + corrected framing: `audits/2026-07-01_oq197_source_h1_crosstab/`. **(3) COMPLETE.**

**(4) RULING — (a), keep the stakeholder source (operator, 2026-07-01).** The cross-tab settles the redundancy
question this OQ raised: (b) is a pure duplicate of `h1_band` (cruft); (a) is non-redundant. `gap_seat_source`
stays `stakeholder` — no code change, the default already implements (a); the `canonical` source + `gap_status/3`
stay as the tested (b) arm and the analysis seam. **Scope of the ruling — non-redundancy established, reliability
explicitly UNRESOLVED (NOT validation):** the ruling resolves *is the omega redundant* (no — (a)≠h1_band, 36/1197
distinct). It does NOT resolve *is authored-stakeholder disagreement real signal or annotation noise* — the 36
divergences could be (a) catching signal h1_band misses OR (a) surfacing noise h1_band correctly ignores, and the
cross-tab cannot tell which. **CONDITION ON REPORTING (binding, not someday-nice):** while OQ-199 is open, every
gap-omega firing sourced from (a) is reported as **"authored-stakeholder disagreement," NEVER as "validated
cover-story detection"** — the unresolved reliability question travels with each firing rather than being dropped
at this ruling boundary. OQ-199 is the condition that would lift that qualifier (an independent structural check,
not the stakeholder facts scoring themselves). **(1) consumer wiring DONE (2026-07-01, commit `fffca9d1`).** The 6-site consumer map resolved to **4 LIVE
sites** wired + 1 dormant + 1 already-fixed: json_report per-constraint `"gap_status"` + `"gap_undetermined_reason"`
label (+schema); corpus-level `constraints_gap_examined`/`constraints_gap_undetermined` companions;
`query.py --detail` (distinguishes UNDETERMINED, fixes a latent `len(None)` crash); `tensions_ledger.py`
dedicated gap-operability line. `collect_omegas` needs no change (undetermined mints no gap-omega, correct — the
new per-constraint `gap_status` surfaces it). **Site 5 `detector_calibration.pl:88` is NOT a live consumer** —
that module is UNTRACKED, UNWIRED WIP (Slice-B apparatus awaiting its proposal ruling), so no live silent-0
there; left an R4-site note on disk (its `already_covered` treating undetermined as not-covered IS the R4
inflation mechanism — changing it = the held recompute). Witnessed at the JSON boundary (pipeline exit 0, mtime
advanced): behavior preserved (`constraints_with_gaps`=57, `omega_count`=57), companions examined=89/undetermined=30,
0 gap_status↔gaps consistency violations, schema 0 errors, human labels distinguish all three (query + ledger),
gate GREEN.

**(1b) enhanced_report.py wired (2026-07-01 follow-up) — a 5th LIVE site first cleared WRONG.** In the wiring pass
I grepped enhanced_report for a `"gaps"` render, found none, and cleared it — checking a PROXY (does it render the
gaps field) not the PROPERTY (does a human surface distinguish undetermined from no_gap). `build_omega_section`
renders per-constraint "Enriched Omega Context" and collapsed no_gap AND undetermined into one "Not yet enriched"
fallback — Pattern 6 at the PRIMARY human surface (`constraint_reports/*.md`, the essay-synthesis input). Caught by
the operator's "does this reach enhanced_report" question. Fixed: `build_omega_section` leads with a `gap_status`
operability line from `enriched_pipeline.json` (enrich already passes it through) and no longer says "not yet
enriched" for a no-gap/undetermined constraint. Witnessed: renders "UNDETERMINED (reason) — NOT 'no gap'" /
"no gap (examined…)" / "gap detected" for one of each; imports clean; gate GREEN. Live-consumer count is **5**, not 4
— the proxy-check miss is itself the thread's recurring failure mode (a nearby observable instead of the property).

**(2) positive controls DONE (2026-07-01, `audits/2026-07-01_oq197_acceptance_controls/`) — acceptance MET, with
a refinement.** Counts reproduced FROM SUBSTRATE (not the doc): kernel_v1 canonical-varying = **944** exactly
(stakeholder_facts=0 confirmed); twin `detector_calibration` net-new = **43/53** and net-new ∩ stakeholders-present
∩ `detect_gap_pattern`-fails = **29/41** exactly (read-only load of the untracked `detector_calibration.pl` to
reproduce its net-new set). **Case (i):** all 944 read `undetermined(no_seats)` under source (a), never silent 0;
NEGATIVE control same run — canonical (b) discriminates (gap=944, no_gap=152), so not a stuck probe. **Case (ii)
REFINEMENT:** the 29/41 are NOT monolithically undetermined — under the three-valued contract they split (haiku
29 = 4 undetermined + 25 no_gap; flash 41 = 12 undetermined + 29 no_gap). The 4/12 (`<2` power positions) are the
genuinely-inexaminable ones the old `gap_coverage`≥1 emitted as FALSE `[]`, now correctly `undetermined`; the
25/29 have ≥2 seats spanning ≥2 powers that genuinely AGREE → real `no_gap`. None silent 0 (all labeled). Honest:
the doc's premise that the 29/41 were uniformly "insufficient" was imprecise — most were present-AND-sufficient-
but-agreeing; the literal "29/41 read undetermined" is refuted for the majority BY the fix being more precise.
NEGATIVE control same run — source (a) produces gap+no_gap+undetermined on both twins (haiku 365/114; flash
480/232), not vacuously undetermined.

**DO-NOT-CITE-FORWARD (record caveat):** the earlier "29/41 present-but-insufficient artifact" — the figure that
drove the ~3× detector_calibration net-new inflation and withdrew the (a) ship-it rec — was measured under the
PRE-FIX conflation (it counted all `detect_gap_pattern`-fails as insufficient). This run shows 25/29 haiku and
29/41 flash of those were genuine `no_gap`, not insufficient — so the artifact was REAL but its 29/41 MAGNITUDE
was itself overstated by the same conflation the fix removes. Do not cite 29/41 forward as the artifact size.

**(5) R4 recompute DONE read-only (2026-07-01, `audits/2026-07-01_oq197_r4_recompute/`) — the ~3× inflation claim
is RETRACTED.** Inverted the circular hold (the proposal ruling was starved for the number R4 produces): loaded
`detector_calibration.pl` READ-ONLY (no wire, no commit) and split net-new by the extraction_blindness `gap_status`.
On the fixed detector, guarding correctly on undetermined: net-new = **39/41 determinable** (haiku 43 = gap 14 +
no_gap 25 + undet 4; flash 53 = gap 12 + no_gap 29 + undet 12). The genuine undetermined-inflation is only **4/12**
(≈1.1–1.3×), NOT ~3×. The old "14/12 genuine" was the OPERATIVE (extraction_blindness-fires) bar; the ~3× came from
mislabeling the `no_gap` bucket (25/29 — extraction_blindness examined-and-CLEARED, detector_calibration adds a
distinct author-vs-engine axis) as artifact, the same no_gap↔undetermined conflation OQ-197 fixed. **Consequence for
the proposal ruling:** net-new is SUBSTANTIAL (39/41), so the not-wire case can no longer rest on inflation/redundancy;
it now turns on the module's own open axes (is it calibrated — Ω_E; acceptable false-positive rate — Ω_P). The
COMMITTED `already_covered` change (wiring `detector_calibration` into the pipeline) stays HELD on the proposal ruling —
but that ruling now has its witnessed number.

**R4 RULED 2026-07-01 → OQ-200 (carry as corpus-level OQ, do NOT wire).** The per-firing + per-constraint diversity
measurement (appended to the same audit) showed the 39/41 determinable net-new is low-KIND-entropy (5–6 signatures,
~90% two directional patterns), decomposing into false-summit re-surface (`mountain→tangled_rope`, 13/8 = OQ-70) + a
`tangled_rope→rope` author-over-claims-contestation residual (21/27, the module's distinct signal) + a small tail. So
the honest carrier is an aggregate OQ, not 39 per-constraint firings: **see OQ-200**. `detector_calibration.pl` stays
tracked-but-unwired reference (never wired). This closes the OQ-197 side of the detector question; OQ-200 carries it
forward.

---

## OQ-198 — tensions ledger counts `unknown` as a diverging perspective (false-positive "perspectives diverge")

**Ω-type:** Ω_E (empirical — a witnessable substrate defect; the minimal fix is a code change with a two-sided before/after witness).

**Status:** mitigated
**Priority:** 3
**Deps:** splits_from OQ-197
**Origin:** OQ-197 consumer inventory, 2026-07-01 — found by tracing the ledger's "no gap pattern
matched" line; `dataset_recycling_amplification`'s ledger read "perspectives diverge" while its real types
(scaffold×3) agree and only `analytical` is untyped. Split OFF OQ-197 rather than folded in because the
failure DIRECTION is opposite — OQ-197 under-reports (false negative on the critical omega machinery); this
over-reports (false positive on informational ledger display) — so it is tracked separately per operator
ruling 2026-07-01, with the minimal filter now and the architectural repoint left open.
**Files:** `python/tensions_ledger.py` (`build_block/2`, the index-mismatch `mism` heuristic ~line 131).

**Defect (fixed):** `mism = (persp and len({v for v in persp.values() if v}) > 1)` — `if v` filters
only falsy values, but `'unknown'` is a truthy sentinel for "didn't type this position." So a constraint
whose REAL types agree but has one untyped position (scaffold, scaffold, scaffold, `unknown`) counted
`{scaffold, unknown}` = 2 distinct → false "perspectives diverge." This is a Build-Discipline Pattern-6
sibling (didn't-look `unknown` mistaken for a measured opinion) at the informational ledger level, and a
SEPARATE code path from `report_generator:detect_gap_pattern` — the OQ-197 detector fix does not touch it.

**Minimal fix landed (2026-07-01):** exclude `'unknown'` from the divergence set — `if v and v != "unknown"`.
Two-sided witness (`build_block` before/after on synthetic entries): `dataset_recycling`-like scaffold×3+unknown
`diverge → none` (fixed); scaffold+unknown `diverge → none` (fixed); genuine snare/scaffold/rope divergence
`diverge → diverge` UNCHANGED (negative control — the fix does not over-correct into a false negative).

**OPEN (deliberately not resolved by the minimal fix):** whether this display heuristic should instead
consume `report_generator:gap_status/2` (the OQ-197 three-valued operability result) rather than compute its
own parallel `perspectives`-divergence. Repointing would probably fix it as a side effect, but couples an
informational display to an operability contract calibrated for a different consumer's comparison threshold —
that architectural decision needs checking whether `gap_status` semantics even match what the ledger's
divergence line is meant to convey. Left open; not gated on OQ-197.

---

## OQ-199 — is authored-stakeholder seat disagreement (the gap omega's (a) signal) reliable signal or annotation noise?

**Ω-type:** Ω_E (empirical — answerable by a validation design that scores the divergences against an
independent structural check; the design is not yet built, so the trust question is open, not the type).

**Status:** open
**Priority:** 3
**Deps:** splits_from OQ-197
**Origin:** OQ-197 (a)/(b) ruling, 2026-07-01. The `audits/2026-07-01_oq197_source_h1_crosstab/` cross-tab
established that the gap omega's stakeholder source (a) is NOT redundant with `h1_band` — it diverges on
36/1197 both-determinate constraints (one independent corpus + one correlated twin pair). Ruling (a) was made
on that non-redundancy. But non-redundancy ≠ reliability.

**The open question:** the cross-tab shows (a) carries signal `h1_band` structurally cannot, but NOT that the
36 divergences are cases where (a) is *correct* and the canonical orbit is missing something — as opposed to
cases where the authored `constraint_stakeholder/7` facts are themselves noisy, inconsistent, or wrong.
Authored stakeholder facts are exactly the single-pass authored input this project treats as unverified by
default (the same class as authored d-values and type claims — cf. the seat-disagreement-direction ≠
detector-accuracy distinction). So the (a) signal's *worth* is unestablished: seat disagreement is a fact
about the authoring, not yet a validated fact about the world.

**What would resolve it:** a validation design that scores the divergence constraints against an independent
structural check (not the stakeholder facts themselves) — e.g. do the (a)-only gaps correspond to authored
directionality/beneficiary structure, or to grid-authoring artifacts? Until then, gap-omega firings sourced
from (a) are reported as "authored-stakeholder disagreement," never as validated cover-story detection.
**Files:** `prolog/report_generator.pl` (`seat_type_reading/2` stakeholder clause), `prolog/stakeholder_seats.pl`
(`dr_type_for_stakeholder/3`), `narrative_ontology:constraint_stakeholder/7` authored facts.

---

## OQ-200 — author-vs-engine directional disagreement (the detector_calibration question), carried as a corpus-level OQ not a wired per-constraint detector

**Ω-type:** Ω_E (is the author↔engine directional call *calibrated* — external, no corpus ground truth) compounded with Ω_P (what false-positive rate is *acceptable* — a value decision no run produces). Both open by construction; no computation available today resolves either.

**Status:** open
**Priority:** 3
**Deps:** splits_from OQ-197
**Origin:** detector_calibration Slice-B proposal ruling, 2026-07-01. The R4 recompute on the fixed detector
(`audits/2026-07-01_oq197_r4_recompute/`) gave the module a real net-new figure (39/41 determinable, ~3× inflation
retracted); the per-firing/per-constraint diversity measurement then answered the wire-vs-carry question.
**Files:** `prolog/detector_calibration.pl` (the reference implementation — tracked but UNWIRED by this ruling),
`docs/design/detector_calibration_omega_proposal.md`.

**RULING (operator, 2026-07-01) — carry the question as this corpus-level OQ; do NOT wire per-constraint firings.**
The module's value was always its distinct *question* (author-claimed type vs engine-computed type, a directional
disagreement), never its per-constraint resolution. The evidence: on the twins the 39/41 determinable net-new
firings carry only 5–6 distinct `(Class, author→engine)` signatures, and the two dominant ones account for ~90% of
seat-firings — low KIND-entropy. A reviewer reading 39 per-constraint firings would mostly re-read two patterns,
each carrying the identical "calibration open, FP-rate unset" caveat: that is a *query* ("which constraints carry
the pattern"), not 39 *findings*. The honest carrier matches the entropy of the content — an aggregate OQ, not 39
near-identical omegas. (R2 keep-the-pair logic at module scale: don't collapse distinct things, don't multiply one
thing into 39 instances.)

**Net-new DECOMPOSES — do not cite 39/41 forward as novel findings (constraint-level, both twins):**
- **false-summit re-surface — `mountain→tangled_rope`, 13 haiku / 8 flash constraints.** Author claims `mountain`,
  engine computes `tangled_rope`. This is OQ-70's false-summit / FNL-bait pattern seen through the author-engine
  axis — **NOT genuinely net-new** (the apparatus already knows it via a different mechanism; same shape as the
  (b)≡`h1_band` result — a "distinct" detector re-surfacing something computed elsewhere).
- **the genuinely-distinct residual — `tangled_rope→rope`, 21 haiku / 27 flash constraints (the constraint
  MAJORITY).** The mirror of false-summit: author claims contested `tangled_rope`, engine reads a clean `rope` —
  author *over-claims* contestation. This is the module's real distinct signal, and it is a coherent nameable
  phenomenon, not noise. NB the two diagnostics disagree by denominator: per-SEAT-firing false-summit dominates
  (loud-but-narrow, ~2.8 seats × 13 constraints); per-CONSTRAINT `tangled_rope→rope` dominates (quiet-but-broad,
  ~1 seat × 21). Both recorded; the residual is the constraint-majority, refining the earlier "small residual"
  expectation.
  **Note for whoever picks up Ω_E (build-time, not to act on now):** the residual is not just "the leftover," it is
  the *specifically-informative* leftover. Its DIRECTION is the opposite of false-summit and of most of what this
  thread circled — here the **engine reads LESS extractive than the author** (author says contested `tangled_rope`,
  engine says clean `rope`), whereas false-summit and the snare cases are the engine reading *more*. "Engine
  under-reads the author's contestation claim" and "engine over-reads into extraction" are different failure modes
  with different costs. Because the residual is entirely the former AND is the one bucket NOT confounded by a known
  corpus pattern (false-summit already belongs to OQ-70), it is the **cleanest available signal on whether the
  detector's directionality is calibrated at all** — so if OQ-200 ever gets the independent structural check its Ω_E
  needs (one that scores divergences against something OTHER than the authored inputs that produced them), the
  `tangled_rope→rope` residual is the bucket to score first.
- **severity + singleton tail — ~5–7 constraints** (`tangled_rope→snare`, `rope→tangled_rope`, `snare→rope`,
  `rope→snare`): the small genuinely-diverse remainder.

**REPORTING CONDITION (binding).** Wherever this question is surfaced (this OQ, any future summary), a firing is
reported as **"author↔engine directional disagreement, calibration open (Ω_E), FP-rate unset (Ω_P)"** — NEVER as
"miscalibration detected." Same discipline as OQ-199 for the gap omega: the unresolved calibration question travels
with the claim.

**Disposition of the module.** `detector_calibration.pl` stays **tracked-but-unwired reference** (this ruling makes
it tracked — no longer untracked WIP; nothing `use_module`s it, it is loaded by nothing in the pipeline). Kept so
the implementation exists if Ω_E ever gets a ground-truth answer that would make per-constraint firing worth
revisiting; until then this OQ is the carrier. **What would change the ruling toward wiring:** an external
calibration answer (Ω_E) plus an accepted FP-rate (Ω_P), OR evidence that the `tangled_rope→rope` residual carries
per-constraint signal a reviewer needs at the row level rather than the aggregate.

---

## OQ-201 — Temporal gate consumes non-temporal data: `compute_temporal_stability` folds scalar `constraint_metric` instead of `measurement/5`

**Ω-type:** Ω_C (design choice — repoint the temporal gate to the temporal store, or give it a different closed-form treatment). Pattern-5/6 (a gate reads the wrong store; on the current corpora the variance path is dead, so the gate reduces to a presence-check that is vacuously `stable`).

**Status:** open
**Priority:** 2
**Deps:** splits_from OQ-40
**Origin:** OQ-40 census row-22, spun out per operator ruling 2026-07-01 (close OQ-40 on the doc lift only; do NOT repoint now — repointing into corpus-wide temporal absence would repeat the rows 24–25 off-grid trap inverted). Coverage witness run this session, `audits/2026-07-01_oq41_row26_expansion/probe_row22_coverage.pl`.
**Files:** `prolog/signature_detection.pl` (`compute_temporal_stability/3` at `:277–291`; caller binds `SuppMetricName` at `:177→:191`; gate `TemporalStability == stable` at `:405` inside `natural_law_signature/1`), `prolog/config.pl:23` (`suppression_metric_name`).

**The defect.** `compute_temporal_stability(C, MetricName, Stability)` does
`findall(Val, narrative_ontology:constraint_metric(C, MetricName, Val), Vals)` — it reads the **scalar
observer store `constraint_metric/3`**, never the **temporal committer store `measurement/5`** — then
computes variance and feeds `natural_law_signature`'s `TemporalStability == stable` gate (`:405`,
comment "Doesn't evolve"). So the gate that claims to test temporal evolution consults a non-temporal
representation.

**Witness (this session, HEAD `27afde7a`; testsets/ n=119 and kernel_v1 n=1106):**
- **(a) folded metric identity = `suppression_requirement`** (via `SuppMetricName`, `:177→:191`) — NOT
  `extractiveness`/`base_extractiveness`. The coverage rides on counting the right metric.
- **(b) coverage over the non-circular reach-the-gate denominator** (= authors the gate-feeding scalar
  metric, regardless of resulting class — NOT "currently classified `natural_law`," which is circular):
  reach-the-gate **110 (testsets) / 1106 (kernel_v1)**; of those, **WITH a `measurement/5` temporal
  series: 107 / 934** (85–97%); scalar-only **3 / 172**. Coverage is **SUBSTANTIAL** — the gate ignores a
  temporal series that most reach-the-gate constraints actually author.
- **(b') positive control** catches a known `measurement/5` series on both corpora
  (`ability_ceiling_reading` @T=0=0.58; `abrahamic_covenant__isaac_covenant_reading` @T=0=0.6) — so the
  near-zero scalar-only count is measured, not a byte-broken query.
- **Sharper than the census framing:** **0 constraints author >1 scalar `suppression_requirement` value**
  on either corpus, so the `findall` is always a singleton → `compute_temporal_stability` returns via the
  `[_SingleVal] -> Stability = stable` branch (`:284–285`) universally and the **variance path (`:286–290`)
  is dead**. The gate is currently a degenerate presence-check (author the scalar ⇒ `stable`; don't ⇒
  `unknown`) — it measures neither temporal evolution *nor* cross-level dispersion.

**Verdict / disposition.** SUBSTANTIAL coverage ⇒ **repoint-to-`measurement/5` is the eventual
safe-and-cheap fix** (per the plan's verdict rule), but **NOT now** (operator ruling): repointing must
resolve the off-grid-vs-genuine-absence question first (OQ-83/OQ-178 family) so it does not erase authored
signal the way the rows 24–25 fail-close would have. Route: `drift_events`/`measurement/5` temporal read
gated on on-grid coverage. First future witness before any repoint: confirm the `measurement/5` series is
queried on its authored grid (not a synthetic time).

---

## OQ-202 — Generation authoring gap: haiku + contradictions paths under-emit the fact layer (stakeholders[], founding_problem_status; contradictions also stamps no provenance)

**Ω-type:** Ω_E (mechanical generator fix, witnessable by post-fix census bucket shrinkage + per-file fact presence) + a small Ω_C tail (whether the 25 existing under-authored live-leg files are regenerated, patched, or left as-is until rebuild is an operator call).

**Status:** open

**Priority:** 3

**Deps:** splits_from OQ-136

**Origin:** 2026-07-02, the OQ-136 pre-registered provenance join (`audits/2026-07-02_oq136_census_bucket_provenance/`; minted per operator ruling R1/R2 with R6 folded in — one OQ, one witness, same generation path). q6_unmeasured (26) and no_agent_seats (26) cluster on model AND prompt_commit (p_holm=8e-4 each, Holm family 8) with 25/26 member overlap — ONE generation-path gap, two census buckets. Strata, named: **claude-haiku-4-5-20251001** (16/28 of its files in EACH bucket; prompt_commit `22843cdf…` carries 17) and the **`*_contradictions` path** (9/9 — every one, all also missing `story_provenance/8` entirely). Sonnet-4-5 nearly absent (1/64 and 0/64). The hand-read witness: the haiku files' prose PLANS the seat structure ("the payer seat (palestinian_presence_interpreters) should compute…", d-ranges assigned) and names beneficiaries at `constraint_beneficiary` level, but emits zero `constraint_stakeholder/7` facts and no `founding_problem_status/2` — the material exists in prose; the fact layer was never emitted.

**Scope.** (a) Haiku generation path: emit `stakeholders[]` (→ `constraint_stakeholder/7`) and `founding_problem_status/2` facts. (b) Contradictions path: same two fields PLUS `story_provenance/8` stamping (folded R6 — the missing provenance also blanks the model/prompt_commit axes for these files in EVERY future provenance audit). (c) Post-fix expectation (the witness): q6_unmeasured and no_agent_seats shrink toward genuine residue on newly generated stories; the 3 `extraction_unnameable` members' seat limb migrates to `extraction_fired` (their victim limb is genuine-to-the-reading and should NOT change). (d) The Ω_C tail: regenerate/patch/leave the 25 existing under-authored files — beta posture says test-bed-not-backfill, but these are live-leg stories feeding live censuses; operator's call when (a)/(b) land.

**What resolution changes.** The census absence buckets become interpretable as genuine residue rather than generator artifact; the contradictions files stop being provenance-invisible.

---

## OQ-203 — `excluded` role vocabulary cannot express evidential-vs-structural exclusion (mcc false-positive class)

**Ω-type:** Ω_C (schema design ruling: how should exclusion KIND be authored — qualifier field, distinct role atom, or secondary_role?) + Ω_E once ruled (mechanical schema/prompt change, exercised on the testsets/ test bed per the 2026-06-24 posture).

**Status:** open

**Priority:** 3

**Deps:** splits_from OQ-136

**Origin:** 2026-07-02, the OQ-136 hand-read (HANDREAD.md; minted per operator ruling R4, standalone — the fix site is the generation schema, not the census). `radiative_levitation_stratification` authors `constraint_stakeholder(…, alternative_mechanism_proponents, excluded, …)` while its own ABSENT_VOICES text says "**No voices are structurally absent** — … their exclusion is evidential, not structural." `consensus_provenance/2` (and hence the mcc census bucket) reads `excluded` as a structurally absent seat, so this member is a manufactured-consensus false positive BY THE FILE'S OWN TEXT: the role atom is coarser than the prose's distinction. 1/9 mcc members at n=119; 8/9 are genuinely structural exclusions with in-file witnesses.

**The question.** Give the authoring vocabulary a way to distinguish "excluded from the room" (structural — the mcc-relevant kind) from "participates fully but loses on evidence" (evidential — not an absent seat). Candidates: an exclusion-kind qualifier alongside the role; a distinct role atom (e.g. `dissenting`); or routing evidential exclusion through `secondary_role`. Consumers to update once ruled: `consensus_provenance/2`'s Excl set (and anything reading role=excluded as absence). Interaction: OQ-204's first-class mcc surfacing should re-read membership from the predicate, so this fix flows through automatically.

**What resolution changes.** mcc flag precision (the known false-positive class disappears at the source); the `excluded` role stops conflating two exclusion kinds the corpus's own prose already distinguishes.

---

## OQ-204 — Surface manufactured_consensus_candidate as a first-class census/report statistic (operator GO, 2026-07-02)

**Ω-type:** Ω_E (an output-changing build on an already-taken operator ruling; the design constraints below are part of the go).

**Status:** open

**Priority:** 3

**Deps:** splits_from OQ-136, bundled_with OQ-134

**Origin:** 2026-07-02, OQ-136 ruling R4: mcc ruled a genuine corpus category (8/9 hand-read with in-file witnesses, no provenance clustering) and first-class reporting APPROVED. Minted so the go has a tracking home (operator may fold elsewhere).

**Scope.** Surface mcc beyond the bare census count: per-member lines naming the excluded seats (`Excl` from `consensus_provenance/2`) on the census human table and/or enhanced report. Output-changing — own commit with before/after report diffs. **Design constraints (part of the go):** (1) the flag names a structural footprint (unanimity among typed agent seats + a named excluded seat), NEVER a "manufactured consensus" verdict — the candidate semantics travel to every read site; (2) no prevalence-rate headline without the raw count + n_in_domain alongside (OQ-136 denominator caveat); the census currently declares NO prevalence bucket for the consensus source — revisiting that declaration is in-scope here, but the OQ-136 close's reasoning (candidate flag ≠ positive finding) must be answered, not ignored; (3) membership is read live from `consensus_provenance/2`, never a frozen list — so the OQ-203 vocabulary fix flows through automatically.

**What resolution changes.** The first census bucket promoted from honest count to first-class corpus statistic — the seated-reporting precedent for any later bucket promotion.

---

## OQ-205 — Build ε's declaration discipline (the v8 §6.4 handed-forward artifact)

**Ω-type:** Ω_C (design task — which provenance and stability surfaces ε carries is a declared-seat spec, not a measurement) with an Ω_E build/verification tail (the surfaces, once specced, are mechanical to land and control).

**Status:** open

**Priority:** 2

**Deps:** splits_from OQ-135

**Origin:** 2026-07-02, minted at the operator's direction after the OQ-135 close left the obligation homeless: OQ-03 had said the ε-discipline work is "the same task tracked at OQ-135," and OQ-135 resolved without a successor. v8 §6.4 hands it forward as the framework's main owed artifact.

**The obligation (v8 §6.4; v7 §6 l.147 is the confession it answers).** ε is the framework's least-grounded and most load-bearing primitive — authored by judgment, not computed from anything beneath it. The Coupling Theorem settles the world-anchor question *negatively* (a world-anchored ε would be a seat-free seat), so what is owed is not grounding but **declaration**: (a) **ε provenance** — who/what authored each ε, from which reading, under which prompt/schema/model — carried with the value and surfaced at read sites (the same carry-the-provenance-bit rule Build Discipline applies everywhere else); (b) **ε stability** — whether conclusions anchored on an ε survive small perturbation, generalized from the existing cross-axis-anchor requirement (v7 §6) to a checked, surfaced fact rather than a per-anchor manual discipline. *The no-seat pose the framework detects in its constraints is one it must not strike about its own ε.*

**Scope notes.** (1) Spec before build: the design pass enumerates the read sites that must carry the bits (reports, pipeline output, census surfaces) and the perturbation protocol (radius, which conclusions count as "anchored"), each with a pre-registered positive control — a planted provenance-less ε must fail loud, a planted threshold-proximate anchor must flag. (2) First empirical customer: the OQ-78 ε authoring idiom (0.68 mode, 8/2 last-digit grid) — an authoring-convention fingerprint the provenance surface should make visible rather than a separate one-off finding. (3) Existing partial substrate: per-story model provenance already exists (Critical Distinctions: model provenance is a feature); this OQ adds the ε-specific reading-level declaration and the stability check, not a new provenance system from scratch.

**What resolution changes.** v8 §9.5's second falsifiability leg ("owed and unbuilt") becomes built; ε-dependent findings carry their authorship and stability with them at every read site; OQ-03's 03b (if ever run) and any future cross-axis anchor inherit a checked discipline instead of a manual one.

---

## OQ-206 — Taint-guard residual: runtime-constructed goals are invisible to static reachability

**Ω-type:** Ω_E (mechanical census of dynamic-call construction sites on the cross-axis surface, each classified with a witness) with a small Ω_C tail (accept-as-residual vs extend-the-guard is a ruling once the census is in).

**Status:** open

**Priority:** 3

**Deps:** splits_from OQ-135

**Origin:** 2026-07-02, the residual named-and-accepted at the OQ-135 close, minted at the operator's direction. External reviewer (Gemini, v8 Phase-1 review) raised the same class independently.

**The gap.** `prolog/check_axis_boundary.pl` enforces the one-seat invariant by *static* reachability over loaded clause bodies — it descends into control constructs and meta-calls, but a goal **constructed at runtime from data** (`=..`, `atom_concat`+`call`, a goal term fetched from a fact and called) never appears as a static call edge. A kill-condition path (v8 §5.7 form (c)) built dynamically would be invisible to the guard while the gate stays green — absence-of-edge reading as absence-of-path, the exact success-shaped-absence the guard exists to prevent, one level up.

**The work.** (1) Census every dynamic-goal construction site in the engine files (`call/N` with non-literal goal, `=..`, goal terms stored in facts and later called); (2) classify each: can it, on any input, route committer (`cs_*`) content into observer computation? — each verdict carries a witness, not a code-read; (3) positive control REQUIRED before any "none can" claim: plant a dynamically-constructed cross-axis call, confirm the static guard misses it AND the census tool flags it (the census must be shown to find what the guard cannot); (4) then the ruling: accept-as-documented-residual (bounded by the census) or extend the guard (e.g. a runtime taint assertion on the sanctioned bridge's callee, or a lint on dynamic-goal sites touching `cs_*` atoms).

**What resolution changes.** The one-seat kill-condition's enforcement boundary becomes *stated and witnessed* instead of implicit: either "no dynamic path exists, and the probe that would find one is controlled," or the guard grows to cover the dynamic class. Sibling of OQ-137's opt-in-registry residual (same shape: a guard whose coverage is opt-in/static needs its blind zone censused, not assumed empty).

---

## OQ-207 — Build the stakeholder-frame H¹ (per-seat disagreement spectrum over named stakeholder seats)

**Ω-type:** Ω_E (mechanical wiring — two existing pieces joined, with census + controls) with an Ω_C tail (report/census surface design: which read sites carry the new measure and how).

**Status:** open

**Priority:** 3

**Deps:** splits_from OQ-195

**Origin:** 2026-07-02, the operator's pre-check on the OQ-195 plan: the cohomology was built on the four OBSERVER contexts, but the schema/generation (OQ-83/OQ-109) author a second frame — named stakeholder seats with roles {agenda_setter, beneficiary, payer, excluded, observer} and per-seat computed types — at variable cardinality (live census 3–12 seats/story; kernel_v1 has zero). Nothing aggregates the per-seat types into a disagreement measure; `reading_registry.pl:125–128` itself flags `dr_type_for_stakeholder/3` as "no totalized wrapper yet — first candidate if an aggregate ever consumes per-seat chi". Minted per the OQ-195 plan's embedded ruling 5.

**The build.** Wire the per-story vector of `stakeholder_seats:dr_type_for_stakeholder/3` values through the existing pure core `grothendieck_cohomology:obstruction_from_vector/3` → a stakeholder-frame H¹ per constraint. Constraints (all pre-committed): (1) **commentary-grade** per the stakeholder_seats R3 rule — annotates, never overrides classification; (2) **OQ-51 rule carries over** — `unknown`-typed seats filtered, <2 real seats → null never 0, and the zero-seat stories (the OQ-202 generation-path artifact strata: 26/466/212 across the legs at mint) read null — name them as the denominator caveat in any census surface (OQ-136 rule: raw count + n_in_domain alongside any rate); (3) **register the new reading in `prolog/reading_registry.pl` in the same change** (OQ-137 — an unregistered reading escapes the totality guard silently); (4) **built-in validity check:** the reachable spectrum per real-seat count is the proven law (`docs/h1_gap_spectrum_general_n.md` — H(n), H(n,7) for n>7); any observed value outside it is a bug witness, and the plunit spectrum suite (`tests/test_h1_spectrum.pl`) already witnesses the core at n ≤ 12; (5) **coherence with the mcc flag:** `consensus_provenance/2` (OQ-204's manufactured-consensus candidate) is exactly this measure's H¹=0-with-excluded-seat special case — the two surfaces must not fork (Pattern 2); the graded measure should cite or subsume the flag, not duplicate it.

**What resolution changes.** The framework gains a second live cohomology frame — disagreement measured over WHO IS IN THE STORY (agenda-setters, payers, the excluded, the synthetic observer) rather than over the four canonical vantages — at variable cardinality where the general-n law is the governing spectrum. First analytical customers: mcc reporting (OQ-204) gains its graded generalization; the OQ-199 seat-disagreement question gains its instrument.

---

*Last updated: 2026-07-02. Add new items with sequential OQ-NN labels. Mark
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
