# Purity Audit — is each purity notion correct for its application? (2026-06-03)

**Question:** the corpus header reads like *"Network state: cascading … purity is X/Y contaminated"*
and the user wants to know whether purity — however defined — is correct for its application, so
that surprising readings can be partitioned into instrument artifact vs. genuine signal.

**Pinned population:** 1106 constraints (loaded `[corpus] Loaded 1106 testsets successfully`;
`distinct_constraint_ids=1106`; ID snapshot `/tmp/purity_audit/corpus_ids.txt`). Pipeline manifest:
`pipeline_run_at=2026-06-03T16:10:13Z`, `code_commit_short=669eab5`, `code_dirty=True`,
`n_constraints=1106`. All censuses/cross-tabs below ran over this exact ID list. Raw artifacts:
`audits/2026-06-03_purity/` (census.tsv, fixed_structural.tsv, fpn_scan.tsv, controls, probe
scripts, pinned ID list — 12 files).

**Verdict vocabulary:** CORRECT / DEFECT(sign, target) / DESIGN-AMBIGUOUS(ruling needed) /
UNVERIFIABLE(reason). Sign and target are probe outputs, not pre-labels. A *latent* defect has a
witnessed mechanism but no current victim.

---

## 0. The correction key (read this to read through the instrument)

The key licenses subtraction of the **listed modes only**. Completeness over the space of possible
distortions is **UNVERIFIABLE(open)**: a residual after subtracting these modes is *"genuine signal
OR an artifact mode not yet enumerated,"* never simply "genuine signal."

| # | Distortion mode | Sign | Target | Status |
|---|---|---|---|---|
| K1 | Cascade flag saturated: `network_cascade_count_threshold=3` is an absolute count; witnessed 633 severe vs threshold 3 (211×) | alarmist — "cascading" is quasi-permanent at corpus scale | the corpus header line in every report | **realized** |
| K2 | "Contaminated" band mass is mostly a restatement of corpus *type* composition — **witnessed by band × type cross-tab** (§6a): the contaminated band is 98.1% tangled_rope+snare; converse control: rope 92.3% pristine+sound, mountain 95.1% pristine; per-type driver shown (mean F: TR 0.108 / snare 0.045 vs rope 0.829 / mountain 1.000) | reads "dirty" by construction for tangled_rope/snare-heavy corpora; not story-specific | corpus `purity_summary` + per-constraint bands of TR/snare constraints | **realized + witnessed** |
| K3 | Categorical needle (#3 `structural_purity`) was unconditionally flat `inconclusive` (bound-probe bug, §2) | needle destroyed — no information | `genuine_findings_query` STRUCTURAL_PURITY field; `signature_confidence(_, coupling_invariant_rope, _)` (report confidence); anyone calibrated to "the four tests" | **FIXED 2026-06-03** (§2a) |
| K4 | Band-name collision: `fpn_zone` (pure/clean/**contaminated**/compromised/critical at .80/.60/.40/.20) vs `purity_zone` (pristine/sound/borderline/**contaminated**/degraded at .9/.7/.5/.3). "contaminated" = [0.3,0.5) on one surface and [0.4,0.6) on the other | band names misread across surfaces | any reader mixing abductive evidence lines with report bands | **realized** (vocabulary) |
| K5 | Zero-evidence constraint scores **pristine 1.0** (witnessed): `variant([])` → SI=1.25 (out of range, concealed by final clamp) + vacuous coupling → F=1.0 + clean defaults CC/EX=1.0 | up — absence reads as perfect purity | constraints passing the ≥3-classifications access gate whose `classify_at_context` fails at the test grid | **latent** (0/1106 today) |
| K6 | Sentinel feeds banders → worst zone: `fpn_zone(-1.0)=critical`, `purity_zone(-1.0)=degraded` (witnessed direct-call) | down — unknown→worst | no-access constraints, via any future caller that bands before filtering | **latent** (all current gating callers filter −1.0 first) |
| K7 | Tally denominator silently drops no-access: purity_summary sums 1104 of 1106 (M=2 sentinel) | base rate overstates contamination among *visible* stories; unmeasured stories hidden entirely | the X/Y ratio in the header | **realized** (M=2 today) |
| K8 | Stale header comment in `purity_scoring.pl:22-27` (">0.8 sound / <0.3 contaminated") contradicted canonical spec | mislabels bands for code readers | humans reading the module | **FIXED 2026-06-03** (doc-only) |

Practical use: a surprising-**clean** reading on a sparse-data story → suspect K5. A
surprising-**dirty** reading on a tangled_rope/snare story → mostly K2 (expected by construction).
The header's "cascading" → K1, carries ~no marginal information. Any cross-surface "contaminated"
comparison → K4 first.

---

## 1. There are five purity surfaces, not one

| # | Definition | File | Verdict summary |
|---|---|---|---|
| 1 | `purity_score/2` scalar | `purity_scoring.pl` | formula CORRECT vs spec; absence-handling defects latent (§3) |
| 2 | `purity_zone/2` bands | `logical_fingerprint.pl:605-611` | CORRECT (matches canonical spec, §4) |
| 3 | `structural_purity/2` categorical | `signature_detection.pl:971-991` | **DEFECT — was dead, always `inconclusive`; FIXED 2026-06-03 with witnesses** (§2, §2a) |
| 4 | `effective_purity/4` FPN | `drl_purity_network.pl`, `drl_fpn.pl` | invariants CORRECT (§5) |
| 5 | `fpn_zone/2` bands | `abductive_helpers.pl:97-103` | DESIGN-AMBIGUOUS — second vocabulary colliding with #2 (§4) |

Canonical band spec: `docs/logic_extensions.md` §2.3 (table at lines 776–782), corroborated by the
worked example `logic.md:3023/3028` (0.72=sound, 0.42=contaminated).

---

## 2. DEFECT — `structural_purity/2` is unconditionally `inconclusive` (bound-probe bypasses clause-order)

`signature_detection.pl:975` calls `epistemic_access_check(C, false)` with `false` **bound**. The
check is written for an unbound second argument:

```prolog
epistemic_access_check(C, true)  :- ...count >= MinN, !.
epistemic_access_check(_, false).                       % catch-all
```

With `false` bound, clause 1's head can never unify, the cut never runs, and the catch-all succeeds
for **every** constraint. Witness (data-rich constraint):

```
access_check(C,true) SUCCEEDS
access_check(C,false) SUCCEEDS  <-- smoking gun
access_check(C,Unbound) -> true
structural_purity -> inconclusive
```

Census: `structural_purity = inconclusive` for **1106/1106**, including 93 scalar-pristine and 26
scalar-degraded. The four pass/fail tests, `contaminated(Failures)`, and `determine_pure_subtype`
are unreachable through `structural_purity/2`. This is the repo's documented
*bound-probe-bypasses-clause-order* pattern (`docs/technical/build_discipline.md`). The spec
reproduces the buggy call (`logic_extensions.md:805`). Exactly one consumer exists
(`genuine_findings_query.pl:107`) — its STRUCTURAL_PURITY output has been a constant. Grep confirms
this is the only bound-`false` call site; `purity_scoring.pl:42`'s bound-`true` call is safe (its
failure falls through to the −1.0 sentinel clause correctly).

### 2a. Fix applied and witnessed (2026-06-03, same session)

`signature_detection.pl:975` now calls `epistemic_access_check(C, Access), Access == false, !.`
(inline comment at the call site; warning comment added at the predicate's definition in
`boltzmann_compliance.pl`; both spec snippets in `logic_extensions.md` corrected). Post-fix
witnesses over the pinned 1106 (`postfix.tsv`):

```
LIVE post-fix structural_purity distribution:
  contaminated(...): 1068/1106 = 96.6%   pure_coordination: 27
  pure_natural_law: 9                    inconclusive: 2   (= the 2 no-access sentinels)
live vs projected (fixed_structural.tsv) mismatches: 0
scalar purity_score moved pre->post: 0/1106
```

**Consumer deltas (complete):** (1) `genuine_findings_query` STRUCTURAL_PURITY: constant
`inconclusive` → real values. (2) Found during fix verification — an *internal* consumer the
Pass-D grep missed (it excluded the defining module): `signature_detection.pl:453`
`signature_confidence(C, coupling_invariant_rope, Conf)` maps `pure_coordination → high` (pre-fix
the constant `inconclusive` always took the `medium` branch). Witnessed delta: **20 of the 21**
CI_Rope-signatured constraints are post-fix `pure_coordination` → report confidence medium→high
(1 stays medium: `contaminated+excess_extraction`). `signature_confidence/3` is consumed only by
`report_generator.pl:348` — report metadata, not classification; `dr_type` paths are untouched
(consistent with the 0/1106 scalar non-movement).

**What #3 would say if fixed** *(pre-fix projection, retained for provenance — confirmed exactly by
the live post-fix run above)* (clause-2 logic run directly over the pinned corpus,
`fixed_structural.tsv`): 1068/1106 (96.6%) `contaminated(...)` — every combination includes
`excess_extraction` (the categorical pass bar is excess ≤ 0.05, vs the scalar's gentle 0.20-weighted
decay) — 27 `pure_coordination`, 9 `pure_natural_law`, 2 `inconclusive`. Cross-tab vs the scalar:
**monotone-consistent** (0 cases of scalar-degraded/contaminated ∧ tests-pass) but harsher: **121
scalar-pristine/sound constraints fail the tests** (e.g. `axiom_of_choice_consequence` scalar 0.96
pristine, tests say `contaminated+excess_extraction`). So the two needles, both working, would
disagree on the cut (68% vs 96.6% "contaminated"), not on direction. Whether the ≤0.05 excess pass
bar is intended is DESIGN-AMBIGUOUS → user ruling.

---

## 3. Scalar #1 — formula verified; absence is rewarded, latently

**Verified CORRECT on the pinned corpus (with positive controls):**
- Weights/formula match spec; SI formula consistency 0/1106 mismatches; F/CC/EX default-branch
  consistency 0 violations; direction sane (zone means fall monotonically:
  F 1.0→0.0, EX 0.868→0.071 from pristine→degraded).
- EX direction probe: ε=0.9 → EX=0.0; ε=floor → EX=1.0.

**DEFECT (latent, sign up, target = sparse-data constraints passing the access gate):**
a synthetic constraint holding *nothing but 3 classification facts* scores
`purity_score = 1.0 → pristine` (witnessed; predicted floor was 0.725 — reality is worse):
1. `scope_invariance_test` returns `variant([])` when `classify_at_context` fails at all test
   scopes → `SI = 1−(0−1)·0.25 = 1.25`, **out of range**; the final `min(1.0,…)` clamp conceals it.
   Absence is not just unpunished — it is *rewarded above the maximum*. Corpus today: 0/1106
   `variant_0`, 0/1106 SI>1.0 (latent).
2. `cross_index_coupling` is **total**: `GridSize < 2 -> CouplingScore = 0.0  % Not enough data
   points` → F=1.0 (perfect) on absent data. Positive control: a *nonexistent ID* gets
   `has_coupling=1, F=1.0`. Consequence: the documented F=0.5 neutral default is **unreachable** —
   the prior evidence "default_fired 0/194" (ISSUES.md:1631) and this audit's fresh 0/1106 are
   *vacuous truths* (the branch cannot fire), not data-completeness facts. Old 0/194 and new 0/1106
   are measurements on different populations; both are explained by unreachability.
3. `boltzmann_floor_for` clause 3 silently uses `boltzmann_floor_default=0.05` when
   `coordination_type` is absent — the spec (logic_extensions.md:746) says missing
   coordination_type should yield the −1.0 sentinel. How many corpus constraints hit the default
   floor was **not censused** in this pass (open item below).
4. The fail-after-cut hazard in `purity_score/2` (cut then subscores) is **unreachable by
   construction**: all four subscores are total predicates. Census: 1104 scored + 2 sentinel + 0
   fail/error. Latent-by-construction; becomes live only if a future subscore can fail/throw.

---

## 4. Bands #2/#5 — the zone table is correct; the vocabulary is forked

- `purity_zone/2` **CORRECT**: identical to canonical spec table (logic_extensions.md:776-782).
- The divergent text is `purity_scoring.pl:22-27`'s own header comment (">0.8 sound, <0.3
  contaminated") — **DEFECT (doc-only)**; evidence settles canonicity, no ruling needed.
- `fpn_zone/2` (`abductive_helpers.pl:99-103`) is a **second vocabulary over the same scalar**:
  pure/clean/contaminated/compromised/critical at .80/.60/.40/.20. "Contaminated" denotes
  [0.3,0.5) via `purity_zone` but [0.4,0.6) via `fpn_zone`. Abductive evidence lines emit the
  latter; reports emit the former. DESIGN-AMBIGUOUS: unify, rename, or document.
- Sentinel into banders: `fpn_zone(-1.0)=critical`, `purity_zone(-1.0)=degraded` (witnessed).
  Latent — all current gating callers filter −1.0 first — but the unknown→worst mechanism is live
  for any future caller.

---

## 5. FPN #4 — invariants hold

Corpus scan (`fpn_scan.tsv`, all 1106 at the analytical standard context):
- **No-uplift**: 0/1106 EP > IP. Checker positive control: a synthetic uplift row is flagged.
- **Isolated ⇒ EP = IP**: 8/8 exact (graceful-degradation claim of drl_purity_network.pl:55-57).
- **Sentinel nodes**: the 2 no-access constraints pass EP = −1.0 through (2 and 4 neighbors);
  excluded from drift/severity by the `IP >= 0.0` guards (witnessed in source).
- Contamination realized: 637/1106 have EP < IP; max delta 0.478 — exceeds the per-edge cap 0.30
  because the cap is per-edge and edges sum. Matches design intent; documented here so the number
  isn't read as a cap violation.
- Type tables directionally sane (snare emits 1.0; mountain emits 0.0, immune). Minor latent
  fabricated-middle: `type_immunity(_, 0.5)` catch-all for unknown types.
- `drl_fpn.pl:98`'s `IP = -1.0` fallback is dead code (purity_score is total) — harmless.

---

## 6. The visible sentence — coupled and saturated, denominator filtered

Producers: both halves come from the same `json_report.pl` diagnostic block —
`tally_purity_bands` (line 1244) and `network_stability_assessment` at the analytical standard
context (lines 1267-1273); `enhanced_report.py:262` renders the header.

**Coupling (not concatenation):** severity is a function of the same scalar —
`ep_base_severity(EP)`: EP<0.30→critical, EP<0.70→warning (`network_dynamics.pl:253-255`); drift
requires effective-purity contagion; cascade = NumSevere ≥ 3 *absolute*. Runtime witness on the
pinned corpus:

```
num_drifting=643   num_severe=633   threshold=3   assessment=cascading
```

633 vs 3 is 211× past the boundary: **the assessment is saturated** — no realistic perturbation
moves it, and at this corpus scale "cascading" restates "most purity is < 0.70." The sentence is
one signal wearing two costumes. (A marginal-coupling intervention witness is moot at this distance
from the boundary; the producers were exercised directly at the same context the pipeline uses.)

**Denominator:** purity_summary = {pristine 93, sound 64, borderline 168, contaminated 753,
degraded 26} — sums to **1104**, not 1106: the 2 sentinel (no-access) constraints are silently
dropped (json_report.pl:1552 filter). Correct as exclusion (not collapsed into impure), but the
header never reports "2 unmeasured." Three denominators currently circulate: 1107
(`diagnostic.corpus_size`), 1106 (loaded), 1104 (band sum). The 1107-vs-1106 gap was **not
explained by this audit** — UNVERIFIED, listed as open item.

**Continuity check:** today's ratio 753/1104 = 68.2% contaminated; the user-remembered "530/770" =
68.8%. The contaminated *fraction* is stable as the corpus grew ~770→1106 — consistent with K2
(structural property of scoring on this corpus composition), not story-specific drift.

### 6a. K2 witnessed: band × type cross-tab (the diagnostic payload)

The claim "the contaminated band restates type composition" is a correlation claim; here is its
witness (census zones joined to pipeline `claimed_type`; artifact `k2_band_x_type.tsv`):

| type | pristine | sound | borderline | contaminated | degraded | total | %contam | %pristine+sound | mean F | mean EX |
|---|---|---|---|---|---|---|---|---|---|---|
| tangled_rope | 0 | 40 | 146 | 572 | 21 | 779 | 73.4% | 5.1% | 0.108 | 0.201 |
| snare | 0 | 0 | 21 | 167 | 3 | 191 | 87.4% | 0.0% | 0.045 | 0.018 |
| rope | 52 | 20 | 0 | 6 | 0 | 78 | 7.7% | 92.3% | 0.829 | 0.769 |
| mountain | 39 | 0 | 0 | 0 | 2(na) | 41 | 0.0% | 95.1% | 1.000 | 0.891 |
| scaffold | 2 | 4 | 1 | 3 | 0 | 10 | 30.0% | 60.0% | 0.475 | 0.594 |
| piton | 0 | 0 | 0 | 5 | 2 | 7 | 71.4% | 0.0% | 0.000 | 0.426 |

- The contaminated band is **98.1% tangled_rope+snare** (572+167 = 739 of 753).
- **Converse control** (the predicted shift on type-restricted subpopulations): rope → 92.3%
  pristine+sound; mountain → 95.1% pristine, 0% contaminated.
- Driver per type: factorization F collapses exactly where type varies across contexts (TR 0.108,
  snare 0.045) and is near-perfect where it doesn't (rope 0.829, mountain 1.000); snares are also
  maximally above the extraction floor (EX 0.018).

K2 is therefore witnessed, not inferred: **read the corpus purity line as a type-composition echo;
the diagnostic residual lives in the off-diagonal cells** — the 6 contaminated ropes, the 40 sound
and 21 degraded tangled_ropes, the 3 contaminated scaffolds. Those are the constraints where purity
says something the type does not.

**FIT ruling forced (user's):** should the corpus header's purity line (a) keep reporting scalar
band counts (mostly restating type composition), (b) report it conditioned on type (purity within
ropes — the spec §2.1 use case), and/or (c) report the unmeasured count alongside? And should
`network_cascade_count_threshold` be proportional rather than absolute?

---

## 7. Fixes — applied (1, 2, 8-spec) and held

1. ~~**signature_detection.pl:975**~~ **APPLIED 2026-06-03** with witnesses (§2a): post-fix
   distribution matches projection 0-mismatch, scalar unmoved 0/1106, consumer deltas enumerated
   (genuine_findings_query; 20/21 CI_Rope report-confidence medium→high). Spec snippets in
   `logic_extensions.md` corrected; warning comment added at `epistemic_access_check/2` definition.
2. ~~**purity_scoring.pl:22-27**~~ **APPLIED 2026-06-03**: header comment now states the canonical
   zones (logic_extensions.md §2.3 / purity_zone).
3. **scope_invariance_subscore**: treat `variant([])` as no-data (0.5 neutral or propagate
   insufficiency), and clamp SI to [0,1] so out-of-range can't hide under the final clamp.
4. **cross_index_coupling** `GridSize < 2`: fail or return a sentinel instead of 0.0 — fail-closed
   on absence rather than awarding perfect factorization (build-discipline Pattern 5).
5. **fpn_zone / purity_zone**: add explicit `< 0.0 → unknown` guard clauses; unify or rename one
   vocabulary (DESIGN-AMBIGUOUS — ruling).
6. **network_cascade_count_threshold**: proportional or severity-band-aware (DESIGN-AMBIGUOUS —
   ruling).
7. **purity_summary**: emit `no_access: M` alongside bands (DESIGN-AMBIGUOUS — ruling).
8. **boltzmann_floor_for** default-floor clause: align code or spec (spec says sentinel).

## 8. Open items this audit did not close

- How many constraints hit `boltzmann_floor_default` (no `coordination_type`) — sub-census not run
  (folded into OQ-60).
- ~~The 1107 vs 1106 `corpus_size` gap~~ — **explained** during fix verification: corpus churn
  between the pipeline run (16:10Z) and the audit load. The pipeline's per_constraint contains
  `catholic_church_1200`, absent from the current `testsets/` (no file, no git history — removed
  untracked); 3 new untracked testsets also in play. Denominators are timestamp-relative, as the
  manifest convention intends.
- Tracker entries filed: **OQ-60** (latent absence-reward, log-only by ruling), **OQ-61** (header
  semantics: cascade saturation / type-echo / hidden no-access — three operator rulings), **OQ-62**
  (band vocabulary fork + sentinel→worst-zone guards). Session note in KNOWN_STATE.md (2026-06-03).
- **Calibration (named next step, user-seeded):** this audit proves *direction* and characterizes
  distortion; it does **not** validate that purity tracks real story quality. That requires a
  handful of anchor stories where the user holds ground truth, tabulated instrument-reading vs.
  expected. Until then the correction key partitions surprise only relative to the instrument's
  internal semantics. Slot reserved here.
