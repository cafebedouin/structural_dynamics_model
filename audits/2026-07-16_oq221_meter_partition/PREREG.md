# OQ-221 — Firing-condition partition of the defect/gate set: PRE-REGISTRATION

**Date:** 2026-07-16 · **Status:** PREREG (committed before any adjudication; the commit
timestamp is the discipline). Plan: `~/.claude/plans/review-issues-md-and-next-effervescent-bubble.md`
(rev 4, post-review ×3). Parent: OQ-221 (`ISSUES.md:10284`), splits_from OQ-214 (`:9696`).

## The law under test (final form)

> A defect is mechanically gateable IFF a specifiable firing condition separates the two
> distributions: **P(fire | witnessed defect) high AND P(fire | witnessed earned) near-zero.**

One rate is not gateability: a condition that fires on nothing scores a perfect earned-rate.
Every RULED row runs its condition over BOTH corpora and reports both rates with intervals.
A condition that misses the witnessed defects is not gateable no matter how clean it is on
earned material.

## Partition states

- **RULED** — condition executed over both corpora; both rates reported with intervals.
  Separation with a tight earned-side bound → gateable; positive earned-side fires →
  reader-held at any arming threshold.
- **PROPOSED** — condition specified, untested: predicted defect-rate, predicted earned-rate,
  test cost. These rows ARE the build queue.
- **UNSPECIFIABLE** — no firing condition constructible after recorded attempts. The only
  true reader-held-by-kind.

**RULED-gateable is never a certificate**: it is "0 fires observed at n=k, Wilson 95% upper
bound X" — rows rank by upper bound.

## Endogeneity kill condition (verbatim from the ratified plan)

> **[EDGE] Endogeneity kill condition (review point 3):** the proposed earned corpus is the
> pipeline's own operator-approved output — survivors, not masterpieces. A vocabulary-shaped
> condition (row 7's *beat/scaffold/arc*) scores near-zero on our own scrubbed output by
> construction; that's the falsifier firing on an artifact of the denominator. Therefore: **if
> the ratified earned corpus contains no prose of external provenance, no row may be ruled
> gateable on a vocabulary-shaped condition** — the checkpoint explicitly asks the operator for
> external earned material, and failing that, every gateable ruling is scoped in the WRITEUP to
> "near-zero on pipeline-approved output" (a claim about our habits, not about craft).

**Pass-0 finding bearing on this:** external-provenance earned prose already exists in-repo —
the 10 classic-literature files in `agent/narrative_transform/originals/` (alice_in_wonderland,
an_occurance_at_owl_creek_bridge, eighty_yard_run, frankenstein, lysistrata, medea,
philosophy_four, the_old_man_and_the_sea, the_velveteen_rabbit, treasure_island), already used
as calibration negatives in `audits/2026-07-13_oq214_theme_meter/`. They are proposed below as
the external-provenance earned leg, **subject to checkpoint ratification**. If ratified, the
kill condition is defused for rows whose condition is executable over them; if struck, every
vocabulary-shaped gateable ruling carries the "our habits, not craft" scope.

## Corpus rosters (proposed; ratification = strike/extend at the checkpoint)

### Defect corpus — prose rows (per-instance, with anchors)

The OQ-218 witnessed instances (`audits/2026-07-12_oq218_scored_snare/MANIFEST_RUN{1,2,3}.md`),
enumerated with side = DEFECT:

| # | instance | source story | anchor |
|---|---|---|---|
| D1 | final-scene philosophy seminar ("So it is done. The ledger wins.") | the_good_name_book_rev1 (run 1 seed) | MANIFEST_RUN1 changes |
| D2 | "At least we remember / That is not nothing" close (stage consolation) | run 1 seed | MANIFEST_RUN1 |
| D3 | warm coda ("They sat together… But it was there.") | run 1 seed | MANIFEST_RUN1 |
| D4 | duplicate §II conversion cycle | run 1 seed | MANIFEST_RUN1 |
| D5 | refrain ×4 "The keeping became accounting." (3 CUT occurrences) | run 1 seed | MANIFEST_RUN1 |
| D6 | F39 residue: Pak Rashid interior "The substrate… ontologically real" | run 1 seed | MANIFEST_RUN1 |
| D7 | Salmah "Fairness is not a number" thesis paragraph | run 1 seed | MANIFEST_RUN1 |
| D8 | §VII thesis-restatement block 1 ("gift and its cost. The same feature.") | the_eighth_commentary_rev1 (run 2 seed) | MANIFEST_RUN2 |
| D9 | §VII thesis-restatement block 2 (quote-back + "fairness was not adequacy") | run 2 seed | MANIFEST_RUN2 |
| D10 | ending run ("The system was fair. It had to be… still mattered.") | run 2 seed | MANIFEST_RUN2 |
| D11 | Wang's duplicated could-not-be-specified syllogism (2 CUT occurrences) | the_table_of_winters_rev1 (run 3 seed) | MANIFEST_RUN3 |
| D12 | narrator recap paragraph ("The knowledge Qiu Yue taught was not mysterious…") | run 3 seed | MANIFEST_RUN3 |
| D13 | closing double anaphora (three "They do not tell us" paragraphs, 2 CUT) | run 3 seed | MANIFEST_RUN3 |
| D14 | hope-clause ("hidden in the spaces between the numbers, waiting…") | run 3 seed | MANIFEST_RUN3 |
| D15 | one "The system worked." local stutter | run 3 seed | MANIFEST_RUN3 |
| D16 | resonant-closer tic ×4 (units landing on a summarizing image, "the way X") | 112_ergodocity_kids_rev1 | OQ-214 promotion context (operator Web-Claude read, 2026-07-13) |
| D17 | counting-defect baseline: numeric register 37.6–47.6 per 1,000 | the-empty-pan rev1/baseline arc | OQ-215 close (`ISSUES.md:9831`), plan baseline 141 number-words + 60 numerals / ~6,100 words |

Explanation over-run is witnessed 3/3 at the run level (D1, D7, D8–D10, D12 are its instances).

### Earned corpus — prose rows (per-instance side assignments declared here)

| # | instance / text | provenance | anchor |
|---|---|---|---|
| E1 | rift3 institutional refrain (whole story, earned-dense 5.12) | pipeline output, operator-approved | `agent/narrative_transform/originals/rift3.md` (measured as rift3 in theme_density_table.txt) |
| E2 | rev5 word-arithmetic (register shown REFUSED; assayer-read percentage positional) | operator-adjudicated EARNED | `ISSUES.md:9820–9823` (OQ-215 arm 3); `stories/the-empty-pan_rev5.md` |
| E3 | run-1 earned kept line: "The keeping became accounting." (the 1 surviving occurrence) | manifest KEEP adjudication | MANIFEST_RUN1 |
| E4 | run-2 kept crystallization: "To make the examination adequate… a constraint to be endured." | manifest KEEP | MANIFEST_RUN2 |
| E5 | run-3 kept syllogism ×2 (cross-generation echo is the design) | manifest KEEP | MANIFEST_RUN3 |
| E6–E15 | the 10 classic-literature originals (full texts) | **EXTERNAL provenance** | `agent/narrative_transform/originals/` (list above) |

**Side-assignment declarations (an instance cannot be on both sides):**
- `the-empty-pan_rev2`'s word-arithmetic (×3) belongs to **row 9's contrast pair**, NOT the
  general earned denominator. Row 9's whole value is the same surface witnessed on both sides.
- D5 (refrain CUT ×3) and E3 (refrain KEPT ×1) are **distinct instances of the same surface in
  the same story** — declared here so neither is double-counted; row-level analyses that cannot
  resolve instance-level assignment must drop the pair, not pick a side.
- Same rule for D11/E5 (syllogism: 2 CUT are defect, 2 KEPT are earned).
- The v0.2 IMPROVED arms (`the_keeping_v02_pathA` etc.) are NOT in the earned denominator —
  they are the pipeline's own scrubbed output, the exact endogeneity trap; they may be cited
  as fix-side context only.

### Row 10 register (code shapes, not prose) — own corpus pair

- **Earned side:** OQ-127's legitimately-authored kernel shapes — the 5/7 cohort-zero pilot
  fires (`demographic_skill_mismatch_c0`, `organization_floor_c0`, `scale_ceiling_c0`,
  `organization_floor_c0_d2`, `organization_floor_c0_d3`;
  `audits/2026-06-12_cohort_zero/pilot_witness.out`).
- **Defect side:** witnessed genuine danger-zone cases — **Pass-0 finding: n=0.** No witnessed
  instance of the engine's scaffold gate actually misfiring on a non-scaffold exists in the
  record (OQ-127 frames SDZ as a *predictor* of a possible unintended classification, not a
  witnessed one). **Declared cap, out loud:** with a defect side of n=0, row 10 cannot be
  RULED gateable (P(fire|defect) is unmeasurable); the reachable rulings are reader-held
  (earned-side fires are already positive at 5/7) or PROPOSED-pending-a-witnessed-misfire.

## Corpus n's and resolution limits (declared before any run)

Wilson 95% upper bounds for 0 observed fires at n:
n=3 → 0.56 · n=5 → 0.43 · n=7 → 0.35 · n=10 → 0.28 · n=13 → 0.23 · n=15 → 0.20 · n=20 → 0.16.

- Earned prose corpus as proposed: 15 members (E1–E5 instances + 10 classics); story-level
  n=12 texts. A clean story-level run bounds the earned rate at ~0.23–0.28 — **unresolved is
  the honest word for anything above the row's tolerable false-positive rate; no certificate
  language.** Per-kind instance-level n's are smaller and stated per row at adjudication.
- Defect prose corpus: 17 enumerated instances across 4 stories + the counting baseline arc.
- Row 10: earned n=7 (5 fires observed — a *positive-rate* measurement, which small n supports);
  defect n=0 (nothing bounds).
- **The asymmetry is structural:** the reader-held direction is cheap (a positive earned-side
  fire is a measurement small n supports); the gateable direction is bounded by corpus size.
  H1 can fail to fire; it cannot be *confirmed* gateable-forever at small n.

## Controls and predictions (run FIRST; a control miss stops the run for diagnosis)

- **H1 (control) — counting.** Condition: `_numeric_inventory` density ≥ 10.0/1,000
  (`agent/uke_narrative_orchestrator.py:318`; threshold OQ-215-calibrated). Expected under the
  law: fires on the witnessed defect baseline (band 37.6–47.6) and 0-fires on the earned corpus
  (reported as a Wilson upper bound, per the resolution limit). A miss on either side is a
  control failure → stop, diagnose the instrument before ruling any novel row.
- **H2 (control) — theme-naming.** Condition: `_theme_inventory` density-bearing kinds
  (`:810`). Expected under the law: fires POSITIVELY on earned material (rift3 5.12; classics
  up to 1.88 with nonzero counts) — the witnessed reader-held shape. A zero earned-side rate
  here would be a control failure (the OQ-214 calibration already witnessed positives).
- **P3 (prediction, NOT a control):** explanation over-run lands **UNSPECIFIABLE**. Stated as
  a single-state prediction so it can be wrong. (If Pass 2 constructs a condition that
  separates, P3 is falsified and the row is the partition's most valuable output.)

Predicted rates below are **hypotheses, not pass criteria** — the pass criterion is the pasted
run output (audit-plan discipline: plans must not pre-assert what the run discovers).

## Per-row protocol (applies to every row)

1. Specify the firing condition to the `_numeric_inventory` precision standard
   (declared-vs-actual: the exact predicate/lexicon/threshold is written down BEFORE the run).
   Record failed specification attempts → UNSPECIFIABLE.
2. Run over BOTH corpora where executable; report both rates with intervals.
3. **Within-row positive control:** any zero-claim first plants/locates a known instance of
   that row's OWN firing condition and shows the instrument surfaces it. Cross-row controls
   unused (per-instrument controls discipline).
4. Parallel column (R6): contract-violation vs standard-deficit; agreement/disagreement with
   the separation verdict reported at adjudication. This roster pre-announces nothing about it.

## Roster — rows, firing-condition attempts, predictions, outcome meanings

### Tier 1 (mandated)

**Row 1 — counting (H1 control).**
Condition: `_numeric_inventory` density_per_1000 ≥ 10.0. Within-row positive control: the
empty-pan defect baseline must fire. Predicted (hypothesis): defect-rate 1/1 baseline arc;
earned-rate 0/n. Outcome meanings: both hold → the law's anchor case confirmed on the new
two-corpus instrument; either misses → instrument diagnosis before all novel rows.

**Row 2 — theme-naming (H2 control).**
Condition: `_theme_inventory` density-bearing kinds (anaphora + causal_chain), any fire.
Within-row positive control: rift3 must flag (witnessed 5.12). Predicted: earned-side fires
POSITIVE → reader-held at any arming threshold. Outcome meanings: positive earned fires →
the OQ-214 null re-confirmed on the ratified instrument; zero earned fires → control failure.

**Row 3 — explanation over-run (P3).**
Recorded failed attempts (carried from OQ-214, which IS the recorded attempt): (a) density-bearing
theme kinds — killed by identical SEED/IMPROVED counts in runs 1–2 (WRITEUP.md, theme_density_table);
(b) adjudication-only kinds — forbidden as gate by the bucket rule (arming refrain would have
issued revision calls against rift3's creed: craft suppression, the OQ-214 kill condition).
Pass-2 obligation: one genuine fresh specification attempt (not a strawman), recorded verbatim,
before ruling. Predicted: UNSPECIFIABLE. Outcome meanings: UNSPECIFIABLE → P3 confirmed,
reader-held-by-kind; a separating condition found → P3 falsified, highest-value build-queue item.

**Row 4 — sentence-level ceiling.**
Defect: prose reads machine-made at the SENTENCE level (the Q2 double-No; residue-inversion —
cold readers Q2-pick out-of-register jargon as INIMITABLE, so sentence-level taste cannot
adjudicate residue from either side of the blind wall; OQ-214 promotion context, OQ-218
READOUT_STAGE2). Specification attempts to record in Pass 2: any deterministic sentence-level
extractor proposal must name its surface; the known candidates (rhythm regularity, clause-length
variance) are unanchored to any witnessed instance list — the defect corpus has no per-instance
enumeration at sentence grain. Predicted: UNSPECIFIABLE (no per-instance defect corpus exists at
this grain — the graduation step for a future revisit is a reader-annotated sentence-level
instance list). Outcome meanings: UNSPECIFIABLE-with-graduation-step → honest OPEN; a condition
+ instance list constructed → PROPOSED.

**Row 5 — break-execution.**
Defect: the break contract stated-not-executed (target_prior restated in narration instead of
landed in dramatized material; the contract's ADDRESS is authored at stage 0, execution belongs
downstream — `uke_narrative_orchestrator.py:937–939`). Condition attempt (to the precision
standard): n-gram overlap ≥ k between the run's `break_contract` target_prior text and story
prose outside dialogue-attributed positional statements — the "story explains its own break"
tell. Vocabulary-shaped in part (subject to the endogeneity kill for the contract-vocabulary
leg). Predicted: PROPOSED (condition specified, untested). Predicted defect-rate: moderate on
D1/D8–D10 (hypothesis); predicted earned-rate: unknown, honestly. Test cost: low — pure-function
scan over run dirs carrying `break_contract` files; no spend.

**Row 6 — beneficiary-collapse.**
Defect: the Beneficiary-voice structural blindness breaking — narrator signals awareness of
privilege/judgment ("If the Beneficiary starts feeling sorry for the Condemned, the structural
blindness has broken", `agent/narrative_transform/uke_narrative_v1_4.md:585–595`). Governed
today by a waivable model scan (OQ-214 origin). **Pass-0 finding: defect side n=0** — no
enumerated witnessed beneficiary-collapse instance exists in the manifests. Condition attempt:
awareness-signal surface (self-aware privilege lexicon + sympathy-turn toward the Condemned
inside Beneficiary-voice sections) — merit-correlated on its face (an earned turn can use the
same surface). Predicted: PROPOSED-capped (cannot be RULED with an empty defect side; the
graduation step is one witnessed instance). Test cost: low once an instance exists.

**Row 7 — F39 framework-residue (vocabulary-shaped — endogeneity kill applies).**
Defect: pipeline/protocol vocabulary in prose (witnessed: D6 "The substrate… ontologically
real"). Condition (declared now, two tiers, declared-vs-actual standard):
- Tier A (hard protocol tokens): substrate, ontologically, extraction, coordination function,
  legibility, constraint (as abstract noun), scaffold/piton/snare/tangled rope (metaphor-free
  uses), theater ratio, classification.
- Tier B (soft ambient tokens): "the system", extraction (economic sense), coordination.
Fire = any Tier-A token in story prose; Tier B reported but not firing. Within-row positive
control: D6 must fire on Tier A. Predicted: defect-rate 1/1; earned-rate near-zero on classics
for Tier A (hypothesis — Frankenstein-era diction is the live risk, which is exactly why the
external leg matters), NOT near-zero for Tier B. Outcome meanings: Tier-A separation on the
external leg → the first RULED-gateable novel row (bounded, no certificate); Tier-A fires on
classics → reader-held; **if the classics are struck at the checkpoint, this row cannot be
ruled gateable at all** (kill condition, verbatim above). Test cost: trivial grep-class scan.

**Row 8 — resonant-closer.**
Condition: `_detect_resonant_closer` (`uke_narrative_orchestrator.py:772` — paragraph-final
"the way X" or aphorism-shaped closer; exists, executable). Within-row positive control: the
ergodicity story's ×4 operator-witnessed closers must surface (D16). Predicted: earned-side
fires POSITIVE (classics carry paragraph-final aphorism-shaped closers — old-man clos=31 in the
calibration table) → reader-held. Outcome meanings: positive earned fires → RULED reader-held
(the kind stays adjudication-only, as built); clean earned side → surprising, bounds reported,
candidate for arming discussion (NOT auto-armed; operator seat).

**Row 9 — word-arithmetic (both-sides contrast pair).**
Condition: `_detect_word_arithmetic` (`:790`; exists, executable). Side assignments per
instance, declared above: earned = rev2 ×3 ("defensible in context", OQ-214) + rev5 (adjudicated
EARNED); defect = no adjudicated-unearned instance currently witnessed (the_platform_knows
wmath=1 is UNADJUDICATED — it enters neither side unless the operator adjudicates it at the
checkpoint). Predicted: RULED reader-held — the same surface fires on operator-earned instances
by construction; the row's whole value is witnessing the same condition firing on both sides.
Outcome meaning: confirms flag-not-fail as the ceiling for this kind.

### Tier 2 (capped out loud)

**Row 10 — SCAFFOLD_DANGER_ZONE (own register pair; code shapes).**
Condition: exists — `linter.py:292–308` (ε ≤ ceil ∧ beneficiary data ∧ no enforcement ∧ no
sunset ∧ low theater). Earned side: 5/7 fires witnessed (pilot_witness.out). Defect side: n=0
(declared cap above). Predicted: RULED reader-held-as-armed at the earned-rate leg (5/7 supports
exactly "not near-zero," nothing more), UNRESOLVED at the defect leg. Outcome meanings: feeds
OQ-127 remedy framing (narrow/demote/reword) as a cross-note, not a self-resolved ruling —
OQ-127's remedy is the operator's call.

**Row 11 — scaffold rising-suppression (OQ-185).**
Defect-or-not is the operator's seat (OQ-185 option (d): a scaffold may legitimately tighten
before sunset — if so, this is not a defect and the row exits the partition). Branches written
now, seat escalated at the checkpoint:
- Branch (defect): condition exists — the OQ-39 `cs_verdict(C, scaffold_suppression_escalating)`
  detector; defect side = rising-suppression scaffolds (13:2 / 53:7 / 43:9 across legs); earned
  side = legitimately-tightening scaffolds, which is EXACTLY the contested set — the earned
  side cannot be populated until the seat rules, so even this branch cannot be RULED today.
- Branch (not-defect): row exits; OQ-185 re-grades the verdict to neutral descriptor (its
  option d), no partition entry.
Predicted: BLOCKED-ON-SEAT (recorded as such, not forced into the three states).

### Rows 12–14 — OPEN-by-scope (declared here with graduation steps; not adjudicated)

- **Row 12 — OQ-198 (tensions ledger counted `unknown` as diverging).** Engine-side, already
  mitigated with a deterministic filter. Graduation step: run the filter's condition over the
  three live legs as a two-distribution measurement (defect = pre-fix false-positive set;
  earned = genuinely-diverging sets) — a mechanical re-run, deferred for scope only.
- **Row 13 — OQ-197/OQ-201 class (Pattern-6 gates reading absent/wrong stores).** Code-shaped;
  needs its own register pair like row 10 (defect = gates witnessed vacuous; earned = gates
  passing on authored data). Graduation step: per-gate coverage witnesses from the OQ-44-class
  audit; out of prose scope here.
- **Row 14 — OQ-58 (dangling cs_reading_relation edges).** Deterministic detector already wired
  (non-gating run_pipeline step). The defect-or-sparsity question is settled policy
  (`testsets/` singleton sparsity is INTENDED — beta posture); graduation step: the deferred
  per-edge content sort at the narrative-read pass, an operator-seat item, not a meter.

## R5 selection check (Pass-0 result; feeds WRITEUP scoping)

**Was counting reader-noticed before `_numeric_inventory` existed? YES.** The 2026-07-11
counting-defect plan documents the witnessed baseline (141 number-words + 60 numerals /
~6,100 words) from operator reads that PREDATE the meter; the meter was built because a reader
noticed. **Scoping consequence, to be carried in the WRITEUP:** the defect roster is selected
on reader-noticeability. Every partition ruling is over *reader-noticed* defects; a defect no
reader has noticed can enter neither corpus, so no floor claim ("no meterable defect remains")
can be supported by this instrument — only "no meterable defect remains among the noticed."

## Execution sequence (binding)

1. This PREREG committed ALONE (commit 1).
2. **Checkpoint:** operator ratifies the per-register corpus pairs (strike/extend; explicitly
   asked for external-provenance earned prose — the classics leg — and for the two open
   adjudications: the_platform_knows wmath instance; row 11 seat). **Adjudication is blocked
   until ratified.**
3. Pass 2: H1/H2 first (a miss stops the run); then per-row protocol over the ratified corpora.
4. Pass 3: WRITEUP.md + ISSUES.md (OQ-221 → mitigated with build queue) + index regeneration,
   commit gated on `./scripts/gate.sh` GREEN (pasted).
