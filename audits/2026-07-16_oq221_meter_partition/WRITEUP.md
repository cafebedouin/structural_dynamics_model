# OQ-221 — Firing-condition partition of the defect/gate set: WRITEUP

**Date:** 2026-07-16 · **PREREG:** `PREREG.md` (commit `a823cd47`; ratification AMENDMENT 1 at
`ad132911`, both predating every run). **Raw runs:** `partition_run.txt` (the H1 stop witness),
`partition_run2.txt` (post-diagnosis full run), `partition_run.py` (the instrument).
All conditions are pure functions; zero LLM spend.

## Headline finding (F1) — the law's anchor case is deployment-distribution-relative

**H1's earned-side prediction was falsified.** The counting condition (`_numeric_inventory`
density ≥ 10.0/1,000) fired on **4/12 ratified earned texts** — eighty_yard_run 16.32,
philosophy_four 14.13, treasure_island 10.78 (partially TOC-inflated, see footnotes), and
operator-approved **rift3 at 46.04, inside the recorded defect band (37.6–47.6)**. Per the
PREREG stop rule the run halted for diagnosis (`partition_run.txt`); diagnosis witnessed
in-session: within-row positive control PASSED (baseline fires at 50.57), the earned fires are
real tokens (ordinary earned prose carries number-words at ~10–16/1,000: "Fifteen years ago",
"Two frowning boys"; rift3's vent-logging voice — "14.8%", "Vent Three" — is genuinely dense
*in-register*, and the register is the craft). **Prediction error, not instrument error:**
threshold 10.0 was variance-calibrated on pipeline output (improved ceiling ~0.5/1,000), never
on human-prose base rates.

Consequence: **counting is gateable relative to the distribution the gate actually meets**
(within the pipeline: defect band 37.6–50.6 vs improved ≤ 0.5 — clean separation, witnessed at
OQ-215) **and reader-held as a universal craft meter** (number-dense masterpieces exist; rift3's
logging register would gate). The OQ-214 law survives in sharpened form:

> A defect is mechanically gateable IFF a specifiable condition separates
> P(fire | witnessed defect) from P(fire | witnessed earned) **over the distribution the gate
> will actually meet. Gateability is deployment-relative; a gateable verdict must name its
> denominator.**

**With the decay rider (added on operator review):** deployment-relativity converts every gate
from a fact into a dated measurement — the deployment distribution moves whenever the generation
model, genre, or prompt discipline changes, and F1 is itself the demonstration (a threshold
calibrated on pipeline variance met a different distribution and failed silently until this
session constructed the test). So the restatement must name the decay condition, not only the
denominator: **an armed gate carries its calibration distribution and a re-validation trigger —
re-run the two-corpus measurement when the deployment distribution changes materially** (model
swap, genre shift, prompt-regime change). Without the rider, §11b would ratify gates-as-facts
with a footnote, and the next F1 waits for the next accidental audit.

This is the restatement **proposed (flagged, not rewritten)** for
`docs/technical/build_discipline.md` ("A deterministic gate is buildable IFF…") and
`docs/design/design_discipline.md` §11b — operator ratifies the doc edits, not this audit.

### Stop-rule accounting (why ruling proceeded after the H1 miss)

The PREREG stop-rule fired and the run halted (`partition_run.txt`); after diagnosis the run
proceeded, and **the diagnosis licensed exactly the rulings made and no others**: every novel
ruling landed in the direction insensitive to the miscalibration F1 exposed. Reader-held
rulings are existence proofs — earned-side fires were *observed*, and a fire is real regardless
of where any threshold sits; UNSPECIFIABLE and PROPOSED assert nothing about separation. **No
novel row was ruled gateable.** Had any row come back gateable post-F1, ruling it would have
violated the stop-rule (a gateable verdict leans on the calibrated half of the instrument F1
broke) and it would have been held for a re-calibrated instrument instead.

## The partition table

Units per AMENDMENT 1: fires/1,000 words + per-text binary fire, identical over both legs.
Earned story leg n=12 (10 external classics + rift3 + rev5). Bounds are Wilson 95%. **No
certificate language — bounds only.** Zero-defect-side rows are PROPOSED-capped by the
ratified zero-D rule regardless of earned-side rate.

| row | state | condition | defect-rate | earned-rate [Wilson95] | R6: contract vs deficit | anchors |
|---|---|---|---|---|---|---|
| 1 counting | **RULED — gateable within pipeline distribution; reader-held as universal meter (F1)** | `_numeric_inventory` ≥ 10.0/1000 | 1/1 baseline (50.57; OQ-215 band 37.6–47.6) | 4/12 [0.138, 0.609] | contract-violation ("no numbers added") — AGREE within distribution | partition_run2.txt H1; OQ-215 |
| 2 theme-naming | **RULED reader-held** | `_theme_inventory` density-bearing kinds > 0 | 3/3 seed runs (3.30–9.08) | 11/12 [0.646, 0.985] | standard-deficit — AGREE | partition_run2.txt H2; OQ-214 null re-confirmed |
| 3 explanation over-run | **UNSPECIFIABLE (P3 confirmed)** | 3 recorded attempts, all fail (below) | 6 D-instances exist; no condition reaches them | — | standard-deficit — AGREE | MANIFEST_RUN1–3; OQ-214 WRITEUP |
| 4 sentence-level ceiling | **UNSPECIFIABLE** | no per-instance defect corpus at sentence grain; no extractor names a surface | 0 instances at grain | — | standard-deficit — AGREE | OQ-218 Q2 double-No (run-level testimony) |
| 5 break-execution | **PROPOSED** | n-gram overlap: run-dir `break_contract` target_prior text × story prose outside positional dialogue | predicted ≥3/5 mapped instances (hypothesis) | predicted unknown (honest) | contract-violation — pending | D1, D7, D8–D10; orchestrator :937–939. Test cost: low, pure function |
| 6 beneficiary-collapse | **PROPOSED-capped (defect n=0)** | awareness-signal surface in Beneficiary-voice sections (needs voice-map sidecar) | 0 witnessed | not run (condition needs absent input) | contract-violation (v1_4:585–595) — pending | graduation: one witnessed instance |
| 7 F39 residue (as declared) | **RULED reader-held** | Tier-A lexicon as PREREG-declared | 2/4 seed stories (D6 control: 3 hits PASS) | 3/12 [0.089, 0.532] | contract-violation ("no protocol vocabulary") — PARTIAL | partition_run2.txt Row 7 |
| 7b F39 narrowed (build queue #1) | **PROPOSED** | hard-protocol sublist {substrate, ontologically, coordination function, theater ratio, tangled rope, piton, legibility} — "scaffold"/"snare"/"extraction"/"constraint"/"classification" excluded (English homonyms, witnessed) | 1/4 stories post-hoc (the D6 story) | 0/12 post-hoc [0.000, 0.243] — **post-hoc observation, NOT a ruling**; needs fresh pre-registered confirmation set | contract-violation — AGREE if confirmed | qualifier (b) verbatim below |
| 8 resonant-closer | **RULED reader-held** | `_detect_resonant_closer` (control: ergodicity 8 surfaced incl. 3 "the way X" — PASS) | 4/4 stories (3.51–7.67/1000) | 11/12 [0.646, 0.985]; density variant also fails — rev5 at 5.21 sits INSIDE the defect range | standard-deficit — AGREE | partition_run2.txt Row 8 |
| 9 word-arithmetic | **PROPOSED-capped (defect n=0)** | `_detect_word_arithmetic` (control: rev2 ×3 PASS) + **recall gap witnessed**: rev5's operator-adjudicated earned instance is percentage/portion-form (rev5 L101/L129/L157) and evades the minus-equals regex | 0 witnessed | 0/12 [0.000, 0.243], with the recall caveat | register-contract boundary — pending | pathB instance left unadjudicated (Q3); graduation: cold-read defect adjudication firewalled from row needs |
| 10 SCAFFOLD_DANGER_ZONE (code register) | **PROPOSED-capped (defect n=0)** | `linter.py:292–308` (exists) | 0 witnessed genuine misfires | 5/7 [0.359, 0.918] legitimately-authored shapes fire (pilot_witness.out, as-of 2026-06-12) | lint predictor, neither — capped | feeds OQ-127 remedy framing (operator's call) |
| 11 rising-suppression | **BLOCKED-ON-SEAT** (Q4 ruling: deferred) | branch (defect): OQ-39 `cs_verdict` detector exists; earned side IS the contested set — unpopulatable until the seat rules | 13:2 / 53:7 / 43:9 base rates | — | — | OQ-185 keeps the ruling |
| 12 OQ-198 | OPEN-by-scope | graduation: two-distribution re-run of the landed filter over the three legs | — | — | — | PREREG rows 12–14 |
| 13 OQ-197/201 class | OPEN-by-scope | graduation: per-gate coverage witnesses (own code-register pair, row-10-style) | — | — | — | — |
| 14 OQ-58 | OPEN-by-scope | detector wired (non-gating); residual is the operator-seat content sort | — | — | — | — |

### Row 3 — the recorded specification attempts (why UNSPECIFIABLE)

1. *Density-bearing theme kinds* (OQ-214, the standing recorded attempt): killed by identical
   SEED/IMPROVED anaphora/causal counts in runs 1–2 — the improvement signal lives in the
   forbidden merit-correlated kinds.
2. *Adjudication-only kinds as gate*: forbidden by the bucket rule — arming refrain would have
   issued revision calls against rift3's creed (craft suppression, the OQ-214 kill condition).
3. *Fresh attempt (this session, recorded verbatim):* "a narrator-voice paragraph in the final
   15% of the story whose sentences share ≥2 abstract-noun tokens with the story's own
   compression-test statement and carry zero scene deixis" — thesis-restatement via
   self-similarity. Fails three ways: (i) not computable from the story text alone (the thesis
   is authored per-story in the manifest — the condition imports the adjudicator's knowledge);
   (ii) the same surface KEPT once is earned (run-2's crystallization was deliberately kept at
   the pivot; run-3 kept 2 syllogism instances as cross-generation design) — the defect is
   count × placement × earnedness, and earnedness is the reader's call; (iii) any
   count-threshold over restatements reduces to attempt 2.

**P3 confirmed.** Explanation over-run is reader-held-by-kind.

## Build queue (ranked by earned-side Wilson upper bound)

1. **Row 7b — narrowed F39 hard-token lexicon.** Post-hoc 0/12 earned (UB 0.243), 1/4 defect
   stories. Cost: trivial. Graduation to RULED: pre-register the exact sublist, run over a
   FRESH confirmation set including genre-adjacent earned material; arming is the operator's.
   **Recall control (added on operator review):** the 0/12 was measured with a sibling of an
   instrument family with a WITNESSED earned-side recall gap (`_WORD_ARITH_RE` misses rev5's
   percentage-form earned instance) — an undercount that biases exactly toward gateable. All
   reader-held rulings survive a fortiori (more fires only strengthen them), but a zero does
   not count until the confirmation instrument first demonstrates it surfaces a PLANTED
   earned-form instance (the within-row positive control, again).
2. **Row 9 — word-arithmetic: recall extension + defect witness.** Extend the surface to
   percentage/portion forms (the witnessed rev5 shapes), keep flag-not-fail; blocked on one
   cold-read defect-side adjudication (firewalled per Q3).
3. **Row 5 — break-execution overlap condition.** Specified, untested; cost low (pure function
   over uke run dirs carrying `break_contract`).
4. **Row 6 — beneficiary-collapse.** Blocked on a witnessed instance + a voice-map sidecar
   (stage-3 annotation) as the condition's input.

## Floor-claim reframing (unprovable as stated)

The OQ-214 [EDGE] floor claim ("no remaining known defect has a merit-independent signature")
is reframed three ways, all binding:

- **R5 selection scope (Pass-0 check: YES).** Counting was reader-noticed before
  `_numeric_inventory` existed (the 2026-07-11 plan's witnessed baseline predates the meter).
  The whole defect roster is selected on reader-noticeability; every ruling here is over
  *noticed* defects. No floor claim over unnoticed defects is supportable by this instrument.
- **F1 (deployment relativity).** Even the anchor case is gateable only relative to its
  deployment distribution. "Merit-independent signature" was always implicitly
  denominator-relative; now explicitly.
- **Endogeneity scope on vocabulary rulings** (operator qualifier (b), verbatim): *"F39's
  tokens are earned precisely in prose about systems/craft, and neither 19th-century classics
  nor our own scrubbed output covers that region. A row-7 gateable ruling still carries:
  near-zero on classics + pipeline output; the genre-adjacent region is unmeasured."*

Qualifier (a), carried per ratification: the classics were the OQ-214 calibration negatives, so
H2's re-confirmation on them is partially circular — fine for a demonstration row (row 2 is a
control, not a novel ruling), noted here as required.

## R6 parallel column — agreement report

Where a RULED verdict exists, the contract-vs-deficit sort AGREES with the separation verdict
in every case (rows 1, 2, 3, 4, 8; row 7 partial): **contract-violations name a finite token
surface and trend gateable exactly when that surface is protocol-unique; standard-deficits
trend reader-held/unspecifiable.** The alignment rests on 3–4 RULED rows, all in one
direction — consistent-with, not warrant-for.

## Escalated seat questions

1. **Row 7b arming** — after the fresh confirmation set, whether the narrowed lexicon arms
   (and at what severity) is the operator's.
2. **Row 11** — the OQ-185 (a)–(d) ruling; the partition holds BLOCKED-ON-SEAT until then.
3. **The two-distribution restatement** for build_discipline.md / design_discipline.md §11b —
   proposed above, flagged; the operator rules the doc edit.

## Footnotes (corpus hygiene + provenance)

- `treasure_island.md` retains its table of contents; its numeric count is partially
  front-matter page numbers. Even discounting it entirely, F1 stands on eighty_yard_run,
  philosophy_four, and rift3.
- Baseline-band footnote: neither empty-pan candidate lands exactly in the recorded 37.6–47.6
  band under `_word_count` (`the_empty_pan_rev1.md` = 50.57 @ 5,932 words;
  `the-empty-pan_rev1.md` = 30.66 @ 2,805). The 5,932-word file is evidently the ~6,100-word
  baseline story (word-counter drift explains the delta); the positive control fired either
  way. Flagged, not silently resolved.
- Row 8 control honesty: the detector surfaced 8 closers in the ergodicity story including 3
  "the way X" forms; the operator witnessed ×4. 8 ≥ 4 satisfies "the instrument surfaces the
  known instances," but the per-instance identity of the operator's 4 was not re-derived —
  recall against that specific list is asserted at ≥3/4, not 4/4.

## Closeout verification (added on operator review — the a6820230 WRITEUP lacked the gate paste)

Spot-check result, reported honestly: the gate output for commit `a6820230` was pasted in-chat
only, not in the committed WRITEUP. Fixed here: the closeout gate run below was executed after
the review edits (stop-rule accounting, 7b recall control, decay rider, R6 honesty sentence,
OQ-127 promotion) and immediately before the closeout commit.

**Two distinct claims, one witnessed, one not:** the paste below witnesses the substrate's
CURRENT state (post-review-edits, immediately pre-closeout-commit) — it does not retroactively
witness that `a6820230` was GREEN at its commit time. The a6820230-era gate ran in-session and
was not committed; that historical claim is permanently chat-only and is recorded here as
UNWITNESSED-in-substrate, not re-asserted.

Commit chain: `a823cd47` PREREG (alone) → `ad132911` ratification AMENDMENT (before Pass 2) →
`a6820230` writeup + ISSUES.md + index → `b02e089f` KNOWN_STATE F1 entry → closeout commit
(this file's HEAD).

```
# Gate checks
  ✓ issues_status    226 parsed, 0 malformed
  ✓ omega check      0 problems
  ✓ omega selftest   selftest: all positive controls fired (10/10)
  ✓ omega index      index --check: fresh (226 rows, 71 active / 155 archive)
  ✓ known_state      235 entries parsed, 0 problems
  ✓ axis boundary    [AXIS-SELFTEST] ALL PASS
  ✓ audit cites      ERRORS: 0
  ✓ gap surfaces     gap surfaces check: 3/3 human surfaces distinguish no_gap vs undetermined (self-test OK)
  ✓ cli selftest     cli selftest: OK (226 commands across 17 groups)

GATE: GREEN
```
