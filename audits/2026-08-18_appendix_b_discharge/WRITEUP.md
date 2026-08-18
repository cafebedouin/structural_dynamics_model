# OQ-309 — Appendix B discharged, and the manifest row that certifies the paper's numbers was itself an uncertified number

**Executed:** 2026-08-18
**OQ:** OQ-309 (minted before the pass, because the pass moves the issues-status row it must then quote)
**Verdict:** Both items the paper names as its circulation blocker are discharged — every Appendix B
row re-run under one as-of stamp, and the 35-item `V04_CONSOLIDATION_MANIFEST.md` pass completed with
its one U-BLOCKING item resolved — but the discharge produced **two new bounds on §5.4's central
figure, not just a new value**: the census's precision direction is now measured (13/83 hygiene-only,
a floor over one mechanism) and the pooled rate is a mixture over a **non-stationary** monthly series
(36.7% → 57.8% → 80.0%), so the figure may not be compared across time. The circulation GO itself is
the operator's and is not settled here.
**Substrate:** no pipeline run. Docs and audit apparatus only. Census frame frozen at
`frame/frame_manifest.txt` — repo commit `c1c3ef77`, dirty yes, 187 dirs on disk, 185 in frame,
83 numerator, `/usr/bin/grep` (GNU 3.11) pinned.
**Fired:** live — the audit-directory row's stated command was shown never to have produced its
stated value (174 vs 175); the incidence figure moved 42% → 45% and gained a fifth instrument defect
plus a non-stationarity bound; the V04 manifest's own item-count self-check was found reading 32
against a documented 35; four current-value claims in the paper's prose were found stale after the
table had been refreshed; and the promotion-test trial reproduced its predicted silent mistake on a
live draw.

## Evidence map

| artifact | what it is | what it witnesses |
|---|---|---|
| `crosswalk_v04_to_v06.md` | all 35 V04 rows adjudicated against v0.6 by **anchor text**, with a scope note bounding the NOT-LANDED verdicts | the V04 pass; item 17's discharge |
| `EVIDENCE_v04_gap5_duplicate.md` | the duplicate §7 gap-"5", assembled **before** any edit: both blocks verbatim, the git diff showing the second-reader commit inserted above and never touched the old block, and the three dispositions | the operator's gap-5 ruling rests on evidence, not on an instance's read |
| `PREREGISTRATION.md` (md5 `5f5785ba`) | the §8.2 trial's design, rubric and pre-committed readings, frozen before any call | the trial is a test, not a demonstration |
| `TRIAL_promotion_test.md` | the trial's result **and the incident in which its own re-scoring harness destroyed the raw data** | the §8.2 `[UNWITNESSED]` slot; an introduced-instrument defect |
| `frame/freeze_frame.sh` | the census frame, re-pointed from the 08-10 original, self-excluding both arc dirs, `/usr/bin/grep` pinned, denominator computed **both ways** | the new 83/185 and the denominator-rule delta of 0 |
| `frame/control_frame_command.py` | the 08-10 census control, re-run verbatim | **exit 0 — the precondition for citing any census figure** |
| `frame/precision_probe.py` | the hygiene-only false-positive probe, two-sided (5 fire / 8 decline, all corpus-drawn) | instrument defect #5 |
| `frame/v04_section7_check.sh` | §7 block numbers sequential-from-1 | the check the manifest's own self-checks structurally cannot perform |
| `frame/promotion_test_trial.py` | the trial driver, now entry-point guarded, AMENDMENT 1 declared in source | reproducibility of the trial (not of its value) |
| `frame/score_only.py` | non-spending re-scorer duplicating the rubric regexes | re-scoring can no longer make an API call |
| `frame/*.txt`, `frame/frame_manifest.txt` | frozen listings + md5s | the frame is pinned, not recomputed at read time |
| `evidence/census_control.txt` | the control's run | exit 0, 6/6 fixtures |
| `evidence/census_frame_run.txt` | the frame run | 83/185 both ways |
| `evidence/frame_delta_attribution.txt` | +11 dirs, 0 removed, 10 into the numerator, 0 lost | the movement is growth, fully attributed |
| `evidence/census_precision_probe.txt` | 13/83 hygiene-only, with the control | instrument defect #5 |
| `evidence/census_precision_timeseries.txt` | monthly incidence and monthly hygiene share | the non-stationarity finding, and that hygiene does not carry it |
| `evidence/appendixB_rows_scale.txt` | raw output for every §1.1/§1.2 row | the re-dated scale rows |
| `evidence/appendixB_command_validation.txt` | the new Python line-count command replayed at the 08-10 commit | prove-before-replace on a command that did not exist |
| `evidence/appendixB_rows_51_85_102.txt` | §5.1, §8.5, §10.2 rows | those rows |
| `evidence/appendixB_row_54_substrata.txt` | 4/12 of 102, with a two-sided control | the §5.4 sub-row |
| `evidence/appendixB_row_85_memory.txt` | the **name-level** memory diff replacing a balancing arithmetic | a discrimination check where a consistency check stood |
| `evidence/appendixB_row_104_cadence.txt` | cadence both ways, with a positive control on the exclusion | the §10.4 row, and the no-op-filter defect |
| `evidence/appendixB_rows_104_74.txt` | the first cadence run | superseded by the above; retained because it contains the no-op-filter defect verbatim |
| `evidence/crosswalk_absence_probes.txt` | absence probes with two-sided controls | the NOT-LANDED verdicts |
| `evidence/v04_section7_discrimination_record.txt` | the §7 check fires at the defective commit, declines after | the check discriminates, on naturally-arising states |
| `evidence/sync_sweep_witness.txt` | all 38 sweep hits assigned to a declared class | **Class C was not empty** — four stale prose claims |
| `evidence/promotion_test_prompt_*.txt`, `evidence/promotion_test_response_*_draw{1,2}.txt`, `evidence/promotion_test_scores.txt` | the trial's raw inputs and outputs; **draw 1's responses are transcript-restored, not file-persisted** | the trial result and the incident |
| `evidence/new_numerator_hits_raw.txt` | every keyword hit in the 10 new numerator dirs | the hand-scoring behind the contamination read |

## What the pass found that it was not looking for

**1. A manifest row can pair a value with a command that does not produce it.** The audit-directory
row read **174** and its command returned **175**. 174 was the *census frame* after self-excluding an
in-progress arc directory — a different quantity, merged into one cell. A manifest exists to be the
pairing of a value with its command; this row had quietly stopped being one, and no amount of
re-reading would show it. Only running the command did.

**2. The census has a measured false-positive rate, and its mechanism is our own prose.** The keyword
proxy counts directories whose only hit is the contrastive hygiene form *"recorded rather than
silently deleted"* — an author describing their own discipline, not a defect they found. **13 of 83
(15.7%)**, a floor over one mechanism; a second mode (bare *"was never"*) is visible and unmeasured.
The probe's own two-sided control caught its first version firing on a genuine defect report.

**3. The bigger finding is not the movement — it is that the figure has a slope.** June 36.7%, July
57.8%, August 1–18 80.0%. A pooled value over a rate that more than doubles moves when the
denominator's age distribution shifts, with no correction and no change in the world. Two readings
(genuine phase change; lexicon adoption) are stated and neither is picked — the hygiene probe bounds
one channel of the second and does not carry it (monthly hygiene share 19.4 / 7.7 / 25.0, no trend).
This also puts a **second instrument** on §10.4's three-way fork, favouring *change of project phase*
without settling it.

**4. A self-check can be minted, published, instructed-to-be-re-run, and still never compared.** The
V04 manifest's item-count check read **32 against a documented 35** from the moment §4b was added,
because three `‡`-marked rows do not match its regex. Not this pass's defect; found by running it.
The instruction *"re-run after ANY edit"* existed; the instruction *"and compare the output to 35"*
did not, and that is the whole difference.

**5. Three instruments this pass introduced committed the defect they were introduced to catch.**
A line-oriented absence probe scored a *present* control phrase absent (the paper is hard-wrapped).
A cadence exclusion filter matched nothing, for a trailing-slash mismatch, and printed a plausible
number identical to the unfiltered one. And the promotion-trial re-scorer, run by **importing** the
driver, executed its module-level `sys.exit(main())`, made two fresh API calls, and **overwrote the
raw responses the driver had persisted precisely so re-scoring would never need a re-run.** The
persist-before-parse rule protects the write path and says nothing about who may import the writer.
Full account and fixes: `TRIAL_promotion_test.md`.

## The §8.2 trial, in one paragraph

Handing a fresh instance the files a promoted tripwire names, with the tripwire withheld, produced
the predicted silent mistake on **1 of 2** draws; with the tripwire supplied, **0 of 2**. n = 2 per
arm is an **existence witness that the promotion test is runnable**, not a rate. The sharpest datum
is not the score: the failing draw emitted the defective goal and then explained, in the next
paragraph, exactly why it was defective — the knowledge was present and did not reach the artifact.
That is a *different* failure from the one the tripwire is written against, and it is why the test
must be scored on outcome rather than on whether the instance appears to know. **§8.2's missing
stakes term is untouched and remains a declared gap.**

## Residue — what a cold reader must pick up

- **OQ-309 is `partial`, not resolved**, and is the declared home for: the 10 still-open V04 rows
  (8, 9, 10, 11, 19, 23, 28 and the open halves of 14, 22, 31); the four rows still `[UNWITNESSED]`
  with blockers re-verified; and two operator decisions.
- **Operator decisions, neither of which an instance may make (§9.2):** the **circulation GO** —
  recorded as `blocked_on_human` on OQ-309, the factual half discharged and the judgment not — and
  the **§8.2 stakes-term** question (Ω_P: whether the gap is worth pricing).
- **Explicitly NOT discharged:** §10.4's standing-gate catch series (`:1893` region; the paper cites
  it as RQ-c and RQ4's method also reaches it). Nobody has collected it. Refreshing the cadence row
  is a different quantity and does not touch it.
- **Substrate changes:** `docs/amnesiac_institution/amnesiac_institution_v0_6.md` (status line,
  circulation-blocker box, a dated in-place-corrections block, §0 W2, §1.1, §2.A, §5.4, §6.2, §10.2,
  §10.4, §10.5, Appendix B); `docs/amnesiac_institution/V04_CONSOLIDATION_MANIFEST.md` (§7 gap-5
  folded-and-deleted per operator ruling; self-checks repaired; dated landing-status note);
  `ISSUES.md` OQ-309; `KNOWN_STATE.md`; `audits/README.md` index row.
- **A pre-existing gate red, untouched:** `gap surfaces` fails under `scripts/gate.sh` because the
  script calls bare `python3` and the system interpreter has no `pandas`. Run under the project
  virtualenv's interpreter instead, the check passes (3/3 human surfaces, self-test OK). Unrelated
  to this pass; changing how the gate resolves its interpreter is not this pass's call, and it is
  already recorded as a tripwire at KNOWN_STATE 2026-08-18.
  *(The first draft of this bullet wrote the interpreter as a literal path, which the `audit cites`
  gate correctly read as a citation of an untracked file and turned RED. An interpreter is not
  evidence; the gate was right and the prose was wrong.)*
