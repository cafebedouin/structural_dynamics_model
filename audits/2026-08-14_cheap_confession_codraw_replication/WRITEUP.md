# Co-draw replication of the cheap_confession kernel + v4 closing repair-check

**Executed:** 2026-08-14
**OQ:** OQ-264 (same-input redraw stability), applied to `positional_disagreement_as_evidence`
**Verdict (scoped):** The standpoint×instrumentalist twinning **did not replicate** across
same-input co-draws — REPLICATED was unreachable after two draws. The closing finding of
`blog/2026-08/cheap-confession-v4.md` §6 is not supported at k=3 and must be rewritten.
**Fired:** live

**Corpus/manifest cite:** frozen SCOPE manifest
`agent/decompose_manifests/flat/cheap_confession_2026_20260814_151329.manifest.json`;
baseline = live corpus n=273 at code `efc8280c`. The three co-draws and the repair-check were
run-tagged (`--run-tag`), so no pipeline manifest was stamped and **no artifact from this audit
entered the live corpus**.

---

## 1. Primary: the ordinal claim

Pinned before the run (`PREREGISTRATION.md`, md5 `5065af53cc54798b91415adf68322fe9`):

> Of the 6 reading pairs, **exactly one** has non-zero overlap, and it is
> (standpoint_reading, instrumentalist_reading).

| pair | baseline | draw 01 | draw 02 | draw 03 |
|---|---|---|---|---|
| standpoint × instrumentalist | **0.270** (62/84) | **0.277** (62/81) | **0.270** (62/84) | **0.277** (62/81) |
| standpoint × proceduralist | 0.000 | **0.248** (62/94) | **0.412** (91/65) | — |
| instrumentalist × proceduralist | 0.000 | **1.000** (143/0) | **0.453** (91/55) | — |
| standpoint × pragmatist | 0.000 | 0.000 | 0.000 | 0.000 |
| pragmatist × instrumentalist | 0.000 | 0.000 | 0.000 | 0.000 |
| pragmatist × proceduralist | 0.000 | 0.000 | 0.000 | — |
| **ordinal claim holds?** | yes | **NO** (3 non-zero) | **NO** (3 non-zero) | uncountable |

**Draw 03 is uncountable, not a pass or a fail.** `proceduralist_reading` failed to generate, so
only 3 readings and 3 pairs exist; a claim quantified over 6 pairs cannot be evaluated on 3. Per
the prereg's null rule (uncountable ⇒ reduces k, never falsifies), k drops 3 → 2. *Noted for
completeness, not as evidence:* on its reduced pair set the shape did hold — standpoint ×
instrumentalist was the only non-zero of the three.

**Verdict: COLLAPSED at reduced k (0 of 2 countable draws).** The pinned COLLAPSED limb specified
0/3 and I have 0/2, one draw short of the pinned standard. The direction is nonetheless
unambiguous: **REPLICATED was already unreachable at 0/2**, since even a passing draw 03 would
have yielded 1/3 = OBSERVATION. The claim cannot reach its supporting limb on this evidence.

**What actually happens on redraw.** The standpoint × instrumentalist cell is strikingly *stable*
— agree=62 in all four runs, J ∈ {0.270, 0.277}. What is unstable is its **uniqueness**: in both
countable draws, proceduralist joined the overlapping set, once at J=1.000 (agreeing with
instrumentalist at all 143 comparable contexts). So the finding is not "standpoint twins with
instrumentalist" — it is "**standpoint twins with whichever readings happen to converge that
draw**," and in draw 01 the tightest twin in the set was a pair the frozen manifest declares
*axiomatically contradictory* (proceduralist↔instrumentalist, the run's one emitted contradiction).

**Secondary read is moot.** The prereg's non-`rope` residual partition existed to test whether a
*replicating* twinning was role-authored. Nothing replicated, so the question does not arise. Not
run; recorded as not-run rather than silently dropped.

## 2. Repair-check: did v4's closing repair change structure or label?

Pinned (`PREREGISTRATION_repair_check.md`, md5 `26601d72599ec2335f1b1ec798870615`), including the
declaration that **a non-firing `false_natural_law` carries no verdict** (uninformative by
construction — v4's text states outright that the claim is not a fact about nature).

**Churn interval, supplied by the co-draws as pinned** (`omega_production_cost_asymmetry`, same
frozen manifest, three redraws):

| field | draw 01 | draw 02 | draw 03 | interval | v4 closing | moved? |
|---|---|---|---|---|---|---|
| base_extractiveness | 0.15 | 0.61 | 0.15 | [0.15, 0.61] | 0.42 | inside — no |
| suppression_requirement | 0.10 | 0.35 | 0.05 | [0.05, 0.35] | 0.31 | inside — no |
| theater_ratio | 0.58 | 0.58 | 0.62 | [0.58, 0.62] | 0.48 | **outside — yes** |

1 of 3 fields moved. Pinned rule: ≤1 ⇒ **LEXICAL**.

**Verdict: LEXICAL** — the repair changed what the closing claims, not the structure it commits to.

**Declared limitation on that verdict: the test has low power on two of three fields.** The ε
interval [0.15, 0.61] spans nearly the entire authored range, so almost any value would read as
"unchanged"; same for suppression. Only `theater_ratio` had a usable noise floor (width 0.04), and
that field *did* move, downward. I am **not** re-reading the verdict on that basis — the rule was
pinned at ≥2 fields and I am honoring it — but the honest statement is that this instrument mostly
lacks the resolution to answer the question, and a future test should pre-register on
`theater_ratio` alone, which is the only field with a measurable floor.

**Design miss, reported not concealed.** The repair-check decompose treated v4's closing as a
*kernel* (`commitment_cost_location`, 3 readings + flat control), not the flat axis the baseline
was. No story is a like-for-like referent. I compared against `commitment_cost_location_flat_control`
on the principled ground that the baseline was itself a non-kernel flat axis — **but that mapping
was chosen after seeing the output**, which the prereg did not anticipate. All four stories are
recorded so the choice is inspectable:

| story | ε | suppression | theater |
|---|---|---|---|
| *flat_control (used)* | 0.42 | 0.31 | 0.48 |
| enforcement_deflation_reading | 0.62 | 0.20 | 0.78 |
| legibility_reading | 0.28 | 0.35 | 0.22 |
| temporal_identity_reading | 0.28 | 0.35 | 0.15 |

Note `enforcement_deflation_reading` at ε=0.62 / theater=0.78 — one reading of the *repaired*
closing is as extractive as the original and more theatrical.

## 3. Unplanned findings

**(a) Generation dropout is a base rate, not a fact about standpoint.** Across four runs of this
frozen manifest, three dropped a declared story: the original run lost `standpoint_reading`,
draw 02 lost `menu_curation_capture`, draw 03 lost `proceduralist_reading`. Draw 03 emitted its own
`cs_reading_relation_quarantine.json`, the same artifact the original run produced.

This **undercuts v4 §6's fourth bullet**, which narrows the standpoint dropout to a claim about
pipeline defaults ("reachable, not default") while still implying the dropout tracked that reading's
content. It does not. It hits a different story each run.

**(b) `false_natural_law` fires in 2 of 4 runs, and the mountain claim is adjudicated in only 3
of 4.** Measured, not inferred from thresholds. **CORRECTED 2026-08-15** — this section first
read "the mountain claim fails in 4 of 4," asserting a stable direction under a varying signature
name. Pulling each detector's own evidence term shows that is wrong:

| run | ε | signature | trigger term | adjudicates the mountain claim? |
|---|---|---|---|---|
| baseline | 0.61 | `false_natural_law` | `explicit_mountain_claim` | yes |
| draw 02 | 0.61 | `false_natural_law` | `explicit_mountain_claim` | yes |
| draw 03 | 0.15 | `false_summit_mountain` | `fsm_evidence(2,0,open)` | yes (summit limb) |
| draw 01 | 0.15 | `false_ci_rope` | **`low_extraction_profile`** | **no** |

Draw 01's detector fires because extraction is *low* — it reads the story as a coordination rope,
and the naturality claim is not what is being tested there. So the honest count is: the mountain
claim failed in **3 of 4**, on two different grounds, and the fourth run was not a test of it. The
verdict family is fully determined by the ε mode (§3c), so these are four runs across two
conditions, not four independent tests.

The original "4 of 4" was the same over-reach this audit was convened to catch — a stable-sounding
denominator assembled from runs that were not answering the same question — arriving one section
earlier than the finding it was written to report. Corrected in place rather than appended: the
table above is the evidence, `signature_detection:false_natural_law/2`, `false_summit_mountain/2`
and `false_ci_rope/2` queried per draw against `evidence/codraw_0N/`.

## 4. What this means for the essay

- **§6 bullet 5 and the closing section do not stand as written.** The twinning is not a property of
  the kernel. v4's existing hedge ("a suggestion with a named artifact channel, not a measurement")
  was correct and understated — the honest replacement is that it was tested and did not replicate.
- **§6 bullet 4 needs widening** from a standpoint-specific dropout to a base rate.
- **§6 bullet 1 does not survive** (revised 2026-08-15, per the §3b correction). It cannot be
  rescued by narrowing to "a stable direction under varying signature names" — there is no stable
  direction: one of the four runs does not adjudicate the naturality claim at all.

**Superseded 2026-08-15:** `cheap-confession-v6.md` retires §6 entirely, so none of the three
bullet-level repairs above are live edits any more. They are kept as the record of what the audit
found, not as outstanding work. What survived into v6 is the plain-language falsifier set; what
this audit killed is the twinning, which v6 no longer asserts.
- **"Who this is for"** loses the empirical support v4 gave it. Whether it reverts toward v3 or
  keeps the reform-capture argument on other grounds is the author's call, not this audit's — the
  reform-capture *question* remains authored in the corpus regardless of the twinning.

## 5. Limitations

- k=2 countable, not the pinned 3. A fourth draw would settle COLLAPSED vs OBSERVATION, though not
  the REPLICATED limb, which is already unreachable.
- The repair-check is n=1 and low-powered on 2 of 3 fields (§2).
- Both experiments hold the frozen manifest constant, so manifest-determined structure is
  *controlled for*, not measured.
- Seven constraints, all peripheral to the corpus, none in the giant component. Nothing generalizes.
- Read path validated before spend: `compare_kernel_readings/3` reproduced the report's
  `62/84/0.270` on the live corpus (n=273) and byte-identically on a 5-file isolate — two-sided,
  since the signature layer carries corpus-relative inputs that could have moved it.

## 6. Evidence map

| artifact | what it is |
|---|---|
| `PREREGISTRATION.md` | co-draw prereg, md5 `5065af53…`, written before draw 01 |
| `PREREGISTRATION_repair_check.md` | repair prereg, md5 `26601d72…`, written before the repair run |
| `evidence/codraw_01/`, `codraw_02/`, `codraw_03/` | the three co-draws, `.pl` + `json/`; irreproducible (stochastic generation) |
| `evidence/repaircheck/` | four stories generated from v4's closing |
| `evidence/v4_what_survives.md` | the exact 1.6 KB input to the repair-check |
| `evidence/codraw_run.log` | full co-draw run log incl. batch ids and dropouts |
| `evidence/repaircheck_run.log` | full repair-check run log |
