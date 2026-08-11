# OQ-78 — the `.x8` rail is FAMILY-bound, not model-bound: no close licensed (cell 2)

**Executed:** 2026-08-10
**OQ:** OQ-78 (idiom half; the designed-quantization half closed working-as-designed 2026-06-12)
**Verdict:** Condition (ii) holds and condition (i) **fails** — sonnet-5 concentrates on the
`.x8` rail *within* its claimed_type bands (pooled concentration 0.331 against a pre-committed
floor of 0.25), so **the pre-registered close is NOT licensed**; the pinned cell-2 branch fires.
What the pass *does* establish, on 957 matched seeds with topic held fixed, is that the rail is
**bound to the model family, not to the corpus**: four models, three distinct argmax digits
(Claude→8, kimi→2, flash→5), all-agree rate 1.25%. Scope caveat the body carries: this is
matched-seed **across models**, not the withheld-arm design — the hypothesis-withholding
falsifier (Ω_E) remains **unpurchased**.
**Substrate:** five corpus legs + one archive. Test = `testsets_sonnet` (n=1001,
`claude-sonnet-5`); calibration = `testsets_haiku` (960), `testsets_flash` (960),
`testsets_kimi` (1005), default-leg sonnet-4.5 strata (64 derived / 11 authored), archive
`kernel_v2_test2` (60). Leg outputs from serialized `classify_corpus` at code
`820ba021172310171ec0c487bb3997cce3f305fb`. Default leg frozen as a slim slice, manifest
`2026-08-10T16:43:14Z`, n_constraints=249, code_commit `820ba02`, code_dirty=true.
**Fired:** live — two pre-freeze controls fired and changed the design before any test datum was
read: condition (ii)'s pinned measure was **vacuous** (scored 1.0 on the comparator itself, so
every possible banding would have passed) and was replaced; and the pinned minimum cell size of 5
was shown to sit *below the uniform null median*, so small cells would have fired on noise. Both
would have produced a success-shaped close.
**Evidence map:**
- `PREREGISTRATION.md` — frozen design, md5 `384e68bbac80e0959dba1294a6f6ee87`; all literal
  values (band grid, floors, threshold) pinned pre-read.
- `RUN_LOG.md` — freeze block with the prereg md5 and corpus fingerprints, positioned
  **physically above** the first sonnet-5 result line; then the cell table and descriptives.
- `evidence/pipeline_output.frozen.slim.json` — frozen default-leg slice (the live file moved
  243→249 mid-session under an operator topic run).
- `evidence/oq78_railband_calibration.json` — calibration pass: family map, controls, band grid,
  bootstrap threshold, kind-check, residue check.
- `evidence/oq78_railband_test.json` — test pass: the (claimed_type × ε) joint, both conditions,
  paired primary, pooled secondary.
- `oq78_test.stderr.log` — test-run stderr.
- Instrument: `python/audits/oq78_railband_crosstab.py` (phase-gated; `--phase test` refuses to
  run unless the on-disk prereg md5 matches).

---

## 1. Result

| | measure | value | criterion | outcome |
|---|---|---|---|---|
| **(ii)** precondition | worst adjacent-pair AUC over {rope, tangled_rope, snare} | **0.8858** | ≥ 0.6347 | **HOLDS** |
| **(i)** rail, pooled | concentration at argmax digit **8** | **0.3310** (share 0.4310) | < 0.25 to satisfy | **FAILS** |
| **(i)** rail, tangled_rope (n=690) | argmax **8** | 0.3161 | < 0.25 | fails |
| **(i)** rail, rope (n=165) | argmax **8** | 0.4273 | < 0.25 | fails |
| **(i)** rail, snare (n=59) | argmax **1** | 0.3386 (share at 8 = 0.281) | < 0.25 | fails |
| **paired** primary | `tv_model_digit`, 4-way matched n=957 | **0.3654** | ≥ 0.15 fires | **fires**, model-bound |

**Which type carried the verdict.** Not one — condition (i) fails on all three scored types
independently. It is *not* a tangled-only artifact despite tangled_rope holding 690/1001. The
strongest single cell is **rope** (0.427), not the dominant tangled_rope (0.316). **snare (n=59)
is the thinnest scored leg**, barely clearing the minimum scored cell of 50, and it is the one
type whose argmax is *not* 8 — it localizes at digit **1** while still carrying 28.1% at 8.
Cells below n=50 were excluded and are listed, never dropped silently: scaffold 24, mountain 34,
piton 29.

**Paired and unpaired agree.** The pinned conflict rule (paired governs, disagreement is a
headline finding) was **not triggered** — both read sonnet-5 as sitting on 8. That agreement is
itself informative: the composition confound the paired design exists to remove was not biting
here.

**Cross-regime secondary (pooled only, n=95, never a pinned condition).** Default-leg sonnet-5 —
a different generation regime (c-orchestrator topic runs) from the same model — localizes at
**8**, concentration 0.342, **concordant** with the test stratum's 0.331. So the digit tracks the
*model*, not the *regime within the model*. Per the pinned interpretation rule, concordance here
is reassuring rather than decisive at this n.

## 2. The finding the calibration pass produced, before the test read

Recorded in the prereg §6 so it could not be reconstructed afterward. On 957 matched seeds —
one seed set re-authored per model, so topic and claimed_type mix are held fixed by construction:

| model | argmax digit | share | concentration | share at 8 |
|---|---|---|---|---|
| haiku | **8** | 0.625 | 0.525 | 0.625 |
| flash | **5** | 0.623 | 0.523 | 0.029 |
| kimi | **2** | 0.380 | 0.280 | 0.273 |
| sonnet-5 | **8** | 0.427 | 0.327 | 0.427 |

`tv_model_digit` = 0.365 against a label-permutation null max of 0.045 (p99 0.038). **All-agree
rate 1.25%** — on matched seeds the four models almost never choose the same last digit.

**So the rail is model-dependent, but it partitions by FAMILY, not by model.** Every
Claude-family population measured lands on 8 — sonnet-5 (0.427), haiku (0.625), sonnet-4.5
derived (0.766), sonnet-4.5 authored (0.818), sonnet-4 (0.857), and the pre-reset archive
(0.767). Two of those are *independent* populations with a **zero-story id intersection**
(archive vs sonnet-4.5 derived) landing on the same digit at nearly the same concentration. Only
flash leaves the rail entirely; kimi sits on the rail's `.x2` arm.

This is why cell 2 is the honest branch and not a disappointment: condition (i) was pinned to ask
"is the `.x8` rail sonnet-5's own?", and the answer is no — it is the Claude family's, which
sonnet-5 inherits. The idiom did not fail to be model-bound; it is bound at a coarser grain than
the condition was written to detect.

## 3. The idiom half splits: the point mass diluted, the rail held

Post-hoc descriptive, **not a pinned condition** — but it answers OQ-78's own tracking question
(a) directly ("does 0.68's share fall toward a spread, or hold?").

| population | n | `.x8` | `.x2` | `.x8`+`.x2` | ε=0.68 | distinct ε |
|---|---|---|---|---|---|---|
| archive `kernel_v2_test2` | 60 | 76.7% | 15.0% | 91.7% | **30.0%** | **13** |
| default derived sonnet-4.5 | 64 | 76.6% | 20.3% | 96.9% | **50.0%** | **12** |
| leg haiku | 960 | 62.3% | 19.8% | 82.1% | 31.8% | 42 |
| **leg sonnet-5 (TEST)** | 1001 | 42.8% | 35.5% | **78.2%** | **7.3%** | **52** |
| leg kimi | 1005 | 27.7% | 38.1% | 65.8% | 5.3% | 68 |
| leg flash | 960 | 2.9% | 0.5% | **3.4%** | 1.6% | 30 |

Historical baselines (ISSUES.md OQ-78): n=91 build — `.x8` 86%, ε=0.68 34%, 13 distinct; n=60
archive — `.x8` 77%, ε=0.68 30%, 13 distinct.

**The two components of the idiom half came apart.** The **0.68 point mass has largely
dissolved** in the post-reset corpora — 30% (archive) → **7.3%** (sonnet-5), with distinct ε
values quadrupling from 13 → 52. The **`.x8`/`.x2` rail held**: 91.7% → 78.2%. What changed
inside the rail is the *split between its arms* — sonnet-5 moved mass from `.x8` toward `.x2`
(76.7/15.0 → 42.8/35.5). So sonnet-5's marginal is a **flattening within the rail**, not a rail
absence; the localization statistic distinguishes these and reads flattening.

flash is the only genuine departure from the rail (3.4% combined), consistent with the standing
readout's `.x5`/`.x0` note.

## 4. Banding, and why a pass here is a low bar

Worst adjacent-pair AUC: **archive 0.813**, haiku 0.839, kimi 0.885, sonnet-5 **0.886**, flash
0.908. **All four twin legs band better than the archive comparator — the comparator is the
weakest bander in the set**, so the 0.6347 threshold is a low bar and this pass must **not** be
read as "banding is strong." It says only that band structure has not collapsed, which is what a
precondition is for.

Five independent models banding stably under the same feeding path is positive evidence that the
band half is **hypothesis-driven rather than model-mediated** — which is why outcome cells 3 and
4 had their priors lowered before the read, and neither fired.

**Type-vocabulary delta — a headline, not a footnote.** "Banding persists" is tested on a partly
different construct:

| claimed_type | sonnet-5 (n=1001) | archive (n=60) |
|---|---|---|
| tangled_rope | 690 (68.9%) | 23 (38.3%) |
| rope | 165 (16.5%) | 10 (16.7%) |
| snare | 59 (5.9%) | 17 (28.3%) |
| mountain | 34 (3.4%) | 9 (15.0%) |
| piton | 29 (2.9%) | 1 (1.7%) |
| scaffold | 24 (2.4%) | **0 (0.0%)** |

The regime shifted hard toward tangled_rope and away from snare and mountain, and scaffold exists
in the test stratum with no archive band at all. The delta is itself a regime finding.

**`institutional_trust_erosion_c0` is unfollowable in this stratum.** The mountain band-break on
the rail (claim=mountain, ε=0.68) is present in the frozen default leg, but mountain in the test
stratum is n=34 — **below the minimum scored cell of 50**, so per-type localization for mountain
is not licensed here. This is stated as unfollowable, not as "not promoted."

## 5. Two instrument defects caught pre-freeze

Both would have produced a success-shaped close, and both are Build Discipline Pattern 5 (a gate
passing because its input is degenerate rather than because a condition was checked).

**(a) Condition (ii)'s pinned measure was vacuous.** The p10–p90 interval-overlap measure scored
**1.0 on the comparator itself** — archive rope's p90 is 0.68, dragged by the three kernel-reading
exceptions ISSUES.md already documents — so its bootstrap threshold calibrated to 1.0 and *every
possible banding would have passed*. The p25–p75 variant is vacuous in the opposite direction
(0.000 on all four non-test legs — no variance to threshold). Worst-pair AUC is the only
candidate that varies across legs (0.813 / 0.839 / 0.885 / 0.908), so it is the only one carrying
information. The retired measure is kept as a descriptive, never as a criterion.

**(b) The pinned minimum cell size of 5 could not clear any defensible floor.** Uniform-digit
null concentration is p50 **0.300** at n=5 and **0.200** at n=10 — a 0.10 floor sits *below the
null median*, so small cells fire on noise. Minimum scored cell raised to 50, where null p99 ≤
0.136. The floor was set to **just admit the weakest true positive** (kimi at 0.281, so 0.25
leaves 0.03 of headroom) — it is not comfortably clear of everything and should not be read that
way.

## 6. Declared residues and unpurchased falsifiers

- **The withheld-arm falsifier is NOT discharged.** This design holds the seed fixed and varies
  the **model**, with input feeding constant across all four legs. That removes the
  topic/claimed_type composition confound by construction — which is exactly what an unpaired
  per-stratum statistic cannot do — and nothing more. It is **"matched-seed across models," never
  a "matched-seed arm."** The hypothesis-withholding contrast stays **unpurchased**, typed
  **Ω_E**, activation = cells 3/4 (neither fired).
- **kimi matched-set selection.** 45 kimi stories fall outside the matched set, with scaffold
  over-represented (15.6% of residue vs 1.0% of matched, delta 0.145). Scaffold is not in the
  comparator type set and is below the minimum scored cell on every leg, so it enters no pinned
  condition — declared, not silent. haiku and flash residue is 0.
- **Small strata excluded from localization**, listed: default-leg derived haiku (28), derived
  flash (11), authored sonnet-4.5 (11), derived sonnet-4 (7), authored kimi-k3 (5). Pooled values
  recorded as descriptives only. Archive per-type localization is not licensed (cells 9–23).
- **Model-name trap.** The kimi twin leg is `kimi-k2.6`; the default leg's kimi stratum is
  `kimi-k3`. Different models, never pooled.
- **The paired statistic's join basis is a filename convention, not an authored identity
  (declared GAP-35).** The 957-way matched set is a `constraint_id` equality join. The project's
  standing rule is that names are not identity across a regeneration boundary; `cs_story_uid`, the
  field that looks like it should serve, **deliberately does not join** (0/956 across twins), and
  `seeded_from` is not emitted. The pairing is real for the twin legs — they were built by
  re-authoring one seed set, and spot-checks show same subject with different `human_readable` —
  but it rests on convention plus spot-check rather than on a field, and that is a limitation of
  the instrument, not a defect in the result. The kernel-level join (`cs_kernel_id`, 331/331 across
  twins) does not generalize either: 0 shared with default-leg sonnet-4.5, and the archive has no
  kernel ids at all.
- **Corpus motion.** The default leg moved 243 → 249 mid-session under an operator topic run
  while all four twin-leg md5s stayed byte-identical. The default leg feeds null construction, so
  it was frozen as a slim slice; the audit reads that copy, never the live file.

## 7. What this leaves open (the cell-2 escalation)

The pre-committed cell-2 branch is **escalate with the family map complete** — and the map *is*
complete, so this is a well-posed ruling on close semantics, not an open question about what the
data mean:

> **Is "the `.x8` rail is a Claude-family idiom, relocating across model families" a close of
> OQ-78's idiom half, or does the half stay open?**

Evidence for treating it as closeable: the rail's model-dependence is measured, not inferred —
four models, three digits, on matched seeds, with both known-negatives silent. Evidence for
holding it open: condition (i) as pinned was the falsifier, and it failed; the point-mass and
rail components have now demonstrably come apart (§3), which is a *new* two-part structure the
original OQ framed as one thing.

A zero-spend read that would sharpen the ruling: measure the rail on **additional Claude-family
models** to test whether "family-bound" survives contact with a Claude model that is *not* on 8 —
the current family map has no such counterexample, and its absence is what makes family-sharing
un-falsified rather than confirmed.

**Not the remedy:** the matched-seed-across-models arm. It isolates the model, and the model is
what already varied.

---

## ADDENDUM (same day, operator-directed) — the rail's arm structure

**Status: EXPLORATORY characterization, no falsifier attached.** Run after the cell-2 read, on
data already on disk, to make the re-scoped rail question concrete before anything is
pre-registered against it. Instrument: `python/audits/oq78_rail_arm_structure.py`; evidence:
`evidence/oq78_rail_arm_structure.json`. Nothing here is a pinned condition.

The rail held while the point mass dissolved, and mass moved *between* the rail's arms. Three
readings were separable on data in hand: **R1** one habit, seed-driven; **R2** two independent
per-model habits; **R3** the `.x2` arm is just the vacated 0.68 point mass spreading to its
neighbours.

**Arm split within the rail (`.x8` share):** archive 83.6%, sonnet-4.5 79.0%, haiku 75.9%,
sonnet-5 **54.7%**, kimi 42.1%. (flash is on the rail at only 3.4%, so its 84.9% is 28 stories
and carries nothing.)

**R3 is falsified where it mattered most.** If `.x2` were the dissolved point mass spreading, its
gain would sit at 0.62/0.72. It does for kimi (61.4% adjacent) and haiku (54.7%) — but for
**sonnet-5 only 14.1%**: its `.x2` mass sits at **0.42 (175 stories) and 0.52 (70)**, a different
region entirely. The model whose point mass dissolved most is precisely the one whose `.x2` growth
is *not* the vacated neighbourhood. The two components are independent, not one artefact.

**R1 vs R2 — and the type confound partialled out.** Arm choice tracks claimed_type, and two
models on one seed often agree on the type, so raw concordance could be type agreement in a shared
habit's costume. Restricting to seeds where both models assigned the **same** claimed_type and
shuffling **within** that type removes the channel:

| pair | n (same type, both on rail) | observed | within-type null p50 | null p99 | excess | above p99 |
|---|---|---|---|---|---|---|
| haiku × sonnet-5 | 547 | 0.5649 | 0.5101 | 0.5539 | **+0.0548** | **yes** |
| haiku × kimi | 434 | 0.4793 | 0.4516 | 0.4977 | +0.0276 | no |
| kimi × sonnet-5 | 424 | 0.4906 | 0.4906 | 0.5425 | **+0.0000** | no |

**Within the Claude family, arm choice carries genuine seed-level signal that survives the type
partial; across families it is exactly zero.** So the rail behaves as **one inherited habit applied
per story within the family** (R1), not two independent co-occurring habits (R2) — while remaining
wholly absent across families. The magnitude is **modest**: 56.5% against a 51.0% baseline, real
but a 5.5-point excess, and it should not be reported as a strong effect.

**What this changes for the re-scoped question.** "Family-bound" is no longer only a statement
about *which digit* each family lands on; there is within-family shared structure at the finer
arm level that no cross-family pair shows. That is a sharper object to test a family boundary
against than the argmax digit alone, and it is measurable on corpora already on disk.
