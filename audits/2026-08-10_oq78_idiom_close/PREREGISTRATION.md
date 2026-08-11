# PREREGISTRATION — OQ-78 idiom half (0.68 point-mass + .x8 rail)

**Frozen:** 2026-08-10, before any read of the (claimed_type × ε) joint for the test model.
**OQ:** OQ-78 (idiom half; the designed-quantization half closed working-as-designed 2026-06-12).
**Spend:** zero — observational read over corpora already on disk.

Everything below is pinned as **literal values** computed in the pre-freeze calibration pass
(`outputs/oq78_railband_calibration.json`). Nothing here may be re-derived after the test read.

---

## 1. What is being tested

The idiom half asserts two things no surface discloses: an in-bin **point mass at ε=0.68** and a
**`.x8` last-digit rail**. The question is whether they are a property of *the authoring model* or
of *the corpus/regime*. Condition (i) reads the rail **within** the claimed_type bands, so that a
rail explained entirely by band structure does not count.

**Dead premise, corrected.** OQ-78's pinned graduation was a zero-spend cross-arm read over
OQ-109 Phase C's 60-seed regen. That regen was **descoped to a 5-seed pilot** (OQ-109 resolved
2026-06-13, n corrected to 5). The graduation step no longer exists as written. This
pre-registration replaces it.

## 2. Populations (recon-determined, both checkpoints fired pre-freeze)

**Checkpoint 1 — sonnet-leg identity: FIRED (test side).** `testsets_sonnet` is 1001/1001
`claude-sonnet-5`, verified from in-file `story_provenance` and re-asserted by
`classify_corpus`'s single-model fingerprint refusal. Pre-committed branch applied: it moves to
the TEST side and calibration loses the leg.

**Checkpoint 2 — cross-leg seed matching: FIRED HIGH (matched corpus).** 957 ids common to all
four twin legs; pairwise 99.7–100% of the smaller leg. Spot-checked as **re-authoring, not
copying** (`animal_status_kernel__property_reading` is haiku-4.5 in `testsets`, sonnet-5 in
`testsets_sonnet`, different `human_readable`). The twin legs are one seed set re-authored per
model.

| Role | Population | n | Model (verified from data) |
|---|---|---|---|
| **TEST** | `testsets_sonnet` | 1001 | `claude-sonnet-5` |
| calibration | `testsets_haiku` | 960 | `claude-haiku-4-5-20251001` |
| calibration | `testsets_flash` | 960 | `gemini-2.5-flash` |
| calibration | `testsets_kimi` | 1005 | `kimi-k2.6` |
| calibration | default-leg derived sonnet-4.5 | 64 | `claude-sonnet-4-5-20250929` |
| calibration (kind-check) | default-leg authored sonnet-4.5 | 11 | `claude-sonnet-4-5-20250929` |
| calibration (independent 4th leg) | archive `kernel_v2_test2` | 60 | pre-reset regime |
| **secondary, never a pinned condition** | default-leg sonnet-5 not in the sonnet leg | 95 | `claude-sonnet-5` |

**Test-side overlap.** The default leg's sonnet-5 stratum is 100; 5 of those ids are also in
`testsets_sonnet` and are **ε-identical** (literal duplicates, listed in the evidence JSON). They
are removed from the secondary, leaving 95. The two sonnet-5 populations are otherwise disjoint
and are **never pooled** — they are different generation regimes (c-orchestrator topic runs vs
bulk kernel build), and pooling regimes inside the test stratum is the per-Author-never-pooled
failure one level down.

**Model-name trap.** The kimi twin leg is `kimi-k2.6`; the default leg's kimi stratum is
`kimi-k3`. **Different models — never pooled.**

## 3. Frozen inputs

| Input | Fingerprint | n |
|---|---|---|
| `prolog/testsets_haiku` | `f697246d3331b4528e6f1b2591ae5b5c` | 960 |
| `prolog/testsets_flash` | `6c6a2dbd832f33031441e286089e3dd6` | 960 |
| `prolog/testsets_kimi` | `57d485238b4c33bf604c896ff3ebcec7` | 1005 |
| `prolog/testsets_sonnet` | `2427448c1b3c7d6e4b607cb883d3918c` | 1001 |
| archive `kernel_v2_test2/json` | `707aaac6c92a56c4b06c0426bfc5d9c3` | 60 |
| `outputs/oq78_leg_haiku.json` | `360de25ed93dd311a02a2c7f60f27e9a` | — |
| `outputs/oq78_leg_flash.json` | `a0968158118f3b19d8122412ffc82b65` | — |
| `outputs/oq78_leg_kimi.json` | `39f475b8c40face2f6892cb373adf44e` | — |
| `outputs/oq78_leg_sonnet.json` | `4120ee707e83a69205e3b5a786b07055` | — |

All four leg outputs were produced by serialized `classify_corpus` at code
`820ba021172310171ec0c487bb3997cce3f305fb`, each with its own manifest.

**The default leg is frozen as a slim slice** (`evidence/pipeline_output.frozen.slim.json`,
manifest `2026-08-10T16:43:14Z`, n=249, code `820ba02`). It moved **243 → 249 mid-session** under
an operator topic run while the four twin-leg md5s stayed byte-identical throughout. The default
leg feeds null construction, so the audit reads the frozen copy, never the live file.

**Code state.** All five reads share one engine state: the only commit between the first read and
the freeze (`4bef6bd..820ba02`) adds three corpus stories and touches **zero engine files**.

## 4. Pinned definitions (literal values — not re-derivable after the read)

**Band grid** — the literal ε value set from **calibration strata + archive ONLY**; the test
stratum is excluded from null construction. 76 values:

```
0.0 0.01 0.02 0.03 0.04 0.05 0.06 0.08 0.1 0.12 0.15 0.16 0.18 0.19 0.2 0.22 0.23 0.25 0.28
0.3 0.31 0.32 0.35 0.36 0.37 0.38 0.4 0.41 0.42 0.44 0.45 0.46 0.48 0.5 0.51 0.52 0.53 0.54
0.55 0.56 0.57 0.58 0.6 0.61 0.62 0.63 0.64 0.65 0.66 0.67 0.68 0.7 0.71 0.72 0.73 0.74 0.75
0.76 0.77 0.78 0.79 0.8 0.81 0.82 0.83 0.84 0.85 0.86 0.87 0.88 0.89 0.9 0.91 0.92 0.95 0.96
```

Digit support = `{0..9}` (all ten), so **uniform expectation = 0.100**. Test mass outside the
grid counts as **non-rail** and is reported as `off_grid_share`.

**Localization statistic (unpaired).** Per stratum × claimed_type: argmax last digit over the
grid, and `concentration = share_at_argmax − 0.100`. An **effect size applied identically at
every n — never an α.**

**Floor = 0.25. Minimum scored cell n = 50.** Uniform-digit null concentration by cell size:

| n | 5 | 10 | 20 | 55 | 90 | 960 |
|---|---|---|---|---|---|---|
| null p50 | 0.300 | 0.200 | 0.100 | 0.064 | 0.056 | 0.016 |
| null p99 | 0.500 | 0.400 | 0.250 | 0.136 | 0.111 | 0.031 |

At n≤10 the null *median* exceeds a 0.10 floor — small cells fire on noise, which is why the
minimum scored cell is 50, not 5. **The floor was set to just admit the weakest true positive:
kimi's pooled concentration is 0.281, so 0.25 admits it with 0.03 of headroom.** It is not
comfortably clear of everything, and should not be read that way.

**MDE at floor 0.25** (smallest excess share above uniform detectable at power ≥ 0.80):
n=59 → 0.24; n=95 → 0.24; n=165 → 0.22; n=690 → 0.20; n=1001 → 0.20. Cells below n=50 are
excluded from scoring entirely and are not tabulated.

**Paired statistic (PRIMARY).** Over the four-way matched seed set:
`tv_model_digit` = mean over models of the total-variation distance between that model's last-digit
distribution and the pooled last-digit distribution. **Paired floor = 0.15** — a magnitude
pinned at >3× the label-permutation null maximum (0.045), and far below the three-leg calibration
observation (0.386). The paired floor is **not inherited** from the unpaired floor; paired and
unpaired nulls do not behave alike.

**Condition (ii) — PRECONDITION, not a falsifier.** Worst adjacent-pair AUC (probability of
superiority, tie-aware) over `{rope, tangled_rope, snare}` ordered by median ε, versus
**threshold 0.6347** = the archive's bootstrap 5th percentile (2000 draws; p05 0.6347, p10 0.6696,
p50 0.8196, p95 0.9233). It exists to confirm band structure has not collapsed so the rail reading
is interpretable. **Falsification sits with (i) and the paired contrast.**

> **Measure swap, declared.** The originally pinned p10–p90 interval-overlap measure was
> **vacuous**: it scored **1.0 on the comparator itself** (archive rope p90 = 0.68, dragged by the
> three kernel-reading exceptions ISSUES.md documents), so its bootstrap threshold calibrated to
> 1.0 and *every* banding passed. The p25–p75 variant is vacuous in the opposite direction (0.000
> on all four non-test legs). AUC is the only candidate that varies across legs, so it is the only
> one carrying information. Retained as a descriptive, never as the criterion.

**Condition (i).** Within-band test-stratum concentration at the `.x8` anchor stays **below 0.25**,
reported **per claimed_type and pooled**. Per-type is load-bearing: with tangled_rope at 690/1001
the pooled read is substantially a tangled-only test. The WRITEUP must say which type carried the
verdict.

**Conflict rule — pinned before any test datum is visible.** Where paired and unpaired disagree,
**the paired result governs the verdict**, and the disagreement is a **headline WRITEUP finding**,
never arbitrated once both are visible.

## 5. Controls, all discharged pre-freeze

| Control | Requirement | Result |
|---|---|---|
| Known-positive (sonnet-4.5 derived) | must localize digit 8 | **8**, share 0.766, conc 0.666 ✓ |
| Known-positive (archive, independent) | must localize digit 8 | **8**, share 0.767, conc 0.667 ✓ |
| Known-negative, unpaired | permuted digits must not fire | null p99 0.031, max 0.049 at n≈960 — silent ✓ |
| Known-negative, paired | model-label permutation must go silent | null p50 0.024, p99 0.038, **max 0.045** vs observed **0.386** ✓ |
| Kind-check (pinned criterion) | divergence = different argmax AND both above floor | authored 4.5 argmax **8** conc 0.718; derived 4.5 argmax **8** conc 0.666 → **same argmax = concordant, non-blocking** ✓ |
| Calibration ↔ archive id intersection | count into the prereg | **0 — fully disjoint**; the archive is an independent fourth calibration leg ✓ |
| Residue / selection check | matched-set dropout not concentrated by type | haiku 0, flash 0, kimi 45 residue; **scaffold over-represented in kimi's residue (15.6% vs 1.0%, delta 0.145)** — declared, see §8 |

**The archive is a stronger known-positive than assumed.** Two *independent* Claude-family
populations (disjoint id sets) land on the same digit at nearly the same concentration —
0.767/0.667 archive vs 0.766/0.666 sonnet-4.5 derived.

## 6. What the calibration pass already established (recorded BEFORE the test read)

**The rail is model-bound.** On 960 matched seeds, with topic and claimed_type mix held fixed by
construction:

| leg | argmax digit | share | concentration | share at 8 |
|---|---|---|---|---|
| haiku | **8** | 0.623 | 0.523 | 0.623 |
| flash | **5** | 0.624 | 0.524 | 0.029 |
| kimi | **2** | 0.380 | 0.280 | 0.273 |

`tv_model_digit` = **0.386** against a label-permutation null max of **0.045**. All-agree rate
**2.7%** — on matched seeds the three models almost never choose the same last digit.

**Banding is stable across models.** Worst-pair AUC: archive **0.813**, haiku 0.839, kimi 0.885,
flash 0.908. **All three twin legs band BETTER than the archive comparator — the comparator is the
weakest bander in the set, so the 0.6347 threshold is a LOW BAR and a pass must not be read as
"banding is strong."** Four independent models banding stably under the same feeding path is
positive evidence that the band half is **hypothesis-driven rather than model-mediated**, which
lowers the prior on outcome cells 3 and 4.

**Consequence for what the test read can do.** The model-bound finding is already established on
non-test data. The test read asks something narrower: **does sonnet-5 also have a locatable digit,
and where?**

## 7. Declared leak (both directions)

- **(a)** Per-stratum ε marginals are already observed, including sonnet-5's `.x8` plurality in
  the default leg ⇒ **(i) enters leaning toward failure.**
- **(b)** Test-stratum claimed_type **marginals** are observed — tangled_rope 690, rope 165,
  snare 59, mountain 34, piton 29, scaffold 24 ⇒ power inputs disclosed. **snare at 59 is the
  thinnest per-type leg** and barely clears the minimum scored cell of 50.
- **(c)** The comparator restriction is the archive's least-separated type set, and the wide
  anchor (mountain vs snare) is absent from it ⇒ **(ii) enters leaning toward pass** — reinforced
  by (ii) now being an explicit precondition.
- **Genuinely unobserved:** the **(claimed_type × ε) joint within sonnet-5**. Every condition is
  defined over that joint. The calibration phase is structurally blind to it — `--phase
  calibration` filters the test stratum out at load.

## 8. Declared residues

- **kimi matched-set selection.** 45 kimi stories fall outside the 960-way matched set, with
  scaffold over-represented (15.6% of residue vs 1.0% of matched, delta 0.145). The paired set's
  composition is therefore mildly selected against scaffold on the kimi arm. Scaffold is not in
  the comparator type set and is below the minimum scored cell on every leg, so it does not enter
  any pinned condition — but the selection is declared, not silent.
- **Small strata excluded from localization** (below n=50, listed never dropped): default-leg
  derived haiku (28), derived flash (11), authored sonnet-4.5 (11), derived sonnet-4 (7), authored
  kimi-k3 (5). Their pooled values are recorded as descriptives only.
- **Archive per-type localization is not licensed** (cells 9–23, all below 50). The archive enters
  as a pooled localization leg (n=60) and as the banding comparator.

## 9. Outcome table (pre-committed; five branches)

| Cell | (ii) precondition | (i) rail | Outcome |
|---|---|---|---|
| **1a** | holds | no in-band `.x8` concentration ≥ 0.25 | **CLOSE licensed (clean).** Idiom model-bound, closed on a measured contrast: sonnet-5 is a fourth relocation on top of flash→5 and kimi→2. |
| **1b** | holds | as 1a, but no powered relocation leg exists | **CLOSE licensed ON AN ABSENCE** — stated in the WRITEUP's one-line verdict and the OQ close line, not a footnote. *Expected unreachable:* flash and kimi are powered relocations at n≈960, established pre-freeze. If 1b is nonetheless reached, that itself is the finding. |
| **2** | holds | `.x8` concentration persists in-band ≥ 0.25 | **NO close on "model-bound" as stated.** Escalate with the family map complete. Framing corrected at freeze: haiku (8) and both sonnet-4.5 kinds (8) already sit on the rail while flash (5) and kimi (2) sit elsewhere, so **family-sharing is already the live hypothesis regardless of what sonnet-5 does** — the escalation is a well-posed ruling on close semantics ("Claude-family-bound idiom, relocating across families" vs held-open), not an open question about what the data mean. The matched-seed-across-models arm is NOT the remedy: it isolates the model, and the model is what already varied. |
| **3** | collapses | no concentration | **NO close.** Band link was model-mediated; the §6 four-leg banding-stability observation is contradicted; operator ruling. Prior lowered pre-freeze. |
| **4** | collapses | concentration persists | **NO close.** Rail route/regime-bound; the hypothesis-withholding arm becomes the live next step (spend-go to operator). Prior lowered pre-freeze. |

## 10. Riders carried into the WRITEUP

1. **Claim language.** This is **"matched-seed across models"** — seed held fixed, model varied,
   **feeding constant across all four legs**. It is **NOT** the withheld-arm design. The
   hypothesis-withholding falsifier (typed **Ω_E**) stays **unpurchased**; activation = cells 3/4.
   The phrase "matched-seed arm" must not appear.
2. **The 0.6347 bar is low**, and the comparator is the weakest bander in the set. Say so plainly.
3. **Name which claimed_type carried condition (i)**; snare (59) is the thinnest leg.
4. **The 95-story secondary is pooled-only** and never a pinned condition. Concordance is
   reassuring; discordance is a flag to escalate — it would mean "model-bound" is wrong as stated
   and the idiom is **regime-bound within model**, which is a caveat on the close's **scope**, not
   a condition failure. Neither outcome is decisive at n=95.
5. **Headline observables, not footnotes:** the type-vocabulary delta (mountain 34/1001 in the
   test stratum vs 9/60 in the archive; scaffold 24/1001 with no archive band — "banding persists"
   is tested on a partly different construct), and where sonnet-5's mass sits (a flattening
   between the archive's historical digits vs a rail absence — the localization statistic
   distinguishes these).
6. `institutional_trust_erosion_c0` (the mountain band-break on the rail) is **unfollowable in
   this stratum** — stated as such, not as "not promoted."

## 11. Execution order from here

1. Freeze this file; md5 it into the run log **above** the first sonnet-5 result line.
2. `--phase test` with the frozen values on the command line. The instrument **refuses to run**
   unless the on-disk md5 of this file matches the supplied `--prereg-md5`.
3. Read the cell table; write the WRITEUP; edit ISSUES.md per the fired cell only.
