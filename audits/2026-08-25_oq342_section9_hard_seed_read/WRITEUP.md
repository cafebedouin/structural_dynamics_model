# OQ-342 §9 — the hard-seed read: are a model's hard seeds structurally different stories?

**Executed:** 2026-08-25
**OQ:** OQ-342 (the umbrella's own open deliverable — the §9 write-up), feeding OQ-378
**Fired:** live

**Verdict, at its scoped altitude:** on the two legs carrying an occupied `+rescue1` stratum
(`testsets_nemotron` 144, `testsets_stealth` 36), a model's hard seeds are **not structurally
different stories** — their per-seat type-share vector sits *closer* to the first-pass stratum
than two random first-pass halves sit to each other (**R = 0.599 / 0.582** against each leg's own
size-matched, **variance-matched** null; 3/1000 draws exceeded T95 on each — see *Which R is the
reported figure*). This is a **tested absence**, not a
failure to look: the same instrument, at the same m and computed the same way, resolves a
known-real between-model difference at 1.91 against a T95 of 1.11. Scoped to the seat-type
distribution on these two legs at `a3966e7`; it is not a claim that rescue and first-pass stories
are indistinguishable on every axis.

**Manifest cite:** the coherent 19-leg set at `a3966e7`, `code_dirty: False` —
`pipeline_output.nemotron.json` (n_stories 1000, run 2026-08-25T09:03:25Z),
`.stealth.json` (1005, 10:02:57Z), `.haiku.json` (960, 08:26:37Z),
`.flash.json` (960, 07:33:08Z). Strata read from the legs' own `.pl` files, not the JSON.

## Evidence map

| artifact | what it holds |
|---|---|
| `PREREGISTRATION.md` | design frozen before the run (md5 `1989b5536578a4e7ce2503baacbfd4ad`): vocabulary, primary statistic, C1/C2/C3, the outcome table, and the two declared deviations |
| `arm_b_hard_seed.py` | the instrument — strata via `PROV_RE` field 5, per-seat type-share L1, C2 null, C1 arms, C3 hypergeometric, descriptives |
| `arm_b_results.json` | its full output: manifests, strata, four arms, MDE anchor, C3, 8 descriptive blocks, verdict |
| `arm_b_variance_matched_null.py` / `.json` | the post-hoc sensitivity check on C2's noise asymmetry (declared as post-hoc) |
| `arm_a_pinned_arms.md` | Arm A, the pinned-arm tripwire (one paragraph, as specified) |

---

## Arm A — the pinned-arm tripwire

See `arm_a_pinned_arms.md`. **Pass.** Full detail there; one line here: the measurement body of
`backfill_diff.py` on haiku and flash is byte-identical (md5 `c4a669d5…` / `5a677f24…`) to the
committed `backfill_diff_{haiku,flash}_2026-08-22.txt`, so the preserved arms are pinned. The
whole-file comparison differs by exactly one line — an `AFTER arm: <path>` provenance header the
script gained in commit `a3966e7c6`, the freeze commit that preserved the arms. That is an
instrument change with a git witness, not a pinning failure.

## Arm B — the hard-seed question, measured

§9 asks whether a model's hard seeds are structurally different stories or just mis-authored
ones. The stakeholder-backfill stratum does not answer it — those stories were authored fine, they
merely predate the `stakeholders` gate. The stratum that answers it is `+rescue1`
(`testsets_nemotron` 144, `testsets_stealth` 36), and it had not been contrasted with anything.

**Tag discipline (RULING-1).** Arm B slices the **occupied `+rescue1`** stratum —
`testsets_nemotron` **144**, `testsets_stealth` **36** — and NOT `+seed_rescue1`
(`testsets_nemotron` **4**, `testsets_nemotron_think` **2**), a distinct generation event one
token away. Occupancy re-verified live at execution from the legs' own `.pl` files. Every stratum
present per leg is enumerated; no binary rescue-vs-rest cut was taken:

| leg | strata (all of them) | total |
|---|---|---|
| `testsets_nemotron` | `no_scope_rebuild_nemotron` 852 · `+rescue1` **144** · `+seed_rescue1` 4 | 1000 |
| `testsets_nemotron_think` | `no_scope_rebuild_nemotron_think` 1003 · `+seed_rescue1` 2 | 1005 |
| `testsets_stealth` | `no_scope_rebuild_stealth` 969 · `+rescue1` **36** | 1005 |
| `testsets_haiku` | `no_scope_rebuild` 505 · `+stakeholder_backfill` 455 | 960 |
| `testsets_flash` | `no_scope_rebuild_gemini` 754 · `+stakeholder_backfill` 206 | 960 |

Strata come from `audits.leg_diagnostic_table.PROV_RE` field 5 over the multi-line
`story_provenance/8` term, never a leg-name-derived grep — both documented tripwires are live on
this data (Flash carries `no_scope_rebuild_gemini*`; haiku's June originals carry bare
`no_scope_rebuild`), and either would have returned empty, reading as "no strata found".

### The arms

Primary statistic: **L1 between per-seat type-share vectors** (4 seats × 8 pre-registered types,
zero-filled; range [0,8]). R = observed mean L1 ÷ that leg's own size-matched T95.

**R below is the VARIANCE-MATCHED figure** — the null built with the same noise structure as its
own observed arm. The pre-registered (noise-asymmetric) R is carried alongside it, never alone; see
*Which R is the reported figure*.

| arm | m | n_target | n_firstpass | observed mean L1 | T95 (var-matched) | **R (var-matched)** | R (pre-reg) | draws > T95 |
|---|---|---|---|---|---|---|---|---|
| **nemotron `+rescue1`** (primary) | 144 | 144 | 852 | 0.674 ± 0.141 | 1.126 | **0.599** | 0.601 | 3/1000 |
| stealth `+rescue1` (corroboration) | 36 | 36 | 969 | 1.100 ± 0.262 | 1.889 | **0.582** | 0.589 | 3/1000 |
| haiku `+stakeholder_backfill` (C1) | 252 | 455† | 505 | 0.715 ± 0.135 | 0.786 | **0.910** | 0.886 | 306/1000 |
| flash `+stakeholder_backfill` (C1) | 206 | 206 | 754 | 0.840 ± 0.165 | 0.942 | **0.892** | 0.945 | 255/1000 |

† haiku's C1 runs at m=252, not 455 — see *Declared deviations*.

### Which R is the reported figure — and why it is not the pre-registered one

The pre-registered C2 null is **noise-asymmetric with its own observed arm**: observed compares a
*fixed* target against a *random* first-pass sample (one random side), while the null compares two
*random* first-pass samples (two random sides). Two random sides carry more sampling noise, so the
null is wider, T95 is inflated, and R is biased **downward — toward row 4, the outcome this
pre-registration itself calls "the valuable null."** A design biased toward its author's convenient
answer is one to correct, not to footnote.

So the **variance-matched** null is the reported figure: it fixes one first-pass subset and
randomises the other (50 fixed choices × 20 draws), giving the null exactly the observed arm's
structure. Instrument: `arm_b_variance_matched_null.py`.

**It changes nothing, and that is the point of reporting it rather than the reverse.** R_nemotron
0.599 vs 0.601; T95 1.126 vs 1.112 — the asymmetry is worth about 1% of T95. Both are tabled above,
so the correction is visible without a later reader having to rediscover the concern in order to
learn that it was handled. The pre-registered numbers are not withdrawn; they are simply not the
headline.

**Power check (row 1, computed before the arms were read).** MDE anchor = mean L1 between the
nemotron and stealth **first-pass** strata, computed the same way at the same m=144: **1.914 ±
0.261** (p05 1.486). T95_nemotron = 1.112 < 1.914, so the instrument resolves a known-real
cross-model difference at this m with room to spare. Row 1 does **not** fire; rows 2–4 are
evaluable.

**Outcome: row 4 — `just_misauthored`.** R_nemotron = 0.599 ≤ 1 (pre-reg 0.601 — the row fires
identically under both nulls). Reported whatever fires, variance-matched with pre-reg in
parentheses: R_nemotron **0.599** (0.601), R_C1(haiku) **0.910** (0.886), R_C1(flash) **0.892**
(0.945), R_stealth **0.582** (0.589).

stealth corroborates in direction (R 0.582, concordant with nemotron's 0.599) at n=36, and it
neither set, blocked, nor downgraded the call — the verdict is nemotron's alone.

### The result is stronger than "no difference detected"

The rescue stratum is not merely *indistinguishable* from first-pass — it is **more similar to
first-pass than first-pass is to itself under resampling** (observed 0.668 < null mean 0.784;
zero of 1000 draws reached T95). Two independent descriptive readings agree:

- **h1_band null share** — nemotron rescue 12.5% vs first-pass 12.3%; stealth 5.6% vs 9.5%.
- **red-verdict rate** — nemotron rescue 13.2% (19/144) vs first-pass 13.3% (113/852).

Both are descriptive, never criterial (they are computed from the same seat-type vector as the
primary, so counting them as independent evidence would be a criterion whose name is not true of
what it counts).

### What *does* move: the C1 arms, not the rescue arms

Both C1 arms sit closer to their own nulls (R 0.886 / 0.945, with 25–37% of draws exceeding T95)
than either rescue arm does (0.599 / 0.582, 0.3% each). ε moves the same way and in the same
direction: the backfill strata author **higher** ε than their first-pass (haiku 0.593 vs 0.555;
flash 0.548 vs 0.501) while the rescue strata author **lower** ε (nemotron 0.458 vs 0.489;
stealth 0.508 vs 0.548). So whatever structural movement exists in this data is on the
**regeneration-under-a-new-prompt** side, not the **hardness** side — which is exactly the
confound row 4 needed ruled out, and it is ruled out in the direction that supports row 4 rather
than against it. Neither C1 arm exceeds its own null either, so this is a difference in degree
between two sub-threshold effects, not a second finding.

### C3 — is hardness a property of the seed or the model?

nemotron's 144 ∩ stealth's 36 = **5** shared seeds against **5.16** expected under a one-sided
hypergeometric over the shared 1005-seed pool (p = 0.61). Dead on chance. **Declared low-powered
by construction** — with an expectation of ~5, only a large enrichment is detectable — so this is
`underpowered`, **not** `no enrichment`, and it is directional only. It does not carry the
verdict; it is consistent with it. The 5 ids are recorded in `arm_b_results.json`.

### Declared deviations from the plan's Phase 2, forced by the data

1. **haiku's C1 runs at m=252, not 455.** C2's null needs two *disjoint* m-samples from the
   first-pass stratum, i.e. n_firstpass ≥ 2m; haiku is 505 first-pass vs 455 backfill, and
   2×455 = 910 > 505, so the matched-disjoint design is **not constructible at m=455**. The rule
   **m = min(n_target, ⌊n_firstpass/2⌋)** was applied uniformly to every arm and binds only here.
   R is a ratio to the leg's own null at the same m, so the arm stays internally valid — but it is
   measured on a subsample and its m is reported everywhere it appears.
2. **The MDE anchor is computed at the primary arm's own m=144**, as a mean over 1000 draws, so
   that it and T95 are the same estimator. A full-stratum anchor compared against an m=144 T95
   would compare two different things.

### Provenance of the correction

The variance-matched null was written **after** the primary result was seen, and is declared
post-hoc for that reason — the concern was raised by the observed-below-null pattern in the
pre-registered arms, not anticipated at freeze time. It is promoted to the reported figure
nonetheless (*Which R is the reported figure*) because it is the better-specified estimator on its
own merits, and because it moves the verdict nowhere: had it flipped the row, the honest report
would have been a FAILED pre-registration, not a corrected one. The pre-registration is preserved
unedited in `PREREGISTRATION.md` (md5 `1989b5536578a4e7ce2503baacbfd4ad`) so the asymmetry it froze
stays on the record.

### Zero classification

| zero | type |
|---|---|
| 0/1000 nemotron draws exceeded T95 | **tested absence** — the instrument resolves the between-model anchor (1.914) at the same m, and the C1 arms produce 25–37% exceedance, so it is not stuck at zero |
| C3 overlap 5 vs 5.16 expected, p=0.61 | **underpowered** by construction — declared before the run; not `no enrichment` |
| `verdict_join` absent on 118 nemotron records | **structurally undetermined, not missing** — `verdict_join` is null exactly where `h1_band` is null (118 of the 123 h1-nulls), the OQ-51 `<2 real seats` rule; e.g. `ai_dignity_safeguarding__imago_dei_reading` has `unknown` at 3 of 4 seats. Never coerced to 0 |
| `purity_band` null (nemotron 8/852, flash 86/754, …) | **absence token, not a score** — OQ-60's two tokens both serialize as JSON null; `n_scored`/`n_total` reported per stratum rather than averaged through |
| stealth rescue red-verdict 0/36 | **untested at this n** — 36 stories against a 4.2% base rate expects ~1.5; its absence carries no signal |

## What this changes

**OQ-378** (batch-family seed rescues, ~257 residue seeds) is the consumer. Its entry predicts "a
tail that never closes," and the missing warrant for the spend was whether the un-rescued residue
**biases** a leg or merely **shortens** it. On the seat-type distribution, at the two legs where
the question is answerable, it shortens: the rescued stratum is statistically indistinguishable
from — indeed closer than resampling noise to — the first-pass stratum. Under the pre-registered
row 4 reading, residues stop being a standing concern and OQ-378's spend becomes a **composition
nicety** rather than a bias correction. That is a change in the warrant, not a recommendation to
drop the spend; the operator's seat.

**Scope, stated plainly.** This measures the *rescued* residue, which is the only residue that
exists as stories. The **never-generated** seed pool is a third hardness stratum and is not
measured here — it cannot be, without generating it. That is the natural extension of Arm B and
is noted, not built.

## Selection confound, stated explicitly

Seeds land in `+rescue1` (`testsets_nemotron` 144, `testsets_stealth` 36) **because they failed
the first pass**, so any structural difference would have been confounded with selection. The confound matters for a positive result; this is a
null, and a null under selection is the *stronger* direction — the stratum most likely to differ,
by construction, does not. C1 addresses the complementary channel (regeneration without hardness)
and also fails to exceed its own null. **Residual, declared:** C1 is a cross-leg transfer (haiku
and flash are different models with different type mixes and first-pass strata), which is why the
arms are compared as null-normalised ratios and why a row-3 reading would have been marked
INFERRED. Row 4 does not depend on the transfer — it is decided by R_nemotron alone.
