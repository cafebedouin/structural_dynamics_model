# Five-leg cross-model comparison of `prolog/testsets*`

**Date:** 2026-07-20 · **Engine:** HEAD `9c226e8` (all five legs classified at the SAME commit —
`code_state_coherent = True`) · **Script:** `five_leg_twin_comparison.py` · **Evidence:**
`five_leg_comparison.json`

> **v2 (full N).** The kimi leg is now COMPLETE at n=1005 (was a 334-story partial in v1). Full-N
> confirmed the two headline findings and **falsified** the provisional "kimi is the cleanest leg"
> claim — see §A verdict_join. This is why partial-N marginals were flagged provisional.

## Setup

Five model legs, each classified fresh at the SAME engine HEAD via `classify_corpus` (gate-free,
single-model fingerprint enforced), so any difference is the **generation model**, not the engine:

| leg | provenance model | N | role |
|-----|------------------|---|------|
| `testsets`        | mixed | 153 | live mixed working set (edge-case bed) |
| `testsets_haiku`  | claude-haiku-4-5-20251001 | 960 | twin |
| `testsets_flash`  | gemini-2.5-flash | 960 | twin |
| `testsets_sonnet` | claude-sonnet-5 | 1001 | twin |
| `testsets_kimi`   | **kimi-k2.6** | **1005** | twin (NEW this session — full pool) |

**Framing (CLAUDE.md *Generation is stochastic*):** the four twins share the `never_generated`
seed pool, but re-generating a seed under a different model is a NEW DRAW, not a re-measurement. So
same-seed cross-model differences are read as **seat-expressive** (model-dependent draw), NOT an
error rate; agreement is read as **situation-fixed** (seed-determined).

**Caveat.** `kimi-k2.6` is a **reasoning-heavy** model (~11.7k reasoning tok/story); the
haiku/flash/sonnet twins were generated thinking-off, so kimi carries that asymmetry by
construction. `testsets` (153) is the mixed edge-case working set, not a twin.

## A. Per-leg marginals — model fingerprints

**H¹ band (cohomological obstruction; higher = more fractured):**

| leg | 0 | 2 | 3 | 4 | 5 | 6 | null | band-3 share |
|-----|---|---|---|---|---|---|------|--------------|
| haiku  | 192 | 43 | 305 | 34 | 290 | 17 | 77 | 32% |
| flash  | 228 | 29 | 322 | 44 | 245 | 1  | 91 | 34% |
| sonnet | 301 | 42 | 264 | 10 | 197 | 68 | 119| 26% |
| kimi   | 130 | 19 | **631** | 17 | 160 | 7 | 40 | **63%** |

- **kimi is strikingly homogeneous — 63% band-3** (vs 26–34% for the others), and this held EXACTLY
  from the 334-story partial (also 63%) to the full 1005. Reasoning-heavy generation converges on a
  characteristic 4-seat disagreement structure. **This is the standout, robust model signature.**
- **sonnet fractures to the extremes** — most band-0 (301, agreement) AND most band-6 (68, maximal
  4-seat obstruction).
- No forbidden bands (band-1 only in haiku/kimi as legitimate 2-real-seat cases; nulls = <2 real
  seats per OQ-51, never 0).

**verdict_join (headline verdict):**

| leg | green | yellow | red | null | red% |
|-----|-------|--------|-----|------|------|
| haiku  | 144 | 707 | 38 | 71  | 4.0% |
| flash  | 115 | 677 | 77 | 91  | 8.0% |
| sonnet | 169 | 677 | 37 | 118 | 3.7% |
| kimi   | 151 | 787 | 27 | 40  | **2.7%** |
| testsets| 0  | 110 | 33 | 10 | 22% (edge-case bed) |

- **CORRECTION vs v1:** the partial (334) showed kimi red% = 0.3% ("cleanest leg"). At full N it is
  **2.7% red** — comparable to sonnet (3.7%) and haiku (4.0%). **The "kimi is dramatically cleaner"
  finding was a partial-N artifact (the first 334 seeds were unrepresentative) and does NOT hold.**
  kimi is mildly cleaner than flash (8% red) but not an outlier.
- **flash is the most alert-triggering** twin (8% red) — this DID hold across N.

**maxent_top_type (dominant modal type):**

| leg | snare | tangled_rope | scaffold | rope | piton | mountain |
|-----|-------|--------------|----------|------|-------|----------|
| haiku  | 494 | 259 | 91 | 46 | 57 | 13 |
| flash  | 463 | 208 | 132| 88 | 34 | 35 |
| sonnet | 276 | **385** | 81 | 56 | **194** | 9 |
| kimi   | 488 | 349 | 59 | 41 | 53 | 15 |

- **sonnet remains the outlier** — the only leg where **tangled_rope > snare**, with far more
  **piton** (194) than any other. Sonnet leans "genuinely-entangled" + "dead-coordination."
- haiku, flash, kimi are all snare-dominant; kimi's snare/tangled_rope split (488/349) sits closest
  to haiku's shape.

## B. Paired same-seed comparison (957 seeds shared across all four twins)

| agreement | all-4 agree | rate |
|-----------|-------------|------|
| **h1_band**         | 139/957 | **14.5%** |
| **maxent_top_type** | 336/957 | **35.1%** |

Pairwise H¹-band agreement: haiku~flash 46.5%, flash~kimi 45.9%, flash~sonnet 45.6%, haiku~kimi
42.1%, haiku~sonnet 39.4%, sonnet~kimi 38.4%. **No two models agree on the exact H¹ band for more
than ~47% of shared seeds.** (Both rates rose only ~1 pt from the 334-partial — the finding is
N-stable.)

### Headline finding (robust at full N)

**The cohomological obstruction (H¹ band) is overwhelmingly model-dependent** — only 14.5% of seeds
produce the same band across all four models; modal **type** is more seed-anchored (35.1%) but still
a minority. Under the seat-theoretic reading this is the mechanism *working*, not noise: the H¹ band
is a property of *how a given model draws the constraint* (which seats it populates and how they
disagree), so a redraw legitimately occupies a new seat. Type is closer to situation-fixed;
obstruction structure is closer to seat-expressive. Direct empirical support for **verdicts being
seat-indexed** (v8 seat/gauge/orientation) rather than seed-determined.

The **kimi band-3 homogeneity** (63%, N-invariant) is the sharpest single-model signature in the
set and the most interesting follow-up: *why* does a reasoning-heavy model collapse so hard onto one
obstruction level? Candidate: mandatory max-effort reasoning drives its four authored perspectives
toward a stereotyped 3-of-4 disagreement pattern.

## Method note / lessons

- **A model swap is an engine change** — this is the first `kimi-k2.6` batch corpus; both driver
  bugs it surfaced (Moonshot `status_code==0` batch rows; the 100 MB `/files` cap) are fixed and
  documented (runbook §7b, KNOWN_STATE 2026-07-19/20).
- **Partial-N marginals mislead** — the 334-partial's "0.3% red" inverted at full N. Marginals over
  a non-random slice (first-N seeds) are provisional; the paired agreement rates, by contrast, were
  N-stable.
- Deeper cuts not yet run: per-type **confusion matrices** (which X→Y a seed shifts to across
  models), the **committer axis** (`cs_pattern`/`cs_verdicts`), signature and per-domain breakdowns.
- Reproduce: `python3 python/audits/five_leg_twin_comparison.py` (consumes
  `outputs/pipeline_output_<leg>_head.json`; regenerate via `classify_corpus`).
