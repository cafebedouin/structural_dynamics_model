# Five-leg cross-model comparison of `prolog/testsets*`

**Date:** 2026-07-20 · **Engine:** HEAD (data-only commits `bafd87b`→`89d8fba`; no engine `.pl`
changed — verified) · **Script:** `five_leg_twin_comparison.py` · **Evidence:** `five_leg_comparison.json`

## Setup

Five model legs, each classified fresh at the SAME engine HEAD via `classify_corpus` (gate-free,
single-model fingerprint enforced), so any difference is the **generation model**, not the engine:

| leg | provenance model | N | role |
|-----|------------------|---|------|
| `testsets`        | mixed (haiku/sonnet/flash/…) | 150 | live mixed working set |
| `testsets_haiku`  | claude-haiku-4-5-20251001    | 960 | twin |
| `testsets_flash`  | gemini-2.5-flash             | 960 | twin |
| `testsets_sonnet` | claude-sonnet-5              | 1001| twin |
| `testsets_kimi`   | kimi-k2.6                    | 334 | twin (NEW, this session; partial — ~671 pending) |

**Framing (CLAUDE.md *Generation is stochastic*):** the four twins share the `never_generated`
seed pool, but re-generating a seed under a different model is a NEW DRAW, not a re-measurement.
So same-seed cross-model differences are read as **seat-expressive** (model-dependent draw), NOT an
error rate; agreement is read as **situation-fixed** (seed-determined).

**Caveats.** (1) `kimi` is n=334 (partial: a within-budget batch + cancel-harvest; ~671 seeds
pending a balance top-up), so its marginals are on a smaller, possibly non-representative slice —
the first 334 seeds of the pool. (2) `kimi-k2.6` is a **reasoning-heavy** model (~11.7k reasoning
tok/story); the haiku/flash/sonnet twins were generated thinking-off, so kimi carries that asymmetry
by construction. (3) `testsets` is the 150-story mixed edge-case working set, not a twin.

## A. Per-leg marginals — each model has a distinct fingerprint

**H¹ band (cohomological obstruction = genuine multi-seat disagreement; higher = more fractured):**

| leg | 0 | 2 | 3 | 4 | 5 | 6 | null | band-3 share |
|-----|---|---|---|---|---|---|------|--------------|
| haiku  | 192 | 43 | 305 | 34 | 290 | 17 | 77 | 32% |
| flash  | 228 | 29 | 322 | 44 | 245 | 1  | 91 | 34% |
| sonnet | 301 | 42 | 264 | 10 | 197 | 68 | 119| 26% |
| kimi   | 43  | 8  | **211** | 2 | 52 | 2 | 16 | **63%** |

- **kimi is strikingly homogeneous** — 63% of its stories land in band-3 vs 26–34% for the others.
  Reasoning-heavy generation produces a much more *uniform obstruction structure*.
- **sonnet fractures hardest** — most band-0 (agreement, 301) AND most band-6 (68, the maximal
  4-seat obstruction); it spreads to the extremes.
- No forbidden bands: band-1 appears only in haiku (2 stories) = legitimate 2-real-seat cases
  (the {1,2}-forbidden law is 4-real-seat only). Nulls = <2 real seats (OQ-51), never 0.

**verdict_join (headline verdict):**

| leg | green | yellow | red | null | red% |
|-----|-------|--------|-----|------|------|
| haiku  | 145 | 706 | 38 | 71  | 4.0% |
| flash  | 117 | 675 | 77 | 91  | 8.0% |
| sonnet | 170 | 677 | 36 | 118 | 3.6% |
| kimi   | 70  | 247 | **1** | 16 | **0.3%** |
| testsets| 3  | 104 | 33 | 10 | 22% (edge-case bed) |

- **kimi produces the "cleanest" stories** — essentially no red (1/334) and the highest green share
  (21%). Reasoning-heavy drafting yields more internally-coherent constraints (fewer severe alerts).
- **flash is the most alert-triggering** twin (8% red).

**maxent_top_type (dominant modal type):**

| leg | snare | tangled_rope | scaffold | rope | piton | mountain |
|-----|-------|--------------|----------|------|-------|----------|
| haiku  | 494 | 259 | 91 | 46 | 57 | 13 |
| flash  | 463 | 208 | 132| 88 | 34 | 35 |
| sonnet | 276 | **385** | 81 | 56 | **194** | 9 |
| kimi   | 153 | 129 | 23 | 15 | 12 | 2 |

- **sonnet is the outlier** — the only leg where **tangled_rope > snare**, and far more **piton**
  (194) than any other. Sonnet leans toward "genuinely-entangled" and "dead-coordination" readings.
- haiku/flash/kimi are all snare-dominant; flash surfaces the most benign types (rope 88, mountain 35).

## B. Paired same-seed comparison (322 seeds shared across all four twins)

| agreement | all-4 agree | rate |
|-----------|-------------|------|
| **h1_band**         | 43/322 | **13.4%** |
| **maxent_top_type** | 110/322| **34.2%** |

Pairwise H¹-band agreement: haiku~flash 46.5% (highest), flash~kimi 44.7%, flash~sonnet 45.6%,
sonnet~kimi 39.5%, haiku~kimi 38.8%, haiku~sonnet 39.4%. **No two models agree on the exact H¹ band
for more than ~47% of shared seats.**

### Headline finding

**The cohomological obstruction (H¹ band) is overwhelmingly model-dependent** — only 13% of seeds
produce the same band across all four models. The modal **type** is more seed-anchored (34% all-agree)
but still a minority. Under the seat-theoretic reading this is the mechanism *working*, not noise:
the H¹ band is a property of *how a given model draws the constraint* (which seats it populates and
how they disagree), so a redraw legitimately occupies a new seat. Type is closer to
situation-fixed; obstruction structure is closer to seat-expressive. This is direct empirical
support for verdicts being seat-indexed (v8 seat/gauge/orientation) rather than seed-determined.

## Open / next

- **kimi at n=334** is partial and is the *first* 334 seeds (not a random slice) — re-run the
  comparison after the top-up (~671 pending) before treating kimi marginals as final. The
  homogeneity finding (63% band-3, 0.3% red) is striking enough to flag now but must be re-checked
  at full N.
- Deeper cuts available and not yet run: per-type **confusion matrices** (which type X→Y a seed
  shifts to across models), the **committer axis** (`cs_pattern`/`cs_verdicts`) marginals, signature
  distributions, and per-domain breakdowns.
- Reproduce: `python3 python/audits/five_leg_twin_comparison.py` (consumes
  `outputs/pipeline_output_<leg>_head.json`, regenerate with `classify_corpus`).
