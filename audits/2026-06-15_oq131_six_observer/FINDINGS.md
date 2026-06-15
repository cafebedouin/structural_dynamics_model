# FINDINGS — OQ-131 Q1: six-vs-four observer site cohomology

Adjudicated **against `PRE_REGISTRATION.md` only**. Engine at commit `a06b5c7f`
(six-observer modes). Probe: `python/audits/oq131_six_observer_probe.py`. Raw per-cell
JSONs in `raw/`; machine output in `analysis.json`; console in `run_summary.txt`.

Substrate hashes (deterministic re-measurement key): live `27fb6031e58f` (n=57),
haiku `6a00db754df3` (n=960), flash `3ac23952cd03` (n=960).

## Gate results (all PASS — none triggered a halt/abort branch)

| gate | live | haiku | flash |
|---|---|---|---|
| len-6 fallback violations (§7) | 0 | 0 | 0 |
| exchangeability gate (§4a) | PASS (0 mism.) | PASS (0 mism.) | PASS (0 mism.) |
| 9-pair basis (H¹₆−H¹₄ == direct 9-pair) | 57/57 | 960/960 | 960/960 |
| seat-marginal entropy ≥ 0.20 (§4b) | s5=0.74 s6=0.76 | s5=0.56 s6=0.71 | s5=0.76 s6=0.66 |
| liveness — new type at seat 5/6 (§5) | 10 PASS | 293 PASS | 367 PASS |

- **Exchangeability PASS** (a re-run of `canonical_6` produced byte-identical orbit vectors
  for every constraint) ⇒ `dr_type(C, seat)` is a pure function of C on this substrate ⇒ the
  permutation null is valid. No corpus hit the §8 halt.
- **9-pair basis PASS for every constraint** ⇒ the canonical-first ordering keeps the 6 canonical
  observer-pairs byte-identical; the entire 4→6 H¹ delta lives in the 9 new pairs (the headline's
  denominator is real, witnessed not assumed).
- Entropy non-degenerate on every corpus/seat (all ≥ 0.56 normalized, far above the 0.20 halt) ⇒
  every permutation band is interpretable.

## Headline finding (pre-registered §2/§4, all three corpora agree)

**The observed (H¹₆−H¹₄)/9 falls BELOW the permutation band in every corpus → verdict
`consonant_suppressing` (PRE_REGISTRATION §4 verdict grammar).**

| corpus | observed (H¹₆−H¹₄)/9 | permutation band [2.5, 97.5] | verdict |
|---|---|---|---|
| live  | **0.446** | [0.741, 0.825] | below → consonant-suppressing |
| haiku | **0.562** | [0.738, 0.755] | below → consonant-suppressing |
| flash | **0.550** | [0.754, 0.775] | below → consonant-suppressing |

(N=1000 shuffles, seed=20260615 — pinned pre-run; band re-witnessable on the fixed substrate.)

**Reading.** The two new observer seats (`powerful`, `organized`) disagree with each
constraint's fixed 4-seat configuration **less than chance** reassignment of the *same* seat-type
marginal would produce. Because the permutation holds the seat-5/6 marginal exact by construction,
"a new type appeared" is constant across null and observed — so **below-band cannot be the
type-appearance / combinatorial artifact** the design was built to defeat. The widening does **not**
manufacture disagreement; the new positions largely **echo** the existing site.

This is mechanistically explained by the co-classification arm (§6): in every corpus
`echoes_both + new_type_either == n` exactly (live 47+10=57; haiku 667+293=960; flash 593+367=960).
The **majority** of constraints have *both* new seats reproducing a type already in the 4-seat
vector (live 82%, haiku 69%, flash 62%) — direct redundancy, which the below-band consonance
quantifies as *more* than chance.

## Two-findings decomposition (PRE_REGISTRATION §4c / §5)

- The permutation verdict is **below-band**, not above-band, so the "structured constraint-
  correlated fracture" reading does **not** apply on any corpus.
- The redundancy arm (#6, routed here **not** to the null per §4c) shows the new seats are
  **largely redundant**: 62–82% echo both, and where they do add a type (orbit splits: live 10,
  haiku 293, flash 367) the addition is consonant enough that the aggregate still sits below the
  null band.
- Net: under these declared seats, widening 4→6 **adds positions that mostly co-classify with the
  canonical four**; it does not expose hidden axiom-level fracture.

## Co-outputs (arm #6)

| corpus | orbit splits (4→6) | H⁰ singletons 4-seat → 6-seat |
|---|---|---|
| live  | 10  | 16 → 16 |
| haiku | 293 | 139 → 117 (−22 lose global section) |
| flash | 367 | 146 → 144 (−2) |

Adding seats can only break global sections, never create them; the H⁰ drop (notably haiku −22)
is the comparability-with-the-4-position-corpus boundary OQ-131 flagged.

## Single-coordinate control (arm #2) & per-seat sensitivity (arm #3)

- **`power_only_4` → `power_only_6`** (T/E/S held at `observer_baseline_tes`, only power
  vocabulary widens): mean delta/9 = live 0.454, haiku 0.571, flash 0.567 — within ~0.01–0.02 of
  the realistic-bundle headline. **The effect is driven by the power-atom widening, not the
  bundle-coordinate choice.**
- **Per-seat bundle sweep** (one-at-a-time, organized/powerful ladders, the other pinned
  realistic): headline stays in a tight envelope across all ladder points —
  live [0.442, 0.478], haiku [0.542, 0.592], flash [0.534, 0.623]. **Every** sweep point remains
  **below** its corpus's permutation band, so the consonant-suppressing finding is **bundle-robust
  within this envelope** (the highest-disagreement bundle, `powerful=(biographical,trapped,local)`,
  reaches 0.62 on flash — still below the [0.754, 0.775] band). The `powerful` seat's coordinates
  drive slightly more movement than `organized`'s, but neither flips the verdict.

## Model comparison on the matched stratum (arm #7)

Grid census (authored `accessibility_collapse/stakes_inflation/suppression/resistance`):
**haiku 87, flash 0** — reproduces the plan's 87/0 split exactly. Matched intersection 960/960;
non-grid matched stratum **873**.

- haiku non-grid headline 0.5498 (n=873) vs flash non-grid 0.5375 (n=873): **residual gap 0.012**.
  On the composition-matched set the two models are nearly identical ⇒ the small twin difference is
  **composition, not model**.
- Haiku grid sub-stratum (87, haiku-only): headline 0.679 — the authored-grid stories carry more
  4→6 disagreement, reported separately as haiku-only (not a model effect).

## Scope walls carried into the fold-in (PRE_REGISTRATION §0 — no silent narrowing)

1. **H⁰/H¹ only.** Subobject-classifier / topos-structure effects OQ-131 records on a larger site
   are **out of scope and remain open**.
2. **Bundle-dependent.** The result is "at these declared seat-bundles, within the §3 sensitivity
   envelope," **not** "the 6-point cohomology of this corpus." The sweep **bounds** robustness; it
   does not eliminate the seat-bundle as a parameter. The Ω_E finding is **not bundle-independent**.
3. **This corpus, wider probe — not a forecast.** Measures the committed corpus under a 6-point
   site, **not** a natively-6-authored corpus. **The Ω_C adoption arm stays `future`.**

## Bottom line (committed, with its falsifier)

Under the declared elite/collective seats, widening the observer site 4→6 is **consonant-
suppressing** on all three corpora — it adds positions that co-classify with the canonical four
more than chance, rather than exposing new fracture. The kill condition was the pre-registered
permutation band; the observed value fell below it cleanly and identically in sign across live,
haiku, and flash. The combinatorial artifact the design feared (mechanical H¹ inflation from more
pairs) is **falsified**: it would have shown up as within- or above-band, and did not.
