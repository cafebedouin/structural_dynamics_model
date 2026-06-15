# PRE-REGISTRATION — OQ-131 Q1: six-vs-four observer site & the Grothendieck cohomology

**Committed before any experimental run.** Adjudication in `FINDINGS.md` is against THIS
document only. Any criterion found mis-specified after the fact is halt-and-escalate, never
inline-amended.

## 0. Scope walls (declared up front, no silent narrowing)

- This run measures **H⁰ and H¹ only** (Čech 0/1-cohomology of the type presheaf on the observer
  site). The subobject-classifier / topos-structure effects OQ-131 records on a larger site are
  **out of scope and remain open**.
- The result is **"at these declared seat-bundles, within this sensitivity envelope,"** not "the
  6-point cohomology of this corpus" full stop. The bundle sweep **bounds** robustness; it does
  **not** eliminate the seat-bundle as a free parameter. The Ω_E finding is **not bundle-independent**.
- This measures *this committed corpus* under a wider probe — **not** a forecast of a
  natively-6-authored corpus. The **Ω_C (corpus-adoption) arm stays `future`**.

## 1. Substrate (fixed, deterministic to re-measure)

- Corpora: `testsets` (live, 57), `testsets_haiku` (960), `testsets_flash` (960). Committed,
  fixed substrate ⇒ deterministic re-measurement.
- Twin pairing key = **file basename** (story id). Verified `comm -12` = **960/960** shared
  basenames. Only composition difference on record: 87 haiku-only authored grids; flash 0.
- Engine modes (committed `a06b5c7f`): `canonical_6`, `power_only_4`, `power_only_6`; seat
  bundles `observer_bundle_powerful = bundle(generational,mobile,global)`,
  `observer_bundle_organized = bundle(generational,constrained,national)`,
  `observer_baseline_tes = bundle(biographical,mobile,national)`.
- One swipl process per (corpus × site config) — **no `cached_obstruction/3` bleed** (the
  `perturb.py` pattern). Serial only (OQ-77). Nothing writes `outputs/pipeline_output.json`.

## 2. Headline quantity

**(H¹₆ − H¹₄) / 9**, paired by constraint id — disagreeing *new* pairs over the 9 that *can* move,
conditional on the fixed 4-seat configuration. `9 = C(6,2) − C(4,2) = 15 − 6`.

- **Never** H¹/15 (dilutes with 6 immovable pairs); **never** raw H¹.
- Basis is witnessed, not assumed: canonical-first ordering ⇒ the 4-seat sub-vector is a positional
  **prefix** of the 6-vector (control already PASS at `a06b5c7f`), so the 6 canonical pairs are
  byte-identical between modes and the entire delta is the 9 new pairs. Per-constraint check at
  run time: H¹ over the first-4 of the 6-vector == standalone H¹₄.

## 3. Experimental arms (as designed; pre-registered)

1. **Conditioned normalization** — the headline (H¹₆−H¹₄)/9 per corpus.
2. **Single-coordinate control** — `power_only_4` vs `power_only_6` (T/E/S held at
   `observer_baseline_tes`, only power vocabulary widens). The baseline the realistic-seat arms are
   read against.
3. **Per-seat bundle sensitivity (one-at-a-time, NOT Cartesian)** — sweep `powerful` over its
   ladder with `organized` pinned at realistic, then vice versa; plus the realistic joint point.
   Yields a per-seat sensitivity map (which seat's coordinates drive the move), not a confounded
   joint one. Ladder points pre-registered in §6.
4. **Null model — permutation, LOAD-BEARING** (see §4).
5. **Positive control the new seats are LIVE** (distinct from the cache fix): (i) `orbit_vector`
   length == 6 under every 6-point mode (4 ⇒ silent fallback ⇒ **abort**); (ii) ≥1 constraint where
   `powerful`/`organized` emit a type absent from its 4-seat vector ⇒ H⁰ 1→0 / H¹ increments.
   No constraint flips ⇒ new seats redundant (a *result*, believable only with the length-6 witness).
6. **H⁰ & orbit churn — co-outputs AND the redundancy witness.** Report orbit **splits** and the
   **singleton (H⁰=1) count** shift. This arm carries the **second finding** permutation cannot:
   whether the new seats' constraint-correlated signal is *already carried by the 4 canonical seats*
   — via co-classification (does seat 5/6 emit a type absent from the 4-seat vector, or merely echo
   an existing seat?).
7. **Model comparison on the matched stratum.** Key = shared basename (960/960). Compare
   haiku-vs-flash on the **ids where both are non-grid** (exclude the 87 haiku-grid ids); report the
   haiku grid sub-stratum separately as haiku-only. Residual gap on the matched set ⇒ model;
   otherwise composition. (Exact matched-id count is an OUTPUT, computed at run time from the two
   manifests, not asserted here.)

## 4. Permutation null — pinned parameters and verdict grammar

- **N = 1000 shuffles. RNG seed = 20260615.** (Pinned content, not execution choices — otherwise
  band width is a free parameter chosen after seeing the observed value. On the deterministic
  substrate this also makes the band itself re-witnessable.)
- Procedure: shuffle the observed `powerful`-seat types across constraints (**independently** for
  `organized`), recompute the 9-new-pair disagreements vs each constraint's **fixed** 4-seat config,
  ×N → band (2.5/50/97.5 pct). Observed (H¹₆−H¹₄)/9 vs band, **per corpus**.
- Holds the seat-5/6 marginal **exact by construction** — "a new type appeared" is constant across
  null and observed, so **above-band cannot be the type-appearance artifact**.
- **Verdict grammar (pre-registered):**
  - **above band** ⇒ *structured, constraint-correlated fracture* — the new seats carry signal
    beyond chance.
  - **within band** ⇒ *combinatorial-only* — the move is what random reassignment of the same
    marginal would produce.
  - **below band** ⇒ *consonant-suppressing* — new seats agree with the 4-seat config more than
    chance.

### 4a. Exchangeability gate (substrate-checked FIRST, before trusting any band)

Permutation is valid only if `dr_type(C, powerful_seat)` is a **pure function of C** (same
constraint+seat ⇒ same type every run). **Witness on the substrate** (re-run ⇒ identical type
vector) before computing any band. If seat-5/6 type depends on anything corpus-position-like,
exchangeability weakens ⇒ **halt-escalate**.

### 4b. Seat-marginal entropy guard (singleton problem, one level up)

Determinism gives "the observed seat vector is the one realization," but permutation also needs the
seat types to be non-degenerate *labels* across constraints. If `powerful` emits one type for
~950/960 constraints, shuffling barely moves ⇒ band artificially tight ⇒ above-band trivially easy.
**Report seat-5 and seat-6 type-marginal Shannon entropy beside every band.**
**Halt-if-degenerate threshold: H_marginal < 0.20 bits** (normalized to log of #types observed at
that seat) ⇒ band uninterpretable, report the entropy and **withhold the band verdict** for that
corpus/seat.

### 4c. What permutation does NOT license (two findings, two witnesses)

- Above-band ⇒ "new seats carry constraint-correlated signal beyond chance." It is **NOT** a
  redundancy test — a seat-5 reading perfectly predicted by the analytical seat is redundant yet
  still lands above-band. **Redundancy is routed to the orbit-split / co-classification arm (#6),
  never to either null.** Permutation-above-band must not silently stand in for non-redundancy.
- **Codomain frequency-weighted draw = labeled SECONDARY diagnostic, not co-equal.** Report, do not
  headline; its Laplace-smoothing is a knob. Divergence from permutation is *informative* (⇒ seats
  5–6 emit a type the corpus-wide marginal underweights — a finding about the new vocabulary), not a
  null failure. **Per-constraint marginal null = tertiary, upward-biased** (degenerate on singleton
  orbits — the consonant-at-4 cases — over-fires "structured").

## 5. Combined two-findings decomposition (pre-registered reading)

- permutation-above-band **+** new-type-at-seat-5/6 ⇒ **genuine added information**.
- permutation-above-band **+** always-echoes-a-canonical-seat ⇒ **correlated but redundant**.
- within-band ⇒ combinatorial-only regardless of #6.

## 6. Per-seat sweep ladder (pinned)

Power-axis ordering, lowest→highest agency, used for both the `powerful` and `organized` one-at-a-
time sweeps (the OTHER seat pinned at its realistic bundle):

- `powerful` ladder (vary its bundle T/E/S among the canonical observer coordinates while holding
  power atom = powerful): realistic `(generational,mobile,global)`; plus
  `(biographical,trapped,local)`, `(biographical,mobile,national)`, `(civilizational,analytical,global)`.
- `organized` ladder (power atom = organized): realistic `(generational,constrained,national)`; plus
  `(biographical,trapped,local)`, `(generational,mobile,global)`, `(civilizational,analytical,global)`.
- The realistic joint point `(canonical_6 @ realistic defaults)` is the shared anchor.

These are observer-coordinate seats, not new vocabulary — each T/E/S triple is drawn from the
canonical context dimensions (`docs/logic.md`). Reported as a per-seat sensitivity map; the spread
**bounds** the Ω_E robustness, it does not collapse the seat to a single number.

## 7. Positive-control pass/fail (gating)

- **Cache-bleed / fallback witness:** every 6-point run asserts `length(orbit_vector) == 6`. Any 4
  ⇒ silent fallback ⇒ **abort that cell**, do not analyze.
- **Liveness control (arm 5):** ≥1 constraint with a new type at seat 5/6 (already witnessed at
  `a06b5c7f`: `fourteenth_amendment_equal_protection__formal_equality_reading`). If a corpus shows
  **zero** flips, that is reported as "new seats redundant on this corpus" — believable **only**
  with the length-6 witness in hand.
- **9-pairs basis check:** per constraint, H¹ over first-4 of the 6-vector == standalone H¹₄.

## 8. Halt-and-escalate branches (pre-registered)

- Exchangeability gate (§4a) fails on any corpus ⇒ **halt**, report, do not compute that corpus's band.
- Seat-marginal entropy < 0.20 bits (§4b) ⇒ **withhold band verdict** for that corpus/seat,
  report entropy.
- `orbit_vector` length ≠ 6 in any 6-point cell ⇒ **abort that cell** (silent fallback).
- Any pre-registered criterion found mis-specified at adjudication ⇒ **halt-and-escalate**, not
  inline-amend.

## 9. Fold-in target

Measured Ω_E findings fold into **ISSUES.md OQ-131 Q1** with the two scope walls of §0 explicit.
Ω_C adoption arm stays `future`.
