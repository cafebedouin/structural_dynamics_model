# OQ-195 — General-n H¹ Gap Spectrum: Verification Proposal (pre-registered)

**Date:** 2026-07-02 (registered BEFORE any verification run).
**Object:** the pure engine predicate `grothendieck_cohomology:obstruction_from_vector/3` —
H¹ = pairwise disagreement count over the `is_real_type`-filtered type vector; for an
agreement partition λ of n real seats, H¹ = C(n,2) − Σ C(λᵢ,2).

## Claims under test (the theorem set of `docs/h1_gap_spectrum_general_n.md`)

- **Lemma 1:** H¹(λ) = C(n,2) − Σ C(λᵢ,2) = Σ_{i<j} λᵢλⱼ = (n² − Σλᵢ²)/2, and equals the
  engine's pairwise count.
- **Theorem A:** min nonzero H¹ = n−1; {1,…,n−2} forbidden (n ≥ 3).
- **Theorem B (per-band):** band_j (partitions with largest part n−j) has value set
  {jn − C(j+1,2) − d : d ∈ D(j; parts ≤ n−j)}, D the triangular-sum set one level down;
  H(n) = {0} ∪ ⋃ⱼ band_j.
- **Theorem C (full-spectrum inter-band iff):** the interval (jn − C(j+1,2), (j+1)(n−j−1))
  contains a spectrum-forbidden value ⟺ n ≥ j + 3 + C(j+1,2).
- **Theorem D (token bound):** with k ≤ T blocks (T = real type tokens), H(n,T) = the
  restricted set; = H(n) for n ≤ T; top-truncated for n > T.

## Pre-registered acceptance criteria (BLOCKING — a miss halts the close, the theorem is
never edited to fit)

1. **(a/b) Per-band exact match**, n = 2..40: brute-force partitions grouped by largest part
   vs the Theorem-B recursive predictor — every band identical, both unbounded and T-bounded
   variants. *Union-only matching is disallowed* (review 2026-07-02 item 1: the union is
   invariant under dropping the parts-constraint).
2. **(b-control) Discriminating control:** the deliberately UNCONSTRAINED predictor must
   produce identical unions yet mismatched per-band sets for at least one (n, j) — proving
   the per-band check catches what a union check cannot. If it fails to discriminate, the
   check design is wrong.
3. **(c) Theorem A:** min nonzero = n−1 for every n = 3..40.
4. **(d) Record match:** n=4 → {0,3,4,5,6}; n=5 → {0,4,6,7,8,9,10} (v6.13.1:154,156).
5. **(e) Negative control:** a band-off-by-one perturbed predictor must be FLAGGED.
6. **(f) Theorem C iff over the full spectrum:** ∀n ≤ 40, ∀j ≥ 1 with both bands nonempty:
   interval-(T_j, B_{j+1})-contains-spectrum-gap ⟺ n ≥ j + 3 + C(j+1,2). Exact biconditional,
   zero exceptions.
7. **(g) T derived, not assumed:** T = |ALL_TYPES ∖ {unknown}| imported from
   `python/axiom_reachability.py`; assert T == 7 (the proof doc's value); Theorem-D
   enumeration runs on the derived value.
8. **Engine witness (plunit `prolog/tests/test_h1_spectrum.pl`):** exhaustive n=2–4 spectrum
   equality; constructive per-partition witnesses n=5–12 under the T-bound realizing every
   ≤T-block partition (complete for H(n,T)); unknown-padded vectors at n=8–12 (OQ-51 filter
   at new cardinalities); a `\+`-asserted perturbed-expectation negative control.

## Outputs
`enumeration_results.json` (per-n, per-band sets + all check verdicts), `run.log`, plunit
output, seat-count census (`stakeholder_seat_census.txt`, as-of stamped), WRITEUP.md.
Script: `python/audits/oq195_h1_spectrum_check.py` (re-runnable).
