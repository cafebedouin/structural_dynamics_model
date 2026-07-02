# OQ-195 — General-n H¹ Gap Spectrum: Verification Writeup

**Date:** 2026-07-02. **Deliverable:** `docs/h1_gap_spectrum_general_n.md` (Lemma 1,
Theorems A/B/C/D, Remark E). **Scripts:** `python/audits/oq195_h1_spectrum_check.py`
(re-runnable), `prolog/tests/test_h1_spectrum.pl` (standing plunit witness).

## Results against the pre-registered criteria (PROPOSAL.md — all BLOCKING)

All seven checks PASS on the first run (`run.log`, n ≤ 40):

| Check | Result |
|---|---|
| (g) T derived from code | ALL_TYPES = 8 tokens, real = 7 = proof value — PASS |
| (a/b) per-band exact match, unbounded + T=7-bounded | PASS, n = 2..40, every band |
| (b-control) unconstrained classifier discriminated | unions identical for ALL n; bands mismatch at 38/39 n-values — PASS (a union-only check would have passed the wrong classifier at every n; the review-mandated per-band architecture is what catches it) |
| (c) Theorem A min-nonzero = n−1 | PASS, n = 3..40 |
| (d) record match n=4 / n=5 | PASS / PASS |
| (e) perturbed-predictor negative control | FLAGGED — PASS |
| (f) Theorem C full-spectrum iff + every-value-forbidden | PASS, zero exceptions, all n ≤ 40, all j |

Engine-side witness: `test_h1_spectrum.pl` **23/23 passed** (exhaustive n = 2–4 over
7 real tokens + `unknown`; constructive realization of every ≤7-bloc partition at n = 5–12
with per-partition Lemma-1 equality and per-n set equality against the enumerator-verified
spectra; `unknown`-padded vectors at n = 12 exercising the OQ-51 filter at a cardinality it
had never been run at; two negative controls, both shown able to fail).

Hand-derivation cross-checks that agreed with the enumeration before it ran: H(6) and H(7)
full sets, the n=12 T-bounded maximum 61, the forbidden-10-at-n=6 band-3 hole, and the
operator's independent review (Lemma 1, the Theorem A bound, the band formula, the
separation-condition algebra, full H(6)).

## Adversarial review record (honest gap + substitute)

A three-lens multi-agent adversarial pass (proof-soundness / statement-fidelity /
misreading-surface) was launched and **blocked by a session subagent limit** (all three
refuters failed pre-start; `wf_7a551a32-b44`). Substitute, declared: (1) the operator's
prior independent hand-derivation of the core results; (2) the machine enumeration with
negative controls (above), which independently confirms every quantitative claim; (3) a
systematic author re-derivation pass over every proof step and edge case, which **caught
and fixed one real prose defect** — §4's lead had called B_{j+1} "band j+1's minimum,"
false for j+1 > n/2 (n=6: band 4's true minimum is 12, not B₄ = 8); corrected to the floor
formulation, which is all Theorem C uses (theorem and machine checks unaffected). Residual:
an independent-agent prose review remains available post-reset if the operator wants it;
the quantitative content does not depend on it.

## Artifacts

`PROPOSAL.md` (pre-registered before any run) · `run.log` · `enumeration_results.json`
(per-n per-band sets, both variants, all verdicts) · `stakeholder_seat_census.txt`
(as-of HEAD `2b579d2b`: live seat counts 3–12 across the three legs; kernel_v1 has zero
stakeholder facts) · plunit output in the commit message.

## What this changes

The |real seats| = 4 caveat on Theorem 2 is now a proven special case of a general law:
bottom gap {1,…,n−2} at every n; exact band decomposition; inter-band gap iff
n ≥ j+3+C(j+1,2); token-bounded truncation for n > 7 (live in the stakeholder frame, seat
counts to 12). Propagation targets (v8 §3.4, v7 amendment note, v6.13.1 changelog pointer,
`grothendieck_cohomology.pl` stale-range flag at lines 167–182 — NB every external pointer
cites it as `:158`, line-drifted) land in the close commit.
