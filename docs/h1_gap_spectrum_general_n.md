# The H¹ Gap Spectrum at Every Cardinality

**The reachable disagreement spectrum over n real seats is characterized exactly, for all n:
the quantum below which nonzero disagreement cannot fall grows linearly (n−1), the spectrum
decomposes into bands indexed by the largest agreement bloc, and every gap — boundary and
interior — is an instance of one self-similar recursion.**

**Version: v1.0** (2026-07-02)

> **Cross-reference.** This note proves the general-n form of Theorem 2 of the observer-axis
> record (`docs/deferential_realism_paper_v6.13.1.md` §3, stated there for |real seats| = 4
> and extended by enumeration to n ∈ {2,3,5} in its OQ-27 amendment), under the OQ-51
> variable-real-seat regime carried in-body by v8 (`docs/deferential_realism_paper_v8.md`
> §3.4). It resolves **OQ-195**. The engine object it characterizes is
> `grothendieck_cohomology:obstruction_from_vector/3` — the pure pairwise disagreement count
> over the `is_real_type`-filtered type vector; nothing here changes engine behavior.
> Machine verification: `audits/2026-07-02_oq195_general_n_gap/` (all pre-registered checks
> pass, n ≤ 40); engine-side witness: `prolog/tests/test_h1_spectrum.pl` (23 tests).

---

## Abstract

H¹ counts disagreeing pairs of real seats over a classification orbit. When k agreement
blocs partition n real seats into sizes n₁,…,n_k, H¹ = C(n,2) − Σ C(nᵢ,2). The reachable
set H(n) — the values H¹ can take at all, for any classification rule — is therefore a fact
of partition arithmetic, independent of the site's morphisms, the cascade's thresholds, and
the corpus. This note characterizes H(n) completely for every n: the minimum nonzero value
is n−1 (Theorem A — so {1,…,n−2} is forbidden at every cardinality, generalizing the
forbidden {1,2} of the four-seat record); the spectrum decomposes exactly into bands indexed
by the largest bloc, with a self-similar recursion generating all interior structure
(Theorem B); the interval between adjacent bands contains forbidden values precisely when
n ≥ j + 3 + C(j+1,2), and everything in it is forbidden (Theorem C, via an unconditional
band-floor lemma); and when the number of available type tokens T is smaller than n — live
today in the stakeholder frame, where authored seat counts reach 12 against T = 7 — the top
of the spectrum truncates, with the maximum at the balanced T-bloc partition (Theorem D).
The proofs are elementary and complete; a machine enumeration confirms every statement
exactly for n ≤ 40, with per-band bookkeeping (a union-level check provably cannot verify
the band classification) and negative controls.

---

## 1. Notation and the object

Let n ≥ 2 be the number of **real seats**: positions whose computed type is not `unknown`
(the OQ-51 rule — an `unknown` seat is N/A, neither agreeing nor disagreeing; with fewer
than two real seats the obstruction is undetermined, serialized null, never 0). A
classification of the n seats induces an **agreement partition** λ = (n₁ ≥ … ≥ n_k),
Σnᵢ = n: the blocs of seats assigned the same type. Following the record's notation,
C(m,2) = m(m−1)/2.

**Lemma 1 (three forms, and the engine's).** For any agreement partition λ of n:

> H¹(λ) = C(n,2) − Σᵢ C(nᵢ,2) = Σ_{i<j} nᵢnⱼ = (n² − Σᵢ nᵢ²)/2,

and this equals the value computed by `obstruction_from_vector/3`.

*Proof.* A pair of seats disagrees iff its members lie in different blocs. Total pairs
C(n,2); within-bloc pairs Σ C(nᵢ,2); their difference is the cross-bloc count, which
expands as Σ_{i<j} nᵢnⱼ, and 2·Σ_{i<j} nᵢnⱼ = (Σnᵢ)² − Σnᵢ². The engine predicate counts
exactly the i<j pairs with distinct types over the real-filtered vector — the cross-bloc
pairs. ∎

Define **H(n)** = {H¹(λ) : λ ⊢ n} — the reachable spectrum. Since every λ is realizable by
*some* assignment of types to seats (given at least k distinct tokens; §5), H(n) is the
spectrum of the arithmetic, not of any particular classifier: "the spectrum is determined
entirely by the number of observers, not by the site's morphism structure" (the record's
methodological note, now at every n).

---

## 2. Theorem A — the disagreement quantum

**Theorem A.** For n ≥ 2: 0 ∈ H(n); the minimum nonzero value of H(n) is n−1; hence
{1, …, n−2} ∩ H(n) = ∅ for n ≥ 3.

*Proof.* λ = (n) gives 0. For λ ≠ (n), let m < n be the largest part. The cross pairs
between the largest bloc and the rest alone number m(n−m); on 1 ≤ m ≤ n−1 the product
m(n−m) is minimized at the endpoints, where it equals n−1. So H¹(λ) ≥ n−1, with equality at
λ = (n−1, 1). ∎

The four-seat forbidden {1,2} is the n=4 instance. The general reading: **breaking
unanimity costs at least n−1 disagreeing pairs** — one dissenter disagrees with everyone.
Small nonzero H¹ values do not exist at any cardinality; a reader of stakeholder-frame
spectra (n up to 12 live) must expect the bottom gap to widen linearly, e.g. {1,…,10}
forbidden at n = 12.

---

## 3. Theorem B — the band decomposition

Classify λ by its largest part ℓ = n−j, j ∈ {0, …, n−1} (call j the **band index**; j = 0
is unanimity). Writing λ = (ℓ) ⊎ μ with μ ⊢ j and parts(μ) ≤ ℓ:

**Theorem B.** H(n) = ⋃ⱼ band_j(n), exactly, with

> band_j(n) = { jn − C(j+1,2) − d : d ∈ D(j; parts ≤ n−j) },

where D(j; parts ≤ c) = { Σ C(μᵢ,2) : μ ⊢ j, every part ≤ c }, D(0; ·) = {0}, and D obeys
the largest-part recursion D(j; ≤c) = ⋃_{p ≤ min(j,c)} ( C(p,2) + D(j−p; ≤p) ).

*Proof.* H¹(λ) = C(n,2) − C(n−j,2) − Σ C(μᵢ,2), and C(n,2) − C(n−j,2) = jn − C(j+1,2). The
classification by largest part is a partition of the set of partitions, so the union is
exact; the recursion is the same classification applied to μ. ∎

Two structural consequences. **(i) Self-similarity:** interior holes of H(n) are images of
holes of D(j) one level down — a value is a hole of the *spectrum* only when it is missing
from **every** band whose range covers it. (Worked instance: H(6) misses 10 because band 2
tops at 9, band 3 = {9, 11, 12} has a hole at 10 — the image of 2 ∉ D(3) — and bands ≥ 4
start at 12.) **(ii) The parts-constraint is load-bearing for the classification but
invisible to the union:** dropping "parts ≤ n−j" misassigns partitions to wrong bands while
producing the *identical union* — any violating (n−j) ⊎ μ is still a genuine partition of n,
just of a different band. A union-level check therefore cannot verify Theorem B; the machine
verification compares **per-band sets** and includes the unconstrained classifier as a
discriminating control (it must, and does, match all unions while mismatching bands).

---

## 4. Theorem C — where adjacent bands leave a gap

Band j's maximum is T_j = jn − C(j+1,2) (at d = 0, μ = 1ʲ), and T_j is increasing in j.
Write B_{j+1} = (j+1)(n−j−1). B_{j+1} is a **floor for every band beyond j** (the lemma
below) — and it is band j+1's exact minimum precisely when j+1 ≤ n−j−1, where the two-bloc
partition (n−j−1, j+1) realizes it. (For j+1 > n/2 that partition belongs to a lower band
and band j+1's true minimum is larger — e.g. n=6: band 4's minimum is 12, not B₄ = 8.
Theorem C only ever uses the floor, so this does not weaken it.)

**Band-floor Lemma (unconditional).** Every partition of n whose largest part is at most
n−j−1 satisfies H¹ ≥ (j+1)(n−j−1).

*Proof.* Let s ≤ n−j−1 be the largest part. If s ≤ n/2: Σλᵢ² ≤ s·Σλᵢ = sn, so
H¹ ≥ (n² − sn)/2 = n(n−s)/2 ≥ n²/4 ≥ (j+1)(n−j−1), the last step because two numbers
summing to n have product at most n²/4. If s > n/2: H¹ ≥ s(n−s) (cross pairs at the largest
bloc alone) = f(n−s) with f(x) = x(n−x) increasing on [0, n/2]; and j+1 ≤ n−s < n/2, so
f(n−s) ≥ f(j+1) = (j+1)(n−j−1). ∎

**Theorem C.** For n ≥ 3 and 1 ≤ j ≤ n−2: the open interval (T_j, B_{j+1}) contains an
integer **iff** n ≥ j + 3 + C(j+1,2); and when it does, **every** integer in it is
forbidden in the full spectrum H(n).

*Proof.* B_{j+1} − T_j = n − C(j+2,2), so the interval contains an integer iff
n − C(j+2,2) ≥ 2, and C(j+2,2) + 2 = j + 3 + C(j+1,2). For the forbidden-ness: any λ either
has largest part ≥ n−j — then it lies in a band i ≤ j and H¹(λ) ≤ T_i ≤ T_j — or largest
part ≤ n−j−1, and the Band-floor Lemma gives H¹(λ) ≥ B_{j+1}. Nothing lands strictly
between. ∎

j = 1 recovers the second gap {n, …, 2n−5} for n ≥ 5 — the record's "new interior gap at 5"
for five seats is the first instance. The thresholds grow quadratically in j, so each new
band gap needs a substantially larger n: gap after band 1 at n ≥ 5, after band 2 at n ≥ 8,
after band 3 at n ≥ 12 (live at the stakeholder frame's ceiling), after band 4 at n ≥ 17.

---

## 5. Theorem D — the token bound (live in the stakeholder frame)

Realizing k agreement blocs requires k distinct type tokens. The cascade emits T = **7**
real tokens (derived, not assumed: the type universe minus `unknown`, asserted against the
code in the machine verification). Define H(n, T) = {H¹(λ) : λ ⊢ n, k ≤ T}.

**Theorem D.** H(n, T) = H(n) for n ≤ T. For n > T, the spectrum truncates from above:
writing n = qT + r (0 ≤ r < T), the maximum of H(n, T) is

> H¹_max(n, T) = ( n² − (T−r)·q² − r·(q+1)² ) / 2 < C(n,2),

attained at the balanced T-bloc partition; the recursion of Theorem B restricts by carrying
a bloc-count budget (band j admits μ with at most T−1 parts).

*Proof.* For n ≤ T every partition is realizable. For n > T, among partitions with at most
T parts, Σλᵢ² is minimized by the balanced one (a standard smoothing step: moving a unit
from a larger part to a smaller strictly decreases Σλᵢ²), and fewer than T parts never
beats T balanced parts; minimum Σλᵢ² gives maximum H¹ by Lemma 1. Since (1ⁿ) is excluded,
C(n,2) is unreachable. ∎

Instance at the live ceiling: n = 12, T = 7 → q = 1, r = 5, Σλᵢ² = 2·1² + 5·2² = 22,
H¹_max = (144 − 22)/2 = **61**; values 62–66 = C(12,2) are unreachable for real stories.

---

## 6. The spectra, n = 2 … 12

Reachable sets (forbidden = complement in [0, C(n,2)]). Where the token bound (T = 7)
truncates, the second line gives the realizable set.

| n | reachable H¹ |
|---|---|
| 2 | {0, 1} |
| 3 | {0, 2, 3} |
| 4 | {0, 3, 4, 5, 6} |
| 5 | {0, 4, 6, 7, 8, 9, 10} |
| 6 | {0, 5, 8, 9, 11, 12, 13, 14, 15} |
| 7 | {0, 6, 10, 11, 12, 14, 15, …, 21} |
| 8 | {0, 7, 12, 13, 15, 16, …, 28} — with T=7: top truncates at 27 |
| 9 | {0, 8, 14, 15, 18, 20, 21, 23, 24, …, 36} — T=7: top 34 |
| 10 | {0, 9, 16, 17, 21, 23, 24, 25, 27, …, 45} — T=7: top 42 |
| 11 | {0, 10, 18, 19, 24, 26, 27, 28, 30, …, 55} — T=7: top 51 |
| 12 | {0, 11, 20, 21, 27, 29, 30, 32, 35, …, 39, 41, …, 66} — T=7: top 61 |

(Exact sets, including every interior hole, in
`audits/2026-07-02_oq195_general_n_gap/enumeration_results.json`.)

**Remark E (top contiguity — computational observation, no closed form claimed).** Above
its last interior hole each spectrum is contiguous; the enumeration gives the contiguity
thresholds (n → first value of the contiguous top run): 5→6, 6→11, 7→14, 8→15, 9→23,
10→27, 11→30, 12→41. Equivalently these are C(n,2) minus the last initial run of
consecutively-achievable triangular sums; the sequence follows the self-similar recursion
and does not appear to admit a simple closed form. Declared open as arithmetic, with no
engine consumer waiting on it.

---

## 7. Engine corollary, and the two frames

**Engine corollary.** For a constraint with NReal real seats: the reachable `h1_band`
spectrum is exactly H(NReal) (H(NReal, 7) where NReal > 7); NReal < 2 ⇒ null (undetermined
— never 0); the contextuality-fraction denominator C(NReal,2) is consistent with Lemma 1.
Any observed value outside the spectrum for its real-seat count is a bug witness, and any
*reader* that treats an in-gap value (e.g. H¹ = 2 at four real seats) as possible is
misreading the arithmetic. Conversely: band 2 at three real seats is legitimate — the
cardinality must always be read alongside the band.

**One law, two frames.** The spectrum law is frame-independent — it constrains any
disagreement count over any gauge set (in v8 vocabulary: positions over one content).
Today the engine has two such frames:

- **Observer frame:** the canonical site — n = 4 fixed (or 6 under the `canonical_6` site
  mode), so only the n ≤ 6 rows above apply, and the token bound never binds.
- **Stakeholder frame:** named stakeholder seats authored per story
  (`stakeholders[]` in the generation schema; roles {agenda_setter, beneficiary, payer,
  excluded, observer}; per-seat types computed by
  `stakeholder_seats:dr_type_for_stakeholder/3`). Seat counts are **variable and larger**:
  the live census at this writing spans 3–12 seats per story across the three legs
  (re-runnable: `python3 python/audits/oq195_h1_spectrum_check.py --census`; as-of-stamped
  output in the audit dir). No aggregate currently consumes these per-seat types as a
  disagreement measure; if one ever does, this note is its spectrum law, the OQ-51 rule
  carries over unchanged (zero-seat stories — a known generation-path artifact, OQ-202 —
  read null, never 0), and the existing unanimity flag
  (`stakeholder_seats:consensus_provenance/2`) is the H¹ = 0 special case of exactly such a
  measure.

---

## 8. Verification record

Pre-registered acceptance criteria (`audits/2026-07-02_oq195_general_n_gap/PROPOSAL.md`,
registered before any run; all BLOCKING): per-band exact match of brute-force enumeration
vs the Theorem-B/D recursions for n = 2..40, both variants; the unconstrained-classifier
discriminating control (identical unions, mismatched bands — witnessed at 38 of 39 n
values); Theorem A minima; record match against the four- and five-seat published sets;
a perturbed-predictor negative control (flagged); the Theorem-C biconditional with the
every-value-forbidden strengthening (zero exceptions, all n ≤ 40, all j); T derived from
the code's type universe and asserted equal to 7. Engine-side:
`prolog/tests/test_h1_spectrum.pl` — exhaustive vectors at n = 2–4, constructive
realization of every ≤7-bloc partition at n = 5–12 (complete for H(n,7)), `unknown`-padded
vectors exercising the OQ-51 filter at n = 12, and two negative controls; 23/23 passed.

*CC0 Universal.*
