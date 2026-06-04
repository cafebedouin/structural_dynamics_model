# reading_diff within-kernel census (OQ-59 #3)

**Tool:** `prolog/reading_diff_census.pl` (delegates to `prolog/reading_diff.pl`, authored-cells-only).
**Raw rows:** `audits/2026-06-03_reading_diff_census/reading_diff_census.tsv` (one row per within-kernel reading-pair).
**Corpus:** committed testsets at `90bb5a6b` — **803 testsets, 189 multi-reading kernels, 615
within-kernel reading-pairs.** (A 256-testset generation run was in flight at census time and is
**excluded** — it is uncommitted. Including it, 1059 testsets / 864 pairs, gives 40.9 / 52.0 / 7.2 %,
so the distribution below is insensitive to the in-flight set. Re-run after that generation lands.)

For each unordered within-kernel reading-pair, `reading_diff` is run under the declared key chain
`[exact, fuzzy_agent_power]` and the pair is classified by its order-independent stability verdict.

## Verdict distribution (615 committed within-kernel pairs)

| verdict | count | % | meaning |
|---|---|---|---|
| `key_fragile` | 330 | **53.7%** | binocular/undersampled flips with the alignment seat |
| `robustly_binocular` | 243 | **39.5%** | ≥1 disparity under *every* key → genuine depth, preserve |
| `robustly_undersampled` | 42 | **6.8%** | 0 disparity under every key → coverage gap, NOT convergence |

**Headline:** `key_fragile` is the plurality. For most sibling-reading pairs, *whether the two
readings disagree at all* is determined by how you define "the same vantage" (exact (P,T,E,S) vs
same-`agent_power`). The cyclopean seat the operator was built to expose operates **corpus-wide**,
not only on the westphalian case that motivated it — over half the corpus's reading-pairs are
undersampled under exact alignment but become binocular once vantages are aligned on agent_power.

`robustly_binocular` (~40%) is the set where preserving both readings is unambiguously right: they
disagree on a shared vantage under every key. `robustly_undersampled` (~7%) are the genuine
coverage gaps — the readings barely sample the same (P,T,E,S) cells, so the prior task there is
coverage, not interpretation.

## Notable cases

**Strongest binocular (3 exact-disparity vantages):** `licensing_statute_mandate`
(graduated_access_filter / public_safety_coordination); `fifth_republic_constitution`
(hyper_presidential / parliamentary_constraint); `derivative_work_statutory_boundary`
(enclosure / hybrid_carveout); `abrahamic_covenant` (isaac_covenant / land_promise). These are
multi-cell head-to-head disagreements — depth no single reading discloses.

**Coverage gaps (all of a kernel's pairs robustly_undersampled):** `rbio_practice_norm_complex`,
`nafta_jurisdictional_boundary`, `irc_469_material_participation_kernel` (3/3 pairs each). Their
readings are authored over disjoint vantage sets — you cannot read disparity off eyes pointed at
different scenes; author shared vantages before any binocular claim.

**`westphalian_sovereignty` (5 readings, 10 pairs) — internal structure:** the 4 pairs involving
`absolute_sovereignty` are all `robustly_binocular` (it genuinely disagrees with every sibling on a
shared vantage); the graded-family pairs among themselves (conditional / governance_quality /
graduated / r2p) are mostly `key_fragile`; `graduated_sovereignty / r2p_reading` is
`robustly_undersampled`. So `absolute_sovereignty` is the binocular anchor of the kernel, and the
graded readings differ from each other mainly by sampling, not by shared-vantage disagreement.

## Reproduce

```
cd prolog && swipl -g "[stack], corpus_loader:load_all_testsets, [reading_diff_census], \
  reading_diff_census:census_to('../audits/2026-06-03_reading_diff_census/reading_diff_census.tsv'), \
  reading_diff_census:run_census, halt" -t "halt(1)"
```
(On a clean checkout of `90bb5a6b` this yields the 615-pair committed census above; a working tree
with uncommitted generated testsets will score them too — filter to committed readings for the
citable figure.)
