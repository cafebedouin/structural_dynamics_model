# B/C Coupling Audit

## Pass 1 — Coupling Existence

| Slice | n_extractive | n_edges | ⚠degen | homophily | n_components | lcc | avg_path |
|---|---|---|---|---|---|---|---|
| U_4 | 18 | 1 | ⚠ | 0.037 | 17 | 2 | 1.000 (exact) |
| U_3_imm | 53 | 11 |  | 0.981 | 45 | 3 | 1.000 (exact) |
| U_3_civ | 9 | 1 | ⚠ | 0.040 | 8 | 2 | 1.000 (exact) |
| U_1 | 41 | 15 | ⚠ | 0.998 | 29 | 3 | 1.000 (exact) |
| U_2 | 72 | 24 |  | 0.997 | 53 | 3 | 1.000 (exact) |
| organized | 17 | 1 | ⚠ | 0.171 | 16 | 2 | 1.000 (exact) |
| U_1_nat | 60 | 26 |  | 1.000 | 40 | 4 | 1.333 (exact) |
| U_4_glob | 13 | 2 | ⚠ | 0.541 | 11 | 2 | 1.000 (exact) |
| org_nat | 15 | 1 | ⚠ | 0.402 | 14 | 2 | 1.000 (exact) |
| U_3_nat | 58 | 18 |  | 0.994 | 42 | 4 | 1.333 (exact) |

**Degenerate slices (n < 50):** ['U_4', 'U_3_civ', 'U_1', 'organized', 'U_4_glob', 'org_nat']

**Variation (non-degenerate slices):**
  - homophily range: 0.019
  - n_components CV: 0.127
  - avg_path_length range: 0.333

**Verdicts:** homophily=tight, n_components=tight, avg_path_length=tight

> **Sub-claim 1 (coupling exists): False**

## Pass 2 — Asymmetry Test

- Forward (PTES distance → structural distance): r=0.530, p=0.280
- Reverse (PTES proximity → structural-profile similarity): r=0.090, p=0.556

**Distance sensitivity (forward direction):**
  - hamming: r=0.530
  - weighted: r=0.530
  - learned: r=0.135

**Confound diagnostic** (structural sim vs classification agreement): r=0.121
*High value means structural-profile similarity and classification agreement move together, partially inflating the reverse direction.*

> **Sub-claim 2 (asymmetry): forward_dominant**

## Pass 3 — Decoupled Cases

Mountains never extractive at any slice: **True**
*Metric 1 verdict: `indeterminate_design_consistent` — Metric 1 (homophily) for mountains is ambiguous: consistent with decoupling hypothesis AND Axiom 3 apparatus design — cannot distinguish readings from this metric alone. Metrics 2 & 3 are the cleaner test.*

⚠ **Coverage artifact**: 100% of mountain-slice appearances concentrate at one slice (U_4). Only 0 slice(s) have n_active ≥ 20. The CV is driven by coverage variation (presence/absence) not structural coupling variation. Verdict based on coverage-adequate slices only.

### Mountain Subset (Metrics 2 & 3, all mountains active at each slice)
| Slice | n_active | n_extractive | n_components | lcc | avg_path |
|---|---|---|---|---|---|
| U_4 | 4 | 0 | 4 | 1 | n/a |
| U_3_imm | 0 | 0 | 0 | 0 | n/a |
| U_3_civ | 0 | 0 | 0 | 0 | n/a |
| U_1 | 0 | 0 | 0 | 0 | n/a |
| U_2 | 0 | 0 | 0 | 0 | n/a |
| organized | 0 | 0 | 0 | 0 | n/a |
| U_1_nat | 0 | 0 | 0 | 0 | n/a |
| U_4_glob | 0 | 0 | 0 | 0 | n/a |
| org_nat | 0 | 0 | 0 | 0 | n/a |
| U_3_nat | 0 | 0 | 0 | 0 | n/a |

Mountain CV n_components: n/a, avg_path: n/a, combined: n/a
Extractive CV n_components: 0.588, avg_path: 0.132, combined: 0.360
Ratio (mountain/extractive): n/a

> **Sub-claim 3 (decoupled mountains): coverage_artifact_indeterminate**

## Pass 4 — Synthesis

> **Combined verdict: coupling_hypothesis_fails**

*Structural properties approximately observer-invariant. Coupling reading not supported. Paper 2 spine requires different empirical grounding.*

## Methodological Self-Report

- **Metric 1 source**: precomputed `type_mixing_matrix` (idea_site pass2 coupling topology).
- **Metrics 2+3**: built from contamination network edges in pipeline_output.json.
- **Homophily**: edge-centric EE/(EE+EN); only explicit contamination edges.
- **Degenerate threshold**: n_extractive < 50.
- **Pass 2 reverse**: structural-profile (neighbor-extractive-fraction vectors), not classification agreement, to reduce apparatus-determinism confounding.
- **Pass 3 variation**: coefficient of variation (std/mean) normalizes 6.8× size disparity.
- **Mountain Metric 1 ambiguity**: never-extractive consistent with both decoupling hypothesis and Axiom 3 design; readings indistinguishable from this metric alone.
- **Mountain coverage artifact**: mountains are only classified at analytical/universal observer positions (U_4 dominates with >80% of coverage). Mountain CVs computed on coverage-adequate slices (n_active ≥ 20) only. Verdict overridden to `coverage_artifact_indeterminate` when fewer than 2 adequate slices exist.
- **Mountain subset**: 425 constraints including 14 false-summit candidates.
- **Slice family**: idea_site 10 working slices only.
- **Alternatives not tested**: clustering coefficient, betweenness centrality.