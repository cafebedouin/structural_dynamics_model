# OQ-188 + OQ-186 read-site audit — Phase-1 evidence and decisions

Pre-registration: `PREREG.md` (commit `57159a36`, predates every run below — witness:
`git log --follow` on this directory). Execution date: 2026-07-11. All runs against
`outputs/pipeline_output.json` manifest `run_at=2026-07-05T19:55:12Z n=130 commit=ea8ed72
dirty=True` (cited per the manifest convention).

## 1. A/B probe (`probe_oq186.pl` → `probe_oq186.log`)

Topic A (3 co-authored slices: shared victim + beneficiary, ε 0.68/0.69/0.68) formed the
**full 3-clique** — every pair got a shared-agent edge:

```
oq186_a1 neighbors: [neighbor(oq186_a2,0.3,shared_beneficiary),neighbor(oq186_a3,0.3,shared_beneficiary)]
oq186_a2 neighbors: [neighbor(oq186_a1,0.3,shared_beneficiary),neighbor(oq186_a3,0.3,shared_beneficiary)]
oq186_a3 neighbors: [neighbor(oq186_a1,0.3,shared_beneficiary),neighbor(oq186_a2,0.3,shared_beneficiary)]
```

Topic B (3 pairwise-distinct-agent extractive constraints) formed **zero edges** (all three
neighbor lists `[]`). Topic A doubles as the positive control: the probe's empty B-lists are
a filter result, not a dead probe.

**Pre-registered verdict: outcome (a).** The machinery forms the edges and has no node-
independence notion (the A-clique is indistinguishable in kind from a genuinely-distinct
clique); the defect is the corroboration READ → the read-site caveat proceeds. Detail
confirmed: `deduplicate_neighbors` collapses the pair's shared_victim edge into the
shared_beneficiary label (one edge per pair), so "shares BOTH sides" is not expressible
per-edge Prolog-side — the discriminator lives Python-side as pre-registered.

## 2. OQ-188 fire-rate census (`census_oq188.py` → `census_oq188.log`)

Root from serialized params: d\* = 0.16418. Institutional seat buckets (n=130):
**matched=105, unmatched=16, canonical=0, null=9**; unmatched d values surfaced in the log
(0.15 ×6 as recon predicted, plus 0.07/0.20/0.35/0.45/0.55/0.68/0.70/0.88). Flip predicate
fires **103/105 matched = 98.1%** (agenda_setter→beneficiary ×101, beneficiary→agenda_setter
×2).

**Pre-registered gate: ≥50% → STANDING type-level form** (one legend sentence + per-line `‡`
glyph; never repeated per-line caveat text).

Other seats (all buckets reported per Pattern 6): analytical matched=120, fires 0 (observer
0.72 / nearest-alt 0.85, same sign); moderate matched=0 (all unmatched — d 0.65/0.68/0.70
are not role constants); powerless matched=57, fires **1**
(`performance_legitimacy_flat_control`, d=0.25 beneficiary ↔ agenda_setter).

**Declared deviation from PREREG Block 3 (extension, not a criterion change):** PREREG pinned
the glyph to the institutional per-seat type; the census witnessed 1 powerless firing of the
same zero-free-parameter predicate. Suppressing it would silently unflag a witnessed firing
(Pattern 6), so the implementation computes the flag per-seat and glyphs ANY fired seat; the
legend sentence keeps the institutional straddle as its worked example. Predicate, tolerance,
and branch gate are untouched.

## 3. ε-clause discrimination census (`census_eps_clause.py` → `census_eps_clause.log`)

22 unique live agent-edge pairs (0 dangling, 0 null-ε). Both-sides pairs: **1** —
`dispositional_reading` ↔ `moral_causation_locus_flat_control`, ε 0.68/0.68 (inside margin).
Non-both-sides: 21, of which 9 (42.9%) inside |Δε| ≤ 0.02 — **not a majority**.

**Pre-registered gate: KEEP the ε clause.** Note the confirmatory detail: the one live
both-sides + ε-close pair IS the witnessed OQ-186 instance family (`moral_causation_locus`).

## Phase 2–4 artifacts (this audit dir records decisions; code lands in-repo)

- OQ-188: `enhanced_report.py` `_role_flip_caveat()` legend sentence + `‡` glyph on fired
  seats' types (Live/Batch Type lines); `tensions_ledger.py` per-position glyph + one header
  legend line; behavior-preserving `config.pl:156-160` straddle comment (separate commit).
- OQ-186: `enhanced_report.py` `_edge_is_common_cause` + Independence column + legend;
  `evaluative_convergence.py` evidence booleans + defensibility downgrade + XCON suppression;
  `tensions_ledger.py` edge marker.
- Regression: `prolog/tests/test_oq186_common_cause_clique.pl`;
  `python/tests/test_role_flip_flag.py`.
