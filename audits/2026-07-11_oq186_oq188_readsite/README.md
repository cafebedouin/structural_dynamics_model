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

## Post-close reconciliations (operator review, 2026-07-11)

**R1 — census arithmetic vs recon.** Exact matched-bucket histogram (institutional seat,
same manifest), reconciling the recon's summary against the final 103/105:

| bucket | recon said | final census | note |
|---|---|---|---|
| d=0.12 (agenda_setter) | 101 | 101 — all fire | the recon population |
| d=0.25 (beneficiary) | (not enumerated) | **2 — both fire** | the 2 "extra" flippers: the symmetric straddle partner; recon folded these into its "~15 non-role-constant" catch-all |
| d=0.72 (observer) | (not enumerated) | 1 — silent | matched, nearest-alt same sign |
| d=0.85 (payer) | (not enumerated) | 1 — silent | matched, nearest-alt same sign |
| unmatched | "~15 non-role-constant" | 16 (0.07, 0.15×6, 0.20×2, 0.35, 0.45×2, 0.55, 0.68, 0.70, 0.88) | enumerated in census_oq188.log |
| null | 9 | 9 | excluded from denominator (OQ-51 idiom) |

Sum: 101+2+1+1+16+9 = 130 ✓. **The d=0.15 question is settled by the predicate, not
judgment:** the five role constants are 0.12/0.25/0.72/0.85/0.90; 0.15 matches none at
1e-6, so it is `unmatched` by construction — surfaced, denominator-shrinking, never
silently unflagged (PREREG Block 1 said exactly this pre-run).

**R2 — the grep-0 positive controls.** "`coordinated rather than independent operation`
greps 0 in the regenerated dispositional_reading report" is an absence claim; the same
grep (same pattern, same encoding) was run against places the phrase MUST appear:

- current `evaluative_convergence.json`: **2 hits** (the clean sets network_2638bfb4 /
  network_a6b8a722 keep the original ruling);
- pre-fix artifact (`evaluative_convergence.before.json`, saved before regeneration):
  **13 hits**, of which **1 in the `character_education_institutions` set** — the exact
  set whose report carried the witnessed defect;
- report level, same render path: regenerated `distributed_verification_report.md`
  (a network_2638bfb4 member) → **1 hit**; `dispositional_reading_report.md` → **0**.

So the 0 is the fix discriminating, not a dead grep.

**R3 — fix-on-sight commit hygiene.** As originally landed, the ledger key fix WAS folded
into the OQ-186 commit — the operator's review caught it. History was split locally
(nothing had been pushed): the fix now lives in its own commit `1bcb7421`
("fix(tensions_ledger): edge lines used wrong serialized keys…") carrying its own
two-sided witness in the message (before: `? [explicit; strength ?]…`; after: real
constraint_id/strength render), with the OQ-186 marker landing separately on top.
Equivalence witness: `git diff oq-backup HEAD` is byte-empty (oq-backup = the pre-split
head). **Disclosure found during the split:** `bb7cfcbe` (the OQ-188 commit) swept in one
pre-existing uncommitted hunk in `enhanced_report.py` — the HOW TO READ d-derivation text
rewrite ("d is DERIVED per seat — precedence: authored override → …" replacing the older
"d is a config lookup" wording), which predates this session (visible as `M
python/enhanced_report.py` in the session-start status). It is content-accurate and
topically adjacent (it is the d-precedence correction the OQ-188 legend sits beside), so
it was left in place rather than unpicked — flagged here for the operator's awareness.

## Phase 2–4 artifacts (this audit dir records decisions; code lands in-repo)

- OQ-188: `enhanced_report.py` `_role_flip_caveat()` legend sentence + `‡` glyph on fired
  seats' types (Live/Batch Type lines); `tensions_ledger.py` per-position glyph + one header
  legend line; behavior-preserving `config.pl:156-160` straddle comment (separate commit).
- OQ-186: `enhanced_report.py` `_edge_is_common_cause` + Independence column + legend;
  `evaluative_convergence.py` evidence booleans + defensibility downgrade + XCON suppression;
  `tensions_ledger.py` edge marker.
- Regression: `prolog/tests/test_oq186_common_cause_clique.pl`;
  `python/tests/test_role_flip_flag.py`.
