# OQ-207 — Stakeholder-frame H¹ census (three live legs + kernel_v1)

**Date:** 2026-07-12. **Code state:** commits `8048a568` (engine+tests) +
`96047f19` (JSON emission); census run at working tree of those commits.
**Script:** `python/audits/oq207_stakeholder_h1_census.py` (spectrum machinery
imported from `oq195_h1_spectrum_check.py`, not forked).

## Design (D8 — two-stage)

Stage 1: serialized one-shot swipl per corpus (`corpus_path` overlay via
retractall+assertz → `cache_registry:clear_all_caches` (D7) → load → dump
`census_input_<leg>.json` of per-constraint records). Stage 2: the Python
assertion layer consumes ONLY those artifacts. The artifact carries the
consensus verdict class per record because `(h1, n_seats, n_real)` alone
cannot distinguish the `seats_untyped` stratum from cell (b) (both are
n_real=0 with seats present).

## Results (all four corpora; every rate with raw count + n_in_domain)

| leg | n_corpus | numbered H1 | null | zero-seat | single-real (cell a) | cell (b) | mixed plural |
|---|---|---|---|---|---|---|---|
| testsets | 135 | 108 | 27 | 26 | 0 | **1** | 19 |
| testsets_haiku | 960 | 484 | 476 | 466 | 10 | 0 | 66 |
| testsets_flash | 960 | 724 | 236 | 212 | 21 | **3** (+1 cell-a in cells) | 129 |
| kernel_v1 | 1106 | 0 | 1106 | 1106 | 0 | 0 | 0 |

- **Spectrum violations: 0** on every leg — all 1,316 numbered H¹ values lie
  inside the proven H(n_real) (T=7-bounded; `docs/h1_gap_spectrum_general_n.md`).
- **Null-rule violations (OQ-51 two-sided): 0**; **incoherent mcc cells: 0**;
  **table_exhausted (n_real>12): 0**.
- H1-by-n_real distributions: `census.json` → `legs.<leg>.h1_distribution_by_n_real`.

## Controls

1. **kernel_v1 all-null (negative domain):** n=1106, every record
   `(null, null, 0, 0, no_agent_seats)` — PASS.
2. **Planted violation (D5/D8):** doctored COPY of `census_input_testsets.json`
   (`ability_ceiling_reading` h1→1 at n_real=5; 1 ∈ forbidden {1..n−2}) run
   through the SAME assertion path — FLAGGED. The probe fires on the artifact
   it audits, not on post-read fabrication. Doctored copy kept:
   `census_input_testsets.DOCTORED.json`.
3. **Zero-seat vs OQ-202 mint strata:** 26/466/212 observed = 26/466/212 at
   mint; drift 0 on every leg.

## KILL CONDITION (D4): TRIGGERED

Cell (b) — all agent seats typed `unknown`, consensus verdict reads unanimous —
has nonzero LIVE population: **4 stories**.

- `testsets`: `livelihood_security_reading` (unanimous_no_excluded_seats)
- `testsets_flash`: `fair_use_statutory_exception__transformative_right_reading`
  and `gdpr_article_3_scope__market_access_reading` (both
  **manufactured_consensus_candidate** — the flag names excluded seats as
  manufacturing a consensus that is actually just untypeable seats: the
  sharpest form of absence-read-as-agreement), and
  `second_amendment_scope__civic_right_reading` (unanimous_no_excluded_seats).

Per the operator's pre-committed D4 ruling (2026-07-11): tightening
`consensus_provenance/2` (require ≥2 real-typed seats for unanimity) is now an
**OBLIGATORY follow-up commit** — output-changing, its own witness. Minted as
an OQ in ISSUES.md (see OQ-207 close entry for the number).

## Additional finding: the mixed plural([T,unknown]) cell

The plan's D4 table named two divergence cells; a third is reachable and
heavily populated: `plural([T, unknown])` — a real type beside seats typing
literal `unknown` (consensus counts `unknown` as a type token; the H¹ filters
it). Live counts: 19 / 66 / 129 (testsets / haiku / flash). H¹ follows the
real seats only (0 if they agree, null if <2), so a "plural" verdict can sit
beside H¹=0 or null coherently — but the verdict's disagreement reading is
partly driven by an absence token. In-scope for the same tightening ruling as
cell (b) (the follow-up OQ names both).

## Files

- `census.json` — full assertion-layer output (verdict PASS).
- `census_input_<leg>.json` — the four stage-1 artifacts (read-site inputs).
- `census_input_testsets.DOCTORED.json` — the planted-violation control input.
- `census_run.log` — assertion-layer rerun over the committed artifacts
  (`--skip-dump`, exit 0).
