# Cross-corpus signature-liveness sweep (2026-06-10)

Method: overlay `config:param(corpus_path, <archive>)` (retract default, assert; resolves against
`prolog/`), `corpus_loader:load_all_testsets`, classify every `corpus_constraint/1` via the CURRENT
`signature_detection:constraint_signature/2`, per-constraint `catch` for throws. Non-recursive glob
= top-level `.pl` only (run-tag subdirs excluded). Enabled by the 2026-06-09 fail-closed fix
(OQ-89): every old/under-vectored story abstains to `unknown` instead of throwing — **0 throws on
all four corpora** (bucket sums equal the loaded count exactly).

Caveat: counts are LIVENESS evidence (can the signature fire), NOT prevalence — archives are
bait-era (kernel_v1, OQ-70) / ID-reuse (original_v6, OQ-25) and 67–81% abstain under the current
schema. Per build_discipline "Unwired ≠ worthless", firing-anywhere feeds the value question; it
does not by itself rule a dark signature cruft.

| signature                   | live(34) | kernel_v1(1106) | original_v5(702) | original_v6(3380) | live? |
|-----------------------------|---------:|----------------:|-----------------:|------------------:|-------|
| unknown (abstain)           |        9 |             739 |              572 |              2550 | —     |
| false_ci_rope               |        8 |             273 |              115 |               290 | LIVE  |
| coupling_invariant_rope     |        4 |              57 |                0 |               104 | LIVE  |
| constructed_high_extraction |       13 |               1 |                0 |                17 | LIVE  |
| natural_law                 |        0 |              26 |                0 |               404 | LIVE (narrow on live) |
| false_summit_mountain       |        0 |              10 |                0 |                15 | LIVE (narrow on live) |
| false_natural_law           |        0 |               0 |               15 |                 0 | LIVE (only v5) |
| coordination_scaffold       |        0 |               0 |                0 |                 0 | DARK everywhere |
| piton_signature             |        0 |               0 |                0 |                 0 | DARK everywhere |
| constructed_low_extraction  |        0 |               0 |                0 |                 0 | DARK everywhere |
| constructed_constraint      |        0 |               0 |                0 |                 0 | DARK everywhere |
| ambiguous                   |        0 |               0 |                0 |                 0 | DARK everywhere |

Totals: 34 / 1106 / 702 / 3380 (each row-sum matches → 0 throws, 0 no_sig).

## Reads
- **7 of 12 signatures fire somewhere across ~5,222 stories** → LIVE. Five that were zero on the
  live n=34 are resolved live-but-narrow: `natural_law` (404 on v6), `false_summit_mountain`
  (kernel_v1+v6), `false_natural_law` (v5).
- **5 DARK across all four corpora:** `coordination_scaffold`, `piton_signature`,
  `constructed_low_extraction`, `constructed_constraint`, `ambiguous`. Strongest cruft-candidates,
  but NOT a verdict — next discriminator is the reference-exemplar control (`constraint_instances.pl`:
  SI-units→scaffold, QWERTY→piton) + the value question. The three constructed_*/ambiguous are
  intermediate/fallback bands (data lands in constructed_high or is overridden) = narrow-data, not
  proven dead-code.
- **`natural_law` = 404 on original_v6** reproduces the OQ-43 "404 NL on testsets_3000" count — the
  current engine matches the historical figure (consistency check).
- **`false_natural_law` = 0 on kernel_v1** despite OQ-70 recording FNL as the bait-confounded
  dominant signature on kernel_v1's ancestors → corroborates that the OQ-70 bait clause removal
  worked (bait gone ⇒ FNL collapses, genuine natural_law surfaces).
