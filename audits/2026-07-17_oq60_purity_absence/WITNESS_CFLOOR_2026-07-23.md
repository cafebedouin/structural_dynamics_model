# C-FLOOR witness (2026-07-23) — mechanism 5, THE live commit

Edit: `boltzmann_floor_for/2` clause 3 removed (`boltzmann_compliance.pl`) — absent
override + absent `coordination_type` now FAILS the floor instead of fabricating
`boltzmann_floor_default = 0.05`. Wiring (C-LATENT, already landed): floor fails ⇒
`excess_extraction` fails ⇒ EX subscore `unknown` ⇒ `purity_score` `unknown` ⇒ JSON `null`.
`config.pl` param annotated RETIRED-from-engine; `docs/logic_extensions.md` fabricated-default
paragraph updated (offset half still open as OQ-41).

## RED→GREEN

`purity_absence_floor` unit: `m5_absent_coordination_type_floor_fails` RED at pre-fix HEAD
(pasted: "1 test failed, 2 passed" — the two authored-path tests GREEN before AND after,
witnessing override/coordination-type floors unchanged). Post-edit: **17/17 green** across all
three units. (One test repair en route: the preflight injection test's non-target control was
corpus row `epistemic_collapse` — itself an m5 flip row, legitimately unknown post-C-FLOOR;
replaced with a synthetic scored control carrying authored extractiveness + coordination_type.)

## Pre-registered delta-mean screens (computed from census BEFORE the run)

| leg | scorable n | mean before → predicted after | predicted Δ |
|---|---|---|---|
| testsets | 164→153 | 0.5490640244 → 0.5450032680 | −0.0040607564 |
| haiku | 494→492 | 0.4913613360 → 0.4915548780 | +0.0001935420 |
| flash | 748→668 | 0.5783175134 → 0.5711254990 | −0.0071920144 |
| kernel_v1 | 1104→1102 | 0.4810205314 → 0.4812507562 | +0.0002302248 |

HANDOFF whole-story prediction (operator): flash scorable-mean must move **DOWN** — it does
(−0.0072). Match window: |Δ| < 1e-5 vs measured (JSON 6-decimal rounding + accumulation order).

## The witness: per-constraint census join, ALL FOUR legs — CLEAN

`classify_corpus` on the edited engine, serialized, corpus md5 fingerprint asserted stable
around every leg (`cfloor_run.log`). testsets joined against the same-day post-C-LATENT v2
census (`census_testsets_v2_2026-07-23.tsv`, pop 199, gate-pass victims m1–m4 = 0 / m5 = 11,
positive control all-5-branches + purity=unknown); other legs against their pinned TSVs.

```
[testsets]        flips=11 over_flip=[] under_flip=[] value_diff=[]  screen n=153 mean 0.5450033660 OK
[testsets_haiku]  flips=2  over_flip=[] under_flip=[] value_diff=[]  screen n=492 mean 0.4915549390 OK
[testsets_flash]  flips=80 over_flip=[] under_flip=[] value_diff=[]  screen n=668 mean 0.5711255269 OK
[kernel_v1]       flips=2  over_flip=[] under_flip=[] value_diff=[]  screen n=1102 mean 0.4812508113 OK
[money pair] conceptual_framework_reading: 0.972 -> None
[money pair] vocabulary_collision_reading: 0.948 -> None
```

**Flip set = exactly the census `disposition==unknown` rows on every leg (11/2/80/2); zero
over-flips, zero under-flips, zero value changes on scored rows, membership exact.** Neither
HALT branch (over-flip ⇒ census falsified / under-flip ⇒ missed caller) fired. This join also
**discharges the C-LATENT OPEN legs** (haiku, kernel_v1): every non-flip scored row matches its
pre-C-LATENT census value at JSON precision, so m1–m4 changed nothing there either.

## Declared downstream ripple (byte-diff vs pre-C-FLOOR dumps, testsets + flash)

Full-row byte-diff against the C-LATENT dumps shows, BEYOND the flip rows, diffs confined to
**corpus-relative diagnostic layers**: `wasserstein_*` (ensemble pairwise), `maxent_*`
(empirical profiles computed from corpus statistics — `maxent_classifier.pl:140-141` — plus
signature overrides), `arakelov_height(_context)`, `signature_pressure`,
`contamination_network` (FPN neighbors of flip rows), `drift_events` (floor-dependent).
Attribution verified at the consumer, not assumed: MaxEnt consumes corpus profiles +
`constraint_signature`; wasserstein/arakelov are ensemble-indexed; 93 rows' inputs changed ⇒
fits move ⇒ near-boundary shadow values shift.

Token-level drill (pasted in session):
- **`classifications` (headline dr_type per context): changed on ZERO rows corpus-wide.**
- `maxent_top_type` (shadow) flips on 7 testsets + 5 flash near-boundary rows; on 9 of these
  the maxent-divergence alert downgraded ⇒ `verdict_join`/`diagnostic_verdict` red→yellow
  (alert-severity channel, not base verdict).
- One flash **gate-fail raw-m5 victim** (`acceptable_risk_energy__option_value_preserving`,
  census sentinel, m5=1): signature `false_ci_rope → constructed_constraint` — its only FCR
  failure was `excess_above_floor` computed off the fabricated floor; with the floor gone the
  fabricated failure goes too. This is the fix operating on a gate-fail row, same mechanism.

haiku/kernel_v1 have no pre-C-FLOOR dumps (C-LATENT ran testsets+flash per witness-economy),
so the equivalent ripple there is expected but unwitnessed at byte level; their purity join
(above) is the plan's witness and is clean.

## Artifacts

`cfloor_witness.py` (driver + join), `cfloor_bytediff.py`, `cfloor_run.log`,
`census_testsets_v2_2026-07-23.tsv`, `oq60_cfloor_{testsets,haiku,flash,kernel_v1}.json.gz`
(post-C-FLOOR dumps = Phase-2 baseline).
