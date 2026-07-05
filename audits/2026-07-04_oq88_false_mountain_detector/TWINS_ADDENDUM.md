# OQ-88 ADDENDUM — twin legs (testsets_haiku / testsets_flash), 2026-07-05

Extension of the 2026-07-04 sweep to the two twin legs, operator-requested. Same pinned
predicate, same manifest maps, same discriminator tiers (PROPOSAL.md); per-corpus liveness
controls (claimed-type distribution + alert-channel coverage) built into the extended script
(`sweep_corpus` mode). Fresh `classify_corpus` per leg at HEAD `8a529c73` (dirty), 960/960 each,
single-model provenance fingerprint enforced (`claude-haiku-4-5` / `gemini-2.5-flash`);
witnesses: `classify_twins.log` / `.stderr.log`. Raw sweeps: `oq88_sweep_testsets_haiku.json`,
`oq88_sweep_testsets_flash.json`.

## Headline: D fires on ZERO twin stories — by corpus construction, not detection

Both twins are **100% kernel-linked by construction**: 960/960 files in each carry an in-file
`cs_kernel_id` fact (seed-pipeline `stamp_kernel_linkage`), including 21/21 (haiku) and 48/48
(flash) of the alert-firing false-mountains. There is no flat-routed story in either twin, so
Layer A can never conjoin and D=0 is composition, not a negative result. The twins measure the
**Layer-B rate and the discriminator on all-kernel corpora**; they cannot exercise D.

**Second Layer-A instrument found.** The manifest walk reads every twin firing as
routing-unknown (seed-pipeline stories have no `outputs/kernel_manifests/` lineage) — the
fail-closed rule handled this correctly, but the in-file `cs_kernel_id` fact is a decisive
routing source the manifest-only Layer A misses. Any future wiring of D should read
`kernel-routed ⟺ build_constraint_map() ∨ in-file cs_kernel_id`.

**Retro-check on the 2026-07-04 live partition (stands).** None of the live routing-unknown
four (`architectural_pattern_validity`, `demographic_resource_allocation`,
`propagation_speed_asymmetry`, `validation_judgment_separation`) nor `organization_floor_c0`
carries `cs_kernel_id`, and none of the four flat candidates does (negative control clean) —
yesterday's buckets are unchanged by the new instrument.

## Layer-B rates (descriptive; REGIME- AND MODEL-BOUND, not detector evidence)

| Corpus | n | claimed mountain | mountain→rope (alert-firing + no-alert) | rate |
|---|---|---|---|---|
| live `testsets/` (2026-07-04 output) | 128 | 18 | 10 (9 + 1) | 55.6% |
| kernel_v1 (archive, pre-reset) | 1,106 | 41 | 2 (1 + 1) | 4.9% |
| `testsets_haiku` | 960 | 72 | 22 (21 + 1) | 30.6% |
| `testsets_flash` | 960 | 104 | 55 (48 + 7) | 52.9% |

The post-reset corpora run 6–11× kernel_v1's rate, and the two twins disagree with each other
(30.6% vs 52.9% on the same seed pool; flash also *claims* mountain more often, 104 vs 72).
Per the OQ-70 rule these rates are **authoring-regime and model-idiosyncratic conventions**,
not detection results — the kernel_v1-vs-twins gap confounds generation era with corpus kind,
so it may NOT be cited as "false-mountain density tracks contested-ness." What it does show,
consistent with the OQ-87 twins characterization: the mountain-claim convention itself is
model-bound.

Alert-channel liveness (didn't-look controls): haiku 861/960, flash 867/960 alert-bearing;
both readers resolved all their firing stories' `.pl` files (21/21, 48/48).

## Discriminator saturation REPLICATES on both twins

Regime-omega present on 69/69 firing stories (haiku 21/21, flash 48/48; Tier 1 only 4 per leg —
the saturation is Tier-2-driven, as on the live leg). The 2026-07-04 conclusion is now
corpus-family-wide: **D′ (= D ∧ regime-omega) has zero discriminating power at this
operationalization on every leg measured.**

## Undetermined buckets (reported, not folded)

haiku 1 (`press_reformation_causation__technological_determinism`); flash 7
(`ai_human_relationship__incarnational_humanism`, `beta_designation_doctrine__severity_carve_out_reading`,
`digital_money_emergence_boundary__infrastructure_reading`, `hebrew_living_language__liturgical_continuity_reading`,
`orthographic_legitimacy_kernel__continuity_reading`, `quantum_formalism__copenhagen_reading`,
`war_winnability_post_1945__deterrence_unthinkable`) — mountain→rope without the false-summit
alert; full rows in the sweep JSONs.

## What this changes for the OQ-88 residual

Nothing about the verdict cells (the twins cannot exercise D), two things about the wiring:
(1) any gate/prompt built on D must include the in-file `cs_kernel_id` check in Layer A, which
also formally closes the twins out of scope by construction; (2) D′ is confirmed dead — the
operator's Ω_P choice is between auto-route and the review prompt with no refined-gate middle
option at this discriminator.
