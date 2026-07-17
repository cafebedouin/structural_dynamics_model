# 0c — consumer polarity classification (throw-site map)

Read-only classification of every `purity_score/2` reader and every producer call site,
against the post-fix reality that `purity_score` can succeed with the atom `unknown`.
Mechanical facts: `unknown >= 0.0` / `unknown < 0.0` / `unknown - X` THROW
`type_error(evaluable, unknown/0)`; `unknown \= -1.0` SUCCEEDS (so `\= -1.0` guards do NOT stop
it); `purity_zone/2` is pure arithmetic → `purity_zone(unknown,_)` THROWS; `write_json_number/2`
has an explicit `unknown → null` clause (safe). No consumer uses `@<`/`@>`/`sort`/`min`/`max` on a
raw purity value → **no silent-misorder category; every unsafe site is a hard THROW** (loud).

## (A) THROW sites needing a `number(P)` guard in Commit 0a
- abductive_triggers.pl:411
- metric_drift_events.pl:378, 390, 399, 495, 501
- network_dynamics.pl:67, 82, 100, 129
- diagnostic_summary.pl:229
- drl_fpn.pl:253
- json_report.pl:322, 1271, 2013
- logical_fingerprint.pl:433 (downstream `purity_zone` :599)
- drl_purity_network.pl:224, 300, 323, 324, 421, 462
- grothendieck_cohomology.pl:791
- drl_boltzmann_analysis.pl:269, 292, 425, 544, 601, 616, 627, 661
- maxent_report.pl:375
- maxent_diagnostic.pl:606, 616, 625
- **state-poisoning stores (guard before assert):** drl_fpn.pl:107 (`fpn_intrinsic`),
  giant_component_analysis.pl:353 (`gc_node_purity`)

## (B) Silent-misorder sites
**None.** Every unsafe path is a hard arithmetic throw.

## (C) Aggregate −1.0 consumption today (OQ-62)
Explicit means/counts already filter −1.0 (`>= 0.0` / `\= -1.0`): maxent_report:375,
maxent_diagnostic:606/616/625, grothendieck_cohomology:791, json_report:2013, giant_component:893.
Real exposure = propagation into shared state unfiltered at the store: drl_purity_network:224
(`effective_purity = -1.0`), drl_fpn:107 (`fpn_intrinsic(C,-1.0)`), giant_component:353
(`gc_node_purity(C,-1.0,-1.0)`). **Filed to OQ-62.**

## (D) Producer fail-open sites (for the producer commits, not Commit 0)
When `cross_index_coupling` / `detect_nonsensical_coupling` / `excess_extraction` start FAILING,
these read the failure as clean/absent silently:
- Detection suppressed: signature_detection.pl:1113 (FNL), 1283, 1291, 1552, 1558.
- Drift/severity under-reported: metric_drift_events.pl:302, 324, 343, 413, 422, 312, 332, 353,
  362, 547, 554.
- Coupling read 0.0/benign: drl_boltzmann_analysis.pl:218, context_profile_mining.pl:186,
  abductive_triggers.pl:422, 640, logical_fingerprint.pl:428, diagnostic_summary.pl:433.
- Not silent-clean (explicit unknown/'?'/−1.0 or loud fail): signature_detection.pl:534, 1388,
  1666, 1118, 1204, 1392; abductive_triggers.pl:468, 1023; context_profile_mining.pl:773, 774;
  context_profile_report.pl:142, 248; genuine_findings_query.pl:105; logical_fingerprint.pl:424;
  diagnostic_summary.pl:451.

Genuinely safe readers (no change): abductive_triggers.pl:87, 467, 531, 824, 957;
context_profile_mining.pl:191; genuine_findings_query.pl:101; json_report.pl:1338.
