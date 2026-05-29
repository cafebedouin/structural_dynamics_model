Observer-Related Diagnostics: Survey for Kernel/Reading Revisit

  Here is what I found and where the gaps are. I'll organize by proximity to the new CS infrastructure.

  ---
  1. abductive_triggers.pl — No CS-layer imports at all

  The biggest structural gap. The 15-trigger abductive engine imports drl_core, constraint_indexing,
  grothendieck_cohomology, dirac_classification, maxent_classifier, purity_scoring, drl_lifecycle — but no CS module at
  all (cs_kernel_registry, cs_axiom_engine, cs_drift_engine, cs_pattern_detection are all absent from the module header
  at lines 48–61).

  This means none of the CS-layer findings can surface as abductive hypotheses. The trigger engine is
  observer-context-indexed (all 15 triggers take a Context arg), and cs_kernel_divergence/4 produces the most directly
  analogous reading: same kernel, different readings, different DR-type at the same Context. There is no trigger that
  fires on this.

  The comment at cs_kernel_registry.pl:14 says it explicitly: "the CS-layer analogue of perspectival_incoherence". But
  perspectival_incoherence surfaces in the abductive engine; its CS analogue does not.

  ---
  2. perspectival_incoherence vs cs_kernel_divergence — parallel but unconnected

  drl_core.pl:528–537 — perspectival_incoherence fires when the same constraint classifies differently across two
  standard_context/1 pairs (intra-constraint, cross-context).

  cs_kernel_divergence/4 fires when two readings of the same kernel classify differently at the same context
  (inter-reading, fixed-context).

  These are logically orthogonal: a reading can be perspectivally coherent (H¹ = 0 for the reading in isolation) yet be
  in cross-reading divergence (it classifies differently from its sibling reading at that same context). No existing
  diagnostic combines these two dimensions — the "double incoherence" case where a reading is both internally
  perspectivally incoherent AND diverges from its kernel siblings.

  ---
  3. gauge_orbit/2 / cohomological_obstruction/3 — per-constraint, no kernel-level aggregation

  dirac_classification.pl:144–150 — gauge_orbit/2 uses site_contexts/1 (configurable canonical or product site) and
  drl_core:dr_type/3. grothendieck_cohomology.pl:154–165 — cohomological_obstruction/3 calls orbit_vector, which also
  uses dr_type/3.

  These operate per-constraint. Each reading in a kernel triplet gets its own orbit shape and its own H¹. But the
  interesting kernel-level question — whether the orbit shapes across readings are systematically different, or whether
  all readings have H¹ = 4 (meta-hub) — has no predicate. The cs_kernel_registry knows the readings; the orbit engine
  doesn't know about kernels.

  One wrinkle worth noting: cohomological_obstruction uses dr_type/3 (static, no time dimension), while
  cs_kernel_divergence/4 uses classify_at_time(C, 0, Ctx, Type) (time-threaded even at T=0, with nb_setval globals for
  theater_ratio and ε). At T=0 with no authored measurements, both paths fall through to the same
  classify_from_metrics/6, but the nb_setval side-effects differ. If any downstream predicate reads classify_at_time_eps
   or classify_at_time_theater after a cohomology call, it will find stale or absent globals.

  ---
  4. arakelov_height.pl — per-constraint boundary complexity, no differential across readings

  arakelov_height_pair/3 (line 100) — computes max boundary complexity over the canonical contexts using
  raw_confidence_margin (from maxent_distribution_raw) and signature_pressure. Returns a scalar + the argmax context.

  For a kernel triplet, each reading has its own height. The analytically interesting question is whether the height
  differential across readings is itself a signal — a kernel where reading C1 has high Arakelov height but C2 has low
  height suggests the contested kernel produces a MaxEnt-confident reading and a MaxEnt-uncertain one. That differential
   doesn't exist as a predicate.

  ---
  5. snapshot_type/3 vs cs_drift_trajectory/3 — two temporal paths with no bridge

  transition_paths.pl:115–134 — snapshot_type/3 uses the DR metric pipeline (ε, suppression, sigmoid χ) at a specific
  time from the default analytical context. It classifies the constraint type at time T.

  cs_drift_engine.pl:46–49 — cs_drift_trajectory/3 takes the authored cs_drift_state/3 gap descriptor and maps it to a
  terminal attractor (stable_pattern, husk, extinction, revival, repudiation, axiom_foreclosure).

  These are semantically parallel — both predict a temporal trajectory — but use entirely different mechanisms. There is
   no predicate that checks whether they agree or disagree. A mismatch would be structurally interesting: e.g.,
  snapshot_type shows a stable classification over measured time points while cs_drift_trajectory predicts husk or
  axiom_foreclosure. That's a grounding signal not currently surfaced anywhere.

  ---
  6. husk_series/3 / husk_report.pl — single-constraint, no cross-reading comparison

  husk_report.pl runs husk_series/3 in the powerless canonical context for each constraint. For CS readings, each
  reading has its own EP decay trajectory. A kernel where reading C1 shows a husk trajectory and C2 shows a stable
  trajectory at the same observer context would be a structural finding — the contested kernel reads differently through
   the temporal lens. Not currently compared.

  ---
  7. boltzmann_compliance.pl:441–446 — temporal ε threading is per-constraint

  excess_extraction/2 has a hook: reads classify_at_time_eps from nb_getval if available. This allows classify_at_time/4
   to thread temporal ε through the Boltzmann calculation. But for CS readings, each reading has its own ε. The
  Boltzmann compliance status — whether a reading exceeds its coordination floor — could differ across readings of the
  same kernel, and no diagnostic compares them.

  ---
  8. trigger_epistemic_trap/3 (Trigger 15) — powerless restricted view, single-constraint

  abductive_triggers.pl:875–925 — fires when classify_from_restricted/3 (powerless observer, restricted feature access)
  differs from dr_type/3 (full data). This is single-constraint.

  For readings: the same powerless observer might classify reading C1 into type X but classify reading C2 into type Y,
  where in both cases the restricted and full-data types agree — the epistemic trap fires for one reading but not the
  other. Or the restricted classification might systematically agree with one reading and disagree with another. The
  trigger as written would fire independently per-reading but the cross-reading comparison of epistemic trap status is
  not a surfaced finding.

  ---
  9. cs_corpus_analysis.pl — misses kernel-level and axiom/drift statistics

  run_cs_corpus_analysis/0 covers: pattern distribution, cs_authority_masking, cs_cover_story_active,
  cs_displaced_beneficiary. It does not report:
  - Kernel divergence counts (how many context-pairs diverge per kernel)
  - cs_axiom_inconsistent rates (Type B: single constraint holds contradiction)
  - cs_kernel_axiom_conflict rates (cross-reading axiom conflicts)
  - cs_drift_unacknowledged instances
  - cs_drift_trajectory terminal attractor distribution
  - cs_axiom_foreclosed instances

  This isn't a logic gap — the predicates exist. It's a reporting surface that doesn't enumerate them yet.

  ---
  10. test_snapshot_migration.pl — doesn't cover CS temporal scenarios

  The migration test verifies that classify_at_time/4 uses the canonical sigmoid pipeline. It doesn't test what
  classify_at_time/4 returns for a reading that has authored cs_drift_state facts — i.e., whether the drift state
  interacts with the metric classification (it shouldn't, since classify_at_time ignores CS fields, but the null
  assumption is untested).

  ---
  Summary Table

  ┌──────────────────────────┬────────────────────────────┬─────────────────────────────────────────────────────────┐
  │        Diagnostic        │      Current behavior      │             Gap relative to kernel/reading              │
  ├──────────────────────────┼────────────────────────────┼─────────────────────────────────────────────────────────┤
  │ perspectival_incoherence │ per-constraint,            │ no cross-reading at fixed context                       │
  │                          │ cross-context              │                                                         │
  ├──────────────────────────┼────────────────────────────┼─────────────────────────────────────────────────────────┤
  │ gauge_orbit/cohomology   │ per-constraint, dr_type    │ no kernel-level orbit aggregation; uses different       │
  │                          │ pipeline                   │ pipeline than classify_at_time                          │
  ├──────────────────────────┼────────────────────────────┼─────────────────────────────────────────────────────────┤
  │ arakelov_height          │ per-constraint scalar      │ no differential height across readings                  │
  ├──────────────────────────┼────────────────────────────┼─────────────────────────────────────────────────────────┤
  │ snapshot_type            │ metric-based temporal      │ no comparison to cs_drift_trajectory                    │
  ├──────────────────────────┼────────────────────────────┼─────────────────────────────────────────────────────────┤
  │ husk_series              │ per-constraint             │ no cross-reading comparison                             │
  ├──────────────────────────┼────────────────────────────┼─────────────────────────────────────────────────────────┤
  │ boltzmann_compliance     │ per-constraint ε           │ no cross-reading Boltzmann differential                 │
  ├──────────────────────────┼────────────────────────────┼─────────────────────────────────────────────────────────┤
  │ trigger_epistemic_trap   │ single-constraint          │ no cross-reading restricted-view comparison             │
  │                          │ restricted view            │                                                         │
  ├──────────────────────────┼────────────────────────────┼─────────────────────────────────────────────────────────┤
  │ abductive_triggers       │ 15 triggers, no CS imports │ zero CS-layer findings can surface                      │
  ├──────────────────────────┼────────────────────────────┼─────────────────────────────────────────────────────────┤
  │ cs_corpus_analysis       │ pattern + masking only     │ no axiom, drift, kernel-divergence stats                │
  ├──────────────────────────┼────────────────────────────┼─────────────────────────────────────────────────────────┤
  │ snapshot_migration tests │ pipeline correctness       │ no CS-authored drift state scenarios                    │
  └──────────────────────────┴────────────────────────────┴─────────────────────────────────────────────────────────┘

  The sharpest single intervention point is the abductive trigger engine: it's the junction where observer-indexed math
  findings and structural findings converge into hypotheses, and the CS layer is entirely absent from it.
