# Mountain NL Profile Backfill List

These files declare `emerges_naturally` but lack the NL profile metrics
required for `natural_law_signature` certification. Each needs:
- `constraint_metric(Name, accessibility_collapse, V)` where V >= 0.85
- `constraint_metric(Name, resistance, V)` where V <= 0.15

Without these, the constraint classifies as mountain but fails
signature certification silently (defaults to 0.5, which fails both gates).

Do NOT auto-generate the metric values. These require domain judgment
(is this constraint truly accessibility_collapse >= 0.85? is resistance truly <= 0.15?).
Flag for manual review or targeted LLM regeneration.

## Files (90 total)

| # | File | Missing Metrics |
|---|------|----------------|
| 1 | advice_as_dangerous_gift.pl | accessibility_collapse, resistance |
| 2 | armra_colostrum_regulation.pl | resistance |
| 3 | automatic_enrollment_defaults.pl | accessibility_collapse, resistance |
| 4 | availability_heuristic.pl | accessibility_collapse, resistance |
| 5 | banach_tarski_paradox.pl | accessibility_collapse, resistance |
| 6 | base_pair_complementarity.pl | accessibility_collapse, resistance |
| 7 | basel_problem_convergence.pl | accessibility_collapse, resistance |
| 8 | belief_argument_conclusion.pl | accessibility_collapse, resistance |
| 9 | biological_curiosity.pl | accessibility_collapse, resistance |
| 10 | brouwer_fixed_point.pl | accessibility_collapse, resistance |
| 11 | buffons_needle_pi_estimation.pl | accessibility_collapse, resistance |
| 12 | burali_forte_paradox.pl | accessibility_collapse, resistance |
| 13 | chaitins_omega_undecidability.pl | accessibility_collapse, resistance |
| 14 | choice_architecture_design.pl | accessibility_collapse, resistance |
| 15 | church_turing_thesis.pl | accessibility_collapse, resistance |
| 16 | cinderella_midnight_deadline.pl | accessibility_collapse, resistance |
| 17 | click_chemistry_paradigm_2026.pl | accessibility_collapse, resistance |
| 18 | climate_attribution_2026.pl | accessibility_collapse, resistance |
| 19 | collatz_conjecture_determinism.pl | accessibility_collapse, resistance |
| 20 | confirmation_bias.pl | accessibility_collapse, resistance |
| 21 | continuum_hypothesis_undecidability.pl | accessibility_collapse, resistance |
| 22 | countable_infinity_cardinality.pl | accessibility_collapse, resistance |
| 23 | cow_field_poop.pl | accessibility_collapse, resistance |
| 24 | currys_paradox.pl | accessibility_collapse, resistance |
| 25 | elencher_identity_transformation.pl | accessibility_collapse, resistance |
| 26 | endowment_effect.pl | accessibility_collapse, resistance |
| 27 | epigenetics_complexity_2026.pl | accessibility_collapse, resistance |
| 28 | ergo_dexy_gold_protocol.pl | accessibility_collapse, resistance |
| 29 | ergo_lets_protocol.pl | accessibility_collapse, resistance |
| 30 | ergo_mixer_protocol.pl | accessibility_collapse, resistance |
| 31 | ergo_nipopows.pl | accessibility_collapse, resistance |
| 32 | ergo_sig_usd_protocol.pl | accessibility_collapse, resistance |
| 33 | euler_characteristic_topology.pl | accessibility_collapse, resistance |
| 34 | exoplanet_habitability_arbitrage.pl | accessibility_collapse, resistance |
| 35 | extraordinary_narrative_shift.pl | accessibility_collapse, resistance |
| 36 | fundamental_theorem_of_algebra.pl | accessibility_collapse, resistance |
| 37 | galois_theory_symmetry.pl | accessibility_collapse, resistance |
| 38 | genetic_algorithms_evolution.pl | accessibility_collapse, resistance |
| 39 | gita_kurukshetra.pl | accessibility_collapse, resistance |
| 40 | godels_incompleteness_theorems.pl | accessibility_collapse, resistance |
| 41 | goldbach_conjecture.pl | accessibility_collapse, resistance |
| 42 | gradient_descent_optimization.pl | accessibility_collapse, resistance |
| 43 | halting_problem_undecidability.pl | accessibility_collapse, resistance |
| 44 | hawthorne_effect.pl | accessibility_collapse, resistance |
| 45 | heisenberg_uncertainty.pl | accessibility_collapse, resistance |
| 46 | helsinki_bus_theory.pl | accessibility_collapse, resistance |
| 47 | heuristic_optimization.pl | accessibility_collapse, resistance |
| 48 | hilberts_hotel_infinity.pl | accessibility_collapse, resistance |
| 49 | indexical_relativity_core.pl | accessibility_collapse, resistance |
| 50 | information_foraging_theory.pl | accessibility_collapse, resistance |
| 51 | kirby_paris_theorem.pl | accessibility_collapse, resistance |
| 52 | kjv_great_awakening.pl | accessibility_collapse, resistance |
| 53 | kjv_puritan_new_world_exit.pl | accessibility_collapse, resistance |
| 54 | kleene_recursion_theorem.pl | accessibility_collapse, resistance |
| 55 | landscape_of_fear_2026.pl | accessibility_collapse, resistance |
| 56 | layered_brain_processing.pl | accessibility_collapse, resistance |
| 57 | liar_paradox.pl | accessibility_collapse, resistance |
| 58 | lindy_effect.pl | accessibility_collapse, resistance |
| 59 | litany_of_the_real.pl | accessibility_collapse, resistance |
| 60 | local_vs_global_optima.pl | accessibility_collapse, resistance |
| 61 | lowenheim_skolem_theorem.pl | accessibility_collapse, resistance |
| 62 | marriage_problem.pl | accessibility_collapse, resistance |
| 63 | martian_signal_latency.pl | accessibility_collapse, resistance |
| 64 | med_diet_consensus_2026.pl | accessibility_collapse, resistance |
| 65 | micro_robot_electronics_integration.pl | accessibility_collapse, resistance |
| 66 | microbiome_symbiosis.pl | accessibility_collapse, resistance |
| 67 | newtons_method_convergence.pl | accessibility_collapse, resistance |
| 68 | no_cloning_theorem.pl | accessibility_collapse, resistance |
| 69 | noethers_theorem_symmetry.pl | accessibility_collapse, resistance |
| 70 | pareto_principle.pl | accessibility_collapse, resistance |
| 71 | poincare_conjucture.pl | accessibility_collapse, resistance |
| 72 | prime_number_theorem.pl | accessibility_collapse, resistance |
| 73 | proof_of_work_consensus.pl | accessibility_collapse, resistance |
| 74 | pythagorean_theorem_geometric_constancy.pl | accessibility_collapse, resistance |
| 75 | quine_self_replication.pl | accessibility_collapse, resistance |
| 76 | qwerty_vs_dvorak.pl | accessibility_collapse, resistance |
| 77 | reciprocity_laws_math.pl | accessibility_collapse, resistance |
| 78 | rices_theorem_undecidability.pl | accessibility_collapse, resistance |
| 79 | sadhu_integrity_protocol.pl | accessibility_collapse, resistance |
| 80 | skills_based_hiring.pl | accessibility_collapse, resistance |
| 81 | skolems_paradox.pl | accessibility_collapse, resistance |
| 82 | social_narrative_casting.pl | accessibility_collapse, resistance |
| 83 | solar_system_weirdness.pl | accessibility_collapse, resistance |
| 84 | sturgeons_law.pl | accessibility_collapse, resistance |
| 85 | suslin_hypothesis_undecidability.pl | accessibility_collapse, resistance |
| 86 | sylow_theorems_group_theory.pl | accessibility_collapse, resistance |
| 87 | trade_secret_law.pl | accessibility_collapse, resistance |
| 88 | universal_mathematics_communication.pl | accessibility_collapse, resistance |
| 89 | whitehead_problem_undecidability.pl | accessibility_collapse, resistance |
| 90 | winners_curse.pl | accessibility_collapse, resistance |
