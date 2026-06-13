% ============================================================================
% CONSTRAINT STORY: superheavy_decay_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_superheavy_decay_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: superheavy_decay_reading
 *   human_readable: Superheavy Element Decay Chain Actinide Replenishment
 *   domain: astrophysics/nuclear_physics
 *
 * SUMMARY:
 *   This constraint is the superheavy-decay reading of the actinide
 *   replenishment kernel. It claims that undiscovered superheavy elements
 *   from the island of stability (298Fl, 304Ubn, or 310Ubh with billion-year
 *   half-lives) were produced in supernovae, are present in stellar
 *   atmospheres, and decay into the observed actinide daughter products. The
 *   reading is claimed as rope (genuine coordination solving the actinide
 *   abundance puzzle) but the metrics describe moderate-to-high extraction:
 *   the constraint increasingly benefits researchers whose careers depend on
 *   the island-of-stability framework while suppressing alternative
 *   explanations. Resistance is high because competing mechanisms (neutron
 *   star bombardment, artifact disposal) remain live and well-funded.
 *   Accessibility collapse is low because the sibling readings are not ruled
 *   out by current data—spectroscopic non-detection of superheavy parents
 *   does not foreclose the hypothesis if the isotopes are below detection
 *   limits.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(superheavy_decay_reading, 0.68).
domain_priors:suppression_score(superheavy_decay_reading, 0.42).
domain_priors:theater_ratio(superheavy_decay_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(superheavy_decay_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(superheavy_decay_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(superheavy_decay_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(superheavy_decay_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(superheavy_decay_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(superheavy_decay_reading, rope).
narrative_ontology:human_readable(superheavy_decay_reading, "Superheavy Element Decay Chain Actinide Replenishment").
narrative_ontology:topic_domain(superheavy_decay_reading, "astrophysics/nuclear_physics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(superheavy_decay_reading, 'a472748a-087a-4920-9e0e-51a2665852f2').
narrative_ontology:cs_kernel_codification('a472748a-087a-4920-9e0e-51a2665852f2', distributed).
narrative_ontology:cs_authority_grounding('a472748a-087a-4920-9e0e-51a2665852f2', expertise).
narrative_ontology:cs_interpretation_layer_present('a472748a-087a-4920-9e0e-51a2665852f2').
narrative_ontology:cs_reading_relation('a472748a-087a-4920-9e0e-51a2665852f2', superheavy_decay_reading__neutron_star_bombardment_reading, coexists_with).
narrative_ontology:cs_reading_relation('a472748a-087a-4920-9e0e-51a2665852f2', superheavy_decay_reading__artifact_disposal_reading, coexists_with).
narrative_ontology:cs_axiom('a472748a-087a-4920-9e0e-51a2665852f2', foundational, r_process_superheavy_production_viable).
narrative_ontology:cs_axiom_status(r_process_superheavy_production_viable, holdable).
narrative_ontology:cs_axiom_grounding('a472748a-087a-4920-9e0e-51a2665852f2', r_process_superheavy_production_viable, empirically_contingent).
narrative_ontology:cs_axiom('a472748a-087a-4920-9e0e-51a2665852f2', foundational, island_stability_billion_year_half_lives).
narrative_ontology:cs_axiom_status(island_stability_billion_year_half_lives, holdable).
narrative_ontology:cs_axiom_grounding('a472748a-087a-4920-9e0e-51a2665852f2', island_stability_billion_year_half_lives, empirically_contingent).
narrative_ontology:cs_reference_frame('a472748a-087a-4920-9e0e-51a2665852f2', standard_nucleosynthesis_framework).
narrative_ontology:cs_drift_state('a472748a-087a-4920-9e0e-51a2665852f2', post_kilonova_observations, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a472748a-087a-4920-9e0e-51a2665852f2', '').
narrative_ontology:cs_kernel_id(superheavy_decay_reading, actinide_replenishment_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(superheavy_decay_reading, superheavy_nucleosynthesis_researchers).
narrative_ontology:constraint_beneficiary(superheavy_decay_reading, island_stability_theorists).
narrative_ontology:constraint_vindicates(superheavy_decay_reading, island_of_stability_hypothesis).
narrative_ontology:constraint_vindicates(superheavy_decay_reading, r_process_superheavy_production).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Research careers and funding streams depend on the island-of-stability framework remaining viable as an explanation for observed actinide abundances. If superheavy decay is the mechanism, their theoretical predictions about r-process nucleosynthesis pathways and stability island locations are vindicated. They can pivot to alternative mechanisms if this reading fails, but institutional investment in superheavy element synthesis facilities creates momentum.
narrative_ontology:constraint_stakeholder(superheavy_decay_reading, superheavy_nucleosynthesis_researchers, beneficiary,
    organized, biographical, mobile, global).

% Nuclear structure theorists whose shell-model predictions of long-lived superheavy isotopes gain empirical support if stellar actinides trace to superheavy decay. The reading does not extract from them but validates decades of theoretical work. They can abandon the framework if spectroscopic evidence rules it out.
narrative_ontology:constraint_stakeholder(superheavy_decay_reading, island_stability_theorists, beneficiary,
    organized, biographical, mobile, global).

% Astrophysicists studying kilonova r-process nucleosynthesis who would argue that neutron-rich ejecta from mergers, not superheavy decay, explains actinide abundances. Their observational programs compete for the same explanatory territory. They are not structurally suppressed but their alternative reading is not represented in this constraint's framing.
narrative_ontology:constraint_stakeholder(superheavy_decay_reading, neutron_star_merger_observers, excluded,
    organized, biographical, mobile, global).

% Measure actinide absorption lines in stellar atmospheres and could in principle detect superheavy parent isotopes if spectral signatures were known and instruments sensitive enough. They provide the empirical data all readings must explain but do not depend on any particular mechanism being correct.
narrative_ontology:constraint_stakeholder(superheavy_decay_reading, stellar_spectroscopists, observer,
    organized, biographical, analytical, global).

% Compile and assess decay chain data, half-life measurements, and branching ratios. They would adjudicate whether observed actinide ratios match predicted superheavy decay products. Their institutional role is to synthesize evidence, not to advocate for mechanisms.
narrative_ontology:constraint_stakeholder(superheavy_decay_reading, nuclear_data_evaluators, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified nuclear-physics explanation for actinide abundances in stellar atmospheres that otherwise appear anomalous: if billion-year superheavy isotopes were produced in supernovae and are now decaying, the observed actinide ratios are natural decay products rather than requiring continuous replenishment.
% TRANSFER_FUNCTION: Transfers explanatory authority and research funding from alternative mechanisms (neutron star bombardment, artifact disposal) to the superheavy nucleosynthesis and island-of-stability research programs. The constraint does not move material resources but reallocates epistemic and institutional capital.
% ABSENT_VOICES: Proponents of neutron star merger r-process models and researchers investigating anthropogenic or non-natural actinide sources are structurally excluded from this reading's framework. They would contest the superheavy production pathway and argue for alternative replenishment mechanisms, but their objections are not incorporated here.
% DISAPPEARANCE_RATIONALE: If this reading were abandoned, research programs would pivot to the sibling mechanisms (neutron star bombardment or artifact disposal), funding for superheavy element synthesis would redirect toward other nuclear structure questions, and the island-of-stability hypothesis would lose its primary astrophysical vindication. The stellar actinide data would remain but its interpretation would reorganize.
% FOUNDING_PROBLEM: Stellar atmospheres show actinide absorption lines at abundances inconsistent with standard nucleosynthesis models. The elements should have decayed away or been diluted, yet they persist. The founding problem is explaining this anomalous presence without invoking continuous exotic replenishment.
% FOUNDING_PROBLEM_CORROBORATION: Stellar spectroscopists and nuclear data evaluators from outside the superheavy research community confirm the actinide abundance anomaly is real and requires explanation. The problem's liveness is not contested; the mechanism is.
narrative_ontology:disappearance_verdict(superheavy_decay_reading, world_rearranges).
narrative_ontology:founding_problem_status(superheavy_decay_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(superheavy_decay_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(superheavy_decay_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(superheavy_decay_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(superheavy_decay_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(superheavy_decay_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises over the interval (0.45 → 0.68) as institutional investment in superheavy synthesis facilities and island-of-stability theory grows, creating path dependence. Theater ratio is moderate-low (0.28 at interval end): the decay-chain calculations and nucleosynthesis modeling are real physics, but an increasing share of activity is defending the reading against sibling mechanisms rather than testing it. Suppression is moderate (0.42) because the constraint does not actively prevent alternative research but does channel funding and explanatory authority toward the superheavy framework. The measurement grid is aligned: every metric is authored at every time point (0, 8, 16, 24, 32, 40).
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (superheavy researchers, island theorists), the constraint is genuine coordination: it solves the actinide abundance puzzle with a unified nuclear-physics mechanism. From the excluded seat (neutron star observers), the same structure operates as a competitor that channels resources away from their alternative explanation. From the observer seats (spectroscopists, data evaluators), the constraint is one hypothesis among several, to be tested against data. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Superheavy nucleosynthesis researchers and island-of-stability theorists are structural beneficiaries: their theoretical frameworks gain empirical support if this reading is correct, and their research programs capture funding and institutional prestige. They are not targets—the constraint does not extract from them. Neutron star merger observers are excluded rather than coordinated; their alternative reading competes for the same explanatory territory but is not represented in this constraint's structure. Stellar spectroscopists and nuclear data evaluators are analytical observers: they provide the empirical data all readings must explain but do not depend on any particular mechanism.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    superheavy_production_pathway,
    'Can r-process nucleosynthesis in supernovae produce superheavy isotopes in the island of stability at sufficient abundance to explain observed actinide levels?',
    'Direct detection of superheavy isotopes in supernova ejecta, or nucleosynthesis simulations with updated nuclear data showing viable production pathways. Alternatively, spectroscopic detection of superheavy parent isotopes in stellar atmospheres.',
    'If the production pathway is ruled out, this reading collapses and explanatory authority shifts to the sibling mechanisms. If confirmed, the reading''s extractiveness would decrease as it transitions from contested hypothesis to established mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(superheavy_production_pathway, empirical, 'Whether supernovae can produce the required superheavy isotopes.').

omega_variable(
    decay_chain_match,
    'Do the predicted decay chains from 298Fl, 304Ubn, or 310Ubh produce actinide daughter ratios that match observed stellar abundances?',
    'Laboratory measurement of superheavy decay chains (if isotopes can be synthesized) or nuclear structure calculations with sufficient precision to predict branching ratios. Cross-check against high-resolution stellar spectroscopy.',
    'A mismatch between predicted and observed ratios would falsify this reading without affecting the sibling readings. A match would support the reading but not uniquely—sibling mechanisms could produce the same ratios by different pathways.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decay_chain_match, empirical, 'Whether superheavy decay products match observed actinide ratios.').

omega_variable(
    reading_coexistence_vs_foreclosure,
    'Do the three readings (superheavy decay, neutron star bombardment, artifact disposal) genuinely coexist as alternative explanations, or does evidence from one reading foreclose the others?',
    'If superheavy parents are detected spectroscopically, the artifact-disposal reading is foreclosed (natural origin confirmed). If neutron star merger rates and ejecta compositions are measured precisely enough to account for all observed actinides, the superheavy-decay reading becomes unnecessary. If neither pathway is confirmed, all three readings remain live.',
    'The relation between readings determines whether this constraint is one hypothesis in an open contest (coexists_with) or a claim that rules out alternatives (forecloses). Current evidence supports coexistence, but future data could shift the structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_vs_foreclosure, conceptual, 'Whether the readings coexist or foreclose each other.').

omega_variable(
    institutional_path_dependence,
    'Is the rising extractiveness (0.45 → 0.68) driven by genuine accumulation of supporting evidence, or by institutional path dependence in superheavy research programs?',
    'Meta-analysis of funding allocation, publication patterns, and citation networks in superheavy nucleosynthesis research. Compare the rate of new empirical constraints (spectroscopic detections, decay measurements) to the rate of institutional investment growth.',
    'If extractiveness is driven by path dependence rather than evidence, the constraint is drifting toward a false summit: a claimed coordination mechanism (solving the actinide puzzle) that persists because beneficiaries have invested in it, not because it is empirically superior to siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_path_dependence, empirical, 'Whether rising extraction tracks evidence or institutional momentum.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(superheavy_decay_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, superheavy_decay_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(supe_tr_t0, observed).
narrative_ontology:measurement(supe_tr_t8, superheavy_decay_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement_basis(supe_tr_t8, observed).
narrative_ontology:measurement(supe_tr_t16, superheavy_decay_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement_basis(supe_tr_t16, observed).
narrative_ontology:measurement(supe_tr_t24, superheavy_decay_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement_basis(supe_tr_t24, observed).
narrative_ontology:measurement(supe_tr_t32, superheavy_decay_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement_basis(supe_tr_t32, observed).
narrative_ontology:measurement(supe_tr_t40, superheavy_decay_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(supe_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, superheavy_decay_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(supe_be_t0, observed).
narrative_ontology:measurement(supe_be_t8, superheavy_decay_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(supe_be_t8, observed).
narrative_ontology:measurement(supe_be_t16, superheavy_decay_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement_basis(supe_be_t16, observed).
narrative_ontology:measurement(supe_be_t24, superheavy_decay_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement_basis(supe_be_t24, observed).
narrative_ontology:measurement(supe_be_t32, superheavy_decay_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement_basis(supe_be_t32, observed).
narrative_ontology:measurement(supe_be_t40, superheavy_decay_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(supe_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t0, superheavy_decay_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(supe_su_t0, observed).
narrative_ontology:measurement(supe_su_t8, superheavy_decay_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement_basis(supe_su_t8, observed).
narrative_ontology:measurement(supe_su_t16, superheavy_decay_reading, suppression_requirement, 16, 0.32).
narrative_ontology:measurement_basis(supe_su_t16, observed).
narrative_ontology:measurement(supe_su_t24, superheavy_decay_reading, suppression_requirement, 24, 0.36).
narrative_ontology:measurement_basis(supe_su_t24, observed).
narrative_ontology:measurement(supe_su_t32, superheavy_decay_reading, suppression_requirement, 32, 0.39).
narrative_ontology:measurement_basis(supe_su_t32, observed).
narrative_ontology:measurement(supe_su_t40, superheavy_decay_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(supe_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(superheavy_decay_reading, information_standard).
narrative_ontology:affects_constraint(superheavy_decay_reading, neutron_star_bombardment_reading).
narrative_ontology:affects_constraint(superheavy_decay_reading, artifact_disposal_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the actinide_replenishment_mechanism kernel. The kernel is the contested question of what explains anomalous actinide abundances in stellar atmospheres. The superheavy_decay_reading (this constraint) claims billion-year superheavy isotopes from supernovae decay into actinides. The neutron_star_bombardment_reading claims neutron-rich ejecta from mergers continuously replenish actinides. The artifact_disposal_reading claims the actinides are anthropogenic. The readings have different ε values because they make different structural claims about production pathways, timescales, and beneficiary structures. They are linked via network.affects_constraints because evidence for or against one reading shifts the plausibility of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
