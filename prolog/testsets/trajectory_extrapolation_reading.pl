% ============================================================================
% CONSTRAINT STORY: trajectory_extrapolation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trajectory_extrapolation_reading, []).

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
 *   constraint_id: trajectory_extrapolation_reading
 *   human_readable: Trajectory Extrapolation Reading of Generality Standard
 *   domain: philosophy_of_measurement/ai_capability_assessment/epistemology_of_forecasting
 *
 * SUMMARY:
 *   The trajectory extrapolation reading treats general AI capability as a
 *   continuous scalar quantity climbing a learning curve toward a fixed
 *   threshold. Benchmarks measure progress along this curve; disagreement
 *   about timelines is noise around a convergent empirical fact; more
 *   measurement should tighten estimates. This reading is one of three
 *   sibling interpretations of the generality standard kernel. The partition
 *   choice reading treats capability as a discrete partition over task
 *   domains, making the threshold a choice about which partition counts. The
 *   generation gate reading treats generality as an emergent phase
 *   transition, making the timeline inherently unpredictable from
 *   pre-transition data. This story models ONLY the trajectory extrapolation
 *   reading as a standalone constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trajectory_extrapolation_reading, 0.68).
domain_priors:suppression_score(trajectory_extrapolation_reading, 0.71).
domain_priors:theater_ratio(trajectory_extrapolation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trajectory_extrapolation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(trajectory_extrapolation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(trajectory_extrapolation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trajectory_extrapolation_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(trajectory_extrapolation_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trajectory_extrapolation_reading, tangled_rope).
narrative_ontology:human_readable(trajectory_extrapolation_reading, "Trajectory Extrapolation Reading of Generality Standard").
narrative_ontology:topic_domain(trajectory_extrapolation_reading, "philosophy_of_measurement/ai_capability_assessment/epistemology_of_forecasting").

domain_priors:requires_active_enforcement(trajectory_extrapolation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trajectory_extrapolation_reading, '82ec0c9b-8786-4c1c-b491-74ff5886b6ab').
narrative_ontology:cs_kernel_codification('82ec0c9b-8786-4c1c-b491-74ff5886b6ab', distributed).
narrative_ontology:cs_authority_grounding('82ec0c9b-8786-4c1c-b491-74ff5886b6ab', expertise).
narrative_ontology:cs_interpretation_layer_present('82ec0c9b-8786-4c1c-b491-74ff5886b6ab').
narrative_ontology:cs_reading_relation('82ec0c9b-8786-4c1c-b491-74ff5886b6ab', generality_standard__partition_choice_reading, coexists_with).
narrative_ontology:cs_reading_relation('82ec0c9b-8786-4c1c-b491-74ff5886b6ab', generality_standard__generation_gate_reading, coexists_with).
narrative_ontology:cs_axiom('82ec0c9b-8786-4c1c-b491-74ff5886b6ab', foundational, capability_continuity).
narrative_ontology:cs_axiom_status(capability_continuity, holdable).
narrative_ontology:cs_axiom_grounding('82ec0c9b-8786-4c1c-b491-74ff5886b6ab', capability_continuity, empirically_contingent).
narrative_ontology:cs_axiom('82ec0c9b-8786-4c1c-b491-74ff5886b6ab', foundational, measurement_transparency).
narrative_ontology:cs_axiom_status(measurement_transparency, holdable).
narrative_ontology:cs_axiom_grounding('82ec0c9b-8786-4c1c-b491-74ff5886b6ab', measurement_transparency, empirically_contingent).
narrative_ontology:cs_reference_frame('82ec0c9b-8786-4c1c-b491-74ff5886b6ab', benchmark_transparency_assumption).
narrative_ontology:cs_drift_state('82ec0c9b-8786-4c1c-b491-74ff5886b6ab', post_measurement_theory_critique, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('82ec0c9b-8786-4c1c-b491-74ff5886b6ab', '').
narrative_ontology:cs_kernel_id(trajectory_extrapolation_reading, generality_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trajectory_extrapolation_reading, frontier_labs).
narrative_ontology:constraint_beneficiary(trajectory_extrapolation_reading, capability_forecasters).
narrative_ontology:constraint_victim(trajectory_extrapolation_reading, skeptical_researchers).
narrative_ontology:constraint_victim(trajectory_extrapolation_reading, policy_makers_under_timeline_pressure).
narrative_ontology:constraint_vindicates(trajectory_extrapolation_reading, continuous_progress_doctrine).
narrative_ontology:constraint_vindicates(trajectory_extrapolation_reading, measurement_convergence_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claim that general capability climbs a smooth learning curve toward a measurable threshold, making breakthrough timing forecastable from current benchmark trajectories. This framing justifies current valuations (investors fund the race to a date), regulatory deference (the timeline is a physics question, not a policy choice), and resource concentration (whoever scales fastest wins a predictable prize). They publish benchmark results as evidence of position on the curve.
narrative_ontology:constraint_stakeholder(trajectory_extrapolation_reading, frontier_labs, beneficiary,
    institutional, biographical, mobile, global).

% Operationalize the trajectory framing by fitting curves to benchmark sequences and publishing timeline estimates. Their methodology treats disagreement as measurement noise around a hidden convergent fact. They gain professional standing and influence from producing quantitative forecasts that shape funding and policy decisions.
narrative_ontology:constraint_stakeholder(trajectory_extrapolation_reading, capability_forecasters, agenda_setter,
    organized, biographical, mobile, global).

% Argue that benchmark performance does not measure a unitary general capability, that the learning curve is an artifact of benchmark choice, and that extrapolation assumes away the hardest conceptual problems. Their objections are treated as noise or pessimism rather than structural critique. They bear reputational cost for dissenting from the consensus timeline and find their research framing marginalized in funding and publication.
narrative_ontology:constraint_stakeholder(trajectory_extrapolation_reading, skeptical_researchers, payer,
    moderate, biographical, constrained, global).

% Receive forecasts as authoritative technical input and are pressured to act on the implied timeline—either to accelerate domestic capability or to regulate before the threshold. The trajectory framing forecloses policy questions about whether to pursue the capability at all, replacing them with when-questions that assume the outcome. They bear the cost of premature or misdirected policy built on contested extrapolations.
narrative_ontology:constraint_stakeholder(trajectory_extrapolation_reading, policy_makers_under_timeline_pressure, payer,
    institutional, generational, constrained, national).

% Would argue that the choice of what to measure and how to aggregate performance is a conceptual decision that shapes the apparent trajectory, not a neutral discovery of pre-existing progress. Their framing—that benchmarks construct the phenomenon they claim to measure—is absent from the forecasting discourse, which treats measurement as transparent.
narrative_ontology:constraint_stakeholder(trajectory_extrapolation_reading, measurement_theorists, excluded,
    moderate, generational, constrained, global).

% Examine how the trajectory framing functions as a commitment system: it converts an open conceptual question (what is general capability?) into a closed empirical question (when does the curve cross the threshold?), and treats dissent as poor calibration rather than legitimate disagreement about the object of measurement.
narrative_ontology:constraint_stakeholder(trajectory_extrapolation_reading, epistemology_of_forecasting_analysts, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared quantitative framework for reasoning about AI progress: labs, investors, and policy makers can coordinate expectations and resource allocation around a common timeline derived from public benchmark data.
% TRANSFER_FUNCTION: Moves epistemic authority and resource-allocation power from researchers questioning the measurement framework to labs and forecasters who operationalize the trajectory framing. Moves policy discretion from open-ended capability governance to timeline-driven response planning.
% ABSENT_VOICES: Measurement theorists who would contest the benchmark-to-capability mapping, and researchers who argue the threshold itself is a category error, are structurally excluded from the forecasting discourse—their objections are treated as outside the technical question rather than as challenges to its framing.
% DISAPPEARANCE_RATIONALE: If the trajectory framing vanished, capability assessment would fragment into competing measurement paradigms with no consensus timeline. Investment and policy decisions currently justified by extrapolation would require explicit normative arguments about which capabilities to pursue and why. Labs would lose the rhetorical leverage of imminent-breakthrough claims grounded in curve-fitting.
% FOUNDING_PROBLEM: Early AI capability assessment had no quantitative framework for comparing systems or predicting progress—each advance was a surprise, and resource allocation was ad hoc. Benchmarks and trajectory analysis were introduced to make progress legible and forecastable.
% FOUNDING_PROBLEM_CORROBORATION: Frontier labs and capability forecasters attest the founding problem is still live and the trajectory framework solves it. Skeptical researchers and measurement theorists—outside the benefiting parties—attest that the founding problem has shifted: the challenge is no longer making progress legible but avoiding premature convergence on a contested measurement standard that forecloses conceptual alternatives. Independent epistemology-of-science analysis supports the shifted-function reading.
narrative_ontology:disappearance_verdict(trajectory_extrapolation_reading, world_rearranges).
narrative_ontology:founding_problem_status(trajectory_extrapolation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trajectory_extrapolation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-06-29',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(trajectory_extrapolation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trajectory_extrapolation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trajectory_extrapolation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trajectory_extrapolation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.68) because the framing transfers epistemic authority from conceptual critique to curve-fitting, and resource allocation from open capability governance to timeline-driven planning—benefiting labs and forecasters who operationalize the standard while marginalizing researchers who contest the measurement framework. Suppression is high (0.71) because dissent is reframed as poor calibration or pessimism rather than legitimate disagreement about the object of measurement; the constraint's persistence depends on excluding measurement-theoretic objections from the technical discourse. Theater ratio is moderate (0.42): benchmark publication and curve-fitting are real coordination activities, but a growing share of the apparatus defends the trajectory framing itself (treating benchmark choice as neutral, aggregation as obvious) rather than solving the founding problem of making progress legible. Accessibility collapse is moderate-low (0.48): alternative measurement paradigms remain conceptually available, but the trajectory framing dominates funding and policy discourse. Resistance is substantial (0.62): skeptical researchers and measurement theorists actively contest the framework, but their objections are structurally marginalized.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (labs, forecasters), the trajectory framing is a neutral technical framework that makes progress measurable and timelines forecastable. From the payer seats (skeptical researchers, policy makers), the same structure operates as enforced premature convergence on a contested measurement standard that forecloses conceptual alternatives and drives resource allocation toward a potentially ill-defined target. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontier labs and capability forecasters are beneficiaries: they collect epistemic authority, funding leverage, and policy influence from operationalizing the trajectory standard. Skeptical researchers and policy makers under timeline pressure are targets: they bear reputational cost for dissenting or must act on contested extrapolations. Measurement theorists are excluded rather than coordinated—their critique of the benchmark-to-capability mapping is treated as outside the technical question.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint coordinates expectations around a shared quantitative framework (genuine coordination function), but it also extracts by marginalizing dissent and transferring discretion from open governance to timeline-driven response. The tangled rope classification captures both: coordination and asymmetric extraction operating through the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    benchmark_to_capability_mapping,
    'Do benchmarks measure a pre-existing unitary capability, or do they construct the phenomenon they claim to measure by aggregating performance on a chosen task set?',
    'Conceptual analysis of measurement theory applied to capability assessment, plus empirical tests of whether different benchmark suites produce convergent or divergent capability orderings for the same systems.',
    'If benchmarks construct rather than discover capability, the trajectory is an artifact of benchmark choice, not a law-like regularity. The extrapolation reading collapses and resource allocation justified by timeline forecasts loses its empirical grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benchmark_to_capability_mapping, conceptual, 'Whether the benchmark-to-capability mapping is discovery or construction.').

omega_variable(
    threshold_existence,
    'Is there a well-defined threshold of general capability that systems approach, or is ''general capability'' a family resemblance concept with no natural boundary?',
    'Philosophical analysis of the generality concept plus empirical investigation of whether capability profiles cluster around a threshold or distribute continuously without natural joints.',
    'If no threshold exists, the trajectory framing is a category error: it treats a vague concept as a measurable quantity and generates forecasts about crossing a boundary that does not exist. Policy and investment decisions built on the timeline would be systematically misdirected.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threshold_existence, conceptual, 'Whether the target threshold is a real joint in capability space or a constructed boundary.').

omega_variable(
    committer_frame_kernel_ambiguity,
    'Is the trajectory extrapolation reading the correct interpretation of the generality standard kernel, or is it one of several incommensurable framings of the same underlying question?',
    'Cross-reading comparison: if sibling readings (partition choice, generation gate) produce systematically different timeline estimates and resource-allocation recommendations from the same benchmark data, the readings are incommensurable and the kernel is under-determined.',
    'If the kernel is under-determined, the trajectory reading''s claim to be measuring a convergent empirical fact is false. The timeline is a function of which reading you adopt, not a property of the world. This would reclassify the constraint from contested coordination to enforced framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_kernel_ambiguity, conceptual, 'Whether the generality standard kernel admits a unique correct reading or multiple incommensurable framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trajectory_extrapolation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(traj_tr_t0, trajectory_extrapolation_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(traj_tr_t5, trajectory_extrapolation_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(traj_tr_t10, trajectory_extrapolation_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(traj_tr_t15, trajectory_extrapolation_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(traj_tr_t20, trajectory_extrapolation_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(traj_be_t0, trajectory_extrapolation_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(traj_be_t5, trajectory_extrapolation_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(traj_be_t10, trajectory_extrapolation_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(traj_be_t15, trajectory_extrapolation_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(traj_be_t20, trajectory_extrapolation_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(traj_su_t0, trajectory_extrapolation_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(traj_su_t5, trajectory_extrapolation_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(traj_su_t10, trajectory_extrapolation_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(traj_su_t15, trajectory_extrapolation_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(traj_su_t20, trajectory_extrapolation_reading, suppression_requirement, 20, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trajectory_extrapolation_reading, information_standard).
narrative_ontology:affects_constraint(trajectory_extrapolation_reading, partition_choice_reading).
narrative_ontology:affects_constraint(trajectory_extrapolation_reading, generation_gate_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the generality_standard kernel. The trajectory_extrapolation_reading treats capability as a continuous scalar; partition_choice_reading treats it as a discrete partition; generation_gate_reading treats it as an emergent phase transition. Each reading produces different timeline forecasts and resource-allocation recommendations from the same benchmark data. The readings are linked via network.affects_constraints because they compete for epistemic authority over the same measurement question.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
