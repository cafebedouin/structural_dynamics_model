% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__hybrid_pragmatic_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: deferential_realism_ontology__hybrid_pragmatic_reading
 *   human_readable: Deferential Realism Ontology: Hybrid Pragmatic Reading
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the 'hybrid pragmatic reading' of the
 *   Deferential Realism ontology. This reading posits that the core of the
 *   typology (Mountains, Ropes) is grounded in objective, measurable
 *   properties, while the periphery (Tangled Ropes, Snares) necessarily
 *   involves normative judgments about legitimate beneficiaries and
 *   extraction. This approach aims to bridge purely observational and purely
 *   rhetorical interpretations, acknowledging the role of both empirical data
 *   and ethical considerations in classifying complex constraints. The
 *   claimed type is 'rope' because it facilitates coordination among diverse
 *   analytical perspectives, with moderate extractiveness reflecting the
 *   intellectual effort required to maintain this hybrid stance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, 0.45).
domain_priors:suppression_score(deferential_realism_ontology__hybrid_pragmatic_reading, 0.55).
domain_priors:theater_ratio(deferential_realism_ontology__hybrid_pragmatic_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__hybrid_pragmatic_reading, rope).
narrative_ontology:human_readable(deferential_realism_ontology__hybrid_pragmatic_reading, "Deferential Realism Ontology: Hybrid Pragmatic Reading").
narrative_ontology:topic_domain(deferential_realism_ontology__hybrid_pragmatic_reading, "epistemology/normative_theory/institutional_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__hybrid_pragmatic_reading, '52969615-dd15-4763-a735-4d4562f687a1').
narrative_ontology:cs_kernel_codification('52969615-dd15-4763-a735-4d4562f687a1', distributed).
narrative_ontology:cs_authority_grounding('52969615-dd15-4763-a735-4d4562f687a1', expertise).
narrative_ontology:cs_interpretation_layer_present('52969615-dd15-4763-a735-4d4562f687a1').
narrative_ontology:cs_reading_relation('52969615-dd15-4763-a735-4d4562f687a1', deferential_realism_ontology__immutable_diagnostic_reading, coexists_with).
narrative_ontology:cs_reading_relation('52969615-dd15-4763-a735-4d4562f687a1', deferential_realism_ontology__rhetorical_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('52969615-dd15-4763-a735-4d4562f687a1', foundational, epistemic_pluralism_is_necessary).
narrative_ontology:cs_axiom_status(epistemic_pluralism_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('52969615-dd15-4763-a735-4d4562f687a1', epistemic_pluralism_is_necessary, conventional).
narrative_ontology:cs_axiom('52969615-dd15-4763-a735-4d4562f687a1', foundational, normative_judgments_are_integral_to_social_classification).
narrative_ontology:cs_axiom_status(normative_judgments_are_integral_to_social_classification, holdable).
narrative_ontology:cs_axiom_grounding('52969615-dd15-4763-a735-4d4562f687a1', normative_judgments_are_integral_to_social_classification, deontological).
narrative_ontology:cs_reference_frame('52969615-dd15-4763-a735-4d4562f687a1', integrated_analytical_framework).
narrative_ontology:cs_drift_state('52969615-dd15-4763-a735-4d4562f687a1', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('52969615-dd15-4763-a735-4d4562f687a1', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, analytical_observers).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, policy_analysts).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__hybrid_pragmatic_reading, epistemic_pluralism).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__hybrid_pragmatic_reading, normative_realism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a framework that acknowledges both objective and normative dimensions of constraints, allowing for nuanced analysis without forcing a single, reductive classification. This reading provides a more accurate lens for complex social phenomena.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, analytical_observers, beneficiary,
    analytical, generational, analytical, universal).

% Finds the hybrid approach useful for designing interventions, as it distinguishes between constraints that can be objectively measured (and thus engineered) and those that require normative debate (and thus political engagement). This helps in framing policy recommendations.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, policy_analysts, beneficiary,
    moderate, biographical, mobile, global).

% Would argue that any normative judgment contaminates the diagnostic purity of the framework, insisting on a purely observational approach to classification. They are excluded from this reading's core premise by its embrace of normative judgment for peripheral types.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, immutable_diagnostic_advocates, excluded,
    organized, generational, identity_locked, global).

% Would argue that the entire typology is a rhetorical tool, and that claiming an 'objective core' is a form of reification that obscures its persuasive function. They are excluded by this reading's commitment to an observational core.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, rhetorical_scaffold_advocates, excluded,
    organized, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates understanding across different epistemic communities by providing a framework that integrates both empirical observation and normative judgment in classifying constraints, allowing for shared discourse on complex social structures.
% TRANSFER_FUNCTION: Transfers analytical clarity and methodological flexibility to observers and analysts, enabling them to apply the typology to a wider range of phenomena without forcing a purely objective or purely subjective interpretation.
% ABSENT_VOICES: Advocates for purely objective or purely rhetorical interpretations of the typology are absent from this reading's core assumptions, as their positions are explicitly integrated as alternative, but not dominant, framings. They would argue for the primacy of their own readings.
% DISAPPEARANCE_RATIONALE: If this hybrid reading disappeared, the analytical community would likely revert to more polarized debates about the nature of constraints, either over-emphasizing objective measurement or reducing all classification to rhetorical moves, losing the nuanced middle ground this reading provides.
% FOUNDING_PROBLEM: The problem of classifying complex socio-technical constraints where purely objective metrics fail to capture the full picture, and purely subjective interpretations lack rigor, leading to unproductive debates between different analytical camps.
% FOUNDING_PROBLEM_CORROBORATION: Independent philosophers of science and interdisciplinary researchers attest to the ongoing challenge of integrating empirical and normative dimensions in social theory, corroborating the live status of the problem this reading addresses. This corroboration comes from outside the direct beneficiaries of the framework itself.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__hybrid_pragmatic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(deferential_realism_ontology__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).
:- end_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) as this reading requires intellectual effort to integrate different epistemic stances, but it doesn't actively extract resources. Suppression is also moderate (0.55) because it implicitly suppresses extreme positions (pure objectivism or pure relativism) that would deny the validity of a hybrid approach. Theater ratio is low (0.1) as the reading is primarily functional for analysis, not performative. Accessibility collapse is moderate (0.4) because while it offers a clear path, other analytical frameworks remain accessible. Resistance is low (0.3) as it's a conceptual framework, not an actively enforced social structure.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between those who embrace this hybrid approach and those who advocate for more monolithic, either purely objective or purely subjective, interpretations of the typology. This reading aims to resolve that gap by providing a common ground, but it inherently marginalizes the extreme positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Analytical observers and policy analysts are beneficiaries, as this reading provides them with a more robust and flexible toolset. There are no direct 'victims' in the sense of material extraction, but advocates of purely objective or purely rhetorical readings are 'excluded' from the core premises of this hybrid approach, experiencing a form of intellectual suppression.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_between_core_and_periphery,
    'Where precisely does the ''fixed core'' (Mountains, Ropes) end and the ''contested periphery'' (Tangled Ropes, Snares) begin, and what criteria govern this boundary?',
    'Development of a formal meta-typology that specifies the epistemic conditions under which a constraint''s classification transitions from purely observational to normatively inflected.',
    'A clearer boundary would reduce ambiguity in classification, potentially shifting some ''contested'' peripheral types into the ''fixed'' core if objective criteria are found, or vice-versa if normative elements are found to permeate the core.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(boundary_between_core_and_periphery, conceptual, 'Ambiguity in the demarcation between objective and normative classification domains within the typology.').

omega_variable(
    legitimacy_of_normative_judgments,
    'What is the meta-ethical grounding for the ''normative judgments about legitimate beneficiaries'' that determine peripheral classifications, and is this grounding universally accepted?',
    'Engagement with meta-ethical theories to establish a robust, intersubjectively defensible basis for normative judgments within the framework, or explicit declaration of the framework''s inherent value commitments.',
    'A strong meta-ethical grounding would increase the legitimacy and acceptance of peripheral classifications; a weak or contested grounding would expose them to charges of arbitrary or ideologically driven classification, potentially pushing the entire periphery towards a ''rhetorical scaffold'' reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_normative_judgments, preference, 'The philosophical basis and acceptance of normative criteria in constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__hybrid_pragmatic_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(defe_tr_t5, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(defe_be_t5, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 5, 0.43).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 10, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(defe_su_t5, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 5, 0.53).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__hybrid_pragmatic_reading, information_standard).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology__immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).

% DUAL FORMULATION NOTE:
% This is one of three readings of the 'deferential_realism_ontology' kernel. This reading attempts to integrate objective and normative dimensions, influencing how the other readings are perceived and debated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
