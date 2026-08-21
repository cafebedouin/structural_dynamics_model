% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_directive__behavioral_competence_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: aneyoshi_stone_directive__behavioral_competence_reading
 *   human_readable: Aneyoshi Stone Directive (Behavioral Competence Reading)
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'behavioral competence' reading of
 *   the Aneyoshi stone directive. In this reading, the stone markers, placed
 *   after the 1933 tsunami, serve as a binding land-use constraint that
 *   successfully guides coastal residents to build above the tsunami
 *   inundation line. The constraint is seen as a 'mountain' because it
 *   reflects a physical reality (tsunami risk) and a deeply ingrained,
 *   effective behavioral response, with negligible extraction. The 2011
 *   tsunami provided a stark validation of the stones' efficacy, as the only
 *   homes spared were those built above the markers. This reading emphasizes
 *   the stones' continued functional role in disaster preparedness.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, 0.1).
domain_priors:theater_ratio(aneyoshi_stone_directive__behavioral_competence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_stone_directive__behavioral_competence_reading, "Aneyoshi Stone Directive (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_stone_directive__behavioral_competence_reading, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__behavioral_competence_reading, '60c68205-9a00-470d-b832-6e9d74e14e4f').
narrative_ontology:cs_kernel_codification('60c68205-9a00-470d-b832-6e9d74e14e4f', fixed_text).
narrative_ontology:cs_authority_grounding('60c68205-9a00-470d-b832-6e9d74e14e4f', practice).
narrative_ontology:cs_interpretation_layer_present('60c68205-9a00-470d-b832-6e9d74e14e4f').
narrative_ontology:cs_reading_relation('60c68205-9a00-470d-b832-6e9d74e14e4f', aneyoshi_stone_directive__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('60c68205-9a00-470d-b832-6e9d74e14e4f', foundational, tsunami_risk_is_permanent).
narrative_ontology:cs_axiom_status(tsunami_risk_is_permanent, holdable).
narrative_ontology:cs_axiom_grounding('60c68205-9a00-470d-b832-6e9d74e14e4f', tsunami_risk_is_permanent, empirically_contingent).
narrative_ontology:cs_axiom('60c68205-9a00-470d-b832-6e9d74e14e4f', foundational, ancestral_wisdom_is_binding).
narrative_ontology:cs_axiom_status(ancestral_wisdom_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('60c68205-9a00-470d-b832-6e9d74e14e4f', ancestral_wisdom_is_binding, conventional).
narrative_ontology:cs_reference_frame('60c68205-9a00-470d-b832-6e9d74e14e4f', post_1933_tsunami_rebuilding_norm).
narrative_ontology:cs_drift_state('60c68205-9a00-470d-b832-6e9d74e14e4f', pre_2011_tsunami_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('60c68205-9a00-470d-b832-6e9d74e14e4f', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__behavioral_competence_reading, coastal_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Residents of Aneyoshi and surrounding coastal areas who, by adhering to the stone markers, build their homes above historical tsunami inundation lines, thereby benefiting from the physical protection against future tsunamis. Their compliance is largely passive, driven by generations of tradition and the visible evidence of past disasters.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, coastal_residents, beneficiary,
    powerless, generational, constrained, local).

% The local municipal authority that implicitly upholds the stone directive through zoning and building codes, even if not explicitly referencing the stones. They benefit from reduced disaster risk and associated costs, but also bear the responsibility of maintaining public safety and infrastructure.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, local_government, agenda_setter,
    institutional, generational, constrained, local).

% Academics and practitioners who study the Aneyoshi stone directive as a case study in indigenous disaster resilience and long-term institutional memory. They analyze its effectiveness and persistence, contributing to global knowledge on disaster preparedness.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, disaster_risk_reduction_experts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates land-use decisions among coastal residents over generations, ensuring that new construction remains above the historical tsunami inundation line, thereby preventing collective exposure to future disaster risk.
% TRANSFER_FUNCTION: Transfers the knowledge of safe building zones across generations, from past disaster victims to future residents, effectively transferring a 'safety dividend' by preventing loss of life and property.
% ABSENT_VOICES: Developers or residents who might prioritize immediate economic gain or scenic views over long-term safety are implicitly excluded by the deeply ingrained cultural norm and the visible markers. Their voices are not formally suppressed but are culturally marginalized.
% DISAPPEARANCE_RATIONALE: If the stone directive and its associated cultural memory vanished, future generations might build closer to the coast, unaware of the historical risk, leading to catastrophic losses in subsequent tsunami events. The physical landscape would remain, but human settlement patterns would shift dangerously.
% FOUNDING_PROBLEM: Repeated catastrophic loss of life and property from tsunamis, leading to the realization that human memory is insufficient to prevent future generations from rebuilding in vulnerable areas.
% FOUNDING_PROBLEM_CORROBORATION: The historical record of multiple tsunamis and the continued geological risk corroborate the founding problem's live status. Disaster anthropologists and geologists attest to the ongoing threat, independent of local residents' adherence to the stones.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__behavioral_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(aneyoshi_stone_directive__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__behavioral_competence_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very low (0.05) because the constraint primarily serves to protect residents from a natural hazard, imposing minimal 'cost' beyond building slightly further inland. Suppression is low (0.1) as compliance is largely voluntary and culturally embedded, rather than coercively enforced. Theater ratio is also low (0.05) because the stones' function is direct and effective, not performative. Accessibility collapse is high (0.9) as building below the markers is understood to be extremely risky, effectively collapsing safe alternatives. Resistance is negligible (0.02) due to the clear and present danger of tsunamis and the historical validation of the directive.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of coastal residents, the stones are a naturalized part of their environment, a silent guardian. From an analytical observer's perspective, it's a remarkable example of long-term institutional memory and disaster risk reduction. There is little 'gap' in understanding its function in this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal residents are beneficiaries (d=0.0) as the stones directly protect their lives and property. The local government also benefits from reduced disaster response costs and increased community resilience. There are no clear 'victims' in this reading, as the constraint is seen as a necessary adaptation to a natural hazard.
 *
 * MANDATROPHY ANALYSIS:
 *   In this 'behavioral competence' reading, mandatrophy is not present. The mandate (preventing tsunami deaths) remains critically live, and the constraint (the stones and associated behavioral norms) continues to fulfill that mandate effectively, as evidenced by the 2011 tsunami. The classification as a Mountain reflects its alignment with physical reality and its non-extractive, protective function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_vs_commemorative_status,
    'Is the Aneyoshi stone directive primarily a functional land-use constraint (behavioral competence reading) or a commemorative artifact that has lost its direct behavioral force (commemorative husk reading)?',
    'Longitudinal ethnographic studies of local decision-making processes regarding land use, and analysis of building permit applications relative to the stone markers in the inter-catastrophe period. The 2011 tsunami provided strong empirical evidence for the behavioral competence reading, but the underlying mechanism of compliance (active memory vs. passive tradition) remains open.',
    'If primarily commemorative, the constraint''s extractiveness and suppression would be near zero, and its classification would shift towards Piton or even Mountain (as a naturalized historical feature). If functional, as this reading asserts, it remains a Mountain reflecting a physical reality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(functional_vs_commemorative_status, empirical, 'Ambiguity regarding the primary function of the stone directive.').

omega_variable(
    compliance_mechanism_ambiguity,
    'Is compliance with the stone directive driven by active, conscious memory of past disasters, or by passive, unthinking tradition and habit?',
    'Sociological surveys and interviews with residents across generations, exploring their explicit knowledge of the stones'' purpose versus their implicit adherence to established building practices. Analysis of how new residents integrate into the community''s land-use norms.',
    'If compliance is purely habitual, the constraint''s resilience to changing demographics or external pressures might be lower than if it''s actively understood. This would affect its long-term stability and potentially shift its classification towards a more fragile type if the underlying ''competence'' erodes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_mechanism_ambiguity, empirical, 'Whether compliance is active or passive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__behavioral_competence_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1933, 0.05).
narrative_ontology:measurement(aney_tr_t1950, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(aney_tr_t1970, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(aney_tr_t1990, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 2011, 0.05).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1933, 0.05).
narrative_ontology:measurement(aney_be_t1950, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1950, 0.05).
narrative_ontology:measurement(aney_be_t1970, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1970, 0.05).
narrative_ontology:measurement(aney_be_t1990, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 2011, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t1933, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 1933, 0.1).
narrative_ontology:measurement(aney_su_t1950, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 1950, 0.1).
narrative_ontology:measurement(aney_su_t1970, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 1970, 0.1).
narrative_ontology:measurement(aney_su_t1990, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(aney_su_t2011, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 2011, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive__behavioral_competence_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
