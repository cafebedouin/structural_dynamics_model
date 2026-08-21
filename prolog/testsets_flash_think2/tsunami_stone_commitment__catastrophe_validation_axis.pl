% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__catastrophe_validation_axis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__catastrophe_validation_axis, []).

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
 *   constraint_id: tsunami_stone_commitment__catastrophe_validation_axis
 *   human_readable: 2011 Tsunami as Empirical Validation of Stone Warnings
 *   domain: disaster_anthropology/commitment_system_analysis/institutional_memory
 *
 * SUMMARY:
 *   This constraint represents the 'catastrophe_validation_axis' reading of
 *   the 'tsunami_stone_commitment' kernel. It frames the 2011 tsunami not
 *   merely as a natural disaster, but as a decisive empirical test that
 *   provided binary validation evidence for the efficacy of ancient stone
 *   warnings. The physical reality of the tsunami's destructive power acts as
 *   an unyielding 'mountain' constraint, adjudicating the success or failure
 *   of intergenerational commitments to disaster preparedness. This reading
 *   focuses on the tsunami's role as an objective, non-negotiable arbiter of
 *   truth regarding human-made warning systems.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__catastrophe_validation_axis, 0.05).
domain_priors:suppression_score(tsunami_stone_commitment__catastrophe_validation_axis, 0.95).
domain_priors:theater_ratio(tsunami_stone_commitment__catastrophe_validation_axis, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, extractiveness, 0.05).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__catastrophe_validation_axis, mountain).
narrative_ontology:human_readable(tsunami_stone_commitment__catastrophe_validation_axis, "2011 Tsunami as Empirical Validation of Stone Warnings").
narrative_ontology:topic_domain(tsunami_stone_commitment__catastrophe_validation_axis, "disaster_anthropology/commitment_system_analysis/institutional_memory").

domain_priors:emerges_naturally(tsunami_stone_commitment__catastrophe_validation_axis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__catastrophe_validation_axis, 'f14dc06a-8882-4990-a04b-afda694bf2ef').
narrative_ontology:cs_kernel_codification('f14dc06a-8882-4990-a04b-afda694bf2ef', formalized).
narrative_ontology:cs_authority_grounding('f14dc06a-8882-4990-a04b-afda694bf2ef', expertise).
narrative_ontology:cs_reading_relation('f14dc06a-8882-4990-a04b-afda694bf2ef', tsunami_stone_commitment__behavioral_competence_reading, influences).
narrative_ontology:cs_reading_relation('f14dc06a-8882-4990-a04b-afda694bf2ef', tsunami_stone_commitment__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('f14dc06a-8882-4990-a04b-afda694bf2ef', foundational, empirical_validation_is_truth_criterion).
narrative_ontology:cs_axiom_status(empirical_validation_is_truth_criterion, holdable).
narrative_ontology:cs_axiom_grounding('f14dc06a-8882-4990-a04b-afda694bf2ef', empirical_validation_is_truth_criterion, empirically_contingent).
narrative_ontology:cs_axiom('f14dc06a-8882-4990-a04b-afda694bf2ef', foundational, natural_catastrophe_as_unambiguous_test).
narrative_ontology:cs_axiom_status(natural_catastrophe_as_unambiguous_test, holdable).
narrative_ontology:cs_axiom_grounding('f14dc06a-8882-4990-a04b-afda694bf2ef', natural_catastrophe_as_unambiguous_test, empirically_contingent).
narrative_ontology:cs_reference_frame('f14dc06a-8882-4990-a04b-afda694bf2ef', scientific_empirical_adjudication).
narrative_ontology:cs_drift_state('f14dc06a-8882-4990-a04b-afda694bf2ef', post_2011_tsunami, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f14dc06a-8882-4990-a04b-afda694bf2ef', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__catastrophe_validation_axis, future_generations).
narrative_ontology:constraint_victim(tsunami_stone_commitment__catastrophe_validation_axis, contemporary_coastal_communities).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__catastrophe_validation_axis, intergenerational_disaster_knowledge).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__catastrophe_validation_axis, empirical_test_of_commitment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authored the stone inscriptions as warnings, establishing a commitment to intergenerational knowledge transfer. Their 'agenda' was to prevent future loss of life, and the tsunami tested the efficacy of their method.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, past_generations_who_built_stones, agenda_setter,
    organized, generational, analytical, local).

% Live in areas vulnerable to tsunamis. Their fate is directly impacted by the empirical test: those who heeded the stones survived, those who did not perished. They pay the ultimate cost of non-compliance with the warnings.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, contemporary_coastal_communities, payer,
    powerless, immediate, trapped, local).

% Benefit from the validated knowledge of tsunami risk and effective warning methods. The 2011 event provides a stark lesson for their planning and preparedness, reinforcing the importance of such warnings.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, future_generations, beneficiary,
    moderate, generational, analytical, global).

% Study the physical mechanisms of tsunamis and their impact. They interpret the 2011 event as a critical data point for understanding natural hazards and the effectiveness of human responses, including traditional warning systems.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, geologists_seismologists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The tsunami, as an empirical test, coordinates human understanding and behavior by providing unambiguous feedback on the efficacy of disaster preparedness strategies and intergenerational warning systems.
% TRANSFER_FUNCTION: The constraint itself (the tsunami's physical force) does not transfer anything in an economic sense. However, the event transfers knowledge and consequences from the natural world to human communities, validating or invalidating prior commitments.
% ABSENT_VOICES: No voices are 'absent' from the physical reality of a tsunami. However, those who ignored or were unaware of the stone warnings were effectively 'absent' from the protective coordination offered by the warnings, bearing the full brunt of the physical force.
% DISAPPEARANCE_RATIONALE: If the 2011 tsunami had not occurred, the physical laws governing tsunamis would remain unchanged. The empirical test would simply not have happened, leaving the efficacy of the stone warnings unvalidated by this specific event, but the underlying natural constraint would persist.
% FOUNDING_PROBLEM: The founding problem addressed by the stone warnings was how to transmit critical survival knowledge across generations in a way that would be heeded when a rare, catastrophic event occurred.
% FOUNDING_PROBLEM_CORROBORATION: Scientific consensus on disaster risk and the ongoing challenge of intergenerational knowledge transfer corroborates the live status of the founding problem. Historical records of past disasters and the continued vulnerability of coastal communities further attest to this, independent of the specific communities who built the stones.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__catastrophe_validation_axis, world_unchanged).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__catastrophe_validation_axis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__catastrophe_validation_axis, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(tsunami_stone_commitment__catastrophe_validation_axis, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__catastrophe_validation_axis, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__catastrophe_validation_axis_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, ExtMetricName, E),
    domain_priors:suppression_score(tsunami_stone_commitment__catastrophe_validation_axis, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(tsunami_stone_commitment__catastrophe_validation_axis),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(tsunami_stone_commitment__catastrophe_validation_axis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The tsunami, as a physical force, exhibits extremely low extractiveness (it doesn't 'collect' from anyone in an economic sense) and negligible theater (it is a pure natural phenomenon). Its suppression is near total (human resistance is futile), and it causes a near-complete collapse of alternatives. Resistance to the tsunami itself is minimal because it is physically impossible. The metrics reflect the unyielding nature of the physical event as a test. The time points are arbitrary as the physical laws are constant; the 'test' is a singular event within this constant reality.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the tsunami itself (as a physical phenomenon), there is no 'perspective' or 'gap' – it simply is. The perspectival gap arises in how human agents interpret its role: as a test, a tragedy, or a reminder. This constraint focuses on the 'test' interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain, the tsunami itself has no direct beneficiaries or victims in the sense of an extractive constraint. However, the *knowledge* derived from its empirical test vindicates propositions about intergenerational disaster preparedness. Past generations are 'agenda-setters' for the commitment, contemporary communities are 'payers' of the consequences, and future generations are 'beneficiaries' of the validated knowledge. Geologists and seismologists act as 'observers' interpreting the data.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint, being a natural phenomenon, is not subject to mandatrophy in the human sense. Its 'mandate' is derived from physical laws, which do not atrophy. The analysis here is about how human systems interpret and respond to such immutable constraints, and whether their responses (like the stone warnings) remain 'live' or become 'dead' in the face of empirical reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tsunami_as_empirical_test_ambiguity,
    'Is the 2011 tsunami fundamentally an ''empirical test'' of a commitment system, or primarily a natural disaster with interpretative overlays?',
    'Analysis of historical and contemporary community responses: if the event consistently triggers re-evaluation of warning systems and intergenerational knowledge transfer, it supports the ''empirical test'' framing.',
    'If primarily a disaster, the ''mountain'' classification for the constraint (the test itself) holds, but its coupling to human commitment systems becomes more tenuous. If a clear empirical test, the mountain classification is strengthened as a direct adjudicator of human action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tsunami_as_empirical_test_ambiguity, conceptual, 'Ambiguity of framing a natural catastrophe as a commitment-system test.').

omega_variable(
    behavioral_competence_interaction,
    'How does the binary validation provided by the tsunami interact with the behavioral competence of communities to heed warnings?',
    'Ethnographic studies of communities that survived vs. those that perished, focusing on local knowledge, social cohesion, and trust in traditional warnings versus modern alerts.',
    'If behavioral competence is the primary determinant of survival, the ''catastrophe_validation_axis'' influences but does not solely determine the outcome, suggesting a stronger ''influences'' relationship with the ''behavioral_competence_reading''. If the physical test is overwhelmingly decisive, it strengthens the ''forecloses'' relationship with the ''commemorative_husk_reading''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_competence_interaction, empirical, 'Interaction between empirical validation and human behavioral factors.').

omega_variable(
    commitment_vs_commemoration_ambiguity,
    'Were the stone inscriptions primarily intended as a binding intergenerational commitment to specific actions, or as a general commemoration of past events?',
    'Linguistic and archaeological analysis of the inscriptions, combined with historical records of their interpretation and use by past generations.',
    'If primarily commemorative, the ''catastrophe_validation_axis'' would have less direct ''foreclosing'' power over the ''commemorative_husk_reading'', as the original intent was not a testable commitment. If clearly a commitment, the empirical test is more direct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commitment_vs_commemoration_ambiguity, conceptual, 'Ambiguity in the original intent of the stone inscriptions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__catastrophe_validation_axis, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t0, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 0, 0.01).
narrative_ontology:measurement(tsun_tr_t10, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 10, 0.01).
narrative_ontology:measurement(tsun_tr_t20, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 20, 0.01).

% Extraction over time
narrative_ontology:measurement(tsun_be_t0, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(tsun_be_t10, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 10, 0.05).
narrative_ontology:measurement(tsun_be_t20, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 20, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(tsun_su_t0, tsunami_stone_commitment__catastrophe_validation_axis, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(tsun_su_t10, tsunami_stone_commitment__catastrophe_validation_axis, suppression_requirement, 10, 0.95).
narrative_ontology:measurement(tsun_su_t20, tsunami_stone_commitment__catastrophe_validation_axis, suppression_requirement, 20, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__catastrophe_validation_axis, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
