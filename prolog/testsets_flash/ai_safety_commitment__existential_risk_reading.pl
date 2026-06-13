% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__existential_risk_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: ai_safety_commitment__existential_risk_reading
 *   human_readable: AI Safety: Existential Risk Prevention
 *   domain: ai_safety/technology_governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the 'existential risk' reading of AI safety,
 *   which prioritizes preventing extinction-level outcomes from misaligned
 *   superintelligent systems. It is a contested framing within the broader AI
 *   safety discourse. The constraint is claimed as a Tangled Rope because it
 *   genuinely coordinates a global research agenda (beneficiaries: humanity,
 *   existential risk researchers) but also extracts resources and attention
 *   from other pressing AI concerns (victims: near-term harms mitigation, AI
 *   development speed) through active advocacy and policy influence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__existential_risk_reading, 0.65).
domain_priors:suppression_score(ai_safety_commitment__existential_risk_reading, 0.4).
domain_priors:theater_ratio(ai_safety_commitment__existential_risk_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__existential_risk_reading, "AI Safety: Existential Risk Prevention").
narrative_ontology:topic_domain(ai_safety_commitment__existential_risk_reading, "ai_safety/technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__existential_risk_reading, 'caf5a1dd-facb-4b1e-987e-067be72fb434').
narrative_ontology:cs_kernel_codification('caf5a1dd-facb-4b1e-987e-067be72fb434', distributed).
narrative_ontology:cs_authority_grounding('caf5a1dd-facb-4b1e-987e-067be72fb434', expertise).
narrative_ontology:cs_interpretation_layer_present('caf5a1dd-facb-4b1e-987e-067be72fb434').
narrative_ontology:cs_reading_relation('caf5a1dd-facb-4b1e-987e-067be72fb434', ai_safety_commitment__near_term_harms_reading, coexists_with).
narrative_ontology:cs_reading_relation('caf5a1dd-facb-4b1e-987e-067be72fb434', ai_safety_commitment__dual_priority_reading, influences).
narrative_ontology:cs_axiom('caf5a1dd-facb-4b1e-987e-067be72fb434', foundational, existential_risk_is_paramount).
narrative_ontology:cs_axiom_status(existential_risk_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('caf5a1dd-facb-4b1e-987e-067be72fb434', existential_risk_is_paramount, deontological).
narrative_ontology:cs_axiom('caf5a1dd-facb-4b1e-987e-067be72fb434', secondary, superintelligence_is_imminent).
narrative_ontology:cs_axiom_status(superintelligence_is_imminent, holdable).
narrative_ontology:cs_axiom_grounding('caf5a1dd-facb-4b1e-987e-067be72fb434', superintelligence_is_imminent, empirically_contingent).
narrative_ontology:cs_reference_frame('caf5a1dd-facb-4b1e-987e-067be72fb434', long_term_catastrophic_risk_prevention).
narrative_ontology:cs_drift_state('caf5a1dd-facb-4b1e-987e-067be72fb434', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('caf5a1dd-facb-4b1e-987e-067be72fb434', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__existential_risk_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, humanity_conditional_on_alignment).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, ai_safety_researchers_existential_risk).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, ai_development_speed).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, near_term_ai_harms_mitigation).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__existential_risk_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_safety_commitment__existential_risk_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_safety_commitment__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because it diverts substantial resources towards highly speculative, long-term problems, often at the expense of addressing immediate, empirically verifiable harms. Suppression (0.40) is moderate, reflecting the active advocacy and institutional pressure to prioritize this framing, which can sideline alternative perspectives. Theater ratio (0.20) is low, as the research and policy efforts are genuinely aimed at the stated goal, even if the problem itself is speculative. The increasing trend in extractiveness and suppression reflects the growing influence and institutionalization of this particular AI safety framing over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of existential risk researchers, this is a crucial, high-stakes coordination problem. From the perspective of those focused on near-term harms, it is an extractive diversion of resources from real, present suffering. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   AI safety researchers focused on existential risk are primary beneficiaries (d near 0.0) as their agenda receives significant funding and influence. Humanity is a conditional beneficiary. AI development speed and near-term harms mitigation are victims (d near 1.0) as they bear the costs of slowdowns, regulatory burdens, and diverted attention. Policymakers are agenda-setters, mediating these pressures.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    speculative_vs_empirical_risk,
    'Is the existential risk from misaligned superintelligence an empirically grounded threat or a speculative philosophical concern?',
    'Development of robust, falsifiable predictive models for AI capabilities and alignment failure modes, or the emergence of empirical evidence for pre-catastrophic misalignment indicators.',
    'If empirically grounded, the high extractiveness and suppression are more justifiable as necessary costs for survival. If purely speculative, the constraint''s extractive nature becomes harder to defend, potentially reclassifying it closer to a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(speculative_vs_empirical_risk, empirical, 'The empirical status of existential AI risk.').

omega_variable(
    resource_allocation_efficiency,
    'Are resources allocated to existential risk mitigation more effective than those allocated to near-term harms mitigation for overall human well-being?',
    'Comparative impact assessments of interventions across different AI safety framings, considering both direct and indirect effects on human welfare.',
    'If near-term interventions prove more effective per unit of resource, the current allocation driven by the existential risk reading would be seen as inefficient and potentially more extractive. If existential risk interventions are demonstrably more impactful, the current resource transfer is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_efficiency, preference, 'Efficiency of resource allocation between different AI safety priorities.').

omega_variable(
    framing_under_determination,
    'Is the ''existential_risk_reading'' the most appropriate framing for AI safety, or does it obscure other critical issues?',
    'A shift in the dominant discourse within the AI ethics and policy community towards a more integrated or near-term focused approach, or a formal re-evaluation of AI safety priorities by major institutional bodies.',
    'If an alternative framing (e.g., ''near_term_harms_reading'') gains dominance, the current constraint would be seen as a misdirection, potentially leading to a reclassification towards a Snare due to its extractive nature and the suppression of alternative concerns. If this framing remains dominant, its current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_under_determination, conceptual, 'The conceptual appropriateness of the existential risk framing for AI safety.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__existential_risk_reading, 2015, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t2015, ai_safety_commitment__existential_risk_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(ai_s_tr_t2020, ai_safety_commitment__existential_risk_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(ai_s_tr_t2025, ai_safety_commitment__existential_risk_reading, theater_ratio, 2025, 0.18).
narrative_ontology:measurement(ai_s_tr_t2030, ai_safety_commitment__existential_risk_reading, theater_ratio, 2030, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t2015, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(ai_s_be_t2020, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(ai_s_be_t2025, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement(ai_s_be_t2030, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2030, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t2015, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2015, 0.25).
narrative_ontology:measurement(ai_s_su_t2020, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2020, 0.32).
narrative_ontology:measurement(ai_s_su_t2025, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2025, 0.38).
narrative_ontology:measurement(ai_s_su_t2030, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2030, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
