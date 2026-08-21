% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__public_health_primary, []).

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
 *   constraint_id: coercion_legitimacy_boundary__public_health_primary
 *   human_readable: Public Health Primary Coercion Legitimacy Boundary
 *   domain: public_health/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'public health primary' reading of the
 *   coercion legitimacy boundary, where the state's power to compel medical
 *   intervention is justified by the imperative of collective harm
 *   prevention. This reading prioritizes the well-being of the community,
 *   especially vulnerable populations, over individual autonomy when faced
 *   with significant public health threats. The high extractiveness and
 *   suppression reflect the direct curtailment of individual liberties for
 *   collective benefit, which is actively enforced by state authorities. This
 *   is one reading of the 'coercion_legitimacy_boundary' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, 0.78).
domain_priors:suppression_score(coercion_legitimacy_boundary__public_health_primary, 0.85).
domain_priors:theater_ratio(coercion_legitimacy_boundary__public_health_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__public_health_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__public_health_primary, "Public Health Primary Coercion Legitimacy Boundary").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__public_health_primary, "public_health/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__public_health_primary, 'b124c3d9-a407-4d13-8ea8-9117e200fcc4').
narrative_ontology:cs_kernel_codification('b124c3d9-a407-4d13-8ea8-9117e200fcc4', formalized).
narrative_ontology:cs_authority_grounding('b124c3d9-a407-4d13-8ea8-9117e200fcc4', lineage).
narrative_ontology:cs_interpretation_layer_present('b124c3d9-a407-4d13-8ea8-9117e200fcc4').
narrative_ontology:cs_reading_relation('b124c3d9-a407-4d13-8ea8-9117e200fcc4', coercion_legitimacy_boundary__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('b124c3d9-a407-4d13-8ea8-9117e200fcc4', coercion_legitimacy_boundary__proportionality_reading, influences).
narrative_ontology:cs_axiom('b124c3d9-a407-4d13-8ea8-9117e200fcc4', foundational, collective_health_supremacy).
narrative_ontology:cs_axiom_status(collective_health_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('b124c3d9-a407-4d13-8ea8-9117e200fcc4', collective_health_supremacy, deontological).
narrative_ontology:cs_axiom('b124c3d9-a407-4d13-8ea8-9117e200fcc4', foundational, state_duty_to_protect_public).
narrative_ontology:cs_axiom_status(state_duty_to_protect_public, holdable).
narrative_ontology:cs_axiom_grounding('b124c3d9-a407-4d13-8ea8-9117e200fcc4', state_duty_to_protect_public, deontological).
narrative_ontology:cs_reference_frame('b124c3d9-a407-4d13-8ea8-9117e200fcc4', unconditional_public_health_imperative).
narrative_ontology:cs_drift_state('b124c3d9-a407-4d13-8ea8-9117e200fcc4', contemporary_rights_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b124c3d9-a407-4d13-8ea8-9117e200fcc4', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, general_public).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, religious_objectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These authorities define the threshold at which collective harm prevention justifies compulsory medical interventions. They issue mandates, enforce compliance, and justify actions based on epidemiological data and public health outcomes. They bear the political cost of public resistance but gain legitimacy from preventing widespread illness.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, state_public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% These individuals are directly protected by the constraint, as it reduces their exposure to infectious diseases. They cannot safely participate in society without high community immunity, making them highly dependent on the state's enforcement of public health measures. They are net beneficiaries of the coercion.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, immunocompromised_individuals, beneficiary,
    powerless, immediate, trapped, local).

% The broader population benefits from reduced disease transmission, maintaining economic activity, and preventing healthcare system overload. They generally support measures that protect collective well-being, even if it infringes on some individual liberties. Their benefit is diffuse but substantial.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, general_public, beneficiary,
    organized, biographical, mobile, national).

% These individuals are directly targeted by compulsory interventions, facing restrictions on movement, employment, or access to public services if they do not comply. They bear the direct cost of compelled medical procedures or social exclusion. Their autonomy is directly curtailed.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals, payer,
    moderate, immediate, constrained, local).

% These individuals face a conflict between their religious beliefs and state mandates. Compliance means violating deeply held convictions, while non-compliance leads to social and economic penalties. Their identity is fused with their objection, making exit (compliance) a profound personal cost.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, religious_objectors, payer,
    moderate, biographical, identity_locked, local).

% These groups monitor and challenge state actions that infringe on individual rights, arguing for a higher bar for compelled medical interventions. They provide legal defense and public commentary, seeking to shift the balance back towards individual autonomy.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to prevent the spread of infectious diseases, ensuring high community immunity and protecting vulnerable populations from severe illness and death.
% TRANSFER_FUNCTION: Transfers individual bodily autonomy and choice to the state in exchange for collective health security and reduced disease burden for the general population, especially the vulnerable.
% ABSENT_VOICES: Individuals who are medically unable to receive interventions but are not immunocompromised (e.g., severe allergies to vaccine components) are often overlooked in the binary of 'vaccinated' vs. 'unvaccinated' and bear the costs of mandates without the benefit of protection.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, states would lose a critical tool for managing public health crises. Disease outbreaks would be more severe, healthcare systems would be overwhelmed, and vulnerable populations would face significantly higher risks, leading to a reorganization of social and economic life around endemic illness.
% FOUNDING_PROBLEM: The problem of managing highly transmissible infectious diseases that pose a significant threat to public health and overwhelm healthcare infrastructure, requiring collective action beyond individual choice.
% FOUNDING_PROBLEM_CORROBORATION: Public health organizations (WHO, CDC), medical associations, and epidemiologists universally corroborate the ongoing threat of infectious diseases and the necessity of collective measures. While civil liberties groups contest the *means*, they do not dispute the *problem* of disease spread itself.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(coercion_legitimacy_boundary__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__public_health_primary, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coercion_legitimacy_boundary__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.78) is high because it directly compels individuals to undergo medical procedures or face significant social penalties, representing a substantial cost to individual liberty. Suppression (0.85) is also high, as the state actively enforces these mandates through legal and social mechanisms, with limited avenues for non-compliance without severe consequences. The theater ratio is low (0.1) because the enforcement is genuinely aimed at public health outcomes, not merely performative. The metrics reflect a robust, actively enforced system designed to achieve collective health goals at the cost of individual choice.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities and vulnerable populations, this constraint is a necessary and legitimate 'tangled rope' that coordinates collective defense against disease. From the perspective of unvaccinated individuals and religious objectors, it operates as a 'snare' that unjustly extracts their bodily autonomy and freedom of conscience. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State public health authorities are the agenda-setters and primary beneficiaries, gaining legitimacy and achieving their mandate. Immunocompromised individuals and the general public are also beneficiaries, receiving protection from disease. Unvaccinated individuals and religious objectors are the primary payers/victims, bearing the direct costs of compelled intervention or social exclusion. Civil liberties advocates act as observers, challenging the constraint's scope and application.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing collective harm from infectious disease) is considered 'live' by its proponents, preventing mandatrophy. The high extractiveness is seen as a necessary cost of coordination, not a sign of atrophy, because the core problem persists and the enforcement is directly tied to addressing it. The classification as a Tangled Rope reflects this dual function: genuine coordination for the many, but with significant extraction from the few.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_harm_threshold_ambiguity,
    'What objective criteria define ''collective harm'' sufficient to outweigh individual autonomy, and is this threshold consistently applied across different diseases and contexts?',
    'Development of a universally accepted, quantitative framework for assessing disease severity, transmissibility, and healthcare system impact, coupled with independent review of its application.',
    'If the threshold is arbitrary or inconsistently applied, the constraint''s legitimacy weakens, and its extractiveness may be reclassified as less justified, potentially shifting it towards a Snare for certain applications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_harm_threshold_ambiguity, conceptual, 'Ambiguity in defining the ''collective harm'' threshold for state intervention.').

omega_variable(
    efficacy_of_compulsion_vs_alternatives,
    'Is compelled medical intervention demonstrably more effective at achieving public health goals than less coercive alternatives (e.g., education, incentives, voluntary measures)?',
    'Comparative studies of public health outcomes in jurisdictions employing different levels of coercion, controlling for other variables.',
    'If less coercive measures prove equally effective, the justification for high suppression and extractiveness diminishes, potentially reclassifying the constraint as a Snare (if coercion is unnecessary) or a Piton (if it persists without clear efficacy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_compulsion_vs_alternatives, empirical, 'Effectiveness of compulsion versus less coercive public health strategies.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties, social exclusion) or internalized (fear of social stigma, self-censorship)?',
    'Post-mandate suppression trajectory: if suppression persists after legal mandates are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more insidious.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in public health mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__public_health_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(coer_tr_t5, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 5, 0.1).
narrative_ontology:measurement(coer_tr_t10, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 10, 0.1).
narrative_ontology:measurement(coer_tr_t15, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 15, 0.1).
narrative_ontology:measurement(coer_tr_t20, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(coer_be_t5, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(coer_be_t10, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(coer_be_t15, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 15, 0.77).
narrative_ontology:measurement(coer_be_t20, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 20, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(coer_su_t5, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(coer_su_t10, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(coer_su_t15, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 15, 0.83).
narrative_ontology:measurement(coer_su_t20, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 20, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, bodily_autonomy_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'coercion_legitimacy_boundary' kernel. Each reading represents a distinct structural claim about the balance between individual autonomy and collective harm prevention in public health policy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
