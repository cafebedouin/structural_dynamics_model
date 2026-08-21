% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__proportionality_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__proportionality_balancing, []).

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
 *   constraint_id: humane_treatment_standard__proportionality_balancing
 *   human_readable: Common Article 3: Proportionality Balancing for Detainee Treatment
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   This constraint represents the 'proportionality balancing' reading of
 *   Common Article 3 of the Geneva Conventions, which requires humane
 *   treatment for detainees. This reading interprets CA3 as mandating a
 *   case-by-case assessment to balance detainee dignity against security
 *   needs, rather than imposing absolute prohibitions or granting unlimited
 *   discretion. It positions judicial bodies as gatekeepers, moderating the
 *   constraint on interrogators with procedural safeguards. This is one
 *   reading of the 'humane_treatment_standard' kernel, distinct from
 *   'absolute_prohibition' and 'contextual_necessity' readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, 0.45).
domain_priors:suppression_score(humane_treatment_standard__proportionality_balancing, 0.6).
domain_priors:theater_ratio(humane_treatment_standard__proportionality_balancing, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, extractiveness, 0.45).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__proportionality_balancing, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__proportionality_balancing, "Common Article 3: Proportionality Balancing for Detainee Treatment").
narrative_ontology:topic_domain(humane_treatment_standard__proportionality_balancing, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__proportionality_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__proportionality_balancing, 'd82fbacc-7071-4fe6-b225-87e2f082b211').
narrative_ontology:cs_kernel_codification('d82fbacc-7071-4fe6-b225-87e2f082b211', fixed_text).
narrative_ontology:cs_authority_grounding('d82fbacc-7071-4fe6-b225-87e2f082b211', lineage).
narrative_ontology:cs_interpretation_layer_present('d82fbacc-7071-4fe6-b225-87e2f082b211').
narrative_ontology:cs_reading_relation('d82fbacc-7071-4fe6-b225-87e2f082b211', humane_treatment_standard__absolute_prohibition, coexists_with).
narrative_ontology:cs_reading_relation('d82fbacc-7071-4fe6-b225-87e2f082b211', humane_treatment_standard__contextual_necessity, coexists_with).
narrative_ontology:cs_axiom('d82fbacc-7071-4fe6-b225-87e2f082b211', foundational, dignity_and_security_are_balanceable).
narrative_ontology:cs_axiom_status(dignity_and_security_are_balanceable, holdable).
narrative_ontology:cs_axiom_grounding('d82fbacc-7071-4fe6-b225-87e2f082b211', dignity_and_security_are_balanceable, deontological).
narrative_ontology:cs_axiom('d82fbacc-7071-4fe6-b225-87e2f082b211', secondary, judicial_review_ensures_proportionality).
narrative_ontology:cs_axiom_status(judicial_review_ensures_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('d82fbacc-7071-4fe6-b225-87e2f082b211', judicial_review_ensures_proportionality, conventional).
narrative_ontology:cs_reference_frame('d82fbacc-7071-4fe6-b225-87e2f082b211', post_geneva_conventions_era).
narrative_ontology:cs_drift_state('d82fbacc-7071-4fe6-b225-87e2f082b211', contemporary_counterterrorism_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d82fbacc-7071-4fe6-b225-87e2f082b211', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__proportionality_balancing, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, detaining_states_security_agencies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, judicial_bodies).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, detainees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate detention facilities and conduct interrogations. This reading allows them flexibility to balance security needs against detainee dignity, avoiding absolute prohibitions but requiring justification for methods. They benefit from discretion but are constrained by judicial oversight.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detaining_states_security_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Are subject to treatment standards that balance their dignity against security imperatives. Their experience of humane treatment is contingent on the proportionality assessment, which can vary case-by-case. They bear the direct costs of any treatment deemed 'proportionate' but still degrading.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detainees, payer,
    powerless, immediate, trapped, local).

% Serve as gatekeepers, reviewing the proportionality of treatment on a case-by-case basis. They interpret and apply the balancing test, providing a check on executive discretion but also legitimizing certain forms of treatment that might otherwise be prohibited. They benefit from expanded interpretive authority.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, judicial_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Argue for absolute prohibitions against degrading treatment and view proportionality balancing as a dangerous erosion of detainee rights. They are often excluded from the direct decision-making process but exert pressure through public discourse and legal challenges.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, human_rights_advocates, excluded,
    organized, generational, constrained, global).

% Analyze the application and interpretation of Common Article 3, including the proportionality balancing approach. They assess its effectiveness in protecting detainees and its implications for state sovereignty and international legal norms.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, international_humanitarian_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for states to manage the tension between security imperatives and the humane treatment of detainees, allowing for context-specific application while maintaining a legal standard.
% TRANSFER_FUNCTION: Transfers a degree of interpretive discretion from absolute prohibitions to judicial and executive bodies, allowing for a 'balancing' of detainee dignity against state security needs, which can result in a reduction of protections for detainees in specific contexts.
% ABSENT_VOICES: Human rights advocates and organizations, who argue for absolute prohibitions, are often marginalized in the legal and policy debates that shape the application of proportionality balancing, as their position is seen as 'unrealistic' by security-focused actors.
% DISAPPEARANCE_RATIONALE: If this proportionality balancing standard vanished, states would either revert to absolute prohibitions (as advocated by some) or claim unlimited discretion (as advocated by others), leading to a highly polarized and unstable legal landscape for detainee treatment. The current 'middle ground' would collapse.
% FOUNDING_PROBLEM: To establish a minimum standard of humane treatment for persons not taking an active part in hostilities, applicable in non-international armed conflicts, without imposing an unworkable absolute standard that states would ignore.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and international bodies generally agree that the tension between security and dignity remains a live problem. Detaining states emphasize the need for flexibility, while human rights groups highlight the ongoing vulnerability of detainees, corroborating the persistence of the underlying problem from different perspectives.
narrative_ontology:disappearance_verdict(humane_treatment_standard__proportionality_balancing, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__proportionality_balancing, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__proportionality_balancing, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(humane_treatment_standard__proportionality_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__proportionality_balancing, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__proportionality_balancing_tests).
:- end_tests(humane_treatment_standard__proportionality_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while it avoids absolute prohibitions, it still imposes a significant burden of justification and review on detaining authorities. Suppression (0.6) is present as the state actively enforces its interpretation against challenges from human rights groups. Theater ratio (0.2) is low, as the judicial review and balancing process is generally genuine, though its outcomes are often contested. The metrics reflect a system that is genuinely trying to balance competing demands, but with inherent costs to detainee protections.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of detaining states, this is a reasonable and necessary coordination mechanism. From the perspective of detainees and human rights advocates, it is a mechanism that allows for the erosion of fundamental rights under the guise of 'balancing'. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Detaining states' security agencies and judicial bodies are beneficiaries, gaining flexibility and interpretive authority, respectively. Detainees are the primary victims, as their rights are subject to a balancing test that can reduce their protections. Human rights advocates are excluded, as their absolute prohibition stance is not fully accommodated by this balancing framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balancing_test_objectivity,
    'Is the proportionality balancing test applied objectively and consistently across different cases and jurisdictions, or is it susceptible to political and security pressures?',
    'Empirical analysis of judicial review outcomes, comparing stated justifications with actual treatment standards and identifying patterns of bias or inconsistency.',
    'If inconsistent or biased, the effective extractiveness from detainees is higher than measured, and the constraint functions more as a ''snare'' for their rights, with the ''balancing'' serving as cover. If objective, it reinforces the ''tangled_rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_test_objectivity, empirical, 'Assesses the integrity and consistency of the proportionality balancing application.').

omega_variable(
    slippery_slope_to_discretion,
    'Does the proportionality balancing approach inevitably ''slide'' towards the ''contextual_necessity'' reading, granting excessive discretion to security agencies over time?',
    'Longitudinal study of legal precedents and state practice over several decades, tracking the evolution of ''proportionality'' interpretations and their practical effects on detainee treatment.',
    'If a consistent slide is observed, the constraint''s long-term classification trends towards ''snare'' or ''piton'' as its protective function atrophies. If the judicial gatekeeping holds, it remains a ''tangled_rope''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(slippery_slope_to_discretion, conceptual, 'Examines the risk of the balancing test eroding into unlimited discretion.').

omega_variable(
    kernel_reading_divergence,
    'Given the ''absolute_prohibition'' and ''contextual_necessity'' sibling readings, is ''proportionality_balancing'' a stable, distinct interpretation, or an unstable compromise?',
    'Analysis of legal and political discourse: if the ''proportionality_balancing'' reading consistently attracts strong challenges from both ''absolute_prohibition'' and ''contextual_necessity'' camps, it suggests an unstable compromise. If it develops its own robust legal and philosophical grounding, it is stable.',
    'If unstable, the constraint is prone to reclassification towards one of the sibling readings, indicating a fundamental conceptual tension. If stable, it reinforces its current classification as a distinct legal framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Assesses the conceptual stability and distinctness of this reading within the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__proportionality_balancing, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__proportionality_balancing, theater_ratio, 0, 0.15).
narrative_ontology:measurement(huma_tr_t5, humane_treatment_standard__proportionality_balancing, theater_ratio, 5, 0.18).
narrative_ontology:measurement(huma_tr_t10, humane_treatment_standard__proportionality_balancing, theater_ratio, 10, 0.2).
narrative_ontology:measurement(huma_tr_t15, humane_treatment_standard__proportionality_balancing, theater_ratio, 15, 0.21).
narrative_ontology:measurement(huma_tr_t20, humane_treatment_standard__proportionality_balancing, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__proportionality_balancing, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(huma_be_t5, humane_treatment_standard__proportionality_balancing, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(huma_be_t10, humane_treatment_standard__proportionality_balancing, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(huma_be_t15, humane_treatment_standard__proportionality_balancing, base_extractiveness, 15, 0.46).
narrative_ontology:measurement(huma_be_t20, humane_treatment_standard__proportionality_balancing, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__proportionality_balancing, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(huma_su_t5, humane_treatment_standard__proportionality_balancing, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(huma_su_t10, humane_treatment_standard__proportionality_balancing, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(huma_su_t15, humane_treatment_standard__proportionality_balancing, suppression_requirement, 15, 0.61).
narrative_ontology:measurement(huma_su_t20, humane_treatment_standard__proportionality_balancing, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
