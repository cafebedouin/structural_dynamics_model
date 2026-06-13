% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__harm_reduction_reading, []).

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
 *   constraint_id: substance_control_kernel__harm_reduction_reading
 *   human_readable: Harm Reduction Approach to Substance Use
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint describes the 'harm_reduction_reading' of the
 *   'substance_control_kernel'. It frames substance use as a public health
 *   issue, shifting focus from criminal punishment to pragmatic interventions
 *   aimed at reducing negative health and social consequences, independent of
 *   cessation. This reading aims to move people who use drugs out of the
 *   criminal justice system's victim set, but maintains a degree of state
 *   intervention and control, particularly over the supply chain, which
 *   remains largely criminalized. The state shifts from a purely punitive
 *   role to a service provider role, offering interventions like needle
 *   exchanges, safe consumption sites, and naloxone distribution.
 *
 * KEY AGENTS:
 *   - people_who_use_drugs: Primary target/beneficiary (powerless/constrained) — bears paternalistic intervention, benefits from reduced criminalization
 *   - public_health_agencies: Agenda setter/beneficiary (institutional/analytical) — administers interventions, gains mandate
 *   - criminal_justice_system: Payer/agenda_setter (institutional/constrained) — cedes some enforcement, maintains supply-side control
 *   - illicit_drug_suppliers: Victim (organized/trapped) — remains criminalized, targeted by enforcement
 *   - advocacy_groups: Observer (organized/mobile) — pushes for further decriminalization/legalization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__harm_reduction_reading, 0.45).
domain_priors:suppression_score(substance_control_kernel__harm_reduction_reading, 0.6).
domain_priors:theater_ratio(substance_control_kernel__harm_reduction_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__harm_reduction_reading, "Harm Reduction Approach to Substance Use").
narrative_ontology:topic_domain(substance_control_kernel__harm_reduction_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__harm_reduction_reading, '33dd86a4-5618-4753-a202-573e824ca0b1').
narrative_ontology:cs_kernel_codification('33dd86a4-5618-4753-a202-573e824ca0b1', distributed).
narrative_ontology:cs_authority_grounding('33dd86a4-5618-4753-a202-573e824ca0b1', practice).
narrative_ontology:cs_interpretation_layer_present('33dd86a4-5618-4753-a202-573e824ca0b1').
narrative_ontology:cs_reading_relation('33dd86a4-5618-4753-a202-573e824ca0b1', substance_control_kernel__prohibition_reading, influences).
narrative_ontology:cs_reading_relation('33dd86a4-5618-4753-a202-573e824ca0b1', substance_control_kernel__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('33dd86a4-5618-4753-a202-573e824ca0b1', foundational, substance_use_is_health_condition).
narrative_ontology:cs_axiom_status(substance_use_is_health_condition, holdable).
narrative_ontology:cs_axiom_grounding('33dd86a4-5618-4753-a202-573e824ca0b1', substance_use_is_health_condition, empirically_contingent).
narrative_ontology:cs_axiom('33dd86a4-5618-4753-a202-573e824ca0b1', foundational, pragmatic_harm_reduction_is_ethical_imperative).
narrative_ontology:cs_axiom_status(pragmatic_harm_reduction_is_ethical_imperative, holdable).
narrative_ontology:cs_axiom_grounding('33dd86a4-5618-4753-a202-573e824ca0b1', pragmatic_harm_reduction_is_ethical_imperative, deontological).
narrative_ontology:cs_reference_frame('33dd86a4-5618-4753-a202-573e824ca0b1', public_health_first_response).
narrative_ontology:cs_drift_state('33dd86a4-5618-4753-a202-573e824ca0b1', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('33dd86a4-5618-4753-a202-573e824ca0b1', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__harm_reduction_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, illicit_drug_suppliers).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, people_who_use_drugs).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__harm_reduction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(substance_control_kernel__harm_reduction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__harm_reduction_reading_tests).
:- end_tests(substance_control_kernel__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is a Tangled Rope because it genuinely coordinates public health efforts to reduce harm (beneficiary: public_health_agencies, people_who_use_drugs) while still extracting from and suppressing certain groups (victims: illicit_drug_suppliers, and to a lesser extent, people_who_use_drugs through paternalistic interventions and continued supply-side criminalization). Extractiveness (0.45) is moderate, reflecting the shift away from punitive extraction but retaining some control. Suppression (0.6) is also moderate, as criminal penalties for possession are reduced or eliminated, but the illicit supply chain remains heavily suppressed. Theater ratio (0.2) is low, as harm reduction interventions are largely functional, though some performative elements may exist in justifying continued supply-side enforcement.
 *
 * PERSPECTIVAL GAP:
 *   People who use drugs experience a complex shift: they are no longer primarily criminal targets, but become subjects of health interventions, which can still feel coercive. Public health agencies gain a clear mandate and resources. The criminal justice system cedes some ground on demand-side enforcement but retains its role in targeting supply, leading to internal tensions and different perceived benefits/costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies are beneficiaries (d=0.1) as they gain a clear mandate and resources. People who use drugs are both beneficiaries (reduced criminalization) and targets (continued paternalism, criminalized supply chain), leading to a directionality near symmetric (d=0.5). Illicit drug suppliers are clear targets (d=0.9) as their activities remain criminalized and suppressed. The criminal justice system is an agenda setter (d=0.4) as it adapts its role, ceding some enforcement but maintaining others.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling harm reduction as either pure coordination (Rope) or pure extraction (Snare). It acknowledges the genuine public health benefits and coordination function while recognizing the continued, albeit modified, extractive and suppressive elements, particularly concerning the illicit supply chain and potential paternalism towards users. It highlights that the 'mandate' has shifted from punishment to health, but the 'trophy' of control over substance use remains contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine harm reduction approach, or a transitional phase towards legalization/prohibition?',
    'Analysis of policy evolution over time: if the policy consistently prioritizes harm reduction without shifting towards full decriminalization or re-criminalization, it is a stable reading. If it consistently shifts towards one of the other readings, it is a transitional phase.',
    'If a stable harm reduction reading, the classification holds. If transitional, the classification would shift to reflect the emergent dominant reading (legalization_reading or prohibition_reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''harm_reduction_reading'' of the ''substance_control_kernel''. Sibling readings include ''prohibition_reading'' and ''legalization_reading''.').

omega_variable(
    paternalism_vs_autonomy,
    'To what extent does the ''health condition'' framing of substance use, within a harm reduction model, still exert paternalistic control over individuals, rather than fully respecting their autonomy?',
    'Qualitative research on user experiences with harm reduction services, focusing on perceived coercion or lack of agency in treatment and intervention choices. Policy analysis of ''conditions'' attached to harm reduction services.',
    'If paternalism is high, the effective extraction from people_who_use_drugs is higher than measured, as their agency is suppressed. If autonomy is genuinely prioritized, extraction is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paternalism_vs_autonomy, empirical, 'Ambiguity in the paternalistic vs. autonomous nature of health interventions within harm reduction.').

omega_variable(
    supply_chain_criminalization_impact,
    'What is the true impact of continued criminalization of the supply chain on the safety and accessibility of substances for people who use drugs, even under a harm reduction framework?',
    'Epidemiological data on overdose rates and adulterated drug supplies in harm reduction jurisdictions versus fully legalized markets. Economic analysis of illicit market dynamics under harm reduction policies.',
    'If supply chain criminalization significantly exacerbates harms (e.g., fentanyl contamination, price volatility), the ''suppression'' and ''extractiveness'' metrics for people_who_use_drugs are underestimated, and the constraint leans more towards a Snare for them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supply_chain_criminalization_impact, empirical, 'The tension between health-focused demand-side intervention and criminalized supply-side dynamics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__harm_reduction_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__harm_reduction_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(subs_tr_t5, substance_control_kernel__harm_reduction_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(subs_tr_t10, substance_control_kernel__harm_reduction_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__harm_reduction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(subs_be_t5, substance_control_kernel__harm_reduction_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(subs_be_t10, substance_control_kernel__harm_reduction_reading, base_extractiveness, 10, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__harm_reduction_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(subs_su_t5, substance_control_kernel__harm_reduction_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(subs_su_t10, substance_control_kernel__harm_reduction_reading, suppression_requirement, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__harm_reduction_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'substance_control_kernel'. It represents a shift from punitive prohibition towards a public health model, influencing but not fully foreclosing the prohibition reading, and coexisting with the legalization reading as a distinct policy approach.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
