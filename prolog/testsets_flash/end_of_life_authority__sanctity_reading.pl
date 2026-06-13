% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__sanctity_reading, []).

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
 *   constraint_id: end_of_life_authority__sanctity_reading
 *   human_readable: Sanctity of Life Principle in End-of-Life Policy
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint represents the 'sanctity of life' reading of end-of-life
 *   authority, which holds that human life has intrinsic value and prohibits
 *   intentional life-ending, regardless of individual preference. It is a
 *   contested kernel, with other readings emphasizing individual autonomy or
 *   the empirical risks of a 'slippery slope'. This reading places vulnerable
 *   populations (elderly, disabled, economically disadvantaged) into the
 *   victim set due to the risk of coercion, and imposes a categorical
 *   prohibition on assisted dying, limiting the physician's role to life
 *   preservation. The constraint is actively enforced through legal and
 *   medical frameworks.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, 0.65).
domain_priors:suppression_score(end_of_life_authority__sanctity_reading, 0.75).
domain_priors:theater_ratio(end_of_life_authority__sanctity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__sanctity_reading, "Sanctity of Life Principle in End-of-Life Policy").
narrative_ontology:topic_domain(end_of_life_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__sanctity_reading, 'dc10795f-9658-44e0-9061-f6ba378db7ea').
narrative_ontology:cs_kernel_codification('dc10795f-9658-44e0-9061-f6ba378db7ea', formalized).
narrative_ontology:cs_authority_grounding('dc10795f-9658-44e0-9061-f6ba378db7ea', lineage).
narrative_ontology:cs_interpretation_layer_present('dc10795f-9658-44e0-9061-f6ba378db7ea').
narrative_ontology:cs_reading_relation('dc10795f-9658-44e0-9061-f6ba378db7ea', end_of_life_authority__autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc10795f-9658-44e0-9061-f6ba378db7ea', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('dc10795f-9658-44e0-9061-f6ba378db7ea', foundational, human_life_has_intrinsic_value).
narrative_ontology:cs_axiom_status(human_life_has_intrinsic_value, holdable).
narrative_ontology:cs_axiom_grounding('dc10795f-9658-44e0-9061-f6ba378db7ea', human_life_has_intrinsic_value, deontological).
narrative_ontology:cs_axiom('dc10795f-9658-44e0-9061-f6ba378db7ea', foundational, intentional_life_ending_is_categorically_wrong).
narrative_ontology:cs_axiom_status(intentional_life_ending_is_categorically_wrong, holdable).
narrative_ontology:cs_axiom_grounding('dc10795f-9658-44e0-9061-f6ba378db7ea', intentional_life_ending_is_categorically_wrong, deontological).
narrative_ontology:cs_reference_frame('dc10795f-9658-44e0-9061-f6ba378db7ea', traditional_medical_ethics_preservation).
narrative_ontology:cs_drift_state('dc10795f-9658-44e0-9061-f6ba378db7ea', contemporary_autonomy_advocacy_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('dc10795f-9658-44e0-9061-f6ba378db7ea', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__sanctity_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, religious_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, pro_life_advocacy_groups).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, healthcare_systems_focused_on_preservation).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, terminally_ill_patients_seeking_assisted_dying).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, elderly_vulnerable_to_coercion).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, disabled_persons_at_risk_of_devaluation).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__sanctity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(end_of_life_authority__sanctity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__sanctity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial, as it denies individuals control over their own death, leading to prolonged suffering for some. Suppression (0.75) is high due to legal prohibitions and professional ethical codes that actively prevent assisted dying. Theater ratio (0.20) is low, indicating that the constraint's enforcement is largely functional in upholding its stated purpose, though some performative aspects exist in public discourse. The increasing extractiveness and suppression over time reflect the hardening of positions in response to growing advocacy for autonomy.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries, the constraint is a necessary moral safeguard, a 'rope' that coordinates society around a fundamental value. From the perspective of patients seeking assisted dying, it is a 'snare' that traps them in suffering. The engine's classification as a 'tangled_rope' reflects this hybrid nature: it coordinates a moral stance but does so with significant extraction from those whose preferences are overridden.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious institutions and pro-life groups are clear beneficiaries, as the constraint aligns with their moral frameworks and policy goals. Healthcare systems and physicians, while also constrained, benefit from clear ethical boundaries. Terminally ill patients seeking assisted dying are direct targets/victims, bearing the cost of denied autonomy. Vulnerable elderly and disabled persons are also victims, as the constraint, while ostensibly protecting them, can also limit their agency and choices, potentially leading to prolonged suffering or a lack of control over their end-of-life process.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sanctity_vs_autonomy_primacy,
    'Which principle (sanctity of life or individual autonomy) should take precedence in end-of-life decisions?',
    'Societal consensus shift, legislative action, or judicial rulings that re-prioritize one principle over the other.',
    'If autonomy gains primacy, the constraint would shift towards a ''rope'' or ''scaffold'' for those seeking assisted dying, with significantly lower extraction. If sanctity is reaffirmed, the current ''tangled_rope'' classification would be reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sanctity_vs_autonomy_primacy, preference, 'The fundamental moral choice between two competing ethical principles.').

omega_variable(
    vulnerability_protection_vs_autonomy_denial,
    'To what extent does the categorical prohibition on assisted dying genuinely protect vulnerable populations from coercion, versus denying autonomy to competent individuals?',
    'Empirical studies on the incidence of coercion in jurisdictions with legalized assisted dying, and qualitative research on the experiences of vulnerable populations under both regimes.',
    'If coercion is rare and manageable, the ''victim'' status of vulnerable groups under this reading would be re-evaluated, potentially reducing the perceived justification for the high suppression. If coercion is widespread, it would reinforce the current structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerability_protection_vs_autonomy_denial, empirical, 'The actual protective effect of the prohibition versus its cost in autonomy.').

omega_variable(
    slippery_slope_empirical_evidence,
    'Does the empirical evidence from jurisdictions that have legalized assisted dying support the ''slippery slope'' hypothesis (expansion beyond initial criteria)?',
    'Longitudinal comparative studies of end-of-life practices and legal changes in jurisdictions with varying assisted dying laws.',
    'Strong empirical evidence for a ''slippery slope'' would strengthen the justification for the sanctity reading''s categorical prohibition, potentially increasing its perceived legitimacy. Weak or absent evidence would undermine a key argument for its persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slippery_slope_empirical_evidence, empirical, 'Empirical validity of the ''slippery slope'' argument.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__sanctity_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t1950, end_of_life_authority__sanctity_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(end__tr_t1970, end_of_life_authority__sanctity_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(end__tr_t1990, end_of_life_authority__sanctity_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(end__tr_t2010, end_of_life_authority__sanctity_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(end__tr_t2024, end_of_life_authority__sanctity_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(end__be_t1950, end_of_life_authority__sanctity_reading, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(end__be_t1970, end_of_life_authority__sanctity_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(end__be_t1990, end_of_life_authority__sanctity_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(end__be_t2010, end_of_life_authority__sanctity_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(end__be_t2024, end_of_life_authority__sanctity_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t1950, end_of_life_authority__sanctity_reading, suppression_requirement, 1950, 0.6).
narrative_ontology:measurement(end__su_t1970, end_of_life_authority__sanctity_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(end__su_t1990, end_of_life_authority__sanctity_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(end__su_t2010, end_of_life_authority__sanctity_reading, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(end__su_t2024, end_of_life_authority__sanctity_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__sanctity_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the 'sanctity_reading' of the 'end_of_life_authority' kernel. It is structurally distinct from the 'autonomy_reading' and 'slippery_slope_mechanism' readings, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
