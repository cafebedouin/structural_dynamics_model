% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, []).

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
 *   constraint_id: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
 *   human_readable: Bodily Autonomy Primacy in Vaccine Mandate Legitimacy
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint models the 'bodily autonomy primacy' reading of vaccine
 *   mandate legitimacy, where individual medical self-sovereignty is
 *   considered an absolute right, and state coercion in health matters is
 *   categorically impermissible, regardless of public health outcomes. It is
 *   presented as a 'mountain' because its proponents view it as a
 *   fundamental, unchangeable principle of individual liberty. The low
 *   extractiveness and suppression reflect this reading's internal
 *   consistency and its claim to natural law status, where the constraint
 *   itself does not actively extract from its beneficiaries but rather
 *   protects them from perceived extraction by the state. However, this
 *   reading places a significant burden of risk on vulnerable populations,
 *   who become victims of the collective outcome.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.15).
domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.05).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, mountain).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "Bodily Autonomy Primacy in Vaccine Mandate Legitimacy").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:emerges_naturally(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'b0cd6646-c18a-4fa0-be51-6e2ca5e9b131').
narrative_ontology:cs_kernel_codification('b0cd6646-c18a-4fa0-be51-6e2ca5e9b131', formalized).
narrative_ontology:cs_authority_grounding('b0cd6646-c18a-4fa0-be51-6e2ca5e9b131', lineage).
narrative_ontology:cs_interpretation_layer_present('b0cd6646-c18a-4fa0-be51-6e2ca5e9b131').
narrative_ontology:cs_reading_relation('b0cd6646-c18a-4fa0-be51-6e2ca5e9b131', vaccine_mandate_legitimacy__public_health_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('b0cd6646-c18a-4fa0-be51-6e2ca5e9b131', vaccine_mandate_legitimacy__risk_stratification_reading, forecloses).
narrative_ontology:cs_axiom('b0cd6646-c18a-4fa0-be51-6e2ca5e9b131', foundational, medical_self_sovereignty_absolute).
narrative_ontology:cs_axiom_status(medical_self_sovereignty_absolute, holdable).
narrative_ontology:cs_axiom_grounding('b0cd6646-c18a-4fa0-be51-6e2ca5e9b131', medical_self_sovereignty_absolute, deontological).
narrative_ontology:cs_axiom('b0cd6646-c18a-4fa0-be51-6e2ca5e9b131', foundational, state_coercion_categorically_impermissible).
narrative_ontology:cs_axiom_status(state_coercion_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('b0cd6646-c18a-4fa0-be51-6e2ca5e9b131', state_coercion_categorically_impermissible, deontological).
narrative_ontology:cs_reference_frame('b0cd6646-c18a-4fa0-be51-6e2ca5e9b131', unqualified_individual_liberty).
narrative_ontology:cs_drift_state('b0cd6646-c18a-4fa0-be51-6e2ca5e9b131', contemporary_pandemic_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b0cd6646-c18a-4fa0-be51-6e2ca5e9b131', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, individuals_opposed_to_mandates).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vulnerable_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their right to refuse medical intervention is upheld as absolute, free from state coercion. They benefit from the absence of mandates and the preservation of individual choice, even if it means accepting higher collective risk.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, individuals_opposed_to_mandates, beneficiary,
    moderate, biographical, mobile, national).

% Actively champion the principle of absolute bodily autonomy and frame vaccine mandates as an impermissible overreach of state power. They benefit from the legal and social recognition of this principle.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements, agenda_setter,
    organized, generational, analytical, national).

% Bear a disproportionate risk of severe illness or death from infectious diseases due to the absence of vaccine mandates. Their health and safety are directly impacted by the choices of others, with no effective means of self-protection if community immunity is low.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_individuals, payer,
    powerless, immediate, trapped, local).

% Includes the elderly, infants, and those with co-morbidities who face higher risks from infectious diseases when vaccine uptake is not universal. They pay the cost of increased exposure risk due to the primacy of individual autonomy.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vulnerable_populations, payer,
    powerless, immediate, constrained, local).

% Their ability to implement population-level health interventions, such as vaccine mandates, is severely curtailed. They are excluded from exercising a core function of their mandate, despite evidence of collective benefit.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_authorities, excluded,
    institutional, generational, constrained, national).

% Adjudicate challenges to vaccine mandates based on constitutional principles, including bodily autonomy. Their rulings shape the legal landscape for public health interventions.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, non-negotiable boundary for individual medical decision-making, preventing state overreach into personal health choices and coordinating individual liberty as a supreme value.
% TRANSFER_FUNCTION: Transfers the burden of collective health risk from individuals to the community, particularly to vulnerable populations, in exchange for absolute individual medical freedom.
% ABSENT_VOICES: Public health ethicists and epidemiologists, who would argue for a balancing of individual rights against collective welfare, are marginalized in this framing. Their arguments for mandate efficacy and harm reduction are deemed irrelevant by the absolute nature of the autonomy claim.
% DISAPPEARANCE_RATIONALE: If the absolute primacy of bodily autonomy vanished, the legal and ethical landscape for public health interventions would fundamentally shift. States would gain broader authority to implement mandates, altering individual freedoms and collective health outcomes.
% FOUNDING_PROBLEM: The historical problem of state-imposed medical procedures and eugenics, where individuals were subjected to interventions against their will, leading to severe abuses of power.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties organizations and historical records corroborate the ongoing risk of state overreach in medical matters. However, public health bodies and medical associations contest that the specific context of vaccine mandates for infectious disease prevention is distinct from historical abuses.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, ExtMetricName, E),
    domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading),
    narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) reflects that, from the perspective of this reading, the constraint primarily prevents state extraction rather than performing its own. Suppression (0.05) is minimal because the principle is asserted as self-evident, requiring little active enforcement to maintain its conceptual force. Accessibility collapse is high (0.9) because, if this principle is truly absolute, alternatives to individual choice (like mandates) are logically foreclosed. Resistance is low (0.05) because, within this framework, the principle is widely accepted by its proponents. The claimed type is 'mountain' because it is presented as an unchangeable, fundamental right. The beneficiaries are those whose autonomy is protected, while the victims are those who bear the collective health risks.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of those whose autonomy is protected, this is a fundamental, non-negotiable principle. From the perspective of vulnerable populations, it is a constraint that imposes severe, unchosen risks. The engine's per-seat classification will highlight this divergence, showing a mountain for beneficiaries and a snare-like experience for victims, despite the overall low extractiveness of the principle itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals opposed to mandates and liberty advocacy movements are beneficiaries (d near 0.0) as the constraint directly protects their perceived rights and advances their agenda. Immunocompromised individuals and vulnerable populations are victims (d near 1.0) as they bear the costs of increased exposure risk due to the absence of mandates. Public health authorities are excluded, as their mandate to protect collective health is overridden by this principle.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_principle,
    'Is the absolute primacy of bodily autonomy a genuine natural law (mountain) or a constructed legal/ethical principle that benefits identifiable agents (false summit)?',
    'Philosophical analysis of foundational rights theory, cross-cultural legal comparison, and examination of historical evolution of autonomy concepts. If its ''naturalness'' is found to be contingent on specific cultural or legal traditions, it leans towards a constructed principle.',
    'If found to be a constructed principle, the classification would shift from mountain to a more constructed type (e.g., rope or tangled_rope), reflecting its dependence on active defense by its beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_principle, conceptual, 'Ambiguity regarding the inherent ''naturalness'' of absolute bodily autonomy.').

omega_variable(
    scope_of_autonomy_vs_harm_principle,
    'Does the absolute scope of bodily autonomy inherently conflict with the harm principle (that one''s liberty extends only so far as it does not harm others)?',
    'Legal and ethical adjudication of cases where individual medical choices directly lead to demonstrable harm to others. If such harm is consistently deemed irrelevant to the autonomy claim, the conflict is absolute. If a balancing is introduced, the autonomy claim is not absolute.',
    'If an inherent conflict is found and the harm principle is overridden, the constraint''s classification as a mountain would be reinforced for its beneficiaries, but its extractiveness from victims would be re-evaluated as a direct consequence of this absolute framing. If a balancing is required, the ''absolute'' nature of the autonomy claim would be undermined, shifting its classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_autonomy_vs_harm_principle, conceptual, 'Conflict between absolute bodily autonomy and the harm principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(vacc_tr_t5, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 5, 0.0).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 10, 0.0).
narrative_ontology:measurement(vacc_tr_t15, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 15, 0.0).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 20, 0.0).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(vacc_be_t15, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 15, 0.15).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 5, 0.05).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 10, 0.05).
narrative_ontology:measurement(vacc_su_t15, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 15, 0.05).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 20, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
