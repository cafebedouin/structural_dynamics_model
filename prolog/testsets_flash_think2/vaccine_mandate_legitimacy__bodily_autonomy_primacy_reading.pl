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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Bodily Autonomy Primacy in Vaccine Mandates
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'bodily_autonomy_primacy_reading'
 *   of the 'vaccine_mandate_legitimacy' kernel. It asserts that medical
 *   self-sovereignty is absolute and state coercion in medical decisions is
 *   categorically impermissible, regardless of public health outcomes. From
 *   this reading's perspective, bodily autonomy is a fundamental, natural
 *   right (claimed_type: mountain). However, the metrics reflect the high
 *   extractiveness and suppression experienced by individuals when vaccine
 *   mandates are imposed, and the high resistance such mandates face. The
 *   structural delta for this reading is that immunocompromised and
 *   vulnerable individuals enter the victim set due to increased exposure
 *   risk.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.85).
domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.7).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, mountain).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "Bodily Autonomy Primacy in Vaccine Mandates").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:emerges_naturally(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'd22acfbc-bfb7-42c8-8d39-fabef92aaeda').
narrative_ontology:cs_kernel_codification('d22acfbc-bfb7-42c8-8d39-fabef92aaeda', formalized).
narrative_ontology:cs_authority_grounding('d22acfbc-bfb7-42c8-8d39-fabef92aaeda', lineage).
narrative_ontology:cs_interpretation_layer_present('d22acfbc-bfb7-42c8-8d39-fabef92aaeda').
narrative_ontology:cs_reading_relation('d22acfbc-bfb7-42c8-8d39-fabef92aaeda', vaccine_mandate_legitimacy__public_health_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('d22acfbc-bfb7-42c8-8d39-fabef92aaeda', vaccine_mandate_legitimacy__risk_stratification_reading, forecloses).
narrative_ontology:cs_axiom('d22acfbc-bfb7-42c8-8d39-fabef92aaeda', foundational, bodily_integrity_absolute).
narrative_ontology:cs_axiom_status(bodily_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('d22acfbc-bfb7-42c8-8d39-fabef92aaeda', bodily_integrity_absolute, deontological).
narrative_ontology:cs_axiom('d22acfbc-bfb7-42c8-8d39-fabef92aaeda', foundational, state_coercion_impermissible_in_medical_decisions).
narrative_ontology:cs_axiom_status(state_coercion_impermissible_in_medical_decisions, holdable).
narrative_ontology:cs_axiom_grounding('d22acfbc-bfb7-42c8-8d39-fabef92aaeda', state_coercion_impermissible_in_medical_decisions, deontological).
narrative_ontology:cs_reference_frame('d22acfbc-bfb7-42c8-8d39-fabef92aaeda', enlightenment_individual_rights).
narrative_ontology:cs_drift_state('d22acfbc-bfb7-42c8-8d39-fabef92aaeda', contemporary_pandemic_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d22acfbc-bfb7-42c8-8d39-fabef92aaeda', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, individuals_asserting_autonomy).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vulnerable_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals claim an absolute right to control their own bodies, rejecting any state-imposed medical intervention. They benefit from the vindication of this principle, but may face social or economic costs if mandates are nonetheless enforced.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, individuals_asserting_autonomy, beneficiary,
    moderate, biographical, identity_locked, national).

% Organizations and movements that champion individual liberty and limited government intervention. They benefit from the legal and philosophical recognition of absolute bodily autonomy, using it to challenge state authority.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements, beneficiary,
    organized, generational, analytical, national).

% Individuals whose immune systems are compromised, making them highly vulnerable to infectious diseases. They bear the increased risk of exposure and severe illness when others decline vaccination based on absolute autonomy claims, as their ability to avoid exposure is limited.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_individuals, payer,
    powerless, immediate, trapped, local).

% Broader groups, including the elderly, infants, and those with chronic conditions, who are at higher risk from infectious diseases and rely on herd immunity for protection. They bear the collective risk of reduced vaccination rates.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vulnerable_populations, payer,
    powerless, immediate, trapped, local).

% Government bodies tasked with protecting public health. From the perspective of this reading, their attempts to implement vaccine mandates are illegitimate coercion, and they are constrained by the asserted absolute right to bodily autonomy.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_authorities, agenda_setter,
    institutional, biographical, constrained, national).

% Legal experts who analyze the scope of individual rights versus state powers. They observe and interpret the implications of absolute bodily autonomy claims for constitutional law and public policy.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, diffuse).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This constraint coordinates individuals around the principle of absolute self-sovereignty, preventing state-led coordination efforts that infringe on individual medical decisions.
% TRANSFER_FUNCTION: It prevents the transfer of individual bodily control to the state, effectively transferring the risk of infectious disease exposure to vulnerable populations who rely on collective immunity.
% ABSENT_VOICES: Public health ethicists who prioritize collective well-being over absolute individual rights, and vulnerable populations who bear the direct health consequences of reduced herd immunity, are often marginalized in discussions framed by absolute autonomy.
% DISAPPEARANCE_RATIONALE: If the principle of absolute bodily autonomy vanished overnight, the state's power to implement public health measures, including vaccine mandates, would be significantly expanded. This would fundamentally alter the balance between individual rights and collective welfare, leading to a reorganization of public health policy and individual freedoms.
% FOUNDING_PROBLEM: The problem of state overreach and potential tyranny, where individual rights and bodily integrity are infringed upon by governmental authority, particularly in medical contexts.
% FOUNDING_PROBLEM_CORROBORATION: Historical abuses of state power in medical contexts (e.g., forced sterilizations, unethical human experimentation) and philosophical traditions emphasizing individual liberty and negative rights provide corroboration for the enduring relevance of this founding problem, attested by legal scholars and human rights advocates.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

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
 *   The claimed type is 'mountain' because this reading asserts an absolute, natural right to bodily autonomy. However, the high 'extractiveness' (0.85) and 'suppression' (0.70) reflect the perceived violation of this right when state mandates are enforced. 'Accessibility collapse' is high (0.90) because, from this perspective, the 'alternative' of not having one's bodily autonomy violated is fundamental. 'Resistance' is high (0.80) due to active opposition to mandates based on this principle. The 'theater_ratio' is low (0.10) as the constraint is about a fundamental principle, not performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this reading, the constraint is a fundamental truth protecting individuals. From the perspective of public health authorities or vulnerable populations, the same principle can lead to collective harm and increased risk, highlighting a deep divergence in how the constraint's operation is experienced.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals asserting autonomy and liberty advocacy movements are beneficiaries, as the constraint vindicates their core principle. Immunocompromised and vulnerable populations are victims, as the operation of this principle (preventing mandates) directly increases their exposure risk. Public health authorities are agenda-setters whose actions are constrained by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_context,
    'Is this constraint a genuine natural law, or a constructed principle that benefits identifiable agents by limiting state power?',
    'Analysis of philosophical grounding and historical context of ''natural rights'' claims, alongside empirical observation of who benefits and who bears costs when this principle is applied in public health crises.',
    'If primarily constructed for benefit, the ''mountain'' claim would be reclassified as a ''tangled_rope'' or ''snare'' from the perspective of those bearing the costs, triggering a false summit detection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_context, conceptual, 'Ambiguity between natural law and constructed principle for bodily autonomy.').

omega_variable(
    scope_of_autonomy_vs_externality,
    'Is bodily autonomy truly absolute, or does its scope diminish when individual choices create direct, unmitigable externalities for vulnerable others?',
    'Ethical and legal deliberation on the ''harm principle'' and the definition of direct vs. indirect harm in public health contexts, potentially leading to a re-evaluation of the principle''s boundaries.',
    'If externalities are deemed to limit autonomy, the ''forecloses'' relationship with public health primacy readings would weaken, potentially shifting to ''coexists_with'' or ''influences'', and the victim set for vulnerable populations would be more explicitly acknowledged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_autonomy_vs_externality, conceptual, 'Limits of bodily autonomy when individual choices impact others.').

omega_variable(
    empirical_basis_of_risk_assessment,
    'Does the claim of absolute bodily autonomy implicitly rely on empirical assumptions about the actual risk posed by unvaccinated individuals or the efficacy of mandates, which could be challenged by scientific evidence?',
    'Systematic review of epidemiological data, vaccine efficacy studies, and public health intervention outcomes. If empirical claims are found to be foundational, their refutation could weaken the axiom''s status.',
    'If empirical claims are foundational and refuted, the ''deontological'' grounding of the axiom might be challenged, potentially leading to an ''axiom_overriding'' drift state and weakening the reading''s structural integrity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_basis_of_risk_assessment, empirical, 'Implicit empirical claims underlying deontological autonomy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 2020, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(vacc_be_t2020, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 2020, 0.75).
narrative_ontology:measurement(vacc_be_t2021, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 2021, 0.8).
narrative_ontology:measurement(vacc_be_t2022, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 2022, 0.85).
narrative_ontology:measurement(vacc_be_t2023, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 2023, 0.85).
narrative_ontology:measurement(vacc_be_t2024, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t2020, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 2020, 0.5).
narrative_ontology:measurement(vacc_su_t2021, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 2021, 0.6).
narrative_ontology:measurement(vacc_su_t2022, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 2022, 0.7).
narrative_ontology:measurement(vacc_su_t2023, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 2023, 0.7).
narrative_ontology:measurement(vacc_su_t2024, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
