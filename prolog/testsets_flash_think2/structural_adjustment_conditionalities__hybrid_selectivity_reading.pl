% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__hybrid_selectivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__hybrid_selectivity_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: structural_adjustment_conditionalities__hybrid_selectivity_reading
 *   human_readable: Structural Adjustment Conditionalities: Hybrid Selectivity Reading
 *   domain: international_political_economy/development_finance
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.85).
domain_priors:suppression_score(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.9).
domain_priors:theater_ratio(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__hybrid_selectivity_reading, tangled_rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__hybrid_selectivity_reading, "Structural Adjustment Conditionalities: Hybrid Selectivity Reading").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__hybrid_selectivity_reading, "international_political_economy/development_finance").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__hybrid_selectivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__hybrid_selectivity_reading, '4fa93e96-70fa-4fbb-b234-b1628595c71f').
narrative_ontology:cs_kernel_codification('4fa93e96-70fa-4fbb-b234-b1628595c71f', formalized).
narrative_ontology:cs_authority_grounding('4fa93e96-70fa-4fbb-b234-b1628595c71f', extraction).
narrative_ontology:cs_interpretation_layer_present('4fa93e96-70fa-4fbb-b234-b1628595c71f').
narrative_ontology:cs_reading_relation('4fa93e96-70fa-4fbb-b234-b1628595c71f', structural_adjustment_conditionalities__creditor_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('4fa93e96-70fa-4fbb-b234-b1628595c71f', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('4fa93e96-70fa-4fbb-b234-b1628595c71f', foundational, geopolitical_interest_drives_application).
narrative_ontology:cs_axiom_status(geopolitical_interest_drives_application, holdable).
narrative_ontology:cs_axiom_grounding('4fa93e96-70fa-4fbb-b234-b1628595c71f', geopolitical_interest_drives_application, empirically_contingent).
narrative_ontology:cs_axiom('4fa93e96-70fa-4fbb-b234-b1628595c71f', secondary, conditionalities_are_tools_of_power).
narrative_ontology:cs_axiom_status(conditionalities_are_tools_of_power, holdable).
narrative_ontology:cs_axiom_grounding('4fa93e96-70fa-4fbb-b234-b1628595c71f', conditionalities_are_tools_of_power, instrumental).
narrative_ontology:cs_reference_frame('4fa93e96-70fa-4fbb-b234-b1628595c71f', universal_fiscal_discipline).
narrative_ontology:cs_drift_state('4fa93e96-70fa-4fbb-b234-b1628595c71f', post_cold_war_geopolitical_realignment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4fa93e96-70fa-4fbb-b234-b1628595c71f', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_states).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_institutions).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, non_strategic_debtor_states).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, vulnerable_populations_in_debtor_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitically_strategic_debtor_states).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitically_strategic_debtor_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Institutions like the IMF and World Bank that design and enforce conditionalities. They benefit from debt repayment, policy alignment, and maintaining their influence in global financial governance. They selectively apply conditionalities based on geopolitical considerations.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Powerful states that benefit from the stability, resource access, and geopolitical influence secured through the conditionalities imposed on debtor nations. They exert pressure for selective application.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_states, beneficiary,
    institutional, generational, arbitrage, global).

% Debtor nations lacking geopolitical importance, subjected to harsh and uncompromising conditionalities. They face severe austerity, privatization, and social cuts, leading to economic hardship and social unrest, with few alternatives for financing.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, non_strategic_debtor_states, payer,
    powerless, biographical, trapped, national).

% Debtor nations with significant geopolitical or resource importance, often receiving waivers, softer terms, or less stringent enforcement of conditionalities. They benefit from continued access to finance while avoiding the harshest impacts.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitically_strategic_debtor_states, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitically_strategic_debtor_states, payer).

% Citizens in debtor states, particularly the poor and marginalized, who bear the direct costs of austerity measures, cuts to public services, and economic restructuring mandated by conditionalities.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, vulnerable_populations_in_debtor_states, payer,
    powerless, immediate, trapped, local).

% Academics and researchers who critically analyze the impact and selective application of conditionalities, often highlighting their negative social consequences and geopolitical underpinnings.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, development_economists_critics, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure fiscal discipline, debt repayment, and market-oriented reforms in debtor states, while also aligning their policies with the geopolitical and economic interests of core creditor institutions and hegemon-aligned states.
% TRANSFER_FUNCTION: Transfers economic policy autonomy, public assets (through privatization), and resources from debtor states (especially non-strategic ones) to creditor institutions and hegemon-aligned states, often at the expense of social welfare and national development priorities.
% ABSENT_VOICES: Citizens, civil society organizations, and labor unions in non-strategic debtor states, who are directly impacted by austerity and structural reforms but are largely excluded from the negotiation and design of conditionalities.
% DISAPPEARANCE_RATIONALE: If conditionalities and their selective enforcement vanished, non-strategic debtor states would regain significant policy autonomy, potentially re-prioritizing social spending and national development over debt servicing. The geopolitical leverage of creditor states and institutions would diminish, leading to a substantial reordering of international financial and political relations.
% FOUNDING_PROBLEM: To address sovereign debt crises, ensure repayment to international creditors, and promote economic stability and market-oriented reforms in developing countries, particularly after the debt crises of the 1980s.
% FOUNDING_PROBLEM_CORROBORATION: Creditor institutions and their aligned states claim the problem of fiscal instability and governance is still live. Critics (development economists, civil society organizations, and some former debtor state officials) attest that the founding problem has largely shifted from genuine crisis resolution to a tool for geopolitical and economic leverage, with selective application as key evidence. This is supported by independent economic analyses and historical case studies.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__hybrid_selectivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__hybrid_selectivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__hybrid_selectivity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(structural_adjustment_conditionalities__hybrid_selectivity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    geopolitical_vs_economic_drivers,
    'To what extent are conditionalities driven by genuine economic concerns for fiscal sustainability versus geopolitical interests of hegemon-aligned states?',
    'Comparative analysis of conditionalities applied to states with similar economic profiles but different geopolitical significance, controlling for other factors. Disclosure of internal decision-making documents from creditor institutions.',
    'If geopolitical drivers are dominant, the constraint''s extractiveness and suppression are more directly tied to power projection, strengthening its Snare-like qualities. If economic drivers are primary, the coordination function is more salient, pushing it closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_vs_economic_drivers, empirical, 'Distinguishing economic vs. geopolitical motivations for conditionalities.').

omega_variable(
    development_vs_creditor_interests,
    'Do conditionalities genuinely promote long-term sustainable development in debtor states, or do they primarily serve short-term creditor interests and market access?',
    'Longitudinal studies tracking development indicators (poverty, inequality, human development) in states under conditionalities versus control groups, disaggregated by geopolitical status. Independent evaluations of policy outcomes.',
    'If conditionalities consistently undermine development, the extraction component is amplified, and the coordination narrative is further exposed as cover. If they show clear development benefits, the coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(development_vs_creditor_interests, empirical, 'Assessing the long-term impact of conditionalities on development outcomes.').

omega_variable(
    mechanism_of_selectivity,
    'What are the precise formal and informal mechanisms through which conditionalities are selectively applied (e.g., explicit waivers, ''constructive engagement,'' differential interpretation of compliance, or implicit threats)?',
    'Detailed case studies of negotiation processes for strategic vs. non-strategic debtor states, including interviews with negotiators and analysis of leaked documents. Ethnographic studies of policy implementation.',
    'Understanding the mechanisms clarifies the ''theater ratio'' and ''suppression'' components. Formal waivers might reduce perceived suppression for strategic states, while informal pressure on weak states might increase it, even if not explicitly codified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mechanism_of_selectivity, empirical, 'Clarifying the formal and informal channels of selective enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stru_tr_t10, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(stru_tr_t20, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(stru_tr_t30, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(stru_tr_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 40, 0.5).

% Extraction over time
narrative_ontology:measurement(stru_be_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(stru_be_t10, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(stru_be_t20, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(stru_be_t30, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement(stru_be_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 40, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(stru_su_t10, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(stru_su_t20, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(stru_su_t30, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 30, 0.85).
narrative_ontology:measurement(stru_su_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 40, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__hybrid_selectivity_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
