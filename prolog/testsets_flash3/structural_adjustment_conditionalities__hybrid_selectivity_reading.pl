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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Structural Adjustment Conditionalities (Hybrid Selectivity Reading)
 *   domain: international_political_economy/development_finance/institutional_economics
 *
 * SUMMARY:
 *   This constraint story, 'Structural Adjustment Conditionalities (Hybrid
 *   Selectivity Reading)', is one reading of the
 *   'structural_adjustment_conditionalities' kernel. It posits that
 *   conditionalities, while ostensibly coordination mechanisms, function as
 *   selectively applied discipline, enforced harshly on geopolitically
 *   non-strategic debtor states while being waived or softened for strategic
 *   ones. This selective application allows hegemon-aligned states and core
 *   creditors to extract benefits and maintain influence, while non-strategic
 *   debtors and their vulnerable populations bear the costs. The claimed type
 *   is 'tangled_rope' because it has both a coordination function
 *   (stabilizing the international financial system) and a strong extractive
 *   component (asymmetric enforcement based on geopolitical power).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.78).
domain_priors:suppression_score(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.85).
domain_priors:theater_ratio(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__hybrid_selectivity_reading, tangled_rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__hybrid_selectivity_reading, "Structural Adjustment Conditionalities (Hybrid Selectivity Reading)").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__hybrid_selectivity_reading, "international_political_economy/development_finance/institutional_economics").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__hybrid_selectivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__hybrid_selectivity_reading, '5d75a05c-1298-4b6a-9cde-bbd64d19aef4').
narrative_ontology:cs_kernel_codification('5d75a05c-1298-4b6a-9cde-bbd64d19aef4', formalized).
narrative_ontology:cs_authority_grounding('5d75a05c-1298-4b6a-9cde-bbd64d19aef4', extraction).
narrative_ontology:cs_interpretation_layer_present('5d75a05c-1298-4b6a-9cde-bbd64d19aef4').
narrative_ontology:cs_reading_relation('5d75a05c-1298-4b6a-9cde-bbd64d19aef4', structural_adjustment_conditionalities__creditor_coordination_reading, influences).
narrative_ontology:cs_reading_relation('5d75a05c-1298-4b6a-9cde-bbd64d19aef4', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('5d75a05c-1298-4b6a-9cde-bbd64d19aef4', foundational, conditionalities_are_geopolitically_contingent).
narrative_ontology:cs_axiom_status(conditionalities_are_geopolitically_contingent, holdable).
narrative_ontology:cs_axiom_grounding('5d75a05c-1298-4b6a-9cde-bbd64d19aef4', conditionalities_are_geopolitically_contingent, empirically_contingent).
narrative_ontology:cs_axiom('5d75a05c-1298-4b6a-9cde-bbd64d19aef4', foundational, fiscal_discipline_is_selectively_enforced).
narrative_ontology:cs_axiom_status(fiscal_discipline_is_selectively_enforced, holdable).
narrative_ontology:cs_axiom_grounding('5d75a05c-1298-4b6a-9cde-bbd64d19aef4', fiscal_discipline_is_selectively_enforced, empirically_contingent).
narrative_ontology:cs_reference_frame('5d75a05c-1298-4b6a-9cde-bbd64d19aef4', post_bretton_woods_debt_management).
narrative_ontology:cs_drift_state('5d75a05c-1298-4b6a-9cde-bbd64d19aef4', contemporary_multipolar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5d75a05c-1298-4b6a-9cde-bbd64d19aef4', '').
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

% Administer and enforce structural adjustment conditionalities, ostensibly to ensure fiscal stability and market access for debtor states. They benefit from the stability of the international financial system and the leverage these conditionalities provide.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, international_financial_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from the stability of the international financial system and the geopolitical influence gained by selectively applying or waiving conditionalities based on strategic interests. They are often the primary shareholders in international financial institutions.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_states, beneficiary,
    institutional, generational, arbitrage, global).

% Receive repayment on loans, often with higher certainty due to the enforcement of conditionalities. They benefit from the market discipline imposed on debtor states, which reduces perceived risk.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_institutions, beneficiary,
    organized, biographical, mobile, global).

% Are forced to implement harsh austerity measures, privatization, and deregulation as a condition for receiving loans or debt relief. Their geopolitical insignificance means conditionalities are strictly enforced, leading to significant social and economic costs.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, non_strategic_debtor_states, payer,
    powerless, generational, trapped, national).

% Often receive waivers or less stringent enforcement of conditionalities due to their strategic importance to hegemon-aligned states. They still bear some costs but can negotiate more favorable terms or delay implementation.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitically_strategic_debtor_states, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitically_strategic_debtor_states, payer).

% Bear the brunt of austerity measures, including cuts to social services, job losses from privatization, and increased cost of living. They have no voice in the negotiation of conditionalities and limited means of resistance.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, vulnerable_populations_in_debtor_states, payer,
    powerless, immediate, trapped, local).

% Monitor the impact of conditionalities on debtor states and advocate for more equitable and sustainable development policies. They analyze the selective application of rules and document its consequences.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, development_advocacy_groups, observer,
    moderate, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate fiscal policy and economic reforms across diverse debtor states to ensure debt sustainability and integrate them into the global market economy, providing a framework for international lending.
% TRANSFER_FUNCTION: Transfers economic policy autonomy from debtor states to international financial institutions and creditor states, in exchange for financial assistance. It also transfers wealth from debtor state populations (via austerity) to creditor institutions (via debt repayment).
% ABSENT_VOICES: The populations of non-strategic debtor states, particularly the most vulnerable, are entirely absent from the negotiation and enforcement of conditionalities. Their interests are represented by governments often under duress, or not at all.
% DISAPPEARANCE_RATIONALE: If conditionalities vanished overnight, the international financial system would face immediate instability as debtor states might default or pursue alternative economic policies. Creditor institutions would lose a key enforcement mechanism, and the geopolitical leverage of hegemon-aligned states would diminish, leading to a significant rearrangement of global power dynamics and financial flows.
% FOUNDING_PROBLEM: The problem of sovereign debt crises and the need for a mechanism to ensure fiscal discipline and economic stability in developing countries, particularly after the oil shocks and debt crises of the 1970s and 80s.
% FOUNDING_PROBLEM_CORROBORATION: International financial institutions and creditor states attest that the problem of sovereign debt and the need for fiscal discipline remain live. Development advocacy groups and non-strategic debtor states corroborate the existence of debt crises but contest the efficacy and equity of conditionalities as a solution, arguing they often exacerbate problems for the poor.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__hybrid_selectivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__hybrid_selectivity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the conditionalities impose significant costs on non-strategic debtor states, often leading to social unrest and economic hardship, while benefiting creditors and strategic actors. Suppression is very high (0.85) due to the lack of viable alternatives for debtor states facing financial crises; they are effectively trapped. The theater ratio (0.45) reflects that while some reforms are genuinely aimed at fiscal stability, a substantial portion of the enforcement is performative, designed to maintain the appearance of universal application while allowing for strategic exceptions. The metrics show a clear trend of increasing extractiveness and suppression over time, indicating a hardening of the constraint's extractive function.
 *
 * PERSPECTIVAL GAP:
 *   The international financial institutions and hegemon-aligned states perceive conditionalities as essential coordination for global financial stability. Non-strategic debtor states and development advocacy groups perceive them as an extractive mechanism that exacerbates inequality and undermines national sovereignty. This reading highlights how the same structural mechanism is experienced as coordination by some and extraction by others, with the difference determined by geopolitical power and strategic importance.
 *
 * DIRECTIONALITY LOGIC:
 *   International financial institutions and hegemon-aligned states are clear beneficiaries (low directionality), as they gain stability and geopolitical leverage. Core creditor institutions also benefit from increased repayment certainty. Non-strategic debtor states and their vulnerable populations are the primary targets (high directionality), bearing the direct costs of austerity and loss of sovereignty. Geopolitically strategic debtor states occupy a hybrid position, sometimes benefiting from waivers but still subject to the overall framework, leading to a more moderate directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure 'rope' (creditor coordination reading) by highlighting the asymmetric enforcement and extraction. It also prevents mislabeling it as a pure 'snare' (debtor extraction reading) by acknowledging the genuine, albeit often secondary, coordination function for the global financial system. The 'tangled_rope' classification captures the hybrid nature, where coordination serves as a cover for selective extraction, and the persistence is due to the active enforcement that benefits powerful actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    geopolitical_influence_quantification,
    'How can the ''geopolitical strategic importance'' that determines selective enforcement be objectively quantified and measured?',
    'Development of a robust, multi-factor index of geopolitical strategic importance, correlated with observed patterns of conditionalities waiver and enforcement.',
    'If quantifiable, it would provide stronger empirical evidence for the selective application mechanism, solidifying the ''tangled_rope'' classification and informing policy interventions to reduce bias. If not, the ''selectivity'' claim remains more conceptual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_influence_quantification, empirical, 'Quantifying the impact of geopolitical strategic importance on conditionalities enforcement.').

omega_variable(
    coordination_vs_extraction_proportion,
    'What is the precise proportion of the constraint''s function that genuinely serves global financial coordination versus that which serves asymmetric extraction?',
    'Counterfactual analysis: model the global financial system''s stability and debt repayment rates in a scenario where conditionalities are applied universally and transparently, versus the current selective application.',
    'A higher proportion of extraction would push the classification closer to a ''snare'', while a higher proportion of coordination would lean it towards a ''rope'' (though still tangled due to asymmetry).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_proportion, conceptual, 'Disentangling the coordination and extraction components of conditionalities.').

omega_variable(
    internalized_suppression_in_debtor_states,
    'To what extent has the suppression experienced by non-strategic debtor states become internalized, leading to self-imposed austerity or a belief in the inevitability of conditionalities, even if structural barriers were reduced?',
    'Post-exit policy trajectory: if debtor states continue to pursue austerity or market-liberalizing reforms after external conditionalities are removed, it suggests internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the debtor states carry the suppression with them after formal exit, making genuine policy autonomy harder to achieve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_in_debtor_states, empirical, 'Structural vs. internalized suppression mechanism in debtor states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(stru_tr_t8, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(stru_tr_t16, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(stru_tr_t24, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 24, 0.43).
narrative_ontology:measurement(stru_tr_t32, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 32, 0.44).
narrative_ontology:measurement(stru_tr_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(stru_be_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(stru_be_t8, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 8, 0.65).
narrative_ontology:measurement(stru_be_t16, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 16, 0.7).
narrative_ontology:measurement(stru_be_t24, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 24, 0.75).
narrative_ontology:measurement(stru_be_t32, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 32, 0.77).
narrative_ontology:measurement(stru_be_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(stru_su_t8, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 8, 0.75).
narrative_ontology:measurement(stru_su_t16, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 16, 0.8).
narrative_ontology:measurement(stru_su_t24, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 24, 0.83).
narrative_ontology:measurement(stru_su_t32, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 32, 0.84).
narrative_ontology:measurement(stru_su_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 40, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__hybrid_selectivity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, creditor_coordination_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, debtor_extraction_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, global_financial_stability_norms).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, sovereign_debt_markets).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'structural_adjustment_conditionalities' kernel. This 'hybrid_selectivity_reading' focuses on the differential application of conditionalities based on geopolitical power, influencing both the 'creditor_coordination_reading' (by revealing its limits) and the 'debtor_extraction_reading' (by providing a mechanism for that extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
