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
 *   This constraint story presents a 'hybrid selectivity' reading of
 *   structural adjustment conditionalities, where their application and
 *   enforcement are heavily influenced by the geopolitical importance of the
 *   debtor state. Ostensibly a coordination mechanism for fiscal stability,
 *   the constraint functions as a Tangled Rope, extracting heavily from
 *   non-strategic debtor states while offering more flexibility to
 *   geopolitically strategic ones. This reading highlights the divergence
 *   between the stated universal principles of conditionalities and their
 *   actual, selective application.
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
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__hybrid_selectivity_reading, '7f39d4a8-23bc-4b3c-87d8-43027cf3f460').
narrative_ontology:cs_kernel_codification('7f39d4a8-23bc-4b3c-87d8-43027cf3f460', formalized).
narrative_ontology:cs_authority_grounding('7f39d4a8-23bc-4b3c-87d8-43027cf3f460', extraction).
narrative_ontology:cs_interpretation_layer_present('7f39d4a8-23bc-4b3c-87d8-43027cf3f460').
narrative_ontology:cs_reading_relation('7f39d4a8-23bc-4b3c-87d8-43027cf3f460', structural_adjustment_conditionalities__creditor_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('7f39d4a8-23bc-4b3c-87d8-43027cf3f460', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('7f39d4a8-23bc-4b3c-87d8-43027cf3f460', foundational, geopolitical_leverage_justifies_selective_enforcement).
narrative_ontology:cs_axiom_status(geopolitical_leverage_justifies_selective_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('7f39d4a8-23bc-4b3c-87d8-43027cf3f460', geopolitical_leverage_justifies_selective_enforcement, conventional).
narrative_ontology:cs_axiom('7f39d4a8-23bc-4b3c-87d8-43027cf3f460', secondary, market_discipline_is_universally_beneficial_but_selectively_applied).
narrative_ontology:cs_axiom_status(market_discipline_is_universally_beneficial_but_selectively_applied, holdable).
narrative_ontology:cs_axiom_grounding('7f39d4a8-23bc-4b3c-87d8-43027cf3f460', market_discipline_is_universally_beneficial_but_selectively_applied, instrumental).
narrative_ontology:cs_reference_frame('7f39d4a8-23bc-4b3c-87d8-43027cf3f460', universal_market_discipline_framework).
narrative_ontology:cs_drift_state('7f39d4a8-23bc-4b3c-87d8-43027cf3f460', post_cold_war_geopolitical_realignment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7f39d4a8-23bc-4b3c-87d8-43027cf3f460', '').
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

% Often receive waivers or less stringent enforcement of conditionalities due to their strategic importance to hegemon-aligned states. While still subject to some discipline, their leverage allows for negotiation and mitigation of the harshest impacts.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitically_strategic_debtor_states, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitically_strategic_debtor_states, payer).

% Bear the brunt of austerity measures, including cuts to social services, job losses from privatization, and increased cost of living. They have no voice in the negotiation of conditionalities and limited means of resistance.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, vulnerable_populations_in_debtor_states, payer,
    powerless, immediate, trapped, local).

% Monitor the impact of conditionalities, advocate for debt relief, and highlight the social costs of structural adjustment. They provide critical analysis but lack direct power to alter the constraint.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, development_advocacy_groups, observer,
    organized, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate fiscal policy and economic reforms across diverse debtor states to ensure global financial stability and facilitate debt repayment, providing a predictable framework for international lending.
% TRANSFER_FUNCTION: Transfers economic policy autonomy from debtor states to international financial institutions and creditor states, in exchange for access to finance. It also transfers wealth from debtor state populations (via austerity) to creditors (via repayment).
% ABSENT_VOICES: The populations of non-strategic debtor states, particularly the poor and marginalized, are entirely absent from the negotiation and enforcement of conditionalities. Their interests are not represented, and their suffering is externalized. Alternative development models that prioritize social welfare over market liberalization are also excluded from the policy discourse.
% DISAPPEARANCE_RATIONALE: If conditionalities vanished overnight, non-strategic debtor states would gain significant policy space, potentially leading to different development paths. International financial institutions would lose a key tool of influence, and the global financial system would need to find new mechanisms for managing sovereign debt and ensuring stability, likely leading to a more fragmented and less predictable lending environment.
% FOUNDING_PROBLEM: The founding problem was to address sovereign debt crises and ensure the stability of the international financial system by imposing fiscal discipline and market-oriented reforms on borrowing nations.
% FOUNDING_PROBLEM_CORROBORATION: International financial institutions and hegemon-aligned states attest the problem is still live, citing ongoing risks to global financial stability. Development advocacy groups and non-strategic debtor states attest that the founding problem has been superseded by a selective enforcement regime that prioritizes geopolitical interests over genuine development, with corroboration from independent economic analyses and historical case studies.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__hybrid_selectivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__hybrid_selectivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.78) because non-strategic debtor states are forced to adopt policies that often undermine their social contracts and transfer wealth to creditors, with limited benefits for their populations. Suppression is very high (0.85) due to the lack of viable alternatives for debtor states facing financial crises and the coercive power of international financial institutions. Theater ratio is moderate (0.45) as the universalist rhetoric of 'fiscal discipline for all' masks the selective enforcement driven by geopolitical considerations. The metrics reflect the experience of non-strategic debtor states, which are the primary victims in this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of international financial institutions and hegemon-aligned states, conditionalities are a necessary, if sometimes harsh, coordination mechanism for global financial stability. From the perspective of non-strategic debtor states and their vulnerable populations, the same conditionalities are a highly extractive and coercive instrument of neo-colonial control, selectively applied to their detriment. Geopolitically strategic debtor states experience a hybrid, less extractive version due to their leverage.
 *
 * DIRECTIONALITY LOGIC:
 *   International financial institutions and hegemon-aligned states are clear beneficiaries (low d) due to the stability and influence gained. Core creditor institutions also benefit (low d) from increased repayment certainty. Non-strategic debtor states and their vulnerable populations are clear targets (high d) due to the severe costs and lack of exit. Geopolitically strategic debtor states have a more complex position, bearing some costs but also benefiting from waivers and leverage (mid-range d).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (creditor coordination reading) or a pure Snare (debtor extraction reading). By identifying it as a Tangled Rope with hybrid selectivity, it acknowledges both the coordination function (global financial stability) and the asymmetric extraction (harsh enforcement on non-strategic debtors, waivers for strategic ones). The mandatrophy analysis suggests that while the original mandate of financial stability persists, its application has drifted to serve geopolitical interests, leading to a performative aspect in its universalist claims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    geopolitical_influence_quantification,
    'How precisely can the ''geopolitical strategic'' factor be quantified in its impact on conditionality enforcement and waivers?',
    'Econometric analysis correlating geopolitical alliance metrics with conditionality stringency and waiver frequency, controlling for economic fundamentals.',
    'A strong, quantifiable correlation would solidify the ''hybrid selectivity'' reading, potentially leading to policy reforms that de-politicize conditionality application. Weak correlation would push towards either the ''creditor coordination'' or ''debtor extraction'' readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_influence_quantification, empirical, 'Quantifying the role of geopolitical strategy in conditionality enforcement.').

omega_variable(
    alternative_development_models_viability,
    'Are there viable alternative development models that could address sovereign debt crises without relying on market-liberalizing conditionalities, and what would their implementation costs be?',
    'Comparative case studies of states that successfully pursued heterodox development paths, or theoretical modeling of alternative international financial architectures.',
    'Demonstrating viable alternatives would weaken the ''necessity'' claim of conditionalities, shifting the constraint towards a Snare or a more clearly extractive Tangled Rope. Lack of alternatives would reinforce the perceived coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_development_models_viability, empirical, 'Viability of non-conditional development models.').

omega_variable(
    kernel_reading_divergence,
    'Given the ''structural_adjustment_conditionalities'' kernel, what specific structural elements do the ''creditor_coordination_reading'', ''debtor_extraction_reading'', and ''hybrid_selectivity_reading'' disagree on?',
    'Comparative textual analysis of policy documents, institutional statements, and critical analyses, mapping specific claims to the constraint''s beneficiary/victim structure, enforcement mechanisms, and stated goals.',
    'Clarifying the points of divergence helps to precisely define the boundaries of each reading as a distinct constraint, enabling more accurate classification and analysis of their respective impacts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Mapping the structural disagreements between sibling readings of structural adjustment conditionalities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__hybrid_selectivity_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t1980, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(stru_tr_t1990, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(stru_tr_t2000, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 2000, 0.5).
narrative_ontology:measurement(stru_tr_t2010, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 2010, 0.48).
narrative_ontology:measurement(stru_tr_t2020, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 2020, 0.45).

% Extraction over time
narrative_ontology:measurement(stru_be_t1980, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(stru_be_t1990, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 1990, 0.75).
narrative_ontology:measurement(stru_be_t2000, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 2000, 0.8).
narrative_ontology:measurement(stru_be_t2010, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 2010, 0.78).
narrative_ontology:measurement(stru_be_t2020, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 2020, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t1980, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(stru_su_t1990, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(stru_su_t2000, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(stru_su_t2010, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 2010, 0.83).
narrative_ontology:measurement(stru_su_t2020, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 2020, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__hybrid_selectivity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, sovereign_debt_restructuring_framework).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, global_financial_stability_regime).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
