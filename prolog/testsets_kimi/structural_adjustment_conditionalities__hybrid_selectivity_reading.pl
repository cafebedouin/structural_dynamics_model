% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__hybrid_selectivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Structural Adjustment Conditionalities â Hybrid Selectivity Reading
 *   domain: international_political_economy/development_finance
 *
 * SUMMARY:
 *   This constraint story instantiates the hybrid_selectivity_reading of the
 *   structural_adjustment_conditionalities kernel. The kernel is the set of
 *   policy conditionalities attached to multilateral crisis lending. This
 *   reading treats the constraint as a tangled rope: it carries a genuine
 *   coordination function in sovereign debt crisis management, but enforces
 *   asymmetric extraction whereby weak, non-strategic debtors surrender
 *   fiscal and monetary autonomy while geopolitically strategic debtors and
 *   hegemon-aligned states are exempted or lightly treated. Core creditors
 *   and allied states capture the policy compliance and debt service streams.
 *   The sibling readings are creditor_coordination_reading (universal
 *   technocratic coordination) and debtor_extraction_reading (uniform
 *   neo-colonial extraction).
 *
 * KEY AGENTS:
 *   - multilateral_administrator (institutional/arbitrage): administers and selectively enforces conditionalities
 *   - core_creditor_bloc (institutional/arbitrage): primary financial beneficiary and agenda setter
 *   - hegemon_aligned_states (powerful/mobile): geopolitical beneficiaries of selective exemption
 *   - weak_non_strategic_debtors (powerless/trapped): primary targets of harsh conditionality
 *   - critical_ipe_scholars (analytical/analytical): observer seat documenting selectivity patterns
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.72).
domain_priors:suppression_score(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.78).
domain_priors:theater_ratio(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__hybrid_selectivity_reading, tangled_rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__hybrid_selectivity_reading, "Structural Adjustment Conditionalities â Hybrid Selectivity Reading").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__hybrid_selectivity_reading, "international_political_economy/development_finance").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__hybrid_selectivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'a6438b6e-ba2f-45db-bf13-bb7b118d04ec').
narrative_ontology:cs_kernel_codification('a6438b6e-ba2f-45db-bf13-bb7b118d04ec', formalized).
narrative_ontology:cs_authority_grounding('a6438b6e-ba2f-45db-bf13-bb7b118d04ec', extraction).
narrative_ontology:cs_interpretation_layer_present('a6438b6e-ba2f-45db-bf13-bb7b118d04ec').
narrative_ontology:cs_reading_relation('a6438b6e-ba2f-45db-bf13-bb7b118d04ec', structural_adjustment_conditionalities__creditor_coordination_reading, influences).
narrative_ontology:cs_reading_relation('a6438b6e-ba2f-45db-bf13-bb7b118d04ec', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('a6438b6e-ba2f-45db-bf13-bb7b118d04ec', foundational, selective_enforcement_serves_systemic_stability).
narrative_ontology:cs_axiom_status(selective_enforcement_serves_systemic_stability, holdable).
narrative_ontology:cs_axiom_grounding('a6438b6e-ba2f-45db-bf13-bb7b118d04ec', selective_enforcement_serves_systemic_stability, instrumental).
narrative_ontology:cs_axiom('a6438b6e-ba2f-45db-bf13-bb7b118d04ec', foundational, debtor_sovereignty_is_geopolitically_contingent).
narrative_ontology:cs_axiom_status(debtor_sovereignty_is_geopolitically_contingent, holdable).
narrative_ontology:cs_axiom_grounding('a6438b6e-ba2f-45db-bf13-bb7b118d04ec', debtor_sovereignty_is_geopolitically_contingent, conventional).
narrative_ontology:cs_reference_frame('a6438b6e-ba2f-45db-bf13-bb7b118d04ec', universal_market_discipline).
narrative_ontology:cs_drift_state('a6438b6e-ba2f-45db-bf13-bb7b118d04ec', post_cold_war_hegemonic_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a6438b6e-ba2f-45db-bf13-bb7b118d04ec', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_bloc).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_states).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, weak_non_strategic_debtors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiates and monitors sovereign loan programs. Applies conditionality templates that vary in hardness based on shareholder strategic priorities. Presents all programs as uniform technocratic responses despite documented variance in enforcement and waiver frequency.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, multilateral_administrator, agenda_setter,
    institutional, generational, arbitrage, global).

% Holds dominant quota shares and voting rights in multilateral financial institutions. Receives sovereign debt service, policy concessions, and market access guarantees from debtor countries. Sets the strategic priorities that determine which borrowers face hard versus soft terms.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_bloc, beneficiary,
    institutional, generational, arbitrage, global).

% Maintain close security and diplomatic ties with the leading creditor state. Receive larger program access, favorable terms, and waivers on standard fiscal targets that are enforced rigorously against non-aligned borrowers.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_states, beneficiary,
    powerful, generational, mobile, national).

% Sovereign states with limited independent market access and no major power patron. Must accept extensive privatization, fiscal austerity, and trade liberalization to access crisis financing. Face program suspension and credit cutoff if they deviate from prescribed targets.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, weak_non_strategic_debtors, payer,
    powerless, biographical, trapped, national).

% Analyze cross-national patterns of conditionality variance. Document that enforcement hardness correlates with geopolitical alignment rather than macroeconomic fundamentals. Do not participate in program design or negotiation.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, critical_ipe_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_bloc).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__hybrid_selectivity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a lender of last resort and policy anchor for sovereigns facing balance-of-payments crises, preventing contagious default and supplying a rules-based framework for international capital flows.
% TRANSFER_FUNCTION: Moves fiscal and monetary policy autonomy, public asset ownership, and priority of external debt service from weak non-strategic debtor states to core creditors and hegemon-aligned states.
% ABSENT_VOICES: Domestic civil society in debtor states, non-Western alternative creditors offering unconditional or counter-cyclical finance, and heterodox development economists are structurally excluded from the conditionality design process.
% DISAPPEARANCE_RATIONALE: If the conditionalities and their selective enforcement vanished, weak debtors would reclaim fiscal and monetary policy space, sovereign risk would reprice away from geopolitical patronage, and the creditor bloc would lose its primary instrument of macroeconomic influence over the periphery.
% FOUNDING_PROBLEM: Chronic balance-of-payments crises and lack of coordinated crisis lending in the mid-twentieth century, creating a need for an institutionalized lender of last resort to prevent sovereign default contagion and protect the open trading system.
% FOUNDING_PROBLEM_CORROBORATION: Creditor institutions and G7 finance ministries attest the problem remains live, citing recurrent liquidity crises. Critical IPE scholars, UNCTAD, and debtor-state civil society attest the original liquidity rationale has been superseded by a geopolitical disciplining function; no intergovernmental body outside the creditor core corroborates the live status without qualification.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__hybrid_selectivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__hybrid_selectivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.72) because the arrangement systematically transfers policy autonomy and fiscal priority from weak debtors to the creditor core. Suppression is higher (0.78) because the constraint persists by actively excluding alternative crisis financing for weak states and disciplining deviation, while enforcement is relaxed for strategic allies. Theater ratio (0.58) reflects that the universalist technocratic rhetoric functions increasingly as performative cover for geopolitically calibrated practice. Accessibility collapse (0.68) is high for weak debtors who lack market alternatives, though not total. Resistance (0.42) is moderate: episodic debtor coalitions and civil society pushback occur but are fragmented and often overridden.
 *
 * PERSPECTIVAL GAP:
 *   From the multilateral administrator's seat, the arrangement is necessary crisis coordination with variable implementation flexibility. From the weak debtor's seat, the same arrangement is asymmetric extraction calibrated by power. From the hegemon-aligned state's seat, the regime is a benign source of liquidity with manageable conditions. The engine computes these divergent seat classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The core creditor bloc and hegemon-aligned states are structural beneficiaries (low d): they collect debt service, policy compliance, and geopolitical alignment without bearing the costs of austerity. Weak non-strategic debtors are the structural targets (high d): they pay through surrendered policy space and compressed domestic spending. The multilateral administrator sits between these poles, deriving institutional authority from administering the arrangement; its directionality is closer to the beneficiary end than the target end because its survival and budget depend on the constraint's continuation. The engine will compute high effective extraction for the weak debtors and low or negative extraction for the creditor bloc.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification prevents mislabeling by requiring both a genuine coordination function (crisis lending prevents contagion) and identifiable asymmetric extraction (weak debtors pay what strategic debtors avoid). If the coordination function were absent, this would be a snare; if the extraction were symmetric or absent, it would be a rope. The hybrid reading insists on the coexistence of both, which the metrics and beneficiary/victim declarations reflect independently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    geopolitical_selectivity_as_design,
    'Is the selective enforcement of conditionalities an intentional feature of the international financial architecture, or an emergent property of differential state capacity and bargaining power?',
    'Comparative case studies of program waivers and deviation permissions, controlling for macroeconomic fundamentals and geopolitical alignment.',
    'If intentional, the constraint operates as a hegemonic instrument with identifiable captors; if emergent, it may reflect coordination failure rather than designed extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_selectivity_as_design, empirical, 'Intentionality of selective enforcement in multilateral lending').

omega_variable(
    kernel_reading_distinctness,
    'Does the hybrid selectivity reading identify a structurally distinct constraint, or is it a composite observation of the creditor coordination and debtor extraction readings?',
    'Evaluate whether the selectivity pattern produces a stakeholder geometry and epsilon value that cannot be reduced to either sibling reading alone.',
    'If not distinct, this story dissolves into its siblings; if distinct, it warrants independent constraint identity and separate classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinctness, conceptual, 'Whether the hybrid reading is irreducibly distinct from its siblings').

omega_variable(
    systemic_stability_vs_hegemonic_rent,
    'Does selective enforcement serve global financial stability, or does it primarily secure rents and strategic compliance for the creditor core?',
    'Macroeconomic modeling of counterfactual universal enforcement versus observed selectivity on global default rates and crisis contagion.',
    'If systemic stability, part of the measured extraction is the price of coordination; if hegemonic rent, the coordination story is largely cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(systemic_stability_vs_hegemonic_rent, preference, 'Normative ambiguity between stability and rent in selective enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(stru_tr_t8, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(stru_tr_t16, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(stru_tr_t24, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement(stru_tr_t32, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 32, 0.54).
narrative_ontology:measurement(stru_tr_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(stru_be_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(stru_be_t8, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(stru_be_t16, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(stru_be_t24, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 24, 0.67).
narrative_ontology:measurement(stru_be_t32, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 32, 0.7).
narrative_ontology:measurement(stru_be_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(stru_su_t8, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement(stru_su_t16, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 16, 0.72).
narrative_ontology:measurement(stru_su_t24, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 24, 0.78).
narrative_ontology:measurement(stru_su_t32, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 32, 0.82).
narrative_ontology:measurement(stru_su_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 40, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__hybrid_selectivity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities__creditor_coordination_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities__debtor_extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the structural_adjustment_conditionalities kernel, decomposed per the epsilon-invariance principle because the creditor coordination, debtor extraction, and hybrid selectivity framings yield structurally distinct epsilon values and stakeholder arrangements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
