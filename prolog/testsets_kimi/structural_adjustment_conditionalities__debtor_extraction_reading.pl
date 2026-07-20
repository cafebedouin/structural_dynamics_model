% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__debtor_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__debtor_extraction_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: structural_adjustment_conditionalities__debtor_extraction_reading
 *   human_readable: Structural Adjustment Conditionalities as Debtor Extraction Regime
 *   domain: international_political_economy/development_finance
 *
 * SUMMARY:
 *   This constraint story instantiates the debtor_extraction_reading of the
 *   structural_adjustment_conditionalities kernel. It models the loan
 *   conditionalities imposed by international financial institutions and
 *   creditor coalitions on sovereign debtor states as a coercive extraction
 *   regime. Under this reading, conditionalities are not neutral coordination
 *   mechanisms for fiscal sustainability but rather instruments that
 *   dismantle public services, privatize state assets, and suppress
 *   democratic policy autonomy to ensure debt service and market access for
 *   transnational capital. The structural asymmetry is stark: IFIs and
 *   private creditors hold global-scale exit options and generational time
 *   horizons, while domestic populations and debtor governments are trapped
 *   or constrained within national boundaries with biographical stakes.
 *
 * KEY AGENTS:
 *   - International financial institutions: Primary agenda-setter (institutional/arbitrage/global) â design and enforce conditionalities
 *   - Private creditor banks: Primary beneficiary (powerful/arbitrage/global) â capture debt service and asset flows
 *   - Foreign strategic investors: Secondary beneficiary (powerful/mobile/global) â gain from privatization and liberalization
 *   - Domestic populations: Primary target (powerless/trapped/national) â bear austerity and service cuts
 *   - Debtor state governments: Structural target despite formal sovereignty (moderate/constrained/national) â implement extraction under liquidity coercion
 *   - Local industrial entrepreneurs and public sector workers: Secondary targets (moderate or powerless/constrained or trapped/national) â face liberalization and layoffs
 *   - Debtor civil society: Excluded voice (organized/constrained/national) â protests without negotiation access
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__debtor_extraction_reading, 0.88).
domain_priors:suppression_score(structural_adjustment_conditionalities__debtor_extraction_reading, 0.85).
domain_priors:theater_ratio(structural_adjustment_conditionalities__debtor_extraction_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__debtor_extraction_reading, snare).
narrative_ontology:human_readable(structural_adjustment_conditionalities__debtor_extraction_reading, "Structural Adjustment Conditionalities as Debtor Extraction Regime").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__debtor_extraction_reading, "international_political_economy/development_finance").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__debtor_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__debtor_extraction_reading, '1c8e28d8-e763-46d9-8703-84f5d58fb523').
narrative_ontology:cs_kernel_codification('1c8e28d8-e763-46d9-8703-84f5d58fb523', formalized).
narrative_ontology:cs_authority_grounding('1c8e28d8-e763-46d9-8703-84f5d58fb523', extraction).
narrative_ontology:cs_interpretation_layer_present('1c8e28d8-e763-46d9-8703-84f5d58fb523').
narrative_ontology:cs_reading_relation('1c8e28d8-e763-46d9-8703-84f5d58fb523', structural_adjustment_conditionalities__creditor_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c8e28d8-e763-46d9-8703-84f5d58fb523', structural_adjustment_conditionalities__hybrid_selectivity_reading, influences).
narrative_ontology:cs_axiom('1c8e28d8-e763-46d9-8703-84f5d58fb523', foundational, creditor_supremacy_over_popular_sovereignty).
narrative_ontology:cs_axiom_status(creditor_supremacy_over_popular_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('1c8e28d8-e763-46d9-8703-84f5d58fb523', creditor_supremacy_over_popular_sovereignty, conventional).
narrative_ontology:cs_axiom('1c8e28d8-e763-46d9-8703-84f5d58fb523', secondary, conditionalities_as_net_extraction).
narrative_ontology:cs_axiom_status(conditionalities_as_net_extraction, holdable).
narrative_ontology:cs_axiom_grounding('1c8e28d8-e763-46d9-8703-84f5d58fb523', conditionalities_as_net_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('1c8e28d8-e763-46d9-8703-84f5d58fb523', neo_colonial_extraction_framework).
narrative_ontology:cs_drift_state('1c8e28d8-e763-46d9-8703-84f5d58fb523', contemporary_multilateral_reform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1c8e28d8-e763-46d9-8703-84f5d58fb523', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, international_financial_institutions).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, private_creditor_banks).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, foreign_strategic_investors).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_populations).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_state_governments).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, local_industrial_entrepreneurs).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, public_sector_workers).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__debtor_extraction_reading, fiscal_discipline_doctrine).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__debtor_extraction_reading, market_liberalization_efficiency).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_rights_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce loan conditionalities, certify compliance before tranche disbursement, and administer debt sustainability frameworks. Their institutional authority and budgets scale with crisis management and the expansion of conditional lending.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, international_financial_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Receive prioritized debt service through austerity mandates, benefit from sovereign guarantee enforcement, and gain from economic restructuring that liberalizes capital flows and privatizes state assets.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, private_creditor_banks, beneficiary,
    powerful, biographical, arbitrage, global).

% Gain access to privatized infrastructure, depressed labor costs, and liberalized markets following conditionalities that dismantle capital controls and domestic protections.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, foreign_strategic_investors, beneficiary,
    powerful, biographical, mobile, global).

% Lose public services, subsidies, and labor protections as conditionalities mandate austerity, user fees, and public sector contraction. They have no seat at the negotiation table and cannot exit the national jurisdiction en masse.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_populations, payer,
    powerless, biographical, trapped, national).

% Formally sign loan agreements but under structural coercion of liquidity crisis and currency collapse; lose fiscal and monetary policy autonomy; must implement cuts or face credit exclusion and punitive spreads.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_state_governments, payer,
    moderate, biographical, constrained, national).

% Lose tariff protection and state support while facing competition from imports and foreign investors advantaged by liberalization mandates; bankruptcy and informalization are common outcomes.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, local_industrial_entrepreneurs, payer,
    moderate, biographical, constrained, national).

% Face wage freezes, layoffs, and elimination of the services they provide as conditionalities mandate public sector contraction and privatization.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, public_sector_workers, payer,
    powerless, biographical, trapped, national).

% Organize protests and democratic mandates against austerity and privatization, but are structurally excluded from the rooms where conditionalities are designed; their policy preferences are overridden by tranche conditionality.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_civil_society, excluded,
    organized, biographical, constrained, national).

narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__debtor_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None at the debtor level; the arrangement coordinates creditor collective action to enforce debt service priority and market liberalization, while presenting a cover story of fiscal stabilization and development coordination.
% TRANSFER_FUNCTION: Moves public assets, policy autonomy, fiscal surplus, and social service capacity from debtor states and domestic populations to transnational creditor banks, international financial institutions, and foreign strategic investors through austerity, privatization, and liberalization mandates.
% ABSENT_VOICES: Domestic populations facing service cuts, heterodox economists advocating debt restructuring or capital controls, and democratically mandated civil society organizations are excluded from the negotiation rooms where conditionalities are set; their representatives are presented with take-it-or-leave-it packages under liquidity threat.
% DISAPPEARANCE_RATIONALE: If conditionalities vanished overnight, debtor governments would regain fiscal and monetary policy autonomy; public services would not be mechanically cut to ensure debt service; capital controls could be reinstated; privatization would halt; and the current architecture of development finance would collapse, forcing genuine sovereign debt restructuring.
% FOUNDING_PROBLEM: The original problem was post-colonial development financing gaps and recurrent sovereign liquidity crises in the 1970sâ1980s, where debtor states faced balance-of-payments shortages and creditor coordination problems.
% FOUNDING_PROBLEM_CORROBORATION: Heterodox economists such as Stiglitz and Chang, UNCTAD reports, and debtor civil society organizations attest from outside the benefiting parties that the founding liquidity crisis has been superseded by persistent structural extraction; historical evidence of repeated debt cycles and net negative resource transfers supports the obsolescence reading.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__debtor_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__debtor_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__debtor_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__debtor_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_adjustment_conditionalities__debtor_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(structural_adjustment_conditionalities__debtor_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is very high (0.88) because conditionalities systematically transfer public assets, fiscal surplus, and policy autonomy to transnational creditors and investors. Suppression is high (0.85) because alternatives such as sovereign default, heterodox policy, capital controls, and democratic mandate are actively suppressed through disbursement tranches, credit exclusion, and institutional discipline. Theater ratio is substantial (0.65): a thick technocratic discourse of fiscal discipline, efficiency, and market confidence obscures the extraction function. Accessibility collapse is high (0.75) because once a debt crisis begins, the option space collapses to conditionalities or catastrophic default. Resistance is moderate-high (0.70) due to persistent popular protests, debt cancellation movements, and occasional government defiance, though rarely successful in altering the core structure.
 *
 * PERSPECTIVAL GAP:
 *   From the IFI seat, conditionalities are technical, apolitical necessities to restore market confidence and repayment capacity. From the domestic population and debtor government seats, the same rules are experienced as the violent dismantling of social contracts and democratic sovereignty. The engine computes this divergence: the IFI seat registers low directionality (beneficiary/agenda-setter with arbitrage exit) while the domestic population seat registers high directionality (target, trapped, paying through austerity). The perspectival gap is among the widest in the development finance corpus.
 *
 * DIRECTIONALITY LOGIC:
 *   International financial institutions and private creditor banks are structural beneficiaries (low d): they set terms, collect debt service, and retain global arbitrage exit. Foreign strategic investors are beneficiaries (low d). Domestic populations, public sector workers, and local industrial entrepreneurs are structural targets (high d): they bear the costs of austerity and liberalization with trapped or constrained exit. Debtor governments sit as targets (high d) despite their formal state power because their exit is constrained by liquidity dependence and sovereign default costs; they administer the extraction but do not set its terms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â acute liquidity crises and development financing gaps in the post-colonial and post-1982 debt crisis periods â is functionally dead for the extraction reading. Conditionalities have persisted through multiple debt cycles, producing net negative resource transfers and repeated crises rather than resolving the original financing gap. The mandate has outlived its function and now operates as steady-state extraction. This prevents misclassification as scaffold (no sunset clause, no transition) or rope (no mutual benefit): the arrangement persists because it extracts, not because it coordinates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the kernel''s core function creditor-debtor coordination for mutual fiscal sustainability, or coercive extraction from debtor populations for transnational capital?',
    'Comparative analysis of net resource transfers, growth outcomes, and debt trajectories under conditionalities versus counterfactual restructuring or default; corroboration from non-beneficiary analytical seats.',
    'If conditionalities produce net extraction with negative or stagnating growth, the debtor_extraction_reading is structurally vindicated; if they produce positive stabilization and mutual benefit, the creditor_coordination_reading gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Empirical ambiguity between coordination and extraction readings of the kernel').

omega_variable(
    sovereignty_consent_ambiguity,
    'Is debtor government agreement to conditionalities a meaningful consent, or a coerced choice under existential liquidity threat with no viable alternative?',
    'Analysis of outside options at the moment of signature (cost of default, availability of alternative financing, negotiation records) and the democratic mandate of the signing government.',
    'If coerced, debtor governments shift toward the target end of directionality; if genuinely consensual, they shift toward symmetric or even beneficiary status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_consent_ambiguity, conceptual, 'Structural vs consensual nature of debtor compliance').

omega_variable(
    selectivity_as_extraction_modality,
    'Does the selectivity documented by the hybrid_selectivity_reading (harsh enforcement on weak states, waivers for strategic debtors) function as a geopolitical modulation of extraction, or as a separate constraint entirely?',
    'Cross-case comparison of conditionality intensity against geopolitical strategic value, holding debt levels constant.',
    'If selectivity tracks geopolitical interest rather than debt sustainability metrics, extraction is the master function and selectivity is its modulation; if selectivity tracks independent strategic logic, the hybrid reading describes a genuinely distinct mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_as_extraction_modality, conceptual, 'Relationship between extraction and selectivity in the kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__debtor_extraction_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t0, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(stru_tr_t9, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 9, 0.55).
narrative_ontology:measurement(stru_tr_t18, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 18, 0.6).
narrative_ontology:measurement(stru_tr_t27, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 27, 0.62).
narrative_ontology:measurement(stru_tr_t36, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 36, 0.64).
narrative_ontology:measurement(stru_tr_t45, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 45, 0.65).

% Extraction over time
narrative_ontology:measurement(stru_be_t0, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(stru_be_t9, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 9, 0.72).
narrative_ontology:measurement(stru_be_t18, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 18, 0.78).
narrative_ontology:measurement(stru_be_t27, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 27, 0.82).
narrative_ontology:measurement(stru_be_t36, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 36, 0.85).
narrative_ontology:measurement(stru_be_t45, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 45, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t0, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(stru_su_t9, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 9, 0.75).
narrative_ontology:measurement(stru_su_t18, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 18, 0.78).
narrative_ontology:measurement(stru_su_t27, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 27, 0.8).
narrative_ontology:measurement(stru_su_t36, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 36, 0.82).
narrative_ontology:measurement(stru_su_t45, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 45, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
