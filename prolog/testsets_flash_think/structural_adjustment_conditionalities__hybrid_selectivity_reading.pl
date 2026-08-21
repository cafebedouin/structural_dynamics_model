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
 *   human_readable: Selectively Applied Structural Adjustment Conditionalities
 *   domain: international_political_economy
 *
 * SUMMARY:
 *   This constraint describes structural adjustment conditionalities as a
 *   selectively applied discipline, enforced harshly on weak states while
 *   often waived or softened for geopolitically strategic debtors. It is a
 *   reading of the 'structural_adjustment_conditionalities' kernel, focusing
 *   on the differential application of rules based on power and geopolitical
 *   leverage. The constraint is claimed as a Tangled Rope, reflecting its
 *   dual function of ostensible coordination (fiscal stability) and
 *   asymmetric extraction (from weak states).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.85).
domain_priors:suppression_score(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.9).
domain_priors:theater_ratio(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__hybrid_selectivity_reading, tangled_rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__hybrid_selectivity_reading, "Selectively Applied Structural Adjustment Conditionalities").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__hybrid_selectivity_reading, "international_political_economy").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__hybrid_selectivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__hybrid_selectivity_reading, '37c7ca72-4ed9-49cd-ba85-78a3508362ab').
narrative_ontology:cs_kernel_codification('37c7ca72-4ed9-49cd-ba85-78a3508362ab', formalized).
narrative_ontology:cs_authority_grounding('37c7ca72-4ed9-49cd-ba85-78a3508362ab', extraction).
narrative_ontology:cs_interpretation_layer_present('37c7ca72-4ed9-49cd-ba85-78a3508362ab').
narrative_ontology:cs_reading_relation('37c7ca72-4ed9-49cd-ba85-78a3508362ab', structural_adjustment_conditionalities__creditor_coordination_reading, influences).
narrative_ontology:cs_reading_relation('37c7ca72-4ed9-49cd-ba85-78a3508362ab', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('37c7ca72-4ed9-49cd-ba85-78a3508362ab', foundational, geopolitical_leverage_modulates_enforcement).
narrative_ontology:cs_axiom_status(geopolitical_leverage_modulates_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('37c7ca72-4ed9-49cd-ba85-78a3508362ab', geopolitical_leverage_modulates_enforcement, empirically_contingent).
narrative_ontology:cs_axiom('37c7ca72-4ed9-49cd-ba85-78a3508362ab', foundational, universal_application_is_a_fiction).
narrative_ontology:cs_axiom_status(universal_application_is_a_fiction, holdable).
narrative_ontology:cs_axiom_grounding('37c7ca72-4ed9-49cd-ba85-78a3508362ab', universal_application_is_a_fiction, empirically_contingent).
narrative_ontology:cs_reference_frame('37c7ca72-4ed9-49cd-ba85-78a3508362ab', post_bretton_woods_stabilization).
narrative_ontology:cs_drift_state('37c7ca72-4ed9-49cd-ba85-78a3508362ab', post_cold_war_globalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('37c7ca72-4ed9-49cd-ba85-78a3508362ab', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_states).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_institutions).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, weak_debtor_states).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, citizens_of_weak_debtor_states).
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

% Administers and enforces structural adjustment programs, setting conditionalities for loans. While ostensibly promoting global financial stability, their actions are perceived as selectively applied, favoring geopolitically strategic debtors and core creditors.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, international_financial_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the stability and market access conditionalities ostensibly provide, and from the geopolitical leverage gained when conditionalities are waived for their strategic allies. They influence the agenda of international financial institutions.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_states, beneficiary,
    institutional, generational, arbitrage, global).

% Benefit from the enforcement of conditionalities that prioritize debt repayment and market liberalization, ensuring a favorable environment for their investments and lending. They exert influence through lobbying and financial markets.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_institutions, beneficiary,
    organized, biographical, mobile, global).

% Are subjected to harsh conditionalities, leading to austerity measures, privatization, and loss of policy autonomy. Their geopolitical weakness means they have few alternatives to accepting these terms, bearing significant economic and social costs.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, weak_debtor_states, payer,
    powerless, immediate, trapped, national).

% Often receive more lenient terms or waivers on conditionalities due to their geopolitical importance, allowing them greater policy flexibility. They still engage with IFIs but can negotiate from a position of strength.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitically_strategic_debtor_states, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitically_strategic_debtor_states, payer).

% Bear the direct social costs of austerity measures, cuts to public services, and economic restructuring imposed by conditionalities. Their identity is often tied to their national context, making exit from the affected system impossible.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, citizens_of_weak_debtor_states, payer,
    powerless, immediate, identity_locked, local).

% Represent economic and social development strategies that diverge from the market-liberalizing agenda promoted by conditionalities. They are often marginalized or actively suppressed in policy discourse, preventing their adoption by debtor states.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, alternative_development_models, excluded,
    moderate, generational, constrained, global).

% Analyze the impact and effectiveness of conditionalities, often highlighting the disparities in their application and their social consequences. Their research provides critical perspectives on the constraint's operation.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, development_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ostensibly to ensure fiscal discipline, macroeconomic stability, and market access for debtor states, and to coordinate creditor expectations regarding repayment and economic reforms.
% TRANSFER_FUNCTION: Transfers economic policy autonomy and resources from weak debtor states to international financial institutions and core creditors, while allowing geopolitically strategic debtors to retain greater policy space.
% ABSENT_VOICES: Citizens of weak debtor states, local civil society organizations, and proponents of alternative development paradigms are often excluded from the policy-making process, their priorities overridden by the conditionalities.
% DISAPPEARANCE_RATIONALE: If conditionalities vanished overnight, weak debtor states would regain significant policy space, potentially leading to diverse development paths and a re-evaluation of debt obligations. International financial institutions and core creditors would lose a key tool of influence and a mechanism for enforcing market-oriented reforms, fundamentally altering global financial governance.
% FOUNDING_PROBLEM: Post-WWII efforts to stabilize the global financial system, prevent sovereign defaults, and promote economic development, particularly in developing nations facing balance-of-payments crises and needing external financing.
% FOUNDING_PROBLEM_CORROBORATION: International financial institutions and core creditors claim the problem of fiscal instability and need for reform is still live. However, development economists and civil society groups attest that while some aspects of the original problem persist, the application of conditionalities has substantially drifted from its founding mandate, becoming a tool for selective discipline and rent extraction, supported by numerous case studies and independent analyses.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__hybrid_selectivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__hybrid_selectivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the significant economic and social costs imposed on weak debtor states, often disproportionate to the stated goals of fiscal adjustment. Suppression (0.90) is severe due to the lack of viable alternatives for these states, which are often trapped by debt and geopolitical vulnerability. The moderate theater ratio (0.45) indicates that while some coordination function (e.g., preventing outright default) exists, a substantial portion of the enforcement activity serves to maintain the selective discipline and extract rents, rather than universally promoting fiscal health. The increasing trend in extractiveness and suppression over the interval reflects the hardening of enforcement for non-strategic debtors and the growing recognition of the selective application.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of international financial institutions and hegemon-aligned states, conditionalities are a necessary coordination mechanism for global financial stability. However, from the perspective of weak debtor states and their citizens, the same conditionalities operate as a highly extractive and suppressive mechanism, selectively applied to their detriment. Geopolitically strategic debtors experience a hybrid situation, benefiting from waivers while still being subject to the overall framework.
 *
 * DIRECTIONALITY LOGIC:
 *   International financial institutions and core creditors are beneficiaries, as they maintain influence and secure debt repayment. Hegemon-aligned states also benefit from the geopolitical leverage. Weak debtor states and their citizens are clear targets, bearing the brunt of extraction and suppression. Geopolitically strategic debtors have a more complex position, sometimes benefiting from leniency but still operating within the constraint's framework. Alternative development models are excluded, their suppression being a key aspect of the constraint's persistence.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope, rather than a pure Rope, prevents mislabeling the constraint as purely coordinative. The high extractiveness and suppression, coupled with the selective application, indicate that the original mandate of universal fiscal stability has atrophied into a mechanism for differential discipline and rent extraction. The 'contested' status of the founding problem further highlights this drift, suggesting that the constraint's persistence is less about its original function and more about the benefits it provides to specific powerful actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_structural_adjustment_conditionalities,
    'Is this constraint a genuine coordination mechanism for global financial stability, or primarily an instrument of selective discipline and extraction?',
    'Comparative analysis of conditionalities applied to geopolitically strategic vs. non-strategic debtors over time, assessing outcomes for fiscal health, social welfare, and debt sustainability in both groups.',
    'If the outcomes for weak states are consistently worse and the waivers for strategic states are frequent, it strengthens the ''hybrid selectivity'' and ''debtor extraction'' readings over the ''creditor coordination'' reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_structural_adjustment_conditionalities, empirical, 'Ambiguity regarding the primary function of structural adjustment conditionalities.').

omega_variable(
    geopolitical_influence_quantification,
    'To what extent can the selective application of conditionalities be quantitatively attributed to geopolitical influence versus genuine economic factors?',
    'Econometric modeling controlling for economic fundamentals, institutional quality, and debt levels, to isolate the effect of geopolitical alignment on the severity and enforcement of conditionalities.',
    'Strong evidence of geopolitical influence would further validate the ''hybrid selectivity'' reading and challenge the narrative of purely technocratic application.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_influence_quantification, empirical, 'Quantifying the role of geopolitical factors in conditionalities'' application.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression on weak debtor states primarily structural (external barriers) or internalized (belief in inevitability/lack of agency)?',
    'Post-exit policy trajectory: if states that manage to exit conditionalities continue to self-impose similar policies, it suggests internalized suppression. If they rapidly pursue alternative policies, it suggests structural suppression was dominant.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them after exit, making reform harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for weak debtor states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__hybrid_selectivity_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t1980, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(stru_tr_t1988, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 1988, 0.35).
narrative_ontology:measurement(stru_tr_t1996, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 1996, 0.4).
narrative_ontology:measurement(stru_tr_t2004, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 2004, 0.42).
narrative_ontology:measurement(stru_tr_t2012, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 2012, 0.44).
narrative_ontology:measurement(stru_tr_t2020, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 2020, 0.45).

% Extraction over time
narrative_ontology:measurement(stru_be_t1980, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(stru_be_t1988, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 1988, 0.72).
narrative_ontology:measurement(stru_be_t1996, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 1996, 0.78).
narrative_ontology:measurement(stru_be_t2004, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 2004, 0.82).
narrative_ontology:measurement(stru_be_t2012, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 2012, 0.84).
narrative_ontology:measurement(stru_be_t2020, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 2020, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t1980, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(stru_su_t1988, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 1988, 0.78).
narrative_ontology:measurement(stru_su_t1996, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 1996, 0.83).
narrative_ontology:measurement(stru_su_t2004, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 2004, 0.87).
narrative_ontology:measurement(stru_su_t2012, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 2012, 0.89).
narrative_ontology:measurement(stru_su_t2020, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 2020, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__hybrid_selectivity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, global_debt_architecture).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, sovereign_debt_markets).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, development_aid_conditionalities).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'structural_adjustment_conditionalities' kernel, focusing on the selective application of discipline based on geopolitical leverage. It is linked to other readings of the same kernel and related constraints in the international political economy domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
