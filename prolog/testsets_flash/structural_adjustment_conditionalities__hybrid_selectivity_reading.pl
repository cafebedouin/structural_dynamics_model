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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: structural_adjustment_conditionalities__hybrid_selectivity_reading
 *   human_readable: Structural Adjustment Conditionalities: Hybrid Selectivity Reading
 *   domain: international_political_economy/development_finance/institutional_economics
 *
 * SUMMARY:
 *   This constraint describes structural adjustment conditionalities as a
 *   mechanism of selectively applied discipline in international development
 *   finance. While ostensibly designed to ensure fiscal responsibility and
 *   market-oriented reforms (a coordination function), their enforcement is
 *   harsh on geopolitically weak or non-strategic debtor states, leading to
 *   significant extraction from their populations. Conversely, geopolitically
 *   strategic debtors often receive waivers or less stringent enforcement,
 *   revealing a hybrid function that blends coordination rhetoric with
 *   strategic extraction. This is one reading of the
 *   'structural_adjustment_conditionalities' kernel.
 *
 * KEY AGENTS:
 *   - core_creditor_institutions: Agenda setter (institutional/arbitrage) — sets and enforces conditionalities.
 *   - hegemon_aligned_states: Beneficiary (institutional/arbitrage) — benefits from stability and influence.
 *   - non_strategic_debtor_states: Payer (powerless/trapped) — bears the full brunt of conditionalities.
 *   - geopolitically_strategic_debtor_states: Payer (powerful/constrained) — receives waivers, bears less extraction.
 *   - vulnerable_populations_in_debtor_states: Payer (powerless/trapped) — suffers direct impacts of austerity.
 *   - development_economists: Observer (analytical/analytical) — analyzes the effects and mechanisms.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.7).
domain_priors:suppression_score(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.85).
domain_priors:theater_ratio(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__hybrid_selectivity_reading, tangled_rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__hybrid_selectivity_reading, "Structural Adjustment Conditionalities: Hybrid Selectivity Reading").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__hybrid_selectivity_reading, "international_political_economy/development_finance/institutional_economics").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__hybrid_selectivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__hybrid_selectivity_reading, '52f41d8f-db4f-42ea-938e-d41003cd3130').
narrative_ontology:cs_kernel_codification('52f41d8f-db4f-42ea-938e-d41003cd3130', formalized).
narrative_ontology:cs_authority_grounding('52f41d8f-db4f-42ea-938e-d41003cd3130', extraction).
narrative_ontology:cs_interpretation_layer_present('52f41d8f-db4f-42ea-938e-d41003cd3130').
narrative_ontology:cs_reading_relation('52f41d8f-db4f-42ea-938e-d41003cd3130', structural_adjustment_conditionalities__creditor_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('52f41d8f-db4f-42ea-938e-d41003cd3130', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('52f41d8f-db4f-42ea-938e-d41003cd3130', foundational, conditionalities_are_geopolitically_differentiated).
narrative_ontology:cs_axiom_status(conditionalities_are_geopolitically_differentiated, holdable).
narrative_ontology:cs_axiom_grounding('52f41d8f-db4f-42ea-938e-d41003cd3130', conditionalities_are_geopolitically_differentiated, empirically_contingent).
narrative_ontology:cs_axiom('52f41d8f-db4f-42ea-938e-d41003cd3130', foundational, strategic_value_modulates_enforcement).
narrative_ontology:cs_axiom_status(strategic_value_modulates_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('52f41d8f-db4f-42ea-938e-d41003cd3130', strategic_value_modulates_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('52f41d8f-db4f-42ea-938e-d41003cd3130', universal_fiscal_discipline_framework).
narrative_ontology:cs_drift_state('52f41d8f-db4f-42ea-938e-d41003cd3130', post_cold_war_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('52f41d8f-db4f-42ea-938e-d41003cd3130', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_states).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_institutions).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, non_strategic_debtor_states).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, vulnerable_populations_in_debtor_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitically_strategic_debtor_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% International financial institutions (e.g., IMF, World Bank) and major state creditors that design, impose, and monitor structural adjustment conditionalities. They benefit from debt repayment, market access for their corporations, and maintaining global financial order.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Powerful states that align with the core creditor institutions, benefiting from the stability of the international financial system, access to resources from debtor nations, and geopolitical influence derived from the conditionalities regime. They often influence the selective application of conditionalities.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_states, beneficiary,
    institutional, generational, arbitrage, global).

% Sovereign states with limited geopolitical leverage or strategic resources, heavily reliant on international loans. They are subjected to strict, often punitive, conditionalities leading to deep austerity, privatization, and social unrest, with few alternatives to compliance.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, non_strategic_debtor_states, payer,
    powerless, generational, trapped, national).

% Sovereign states that hold significant geopolitical importance (e.g., resource-rich, strategically located, large markets). They face conditionalities but often receive waivers, softer enforcement, or more favorable terms due to their strategic value to core creditors and hegemon-aligned states, reducing the extractive burden.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitically_strategic_debtor_states, payer,
    powerful, biographical, constrained, national).

% Citizens within debtor states, particularly the poor and marginalized, who bear the direct costs of austerity measures, cuts to public services, and job losses resulting from conditionalities. They have virtually no exit options from the national economic system.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, vulnerable_populations_in_debtor_states, payer,
    powerless, immediate, trapped, local).

% Academics and researchers who study the impact of structural adjustment conditionalities on economic development, poverty, and inequality. They provide critical analysis of the mechanisms and outcomes, often highlighting the selective application and its consequences.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, development_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_institutions).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__hybrid_selectivity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate debtor states' economic policies with international financial norms, ensuring fiscal stability, market liberalization, and debt repayment capacity, thereby maintaining global financial order.
% TRANSFER_FUNCTION: Transfers economic sovereignty, public assets (via privatization), and social welfare provisions from debtor states (especially non-strategic ones) to core creditor institutions and their aligned interests, in exchange for continued access to finance.
% ABSENT_VOICES: The voices of vulnerable populations in debtor states are largely absent from the negotiation tables where conditionalities are set, as are those of alternative development models that challenge the Washington Consensus. They would advocate for debt relief, social protection, and policies tailored to local contexts.
% DISAPPEARANCE_RATIONALE: If structural adjustment conditionalities vanished overnight, debtor states would immediately regain policy autonomy, potentially leading to diverse economic strategies. Core creditor institutions would lose a primary tool for influence and debt enforcement, forcing a renegotiation of international financial governance and potentially leading to widespread debt defaults and a reordering of global economic power.
% FOUNDING_PROBLEM: The founding problem was perceived as widespread fiscal indiscipline, unsustainable debt burdens, and inefficient state-led economies in developing countries, threatening global financial stability and hindering economic growth.
% FOUNDING_PROBLEM_CORROBORATION: Core creditor institutions and hegemon-aligned states assert the problem is still live, citing ongoing debt crises and governance challenges. However, many development economists and non-strategic debtor states argue that the original problem has been exacerbated, or even created, by the conditionalities themselves, and that the persistence of the regime serves primarily to maintain creditor power and extract resources, rather than genuinely solve the founding problem. Independent academic research and reports from UN bodies corroborate this contested status.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__hybrid_selectivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__hybrid_selectivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'none', 1).

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
 *   The constraint is a Tangled Rope because it has a genuine coordination function (stabilizing debtor economies, ensuring repayment capacity) but also exhibits asymmetric extraction. Extractiveness is high (0.7) for non-strategic debtors due to harsh austerity measures and privatization, while suppression (0.85) is very high, reflecting the lack of viable alternatives for states dependent on international finance. The theater ratio (0.4) indicates that a significant portion of the 'reform' rhetoric serves to legitimize selective enforcement rather than universal application of sound economic principles. The rising extractiveness and suppression over time reflect the increasing leverage of creditors and the hardening of enforcement mechanisms against non-strategic actors.
 *
 * PERSPECTIVAL GAP:
 *   Core creditor institutions and hegemon-aligned states perceive the conditionalities as a necessary coordination mechanism for global financial stability. Non-strategic debtor states and their vulnerable populations experience it as a highly extractive and coercive instrument. Geopolitically strategic debtor states experience a moderated version, benefiting from their strategic position. The engine's per-seat classification will reflect these divergent experiences based on their declared power, exit options, and beneficiary/victim status.
 *
 * DIRECTIONALITY LOGIC:
 *   Core creditor institutions and hegemon-aligned states are beneficiaries (d near 0.0) as they secure repayment, maintain influence, and benefit from market access. Non-strategic debtor states and their vulnerable populations are clear targets (d near 1.0) due to imposed austerity and limited exit options. Geopolitically strategic debtor states are also targets but with lower directionality (d closer to 0.7) due to their ability to negotiate waivers and leverage their strategic importance. Development economists are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (fiscal stability, market integration) is partially resolved for strategic debtors but remains a live, and often worsening, problem for non-strategic ones. The 'hybrid selectivity' reading prevents mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring the coordination rhetoric and strategic differentiation). It highlights how the original mandate has been co-opted and differentiated by geopolitical power dynamics, leading to a persistent, selectively extractive structure rather than a fully atrophied one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''hybrid selectivity'' reading of structural adjustment conditionalities, or is it better described by a ''creditor coordination'' or ''debtor extraction'' reading?',
    'Empirical analysis of conditionalities application across a wider range of debtor states, correlating enforcement stringency with geopolitical alignment and strategic resource endowments.',
    'If ''creditor coordination'' is the true reading, the constraint is a Rope; if ''debtor extraction'' is the true reading, it is a Snare. This ''hybrid selectivity'' reading suggests a Tangled Rope with geopolitically differentiated victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity in the primary reading of structural adjustment conditionalities.').

omega_variable(
    geopolitical_vs_economic_drivers,
    'To what extent is the selective application of conditionalities driven by geopolitical strategic interests versus purely economic considerations (e.g., debt sustainability, market access)?',
    'Regression analysis controlling for economic fundamentals, isolating the effect of geopolitical alliance, voting patterns in international bodies, and strategic resource endowments on conditionality enforcement.',
    'If primarily geopolitical, the ''hybrid selectivity'' reading is strongly supported, highlighting the non-economic basis of extraction. If primarily economic, the ''creditor coordination'' reading gains strength.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_vs_economic_drivers, empirical, 'Drivers of selective conditionality enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(stru_tr_t10, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(stru_tr_t20, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(stru_tr_t30, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(stru_be_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(stru_be_t10, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(stru_be_t20, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(stru_be_t30, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 30, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(stru_su_t10, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(stru_su_t20, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(stru_su_t30, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 30, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__hybrid_selectivity_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'structural_adjustment_conditionalities' kernel, focusing on the selective application of discipline based on geopolitical factors. It is linked to other readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
