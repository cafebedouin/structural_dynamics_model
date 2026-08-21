% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__policy_flexible_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__policy_flexible_reading, []).

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
 *   constraint_id: dollar_gold_convertibility__policy_flexible_reading
 *   human_readable: Dollar Gold Convertibility (Policy-Flexible Reading)
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   This constraint represents the 'policy-flexible' reading of dollar-gold
 *   convertibility, where the US government views its obligation to convert
 *   dollars to gold as conditional and subordinate to domestic economic
 *   stability. This reading emerged and gained prominence during the Bretton
 *   Woods era, particularly as US domestic policy priorities increasingly
 *   diverged from the strictures of fixed exchange rates and gold
 *   convertibility. The constraint functions as a Tangled Rope: it provides a
 *   coordination function (a global reserve currency) but with clear
 *   asymmetric extraction, as the US benefits from monetary autonomy at the
 *   expense of dollar holders. The increasing extractiveness and suppression
 *   over the interval reflect the growing tension between US domestic policy
 *   and its international monetary obligations, culminating in the Nixon
 *   Shock of 1971.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, 0.75).
domain_priors:suppression_score(dollar_gold_convertibility__policy_flexible_reading, 0.8).
domain_priors:theater_ratio(dollar_gold_convertibility__policy_flexible_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__policy_flexible_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__policy_flexible_reading, "Dollar Gold Convertibility (Policy-Flexible Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__policy_flexible_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__policy_flexible_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__policy_flexible_reading, '91bf46bc-3c45-4560-894c-7cdc7f7cd534').
narrative_ontology:cs_kernel_codification('91bf46bc-3c45-4560-894c-7cdc7f7cd534', formalized).
narrative_ontology:cs_authority_grounding('91bf46bc-3c45-4560-894c-7cdc7f7cd534', extraction).
narrative_ontology:cs_interpretation_layer_present('91bf46bc-3c45-4560-894c-7cdc7f7cd534').
narrative_ontology:cs_reading_relation('91bf46bc-3c45-4560-894c-7cdc7f7cd534', dollar_gold_convertibility__strict_convertibility_reading, forecloses).
narrative_ontology:cs_reading_relation('91bf46bc-3c45-4560-894c-7cdc7f7cd534', dollar_gold_convertibility__triffin_structural_reading, coexists_with).
narrative_ontology:cs_axiom('91bf46bc-3c45-4560-894c-7cdc7f7cd534', foundational, domestic_stability_priority).
narrative_ontology:cs_axiom_status(domestic_stability_priority, holdable).
narrative_ontology:cs_axiom_grounding('91bf46bc-3c45-4560-894c-7cdc7f7cd534', domestic_stability_priority, instrumental).
narrative_ontology:cs_axiom('91bf46bc-3c45-4560-894c-7cdc7f7cd534', secondary, convertibility_as_policy_tool).
narrative_ontology:cs_axiom_status(convertibility_as_policy_tool, holdable).
narrative_ontology:cs_axiom_grounding('91bf46bc-3c45-4560-894c-7cdc7f7cd534', convertibility_as_policy_tool, conventional).
narrative_ontology:cs_reference_frame('91bf46bc-3c45-4560-894c-7cdc7f7cd534', national_interest_monetary_sovereignty).
narrative_ontology:cs_drift_state('91bf46bc-3c45-4560-894c-7cdc7f7cd534', post_bretton_woods_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('91bf46bc-3c45-4560-894c-7cdc7f7cd534', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, united_states_government).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_domestic_economy).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, international_dollar_holders).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, international_creditors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Prioritizes domestic economic stability and growth, viewing dollar convertibility as a policy tool to be managed flexibly. Benefits from monetary autonomy and the ability to devalue the dollar to address domestic economic challenges, shifting costs to external holders.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, united_states_government, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefits from the US government's ability to conduct independent monetary policy, which can stimulate growth, manage inflation, and reduce unemployment without being strictly constrained by external convertibility obligations.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_domestic_economy, beneficiary,
    moderate, biographical, mobile, national).

% Hold significant dollar reserves as a primary component of their national wealth and international liquidity. They bear the risk of dollar devaluation, which erodes the value of their reserves, but have limited alternatives for large-scale reserve asset diversification.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks, payer,
    institutional, biographical, constrained, global).

% Private corporations, financial institutions, and individuals outside the US who hold substantial dollar-denominated assets. They are exposed to devaluation risk and face high transaction costs or limited options to exit the dollar system due to its global dominance.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, international_dollar_holders, payer,
    organized, biographical, constrained, global).

% Tasked with overseeing the international monetary system and promoting stability. While formally upholding convertibility principles, its ability to enforce strict adherence on the US is limited by the US's structural power and its own institutional design.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, international_monetary_fund, observer,
    institutional, civilizational, analytical, global).

% Entities that have lent money to the US government or US-based entities, with claims denominated in dollars. They face the risk that US monetary policy, prioritizing domestic stability, could lead to dollar devaluation, reducing the real value of their returns.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, international_creditors, payer,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a framework for international monetary cooperation, a stable reserve currency (the dollar), and a mechanism for facilitating global trade and investment, while allowing sovereign nations (especially the US) some flexibility in managing their domestic economies.
% TRANSFER_FUNCTION: Transfers the costs of US domestic monetary policy adjustments (e.g., inflation, devaluation) from the US economy to foreign central banks, international dollar holders, and international creditors who bear the risk of reduced real value of their dollar-denominated assets.
% ABSENT_VOICES: Advocates for a truly multilateral, non-dollar-centric international reserve system (e.g., a global synthetic reserve asset or a return to a more rigid commodity standard) are structurally marginalized by the existing system's inertia and US power. They would argue for a more equitable distribution of seigniorage and adjustment burdens.
% DISAPPEARANCE_RATIONALE: If the conditional convertibility framework vanished overnight, the global financial system would face immediate and severe disruption. The dollar's role as the primary reserve currency would be undermined, leading to widespread currency instability, capital flight, and a potential collapse of international trade and investment as countries scrambled for alternative arrangements.
% FOUNDING_PROBLEM: The need to establish a stable international monetary order after the economic chaos of the interwar period, avoiding competitive devaluations and promoting global trade, while granting nations the ability to manage their domestic economies to prevent unemployment and deflation.
% FOUNDING_PROBLEM_CORROBORATION: US policymakers and some economists argue that the problem of balancing domestic and international stability remains live, justifying policy flexibility. Foreign central banks, international creditors, and some international economists argue that the original problem has been largely superseded by US monetary dominance, and the arrangement now primarily serves US interests, citing historical events like the Nixon Shock as evidence of this shift in priority.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__policy_flexible_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__policy_flexible_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__policy_flexible_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(dollar_gold_convertibility__policy_flexible_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__policy_flexible_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__policy_flexible_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__policy_flexible_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the US could effectively devalue the dollar against gold, transferring wealth from foreign dollar holders. Suppression is high because foreign central banks and international dollar holders had limited alternatives to holding dollars for international transactions and reserves. Theater ratio increased as the US's commitment to convertibility became increasingly performative, masking a growing prioritization of domestic policy. The claimed type is Tangled Rope because it maintained a coordination function (the dollar as a reserve currency) but with clear and increasing asymmetric extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the US government, this arrangement was a necessary and pragmatic approach to managing a complex global economy while safeguarding national interests. From the perspective of foreign dollar holders, it represented an increasingly extractive system where the US leveraged its reserve currency status to externalize its domestic economic costs. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The US government and its domestic economy are clear beneficiaries, gaining monetary policy flexibility. Foreign central banks, international dollar holders, and international creditors are victims, bearing the costs of devaluation risk and limited exit options. The IMF acts as an observer, with limited power to alter the US's structural position.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    policy_vs_binding_obligation,
    'Was dollar-gold convertibility fundamentally a flexible policy choice for the US, or a binding international legal obligation?',
    'Analysis of international legal precedents, diplomatic communications, and the explicit intent of the Bretton Woods agreements, weighed against US domestic legislative and executive actions.',
    'If primarily a binding obligation, the US''s actions would be reclassified as a breach of international law, increasing the perceived extractiveness and suppression from the perspective of international partners. If a flexible policy, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_vs_binding_obligation, conceptual, 'Ambiguity regarding the legal and political nature of the convertibility obligation.').

omega_variable(
    domestic_vs_external_drivers,
    'To what extent was the increasing extractiveness driven by deliberate US policy choices versus unavoidable structural pressures of the Triffin Dilemma?',
    'Counterfactual historical analysis comparing outcomes under alternative US policy paths with those predicted by structural economic models of the Triffin Dilemma.',
    'If primarily driven by deliberate policy, the constraint''s extractiveness is more attributable to agency. If primarily structural, the extractiveness is an emergent property of the system, potentially shifting the classification towards a more ''mountain-like'' aspect of the international monetary system.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(domestic_vs_external_drivers, empirical, 'Distinguishing agency from structural necessity in the constraint''s evolution.').

omega_variable(
    coordination_extraction_separability,
    'Could the dollar''s role as a global reserve currency (coordination) have been maintained without the asymmetric extraction derived from US monetary autonomy?',
    'Examination of proposals for alternative reserve assets (e.g., SDRs) and their potential for implementation and stability during the Bretton Woods era, or analysis of post-Bretton Woods attempts at multilateral monetary management.',
    'If separable, the extraction component is more clearly a Snare riding on a Rope. If inseparable, the extraction is an inherent cost of the coordination, making it a more ''pure'' Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the coordination and extraction functions of the dollar''s role were structurally linked.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__policy_flexible_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1944, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1944, 0.1).
narrative_ontology:measurement(doll_tr_t1950, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(doll_tr_t1956, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1956, 0.22).
narrative_ontology:measurement(doll_tr_t1962, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1962, 0.3).
narrative_ontology:measurement(doll_tr_t1968, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1968, 0.36).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1971, 0.4).

% Extraction over time
narrative_ontology:measurement(doll_be_t1944, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1944, 0.4).
narrative_ontology:measurement(doll_be_t1950, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1950, 0.48).
narrative_ontology:measurement(doll_be_t1956, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1956, 0.57).
narrative_ontology:measurement(doll_be_t1962, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1962, 0.65).
narrative_ontology:measurement(doll_be_t1968, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1968, 0.72).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1971, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1944, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1944, 0.5).
narrative_ontology:measurement(doll_su_t1950, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1950, 0.58).
narrative_ontology:measurement(doll_su_t1956, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1956, 0.67).
narrative_ontology:measurement(doll_su_t1962, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1962, 0.74).
narrative_ontology:measurement(doll_su_t1968, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1968, 0.78).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1971, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__policy_flexible_reading, global_infrastructure).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, bretton_woods_fixed_exchange_rates).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, eurodollar_market_regulation).

% DUAL FORMULATION NOTE:
% This is one of three readings of the dollar_gold_convertibility kernel, focusing on the US's policy flexibility. It forecloses the 'strict_convertibility_reading' and coexists with the 'triffin_structural_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
