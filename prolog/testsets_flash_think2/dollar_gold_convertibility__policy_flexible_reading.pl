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
 *   This constraint instantiates the 'policy_flexible_reading' of the
 *   'dollar_gold_convertibility' kernel, where convertibility is understood
 *   as a conditional obligation subordinate to domestic economic stability.
 *   This reading emerged as the US increasingly prioritized domestic economic
 *   goals in the 1960s, leading to balance of payments deficits and a
 *   weakening of the dollar's gold backing. This contrasts with the
 *   'strict_convertibility_reading' (which views convertibility as a binding
 *   legal obligation constraining U.S. monetary policy) and the
 *   'triffin_structural_reading' (which views convertibility as an inherently
 *   unsustainable design flaw requiring systemic revision).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, 0.75).
domain_priors:suppression_score(dollar_gold_convertibility__policy_flexible_reading, 0.8).
domain_priors:theater_ratio(dollar_gold_convertibility__policy_flexible_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__policy_flexible_reading, snare).
narrative_ontology:human_readable(dollar_gold_convertibility__policy_flexible_reading, "Dollar Gold Convertibility (Policy-Flexible Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__policy_flexible_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__policy_flexible_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__policy_flexible_reading, '0db5ad24-e404-4667-a3b5-a7e5fce4d1e4').
narrative_ontology:cs_kernel_codification('0db5ad24-e404-4667-a3b5-a7e5fce4d1e4', formalized).
narrative_ontology:cs_authority_grounding('0db5ad24-e404-4667-a3b5-a7e5fce4d1e4', lineage).
narrative_ontology:cs_interpretation_layer_present('0db5ad24-e404-4667-a3b5-a7e5fce4d1e4').
narrative_ontology:cs_reading_relation('0db5ad24-e404-4667-a3b5-a7e5fce4d1e4', dollar_gold_convertibility__strict_convertibility_reading, forecloses).
narrative_ontology:cs_reading_relation('0db5ad24-e404-4667-a3b5-a7e5fce4d1e4', dollar_gold_convertibility__triffin_structural_reading, coexists_with).
narrative_ontology:cs_axiom('0db5ad24-e404-4667-a3b5-a7e5fce4d1e4', foundational, domestic_stability_priority).
narrative_ontology:cs_axiom_status(domestic_stability_priority, holdable).
narrative_ontology:cs_axiom_grounding('0db5ad24-e404-4667-a3b5-a7e5fce4d1e4', domestic_stability_priority, instrumental).
narrative_ontology:cs_axiom('0db5ad24-e404-4667-a3b5-a7e5fce4d1e4', foundational, monetary_sovereignty_imperative).
narrative_ontology:cs_axiom_status(monetary_sovereignty_imperative, holdable).
narrative_ontology:cs_axiom_grounding('0db5ad24-e404-4667-a3b5-a7e5fce4d1e4', monetary_sovereignty_imperative, deontological).
narrative_ontology:cs_reference_frame('0db5ad24-e404-4667-a3b5-a7e5fce4d1e4', bretton_woods_flexible_interpretation).
narrative_ontology:cs_drift_state('0db5ad24-e404-4667-a3b5-a7e5fce4d1e4', post_vietnam_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0db5ad24-e404-4667-a3b5-a7e5fce4d1e4', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, united_states_government).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_domestic_economy).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, international_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the issuer of the global reserve currency, the US government interprets convertibility as conditional, allowing it to prioritize domestic economic stability (e.g., full employment, growth) even if it means devaluing the dollar or suspending convertibility. It actively enforces this policy flexibility through monetary decisions and diplomatic pressure.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, united_states_government, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefits from the US government's ability to conduct independent monetary policy, insulated from external convertibility constraints. This allows for counter-cyclical fiscal and monetary measures without immediate balance-of-payments crises, supporting domestic employment and growth.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_domestic_economy, beneficiary,
    powerful, biographical, mobile, national).

% Hold significant dollar reserves as the primary international medium of exchange and store of value. They bear the risk of dollar devaluation or convertibility suspension, which can erode the value of their reserves and destabilize their own economies. Their options for exiting the dollar system are severely limited due to its global dominance.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks, payer,
    institutional, biographical, constrained, global).

% Hold dollar-denominated assets and conduct international transactions in dollars. They are vulnerable to changes in US monetary policy that affect the dollar's value, leading to potential losses on their investments. While they can shift assets, the sheer volume of dollar-denominated markets makes full exit difficult.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, international_investors, payer,
    powerful, immediate, constrained, global).

% Monitors the international monetary system and provides a forum for discussion and negotiation. While it has a mandate to promote stability, its influence on the US's interpretation of convertibility is limited by the US's structural power within the system.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).

% Economists, policymakers, and political groups who argue for a return to strict convertibility rules, viewing the policy-flexible interpretation as a source of instability and unfair advantage. Their voices are often marginalized in official policy circles due to the perceived impracticality of their proposals.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, strict_convertibility_advocates, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a global reserve currency and a flexible international monetary anchor, facilitating international trade, investment, and financial flows by offering a common unit of account and store of value, while allowing the issuing country policy space.
% TRANSFER_FUNCTION: Transfers the costs of US domestic economic adjustments (e.g., inflation, balance of payments deficits) to foreign holders of dollar reserves and dollar-denominated assets, who bear the risk of devaluation or convertibility suspension.
% ABSENT_VOICES: Advocates for a truly multilateral reserve currency, a return to strict gold convertibility, or a more equitable international monetary system are structurally marginalized by the US's dominant position and its flexible interpretation of convertibility. They would argue for a system where the burden of adjustment is shared more symmetrically.
% DISAPPEARANCE_RATIONALE: If the US dollar's role as a flexible reserve currency, backed by this policy interpretation, vanished overnight, the global financial system would undergo a massive, chaotic reorganization. International trade, debt, and investment flows would lose their primary anchor, leading to severe economic disruption and a scramble for alternative reserve assets.
% FOUNDING_PROBLEM: To establish a stable international monetary system after World War II, providing liquidity for global trade and investment, while also granting the United States sufficient monetary policy autonomy to manage its domestic economy.
% FOUNDING_PROBLEM_CORROBORATION: US government officials and many mainstream economists argue that this policy flexibility remains crucial for global financial stability and US economic health. However, foreign central banks, some international economists, and critics of US monetary dominance argue that the founding problem has evolved, and the current arrangement primarily serves US interests by externalizing costs, making the status 'contested'.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__policy_flexible_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__policy_flexible_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__policy_flexible_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.75) is high because the US's policy flexibility effectively externalizes the costs of its domestic economic management onto foreign dollar holders, who face devaluation risk. Suppression (0.80) is also high, reflecting the limited exit options for foreign central banks and investors from the dollar-centric global financial system. The theater ratio (0.20) is relatively low, as the US's policy flexibility was a genuine, active stance, though the rhetoric of strict convertibility was maintained until the Nixon Shock in 1971. The increasing extractiveness and suppression over the interval reflect the growing tension between US domestic policy and its international monetary obligations.
 *
 * PERSPECTIVAL GAP:
 *   From the US perspective, this reading of convertibility is a necessary and beneficial coordination mechanism for global stability, providing a flexible reserve currency. From the perspective of foreign central banks and international investors, it operates as an extractive mechanism, allowing the US to externalize its economic problems. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The US government and its domestic economy are clear beneficiaries, gaining monetary autonomy and the ability to manage domestic cycles without external constraint (low directionality). Foreign central banks and international investors are the primary targets, bearing the costs of dollar devaluation and inflation (high directionality). The IMF acts as an observer, while strict convertibility advocates are excluded from the dominant policy discourse.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not a case of mandatrophy, as the 'mandate' (providing a flexible reserve currency while maintaining domestic stability) was actively pursued and enforced by the US. The classification as a Snare reflects that this pursuit of flexibility came at a significant, imposed cost to other parties, rather than a decay of function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    policy_necessity_vs_opportunism,
    'Was the US''s policy flexibility a genuine necessity for global economic stability and domestic welfare, or an opportunistic leveraging of its reserve currency status?',
    'Counterfactual economic modeling of alternative international monetary systems during the 1960s, or historical analysis of internal US policy debates regarding the balance between domestic and international obligations.',
    'If primarily opportunistic, the constraint''s extractiveness is more clearly a rent; if genuinely necessary, a portion of the extraction might be reclassified as a coordination cost, though still asymmetrically borne.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_necessity_vs_opportunism, conceptual, 'Ambiguity regarding the motivation behind the policy-flexible interpretation of convertibility.').

omega_variable(
    cost_of_strict_convertibility,
    'What would have been the actual economic and political costs to the US of adhering to a strict convertibility obligation during the 1960s?',
    'Detailed historical economic analysis, including counterfactual simulations of US monetary policy under strict convertibility and its impact on domestic employment, growth, and international relations.',
    'If the costs were prohibitive, it would strengthen the ''necessity'' argument for policy flexibility. If manageable, it would weaken the justification for externalizing costs onto dollar holders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_of_strict_convertibility, empirical, 'The unquantified costs of an alternative, stricter convertibility regime for the US.').

omega_variable(
    convertibility_obligation_ambiguity,
    'Is dollar-gold convertibility fundamentally a binding legal obligation (as per IMF Article IV) or a conditional policy tool subject to national economic priorities?',
    'Legal and historical analysis of the IMF Articles of Agreement, subsequent interpretations, and the diplomatic record leading up to and following the Nixon Shock. This is a core interpretive disagreement.',
    'Resolution would fundamentally alter the perceived legitimacy and structural properties of the constraint, potentially reclassifying it from a Snare (if conditional) to a Tangled Rope or even a Mountain (if strictly binding and universally accepted).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(convertibility_obligation_ambiguity, conceptual, 'The core ambiguity of the dollar-gold convertibility kernel itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__policy_flexible_reading, 1960, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1960, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(doll_tr_t1963, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1963, 0.13).
narrative_ontology:measurement(doll_tr_t1966, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1966, 0.16).
narrative_ontology:measurement(doll_tr_t1969, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1969, 0.18).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1971, 0.2).

% Extraction over time
narrative_ontology:measurement(doll_be_t1960, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1960, 0.6).
narrative_ontology:measurement(doll_be_t1963, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1963, 0.65).
narrative_ontology:measurement(doll_be_t1966, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1966, 0.7).
narrative_ontology:measurement(doll_be_t1969, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1969, 0.73).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1971, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1960, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1960, 0.65).
narrative_ontology:measurement(doll_su_t1963, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1963, 0.7).
narrative_ontology:measurement(doll_su_t1966, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1966, 0.75).
narrative_ontology:measurement(doll_su_t1969, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1969, 0.78).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1971, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__policy_flexible_reading, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
