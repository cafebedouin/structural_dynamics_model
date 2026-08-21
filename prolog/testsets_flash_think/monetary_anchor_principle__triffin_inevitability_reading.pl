% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__triffin_inevitability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__triffin_inevitability_reading, []).

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
 *   constraint_id: monetary_anchor_principle__triffin_inevitability_reading
 *   human_readable: Triffin Dilemma Inevitability of Monetary Anchor Transition
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint describes the structural inevitability of the Bretton
 *   Woods system's collapse due to the Triffin dilemma. The dilemma posits
 *   that a reserve currency issuer (the US) under a gold standard must run
 *   persistent balance of payments deficits to supply sufficient liquidity
 *   for global trade and growth. However, these deficits erode confidence in
 *   the reserve currency's convertibility to gold, eventually exhausting gold
 *   reserves and forcing the abandonment of the gold standard. This reading
 *   frames the transition as a logical and physical impossibility, a
 *   'mountain' that the system could not overcome.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__triffin_inevitability_reading, 0.15).
domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, 0.05).
domain_priors:theater_ratio(monetary_anchor_principle__triffin_inevitability_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__triffin_inevitability_reading, mountain).
narrative_ontology:human_readable(monetary_anchor_principle__triffin_inevitability_reading, "Triffin Dilemma Inevitability of Monetary Anchor Transition").
narrative_ontology:topic_domain(monetary_anchor_principle__triffin_inevitability_reading, "monetary_economics/political_economy/international_finance").

domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__triffin_inevitability_reading, '6f0eee85-a2e6-406d-8c12-69c3ad4f5a23').
narrative_ontology:cs_kernel_codification('6f0eee85-a2e6-406d-8c12-69c3ad4f5a23', formalized).
narrative_ontology:cs_authority_grounding('6f0eee85-a2e6-406d-8c12-69c3ad4f5a23', lineage).
narrative_ontology:cs_interpretation_layer_present('6f0eee85-a2e6-406d-8c12-69c3ad4f5a23').
narrative_ontology:cs_reading_relation('6f0eee85-a2e6-406d-8c12-69c3ad4f5a23', monetary_anchor_principle__punctuated_swap_reading, forecloses).
narrative_ontology:cs_reading_relation('6f0eee85-a2e6-406d-8c12-69c3ad4f5a23', monetary_anchor_principle__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('6f0eee85-a2e6-406d-8c12-69c3ad4f5a23', foundational, gold_reserve_scarcity_limit).
narrative_ontology:cs_axiom_status(gold_reserve_scarcity_limit, holdable).
narrative_ontology:cs_axiom_grounding('6f0eee85-a2e6-406d-8c12-69c3ad4f5a23', gold_reserve_scarcity_limit, empirically_contingent).
narrative_ontology:cs_axiom('6f0eee85-a2e6-406d-8c12-69c3ad4f5a23', foundational, global_liquidity_demand_growth).
narrative_ontology:cs_axiom_status(global_liquidity_demand_growth, holdable).
narrative_ontology:cs_axiom_grounding('6f0eee85-a2e6-406d-8c12-69c3ad4f5a23', global_liquidity_demand_growth, empirically_contingent).
narrative_ontology:cs_reference_frame('6f0eee85-a2e6-406d-8c12-69c3ad4f5a23', bretton_woods_gold_exchange_standard).
narrative_ontology:cs_drift_state('6f0eee85-a2e6-406d-8c12-69c3ad4f5a23', post_1971_nixon_shock, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('6f0eee85-a2e6-406d-8c12-69c3ad4f5a23', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, global_financial_markets).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__triffin_inevitability_reading, triffin_dilemma_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The international monetary system established post-WWII, based on fixed exchange rates and the US dollar convertible to gold. It was structurally unable to escape the dilemma of needing to supply global liquidity while maintaining gold convertibility, ultimately bearing the cost of its own collapse.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework, payer,
    institutional, generational, trapped, global).

% Responsible for managing the US dollar as the global reserve currency and maintaining its gold convertibility. Faced the direct policy challenge of the Triffin dilemma, attempting to balance domestic and international demands until the structural contradiction became insurmountable.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, us_treasury, agenda_setter,
    institutional, biographical, constrained, national).

% The international organization tasked with overseeing the global monetary system. Observed the unfolding dilemma and its implications, but was ultimately unable to resolve the inherent structural contradiction of the Bretton Woods system.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).

% Experienced increasing instability and uncertainty as the Triffin dilemma intensified, leading to speculative attacks on the dollar and ultimately the breakdown of the fixed exchange rate system. Bore the costs of market volatility and regime change.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, global_financial_markets, payer,
    organized, immediate, constrained, global).

% Analyzed and articulated the Triffin dilemma as a fundamental structural contradiction. Their role was to understand and explain the inevitability of the system's collapse, rather than to directly participate in its operation or suffer its costs.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, monetary_theorists, observer,
    analytical, civilizational, analytical, universal).

narrative_ontology:fixing_cost_class(monetary_anchor_principle__triffin_inevitability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Bretton Woods system coordinated global trade and finance by providing a stable, gold-backed reserve currency and fixed exchange rates, facilitating international economic activity.
% TRANSFER_FUNCTION: The system required the US to run balance of payments deficits to supply sufficient dollars for global liquidity, effectively transferring gold reserves from the US to other nations, and eventually transferring the burden of instability back to the US and the global system.
% ABSENT_VOICES: Advocates for alternative, more flexible international monetary systems (e.g., floating exchange rates, a truly international reserve asset like Keynes's Bancor) were largely absent from the initial Bretton Woods design and later marginalized, but their proposals would have addressed the dilemma's structural flaws.
% DISAPPEARANCE_RATIONALE: The Bretton Woods system, specifically its gold-dollar convertibility, did disappear. Its collapse led to a fundamental rearrangement of the global monetary order, ushering in an era of floating exchange rates and a fiat dollar standard, with profound implications for international finance and trade.
% FOUNDING_PROBLEM: The Bretton Woods system was built to solve the problem of monetary instability and competitive devaluations that plagued the interwar period, aiming to create a stable framework for post-WWII economic reconstruction and growth.
% FOUNDING_PROBLEM_CORROBORATION: Economists and historians widely corroborate that the gold-exchange standard component of Bretton Woods became unsustainable due to the Triffin dilemma, leading to its abandonment. The system's architects and later policymakers acknowledged the growing strain, as documented in academic literature and historical records.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__triffin_inevitability_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__triffin_inevitability_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__triffin_inevitability_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(monetary_anchor_principle__triffin_inevitability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__triffin_inevitability_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__triffin_inevitability_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, ExtMetricName, E),
    domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(monetary_anchor_principle__triffin_inevitability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is low because it's not a deliberate human extraction but the inherent cost of a system's structural contradiction. It increases over time as the dilemma tightens its grip. `suppression` is very low because it's not actively enforced coercion but a logical limit. `theater_ratio` is negligible as there's little performative maintenance of a system facing an inevitable structural failure. `accessibility_collapse` is high (0.9) because, from this reading, there were no viable alternatives within the gold-exchange standard framework that could resolve the dilemma. `resistance` is low because one cannot resist a mathematical contradiction.
 *
 * PERSPECTIVAL GAP:
 *   Policymakers at the time often viewed the situation as a series of policy choices and crises, attempting to manage the system through various interventions. Monetary theorists, however, increasingly saw it as a structural inevitability, a logical contradiction that policy could only delay, not avert. This reading aligns with the latter, emphasizing the 'mountain' aspect.
 *
 * DIRECTIONALITY LOGIC:
 *   There are no beneficiaries in this reading, as the dilemma represents a system-level failure from which no party genuinely profits in the long run. The Bretton Woods institutional framework itself is the primary victim, as it was the structure that ultimately failed due to its inherent design flaw. The US Treasury and global financial markets are also payers, bearing the direct costs of managing and experiencing the instability.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    triffin_dilemma_causal_primacy,
    'Was the Triffin dilemma the *sole* and *sufficient* cause of the Bretton Woods collapse, or one of several contributing factors?',
    'Counterfactual historical analysis: could the system have survived if other factors (e.g., Vietnam War deficits, lack of political will) had been absent, even with the Triffin dilemma present?',
    'If Triffin was sole cause, this ''mountain'' classification is strengthened. If it was one of many, the ''overdetermined_composite_reading'' gains strength, suggesting a more complex, less purely inevitable structural failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_dilemma_causal_primacy, empirical, 'Assesses the causal primacy of the Triffin dilemma in the Bretton Woods collapse.').

omega_variable(
    policy_choice_vs_inevitability,
    'Could different policy choices (e.g., earlier devaluation, stricter capital controls, a new international reserve asset) have averted the collapse, or was the structural contradiction truly insurmountable?',
    'Economic modeling of alternative historical paths, or analysis of proposals like Keynes''s Bancor and their feasibility within the political economy of the time.',
    'If viable policy alternatives existed, the ''punctuated_swap_reading'' (emphasizing choice) would be strengthened, and this ''mountain'' reading would be weakened, potentially reclassifying as a ''snare'' or ''tangled_rope'' sustained by policy inertia.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(policy_choice_vs_inevitability, conceptual, 'Examines whether the collapse was a policy failure or a structural inevitability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__triffin_inevitability_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1944, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1944, 0.01).
narrative_ontology:measurement(mone_tr_t1950, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1950, 0.01).
narrative_ontology:measurement(mone_tr_t1958, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1958, 0.01).
narrative_ontology:measurement(mone_tr_t1965, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1965, 0.02).
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1971, 0.02).

% Extraction over time
narrative_ontology:measurement(mone_be_t1944, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1944, 0.05).
narrative_ontology:measurement(mone_be_t1950, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1950, 0.07).
narrative_ontology:measurement(mone_be_t1958, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1958, 0.1).
narrative_ontology:measurement(mone_be_t1965, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1965, 0.13).
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1971, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t1944, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1944, 0.01).
narrative_ontology:measurement(mone_su_t1950, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1950, 0.02).
narrative_ontology:measurement(mone_su_t1958, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1958, 0.03).
narrative_ontology:measurement(mone_su_t1965, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1965, 0.04).
narrative_ontology:measurement(mone_su_t1971, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1971, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
