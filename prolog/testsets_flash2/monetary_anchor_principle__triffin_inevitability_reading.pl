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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Triffin Dilemma Inevitability (Monetary Anchor Principle Reading)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint represents the 'triffin_inevitability_reading' of the
 *   'monetary_anchor_principle' kernel. It asserts that the collapse of the
 *   Bretton Woods gold-backed dollar standard was a structural inevitability
 *   due to the Triffin dilemma. The dilemma states that a reserve currency
 *   issuer (like the US under Bretton Woods) must run persistent balance of
 *   payments deficits to supply sufficient global liquidity, which eventually
 *   undermines confidence in its ability to convert its currency to gold,
 *   leading to the system's collapse. This reading frames the transition as a
 *   physical/logical impossibility, not a policy choice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__triffin_inevitability_reading, 0.05).
domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, 0.95).
domain_priors:theater_ratio(monetary_anchor_principle__triffin_inevitability_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__triffin_inevitability_reading, mountain).
narrative_ontology:human_readable(monetary_anchor_principle__triffin_inevitability_reading, "Triffin Dilemma Inevitability (Monetary Anchor Principle Reading)").
narrative_ontology:topic_domain(monetary_anchor_principle__triffin_inevitability_reading, "monetary_economics/political_economy/international_finance").

domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__triffin_inevitability_reading, 'b7ee154d-16dd-445e-8a86-2ad471b6182c').
narrative_ontology:cs_kernel_codification('b7ee154d-16dd-445e-8a86-2ad471b6182c', formalized).
narrative_ontology:cs_authority_grounding('b7ee154d-16dd-445e-8a86-2ad471b6182c', lineage).
narrative_ontology:cs_interpretation_layer_present('b7ee154d-16dd-445e-8a86-2ad471b6182c').
narrative_ontology:cs_reading_relation('b7ee154d-16dd-445e-8a86-2ad471b6182c', monetary_anchor_principle__punctuated_swap_reading, forecloses).
narrative_ontology:cs_reading_relation('b7ee154d-16dd-445e-8a86-2ad471b6182c', monetary_anchor_principle__overdetermined_composite_reading, forecloses).
narrative_ontology:cs_axiom('b7ee154d-16dd-445e-8a86-2ad471b6182c', foundational, reserve_currency_dilemma_is_structural_inevitability).
narrative_ontology:cs_axiom_status(reserve_currency_dilemma_is_structural_inevitability, holdable).
narrative_ontology:cs_axiom_grounding('b7ee154d-16dd-445e-8a86-2ad471b6182c', reserve_currency_dilemma_is_structural_inevitability, deontological).
narrative_ontology:cs_reference_frame('b7ee154d-16dd-445e-8a86-2ad471b6182c', gold_standard_structural_logic).
narrative_ontology:cs_drift_state('b7ee154d-16dd-445e-8a86-2ad471b6182c', bretton_woods_collapse_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('b7ee154d-16dd-445e-8a86-2ad471b6182c', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, global_financial_markets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The entire system, including its rules and institutions, was structurally unable to resolve the inherent contradiction of the Triffin dilemma, leading to its eventual collapse. It bore the costs of this inevitability.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework, payer,
    institutional, generational, trapped, global).

% As the issuer of the reserve currency, the US Treasury was forced to run deficits to provide global liquidity, which simultaneously undermined confidence in its gold reserves. It administered the system but was caught in its structural trap.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, united_states_treasury, agenda_setter,
    institutional, biographical, constrained, global).

% The IMF was designed to oversee the Bretton Woods system but could not resolve its fundamental structural flaw. Its economists observed and analyzed the growing dilemma, but its institutional mandate did not allow it to fundamentally alter the underlying contradiction.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).

% Markets reacted to the growing imbalance between dollar liabilities and gold reserves, eventually forcing the US hand. While individual actors could move capital, the system as a whole was subject to the dilemma's pressure.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, global_financial_markets, payer,
    organized, immediate, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Bretton Woods system aimed to coordinate international monetary stability and facilitate global trade and investment through fixed exchange rates and a gold-backed dollar.
% TRANSFER_FUNCTION: The Triffin dilemma describes a structural transfer of confidence from the reserve currency to the demand for global liquidity, ultimately leading to a loss of confidence in the reserve currency's convertibility.
% ABSENT_VOICES: Alternative monetary theorists who advocated for a more flexible or non-gold-backed international system were largely excluded from the initial Bretton Woods design, but their arguments gained traction as the dilemma became apparent.
% DISAPPEARANCE_RATIONALE: If the Triffin dilemma (as a structural inevitability) 'disappeared' overnight, it would imply a fundamental change in the laws of monetary economics, making the world's financial systems operate under different principles. The dilemma itself is a description of a structural reality, not a human-made constraint that could simply vanish.
% FOUNDING_PROBLEM: The Bretton Woods system was built to solve the problem of international monetary instability and competitive devaluations that plagued the interwar period, aiming for stable exchange rates and global economic growth.
% FOUNDING_PROBLEM_CORROBORATION: Economists and historians widely corroborate that the specific problem of interwar instability was addressed, but the system's own internal contradictions (the Triffin dilemma) created a new, unavoidable problem that led to its demise. The founding problem was solved, but the solution contained its own fatal flaw.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__triffin_inevitability_reading, world_unchanged).
narrative_ontology:founding_problem_status(monetary_anchor_principle__triffin_inevitability_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__triffin_inevitability_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(monetary_anchor_principle__triffin_inevitability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__triffin_inevitability_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Mountain because it describes a fundamental structural contradiction inherent in the Bretton Woods system, making its eventual collapse unavoidable. Extractiveness is very low (0.05) as it's a system-level failure, not a deliberate extraction by any party. Suppression is very high (0.95) because the structural forces are overwhelming and cannot be resisted by policy. Accessibility collapse is near total (0.98) as no viable alternative to the dilemma existed within the gold standard framework. Resistance is negligible (0.02) because the inevitability was widely recognized by economists, even if policymakers tried to delay it.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Bretton Woods architects, the system was designed for stability and coordination. From the 'triffin_inevitability_reading', the system contained a fatal flaw that made its long-term stability impossible, regardless of intent or policy choices. The engine's classification as a Mountain reflects this structural, rather than agent-driven, reality.
 *
 * DIRECTIONALITY LOGIC:
 *   No single agent benefits from this structural inevitability; rather, the entire Bretton Woods institutional framework is a 'victim' of its inherent contradiction. The US, as the reserve currency issuer, faced the dilemma, but its position was structurally untenable, not beneficial in the long run. Other nations were also caught in the system's logic.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a pure structural inevitability (Triffin dilemma) or a contingent policy choice?',
    'Historical counterfactual analysis: could policy choices have averted the dilemma without abandoning the gold standard? If no, then pure structural inevitability holds.',
    'If pure structural inevitability, the constraint is a Mountain. If contingent policy choice, it would be reclassified as a Snare (if extractive) or Tangled Rope (if coordination with extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''triffin_inevitability_reading'' of the ''monetary_anchor_principle'' kernel. Sibling readings (''punctuated_swap_reading'', ''overdetermined_composite_reading'') emphasize policy choice or multiple factors, respectively.').

omega_variable(
    triffin_dilemma_scope,
    'To what extent does the Triffin dilemma apply beyond a gold-backed reserve currency to other forms of reserve assets or fiat systems?',
    'Theoretical extension and empirical testing of the dilemma''s core logic (liquidity vs. confidence) in modern fiat reserve systems.',
    'If the dilemma''s core logic is universal, the constraint''s scope as a ''mountain'' of monetary economics is broader. If it is strictly tied to gold convertibility, its historical relevance is limited.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_dilemma_scope, empirical, 'Assesses the generalizability of the Triffin dilemma beyond its original context.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__triffin_inevitability_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t0, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(mone_tr_t10, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 10, 0.0).
narrative_ontology:measurement(mone_tr_t20, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 20, 0.0).
narrative_ontology:measurement(mone_tr_t30, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 30, 0.0).

% Extraction over time
narrative_ontology:measurement(mone_be_t0, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(mone_be_t10, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 10, 0.05).
narrative_ontology:measurement(mone_be_t20, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 20, 0.05).
narrative_ontology:measurement(mone_be_t30, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 30, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t0, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(mone_su_t10, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 10, 0.95).
narrative_ontology:measurement(mone_su_t20, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 20, 0.95).
narrative_ontology:measurement(mone_su_t30, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 30, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__triffin_inevitability_reading, global_infrastructure).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'monetary_anchor_principle' kernel, focusing on the structural inevitability of the Triffin dilemma. It is linked to sibling readings that emphasize policy choice or multiple causal factors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
