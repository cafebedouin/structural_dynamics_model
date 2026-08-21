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
 *   Bretton Woods gold-dollar standard was structurally inevitable due to the
 *   Triffin dilemma: the inherent contradiction of a reserve currency issuer
 *   needing to run deficits to supply global liquidity, which eventually
 *   depletes its gold reserves. This reading frames the transition as a
 *   physical/logical impossibility, not a policy choice. The constraint is
 *   classified as a Mountain because its persistence is due to an irreducible
 *   structural contradiction, not active enforcement or human agency.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__triffin_inevitability_reading, 0.05).
domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, 0.05).
domain_priors:theater_ratio(monetary_anchor_principle__triffin_inevitability_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__triffin_inevitability_reading, mountain).
narrative_ontology:human_readable(monetary_anchor_principle__triffin_inevitability_reading, "Triffin Dilemma Inevitability (Monetary Anchor Principle Reading)").
narrative_ontology:topic_domain(monetary_anchor_principle__triffin_inevitability_reading, "monetary_economics/political_economy/international_finance").

domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__triffin_inevitability_reading, '24c77a41-0d12-4c7a-b1bf-d3e06c44843b').
narrative_ontology:cs_kernel_codification('24c77a41-0d12-4c7a-b1bf-d3e06c44843b', formalized).
narrative_ontology:cs_authority_grounding('24c77a41-0d12-4c7a-b1bf-d3e06c44843b', self_enforcing).
narrative_ontology:cs_reading_relation('24c77a41-0d12-4c7a-b1bf-d3e06c44843b', monetary_anchor_principle__punctuated_swap_reading, forecloses).
narrative_ontology:cs_reading_relation('24c77a41-0d12-4c7a-b1bf-d3e06c44843b', monetary_anchor_principle__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('24c77a41-0d12-4c7a-b1bf-d3e06c44843b', foundational, finite_gold_reserves_axiom).
narrative_ontology:cs_axiom_status(finite_gold_reserves_axiom, holdable).
narrative_ontology:cs_axiom_grounding('24c77a41-0d12-4c7a-b1bf-d3e06c44843b', finite_gold_reserves_axiom, empirically_contingent).
narrative_ontology:cs_axiom('24c77a41-0d12-4c7a-b1bf-d3e06c44843b', foundational, growing_global_liquidity_demand_axiom).
narrative_ontology:cs_axiom_status(growing_global_liquidity_demand_axiom, holdable).
narrative_ontology:cs_axiom_grounding('24c77a41-0d12-4c7a-b1bf-d3e06c44843b', growing_global_liquidity_demand_axiom, empirically_contingent).
narrative_ontology:cs_axiom('24c77a41-0d12-4c7a-b1bf-d3e06c44843b', foundational, inherent_contradiction_axiom).
narrative_ontology:cs_axiom_status(inherent_contradiction_axiom, holdable).
narrative_ontology:cs_axiom_grounding('24c77a41-0d12-4c7a-b1bf-d3e06c44843b', inherent_contradiction_axiom, deontological).
narrative_ontology:cs_reference_frame('24c77a41-0d12-4c7a-b1bf-d3e06c44843b', bretton_woods_gold_exchange_standard).
narrative_ontology:cs_drift_state('24c77a41-0d12-4c7a-b1bf-d3e06c44843b', post_1960s_liquidity_crisis, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('24c77a41-0d12-4c7a-b1bf-d3e06c44843b', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The international monetary system established post-WWII, based on fixed exchange rates and the dollar convertible to gold. It was structurally trapped by the Triffin dilemma, which eventually led to its collapse as gold reserves dwindled and global liquidity demand grew.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework, payer,
    institutional, generational, trapped, global).

% Scholars and researchers who identified, analyzed, and continue to study the Triffin dilemma as a fundamental structural contradiction in international monetary systems. They observe its effects and implications without being directly subject to its extractive force.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, analytical_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Triffin dilemma itself does not perform a coordination function; rather, it describes a fundamental structural contradiction within a system (the Bretton Woods gold-dollar standard) that *attempted* to coordinate global liquidity provision with fixed exchange rates.
% TRANSFER_FUNCTION: The dilemma describes the structural transfer of the burden of global liquidity provision to the reserve currency issuer, which in turn led to a 'transfer' of gold reserves out of the issuer's vaults, ultimately forcing the abandonment of gold convertibility.
% ABSENT_VOICES: Those who believed in the perpetual viability of a gold-backed reserve currency system, or who proposed policy fixes that did not address the fundamental structural contradiction. Their voices were ultimately overridden by the economic realities described by the dilemma.
% DISAPPEARANCE_RATIONALE: The Triffin dilemma describes an inherent structural contradiction in a specific type of monetary system. Its 'disappearance' would imply a change in the fundamental laws of economics or the nature of reserve currencies and finite resources, which is not possible. The dilemma itself is a description of an inevitable failure mode, not a constraint that can be removed.
% FOUNDING_PROBLEM: How to provide sufficient global liquidity for expanding trade and investment while maintaining confidence in a reserve currency that is convertible to a finite commodity (gold) at a fixed price.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians, international finance scholars, and central bank archives corroborate that the problem of reconciling global liquidity needs with gold convertibility was a central challenge of the Bretton Woods era, and that the gold-dollar standard ultimately proved unsustainable. The problem, in its gold-standard form, is now considered 'dead' due to the shift to fiat currencies.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__triffin_inevitability_reading, world_unchanged).
narrative_ontology:founding_problem_status(monetary_anchor_principle__triffin_inevitability_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__triffin_inevitability_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The extractiveness is very low (0.05) because the dilemma itself is a structural pressure, not an active extraction by any party; it's a system-level failure. Suppression is also very low (0.05) as it's a natural law, not actively enforced. Theater ratio is negligible (0.01) as there's no performative maintenance of a structural inevitability. Accessibility collapse is very high (0.95) because, within the given parameters (gold standard, reserve currency, global liquidity demand), there are no viable alternatives to the eventual collapse. Resistance is low (0.05) because one cannot 'resist' a mathematical contradiction. The temporal measurements show a very slight increase in extractiveness as the structural pressure built over time, but the core metrics remain low, reflecting the inherent, non-agentic nature of the dilemma.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Bretton Woods institutional framework, the dilemma was an existential threat and ultimately a victimizing force. From an analytical observer's perspective, it is a structural truth. The engine's classification for the institutional framework would reflect its victim status, while the overall constraint remains a Mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bretton Woods institutional framework is the primary victim, as it was the system that ultimately collapsed under the dilemma's pressure. There are no direct beneficiaries of the dilemma itself, as it describes a system failure. Analytical economists are observers, studying the structural forces at play.
 *
 * MANDATROPHY ANALYSIS:
 *   The Triffin dilemma is a structural principle, not a human-designed constraint with a mandate that can atrophy. Its 'function' is to describe an inherent contradiction. Therefore, mandatrophy is not applicable in the traditional sense; the dilemma remains 'live' as a theoretical concept even after the system it described has passed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inevitability_vs_policy_choice,
    'Was the collapse of the Bretton Woods system a purely structural inevitability, or could policy choices have significantly delayed or altered the outcome?',
    'Counterfactual historical analysis comparing outcomes under alternative policy paths (e.g., earlier revaluation of gold, stricter capital controls, or alternative reserve assets).',
    'If policy choices had significant agency, the constraint''s ''mountain'' classification would weaken, potentially shifting towards a ''tangled_rope'' (where the system was maintained by active choices despite structural pressures) or ''snare'' (if the system was maintained for the benefit of some at the expense of others).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inevitability_vs_policy_choice, conceptual, 'Ambiguity between structural inevitability and policy agency in the Triffin dilemma.').

omega_variable(
    triffin_dilemma_scope,
    'Is the Triffin dilemma a universal principle for any reserve currency under a fixed exchange rate, or is its applicability limited to the specific historical context of the Bretton Woods system?',
    'Theoretical modeling and empirical observation of other historical or hypothetical fixed exchange rate systems with a single reserve currency.',
    'If universal, the ''mountain'' classification is reinforced. If context-specific, the ''emerges_naturally'' claim is weakened, suggesting a more constructed or contingent constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(triffin_dilemma_scope, empirical, 'Scope of the Triffin dilemma''s applicability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__triffin_inevitability_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t0, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0, 0.01).
narrative_ontology:measurement(mone_tr_t5, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 5, 0.01).
narrative_ontology:measurement(mone_tr_t10, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 10, 0.01).
narrative_ontology:measurement(mone_tr_t15, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 15, 0.01).
narrative_ontology:measurement(mone_tr_t20, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 20, 0.01).
narrative_ontology:measurement(mone_tr_t25, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 25, 0.01).

% Extraction over time
narrative_ontology:measurement(mone_be_t0, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement(mone_be_t5, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 5, 0.035).
narrative_ontology:measurement(mone_be_t10, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 10, 0.04).
narrative_ontology:measurement(mone_be_t15, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 15, 0.045).
narrative_ontology:measurement(mone_be_t20, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 20, 0.05).
narrative_ontology:measurement(mone_be_t25, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 25, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t0, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(mone_su_t5, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 5, 0.02).
narrative_ontology:measurement(mone_su_t10, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 10, 0.02).
narrative_ontology:measurement(mone_su_t15, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 15, 0.02).
narrative_ontology:measurement(mone_su_t20, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 20, 0.02).
narrative_ontology:measurement(mone_su_t25, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 25, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__triffin_inevitability_reading, global_infrastructure).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'monetary_anchor_principle' kernel, focusing on the structural inevitability of the Triffin dilemma. It is linked to sibling readings that emphasize discrete policy choices or a composite of factors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
