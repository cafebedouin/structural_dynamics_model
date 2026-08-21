% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__triffin_structural_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__triffin_structural_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: dollar_gold_convertibility__triffin_structural_reading
 *   human_readable: Dollar-Gold Convertibility (Triffin Structural Reading)
 *   domain: international_political_economy/monetary_history
 *
 * SUMMARY:
 *   This constraint represents the Triffin Dilemma reading of the dollar-gold
 *   convertibility under the Bretton Woods system. It posits that the system
 *   was inherently unstable due to the conflicting demands of providing
 *   global liquidity (requiring the U.S. to run deficits) and maintaining
 *   confidence in the dollar's convertibility to gold (requiring the U.S. to
 *   avoid deficits). This structural flaw made the system a snare for both
 *   the U.S. and creditor nations, inevitably leading to its collapse. The
 *   claimed type is 'snare' because the system's design extracted from its
 *   key participants through an impossible structural bind, rather than
 *   through explicit coercion, leading to an unavoidable collapse.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, 0.85).
domain_priors:suppression_score(dollar_gold_convertibility__triffin_structural_reading, 0.9).
domain_priors:theater_ratio(dollar_gold_convertibility__triffin_structural_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__triffin_structural_reading, snare).
narrative_ontology:human_readable(dollar_gold_convertibility__triffin_structural_reading, "Dollar-Gold Convertibility (Triffin Structural Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__triffin_structural_reading, "international_political_economy/monetary_history").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__triffin_structural_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__triffin_structural_reading, 'cbe694fb-e81f-41d8-abcd-b9b3271eaa2c').
narrative_ontology:cs_kernel_codification('cbe694fb-e81f-41d8-abcd-b9b3271eaa2c', formalized).
narrative_ontology:cs_authority_grounding('cbe694fb-e81f-41d8-abcd-b9b3271eaa2c', extraction).
narrative_ontology:cs_interpretation_layer_present('cbe694fb-e81f-41d8-abcd-b9b3271eaa2c').
narrative_ontology:cs_reading_relation('cbe694fb-e81f-41d8-abcd-b9b3271eaa2c', dollar_gold_convertibility__strict_convertibility_reading, forecloses).
narrative_ontology:cs_reading_relation('cbe694fb-e81f-41d8-abcd-b9b3271eaa2c', dollar_gold_convertibility__policy_flexible_reading, forecloses).
narrative_ontology:cs_axiom('cbe694fb-e81f-41d8-abcd-b9b3271eaa2c', foundational, impossible_trilemma_structural_inevitability).
narrative_ontology:cs_axiom_status(impossible_trilemma_structural_inevitability, holdable).
narrative_ontology:cs_axiom_grounding('cbe694fb-e81f-41d8-abcd-b9b3271eaa2c', impossible_trilemma_structural_inevitability, empirically_contingent).
narrative_ontology:cs_axiom('cbe694fb-e81f-41d8-abcd-b9b3271eaa2c', foundational, global_liquidity_confidence_tradeoff).
narrative_ontology:cs_axiom_status(global_liquidity_confidence_tradeoff, holdable).
narrative_ontology:cs_axiom_grounding('cbe694fb-e81f-41d8-abcd-b9b3271eaa2c', global_liquidity_confidence_tradeoff, empirically_contingent).
narrative_ontology:cs_reference_frame('cbe694fb-e81f-41d8-abcd-b9b3271eaa2c', bretton_woods_design_principles).
narrative_ontology:cs_drift_state('cbe694fb-e81f-41d8-abcd-b9b3271eaa2c', post_triffin_dilemma_recognition, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('cbe694fb-e81f-41d8-abcd-b9b3271eaa2c', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, united_states_monetary_authorities).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, creditor_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for maintaining both domestic economic growth and dollar convertibility, a structural impossibility. They face the dilemma of either sacrificing domestic goals or undermining convertibility, leading to a constant drain on gold reserves and policy credibility. Their identity is tied to managing the global reserve currency.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, united_states_monetary_authorities, payer,
    institutional, biographical, identity_locked, global).

% Accumulate dollar reserves from trade surpluses, which they can convert to gold. They face the dilemma that converting large amounts of dollars to gold would collapse the system, devaluing their remaining dollar holdings. They are trapped by the very system that benefits them in trade.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, creditor_nations, payer,
    organized, biographical, constrained, global).

% The eventual outcome of the convertibility's inherent unsustainability. This 'regime' benefits from the structural flaws of convertibility, as its emergence is necessitated by the collapse of the prior system. It is an abstract beneficiary, representing the systemic revision.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime, beneficiary,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime).

% The institution designed to oversee the Bretton Woods system. From this reading, the IMF observes the structural flaws but is unable to resolve the inherent contradiction without fundamental systemic change, which is beyond its mandate.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aimed to provide a stable international monetary system by pegging the dollar to gold and other currencies to the dollar, facilitating global trade and investment.
% TRANSFER_FUNCTION: Transferred the burden of maintaining global liquidity and confidence onto the U.S. dollar, while simultaneously transferring the risk of systemic collapse to both the U.S. and creditor nations.
% ABSENT_VOICES: Advocates for a truly multilateral reserve asset (e.g., a strengthened SDR) or a flexible exchange rate system were present but lacked the institutional power to fundamentally alter the Bretton Woods design until its collapse.
% DISAPPEARANCE_RATIONALE: The constraint did disappear, leading to the collapse of the Bretton Woods system and the transition to a floating exchange rate regime. The entire international monetary architecture rearranged itself.
% FOUNDING_PROBLEM: To prevent a return to the competitive devaluations and monetary instability of the interwar period, establishing a stable, predictable international monetary order.
% FOUNDING_PROBLEM_CORROBORATION: Economists and historians widely corroborate that the original problem of interwar monetary instability was addressed, but the solution itself created new, ultimately fatal, structural problems. The Triffin Dilemma is a well-established concept in monetary history, corroborated by numerous independent analyses.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__triffin_structural_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__triffin_structural_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__triffin_structural_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dollar_gold_convertibility__triffin_structural_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__triffin_structural_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__triffin_structural_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__triffin_structural_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the system imposed an impossible choice on the U.S. and a collective action problem on creditor nations, leading to a continuous drain on gold reserves and policy autonomy. Suppression is high because the structural design of the system left no viable exit for participants without collapsing the entire regime. Theater ratio increases over time as the U.S. engaged in increasingly performative measures to defend convertibility while the underlying structural problem worsened. The system's inherent design flaw meant that its persistence depended on suppressing the recognition of its unsustainability.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the U.S. and creditor nations, the system was a trap, forcing them into untenable positions. From the perspective of the 'post_bretton_woods_floating_regime' (an analytical construct), the prior system's collapse was a necessary, beneficial transition. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Both the U.S. monetary authorities and creditor nations are victims/payers, caught in the impossible trilemma. The 'post_bretton_woods_floating_regime' is an abstract beneficiary, representing the systemic revision that ultimately resolved the dilemma through collapse and a new order. The IMF acts as an observer, unable to alter the fundamental structural contradiction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (stable global monetary system) was undermined by its own design. The Triffin Dilemma shows that the system was structurally flawed from the outset, not that it atrophied. The classification as a snare prevents mislabeling it as a rope or scaffold, which would imply a functional coordination or temporary support that was not structurally present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    triffin_dilemma_inevitability,
    'Was the collapse of the Bretton Woods system truly inevitable due to the Triffin Dilemma, or could policy adjustments have sustained it longer?',
    'Counterfactual historical analysis comparing policy choices with outcomes, or comparative analysis with other fixed exchange rate regimes facing similar pressures.',
    'If inevitable, this reading''s ''snare'' classification is strongly reinforced. If avoidable, the ''snare'' aspect might be mitigated, suggesting more agency for policymakers and potentially reclassifying towards a ''tangled_rope'' or ''scaffold'' that failed due to poor management rather than inherent design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_dilemma_inevitability, empirical, 'Whether the structural flaw was truly deterministic or if policy flexibility could have altered the outcome.').

omega_variable(
    structural_vs_policy_failure,
    'Is the primary cause of the Bretton Woods collapse a structural design flaw (Triffin Dilemma) or a failure of U.S. and international policy coordination?',
    'Detailed historical and economic analysis disentangling the effects of structural constraints from specific policy decisions (e.g., U.S. Vietnam War spending, European non-cooperation).',
    'If structural, this ''snare'' classification is robust. If policy failure dominates, the constraint might be reclassified as a ''tangled_rope'' (coordination failure with extraction) or even a ''piton'' (inertial adherence to a failing policy).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_vs_policy_failure, conceptual, 'Distinguishing between inherent design flaws and contingent policy choices as the root cause of collapse.').

omega_variable(
    beneficiary_status_of_floating_regime,
    'Is the ''post_bretton_woods_floating_regime'' truly a beneficiary, or merely an emergent state that resolved the prior contradiction without inherent ''benefit''?',
    'Conceptual clarification of ''beneficiary'' for abstract systemic outcomes. If ''benefit'' implies active collection or advantage, then the floating regime is merely a consequence. If ''benefit'' includes resolution of a prior impossible state, then it is a beneficiary.',
    'If not a beneficiary, the constraint would lack a clear beneficiary, potentially strengthening its ''snare'' classification by removing any perceived ''upside'' to the system''s collapse.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_status_of_floating_regime, conceptual, 'Clarifying the nature of ''benefit'' for abstract systemic outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__triffin_structural_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1944, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1944, 0.2).
narrative_ontology:measurement(doll_tr_t1955, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1955, 0.35).
narrative_ontology:measurement(doll_tr_t1965, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1965, 0.5).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1971, 0.6).

% Extraction over time
narrative_ontology:measurement(doll_be_t1944, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1944, 0.6).
narrative_ontology:measurement(doll_be_t1955, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1955, 0.7).
narrative_ontology:measurement(doll_be_t1965, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1965, 0.8).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1971, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1944, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1944, 0.7).
narrative_ontology:measurement(doll_su_t1955, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1955, 0.78).
narrative_ontology:measurement(doll_su_t1965, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1965, 0.85).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1971, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__triffin_structural_reading, global_infrastructure).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'dollar_gold_convertibility' kernel. This 'triffin_structural_reading' emphasizes the inherent unsustainability of the system, leading to its inevitable collapse and the emergence of a new floating regime. It contrasts with 'strict_convertibility_reading' (binding legal obligation) and 'policy_flexible_reading' (conditional obligation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
