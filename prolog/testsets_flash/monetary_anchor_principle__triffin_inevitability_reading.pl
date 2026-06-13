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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: monetary_anchor_principle__triffin_inevitability_reading
 *   human_readable: Triffin Dilemma Inevitability of Gold Standard Collapse
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint describes the structural inevitability of the Bretton
 *   Woods system's collapse due to the Triffin dilemma. As the US dollar
 *   served as the global reserve currency, the US had to run persistent
 *   balance of payments deficits to supply sufficient liquidity for global
 *   trade and growth. However, these deficits simultaneously undermined
 *   confidence in the dollar's convertibility to gold, leading to a run on US
 *   gold reserves and ultimately forcing the abandonment of the gold
 *   standard. This reading frames the collapse as a logical consequence of
 *   the system's design, rather than a policy choice or a contingent event.
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
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__triffin_inevitability_reading, mountain).
narrative_ontology:human_readable(monetary_anchor_principle__triffin_inevitability_reading, "Triffin Dilemma Inevitability of Gold Standard Collapse").
narrative_ontology:topic_domain(monetary_anchor_principle__triffin_inevitability_reading, "monetary_economics/political_economy/international_finance").

domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__triffin_inevitability_reading, '4a7eb49c-2f90-478d-ad18-4a51dfd6502d').
narrative_ontology:cs_kernel_codification('4a7eb49c-2f90-478d-ad18-4a51dfd6502d', implicit).
narrative_ontology:cs_authority_grounding('4a7eb49c-2f90-478d-ad18-4a51dfd6502d', self_enforcing).
narrative_ontology:cs_reading_relation('4a7eb49c-2f90-478d-ad18-4a51dfd6502d', monetary_anchor_principle__punctuated_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a7eb49c-2f90-478d-ad18-4a51dfd6502d', monetary_anchor_principle__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('4a7eb49c-2f90-478d-ad18-4a51dfd6502d', foundational, gold_convertibility_and_global_liquidity_are_contradictory).
narrative_ontology:cs_axiom_status(gold_convertibility_and_global_liquidity_are_contradictory, holdable).
narrative_ontology:cs_axiom_grounding('4a7eb49c-2f90-478d-ad18-4a51dfd6502d', gold_convertibility_and_global_liquidity_are_contradictory, empirically_contingent).
narrative_ontology:cs_reference_frame('4a7eb49c-2f90-478d-ad18-4a51dfd6502d', triffin_dilemma_as_structural_law).
narrative_ontology:cs_drift_state('4a7eb49c-2f90-478d-ad18-4a51dfd6502d', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4a7eb49c-2f90-478d-ad18-4a51dfd6502d', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__triffin_inevitability_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(monetary_anchor_principle__triffin_inevitability_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__triffin_inevitability_reading_tests).

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
 *   The extractiveness is very low (0.05) because the dilemma is a structural, almost mathematical, contradiction, not a mechanism for rent extraction by any specific party. The 'extraction' is the system's self-consumption of its own reserves. Suppression is high (0.95) because the underlying economic laws and the demand for global liquidity were unyielding; there was no 'alternative' to the dilemma itself. Theater ratio is zero as there was no performative maintenance of a non-functional aspect; the system was genuinely functional until its inherent contradiction became critical. Accessibility collapse is high (0.9) because the alternatives to facing the dilemma (e.g., not supplying liquidity, or not maintaining gold convertibility) were either economically unfeasible or would have meant abandoning the system's core tenets. Resistance is low (0.05) because the dilemma was a systemic force, not something that could be actively resisted by any single actor within the framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Bretton Woods institutional framework, the constraint was an inescapable logical trap. From the perspective of the global economy, it was a necessary but unstable mechanism for liquidity provision. There is no significant perspectival gap in the *nature* of the constraint, only in its implications for different actors.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bretton Woods institutional framework is the primary 'victim' in the sense that it was the entity that failed due to the dilemma (d=1.0). The US Treasury, while an 'agenda setter', was structurally compelled by the dilemma, making its directionality closer to a target than a beneficiary (d=0.7). The global economy was a beneficiary of the liquidity but also subject to the instability (d=0.3).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Mountain, representing a fundamental economic principle. Mandatrophy is not applicable as it describes a structural inevitability, not a human-designed constraint that has outlived its function. The 'mandate' was to provide global liquidity under a gold standard, which was structurally impossible to sustain indefinitely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    triffin_dilemma_natural_law_vs_policy_choice,
    'Is the Triffin dilemma a ''natural law'' of international finance, or could alternative policy choices (e.g., different reserve management, earlier SDR adoption) have averted or significantly delayed the collapse?',
    'Counterfactual historical analysis and economic modeling of alternative policy paths, assessing their feasibility and likely outcomes under the prevailing conditions.',
    'If alternative policies could have averted the collapse, the constraint''s ''mountain'' classification would be weakened, suggesting a greater role for human agency and policy choices, potentially shifting it towards a ''tangled_rope'' or ''snare'' if specific actors benefited from the ''inevitability'' narrative. If no feasible alternatives existed, the mountain classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_dilemma_natural_law_vs_policy_choice, conceptual, 'Ambiguity between structural inevitability and policy contingency in the Triffin dilemma.').

omega_variable(
    causal_primacy_of_triffin_dilemma,
    'To what extent was the Triffin dilemma the *primary* cause of the Bretton Woods collapse, versus other contributing factors like the Vietnam War deficits, rising inflation, or the emergence of alternative financial centers?',
    'Detailed historical and econometric analysis disentangling the causal weight of various factors leading to the collapse, potentially using counterfactual simulations.',
    'If other factors were equally or more primary, this reading''s claim of ''inevitability'' solely due to Triffin would be weakened, potentially supporting the ''overdetermined_composite_reading'' and shifting the constraint''s classification away from a pure mountain towards a more complex, multi-causal ''tangled_rope'' or ''snare'' if specific policy choices amplified the other factors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_primacy_of_triffin_dilemma, empirical, 'The causal primacy of the Triffin dilemma versus other factors in the Bretton Woods collapse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__triffin_inevitability_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1944, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1944, 0.0).
narrative_ontology:measurement(mone_tr_t1950, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1950, 0.0).
narrative_ontology:measurement(mone_tr_t1958, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1958, 0.0).
narrative_ontology:measurement(mone_tr_t1965, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1965, 0.0).
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1971, 0.0).

% Extraction over time
narrative_ontology:measurement(mone_be_t1944, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1944, 0.01).
narrative_ontology:measurement(mone_be_t1950, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1950, 0.02).
narrative_ontology:measurement(mone_be_t1958, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1958, 0.03).
narrative_ontology:measurement(mone_be_t1965, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1965, 0.04).
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1971, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t1944, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1944, 0.95).
narrative_ontology:measurement(mone_su_t1950, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1950, 0.95).
narrative_ontology:measurement(mone_su_t1958, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1958, 0.95).
narrative_ontology:measurement(mone_su_t1965, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1965, 0.95).
narrative_ontology:measurement(mone_su_t1971, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1971, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__triffin_inevitability_reading, global_infrastructure).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'monetary_anchor_principle' kernel, focusing on the structural inevitability of the Triffin dilemma. It is linked to sibling readings that emphasize policy choice or a composite of factors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
