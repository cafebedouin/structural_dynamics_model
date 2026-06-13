% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__lapsed_alternative_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: market_as_natural_default__lapsed_alternative_reading
 *   human_readable: Market as Natural Default (Lapsed Alternative Reading)
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   This constraint describes the perceived 'naturalness' of market
 *   dominance, specifically from the 'lapsed alternative' reading. In this
 *   view, market dominance is not actively enforced or maintained by specific
 *   beneficiaries, but rather results from a historical process where viable
 *   alternatives were simply forgotten or allowed to atrophy, leading to a
 *   default acceptance of the current market structure. The constraint
 *   operates as a cognitive and historical 'mountain' where the path to
 *   alternatives has been obscured by time, rather than actively blocked.
 *   This reading implies low extractiveness and suppression, as there are no
 *   active agents enforcing the 'naturalness' or suppressing alternatives;
 *   the 'closure' is a historical artifact.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__lapsed_alternative_reading, 0.12).
domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, 0.05).
domain_priors:theater_ratio(market_as_natural_default__lapsed_alternative_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__lapsed_alternative_reading, mountain).
narrative_ontology:human_readable(market_as_natural_default__lapsed_alternative_reading, "Market as Natural Default (Lapsed Alternative Reading)").
narrative_ontology:topic_domain(market_as_natural_default__lapsed_alternative_reading, "political_economy/ideology_studies/economic_history").

domain_priors:emerges_naturally(market_as_natural_default__lapsed_alternative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__lapsed_alternative_reading, 'fb58b8ec-2788-4bcf-9df8-3246350ab823').
narrative_ontology:cs_kernel_codification('fb58b8ec-2788-4bcf-9df8-3246350ab823', implicit).
narrative_ontology:cs_authority_grounding('fb58b8ec-2788-4bcf-9df8-3246350ab823', diffuse_epistemic).
narrative_ontology:cs_reading_relation('fb58b8ec-2788-4bcf-9df8-3246350ab823', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('fb58b8ec-2788-4bcf-9df8-3246350ab823', market_as_natural_default__hybrid_amnesia_reading, coexists_with).
narrative_ontology:cs_axiom('fb58b8ec-2788-4bcf-9df8-3246350ab823', foundational, economic_alternatives_atrophied_passively).
narrative_ontology:cs_axiom_status(economic_alternatives_atrophied_passively, holdable).
narrative_ontology:cs_axiom_grounding('fb58b8ec-2788-4bcf-9df8-3246350ab823', economic_alternatives_atrophied_passively, empirically_contingent).
narrative_ontology:cs_axiom('fb58b8ec-2788-4bcf-9df8-3246350ab823', foundational, market_dominance_is_historical_artifact).
narrative_ontology:cs_axiom_status(market_dominance_is_historical_artifact, holdable).
narrative_ontology:cs_axiom_grounding('fb58b8ec-2788-4bcf-9df8-3246350ab823', market_dominance_is_historical_artifact, empirically_contingent).
narrative_ontology:cs_reference_frame('fb58b8ec-2788-4bcf-9df8-3246350ab823', historical_amnesia_as_default).
narrative_ontology:cs_drift_state('fb58b8ec-2788-4bcf-9df8-3246350ab823', contemporary_historical_research_era, gap(revival_pressure, minor, false)).
narrative_ontology:cs_created_at('fb58b8ec-2788-4bcf-9df8-3246350ab823', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_as_natural_default__lapsed_alternative_reading, market_incumbents).
narrative_ontology:constraint_victim(market_as_natural_default__lapsed_alternative_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Through research, they can uncover forgotten economic alternatives and challenge the 'naturalness' narrative, but their findings may not immediately impact policy or public perception.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, economic_historians, observer,
    analytical, generational, analytical, global).

% Accepts the market as a natural default due to historical amnesia, making them 'payers' of the cognitive cost of lost alternatives, though no direct extraction is levied. Their identity is often fused with the existing economic system, making alternatives unthinkable.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, general_public, payer,
    powerless, biographical, identity_locked, national).

% Operate within the perceived 'natural' market framework, often unaware of historically viable alternatives. Their policy choices are constrained by this default, even without active suppression.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, policymakers, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the lack of perceived alternatives, as it reduces competitive pressure. However, in this reading, they do not actively maintain the 'natural' narrative; they merely profit from its historical emergence.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, market_incumbents, beneficiary,
    powerful, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, widely accepted framework for economic activity by presenting the current market structure as the natural and inevitable default, reducing cognitive load and debate over fundamental alternatives.
% TRANSFER_FUNCTION: Transfers cognitive and political energy away from exploring and implementing alternative economic systems, effectively 'locking in' the existing market structure as the default.
% ABSENT_VOICES: Historical proponents of forgotten economic alternatives (e.g., mutualist societies, cooperative commonwealths, alternative property regimes) are absent from contemporary discourse, their arguments lost to collective memory. If present, they would challenge the 'naturalness' claim.
% DISAPPEARANCE_RATIONALE: If the perception of the market as a natural default vanished overnight, and historical alternatives were suddenly salient, it would trigger widespread re-evaluation of economic systems, policy debates, and potentially lead to the emergence of new economic models, fundamentally rearranging the political economy.
% FOUNDING_PROBLEM: The problem of establishing a stable, widely accepted framework for economic organization in complex societies, and the need to reduce the cognitive burden of constantly re-evaluating fundamental economic structures.
% FOUNDING_PROBLEM_CORROBORATION: The need for a stable economic framework remains live, attested by political scientists and sociologists who study institutional stability. However, the 'naturalness' as a solution is contested by economic historians who point to the constructed nature of market institutions. No direct corroboration from outside the benefiting parties for the 'naturalness' claim itself, only for the underlying need for stability.
narrative_ontology:disappearance_verdict(market_as_natural_default__lapsed_alternative_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__lapsed_alternative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__lapsed_alternative_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(market_as_natural_default__lapsed_alternative_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__lapsed_alternative_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, ExtMetricName, E),
    domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(market_as_natural_default__lapsed_alternative_reading),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(market_as_natural_default__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.12) and suppression (0.05) reflect the core premise of this reading: no identifiable agent actively extracts rents or suppresses alternatives. The 'naturalness' is a consequence of historical amnesia, not active coercion. Accessibility collapse is high (0.88) because the knowledge of alternatives has largely vanished from collective memory, making them effectively inaccessible. Resistance is low (0.02) because the constraint is perceived as a natural state, not a contested imposition. Theater ratio is low (0.08) as there's little performative maintenance; the 'naturalness' is largely self-sustaining through inertia.
 *
 * PERSPECTIVAL GAP:
 *   Since this reading posits no active beneficiaries or agenda-setters, there is no significant perspectival gap in terms of extraction. All agents are equally subject to the historical forgetting. However, an analytical observer, through historical research, could perceive the constructed nature of the 'mountain' and the possibility of recovering alternatives, a perspective not available to those operating within the default frame.
 *
 * DIRECTIONALITY LOGIC:
 *   In this reading, there are no direct beneficiaries or victims in the active sense. The 'market' itself, as an abstract system, appears to benefit from its default status, but no specific agent actively captures this. All agents, including market participants and policymakers, are subject to the cognitive constraint of historical forgetting, making them 'targets' of the amnesia, but not of an extractive mechanism. The 'naturalness' is a shared cognitive frame, not a mechanism of transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Mountain by virtue of its 'emerges_naturally: true' claim and low metrics, consistent with a natural law. However, the 'lapsed alternative' reading implies that this 'naturalness' is a historical artifact, not an immutable truth. If the historical amnesia were resolved (e.g., through recovery of forgotten alternatives), the constraint would likely reclassify to a Piton (if it persists purely by inertia) or even a Snare (if the recovered history reveals active suppression that was merely obscured). The current classification prevents mislabeling a historical artifact as an actively extractive mechanism, while the omegas flag the potential for reclassification upon new evidence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_ambiguity,
    'Is the perceived ''naturalness'' of market dominance a genuine emergent property of economic systems, or a constructed narrative sustained by historical forgetting?',
    'Extensive historical research to uncover and reconstruct viable, forgotten alternatives; counterfactual economic modeling.',
    'If genuinely natural, the constraint is a Mountain. If constructed by forgetting, it''s a Piton (inertial persistence of a non-functional narrative) or a Snare (if beneficiaries are found).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_vs_constructed_ambiguity, conceptual, 'Ambiguity between natural economic law and historically constructed default.').

omega_variable(
    lapsed_vs_active_closure,
    'Is the absence of alternatives due to a passive historical forgetting (lapsed alternative reading) or active, ongoing suppression by beneficiaries (beneficiary maintained reading)?',
    'Analysis of contemporary lobbying, regulatory capture, and anti-competitive practices by market incumbents. If active mechanisms are found, reclassify.',
    'If purely lapsed, extractiveness remains low. If active closure is found, extractiveness rises significantly, and the constraint reclassifies to Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lapsed_vs_active_closure, empirical, 'Distinguishing passive historical amnesia from active suppression.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''market_as_natural_default'' kernel. This ''lapsed_alternative_reading'' posits that market dominance is a result of historical forgetting. What would change if the ''beneficiary_maintained_reading'' were adopted?',
    'Empirical investigation into active lobbying, regulatory capture, and anti-competitive practices by market incumbents.',
    'If the ''beneficiary_maintained_reading'' were adopted, the constraint''s extractiveness and suppression would increase significantly, and its classification would shift from Mountain to Snare or Tangled Rope, as identifiable beneficiaries would be actively maintaining the ''natural'' narrative for their own gain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Impact of adopting the ''beneficiary_maintained_reading'' of the market_as_natural_default kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__lapsed_alternative_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mark_tr_t25, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 25, 0.06).
narrative_ontology:measurement(mark_tr_t50, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 50, 0.07).
narrative_ontology:measurement(mark_tr_t75, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 75, 0.08).
narrative_ontology:measurement(mark_tr_t100, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(mark_be_t25, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 25, 0.1).
narrative_ontology:measurement(mark_be_t50, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(mark_be_t75, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 75, 0.12).
narrative_ontology:measurement(mark_be_t100, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 100, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0, 0.03).
narrative_ontology:measurement(mark_su_t25, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 25, 0.04).
narrative_ontology:measurement(mark_su_t50, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 50, 0.05).
narrative_ontology:measurement(mark_su_t75, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 75, 0.05).
narrative_ontology:measurement(mark_su_t100, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 100, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__lapsed_alternative_reading, information_standard).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__hybrid_amnesia_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'market_as_natural_default' kernel. This 'lapsed_alternative_reading' focuses on historical forgetting as the source of perceived naturalness, leading to low extractiveness. The 'beneficiary_maintained_reading' (higher extractiveness) and 'hybrid_amnesia_reading' (intermediate extractiveness) offer alternative explanations for the same phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
