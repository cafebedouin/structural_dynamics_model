% ============================================================================
% CONSTRAINT STORY: market_naturalization__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__lapsed_alternative_reading, []).

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
 *   constraint_id: market_naturalization__lapsed_alternative_reading
 *   human_readable: Market Dominance as Lapsed Closure (Lapsed Alternative Reading)
 *   domain: political_economy/economic_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story represents the 'lapsed alternative' reading of
 *   market naturalization, where market dominance is understood not as
 *   actively maintained or extractive, but as a historical outcome where
 *   alternatives have atrophied through non-use and institutional inertia.
 *   The market structure persists because alternatives are no longer viable
 *   or remembered, rather than being actively suppressed. This reading claims
 *   the constraint is a Mountain, reflecting its perceived naturalness and
 *   lack of active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__lapsed_alternative_reading, 0.15).
domain_priors:suppression_score(market_naturalization__lapsed_alternative_reading, 0.25).
domain_priors:theater_ratio(market_naturalization__lapsed_alternative_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__lapsed_alternative_reading, mountain).
narrative_ontology:human_readable(market_naturalization__lapsed_alternative_reading, "Market Dominance as Lapsed Closure (Lapsed Alternative Reading)").
narrative_ontology:topic_domain(market_naturalization__lapsed_alternative_reading, "political_economy/economic_history/institutional_analysis").

domain_priors:emerges_naturally(market_naturalization__lapsed_alternative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__lapsed_alternative_reading, '4f6ee6a3-0ab8-4d92-bb65-d989f2eb6f76').
narrative_ontology:cs_kernel_codification('4f6ee6a3-0ab8-4d92-bb65-d989f2eb6f76', implicit).
narrative_ontology:cs_authority_grounding('4f6ee6a3-0ab8-4d92-bb65-d989f2eb6f76', practice).
narrative_ontology:cs_interpretation_layer_present('4f6ee6a3-0ab8-4d92-bb65-d989f2eb6f76').
narrative_ontology:cs_reading_relation('4f6ee6a3-0ab8-4d92-bb65-d989f2eb6f76', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('4f6ee6a3-0ab8-4d92-bb65-d989f2eb6f76', market_naturalization__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('4f6ee6a3-0ab8-4d92-bb65-d989f2eb6f76', foundational, alternatives_atrophy_through_non_use).
narrative_ontology:cs_axiom_status(alternatives_atrophy_through_non_use, holdable).
narrative_ontology:cs_axiom_grounding('4f6ee6a3-0ab8-4d92-bb65-d989f2eb6f76', alternatives_atrophy_through_non_use, empirically_contingent).
narrative_ontology:cs_axiom('4f6ee6a3-0ab8-4d92-bb65-d989f2eb6f76', foundational, market_dominance_is_not_actively_defended).
narrative_ontology:cs_axiom_status(market_dominance_is_not_actively_defended, holdable).
narrative_ontology:cs_axiom_grounding('4f6ee6a3-0ab8-4d92-bb65-d989f2eb6f76', market_dominance_is_not_actively_defended, empirically_contingent).
narrative_ontology:cs_reference_frame('4f6ee6a3-0ab8-4d92-bb65-d989f2eb6f76', historical_contingency_of_market_forms).
narrative_ontology:cs_drift_state('4f6ee6a3-0ab8-4d92-bb65-d989f2eb6f76', contemporary_economic_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4f6ee6a3-0ab8-4d92-bb65-d989f2eb6f76', '').
narrative_ontology:cs_kernel_id(market_naturalization__lapsed_alternative_reading, market_naturalization).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, contemporary_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Represent the range of non-market or alternative market structures that once existed but have atrophied due to lack of use or institutional support, rather than active suppression. Their 'exclusion' is a historical fact of non-persistence, not active enforcement.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, historical_alternatives, excluded,
    powerless, generational, trapped, global).

% Operate within the dominant market structure, experiencing it as the natural state of affairs. They bear diffuse costs of limited choice but perceive no active extraction or suppression, as alternatives are simply absent, not suppressed.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, contemporary_consumers, payer,
    moderate, biographical, constrained, national).

% Analyze the historical processes by which market dominance became naturalized, focusing on the atrophy of alternatives rather than active defense by incumbents. They see the 'lapsed closure' as an empirical outcome of historical contingency.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, economic_historians, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint implicitly coordinates economic activity by presenting a single, dominant market structure as the default, reducing search costs and transaction friction for participants who accept it.
% TRANSFER_FUNCTION: Diffusely transfers the benefits of reduced complexity and established infrastructure to all market participants, while also implicitly transferring the costs of foregone alternatives and limited choice to consumers, without a clear, identifiable recipient of 'extraction'.
% ABSENT_VOICES: The voices of historical proponents of alternative economic systems or non-market arrangements are absent, as their institutional and social bases have atrophied. They would argue for the possibility and benefits of diverse economic structures.
% DISAPPEARANCE_RATIONALE: If the 'lapsed closure' aspect of market dominance vanished overnight, the market would largely continue as is, because the alternatives have already atrophied and are not actively suppressed. The perception of naturalness might shift, but the underlying structure would remain due to inertia and the absence of viable, organized alternatives.
% FOUNDING_PROBLEM: The problem of economic fragmentation and inefficiency, where a lack of dominant structures made large-scale coordination difficult and costly.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians corroborate that the initial problem of fragmentation was largely overcome. However, they also attest that the current state of 'lapsed closure' is a historical outcome, not a continuously 'live' problem requiring active maintenance, a view often contested by those who benefit from the current market structure.
narrative_ontology:disappearance_verdict(market_naturalization__lapsed_alternative_reading, world_unchanged).
narrative_ontology:founding_problem_status(market_naturalization__lapsed_alternative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__lapsed_alternative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(market_naturalization__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__lapsed_alternative_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__lapsed_alternative_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, ExtMetricName, E),
    domain_priors:suppression_score(market_naturalization__lapsed_alternative_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(market_naturalization__lapsed_alternative_reading),
    narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(market_naturalization__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because this reading posits no active, concentrated extraction, only diffuse coordination costs. Suppression is also low (0.25) as alternatives are not actively suppressed but have simply atrophied. Theater ratio is negligible (0.05) as there's little performative maintenance. Accessibility collapse is high (0.80) because alternatives are genuinely difficult to access or imagine. Resistance is low (0.10) because the constraint is perceived as natural or inevitable. The claimed type is Mountain, reflecting the 'naturalized' aspect of this reading, where the market structure is seen as an unchangeable feature of the economic landscape.
 *
 * PERSPECTIVAL GAP:
 *   The key perspectival gap is between this 'lapsed alternative' reading and those that posit active maintenance or extraction. While this reading sees market dominance as a naturalized outcome, other readings (e.g., 'beneficiary_maintained_reading') would see clear beneficiaries and active enforcement, leading to very different classifications. The engine's computation of per-seat types would highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   From this reading, there are no clear beneficiaries actively extracting, and victims bear diffuse costs of limited choice rather than targeted extraction. Historical alternatives are 'excluded' by historical process, not active agents. Economic historians, as observers, see the structure as a historical artifact. This leads to a low directionality for all parties, as no one is seen as actively subsidizing or being targeted by a coercive mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a historical atrophy as active extraction. If the constraint were classified as a Snare or Tangled Rope, it would imply active, ongoing coercion and identifiable beneficiaries, which this reading explicitly denies. By classifying it as a Mountain (or a Piton if the 'naturalness' is challenged), it correctly identifies the persistence mechanism as inertia and the absence of alternatives, rather than active rent-seeking. The 'dead' status of the founding problem, combined with 'world_unchanged' for disappearance, supports a Piton-like inertia, even if claimed as a Mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_vs_lapsed_maintenance,
    'Is market dominance truly a lapsed closure requiring no active maintenance, or is there subtle, ongoing maintenance by incumbent capital holders that this reading overlooks?',
    'Detailed institutional analysis of lobbying efforts, regulatory capture, and strategic investments by dominant firms over time, specifically looking for actions that suppress nascent alternatives or reinforce existing structures.',
    'If active maintenance is found, the constraint would shift from a Mountain (or Piton) to a Tangled Rope or Snare, with higher extractiveness and identifiable beneficiaries. This would align it more closely with the ''beneficiary_maintained_reading''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_vs_lapsed_maintenance, empirical, 'Distinguishing between historical atrophy and subtle, ongoing active maintenance of market dominance.').

omega_variable(
    naturalness_vs_contingency,
    'Is the perceived ''naturalness'' of market dominance an objective feature of economic reality, or a socially constructed narrative that obscures its historical contingency and the possibility of alternatives?',
    'Comparative historical analysis of different economic systems and their evolution, as well as critical discourse analysis of how market dominance is framed in public and academic discourse. This would involve examining the ''founding problem corroboration'' more deeply.',
    'If it''s a constructed narrative, the ''emerges_naturally'' claim would be false, and the constraint would be reclassified away from Mountain, likely towards a Piton (if no active beneficiaries) or a Snare (if beneficiaries are identified). This would align with the ''hybrid_reading'' or ''beneficiary_maintained_reading'' depending on the degree of active maintenance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturalness_vs_contingency, conceptual, 'The conceptual distinction between natural economic laws and historically contingent, socially constructed market structures.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (low in this reading) structural (external barriers) or internalized (cognitive patterns that persist after barrier removal)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., if new alternatives are introduced but fail to gain traction due to ingrained habits or beliefs), reclassify as partially internalized. In this case, it would mean the ''lapsed'' nature is partly due to internalized acceptance.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. This would make the ''lapsed'' state more robust and harder to dislodge, even without active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the context of atrophied alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__lapsed_alternative_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(mark_be_t1950, market_naturalization__lapsed_alternative_reading, base_extractiveness, 1950, 0.2).
narrative_ontology:measurement(mark_be_t1965, market_naturalization__lapsed_alternative_reading, base_extractiveness, 1965, 0.18).
narrative_ontology:measurement(mark_be_t1980, market_naturalization__lapsed_alternative_reading, base_extractiveness, 1980, 0.15).
narrative_ontology:measurement(mark_be_t1995, market_naturalization__lapsed_alternative_reading, base_extractiveness, 1995, 0.12).
narrative_ontology:measurement(mark_be_t2010, market_naturalization__lapsed_alternative_reading, base_extractiveness, 2010, 0.1).
narrative_ontology:measurement(mark_be_t2020, market_naturalization__lapsed_alternative_reading, base_extractiveness, 2020, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t1950, market_naturalization__lapsed_alternative_reading, suppression_requirement, 1950, 0.3).
narrative_ontology:measurement(mark_su_t1965, market_naturalization__lapsed_alternative_reading, suppression_requirement, 1965, 0.28).
narrative_ontology:measurement(mark_su_t1980, market_naturalization__lapsed_alternative_reading, suppression_requirement, 1980, 0.25).
narrative_ontology:measurement(mark_su_t1995, market_naturalization__lapsed_alternative_reading, suppression_requirement, 1995, 0.2).
narrative_ontology:measurement(mark_su_t2010, market_naturalization__lapsed_alternative_reading, suppression_requirement, 2010, 0.18).
narrative_ontology:measurement(mark_su_t2020, market_naturalization__lapsed_alternative_reading, suppression_requirement, 2020, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__lapsed_alternative_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This is one of three readings of the 'market_naturalization' kernel. This 'lapsed_alternative_reading' focuses on the atrophy of alternatives, contrasting with 'beneficiary_maintained_reading' (active defense) and 'hybrid_reading' (combination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
