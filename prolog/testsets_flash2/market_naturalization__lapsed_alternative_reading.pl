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
 *   constraint_id: market_naturalization__lapsed_alternative_reading
 *   human_readable: Market Dominance as Lapsed Closure (Lapsed Alternative Reading)
 *   domain: political_economy/economic_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story represents the 'lapsed alternative' reading of
 *   market naturalization. It posits that market dominance, rather than being
 *   actively maintained by powerful incumbents, persists due to the
 *   historical atrophy of alternative market structures and the high cost of
 *   re-establishing them. The constraint is classified as a Mountain because
 *   its persistence is seen as a structural feature of the economic
 *   landscape, requiring no active enforcement or extraction beyond the
 *   inherent costs of coordination. Identifiable beneficiaries exist, but
 *   they are not seen as actively maintaining the constraint.
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
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__lapsed_alternative_reading, mountain).
narrative_ontology:human_readable(market_naturalization__lapsed_alternative_reading, "Market Dominance as Lapsed Closure (Lapsed Alternative Reading)").
narrative_ontology:topic_domain(market_naturalization__lapsed_alternative_reading, "political_economy/economic_history/institutional_analysis").

domain_priors:emerges_naturally(market_naturalization__lapsed_alternative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__lapsed_alternative_reading, '62506634-0165-45a7-affb-b9ff522b789c').
narrative_ontology:cs_kernel_codification('62506634-0165-45a7-affb-b9ff522b789c', implicit).
narrative_ontology:cs_authority_grounding('62506634-0165-45a7-affb-b9ff522b789c', diffuse_epistemic).
narrative_ontology:cs_reading_relation('62506634-0165-45a7-affb-b9ff522b789c', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('62506634-0165-45a7-affb-b9ff522b789c', market_naturalization__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('62506634-0165-45a7-affb-b9ff522b789c', foundational, market_alternatives_atrophy_naturally).
narrative_ontology:cs_axiom_status(market_alternatives_atrophy_naturally, holdable).
narrative_ontology:cs_axiom_grounding('62506634-0165-45a7-affb-b9ff522b789c', market_alternatives_atrophy_naturally, empirically_contingent).
narrative_ontology:cs_axiom('62506634-0165-45a7-affb-b9ff522b789c', foundational, incumbents_do_not_actively_suppress).
narrative_ontology:cs_axiom_status(incumbents_do_not_actively_suppress, holdable).
narrative_ontology:cs_axiom_grounding('62506634-0165-45a7-affb-b9ff522b789c', incumbents_do_not_actively_suppress, empirically_contingent).
narrative_ontology:cs_reference_frame('62506634-0165-45a7-affb-b9ff522b789c', unfettered_market_evolution).
narrative_ontology:cs_drift_state('62506634-0165-45a7-affb-b9ff522b789c', contemporary_economic_analysis, gap(stable, minor, true)).
narrative_ontology:cs_created_at('62506634-0165-45a7-affb-b9ff522b789c', '').
narrative_ontology:cs_kernel_id(market_naturalization__lapsed_alternative_reading, market_naturalization).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_naturalization__lapsed_alternative_reading, market_incumbents).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, potential_entrants).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These firms benefit from the absence of viable alternatives, but do not actively maintain the closure. Their advantage is a legacy effect of past market dynamics, not current enforcement.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, market_incumbents, beneficiary,
    powerful, biographical, arbitrage, national).

% Face high barriers to entry due to the atrophy of alternative market structures, not due to active suppression by incumbents. The cost of re-establishing alternatives is prohibitive.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, potential_entrants, payer,
    moderate, immediate, constrained, national).

% Experience limited choice and potentially higher prices due to lack of competition, but perceive this as the natural state of the market rather than an actively enforced constraint.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, consumers, payer,
    powerless, immediate, constrained, local).

% Analyze the historical processes that led to the current market structure, focusing on the decay of alternatives rather than active maintenance. Their analysis informs the 'lapsed alternative' reading.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, economic_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The market structure coordinates supply and demand within established channels, with minimal friction due to the absence of competing alternatives.
% TRANSFER_FUNCTION: Transfers economic value from consumers and potential entrants to market incumbents, primarily through the absence of competitive pressure rather than direct extraction.
% ABSENT_VOICES: Entrepreneurs with innovative but non-standard business models, and advocates for market decentralization, are absent from the policy discourse, as the current structure is widely perceived as inevitable.
% DISAPPEARANCE_RATIONALE: If the 'lapsed closure' vanished, new market structures and competitive alternatives would emerge, fundamentally altering the economic landscape and incumbent positions.
% FOUNDING_PROBLEM: The problem of efficient resource allocation and capital formation in a nascent industrial economy.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream economists and business leaders attest that efficient allocation remains a live problem, and the current market structure is a functional (if imperfect) solution. Critical economists and historians, however, contest the 'efficiency' claim, arguing it masks historical power dynamics.
narrative_ontology:disappearance_verdict(market_naturalization__lapsed_alternative_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__lapsed_alternative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__lapsed_alternative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The low extractiveness (0.15) reflects that the 'lapsed alternative' reading attributes costs primarily to coordination and the inherent friction of market entry, not active rent-seeking. Suppression (0.25) is low because it refers to the absence of viable alternatives, not active coercion. Theater ratio is negligible (0.05) as there's little performative maintenance. Accessibility collapse is high (0.85) because alternatives have genuinely atrophied. Resistance is low (0.1) because the constraint is perceived as natural or inevitable.
 *
 * PERSPECTIVAL GAP:
 *   The key perspectival gap is between those who see market dominance as a natural outcome of efficiency (this reading) and those who see it as actively maintained extraction. This story focuses on the former, where the constraint's persistence is due to the 'natural' decay of alternatives, not active defense.
 *
 * DIRECTIONALITY LOGIC:
 *   Market incumbents are beneficiaries (d near 0.0) as they profit from the lack of competition, but they are not seen as actively creating or maintaining the constraint. Potential entrants and consumers are payers (d near 1.0) as they bear the costs of limited choice and high entry barriers. Economic historians are observers (d near 0.5) analyzing the structural dynamics.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_maintenance_vs_lapsed_closure,
    'Is the observed market dominance truly a lapsed closure, or is there subtle, active maintenance by incumbents that this reading overlooks?',
    'Detailed forensic economic analysis of lobbying efforts, regulatory capture, and strategic pricing/acquisition patterns by incumbents over time. If active maintenance is found, reclassify towards ''beneficiary_maintained_reading''.',
    'If active maintenance is significant, the constraint''s extractiveness and suppression would be higher, shifting its classification towards a Snare or Tangled Rope. If truly lapsed, the Mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_maintenance_vs_lapsed_closure, empirical, 'Distinguishing between passive benefit from lapsed alternatives and active, subtle maintenance of market dominance.').

omega_variable(
    natural_vs_constructed_atrophy,
    'To what extent was the atrophy of alternatives a ''natural'' market process, versus a consequence of past policy choices or institutional design that favored incumbents?',
    'Historical institutional analysis tracing the policy and regulatory evolution of the market, identifying critical junctures where alternatives were foreclosed by design rather than market forces alone.',
    'If atrophy was largely policy-driven, the ''emerges_naturally'' claim would be weakened, and the constraint would be reclassified as a constructed constraint (e.g., a Piton or Snare) rather than a Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_atrophy, conceptual, 'Ambiguity between natural market evolution and policy-induced atrophy of alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__lapsed_alternative_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(mark_be_t1980, market_naturalization__lapsed_alternative_reading, base_extractiveness, 1980, 0.1).
narrative_ontology:measurement(mark_be_t1990, market_naturalization__lapsed_alternative_reading, base_extractiveness, 1990, 0.12).
narrative_ontology:measurement(mark_be_t2000, market_naturalization__lapsed_alternative_reading, base_extractiveness, 2000, 0.13).
narrative_ontology:measurement(mark_be_t2010, market_naturalization__lapsed_alternative_reading, base_extractiveness, 2010, 0.14).
narrative_ontology:measurement(mark_be_t2020, market_naturalization__lapsed_alternative_reading, base_extractiveness, 2020, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t1980, market_naturalization__lapsed_alternative_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(mark_su_t1990, market_naturalization__lapsed_alternative_reading, suppression_requirement, 1990, 0.28).
narrative_ontology:measurement(mark_su_t2000, market_naturalization__lapsed_alternative_reading, suppression_requirement, 2000, 0.26).
narrative_ontology:measurement(mark_su_t2010, market_naturalization__lapsed_alternative_reading, suppression_requirement, 2010, 0.25).
narrative_ontology:measurement(mark_su_t2020, market_naturalization__lapsed_alternative_reading, suppression_requirement, 2020, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__lapsed_alternative_reading, resource_allocation).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'market_naturalization' kernel. This 'lapsed alternative' reading emphasizes the decay of alternatives rather than active maintenance, leading to a Mountain classification. The other readings (beneficiary_maintained_reading, hybrid_reading) offer different causal accounts and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
