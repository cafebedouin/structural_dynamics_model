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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: market_naturalization__lapsed_alternative_reading
 *   human_readable: Market Dominance as Lapsed Closure (Lapsed Alternative Reading)
 *   domain: political_economy/institutional_analysis
 *
 * SUMMARY:
 *   This constraint models market dominance as a 'lapsed closure' – a
 *   historical outcome where active suppression of alternatives has ceased,
 *   but the alternatives themselves have atrophied to the point where the
 *   market appears naturally dominant. This reading emphasizes the inertial
 *   and historical aspects, rather than ongoing active maintenance by
 *   beneficiaries. It is one reading of the 'market_naturalization' kernel,
 *   contrasting with views that emphasize active maintenance or hybrid
 *   mechanisms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__lapsed_alternative_reading, 0.15).
domain_priors:suppression_score(market_naturalization__lapsed_alternative_reading, 0.2).
domain_priors:theater_ratio(market_naturalization__lapsed_alternative_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__lapsed_alternative_reading, mountain).
narrative_ontology:human_readable(market_naturalization__lapsed_alternative_reading, "Market Dominance as Lapsed Closure (Lapsed Alternative Reading)").
narrative_ontology:topic_domain(market_naturalization__lapsed_alternative_reading, "political_economy/institutional_analysis").

domain_priors:emerges_naturally(market_naturalization__lapsed_alternative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__lapsed_alternative_reading, '58b38c8d-4084-4fff-b12d-c95db109a128').
narrative_ontology:cs_kernel_codification('58b38c8d-4084-4fff-b12d-c95db109a128', implicit).
narrative_ontology:cs_authority_grounding('58b38c8d-4084-4fff-b12d-c95db109a128', practice).
narrative_ontology:cs_reading_relation('58b38c8d-4084-4fff-b12d-c95db109a128', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('58b38c8d-4084-4fff-b12d-c95db109a128', market_naturalization__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('58b38c8d-4084-4fff-b12d-c95db109a128', foundational, market_dominance_is_inertial).
narrative_ontology:cs_axiom_status(market_dominance_is_inertial, holdable).
narrative_ontology:cs_axiom_grounding('58b38c8d-4084-4fff-b12d-c95db109a128', market_dominance_is_inertial, empirically_contingent).
narrative_ontology:cs_axiom('58b38c8d-4084-4fff-b12d-c95db109a128', foundational, alternatives_atrophy_without_active_defense).
narrative_ontology:cs_axiom_status(alternatives_atrophy_without_active_defense, holdable).
narrative_ontology:cs_axiom_grounding('58b38c8d-4084-4fff-b12d-c95db109a128', alternatives_atrophy_without_active_defense, empirically_contingent).
narrative_ontology:cs_reference_frame('58b38c8d-4084-4fff-b12d-c95db109a128', uncontested_market_equilibrium).
narrative_ontology:cs_drift_state('58b38c8d-4084-4fff-b12d-c95db109a128', contemporary_regulatory_scrutiny, gap(stable, minor, false)).
narrative_ontology:cs_created_at('58b38c8d-4084-4fff-b12d-c95db109a128', '').
narrative_ontology:cs_kernel_id(market_naturalization__lapsed_alternative_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__lapsed_alternative_reading, incumbent_firms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, potential_entrants).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These firms benefit from the lack of viable alternatives, which allows them to maintain market share and pricing power without significant active effort. They perceive their position as a natural outcome of market forces.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, incumbent_firms, beneficiary,
    institutional, generational, arbitrage, global).

% Face insurmountable barriers to entry due to the atrophy of alternative market structures and distribution channels. They bear the cost of a market that appears 'natural' but is actually a historical artifact.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, potential_entrants, payer,
    powerless, biographical, trapped, national).

% Experience limited choice and potentially higher prices due to the lack of competition. Their options are constrained by the existing market structure, which offers few alternatives.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, consumers, payer,
    moderate, immediate, constrained, local).

% Analyze the historical processes that led to the current market structure, identifying periods of active closure that have since lapsed into passive dominance. They see the 'naturalness' as a historical contingency.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, economic_historians, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates market activity by providing a stable, predictable environment for incumbent firms, reducing transaction costs associated with competition and innovation for them.
% TRANSFER_FUNCTION: Transfers potential innovation and competitive pricing from potential entrants and consumers to incumbent firms, by virtue of the historical atrophy of alternatives.
% ABSENT_VOICES: The voices of historical alternative market structures and suppressed innovations are absent; they would argue that the current market is a contingent outcome, not an inevitable one, and that active intervention could revive competition.
% DISAPPEARANCE_RATIONALE: If the 'lapsed closure' vanished, the market would immediately become contestable. New entrants would emerge, pricing would adjust, and the incumbent firms would face genuine competition, fundamentally altering the economic landscape.
% FOUNDING_PROBLEM: The original problem was to establish stable market structures for efficient resource allocation and production, which historically involved active efforts to consolidate and rationalize industries.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians corroborate that the initial problem of establishing stable markets was largely solved, and the current dominance is a legacy effect. Incumbent firms, however, claim the problem of market instability is 'live' to justify their position. Independent regulatory bodies often find the problem 'dead' in their assessments.
narrative_ontology:disappearance_verdict(market_naturalization__lapsed_alternative_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__lapsed_alternative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__lapsed_alternative_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(market_naturalization__lapsed_alternative_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__lapsed_alternative_reading_tests).

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
 *   Extractiveness is low (0.15) because no active rent-seeking is required; the 'extraction' is merely the benefit of a non-contestable market. Suppression is also low (0.20) and declining, reflecting the 'lapsed' nature – alternatives are gone, not actively suppressed. Theater ratio is very low (0.05) as there's little performative maintenance. Accessibility collapse is high (0.85) because alternatives have genuinely atrophied. Resistance is low (0.10) because the market appears 'natural' and the costs of challenging it are prohibitive.
 *
 * PERSPECTIVAL GAP:
 *   Incumbent firms perceive their market position as a natural outcome of efficiency and consumer choice, consistent with a Mountain. Potential entrants and consumers, however, experience it as a structural barrier, even if not actively enforced, leading to a Snare-like experience. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent firms are beneficiaries (d=0.0) as they profit from the lack of competition. Potential entrants and consumers are payers (d=1.0 and d=0.8 respectively) as they bear the costs of limited choice and lack of market access. Economic historians are observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (establishing stable markets) is 'dead' in the sense that the active phase of market formation is over. However, the 'lapsed closure' persists, creating a situation where the market appears natural but is a historical artifact. This prevents mislabeling it as a Snare (which implies active, ongoing extraction) or a pure Mountain (which implies inherent, unchangeable natural law).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lapsed_vs_active_maintenance,
    'To what extent is the market dominance truly a ''lapsed closure'' versus subtly maintained by incumbent firms through non-obvious mechanisms (e.g., lobbying, standard-setting, network effects)?',
    'Detailed forensic economic analysis of incumbent firm expenditures on market defense, regulatory capture, and intellectual property enforcement over time. Longitudinal studies of market entry and exit patterns.',
    'If significant active maintenance is found, the constraint would shift towards a ''beneficiary_maintained_reading'' or ''hybrid_reading'', increasing extractiveness and suppression, potentially reclassifying as a Tangled Rope or Snare. If truly lapsed, the Mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lapsed_vs_active_maintenance, empirical, 'Distinguishing between passive historical dominance and subtle active maintenance.').

omega_variable(
    natural_vs_contingent_market,
    'Is the current market structure an inevitable outcome of economic forces (natural law), or a contingent historical artifact that could be altered by policy interventions?',
    'Comparative analysis with other jurisdictions that implemented different regulatory or antitrust policies, observing their market structures. Counterfactual historical analysis.',
    'If found to be contingent, the ''emerges_naturally'' claim would be challenged, potentially reclassifying the constraint away from Mountain, even if extractiveness remains low. This would open pathways for policy intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_contingent_market, conceptual, 'The conceptual boundary between natural market outcomes and historically contingent structures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__lapsed_alternative_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t1980, market_naturalization__lapsed_alternative_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(mark_tr_t1990, market_naturalization__lapsed_alternative_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(mark_tr_t2000, market_naturalization__lapsed_alternative_reading, theater_ratio, 2000, 0.07).
narrative_ontology:measurement(mark_tr_t2010, market_naturalization__lapsed_alternative_reading, theater_ratio, 2010, 0.06).
narrative_ontology:measurement(mark_tr_t2020, market_naturalization__lapsed_alternative_reading, theater_ratio, 2020, 0.05).

% Extraction over time
narrative_ontology:measurement(mark_be_t1980, market_naturalization__lapsed_alternative_reading, base_extractiveness, 1980, 0.1).
narrative_ontology:measurement(mark_be_t1990, market_naturalization__lapsed_alternative_reading, base_extractiveness, 1990, 0.12).
narrative_ontology:measurement(mark_be_t2000, market_naturalization__lapsed_alternative_reading, base_extractiveness, 2000, 0.13).
narrative_ontology:measurement(mark_be_t2010, market_naturalization__lapsed_alternative_reading, base_extractiveness, 2010, 0.14).
narrative_ontology:measurement(mark_be_t2020, market_naturalization__lapsed_alternative_reading, base_extractiveness, 2020, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t1980, market_naturalization__lapsed_alternative_reading, suppression_requirement, 1980, 0.25).
narrative_ontology:measurement(mark_su_t1990, market_naturalization__lapsed_alternative_reading, suppression_requirement, 1990, 0.22).
narrative_ontology:measurement(mark_su_t2000, market_naturalization__lapsed_alternative_reading, suppression_requirement, 2000, 0.21).
narrative_ontology:measurement(mark_su_t2010, market_naturalization__lapsed_alternative_reading, suppression_requirement, 2010, 0.2).
narrative_ontology:measurement(mark_su_t2020, market_naturalization__lapsed_alternative_reading, suppression_requirement, 2020, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__lapsed_alternative_reading, resource_allocation).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'market_naturalization' kernel, focusing on the 'lapsed closure' aspect. It is linked to sibling readings that emphasize active maintenance or hybrid mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
