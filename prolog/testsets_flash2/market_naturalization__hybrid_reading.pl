% ============================================================================
% CONSTRAINT STORY: market_naturalization__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__hybrid_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: market_naturalization__hybrid_reading
 *   human_readable: Market Naturalization (Hybrid Reading)
 *   domain: political_economy/economic_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint describes market dominance as a hybrid phenomenon,
 *   combining elements of historical closures (lapsed alternatives) with
 *   ongoing active maintenance (suppression of new entrants). It is a reading
 *   of the 'market_naturalization' kernel, distinct from readings that
 *   emphasize only active maintenance or only lapsed alternatives. The
 *   claimed type is Tangled Rope, reflecting both a coordination function
 *   (market stability) and asymmetric extraction (from new entrants and
 *   consumers to incumbents).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__hybrid_reading, 0.65).
domain_priors:suppression_score(market_naturalization__hybrid_reading, 0.7).
domain_priors:theater_ratio(market_naturalization__hybrid_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__hybrid_reading, "Market Naturalization (Hybrid Reading)").
narrative_ontology:topic_domain(market_naturalization__hybrid_reading, "political_economy/economic_history/institutional_analysis").

domain_priors:requires_active_enforcement(market_naturalization__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__hybrid_reading, '363669d3-76b2-426c-ad0e-8da826a3991a').
narrative_ontology:cs_kernel_codification('363669d3-76b2-426c-ad0e-8da826a3991a', implicit).
narrative_ontology:cs_authority_grounding('363669d3-76b2-426c-ad0e-8da826a3991a', extraction).
narrative_ontology:cs_interpretation_layer_present('363669d3-76b2-426c-ad0e-8da826a3991a').
narrative_ontology:cs_reading_relation('363669d3-76b2-426c-ad0e-8da826a3991a', market_naturalization__lapsed_alternative_reading, influences).
narrative_ontology:cs_reading_relation('363669d3-76b2-426c-ad0e-8da826a3991a', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_axiom('363669d3-76b2-426c-ad0e-8da826a3991a', foundational, market_dominance_is_historically_contingent_and_actively_maintained).
narrative_ontology:cs_axiom_status(market_dominance_is_historically_contingent_and_actively_maintained, holdable).
narrative_ontology:cs_axiom_grounding('363669d3-76b2-426c-ad0e-8da826a3991a', market_dominance_is_historically_contingent_and_actively_maintained, empirically_contingent).
narrative_ontology:cs_reference_frame('363669d3-76b2-426c-ad0e-8da826a3991a', efficient_market_hypothesis_as_natural_state).
narrative_ontology:cs_drift_state('363669d3-76b2-426c-ad0e-8da826a3991a', contemporary_institutional_analysis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('363669d3-76b2-426c-ad0e-8da826a3991a', '').
narrative_ontology:cs_kernel_id(market_naturalization__hybrid_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, incumbent_capital_holders).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, regulatory_agencies).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, new_market_entrants).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the existing market structure, which they actively maintain through lobbying, strategic acquisitions, and influencing regulatory capture. They frame the market as naturally efficient.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, incumbent_capital_holders, agenda_setter,
    institutional, generational, arbitrage, national).

% Administer regulations that, while ostensibly neutral, often reinforce incumbent advantages due to regulatory capture or path dependence. They benefit from the stability of the existing market structure and the perceived 'naturalness' of its outcomes.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, regulatory_agencies, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(market_naturalization__hybrid_reading, regulatory_agencies, agenda_setter).

% Face significant barriers to entry due to the combined effect of historical market closures and ongoing active maintenance by incumbents. They bear the costs of navigating a market designed to favor existing players.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, new_market_entrants, payer,
    moderate, immediate, constrained, local).

% Experience reduced choice and higher prices due to limited competition. Their ability to exit is constrained by the lack of viable alternatives in a market dominated by a few players.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, consumers, payer,
    powerless, biographical, constrained, national).

% Analyze the historical evolution of market structures, identifying periods of active intervention, institutional inertia, and the naturalization of constructed arrangements. They provide an analytical perspective on the constraint's hybrid nature.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, economic_historians, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable market environment for incumbent firms, allowing for long-term investment and planning, while also offering a framework for regulatory oversight.
% TRANSFER_FUNCTION: Transfers economic rents from new entrants and consumers to incumbent capital holders and, indirectly, to regulatory agencies that benefit from the stability of the existing order.
% ABSENT_VOICES: Potential disruptors and innovators whose market entry is foreclosed by the hybrid mechanisms of lapsed alternatives and active suppression. They would advocate for deregulation or targeted anti-monopoly interventions.
% DISAPPEARANCE_RATIONALE: If the hybrid mechanisms of market naturalization vanished, new entrants would flood the market, competition would intensify, prices would fall, and incumbent firms would face significant disruption, leading to a fundamental reorganization of the economic landscape.
% FOUNDING_PROBLEM: To establish stable economic order, prevent destructive competition, and ensure predictable returns on investment for large-scale capital.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent capital holders and some regulatory bodies attest that market stability and order remain live problems. Economic historians and new market entrants argue that while stability is a valid goal, the current mechanisms primarily serve incumbent interests, with independent economic analysis supporting this view.
narrative_ontology:disappearance_verdict(market_naturalization__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(market_naturalization__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__hybrid_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_naturalization__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_naturalization__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is moderate because the market does provide some coordination benefits, but the costs are disproportionately borne by new entrants and consumers. Suppression (0.70) is high due to the combination of regulatory barriers, incumbent lobbying, and the sheer capital requirements to challenge established players. Theater ratio (0.40) reflects that while some market functions are genuinely efficient, a significant portion of activity is performative maintenance of incumbent advantage. The cyclical pattern in measurements reflects periods of increased regulatory scrutiny or attempted market entry, followed by incumbent responses that re-establish dominance.
 *
 * PERSPECTIVAL GAP:
 *   Incumbents perceive the market as a naturally evolved, efficient system requiring minimal intervention, while new entrants and consumers experience it as a rigged game. This hybrid reading acknowledges both the historical inertia and the active, often subtle, mechanisms of maintenance.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent capital holders and regulatory agencies are beneficiaries (low directionality) as they profit from or maintain the existing structure. New market entrants and consumers are targets (high directionality) as they bear the costs of limited competition and suppressed alternatives. Economic historians serve as analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   This hybrid classification prevents mislabeling the constraint as a pure Snare (ignoring the lapsed elements and coordination function) or a pure Piton (ignoring the active maintenance). It highlights that the mandate for 'market stability' has partially atrophied into a justification for rent extraction, but is still actively defended, making it a Tangled Rope rather than a fully inert Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balance_of_lapsed_vs_active,
    'What is the precise balance between ''lapsed alternatives'' (historical closures requiring no active maintenance) and ''active maintenance'' (ongoing suppression of new entrants) in sustaining market dominance?',
    'Detailed historical case studies comparing periods of regulatory change with market entry rates, and econometric analysis isolating the impact of specific incumbent actions versus structural inertia.',
    'If lapsed elements dominate, the constraint leans towards Piton or a less extractive Rope; if active maintenance dominates, it leans towards Snare. This would shift the balance of coordination vs. extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balance_of_lapsed_vs_active, empirical, 'Determining the relative contribution of historical inertia versus active suppression to market dominance.').

omega_variable(
    regulatory_capture_degree,
    'To what extent are regulatory agencies genuinely coordinating market functions versus being captured by incumbent interests?',
    'Analysis of revolving door phenomena, lobbying expenditures vs. regulatory outcomes, and the independence of regulatory decision-making from industry influence.',
    'Higher capture would increase the effective extractiveness and suppression attributed to the ''regulatory agencies'' seat, pushing the overall constraint closer to a Snare. Lower capture would reinforce the coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_degree, empirical, 'Assessing the degree of regulatory capture influencing market naturalization.').

omega_variable(
    kernel_framing_ambiguity,
    'Is this constraint primarily a ''market naturalization'' phenomenon, or is it better framed as ''incumbent power consolidation''?',
    'Conceptual analysis of the primary causal drivers: if the ''naturalness'' narrative is merely a cover for power, the latter framing is superior. If the historical and institutional inertia is genuinely significant, ''naturalization'' is more apt.',
    'A shift to ''incumbent power consolidation'' would emphasize the agency of beneficiaries and likely increase the perceived extractiveness and suppression, potentially reclassifying to a Snare. The ''naturalization'' framing allows for a more nuanced, hybrid classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Ambiguity in the primary framing of market dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__hybrid_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(mark_tr_t10, market_naturalization__hybrid_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(mark_tr_t20, market_naturalization__hybrid_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(mark_tr_t30, market_naturalization__hybrid_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(mark_tr_t40, market_naturalization__hybrid_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(mark_tr_t50, market_naturalization__hybrid_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__hybrid_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(mark_be_t10, market_naturalization__hybrid_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(mark_be_t20, market_naturalization__hybrid_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(mark_be_t30, market_naturalization__hybrid_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(mark_be_t40, market_naturalization__hybrid_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(mark_be_t50, market_naturalization__hybrid_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__hybrid_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(mark_su_t10, market_naturalization__hybrid_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(mark_su_t20, market_naturalization__hybrid_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(mark_su_t30, market_naturalization__hybrid_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(mark_su_t40, market_naturalization__hybrid_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(mark_su_t50, market_naturalization__hybrid_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__hybrid_reading, resource_allocation).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, beneficiary_maintained_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'market_naturalization' kernel. This 'hybrid_reading' emphasizes both historical inertia and active maintenance. The 'lapsed_alternative_reading' focuses solely on historical inertia, while the 'beneficiary_maintained_reading' focuses solely on active defense by incumbents. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
