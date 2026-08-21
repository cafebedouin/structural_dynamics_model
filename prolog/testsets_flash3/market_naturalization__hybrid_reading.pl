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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   This constraint represents a 'hybrid reading' of market naturalization,
 *   where market dominance is sustained by a combination of historical
 *   factors (lapsed alternatives, institutional inertia) and ongoing, active
 *   maintenance by beneficiaries (lobbying, regulatory capture). It is
 *   distinct from readings that emphasize only active defense or only
 *   historical inertia. The claimed type is Tangled Rope, reflecting both a
 *   coordination function (market stability) and asymmetric extraction (rents
 *   to incumbents, costs to new entrants and consumers), requiring active
 *   enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__hybrid_reading, 0.58).
domain_priors:suppression_score(market_naturalization__hybrid_reading, 0.65).
domain_priors:theater_ratio(market_naturalization__hybrid_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__hybrid_reading, "Market Naturalization (Hybrid Reading)").
narrative_ontology:topic_domain(market_naturalization__hybrid_reading, "political_economy/economic_history/institutional_analysis").

domain_priors:requires_active_enforcement(market_naturalization__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__hybrid_reading, 'bcad07a8-15aa-4fb0-b1ef-8343a89a38b4').
narrative_ontology:cs_kernel_codification('bcad07a8-15aa-4fb0-b1ef-8343a89a38b4', implicit).
narrative_ontology:cs_authority_grounding('bcad07a8-15aa-4fb0-b1ef-8343a89a38b4', extraction).
narrative_ontology:cs_interpretation_layer_present('bcad07a8-15aa-4fb0-b1ef-8343a89a38b4').
narrative_ontology:cs_reading_relation('bcad07a8-15aa-4fb0-b1ef-8343a89a38b4', market_naturalization__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('bcad07a8-15aa-4fb0-b1ef-8343a89a38b4', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_axiom('bcad07a8-15aa-4fb0-b1ef-8343a89a38b4', foundational, market_structure_is_historically_contingent).
narrative_ontology:cs_axiom_status(market_structure_is_historically_contingent, holdable).
narrative_ontology:cs_axiom_grounding('bcad07a8-15aa-4fb0-b1ef-8343a89a38b4', market_structure_is_historically_contingent, empirically_contingent).
narrative_ontology:cs_axiom('bcad07a8-15aa-4fb0-b1ef-8343a89a38b4', foundational, market_power_requires_active_defense).
narrative_ontology:cs_axiom_status(market_power_requires_active_defense, holdable).
narrative_ontology:cs_axiom_grounding('bcad07a8-15aa-4fb0-b1ef-8343a89a38b4', market_power_requires_active_defense, empirically_contingent).
narrative_ontology:cs_reference_frame('bcad07a8-15aa-4fb0-b1ef-8343a89a38b4', dynamic_market_equilibrium).
narrative_ontology:cs_drift_state('bcad07a8-15aa-4fb0-b1ef-8343a89a38b4', contemporary_regulatory_environment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bcad07a8-15aa-4fb0-b1ef-8343a89a38b4', '').
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

% Benefit from the existing market structure, actively lobbying for regulations that favor their position and maintaining barriers to entry. They also benefit from the historical inertia of the market.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, incumbent_capital_holders, agenda_setter,
    institutional, generational, arbitrage, national).

% Tasked with overseeing market fairness, but often influenced by incumbent lobbying. They benefit from the stability of the existing market, which simplifies their regulatory task, and sometimes from revolving-door opportunities.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, regulatory_agencies, beneficiary,
    institutional, biographical, constrained, national).

% Face significant barriers to entry, including high capital requirements, established distribution networks, and regulatory hurdles. They bear the costs of overcoming these obstacles or are suppressed by them.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, new_market_entrants, payer,
    moderate, immediate, constrained, local).

% Pay higher prices or have fewer choices due to reduced competition. While individually mobile, collective action to challenge market dominance is difficult but possible.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, consumers, payer,
    organized, biographical, mobile, national).

% Analyze the historical evolution of market structures, identifying periods of active maintenance versus periods of inertial persistence. Their analysis informs the debate on the constraint's true nature.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, economic_historians, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable market environment for established players, reducing transaction costs and uncertainty for large-scale operations.
% TRANSFER_FUNCTION: Transfers economic rents from new entrants and consumers to incumbent capital holders and, indirectly, to regulatory agencies through reduced oversight burden and career opportunities.
% ABSENT_VOICES: Potential innovators and entrepreneurs who never enter the market due to prohibitive barriers, and a broader public unaware of the historical contingency of 'natural' market structures.
% DISAPPEARANCE_RATIONALE: If the combination of lapsed alternatives and active maintenance vanished, the market would experience a surge of new entrants, price competition, and innovation, fundamentally altering the economic landscape and power distribution.
% FOUNDING_PROBLEM: To create stable economic conditions for industrial growth and capital accumulation, often by consolidating industries and establishing regulatory frameworks.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent capital holders and some regulatory bodies claim the problem of market instability is still live. Economic historians and consumer advocates argue that the initial problem has been superseded by rent-seeking, with corroboration from antitrust case studies and independent market analyses.
narrative_ontology:disappearance_verdict(market_naturalization__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(market_naturalization__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__hybrid_reading, 0.58, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderate (0.58) because while some rents are collected, the market still provides some coordination. Suppression is higher (0.65) due to both structural barriers (lapsed alternatives) and active enforcement (regulatory capture, anti-competitive practices). Theater ratio is moderate-low (0.25), indicating that while some maintenance is performative (e.g., 'innovation' claims masking rent-seeking), a significant portion is genuine active defense of market position. The measurements show a rise in extractiveness and suppression over time, suggesting an increasing reliance on active maintenance as historical inertia wanes.
 *
 * PERSPECTIVAL GAP:
 *   Incumbent capital holders perceive the market structure as a natural outcome of competition and efficiency, requiring only 'fair' regulation. New entrants and consumers perceive it as an actively maintained barrier to competition and choice. Economic historians, from an analytical seat, can trace the evolution of both naturalized and actively constructed elements, revealing the hybrid nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent capital holders are primary beneficiaries and agenda-setters, actively shaping the market to their advantage. Regulatory agencies also benefit from the stability and reduced complexity, even if their formal mandate is neutrality. New market entrants and consumers are payers, bearing the costs of reduced competition and barriers to entry. The 'hybrid' nature means that while some alternatives have simply atrophied, others are actively suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading prevents mislabeling by acknowledging both the historical, inertial components (which might suggest a Piton or even a Mountain if misread) and the active, extractive components (which point to a Snare). By identifying both, it correctly classifies as a Tangled Rope, where the coordination story (market stability) is intertwined with asymmetric extraction and active enforcement. The 'contested' status of the founding problem further supports this hybrid view, as different parties emphasize different aspects of its persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_vs_lapsed_proportion,
    'What is the precise proportion of market dominance attributable to actively maintained barriers versus historically lapsed alternatives?',
    'Counterfactual economic modeling comparing market outcomes under different regulatory regimes (e.g., full deregulation vs. targeted anti-trust enforcement), or detailed historical case studies isolating specific policy changes and their effects.',
    'A higher proportion of active maintenance would push the classification closer to Snare, emphasizing the role of incumbent agency. A higher proportion of lapsed alternatives would emphasize the inertial aspects, potentially pushing it towards Piton if extraction were to decline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_vs_lapsed_proportion, empirical, 'Quantifying the relative contribution of active vs. passive mechanisms to market dominance.').

omega_variable(
    regulatory_capture_degree,
    'To what extent are regulatory agencies genuinely coordinating market stability versus being captured by incumbent interests?',
    'Analysis of regulatory decision-making, lobbying expenditures, and post-employment career paths of regulators, compared against public interest mandates.',
    'Higher capture would increase the effective extractiveness and suppression attributed to the ''agenda_setter'' seat, strengthening the Tangled Rope classification and potentially shifting it towards Snare if the coordination function is fully subverted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_degree, empirical, 'Assessing the extent of regulatory capture in maintaining market dominance.').

omega_variable(
    framing_of_market_efficiency,
    'Is the ''efficiency'' argument for market dominance a genuine coordination claim or a rhetorical cover for rent-seeking?',
    'Conceptual analysis of economic arguments, distinguishing between technical efficiency gains and allocative efficiency distortions, and examining the beneficiaries of claimed efficiencies.',
    'If primarily rhetorical, the coordination function is weaker, increasing the effective extractiveness and pushing the classification towards Snare. If genuine, it reinforces the coordination aspect of the Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_of_market_efficiency, conceptual, 'Distinguishing genuine efficiency from rhetorical justifications for market power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__hybrid_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mark_tr_t10, market_naturalization__hybrid_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(mark_tr_t20, market_naturalization__hybrid_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(mark_tr_t30, market_naturalization__hybrid_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(mark_tr_t40, market_naturalization__hybrid_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement(mark_tr_t50, market_naturalization__hybrid_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__hybrid_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(mark_be_t10, market_naturalization__hybrid_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(mark_be_t20, market_naturalization__hybrid_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(mark_be_t30, market_naturalization__hybrid_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(mark_be_t40, market_naturalization__hybrid_reading, base_extractiveness, 40, 0.59).
narrative_ontology:measurement(mark_be_t50, market_naturalization__hybrid_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__hybrid_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(mark_su_t10, market_naturalization__hybrid_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(mark_su_t20, market_naturalization__hybrid_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(mark_su_t30, market_naturalization__hybrid_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(mark_su_t40, market_naturalization__hybrid_reading, suppression_requirement, 40, 0.67).
narrative_ontology:measurement(mark_su_t50, market_naturalization__hybrid_reading, suppression_requirement, 50, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__hybrid_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is the 'hybrid_reading' of the 'market_naturalization' kernel, acknowledging both active maintenance and lapsed alternatives. It coexists with 'lapsed_alternative_reading' (emphasizing inertia) and 'beneficiary_maintained_reading' (emphasizing active defense).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
