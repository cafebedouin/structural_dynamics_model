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
 *   human_readable: Market Naturalization: Hybrid Reading
 *   domain: political_economy/economic_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story instantiates the 'hybrid_reading' of market
 *   naturalization, which posits that market dominance is sustained by a
 *   combination of historical path dependence (lapsed alternatives) and
 *   ongoing strategic action (active maintenance). It acknowledges elements
 *   of both the 'lapsed_alternative_reading' and the
 *   'beneficiary_maintained_reading' as partial truths, integrating them into
 *   a more comprehensive account. The metrics reflect this hybridity:
 *   moderate extractiveness and suppression, with a non-negligible theater
 *   ratio representing the narrative of 'natural' market outcomes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__hybrid_reading, 0.6).
domain_priors:suppression_score(market_naturalization__hybrid_reading, 0.7).
domain_priors:theater_ratio(market_naturalization__hybrid_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__hybrid_reading, "Market Naturalization: Hybrid Reading").
narrative_ontology:topic_domain(market_naturalization__hybrid_reading, "political_economy/economic_history/institutional_analysis").

domain_priors:requires_active_enforcement(market_naturalization__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__hybrid_reading, '3a0f2f0c-e8a9-4c92-a23b-83ad9e6611b5').
narrative_ontology:cs_kernel_codification('3a0f2f0c-e8a9-4c92-a23b-83ad9e6611b5', implicit).
narrative_ontology:cs_authority_grounding('3a0f2f0c-e8a9-4c92-a23b-83ad9e6611b5', extraction).
narrative_ontology:cs_interpretation_layer_present('3a0f2f0c-e8a9-4c92-a23b-83ad9e6611b5').
narrative_ontology:cs_reading_relation('3a0f2f0c-e8a9-4c92-a23b-83ad9e6611b5', market_naturalization__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a0f2f0c-e8a9-4c92-a23b-83ad9e6611b5', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_axiom('3a0f2f0c-e8a9-4c92-a23b-83ad9e6611b5', foundational, market_dominance_hybrid_maintenance).
narrative_ontology:cs_axiom_status(market_dominance_hybrid_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('3a0f2f0c-e8a9-4c92-a23b-83ad9e6611b5', market_dominance_hybrid_maintenance, empirically_contingent).
narrative_ontology:cs_axiom('3a0f2f0c-e8a9-4c92-a23b-83ad9e6611b5', secondary, balance_of_forces_dynamic).
narrative_ontology:cs_axiom_status(balance_of_forces_dynamic, holdable).
narrative_ontology:cs_axiom_grounding('3a0f2f0c-e8a9-4c92-a23b-83ad9e6611b5', balance_of_forces_dynamic, empirically_contingent).
narrative_ontology:cs_reference_frame('3a0f2f0c-e8a9-4c92-a23b-83ad9e6611b5', competitive_market_equilibrium).
narrative_ontology:cs_drift_state('3a0f2f0c-e8a9-4c92-a23b-83ad9e6611b5', contemporary_neoliberal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3a0f2f0c-e8a9-4c92-a23b-83ad9e6611b5', '').
narrative_ontology:cs_kernel_id(market_naturalization__hybrid_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, incumbent_firms).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, capital_holders).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, new_entrants).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, consumers).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, displaced_labor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Large, established corporations that benefit from market dominance. They actively lobby for regulations that favor their position and leverage historical advantages to suppress new competition, while also benefiting from the inertia of lapsed alternatives.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, incumbent_firms, agenda_setter,
    institutional, generational, arbitrage, global).

% Investors and shareholders who profit from the stable, often supra-normal, returns generated by dominant market positions. They support policies and practices that maintain this dominance, both through active defense and by benefiting from historical market closures.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, capital_holders, beneficiary,
    powerful, generational, mobile, global).

% Start-ups and smaller businesses attempting to enter markets dominated by incumbents. They face significant barriers to entry, including regulatory hurdles, network effects, and active anti-competitive practices, as well as the legacy of past market closures.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, new_entrants, payer,
    moderate, biographical, constrained, national).

% Pay higher prices and have fewer choices due to reduced competition. While they may benefit from some historical coordination aspects of established markets, the hybrid nature of dominance means they also bear the costs of active extraction and suppressed alternatives.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, consumers, payer,
    organized, biographical, constrained, national).

% Workers whose employment opportunities or bargaining power are diminished by market concentration and the suppression of new, potentially more labor-friendly, industries. They are often trapped by local economic conditions and lack viable alternatives.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, displaced_labor, payer,
    powerless, immediate, trapped, local).

% Government agencies tasked with ensuring fair competition and preventing monopolies. They investigate market practices, but their effectiveness is often constrained by political influence from incumbents and the complexity of distinguishing active suppression from historical inertia.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, regulators, observer,
    institutional, biographical, analytical, national).

% Academics who study the long-term evolution of market structures, identifying how initial conditions, policy choices, and technological shifts contribute to current dominance. They provide an analytical perspective on the interplay of lapsed alternatives and active maintenance.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, economic_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Historically, markets coordinated the allocation of resources, capital, and labor, fostering innovation and economic growth by providing a stable framework for exchange. This function is partially maintained by the existing dominant structures.
% TRANSFER_FUNCTION: Transfers wealth, power, and opportunity from new entrants, consumers, and labor to incumbent firms and capital holders. This occurs through pricing power, control over distribution channels, intellectual property enforcement, and the suppression of competitive alternatives.
% ABSENT_VOICES: Potential innovators and small businesses whose ideas never reach the market due to insurmountable barriers to entry; unorganized labor whose collective bargaining power is eroded by concentrated employers; and future consumers who would benefit from more dynamic, competitive markets.
% DISAPPEARANCE_RATIONALE: If market dominance, sustained by both active maintenance and lapsed alternatives, vanished overnight, the economic landscape would fundamentally reorganize. New firms would emerge, prices would likely fall, innovation patterns would shift, and the distribution of wealth and power would be significantly altered.
% FOUNDING_PROBLEM: To efficiently organize complex economic activity, allocate scarce resources, and incentivize productive innovation in a way that benefits society broadly.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent firms and their advocates argue that current market structures still solve the founding problem by providing stability and scale. However, independent economists, antitrust regulators, and consumer advocates, citing evidence of reduced competition and wealth concentration, corroborate that the founding problem is now significantly distorted by the pursuit of private rents.
narrative_ontology:disappearance_verdict(market_naturalization__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(market_naturalization__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__hybrid_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

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
 *   The 'tangled_rope' classification reflects the dual nature of market dominance under this reading: it provides some coordination (e.g., established infrastructure, predictable exchange) but also involves significant asymmetric extraction and active suppression of alternatives. Extractiveness and suppression are moderate, as some market closures are indeed inertial (lapsed alternatives), while others require continuous, active enforcement (beneficiary-maintained). The theater ratio is non-zero because the 'naturalness' of market outcomes is often performatively asserted to mask active rent-seeking. The temporal measurements show a gradual increase in extractiveness, suppression, and theatricality, indicating a drift towards greater rent-seeking over time.
 *
 * PERSPECTIVAL GAP:
 *   Beneficiaries (incumbent firms, capital holders) perceive market dominance as a natural, efficient outcome of competition, or as a necessary structure for stability. Targets (new entrants, consumers, labor) experience it as an extractive and suppressive force. The hybrid reading attempts to bridge this gap by showing how both active and passive mechanisms contribute to the observed outcome.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent firms and capital holders are clear beneficiaries, actively shaping and benefiting from the constraint, placing them at the low-d end. New entrants, consumers, and displaced labor are targets, bearing the costs of reduced competition and suppressed alternatives, placing them at the high-d end. Regulators and economic historians serve as analytical observers, attempting to understand and potentially challenge the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balance_of_maintenance_and_inertia,
    'What is the precise balance between active maintenance (strategic action by incumbents) and historical inertia (lapsed alternatives) in sustaining market dominance in a given sector?',
    'Detailed empirical case studies comparing sectors with varying regulatory histories and technological disruption rates, quantifying the resources spent on lobbying, IP enforcement, and M&A versus the persistence of network effects or switching costs.',
    'If active maintenance dominates, the constraint leans more towards a Snare; if inertia dominates, it leans more towards a Piton. The hybrid reading''s classification as Tangled Rope depends on a significant balance of both.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(balance_of_maintenance_and_inertia, empirical, 'Quantifying the relative contributions of active vs. passive mechanisms to market dominance.').

omega_variable(
    naturalness_narrative_efficacy,
    'To what extent does the ''naturalness'' narrative (theater_ratio) genuinely reduce resistance or merely provide a rhetorical cover for active suppression?',
    'Comparative analysis of public opinion, regulatory scrutiny, and resistance movements in markets where the ''natural'' narrative is strong versus weak, controlling for actual suppressive force.',
    'If the narrative significantly reduces resistance, the effective suppression is higher than structural measures suggest. If it''s primarily rhetorical cover, the theater ratio indicates a performative aspect without necessarily altering the underlying suppressive force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_narrative_efficacy, empirical, 'Assessing the functional impact of the ''naturalness'' narrative on market resistance.').

omega_variable(
    framing_of_market_dominance,
    'Is market dominance fundamentally a problem of ''natural'' economic forces, or a consequence of ''constructed'' institutional and political choices?',
    'Conceptual analysis of economic theory and legal precedent, examining the underlying assumptions about market agency and structure. This is a conceptual omega, not resolvable by empirical data alone.',
    'If framed as natural, interventions are seen as distorting; if constructed, interventions are seen as restoring fairness. This conceptual choice influences the interpretation of all metrics and the policy response.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_of_market_dominance, conceptual, 'Conceptual framing of market dominance as natural vs. constructed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__hybrid_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(mark_tr_t10, market_naturalization__hybrid_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(mark_tr_t20, market_naturalization__hybrid_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(mark_tr_t30, market_naturalization__hybrid_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(mark_tr_t40, market_naturalization__hybrid_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(mark_tr_t50, market_naturalization__hybrid_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__hybrid_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(mark_be_t10, market_naturalization__hybrid_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(mark_be_t20, market_naturalization__hybrid_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(mark_be_t30, market_naturalization__hybrid_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(mark_be_t40, market_naturalization__hybrid_reading, base_extractiveness, 40, 0.59).
narrative_ontology:measurement(mark_be_t50, market_naturalization__hybrid_reading, base_extractiveness, 50, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__hybrid_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(mark_su_t10, market_naturalization__hybrid_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(mark_su_t20, market_naturalization__hybrid_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(mark_su_t30, market_naturalization__hybrid_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(mark_su_t40, market_naturalization__hybrid_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(mark_su_t50, market_naturalization__hybrid_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__hybrid_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
