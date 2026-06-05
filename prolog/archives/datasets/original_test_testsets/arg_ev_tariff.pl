% ============================================================================
% CONSTRAINT STORY: arg_ev_tariff
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_arg_ev_tariff, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: arg_ev_tariff
 *   human_readable: Argentine Tariff on Chinese Electric Vehicles
 *   domain: economic/political
 *
 * SUMMARY:
 *   Argentina's potential tariff on Chinese electric vehicles represents a
 *   classic case of protectionism, creating a structural conflict between
 *   different economic actors. The policy, mirroring moves by the US and EU,
 *   aims to foster a domestic EV industry by raising the cost of competitive
 *   imports. This directly extracts value from Argentine consumers and
 *   Chinese manufacturers, transferring it to the government (as revenue) and
 *   nascent domestic producers (as market protection). The constraint's
 *   classification varies dramatically depending on the observer's structural
 *   position in this value transfer.
 *
 * KEY AGENTS:
 *   - Argentine Consumers: Primary victim (powerless/trapped) — face higher prices and fewer choices.
 *   - Argentine Domestic Auto Industry: Primary beneficiary (organized/constrained) — shielded from competition.
 *   - Chinese EV Manufacturers: Primary target (powerful/mobile) — face market access barriers but can pivot to other markets.
 *   - Argentine Government: Enforcer and beneficiary (institutional/arbitrage) — gains revenue and a tool for industrial policy.
 *   - Analytical Observer: External analyst (analytical/analytical) — views the full system of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(arg_ev_tariff, 0.65).
domain_priors:suppression_score(arg_ev_tariff, 0.75).
domain_priors:theater_ratio(arg_ev_tariff, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(arg_ev_tariff, extractiveness, 0.65).
narrative_ontology:constraint_metric(arg_ev_tariff, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(arg_ev_tariff, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(arg_ev_tariff, tangled_rope).
narrative_ontology:human_readable(arg_ev_tariff, "Argentine Tariff on Chinese Electric Vehicles").
narrative_ontology:topic_domain(arg_ev_tariff, "economic/political").

domain_priors:requires_active_enforcement(arg_ev_tariff).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(arg_ev_tariff, argentine_domestic_auto_industry).
narrative_ontology:constraint_beneficiary(arg_ev_tariff, argentine_government).
narrative_ontology:constraint_beneficiary(arg_ev_tariff, western_ev_manufacturers).
narrative_ontology:constraint_victim(arg_ev_tariff, argentine_consumers).
narrative_ontology:constraint_victim(arg_ev_tariff, chinese_ev_manufacturers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ARGENTINE CONSUMER (SNARE) — Trapped within the national market, the consumer bears the full cost of the tariff through higher prices and reduced choice, with no ability to exit. The policy is pure extraction. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.92.
constraint_indexing:constraint_classification(arg_ev_tariff, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DOMESTIC AUTO INDUSTRY (ROPE) — For the protected domestic industry and its workers, the tariff is a pure coordination mechanism. It shields them from foreign competition, enabling their survival and growth. d≈0.30, f(d)≈0.20, σ=1.0 → χ≈0.13.
constraint_indexing:constraint_classification(arg_ev_tariff, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CHINESE EV MANUFACTURER (TANGLED ROPE) — As a multinational, the manufacturer sees an extractive barrier but is not trapped. It can redirect exports to other markets (mobile exit). The tariff is a coercive rule to be navigated, not an inescapable snare. d≈0.85, f(d)≈1.15, σ=1.2 → χ≈0.90.
constraint_indexing:constraint_classification(arg_ev_tariff, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: ARGENTINE GOVERNMENT (ROPE) — As the enforcer and a beneficiary (via tariff revenue and political goals), the government experiences the policy as a low-extraction coordination tool for industrial and foreign policy. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.08.
constraint_indexing:constraint_classification(arg_ev_tariff, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — The observer sees both the claimed coordination function (protecting domestic industry) and the clear asymmetric extraction from consumers and foreign producers. This is the canonical view of a protectionist tariff. d≈0.73, f(d)≈1.15, σ=1.2 → χ≈0.90.
constraint_indexing:constraint_classification(arg_ev_tariff, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(arg_ev_tariff_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(arg_ev_tariff, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(arg_ev_tariff, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(arg_ev_tariff, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(arg_ev_tariff_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.65) is high, reflecting the significant price increase and market distortion imposed on consumers and foreign firms. Suppression (0.75) is also high, as the tariff's primary function is to coercively block a more efficient market alternative. Theater Ratio (0.30) is moderate; while there is a political signaling component (aligning with Western trade policy), the tariff's main purpose is its direct economic effect of protection and revenue generation.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For consumers, who are trapped and bear all costs, the tariff is a Snare. For the domestic industry it is intended to protect, it is a Rope—a pure coordination good. For the targeted Chinese firms, with global operations and an ability to exit, it is a Tangled Rope—a coercive but navigable rule. The analytical view, which must account for both the protectionist (coordination) intent and the extractive effect, also lands on Tangled Rope, confirming it as the most complete classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality `d` is derived from structural roles. The consumer is a trapped victim (d≈0.95), leading to maximum effective extraction (χ) and a Snare classification. The domestic industry is a constrained beneficiary (d≈0.30), leading to low χ and a Rope classification. The government is a beneficiary with arbitrage (d≈0.05), leading to negative χ (net benefit). The mobile victim (Chinese firms, d≈0.85) and the analytical observer (d≈0.73) both experience high but not maximal extraction, placing them in the Tangled Rope category.
 *
 * MANDATROPHY ANALYSIS:
 *   This case resolves the mandatrophy by demonstrating that a single policy is simultaneously a Rope, a Snare, and a Tangled Rope. An analysis that only considered the government's stated intent would misclassify it as a Rope (coordination). An analysis that only considered the consumer's experience would misclassify it as a Snare (pure predation). The Deferential Realism framework, by indexing to different structural positions, correctly identifies it as a Tangled Rope from the comprehensive analytical view, acknowledging both its coordination function and its highly extractive nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domestic_industry_viability,
    'Will a protected domestic EV industry become genuinely competitive, or will the tariff merely subsidize inefficiency?',
    'Longitudinal analysis of domestic EV production costs, quality, and innovation rates compared to global benchmarks over a 5-10 year period.',
    'If viable, the ''coordination'' function is real, supporting the Tangled Rope classification. If not, the coordination is illusory, and the constraint is functionally a pure Snare on consumers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_industry_viability, empirical, 'Whether the protected domestic industry achieves long-term competitiveness.').

omega_variable(
    consumer_welfare_impact,
    'What is the net economic effect when accounting for the deadweight loss to consumers versus the gains for domestic producers and government revenue?',
    'Computable general equilibrium (CGE) modeling of the Argentine economy with and without the tariff.',
    'A large net negative welfare impact would strengthen the Snare classification from most non-beneficiary perspectives. A net positive impact would support the Tangled Rope/Rope views.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_welfare_impact, empirical, 'Net economic welfare effect of the tariff on the Argentine economy.').

omega_variable(
    geopolitical_motivation,
    'Is the primary driver of the tariff economic protectionism or geopolitical alignment with Western powers against China?',
    'Analysis of diplomatic communications, timing of the policy relative to US/EU actions, and statements by policymakers.',
    'If primarily geopolitical, the ''theater_ratio'' is likely underestimated, and the constraint''s function is partly performative signaling, which could shift it towards a Piton if the economic effects are negligible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geopolitical_motivation, conceptual, 'The balance between economic protectionism and geopolitical signaling as the core motivation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(arg_ev_tariff, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arg__tr_t0, arg_ev_tariff, theater_ratio, 0, 0.4).
narrative_ontology:measurement(arg__tr_t5, arg_ev_tariff, theater_ratio, 5, 0.35).
narrative_ontology:measurement(arg__tr_t10, arg_ev_tariff, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(arg__be_t0, arg_ev_tariff, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(arg__be_t5, arg_ev_tariff, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(arg__be_t10, arg_ev_tariff, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(arg_ev_tariff, enforcement_mechanism).
narrative_ontology:affects_constraint(arg_ev_tariff, arg_lithium_export_policy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
