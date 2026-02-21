% ============================================================================
% CONSTRAINT STORY: sk_dantongbeop
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_sk_dantongbeop, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sk_dantongbeop
 *   human_readable: South Korea's Mobile Device Distribution Improvement Act (Dantongbeop)
 *   domain: economic/technological
 *
 * SUMMARY:
 *   Enacted in 2014, the "Dantongbeop" was officially intended to stabilize
 *   the mobile device market by mandating transparent and non-discriminatory
 *   subsidies. In practice, it eliminated fierce subsidy competition, leading
 *   to higher effective device prices for consumers and increased profitability
 *   for the incumbent mobile network operators, creating a classic
 *   Tangled Rope structure of coordination for producers and extraction from consumers.
 *
 * KEY AGENTS (by structural relationship):
 *   - south_korean_consumers: Primary target (powerless/trapped) — bears extraction through higher prices and fewer discounts.
 *   - mobile_network_operators: Primary beneficiary (institutional/arbitrage) — benefits from reduced marketing costs and stabilized, higher profit margins.
 *   - kcc_regulators: Secondary beneficiary / Enforcer (institutional/constrained) — benefits from a more orderly, manageable market, fulfilling their mandate for "stabilization".
 *   - analytical_observers: Analytical observer — sees the dual function of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sk_dantongbeop, 0.55).
domain_priors:suppression_score(sk_dantongbeop, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(sk_dantongbeop, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sk_dantongbeop, extractiveness, 0.55).
narrative_ontology:constraint_metric(sk_dantongbeop, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(sk_dantongbeop, theater_ratio, 0.40).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(sk_dantongbeop, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(sk_dantongbeop). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(sk_dantongbeop, mobile_network_operators).
narrative_ontology:constraint_beneficiary(sk_dantongbeop, kcc_regulators).
narrative_ontology:constraint_beneficiary(sk_dantongbeop, device_manufacturers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(sk_dantongbeop, south_korean_consumers).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% South Korean consumers face a system with suppressed competition and high prices.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
% χ = 0.55 * 1.42 * 1.0 = 0.781. This high χ, ε, and suppression score makes it a Snare.
constraint_indexing:constraint_classification(sk_dantongbeop, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Mobile network operators see the law as a valuable coordination tool to prevent
% margin-destroying price wars.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
% χ = 0.55 * -0.12 * 1.0 = -0.066. This is a pure coordination function for them.
constraint_indexing:constraint_classification(sk_dantongbeop, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analytical view recognizes the dual nature: a coordination function exists, but it
% enables asymmetric extraction. The presence of beneficiary, victim, and active
% enforcement flags this as a Tangled Rope.
constraint_indexing:constraint_classification(sk_dantongbeop, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% The regulator experiences the constraint differently from the regulated industry.

% Perspective 4A: KCC Regulators (ROPE)
% The regulator benefits from a stable, orderly market that is easy to manage.
% Their exit is constrained by their mandate. As beneficiaries with constrained exit,
% the derived d is low but positive, leading to a Rope classification.
constraint_indexing:constraint_classification(sk_dantongbeop, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sk_dantongbeop_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    constraint_indexing:constraint_classification(sk_dantongbeop, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(sk_dantongbeop, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('Passed: Target (Snare) and Beneficiary (Rope) have different classifications.~n').

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(sk_dantongbeop, tangled_rope, context(agent_power(analytical), _, _, _)),
    format('Passed: Analytical observer correctly identifies the Tangled Rope structure.~n').

test(tangled_rope_gate_requirements_met) :-
    narrative_ontology:constraint_beneficiary(sk_dantongbeop, _),
    narrative_ontology:constraint_victim(sk_dantongbeop, _),
    domain_priors:requires_active_enforcement(sk_dantongbeop),
    format('Passed: All three gate requirements for Tangled Rope are present.~n').

:- end_tests(sk_dantongbeop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.55): High. Represents the significant consumer surplus transferred to mobile carriers due to the suppression of price competition.
 *   - Suppression Score (0.75): High. The law actively and legally forbids the primary mechanism of competition (differentiated subsidies), creating a highly coercive environment.
 *   - Theater Ratio (0.40): Moderate. The law is framed under the pro-consumer narrative of "fairness" and "transparency," which masks its primary extractive function.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For consumers (powerless/trapped), the law is a Snare that eliminates their ability to seek better deals, locking them into a high-price structure. For the mobile carriers (institutional/arbitrage), it's a perfect Rope—a coordination mechanism that solves the prisoner's dilemma of a price war, guaranteeing stable profits for the oligopoly.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `mobile_network_operators` directly profit. `kcc_regulators` benefit from an easily managed market. `device_manufacturers` benefit from stable handset pricing.
 *   - Victims: `south_korean_consumers` bear the full cost of the reduced competition through higher prices.
 *   The engine uses this clear division to derive a low/negative directionality `d` for beneficiaries (low χ, Rope) and a high `d` for victims (high χ, Snare).
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model distinguishes between the two institutional beneficiaries. The `mobile_network_operators` have `arbitrage` exit (they can shift capital, lobby, etc.), resulting in a negative-χ Rope. The `kcc_regulators` have `constrained` exit (they are bound by their legal mandate), resulting in a low-positive-χ Rope. From their view, the law is a functional tool for market order, not a profit-maximization scheme, but it's still a beneficial coordination mechanism. This captures the nuance of regulatory alignment with industry interests, a precursor to capture.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a textbook Tangled Rope, preventing misclassification. Labeling it a pure Snare would ignore the genuine (though self-serving) coordination function it provides to the industry. Labeling it a pure Rope would ignore the massive, asymmetric extraction from consumers. The Tangled Rope classification correctly identifies that the coordination *is the mechanism of extraction*.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_sk_dantongbeop,
    'Would the pre-2014 subsidy "chaos" have led to market instability or simply sustained lower consumer prices?',
    'A natural experiment (e.g., repeal of the law) or a comparative study with a similar but unregulated market.',
    'If chaos was inevitable, the law has a stronger (though still extractive) coordination claim. If not, the law is almost purely extractive.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(sk_dantongbeop, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for the law's lifecycle since 2014. Base extractiveness
% increased as carriers perfected their pricing strategies under the new regime.
% Required because base_extractiveness (0.55) > 0.46.

% Theater ratio over time:
narrative_ontology:measurement(sk_dantongbeop_tr_t0, sk_dantongbeop, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sk_dantongbeop_tr_t5, sk_dantongbeop, theater_ratio, 5, 0.40).
narrative_ontology:measurement(sk_dantongbeop_tr_t10, sk_dantongbeop, theater_ratio, 10, 0.40).

% Extraction over time:
narrative_ontology:measurement(sk_dantongbeop_ex_t0, sk_dantongbeop, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(sk_dantongbeop_ex_t5, sk_dantongbeop, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(sk_dantongbeop_ex_t10, sk_dantongbeop, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The law functions as both, but its primary effect is
% on the allocation of consumer surplus to producers.
narrative_ontology:coordination_type(sk_dantongbeop, resource_allocation).

% Network relationships: This law sets the stage for all other mobile pricing regulations.
narrative_ontology:affects_constraint(sk_dantongbeop, sk_mobile_plan_pricing).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% from beneficiary/victim declarations and exit options accurately models
% the relationships between consumers, carriers, and regulators.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */