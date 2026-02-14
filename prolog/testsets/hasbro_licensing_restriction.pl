% ============================================================================
% CONSTRAINT STORY: hasbro_licensing_restriction
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_hasbro_licensing_restriction, []).

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
 *   constraint_id: hasbro_licensing_restriction
 *   human_readable: Hasbro Licensing Restriction on Licensed Properties
 *   domain: economic
 *
 * SUMMARY:
 *   Hasbro maintains tight control over the licensing of its intellectual property, restricting licensees in various ways to protect its brand and market position.  This can include restrictions on product types, geographic sales areas, and marketing strategies.  While intended to maintain brand consistency and prevent market saturation, these restrictions also limit the potential for licensees to innovate and maximize profits.
 *
 * KEY AGENTS (by structural relationship):
 *   - Small/Startup Toy Licensees: Primary target (powerless/trapped) — bears maximum extraction
 *   - Established Toy Licensees: Secondary target (moderate/constrained) — bears extraction but has more leverage
 *   - Hasbro: Primary beneficiary (institutional/arbitrage) — benefits from constraint
 *   - Consumers: Secondary actor (moderate/mobile)
 *   - Analytical Observer: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hasbro_licensing_restriction, 0.48).
domain_priors:suppression_score(hasbro_licensing_restriction, 0.55).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(hasbro_licensing_restriction, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hasbro_licensing_restriction, extractiveness, 0.48).
narrative_ontology:constraint_metric(hasbro_licensing_restriction, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(hasbro_licensing_restriction, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.
%
% narrative_ontology:constraint_metric(hasbro_licensing_restriction, accessibility_collapse, [0.85-1.0]).
% narrative_ontology:constraint_metric(hasbro_licensing_restriction, resistance, [0.0-0.15]).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(hasbro_licensing_restriction, tangled_rope).

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(hasbro_licensing_restriction).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(hasbro_licensing_restriction). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
%
% domain_priors:emerges_naturally(hasbro_licensing_restriction).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(hasbro_licensing_restriction, hasbro).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(hasbro_licensing_restriction, toy_licensees).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)
%   Scaffold:     beneficiary + (has_sunset_clause OR no enforcement)
%   Snare:        victim required; beneficiary optional

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% A small startup licensee with no negotiating power. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(hasbro_licensing_restriction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(hasbro_licensing_restriction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(hasbro_licensing_restriction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ESTABLISHED LICENSEE (SNARE)
% An established licensee with more resources but still subject to the terms.
% Their 'constrained' exit is better than 'trapped', but the high base
% extractiveness still results in a Snare classification.
constraint_indexing:constraint_classification(hasbro_licensing_restriction, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hasbro_licensing_restriction_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(hasbro_licensing_restriction, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hasbro_licensing_restriction, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hasbro_licensing_restriction, ExtMetricName, E),
    (E =< 0.25 -> true ; E >= 0.46). % Mountain or high-extraction Snare/Tangled.

:- end_tests(hasbro_licensing_restriction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score of 0.48 reflects the significant limitations placed on licensees, preventing them from fully capitalizing on the licensed IP. The suppression score of 0.55 reflects that while licensees are constrained, they do have alternative options (e.g., licensing from other companies, developing their own IP), but these are costly and difficult, especially for smaller players. The theater ratio is low (0.20) as the primary function is economic control, not theatrical performance.
 *
 * PERSPECTIVAL GAP:
 *   Small or established licensees perceive the licensing restrictions as a snare because they are constrained in their ability to maximize profits and innovate. Hasbro, on the other hand, views the restrictions as a rope, a necessary mechanism to maintain brand consistency and prevent market saturation, thus benefiting their overall strategy. The analytical observer sees both the coordination function (brand protection) and the asymmetric extraction, classifying it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Hasbro benefits from the restrictions, as they retain control over their brand and market position. Toy licensees bear the costs, as they are limited in their ability to innovate and maximize profits. The beneficiary is Hasbro, and the victims are the toy licensees.
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *   N/A. This is primarily a relationship between a large corporation and its licensees, not a clash between institutions.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling coordination as pure extraction by acknowledging that the licensing agreement, while extractive, also provides a coordination function: maintaining brand consistency and market stability, benefiting both Hasbro and, to some extent, the licensees by preserving the value of the IP. Without the restrictions, the IP could be devalued by over-licensing and poor quality products, which would ultimately hurt everyone. The Tangled Rope classification captures this duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_hasbro_licensing_restriction,
    'To what extent do the licensing restrictions actually prevent market saturation vs. simply stifle innovation?',
    'A comprehensive market study comparing Hasbro licensed products to those of companies with less restrictive licensing agreements.',
    'If the restrictions primarily prevent saturation, classification trends towards Rope; if they stifle innovation, classification trends towards Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(hasbro_licensing_restriction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
%
% Required for high-extraction constraints (base_extractiveness > 0.46).
% Use at least 3 time points (T=0, midpoint, T=end) for each tracked metric.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(hasbro_licensing_restriction_tr_t0, hasbro_licensing_restriction, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hasbro_licensing_restriction_tr_t5, hasbro_licensing_restriction, theater_ratio, 5, 0.20).
narrative_ontology:measurement(hasbro_licensing_restriction_tr_t10, hasbro_licensing_restriction, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(hasbro_licensing_restriction_ex_t0, hasbro_licensing_restriction, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(hasbro_licensing_restriction_ex_t5, hasbro_licensing_restriction, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(hasbro_licensing_restriction_ex_t10, hasbro_licensing_restriction, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(hasbro_licensing_restriction, information_standard).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(hasbro_licensing_restriction, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(hasbro_licensing_restriction, [other_constraint_id]).

% --- Network Decomposition (Constraint Families) ---
% When a natural-language label covers multiple constraints with different ε
% values, each gets its own file. Link family members with affects_constraint:
%
% DUAL FORMULATION NOTE:
% This constraint is one of [N] stories decomposed from [colloquial label].
% Decomposed because ε differs across observables (ε-invariance principle).
% Related stories:
%   - [sibling_constraint_1] (ε=[value], [Type])
%   - [sibling_constraint_2] (ε=[value], [Type])
%
% narrative_ontology:affects_constraint(hasbro_licensing_restriction, [sibling_constraint_id]).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Use ONLY when the automatic derivation (beneficiary/victim + exit → d)
% would produce an inaccurate directionality value. The derivation chain
% priority is: override > structural > canonical fallback.
%
% Format: directionality_override(ConstraintID, PowerAtom, D_Value)
%   D_Value in [0.0, 1.0]: 0.0 = full beneficiary, 1.0 = full target
%
% Common override scenarios:
%   - Regulatory capture: institution that appears to benefit but is
%     actually partly captured → override d upward (0.25-0.40)
%   - Indirect beneficiary: agent in victim group who actually benefits
%     through secondary effects → override d downward
%   - Asymmetric institutional: two institutional actors that the
%     derivation can't distinguish → override to differentiate
%
% Example (uncomment if needed):
% constraint_indexing:directionality_override(hasbro_licensing_restriction, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */