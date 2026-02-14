% ============================================================================
% CONSTRAINT STORY: superbowl_advertising_extraction
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_superbowl_advertising_extraction, []).

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
 *   constraint_id: superbowl_advertising_extraction
 *   human_readable: Super Bowl Advertising Market
 *   domain: economic
 *
 * SUMMARY:
 *   The Super Bowl presents a unique, high-cost platform for companies to advertise to a massive, concentrated audience. This creates a powerful market dynamic where large companies feel compelled to participate to maintain brand relevance, leading to significant advertising expenditures that extract value. The high barrier to entry also effectively excludes smaller entities. The constraint is the market structure itself, which coordinates the sale of mass attention while extracting rents.
 *
 * KEY AGENTS (by structural relationship):
 *   - Small Businesses/Startups: Primary powerless target (powerless/trapped) — excluded by the high cost, which appears as an insurmountable economic barrier.
 *   - Large Corporate Advertisers: Primary organized target (organized/constrained) — bears direct extraction through high ad-buy costs.
 *   - Broadcasters (e.g., NBC, CBS): Primary beneficiary (institutional/arbitrage) — benefits from selling limited ad slots at maximal prices.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(superbowl_advertising_extraction, 0.55).
domain_priors:suppression_score(superbowl_advertising_extraction, 0.40).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(superbowl_advertising_extraction, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(superbowl_advertising_extraction, extractiveness, 0.55).
narrative_ontology:constraint_metric(superbowl_advertising_extraction, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(superbowl_advertising_extraction, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% This constraint is not a natural law, so these are not declared.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(superbowl_advertising_extraction, tangled_rope).

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(superbowl_advertising_extraction).
domain_priors:requires_active_enforcement(superbowl_advertising_extraction). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% This constraint is human-constructed, not naturally emergent.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(superbowl_advertising_extraction, broadcasters).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(superbowl_advertising_extraction, large_corporate_advertisers).
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
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE EXCLUDED ACTOR (POWERLESS)
% For a small business or startup, the multi-million dollar cost of entry is
% an absolute, unchangeable barrier. It functions as a Mountain of economic
% reality, completely foreclosing participation.
constraint_indexing:constraint_classification(superbowl_advertising_extraction, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(superbowl_advertising_extraction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ORGANIZED TARGET (SNARE)
% Agent who bears the most direct extraction. They are organized and have options,
% but the perceived cost of *not* participating is high, creating a coercive
% dynamic. From this view, it is a Snare.
constraint_indexing:constraint_classification(superbowl_advertising_extraction, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Sees both the genuine coordination function
% (matching advertisers with a massive audience) and the asymmetric extraction
% (high rents extracted due to manufactured scarcity).
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(superbowl_advertising_extraction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(superbowl_advertising_extraction_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between the powerless/excluded and the beneficiary.
    constraint_indexing:constraint_classification(superbowl_advertising_extraction, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(superbowl_advertising_extraction, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(superbowl_advertising_extraction, ExtMetricName, E),
    (E =< 0.25 -> false ; E >= 0.46). % Mountain or high-extraction Snare/Tangled.

:- end_tests(superbowl_advertising_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is 0.55 because advertising on the Super Bowl is extremely expensive, and companies often question the ROI. Suppression is moderate at 0.40; while there are alternative advertising strategies, none offer the same concentrated cultural impact, creating a coercive pressure to participate. The theater ratio is low (0.20) as the advertising is highly functional in driving short-term awareness.
 *
 * PERSPECTIVAL GAP:
 *   The gap is significant. For small businesses (powerless), the high cost is an absolute barrier, appearing as a Mountain. For large advertisers (organized), it's a Snare due to the high cost and pressure to participate. For broadcasters (institutional), it's a Rope that efficiently coordinates the sale of attention for revenue. The analytical observer sees the synthesis: a Tangled Rope combining a genuine coordination function with significant, asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Broadcasters are the clear beneficiaries, receiving substantial advertising revenue. Large corporate advertisers are the victims, bearing the direct cost of expensive ad slots. The framework's directionality derivation correctly maps these structural relationships to produce the different classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope correctly identifies the dual nature of the constraint. It avoids mislabeling it as a pure Snare, because the broadcasters genuinely provide a valuable, unique platform for reaching a massive audience. It also avoids mislabeling it as a pure Rope, because the advertisers are effectively compelled to participate at a high cost to avoid being perceived as irrelevant, indicating asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_superbowl_ad_roi,
    'What is the true long-term return on investment for Super Bowl advertising?',
    'Comprehensive analysis of brand performance metrics over a multi-year period following Super Bowl campaigns.',
    'If ROI is consistently low, the classification shifts towards a Snare. If ROI is high, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(superbowl_advertising_extraction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Required for high-extraction constraints (base_extractiveness > 0.46).
% The final values (T=10) must match the domain_priors declarations.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(superbowl_advertising_extraction_tr_t0, superbowl_advertising_extraction, theater_ratio, 0, 0.10).
narrative_ontology:measurement(superbowl_advertising_extraction_tr_t5, superbowl_advertising_extraction, theater_ratio, 5, 0.15).
narrative_ontology:measurement(superbowl_advertising_extraction_tr_t10, superbowl_advertising_extraction, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(superbowl_advertising_extraction_ex_t0, superbowl_advertising_extraction, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(superbowl_advertising_extraction_ex_t5, superbowl_advertising_extraction, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(superbowl_advertising_extraction_ex_t10, superbowl_advertising_extraction, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(superbowl_advertising_extraction, resource_allocation).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(superbowl_advertising_extraction, [0.0-1.0]).

% Network relationships (structural influence edges)
% narrative_ontology:affects_constraint(superbowl_advertising_extraction, [other_constraint_id]).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The structural derivation from
% beneficiary/victim declarations and exit options accurately models the
% directionality for each agent.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */