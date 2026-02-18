% ============================================================================
% CONSTRAINT STORY: rare_earth_dependency
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_rare_earth_dependency, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: rare_earth_dependency
 * human_readable: Strategic Rare Earth Element Dependency
 * domain: economic/geopolitical
 * * SUMMARY:
 * A nation's strategic reliance on a single foreign power for critical resources, such as rare earth elements (REEs), creates a significant economic and security vulnerability. This dependency can be weaponized through export controls, price manipulation, or supply disruption, effectively extracting geopolitical concessions or economic rent. The constraint is maintained by high barriers to entry for alternative suppliers, including geological scarcity, technical challenges in extraction/processing, and long investment timelines.
 * * KEY AGENTS:
 * - Dependent Nation's Industries (e.g., Japanese Manufacturers): Subject (Powerless)
 * - Dominant Supplier Nation (e.g., Chinese Government): Beneficiary (Institutional)
 * - International Trade Analysts: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(rare_earth_dependency, 0.60). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(rare_earth_dependency, 0.70).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(rare_earth_dependency, 0.20).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(rare_earth_dependency, extractiveness, 0.60).
narrative_ontology:constraint_metric(rare_earth_dependency, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(rare_earth_dependency, theater_ratio, 0.20).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(rare_earth_dependency, tangled_rope).
narrative_ontology:human_readable(rare_earth_dependency, "Strategic Rare Earth Element Dependency").
narrative_ontology:topic_domain(rare_earth_dependency, "economic/geopolitical").

% Binary flags
% narrative_ontology:has_sunset_clause(rare_earth_dependency).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(rare_earth_dependency). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(rare_earth_dependency, dominant_supplier_nation).
narrative_ontology:constraint_victim(rare_earth_dependency, dependent_nations_industries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% High extraction felt as a predatory trap with no immediate escape.
constraint_indexing:constraint_classification(rare_earth_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))). % Global market context amplifies extraction

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as a legitimate tool of economic statecraft and a valuable trade relationship.
constraint_indexing:constraint_classification(rare_earth_dependency, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context (civilizational/analytical/global).
% Recognizes both the coordination function (trade) and the asymmetric extraction (geopolitical leverage).
constraint_indexing:constraint_classification(rare_earth_dependency, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rare_earth_dependency_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(rare_earth_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rare_earth_dependency, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(tangled_rope_conditions) :-
    % The analytical observer must see this as a tangled_rope.
    constraint_indexing:constraint_classification(rare_earth_dependency, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Ensures it's a high-extraction constraint.
    narrative_ontology:constraint_metric(rare_earth_dependency, extractiveness, E),
    E >= 0.46.

:- end_tests(rare_earth_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness (0.60) and suppression (0.70) reflect the high strategic cost and lack of immediate alternatives for dependent nations.
 * - For a powerless industry (e.g., Japanese manufacturer), the dependency is a Snare. Their operations are trapped by the risk of supply cuts, leading to high effective extraction (χ = 0.60 * 1.5 * 1.2 = 1.08).
 * - For the institutional beneficiary (e.g., Chinese state), it's a Rope. It's a tool for coordination and economic benefit, with the extractive nature being a feature, not a bug. Their power drastically reduces the felt extraction (χ = 0.60 * -0.2 * 1.2 = -0.144), appearing as a net benefit.
 * - The analytical observer classifies it as a Tangled Rope. It has a genuine coordination function (a global trade market for a necessary resource) but also features clear asymmetric extraction and requires active enforcement (export quotas, state control) to maintain. This classification correctly captures the dual nature of the relationship, preventing a misclassification as a pure Snare (which would ignore the trade benefits) or a pure Rope (which would ignore the coercive power imbalance).
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification is critical here. A simpler model might view this as a pure Snare, but that would miss the fact that the dependent nations *do* receive vital resources, making it a functional (if coercive) system. The `requires_active_enforcement` flag, coupled with the clear beneficiary/victim roles, confirms the hybrid nature. The system isn't just passively extractive; it's an actively managed system of strategic leverage built on a coordination backbone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_rare_earth_dependency,
    'Will technological breakthroughs (e.g., new magnet tech without REEs, efficient recycling) disrupt the fundamental demand for these elements faster than new supplies can be brought online?',
    'Monitoring R&D in materials science and tracking adoption rates of REE-free technologies in key sectors (EVs, wind turbines).',
    'If True: The constraint dissolves or becomes a Piton as the dependency becomes irrelevant. If False: The constraint remains a potent Tangled Rope/Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(rare_earth_dependency, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the intensification of this dependency over the last decade,
% as REEs became more critical for green and digital technologies.
%
% Theater ratio over time (remains low as this is a functional, not performative, constraint):
narrative_ontology:measurement(rare_earth_dependency_tr_t0, rare_earth_dependency, theater_ratio, 0, 0.10).
narrative_ontology:measurement(rare_earth_dependency_tr_t5, rare_earth_dependency, theater_ratio, 5, 0.15).
narrative_ontology:measurement(rare_earth_dependency_tr_t10, rare_earth_dependency, theater_ratio, 10, 0.20).

% Extraction over time (increases as geopolitical leverage is recognized and applied):
narrative_ontology:measurement(rare_earth_dependency_ex_t0, rare_earth_dependency, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(rare_earth_dependency_ex_t5, rare_earth_dependency, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(rare_earth_dependency_ex_t10, rare_earth_dependency, base_extractiveness, 10, 0.60).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This dependency functions as a mechanism for controlling resource allocation.
narrative_ontology:coordination_type(rare_earth_dependency, resource_allocation).

% Network relationships (structural influence edges)
% The supply of REEs is a direct upstream dependency for semiconductor manufacturing.
narrative_ontology:affects_constraint(rare_earth_dependency, semiconductor_supply_chain).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */