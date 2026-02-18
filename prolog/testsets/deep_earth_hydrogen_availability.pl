% ============================================================================
% CONSTRAINT STORY: deep_earth_hydrogen_availability
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-26
% ============================================================================

:- module(constraint_deep_earth_hydrogen_availability, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: deep_earth_hydrogen_availability
 *   human_readable: Deep Earth Hydrogen Availability Limit
 *   domain: technological
 *
 * SUMMARY:
 *   The amount of hydrogen available in the Earth's core and mantle is limited by geophysical and geochemical processes. This constraint impacts future energy possibilities, specifically related to deep earth hydrogen extraction as a viable resource.  This hydrogen reserve is a physical constraint because its magnitude is determined by the planet's formation and evolution and cannot be increased.
 *
 * KEY AGENTS (by structural relationship):
 *   - Future Energy Extractors: Primary target (powerless/trapped) — faces resource limit
 *   - Scientific Community: Primary beneficiary (analytical/analytical) — benefits from knowledge of limits
 *   - Policymakers: Secondary actor (institutional/constrained) — must plan within physical limits
 *   - Analytical observer: Analytical observer — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deep_earth_hydrogen_availability, 0.15).  % Low, reflects the inherent limit
domain_priors:suppression_score(deep_earth_hydrogen_availability, 0.02).    % Very low, almost no coercion needed - it is a physical limit.
domain_priors:theater_ratio(deep_earth_hydrogen_availability, 0.01).        % Extremely low, very little performative maintenance

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, extractiveness, 0.15).
narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.
narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(deep_earth_hydrogen_availability, mountain).
narrative_ontology:human_readable(deep_earth_hydrogen_availability, "Deep Earth Hydrogen Availability Limit").
narrative_ontology:topic_domain(deep_earth_hydrogen_availability, "technological").

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(deep_earth_hydrogen_availability).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement(deep_earth_hydrogen_availability). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
domain_priors:emerges_naturally(deep_earth_hydrogen_availability).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(deep_earth_hydrogen_availability, scientific_community).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(deep_earth_hydrogen_availability, future_energy_extractors).
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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE/MOUNTAIN)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
%
% NOTE: Per "Dynamic Coalition" extension, this agent's power may be
% upgraded to 'organized' if the constraint is a snare with a critical
% mass of victims, potentially changing the classification.
%
% UNIFORM-TYPE EXCEPTION: For natural law constraints (mountain-only) or pure
% coordination constraints (rope-only), perspectives 1 and 2 may use any power
% atoms — the classification is the same from all perspectives. Include at
% least 2-3 perspectives to demonstrate the invariance.
constraint_indexing:constraint_classification(deep_earth_hydrogen_availability, mountain,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(deep_earth_hydrogen_availability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(deep_earth_hydrogen_availability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deep_earth_hydrogen_availability_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(deep_earth_hydrogen_availability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(deep_earth_hydrogen_availability, TypeBeneficiary, context(agent_power(analytical), _, _, _)),
    TypeTarget = TypeBeneficiary. % Both perceive it as a mountain

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, ExtMetricName, E),
    E =< 0.25. % Mountain

:- end_tests(deep_earth_hydrogen_availability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The availability of deep-earth hydrogen is a fundamentally limited resource due to the planet's geochemical and geophysical processes. The extractiveness is low because it is not a readily available resource, and accessing it would require significant technological advancements. Suppression is very low because there is almost no existing infrastructure to suppress, this is a limit imposed by the nature of planetary formation and composition. It's a physical fact, like the amount of iron in the core.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap. Both future energy extractors and the scientific community (as well as policymakers) recognize the limitations imposed by the availability of deep-earth hydrogen. This constraint is a fundamental limitation of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Future energy extractors bear the cost of the limited resource, as their potential extraction is limited. The scientific community benefits from the increased knowledge regarding the Earth's composition and its potential energy reserves. This understanding guides research and development efforts. The d value reflects this relationship, with the extractors being the target and the scientific community the beneficiary.
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *   N/A - This is a constraint primarily due to a physical limitation. There are currently no institutions significantly extracting deep earth hydrogen.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a mountain prevents mislabeling this constraint as a snare, which would imply that the limited resource is being actively suppressed or hoarded by a specific group. Instead, the limit is due to a natural physical constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_deep_earth_hydrogen_availability,
    'What is the exact amount of hydrogen present in the Earth\'s mantle and core?',
    'Advanced geophysical surveying and deep drilling projects.',
    'Higher quantity leads to more potential deep-earth energy extraction; lower quantity reduces its viability.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(deep_earth_hydrogen_availability, 0, 10).

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
narrative_ontology:measurement(deep_earth_hydrogen_availability_tr_t0, deep_earth_hydrogen_availability, theater_ratio, 0, 0.01).
narrative_ontology:measurement(deep_earth_hydrogen_availability_tr_t5, deep_earth_hydrogen_availability, theater_ratio, 5, 0.01).
narrative_ontology:measurement(deep_earth_hydrogen_availability_tr_t10, deep_earth_hydrogen_availability, theater_ratio, 10, 0.01).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(deep_earth_hydrogen_availability_ex_t0, deep_earth_hydrogen_availability, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(deep_earth_hydrogen_availability_ex_t5, deep_earth_hydrogen_availability, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(deep_earth_hydrogen_availability_ex_t10, deep_earth_hydrogen_availability, base_extractiveness, 10, 0.15).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
% narrative_ontology:coordination_type(deep_earth_hydrogen_availability, information_standard).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(deep_earth_hydrogen_availability, 0.0).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
narrative_ontology:affects_constraint(deep_earth_hydrogen_availability, rare_earth_dependency).

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
% constraint_indexing:directionality_override(deep_earth_hydrogen_availability, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */