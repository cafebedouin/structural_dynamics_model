% ============================================================================
% CONSTRAINT STORY: glen_canyon_water_allocation
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_glen_canyon_water_allocation, []).

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
 *   constraint_id: glen_canyon_water_allocation
 *   human_readable: Colorado River Water Allocation under the Colorado River Compact
 *   domain: political, economic, environmental
 *
 * SUMMARY:
 *   The Colorado River Compact, along with subsequent agreements and legal
 *   precedents, defines the allocation of water resources from the Colorado
 *   River among the Upper and Lower Basin states. Glen Canyon Dam plays a
 *   critical role in regulating water flow and distributing it according
 *   to these agreements.  This system, designed for a wetter climate, is
 *   failing under increasing drought and overuse.
 *
 * KEY AGENTS (by structural relationship):
 *   - Lower Basin States (Arizona, California, Nevada): Primary beneficiary (institutional/arbitrage) — benefits from guaranteed water supply.
 *   - Upper Basin States (Colorado, New Mexico, Utah, Wyoming): Primary target (institutional/constrained) — bear the brunt of water shortages.
 *   - Bureau of Reclamation: Regulator (institutional/constrained) — tasked with managing water allocation, constrained by existing legal framework.
 *   - Colorado River Ecosystem: Victim (powerless/trapped) - Suffers environmental degradation due to altered flow regimes and water extraction.
 *   - Analytical observer: Sees the full system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(glen_canyon_water_allocation, 0.55).
domain_priors:suppression_score(glen_canyon_water_allocation, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(glen_canyon_water_allocation, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(glen_canyon_water_allocation, extractiveness, 0.55).
narrative_ontology:constraint_metric(glen_canyon_water_allocation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(glen_canyon_water_allocation, theater_ratio, 0.40).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(glen_canyon_water_allocation, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(glen_canyon_water_allocation). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(glen_canyon_water_allocation, lower_basin_states).
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(glen_canyon_water_allocation, upper_basin_states).
narrative_ontology:constraint_victim(glen_canyon_water_allocation, colorado_river_ecosystem).

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
% Upper Basin States - constrained exit due to legal obligations and geographic limitations
constraint_indexing:constraint_classification(glen_canyon_water_allocation, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Lower Basin States - access to water allocation with some arbitrage opportunities (e.g., water markets)
constraint_indexing:constraint_classification(glen_canyon_water_allocation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
constraint_indexing:constraint_classification(glen_canyon_water_allocation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES (declare when applicable) ---
% Bureau of Reclamation - tasked with enforcing the compact, but also subject to political pressure
constraint_indexing:constraint_classification(glen_canyon_water_allocation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Colorado River Ecosystem - Powerless entity trapped by the system
constraint_indexing:constraint_classification(glen_canyon_water_allocation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(glen_canyon_water_allocation_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(glen_canyon_water_allocation, TypeTarget, context(agent_power(institutional), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(glen_canyon_water_allocation, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(glen_canyon_water_allocation, ExtMetricName, E),
    E >= 0.46. % high-extraction Tangled Rope.

:- end_tests(glen_canyon_water_allocation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Colorado River Compact initially aimed to coordinate water allocation. However,
 *   it has evolved into a system where the Lower Basin states benefit significantly
 *   at the expense of the Upper Basin states and the environment, particularly
 *   under drought conditions.  The base extractiveness is set to 0.55, reflecting
 *   a substantial level of resource extraction.  The suppression score is 0.65,
 *   indicating limited alternative water sources or management options for the
 *   Upper Basin.
 *
 * PERSPECTIVAL GAP:
 *   The Lower Basin states perceive the agreement as a rope, ensuring a reliable
 *   water supply. The Upper Basin states view it as a snare, as they are forced to
 *   bear the brunt of water shortages and reduced agricultural output due to climate
 *   change and overallocation.
 *
 * DIRECTIONALITY LOGIC:
 *   Lower Basin states are beneficiaries due to guaranteed water access. Upper Basin
 *   states bear the cost through reduced water availability and economic impact. The
 *   Colorado River ecosystem is a victim as the altered flow regimes degrade the
 *   natural environment.
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *   The Bureau of Reclamation is caught between enforcing the existing compact and
 *   addressing the reality of reduced water supply.  Their exit options are
 *   constrained by legal and political considerations.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling the system as a purely
 *   extractive snare because the initial intent and ongoing function of the
 *   compact is coordination, even if that coordination is now heavily skewed
 *   towards benefiting one group at the expense of others and the environment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_glen_canyon,
    'Will climate change cause a permanent reduction in Colorado River flow?',
    'Long-term climate modeling and observed river flow data.',
    'If True: The current allocation system is unsustainable. If False: Adjustments can potentially maintain the current system.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(glen_canyon_water_allocation, 0, 10).

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
narrative_ontology:measurement(glen_canyon_tr_t0, glen_canyon_water_allocation, theater_ratio, 0, 0.2).
narrative_ontology:measurement(glen_canyon_tr_t5, glen_canyon_water_allocation, theater_ratio, 5, 0.3).
narrative_ontology:measurement(glen_canyon_tr_t10, glen_canyon_water_allocation, theater_ratio, 10, 0.4).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(glen_canyon_ex_t0, glen_canyon_water_allocation, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(glen_canyon_ex_t5, glen_canyon_water_allocation, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(glen_canyon_ex_t10, glen_canyon_water_allocation, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(glen_canyon_water_allocation, resource_allocation).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(glen_canyon_water_allocation, 0.25).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(glen_canyon_water_allocation, [other_constraint_id]).

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
% constraint_indexing:directionality_override(glen_canyon_water_allocation, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */