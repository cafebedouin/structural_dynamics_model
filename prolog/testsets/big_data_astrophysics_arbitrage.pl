% ============================================================================
% CONSTRAINT STORY: big_data_astrophysics_arbitrage
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-20
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_big_data_astrophysics_arbitrage, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: big_data_astrophysics_arbitrage
 *   human_readable: Big Data Arbitrage in Modern Astrophysics
 *   domain: technological
 *
 * SUMMARY:
 *   As astronomy shifts from spatial mapping to time-domain analysis, the vast data streams (e.g., 1.6 petabytes
 *   from Pan-STARRS) create a new form of arbitrage. Automated systems and well-funded research groups can
 *   process this "blizzard" of information to prioritize and claim discoveries of fleeting "transient" events.
 *   This industrializes discovery, moving it from "happy accidents" to a systematic data extraction process that
 *   disadvantages those without comparable computational resources.
 *
 * KEY AGENTS (by structural relationship):
 *   - Resource-Limited Astronomers: Primary target (powerless/trapped) — their traditional, serendipitous discovery methods are suppressed and de-legitimized by the sheer data volume they cannot process.
 *   - Institutional Data Centers: Primary beneficiary (institutional/arbitrage) — benefit from the infrastructure, funding, and prestige associated with processing petabyte-scale data and automating discovery.
 *   - Data-Enabled Researchers: Secondary beneficiary (moderate/mobile) — leverage the data infrastructure as a powerful tool, gaining significant advantage over traditional methods.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(big_data_astrophysics_arbitrage, 0.75).
domain_priors:suppression_score(big_data_astrophysics_arbitrage, 0.45).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(big_data_astrophysics_arbitrage, 0.60).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(big_data_astrophysics_arbitrage, extractiveness, 0.75).
narrative_ontology:constraint_metric(big_data_astrophysics_arbitrage, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(big_data_astrophysics_arbitrage, theater_ratio, 0.60).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(big_data_astrophysics_arbitrage, tangled_rope).
narrative_ontology:human_readable(big_data_astrophysics_arbitrage, "Big Data Arbitrage in Modern Astrophysics").

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(big_data_astrophysics_arbitrage).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(big_data_astrophysics_arbitrage). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(big_data_astrophysics_arbitrage, institutional_data_centers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(big_data_astrophysics_arbitrage, resource_limited_astronomers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% The resource-limited astronomer who relies on serendipity. The "conveyor belt" of data
% is a Snare that extracts the legitimacy of their discovery method. They are trapped,
% unable to process the 1.6 petabyte "blizzard".
constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The institutional data center (e.g., Pan-STARRS) that controls the infrastructure.
% For them, the system is a pure coordination Rope, organizing a vast search effort.
% Their arbitrage exit ensures they benefit maximally.
constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The observer sees both the genuine coordination function (a "well-oiled machine" for
% discovery) and the asymmetric extraction (sidelining those without computational power).
% This dual nature is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE DATA-ENABLED RESEARCHER (ROPE)
% A researcher with sufficient resources to use the data stream. For them, it is a
% powerful Rope that enables systematic discovery, moving beyond "happy accidents".
% Their exit is mobile, as they can choose which data sets or projects to work on.
constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(big_data_astrophysics_arbitrage_tests).

test(perspectival_gap) :-
    % Verify the gap between the target (Snare) and beneficiary (Rope).
    constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, snare, context(agent_power(powerless), _, trapped, _)),
    constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, rope, context(agent_power(institutional), _, arbitrage, _)),
    constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements) :-
    % Verify that all three structural requirements for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(big_data_astrophysics_arbitrage, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(big_data_astrophysics_arbitrage, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(big_data_astrophysics_arbitrage).

:- end_tests(big_data_astrophysics_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.75) is high, reflecting the industrial scale of "extracting" transient cosmic
 *   events from the noise of the universe, effectively capturing discovery opportunities. The suppression (0.45)
 *   represents the de-facto invalidation of older, serendipitous methods which cannot compete. The constraint
 *   is fundamentally a Tangled Rope because it possesses both a genuine, powerful coordination function (organizing
 *   a global search) and a clear extractive effect (centralizing discovery power and disadvantaging the unequipped).
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For institutional beneficiaries with arbitrage exit, the system is a pure coordination Rope (negative effective extraction).
 *   For resource-limited astronomers who are trapped, it's a Snare that makes their methods obsolete (high positive effective extraction).
 *   The analytical observer, seeing both functions, correctly identifies it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `institutional_data_centers` directly benefit from the funding, infrastructure, and scientific prestige of running the "conveyor belt".
 *   - Victim: `resource_limited_astronomers` bear the cost, as their mode of scientific production is suppressed by a system they cannot afford to participate in.
 *   This clear division of costs and benefits is what drives the directionality calculation and reveals the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The high extraction score (0.75) is resolved as functional. This is not a system failing at its goal; it is a system succeeding at a new, more extractive goal. It successfully coordinates a massive search effort (the Rope function) while simultaneously extracting discovery opportunities from the commons and concentrating them among those with computational capital (the Snare function). This dual-functionality is the hallmark of a Tangled Rope, not Mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    big_data_astrophysics_arbitrage_extraction_intent,
    "Is the 1.6 petabyte extraction a functional necessity for understanding the universe or a predatory capture of cosmic data?",
    "Audit of scientific value derived from automated transients vs. legacy serendipity discoveries",
    "If necessity: Tangled Rope. If predatory: Snare.",
    confidence_without_resolution(medium)
).

omega_variable(
    data_redundancy_clog,
    "Will the blizzard of data (1.6PB) create a 'noise snare' where true signals are buried under automated artifacts?",
    "Measure the ratio of confirmed transients to 'conveyor belt' errors over time",
    "If signal: Tangled Rope. If noise: Snare/Piton.",
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(big_data_astrophysics_arbitrage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution, extraction_accumulation).
% Theater ratio: Rising from functional survey indexing (0.15) to
% performative "Automated Serendipity" (0.60).
narrative_ontology:measurement(ast_tr_t0, big_data_astrophysics_arbitrage, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ast_tr_t5, big_data_astrophysics_arbitrage, theater_ratio, 5, 0.38).
narrative_ontology:measurement(ast_tr_t10, big_data_astrophysics_arbitrage, theater_ratio, 10, 0.60).

% Extraction: Tracking the intensification of "Industrialized Serendipity"
% as 1.6PB of data liquidates traditional discovery agency.
narrative_ontology:measurement(ast_ex_t0, big_data_astrophysics_arbitrage, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(ast_ex_t5, big_data_astrophysics_arbitrage, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(ast_ex_t10, big_data_astrophysics_arbitrage, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(big_data_astrophysics_arbitrage, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options correctly models the dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */