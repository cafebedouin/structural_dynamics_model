% ============================================================================
% CONSTRAINT STORY: north_sea_wind_grid
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_north_sea_wind_grid, []).

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
 *   constraint_id: north_sea_wind_grid
 *   human_readable: The North Sea 100GW Multinational Wind Power Grid Initiative
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   A coalition of 10 North Sea countries agrees to a massive infrastructure
 *   project to build a shared 100GW offshore wind power grid. This constraint
 *   represents the set of treaties, regulations, and public-private funding
 *   mechanisms that coordinate the project. While it solves a major coordination
 *   problem for energy security and decarbonization, it also involves significant
 *   asymmetric extraction, directing public funds to private energy firms and
 *   actively suppressing the fossil fuel industry.
 *
 * KEY AGENTS (by structural relationship):
 *   - Renewable Energy Consortiums: Primary beneficiary (institutional/arbitrage) — receive massive state-backed contracts and a guaranteed market.
 *   - Participating Governments: Primary beneficiary (institutional/constrained) — achieve climate targets and energy security goals.
 *   - Fossil Fuel Industry: Primary target (organized/constrained) — face accelerated market suppression and loss of political influence.
 *   - Taxpayers of Member States: Secondary target (powerless/trapped) — bear the initial cost of public investment.
 *   - Analytical Observer: Sees both the coordination function and the asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(north_sea_wind_grid, 0.45). % Represents the portion of public investment captured as private profit/rent.
domain_priors:suppression_score(north_sea_wind_grid, 0.75).   % Actively suppresses the fossil fuel industry alternative. High structural barrier.
domain_priors:theater_ratio(north_sea_wind_grid, 0.15).       % Highly functional project; low performative activity.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(north_sea_wind_grid, extractiveness, 0.45).
narrative_ontology:constraint_metric(north_sea_wind_grid, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(north_sea_wind_grid, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(north_sea_wind_grid, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(north_sea_wind_grid). % Requires international treaties, funding laws, and regulatory oversight.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(north_sea_wind_grid, renewable_energy_consortiums).
narrative_ontology:constraint_beneficiary(north_sea_wind_grid, participating_governments).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(north_sea_wind_grid, fossil_fuel_industry).
narrative_ontology:constraint_victim(north_sea_wind_grid, taxpayers_of_member_states).
narrative_ontology:constraint_victim(north_sea_wind_grid, north_sea_fishing_industry).
%
% Gate requirements check for Tangled Rope:
%   - beneficiary: YES (renewable_energy_consortiums, participating_governments)
%   - victim: YES (fossil_fuel_industry, taxpayers...)
%   - requires_active_enforcement: YES
% All conditions met for Tangled Rope.

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

% PERSPECTIVE 1: THE FOSSIL FUEL INDUSTRY (PRIMARY TARGET)
% The constraint is designed to extract their market share. They see it as pure
% coercion with no coordinating benefit for them.
% d is derived high (victim + constrained exit). High χ → Snare.
constraint_indexing:constraint_classification(north_sea_wind_grid, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 2: TAXPAYERS OF MEMBER STATES (SECONDARY TARGET)
% They bear the initial financial burden and are structurally powerless.
% d is derived highest (victim + trapped exit). High χ → Snare.
constraint_indexing:constraint_classification(north_sea_wind_grid, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: THE RENEWABLE ENERGY CONSORTIUM (PRIMARY BENEFICIARY)
% The constraint creates a subsidized, protected market. For them, it is a
% perfect coordination mechanism.
% d is derived lowest (beneficiary + arbitrage exit). Negative χ → Rope.
constraint_indexing:constraint_classification(north_sea_wind_grid, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: PARTICIPATING GOVERNMENTS (INTER-INSTITUTIONAL BENEFICIARY)
% They also benefit, but are constrained by political commitments and public
% accountability, unlike the corporate actors.
% d is derived low, but higher than corporate beneficiary (beneficiary + constrained exit). Low χ → Rope.
constraint_indexing:constraint_classification(north_sea_wind_grid, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: THE ANALYTICAL OBSERVER
% Sees both the genuine coordination function (energy security, climate goals)
% and the asymmetric extraction (public-to-private wealth transfer, suppression).
% This synthesis reveals the Tangled Rope structure.
constraint_indexing:constraint_classification(north_sea_wind_grid, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(north_sea_wind_grid_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify perspectival gap between the primary target and primary beneficiary.
    constraint_indexing:constraint_classification(north_sea_wind_grid, snare, context(agent_power(organized), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(north_sea_wind_grid, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('Passed: Perspectival gap exists (Snare vs Rope).~n').

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(north_sea_wind_grid, tangled_rope, context(agent_power(analytical), _, _, _)),
    format('Passed: Analytical perspective correctly identifies Tangled Rope.~n').

test(tangled_rope_structural_gates_pass) :-
    narrative_ontology:constraint_beneficiary(north_sea_wind_grid, _),
    narrative_ontology:constraint_victim(north_sea_wind_grid, _),
    domain_priors:requires_active_enforcement(north_sea_wind_grid),
    format('Passed: All three structural requirements for Tangled Rope are declared.~n').

test(inter_institutional_nuance_captured, [nondet]) :-
    % Verify that the two institutional beneficiaries get different classifications or at least different χ values
    % due to their different exit options. The engine test harness would check χ values. Here we just check perspectives.
    constraint_indexing:constraint_classification(north_sea_wind_grid, _, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(north_sea_wind_grid, _, context(agent_power(institutional), _, exit_options(constrained), _)),
    format('Passed: Separate perspectives for institutional actors with different exit options are declared.~n').

:- end_tests(north_sea_wind_grid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.45): Set to a high value to reflect the significant transfer of public funds to private corporations. While a public good is produced, a substantial portion of the value is captured as private profit, constituting extraction from the taxpayer base.
 *   - Suppression (0.75): The project is not neutral; its existence is predicated on displacing and suppressing the fossil fuel industry. This is a core function, not a side effect, justifying a high score.
 *   - The combination of a necessary coordination function (beneficiary declaration) with high asymmetric extraction (victim declaration) and active enforcement makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For renewable energy firms (beneficiaries with arbitrage exit), it's a perfect Rope that creates a new, low-risk market. For the fossil fuel industry (victims with constrained exit), it's a perfect Snare designed to destroy their business model. Taxpayers (victims with trapped exit) also see a Snare in the form of costs imposed without their direct consent for diffuse, long-term benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation chain correctly models this complex landscape.
 *   - Beneficiaries: `renewable_energy_consortiums` and `participating_governments` receive low `d` values, resulting in low or negative effective extraction (χ).
 *   - Victims: `fossil_fuel_industry` and `taxpayers_of_member_states` receive high `d` values, resulting in high χ.
 *   The system correctly distinguishes between the two institutional beneficiaries: the corporations with `arbitrage` exit have a more favorable position (lower derived `d`) than the governments with `constrained` exit.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two potential errors. It is not a pure Rope, because that would ignore the massive public-to-private wealth transfer and the coercive suppression of an entire industry. It is not a pure Snare, because that would ignore the genuine, large-scale public good it produces (energy security, decarbonization). The Tangled Rope classification correctly identifies that it is BOTH a coordination mechanism AND an extractive one, preventing the mislabeling that often accompanies large public works.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_north_sea_wind_grid,
    'Will the long-term public benefits (climate mitigation, energy price stability) outweigh the short-term extraction (public cost, private profit capture)?',
    'Longitudinal studies (20-30 years) comparing electricity prices, carbon emissions, and total project ROI against initial projections.',
    'If benefits are high, the constraint is a highly effective Tangled Rope. If benefits are low or fail to materialize, the constraint degrades towards a Snare from the public perspective.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(north_sea_wind_grid, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. Since ε=0.45, this is a high-extraction
% constraint. We model a slight increase in both metrics over time, reflecting
% potential for cost overruns (increasing extraction) and political grandstanding
% (increasing theater) as the project matures.
%
% Theater ratio over time:
narrative_ontology:measurement(nswg_tr_t0, north_sea_wind_grid, theater_ratio, 0, 0.10).
narrative_ontology:measurement(nswg_tr_t5, north_sea_wind_grid, theater_ratio, 5, 0.12).
narrative_ontology:measurement(nswg_tr_t10, north_sea_wind_grid, theater_ratio, 10, 0.15).

% Extraction over time:
narrative_ontology:measurement(nswg_ex_t0, north_sea_wind_grid, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(nswg_ex_t5, north_sea_wind_grid, base_extractiveness, 5, 0.43).
narrative_ontology:measurement(nswg_ex_t10, north_sea_wind_grid, base_extractiveness, 10, 0.45).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This project is a massive undertaking in shared infrastructure.
narrative_ontology:coordination_type(north_sea_wind_grid, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural declarations
% of beneficiary/victim combined with the distinct exit_options for each agent
% allow the derivation engine to compute accurate and nuanced directionality
% values for all key perspectives.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */