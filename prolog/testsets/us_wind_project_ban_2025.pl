% ============================================================================
% CONSTRAINT STORY: us_wind_project_ban_2025
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_us_wind_project_ban_2025, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:boltzmann_floor_override/2,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:omega_variable/3,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_wind_project_ban_2025
 *   human_readable: Executive Ban on New Wind Power Projects (2025)
 *   domain: political/economic
 *
 * SUMMARY:
 *   An executive order is enacted, banning all new onshore and offshore wind
 *   power projects within national jurisdiction. The stated rationale involves
 *   national security, environmental impact on avian life, and grid stability.
 *   The ban is later challenged and overturned by the judiciary. The constraint
 *   is the ban itself during the period it was in force.
 *
 * KEY AGENTS (by structural relationship):
 *   - Wind Power Developers: Primary target (powerless/trapped) — bears direct extraction of market opportunity.
 *   - Fossil Fuel Industry: Primary beneficiary (institutional/arbitrage) — benefits from suppressed competition.
 *   - US Judiciary: Institutional actor (institutional/constrained) — responsible for adjudicating the ban's legality.
 *   - Policy Analyst: Analytical observer — sees the full structure of coordination claims and extractive effects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_wind_project_ban_2025, 0.75).
domain_priors:suppression_score(us_wind_project_ban_2025, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(us_wind_project_ban_2025, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_wind_project_ban_2025, extractiveness, 0.75).
narrative_ontology:constraint_metric(us_wind_project_ban_2025, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(us_wind_project_ban_2025, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% This is a human-constructed policy, not a natural law. These are not applicable.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(us_wind_project_ban_2025, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(us_wind_project_ban_2025). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(us_wind_project_ban_2025, fossil_fuel_industry).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(us_wind_project_ban_2025, wind_power_developers).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are present)

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function. The engine derives d
   from beneficiary/victim membership + exit_options.
   Scope modifiers: national=1.0, global=1.2.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Wind Power Developers who are legally barred from initiating new projects.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
% χ ≈ 0.75 * 1.42 * 1.0 = 1.065. This is well into Snare territory (χ ≥ 0.66).
constraint_indexing:constraint_classification(us_wind_project_ban_2025, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Fossil fuel industry, which faces reduced competition from renewable sources.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
% χ ≈ 0.75 * -0.12 * 1.0 = -0.09. This classifies as a Rope.
constraint_indexing:constraint_classification(us_wind_project_ban_2025, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Policy analyst observing the dual function: a claimed coordination of energy
% policy and a clear, asymmetric extraction of value.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
% χ ≈ 0.75 * 1.15 * 1.2 = 1.035. While numerically high, the analytical
% perspective has access to the structural flags (beneficiary, victim, enforcement)
% which gates the classification to Tangled Rope.
constraint_indexing:constraint_classification(us_wind_project_ban_2025, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: INTER-INSTITUTIONAL ACTOR (TANGLED ROPE)
% The Judiciary is an institutional actor, but unlike the beneficiary, its
% exit options are 'constrained' by constitutional and statutory law. It
% cannot simply choose to ignore the case (arbitrage). This structural
% difference is key.
% Engine derives d from: no beneficiary/victim status + constrained exit -> falls back to power atom -> d ~ 0.5.
% The judiciary sees both the state's claimed power to regulate (coordinate)
% and the harm (extraction), classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(us_wind_project_ban_2025, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_wind_project_ban_2025_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    % Verify the core perspectival gap between the primary victim and beneficiary.
    constraint_indexing:constraint_classification(us_wind_project_ban_2025, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(us_wind_project_ban_2025, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(tangled_rope_analytical_classification) :-
    % Verify the analytical observer correctly identifies the Tangled Rope structure.
    constraint_indexing:constraint_classification(us_wind_project_ban_2025, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_are_met) :-
    % A constraint is only a Tangled Rope if it has both coordination and extraction, and requires enforcement.
    narrative_ontology:constraint_beneficiary(us_wind_project_ban_2025, _), % Implies coordination function
    narrative_ontology:constraint_victim(us_wind_project_ban_2025, _),     % Implies asymmetric extraction
    domain_priors:requires_active_enforcement(us_wind_project_ban_2025).

:- end_tests(us_wind_project_ban_2025_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): The ban directly expropriates the entire future market of new wind projects,
 *     transferring that opportunity to incumbent energy sources. This is a very high degree of extraction.
 *   - Suppression (0.80): The constraint is a legal absolute. It completely forecloses the alternative of
 *     building new wind farms, requiring significant state power to enforce.
 *   - Theater (0.20): While there are public rationales, the ban is functionally potent, not merely
 *     performative. Its primary purpose is to halt development, which it achieves.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For wind developers (powerless, trapped), the ban is a pure Snare, a coercive instrument
 *   that destroys their business opportunities with no upside. For the fossil fuel industry (institutional, arbitrage),
 *   it is a perfect Rope, coordinating the market to their advantage by removing a major competitor, thus reducing
 *   uncertainty and securing market share. This difference is driven entirely by their structural positions, which
 *   the directionality function `f(d)` correctly captures.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `fossil_fuel_industry`. They directly benefit from the elimination of a competitor, leading
 *     to higher prices and market stability for their products. This gives them a low `d` value.
 *   - Victim: `wind_power_developers`. They are the direct target, losing all potential revenue from new
 *     national projects. This gives them a high `d` value.
 *   The engine uses these declarations, combined with exit options, to compute the directionality `d` and thus
 *   the effective extraction `χ` for each perspective, generating the Snare/Rope dichotomy.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model captures the crucial difference between two institutional actors: the beneficiary (fossil fuel industry)
 *   and the adjudicator (judiciary). The beneficiary has `arbitrage` exit options; they can move capital elsewhere if
 *   the political winds change. The judiciary, however, has `constrained` exit; it must operate within the legal
 *   framework and cannot simply ignore the issue. This difference in exit options, even at the same `institutional`
 *   power level, reflects their different roles and results in the judiciary seeing the law's problematic dual-nature
 *   (Tangled Rope) rather than its pure benefit (Rope).
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] This classification correctly avoids mislabeling the policy as a pure coordination mechanism (Rope). While its
 *   proponents frame it as coordinating national energy policy, the high ε value and explicit `victim` declaration
 *   force the analytical perspective into `Tangled Rope`. This acknowledges the coordination *claim* while centering
 *   the asymmetric extraction as a core, undeniable feature of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_us_wind_project_ban_2025,
    'Was the primary intent of the ban to stabilize the grid (coordination) or to benefit the fossil fuel industry (extraction)?',
    'Release of internal executive branch communications and economic impact analyses conducted prior to the ban.',
    'If intent was coordination, the constraint is a poorly designed Tangled Rope. If intent was extraction, it is a Snare masquerading as a Tangled Rope.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
% The reporting engine reads narrative_ontology:omega_variable/3 with structure
% (ID, TypeClass, Description) where TypeClass is one of:
%   empirical   — resolvable by gathering more data
%   conceptual  — depends on definitional or theoretical framing
%   preference  — depends on value judgments or policy choices
% The /3 form is what the engine reads; /5 provides narrative context.
narrative_ontology:omega_variable(omega_us_wind_project_ban_2025, empirical, 'Uncertainty over whether the primary intent was grid coordination or industry protectionism.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(us_wind_project_ban_2025, 0, 2). % Short interval, representing a 2-year period.

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε=0.75 > 0.46), so temporal data is required.
% The metrics are modeled as stable during the short 2-year lifetime of the ban.

% Theater ratio over time:
narrative_ontology:measurement(us_wind_project_ban_2025_tr_t0, us_wind_project_ban_2025, theater_ratio, 0, 0.20).
narrative_ontology:measurement(us_wind_project_ban_2025_tr_t1, us_wind_project_ban_2025, theater_ratio, 1, 0.20).
narrative_ontology:measurement(us_wind_project_ban_2025_tr_t2, us_wind_project_ban_2025, theater_ratio, 2, 0.20).

% Extraction over time:
narrative_ontology:measurement(us_wind_project_ban_2025_ex_t0, us_wind_project_ban_2025, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(us_wind_project_ban_2025_ex_t1, us_wind_project_ban_2025, base_extractiveness, 1, 0.75).
narrative_ontology:measurement(us_wind_project_ban_2025_ex_t2, us_wind_project_ban_2025, base_extractiveness, 2, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The ban allocates the resource of "right to build energy infrastructure".
narrative_ontology:coordination_type(us_wind_project_ban_2025, resource_allocation).

% Network relationships (structural influence edges)
% This policy directly affects broader energy and climate policies.
narrative_ontology:affects_constraint(us_wind_project_ban_2025, us_climate_mitigation_goals).
narrative_ontology:affects_constraint(us_wind_project_ban_2025, us_energy_independence_policy).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation from
% beneficiary/victim declarations and exit options correctly models the
% structural relationships and generates the expected perspectival gaps.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */