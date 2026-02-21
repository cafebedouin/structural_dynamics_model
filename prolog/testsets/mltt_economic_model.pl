% ============================================================================
% CONSTRAINT STORY: mltt_economic_model
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_mltt_economic_model, []).

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
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mltt_economic_model
 *   human_readable: "Major League Table Tennis Economic Model"
 *   domain: economic
 *
 * SUMMARY:
 *   This constraint represents the organizational and economic structure of the
 *   newly formed Major League Table Tennis (MLTT). It's a private, for-profit
 *   venture designed to professionalize and commercialize the sport in the
 *   United States. The structure coordinates players, teams, and media into a
 *   marketable product, while simultaneously creating a vehicle for investor
 *   profit by extracting value from player labor and audience attention.
 *
 * KEY AGENTS (by structural relationship):
 *   - aspiring_players: Primary target (powerless/trapped) — Aspiring pros with
 *     no leverage, for whom the league is the only viable path. They bear the
 *     highest effective extraction.
 *   - established_players: Secondary target (moderate/constrained) — Their labor
 *     generates the core value, and they are bound by league contracts and
 *     compensation structures, but have more leverage than newcomers.
 *   - league_investors_and_owners: Primary beneficiary (institutional/arbitrage) —
 *     They provide capital and governance with the expectation of financial
 *     returns, brand growth, and franchise value appreciation.
 *   - analytical_observer: Analytical observer — Sees the full dual-function
 *     structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mltt_economic_model, 0.48).
domain_priors:suppression_score(mltt_economic_model, 0.55).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(mltt_economic_model, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mltt_economic_model, extractiveness, 0.48).
narrative_ontology:constraint_metric(mltt_economic_model, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(mltt_economic_model, theater_ratio, 0.40).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(mltt_economic_model, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(mltt_economic_model). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(mltt_economic_model, league_investors_and_owners).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(mltt_economic_model, professional_players).
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
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE ASPIRING PLAYER (SNARE)
% For a player with no established career or leverage, the league is the only
% game in town. As a member of the 'victim' group with 'trapped' exit, the
% engine derives a d value near 1.0, maximizing effective extraction. From
% this view, the league is a pure Snare.
constraint_indexing:constraint_classification(mltt_economic_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE ESTABLISHED PLAYER (SNARE)
% As a member of the 'victim' group with constrained exit (their career path
% is tied to the league's success), the engine derives a high d value,
% amplifying the base extractiveness. From their view, the league is a highly
% coercive structure that controls their professional life.
constraint_indexing:constraint_classification(mltt_economic_model, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE LEAGUE INVESTOR (ROPE)
% As a member of the 'beneficiary' group with arbitrage exit (they can sell
% their stake), the engine derives a very low d value, resulting in a
% negative effective extraction (χ). From their view, the league is a pure
% coordination mechanism for creating value and financial returns.
constraint_indexing:constraint_classification(mltt_economic_model, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analytical observer sees the complete structure: a genuine coordination
% function (organizing the sport) combined with significant, asymmetric
% extraction (profit motive) and active enforcement (contracts). This
% combination of features defines a Tangled Rope.
constraint_indexing:constraint_classification(mltt_economic_model, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mltt_economic_model_tests).

test(perspectival_gap) :-
    % Verify the gap between the most vulnerable target and the beneficiary.
    constraint_indexing:constraint_classification(mltt_economic_model, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mltt_economic_model, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(mltt_economic_model, tangled_rope, context(agent_power(analytical), _, _, _)),
    narrative_ontology:constraint_beneficiary(mltt_economic_model, _),
    narrative_ontology:constraint_victim(mltt_economic_model, _),
    domain_priors:requires_active_enforcement(mltt_economic_model).

:- end_tests(mltt_economic_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): High, reflecting the for-profit nature of a
 *     professional sports league. The goal is significant return on investment,
 *     which is necessarily extracted from the value chain created by the sport.
 *   - Suppression (0.55): Moderate. The league doesn't yet have a monopoly, but
 *     its goal is to become the premier destination for top talent, thus
 *     suppressing rival leagues and controlling the career paths of players via
 *     exclusive contracts.
 *   - Theater Ratio (0.40): Significant but not dominant. The involvement of
 *     celebrity investors like Timothée Chalamet is a classic high-theater
 *     maneuver to generate buzz and legitimacy, but the league must also be
 *     functional to survive.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For an investor, the league is a pure Rope: a system for
 *   coordinating assets (players, venues, media) to generate a return. The
 *   extraction is seen as a legitimate profit. For a player, especially an
 *   aspiring one with no leverage (powerless/trapped), the system is a
 *   Snare: a binding structure that dictates their compensation and career,
 *   capturing the lion's share of the value their unique talent generates. They
 *   have no power to change the terms set by the capital-providers.
 *
 * DIRECTIONALITY LOGIC:
 *   The direction of extraction is from the `professional_players` (who create
 *   the on-court product) to the `league_investors_and_owners` (who provide
 *   the capital and organizational infrastructure). This is a classic
 *   labor/capital dynamic where capital structures the rules of the system
 *   to ensure its own return.
 *
 * MANDATROPHY ANALYSIS:
 *   This model avoids two common errors. A naive pro-business analysis might
 *   label the league a pure Rope, ignoring the coercive and extractive
 *   pressures on labor. A naive anti-capitalist analysis might label it a
 *   pure Snare, ignoring the genuine and complex coordination required to
 *   make a professional sports league function at all. The Tangled Rope
 *   classification correctly identifies that BOTH functions are present and
 *   intertwined. The coordination is real, and so is the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_mltt_economic_model,
    'Will the league structure provide a sustainable career path for players, or will its extractive pressure degrade into a "winner-take-all" market that benefits only a few stars and the owners?',
    'Empirical tracking of player salary distribution, career lengths, and league profitability over the next 5-10 years.',
    'If sustainable, it remains a functional Tangled Rope. If it becomes overly extractive, it may degrade into a pure Snare for the majority of its players.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(omega_mltt_economic_model, empirical, 'Long-term sustainability and fairness of the player compensation model vs. investor returns.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mltt_economic_model, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the league's maturation. It starts with high marketing theater and
% lower initial extraction (investment phase), then transitions to a stable,
% more profitable state where the core function dominates the theater.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(mltt_economic_model_tr_t0, mltt_economic_model, theater_ratio, 0, 0.60).
narrative_ontology:measurement(mltt_economic_model_tr_t5, mltt_economic_model, theater_ratio, 5, 0.45).
narrative_ontology:measurement(mltt_economic_model_tr_t10, mltt_economic_model, theater_ratio, 10, 0.40).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(mltt_economic_model_ex_t0, mltt_economic_model, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mltt_economic_model_ex_t5, mltt_economic_model, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(mltt_economic_model_ex_t10, mltt_economic_model, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% A sports league is fundamentally a mechanism for allocating resources
% (capital, talent, media attention, fan engagement) to create a product.
narrative_ontology:coordination_type(mltt_economic_model, resource_allocation).

% The creation of a new professional league directly impacts the existing
% labor market for athletes in that sport.
narrative_ontology:affects_constraint(mltt_economic_model, professional_athlete_labor_market).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation
% based on the declared beneficiary/victim groups and their associated exit
% options accurately models the structural relationships.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */