% ============================================================================
% CONSTRAINT STORY: gs_market_clearing
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_gs_market_clearing, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gs_market_clearing
 *   human_readable: Gale-Shapley Algorithm for Market Clearing
 *   domain: economic
 *
 * SUMMARY:
 *   The Gale-Shapley algorithm, and its variants, provide a mechanism for achieving stable matchings in two-sided markets (e.g., college admissions, residency placements, kidney exchange). The constraint ensures that no two agents would both prefer to be matched with each other, thus increasing the overall stability of the allocation. However, it can also create some inefficiencies by always favouring one side over another (the "proposing" side).
 *
 * KEY AGENTS (by structural relationship):
 *   - Market Participants: Primary target (powerless/trapped) — may experience sub-optimality in their match, especially on the non-proposing side.
 *   - Algorithm Designers & Central Authorities: Primary beneficiary (institutional/arbitrage) — benefits from the existence of a stable, predictable, and orderly matching mechanism.
 *   - Analytical observer — sees full structure as a hybrid coordination/extraction system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gs_market_clearing, 0.35).
domain_priors:suppression_score(gs_market_clearing, 0.45).   % Structural property (raw, unscaled). Increased to reflect the high cost of defecting from the central match.
domain_priors:theater_ratio(gs_market_clearing, 0.15).       % Low theater; the mechanism is highly functional.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gs_market_clearing, extractiveness, 0.35).
narrative_ontology:constraint_metric(gs_market_clearing, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(gs_market_clearing, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(gs_market_clearing, tangled_rope).
narrative_ontology:human_readable(gs_market_clearing, "Gale-Shapley Algorithm for Market Clearing").

% --- Binary flags ---
domain_priors:requires_active_enforcement(gs_market_clearing). % Required for Tangled Rope. The clearinghouse must enforce the matches.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(gs_market_clearing, algorithm_designers).
narrative_ontology:constraint_beneficiary(gs_market_clearing, central_authorities).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(gs_market_clearing, market_participants).
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

% PERSPECTIVE 1: THE PRIMARY TARGET (TANGLED ROPE)
% Agent who bears the most extraction (e.g., a medical student on the non-proposing side).
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% χ = 0.35 * 1.42 * 1.0 = 0.497. This falls into the Tangled Rope range (0.40 <= χ <= 0.90).
% It's not a Snare because the coordination function (a stable match) is still valuable.
constraint_indexing:constraint_classification(gs_market_clearing, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most (e.g., the clearinghouse). Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
% χ = 0.35 * -0.12 * 1.0 = -0.042. A clear Rope.
constraint_indexing:constraint_classification(gs_market_clearing, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.35 * 1.15 * 1.2 (global scope) = 0.483. This is a Tangled Rope.
% The observer sees both the valuable coordination and the asymmetric extraction.
constraint_indexing:constraint_classification(gs_market_clearing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gs_market_clearing_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(gs_market_clearing, TypeTarget,
        context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(gs_market_clearing, TypeBeneficiary,
        context(agent_power(institutional), _, exit_options(arbitrage), _)),
    TypeTarget \= TypeBeneficiary.

test(tangled_rope_conditions_met) :-
    % Verify that the structural conditions for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(gs_market_clearing, _),
    narrative_ontology:constraint_victim(gs_market_clearing, _),
    domain_priors:requires_active_enforcement(gs_market_clearing).

:- end_tests(gs_market_clearing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Gale-Shapley algorithm is a canonical example of a Tangled Rope. It provides a genuine, valuable coordination function: achieving a stable matching that prevents market unraveling. This is its "Rope" aspect. However, it also has inherent, asymmetric extraction: the "proposing" side of the market is mathematically guaranteed a weakly better outcome than the "receiving" side. This is its extractive aspect.
 *   Base extractiveness (ε=0.35) and suppression (0.45) are set to reflect this hybrid nature. The suppression is non-trivial because in many high-stakes markets (like medical residency), defecting from the centralized match is extremely costly or impossible.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the central authority (beneficiary), the system is a pure coordination Rope that creates order and stability. For a participant on the disadvantaged side of the match (target), it is a Tangled Rope that provides a necessary match but at the cost of a sub-optimal outcome they are forced to accept. The analytical observer sees both sides and classifies it as a Tangled Rope, acknowledging both the coordination and the extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Algorithm designers and central authorities (e.g., NRMP) are clear beneficiaries; the system's existence and stability is their primary function. Market participants are the victims, as they bear the costs of the algorithm's inherent asymmetry and lack of individual choice.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification correctly prevents mislabeling this coordination mechanism as a pure Snare. A Snare classification would ignore the immense value of achieving a stable match. Conversely, a pure Rope classification would ignore the non-negotiable, asymmetric costs imposed on one side of the market. The framework correctly identifies it as a system with both functions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_gs_market_clearing,
    'To what extent does the favoring of one side lead to significant, long-term welfare losses for the disadvantaged group?',
    'Longitudinal empirical analysis of career outcomes and satisfaction for both sides of the match.',
    'If welfare loss is high, the base extractiveness (ε) could be higher, pushing it closer to a Snare from the target perspective. If low, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(gs_market_clearing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not strictly required as base_extractiveness < 0.46,
% but is included to model the algorithm's adoption and solidification.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(gs_market_clearing_tr_t0, gs_market_clearing, theater_ratio, 0, 0.10).
narrative_ontology:measurement(gs_market_clearing_tr_t5, gs_market_clearing, theater_ratio, 5, 0.15).
narrative_ontology:measurement(gs_market_clearing_tr_t10, gs_market_clearing, theater_ratio, 10, 0.15).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(gs_market_clearing_ex_t0, gs_market_clearing, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(gs_market_clearing_ex_t5, gs_market_clearing, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(gs_market_clearing_ex_t10, gs_market_clearing, base_extractiveness, 10, 0.35).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(gs_market_clearing, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations accurately models the relationships in this system.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */