% ============================================================================
% CONSTRAINT STORY: boom_bust_path_dependency
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-15
% ============================================================================

:- module(constraint_boom_bust_path_dependency, []).

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
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: boom_bust_path_dependency
 *   human_readable: The Heritage Fund Piton (Fiscal Volatility Path)
 *   domain: economic/policy
 *
 * SUMMARY:
 *   While sovereigntists blame Ottawa, Alberta's own choices—keeping royalties
 *   low, resisting a Norway-style savings fund, and under-investing in health
 *   and social services—created a self-inflicted fiscal vulnerability.
 *   This axis represents the atrophied function of provincial fiscal
 *   stabilization, now maintained through the "theater" of deficit-blaming
 *   and grievance politics.
 *
 * KEY AGENTS (by structural relationship):
 *   - future_generations: Primary target (powerless/trapped) — bears the cost
 *     of depleted resource wealth and under-funded services.
 *   - low_tax_constituencies: Primary beneficiary (organized/mobile) —
 *     benefited from "boom time" low taxes and high immediate consumption.
 *   - public_service_users: Secondary victim (moderate/constrained) —
 *     experiences service stretching during price shocks.
 *   - economic_historians: Analytical observer — compares Alberta's savings
 *     trajectory to peer resource economies.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(boom_bust_path_dependency, 0.22).
domain_priors:suppression_score(boom_bust_path_dependency, 0.08).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(boom_bust_path_dependency, 0.78).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(boom_bust_path_dependency, extractiveness, 0.22).
narrative_ontology:constraint_metric(boom_bust_path_dependency, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(boom_bust_path_dependency, theater_ratio, 0.78).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(boom_bust_path_dependency, piton).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(boom_bust_path_dependency, low_tax_constituencies).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(boom_bust_path_dependency, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE FUTURE GENERATION (PITON)
% As the primary target, future generations are trapped by the consequences of
% depleted resource wealth and under-funded services. They see an atrophied,
% performative system that failed its core stabilization function.
constraint_indexing:constraint_classification(boom_bust_path_dependency, piton,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE TAX-CUT SUPPORTER (ROPE)
% Viewed as a necessary coordination mechanism to keep the "Alberta Advantage"
% and attract investment through low royalties.
constraint_indexing:constraint_classification(boom_bust_path_dependency, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE FISCAL REALIST (PITON)
% Recognizes a former coordination tool (the Heritage Fund) that has atrophied
% into theatrical maintenance while wealth is drained.
constraint_indexing:constraint_classification(boom_bust_path_dependency, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (PITON)
% Identifies the atrophied function and high theater of "fiscal responsibility"
% rhetoric in a boom-bust cycle.
constraint_indexing:constraint_classification(boom_bust_path_dependency, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(boom_bust_path_dependency_tests).

test(perspectival_gap) :-
    % Verify the gap between beneficiaries (organized) and observers (institutional).
    constraint_indexing:constraint_classification(boom_bust_path_dependency, TypeBeneficiary, context(agent_power(organized), _, _, _)),
    constraint_indexing:constraint_classification(boom_bust_path_dependency, TypeObserver, context(agent_power(institutional), _, _, _)),
    TypeBeneficiary \= TypeObserver.

test(piton_signature) :-
    % Verify Piton requirements: theater_ratio >= 0.70.
    narrative_ontology:constraint_metric(boom_bust_path_dependency, theater_ratio, TR), TR >= 0.70.

test(low_extraction) :-
    % Pitons typically have low effective extraction but high inertial maintenance.
    narrative_ontology:constraint_metric(boom_bust_path_dependency, extractiveness, E), E =< 0.25.

:- end_tests(boom_bust_path_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Classified as a Piton because the "stabilization" function of the Heritage
 *   Fund and royalty system has largely atrophied. It is now maintained
 *   through high-theater rhetoric (0.78) that blames external forces (Ottawa)
 *   for internal decisions to privilege low taxes over long-term savings.
 *
 * PERSPECTIVAL GAP:
 *   The gap exists between those who enjoy immediate consumption/low taxes
 *   (seeing a Rope for growth) and those who observe the structural
 *   erosion of resilience (seeing a Piton).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the constituencies that prioritized "immediate
 *   consumption" during boom years. The victim group is 'future_generations',
 *   who inherit a fiscally vulnerable province with depleted resource wealth.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the current volatility as
 *   "natural" (Mountain). By identifying it as a Piton, we show it is the
 *   result of specific, atrophied provincial policy choices.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_royalty_restitution,
    'Could a retroactive change in royalty structure restore the atrophied stabilization function?',
    'Financial modeling of Norway-style royalty adjustments in the Alberta context.',
    'If yes, the Piton is resolvable; if no, the path dependency is permanent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(omega_royalty_restitution, conceptual, 'Reversibility of atrophied fiscal structures').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(boom_bust_path_dependency, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio rising as grievance politics replaces stabilization policy.
narrative_ontology:measurement(bbpd_tr_t0, boom_bust_path_dependency, theater_ratio, 0, 0.40).
narrative_ontology:measurement(bbpd_tr_t5, boom_bust_path_dependency, theater_ratio, 5, 0.60).
narrative_ontology:measurement(bbpd_tr_t10, boom_bust_path_dependency, theater_ratio, 10, 0.78).

% Extraction slowly increasing as short-term needs override long-term policy.
narrative_ontology:measurement(bbpd_ex_t0, boom_bust_path_dependency, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(bbpd_ex_t5, boom_bust_path_dependency, base_extractiveness, 5, 0.19).
narrative_ontology:measurement(bbpd_ex_t10, boom_bust_path_dependency, base_extractiveness, 10, 0.22).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(boom_bust_path_dependency, resource_allocation).

% Network edge: The vulnerability created by this Piton feeds the separatist
% "writing on the wall" narrative used in arbitrage.
narrative_ontology:affects_constraint(boom_bust_path_dependency, sovereignty_as_arbitrage).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */