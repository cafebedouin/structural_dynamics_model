% ============================================================================
% CONSTRAINT STORY: mars_rovers_navigational_autonomy
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_mars_rovers_navigational_autonomy, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:omega_variable/3,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mars_rovers_navigational_autonomy
 *   human_readable: Mars Surface Navigational Autonomy (AutoNav)
 *   domain: technological
 *
 * SUMMARY:
 *   Navigational autonomy on Mars rovers (e.g., Spirit, Opportunity) is
 *   constrained by signal latency (a Mountain) and the high risk of
 *   irrecoverable hardware loss. To overcome these, NASA's JPL implemented
 *   AutoNav, a coordination mechanism that allows rovers to sense hazards
 *   and plan paths without constant human intervention. While intended as a
 *   Rope to increase mission velocity, the software's conservative safety
 *   limits create a hybrid system that also extracts mission time and
 *   computational resources, functioning as a Tangled Rope.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Rover (Spirit/Opportunity): Primary target (powerless/trapped) — bears the cost of computational overhead and overly conservative pathing.
 *   - Rover Planners (JPL): Primary beneficiary (institutional/arbitrage) — benefits from increased mission safety and the ability to operate the rover despite communication delays.
 *   - Mission Efficiency Analyst: Analytical observer — sees the full structure of trade-offs between safety, speed, and scientific yield.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: 0.40. Moderate; autonomous cycles "extract" significant
% onboard CPU time and battery power (computational cost), potentially
% reducing time available for science instruments.
domain_priors:base_extractiveness(mars_rovers_navigational_autonomy, 0.40).

% Rationale: 0.50. The system suppresses high-risk maneuvers. If the
% "safety margin" is too high, it suppresses the visibility of valid
% but borderline navigation paths, forcing slower, safer routes.
domain_priors:suppression_score(mars_rovers_navigational_autonomy, 0.50).

% Rationale: 0.13. The system is primarily functional with low performative
% overhead. Its actions are directly tied to navigation and safety.
domain_priors:theater_ratio(mars_rovers_navigational_autonomy, 0.13).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mars_rovers_navigational_autonomy, extractiveness, 0.40).
narrative_ontology:constraint_metric(mars_rovers_navigational_autonomy, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(mars_rovers_navigational_autonomy, theater_ratio, 0.13).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(mars_rovers_navigational_autonomy, tangled_rope).
narrative_ontology:human_readable(mars_rovers_navigational_autonomy, "Mars Surface Navigational Autonomy (AutoNav)").
narrative_ontology:topic_domain(mars_rovers_navigational_autonomy, "technological").

% --- Binary flags ---
% Rationale: The onboard hazard avoidance software actively halts the rover
% if safety constraints (e.g., tilt angle, obstacle proximity) are violated.
domain_priors:requires_active_enforcement(mars_rovers_navigational_autonomy).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(mars_rovers_navigational_autonomy, mission_planners).
narrative_ontology:constraint_beneficiary(mars_rovers_navigational_autonomy, mission_longevity).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(mars_rovers_navigational_autonomy, the_rover).
narrative_ontology:constraint_victim(mars_rovers_navigational_autonomy, mission_science_yield).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function.
   The engine derives d from beneficiary/victim membership + exit_options.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE ROVER (TANGLED ROPE)
% Agent who bears the cost. For the rover, the software's safety parameters
% (e.g., "stop if tilt > 20°") are as unchangeable as Martian gravity. It has
% zero degrees of freedom to ignore its own "fear" of a hazard, making the
% constraint feel like a Mountain. However, its metrics (ε=0.40) place it
% as a Tangled Rope.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → high χ.
constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, tangled_rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ROVER PLANNERS (JPL) (ROPE)
% Agent who benefits most. Planners view autonomy as a Rope—a functional
% coordination tool. If the rover is too conservative, they can "loosen" the
% rope by adjusting thresholds, or "tighten" it in dangerous areas.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → low/negative χ.
constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE MISSION EFFICIENCY ANALYST (TANGLED ROPE)
% The analyst sees the full trade-off. The system has a clear coordination
% function (enabling missions) but also imposes significant extraction
% (lost time/power), which chokes the potential science yield.
% χ = 0.40 * f(d=0.72) * σ(global=1.2) ≈ 0.40 * 1.15 * 1.2 = 0.552.
% This χ value falls squarely in the Tangled Rope range (0.40 ≤ χ ≤ 0.90).
constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mars_autonomy_tests).

test(perspectival_gap) :-
    % Verify that institutional planners (Rope) and the rover itself
    % (Tangled Rope) have different classifications.
    constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, TypeTarget, context(agent_power(powerless), _, _, _)),
    TypeBeneficiary \= TypeTarget,
    TypeBeneficiary == rope,
    TypeTarget == tangled_rope.

test(tangled_rope_analytical_claim) :-
    % Verify the analytical perspective and constraint claim are Tangled Rope.
    constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, tangled_rope, context(agent_power(analytical), _, _, _)),
    narrative_ontology:constraint_claim(mars_rovers_navigational_autonomy, tangled_rope).

test(computational_extraction) :-
    % Extraction of battery/time is significant for Mars missions.
    narrative_ontology:constraint_metric(mars_rovers_navigational_autonomy, extractiveness, E),
    E > 0.3.

:- end_tests(mars_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.40) reflects the "tax" on mission time and
 *   resources. As noted in Bajracharya et al. (2008), autonomous navigation
 *   is significantly slower than blind driving because the rover must stop,
 *   image, and compute. The suppression score (0.50) reflects how this
 *   safety logic actively prevents the rover from considering potentially
 *   faster but riskier paths.
 *
 * PERSPECTIVAL GAP:
 *   JPL planners, who can modify the safety parameters, see a pure coordination
 *   tool (Rope) that enables the mission. The rover, which is bound by these
 *   parameters as if they were physical laws, experiences a highly coercive
 *   system (Tangled Rope). The analytical observer sees both sides: a
 *   necessary coordination function coupled with significant, asymmetric
 *   extraction of mission resources, the definition of a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are the mission planners and the overall mission
 *   longevity, as the system prevents catastrophic failure. The victims are
 *   the rover itself (whose agency is constrained) and the mission's
 *   potential science yield (constrained by slow traverse speeds and high
 *   computational overhead). This defines the asymmetric cost/benefit
 *   structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the system as a hybrid. A pure
 *   Snare classification would miss its essential coordination function (without
 *   it, the mission would be impossible or far riskier). A pure Rope
 *   classification would ignore the significant, non-negotiable costs imposed
 *   on the rover and the science objectives. The Tangled Rope classification
 *   captures this necessary but costly trade-off.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    autonomy_trust_threshold,
    'At what point does human "trust" in the algorithm allow the safety parameters to be relaxed enough for the system to become a net speed-enhancing tool versus a speed-limiting one?',
    'Comparative analysis of traverse speed in "AutoNav" vs. "Directed" modes on MER missions across varied terrain types.',
    'If Directed Driving is consistently faster, AutoNav is primarily extractive. If AutoNav allows more meters per sol in complex terrain, its coordination function is dominant.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(autonomy_trust_threshold, empirical, 'Is autonomous navigation a net positive or negative for traverse speed compared to human-directed driving?').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(mars_rovers_navigational_autonomy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The trade-offs of the AutoNav system were relatively stable during the
% main operational period of the Spirit and Opportunity rovers. The trajectory
% is therefore modeled as flat.
%
% Theater ratio over time:
narrative_ontology:measurement(mars_nav_tr_t0, mars_rovers_navigational_autonomy, theater_ratio, 0, 0.13).
narrative_ontology:measurement(mars_nav_tr_t5, mars_rovers_navigational_autonomy, theater_ratio, 5, 0.13).
narrative_ontology:measurement(mars_nav_tr_t10, mars_rovers_navigational_autonomy, theater_ratio, 10, 0.13).

% Extraction over time:
narrative_ontology:measurement(mars_nav_ex_t0, mars_rovers_navigational_autonomy, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(mars_nav_ex_t5, mars_rovers_navigational_autonomy, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(mars_nav_ex_t10, mars_rovers_navigational_autonomy, base_extractiveness, 10, 0.40).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The system allocates scarce resources (CPU time, power,
% mission time) between the competing demands of safe navigation and
% scientific data collection.
narrative_ontology:coordination_type(mars_rovers_navigational_autonomy, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The standard derivation from beneficiary/victim
% declarations accurately models the structural relationships.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */