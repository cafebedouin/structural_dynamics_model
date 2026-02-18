% ============================================================================
% CONSTRAINT STORY: cuban_missile_crisis_excomm_deliberation
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_cuban_missile_crisis_excomm_deliberation, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cuban_missile_crisis_excomm_deliberation
 *   human_readable: The ExComm Multi-Channel Deliberation Protocol
 *   domain: political/military
 *
 * SUMMARY:
 *   Following the failure of the Bay of Pigs, President Kennedy established
 *   the Executive Committee of the National Security Council (ExComm). This
 *   constraint story models the ExComm protocol itself as a deliberate
 *   "anti-silo" mechanism. It forced decision-makers to weigh multiple
 *   competing options—primarily a naval blockade versus a direct air
 *   strike—under the existential threat of Nuclear Mutually Assured
 *   Destruction (MAD). The protocol functioned to prevent groupthink and
 *   create a path for de-escalation.
 *
 *   CRITICAL DISTINCTION (ε-Invariance): This story models the *protocol*,
 *   not the background threat of nuclear war. The protocol is a low-extraction
 *   coordination mechanism (ε ≈ 0.20). The threat of MAD is a separate,
 *   high-extraction constraint (likely a Mountain or Snare) that this
 *   protocol was designed to navigate. Conflating the two would violate the
 *   ε-invariance principle.
 *
 * KEY AGENTS (by structural relationship):
 *   - Global Population: Primary beneficiary (powerless/trapped) — benefits from the avoidance of nuclear war.
 *   - The "Doves" (e.g., Robert Kennedy, McNamara): Primary beneficiary (powerful/mobile) — successfully used the protocol to achieve their de-escalation goals.
 *   - The "Hawks" (e.g., JCS, Gen. LeMay): Constrained party (institutional/constrained) — the protocol prevented their preferred military action.
 *   - Castro Regime: Primary victim (organized/trapped) — their national autonomy was a bargaining chip in the resolution.
 *   - Analytical Observer: Sees the full structure of the protocol as a coordination device.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cuban_missile_crisis_excomm_deliberation, 0.20).
domain_priors:suppression_score(cuban_missile_crisis_excomm_deliberation, 0.15).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(cuban_missile_crisis_excomm_deliberation, 0.16).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cuban_missile_crisis_excomm_deliberation, extractiveness, 0.20).
narrative_ontology:constraint_metric(cuban_missile_crisis_excomm_deliberation, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(cuban_missile_crisis_excomm_deliberation, theater_ratio, 0.16).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(cuban_missile_crisis_excomm_deliberation, rope).
narrative_ontology:human_readable(cuban_missile_crisis_excomm_deliberation, "The ExComm Multi-Channel Deliberation Protocol").
narrative_ontology:topic_domain(cuban_missile_crisis_excomm_deliberation, "political/military").

% --- Binary flags ---
domain_priors:requires_active_enforcement(cuban_missile_crisis_excomm_deliberation). % JFK intentionally absent from meetings to encourage dissent.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(cuban_missile_crisis_excomm_deliberation, global_population).
narrative_ontology:constraint_beneficiary(cuban_missile_crisis_excomm_deliberation, us_political_leadership).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(cuban_missile_crisis_excomm_deliberation, castro_regime_autonomy).

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

% PERSPECTIVE 1: THE GLOBAL CIVILIAN (BENEFICIARY)
% The protocol is a Rope that coordinates powerful actors to prevent global
% annihilation. While civilians are trapped by the background threat of MAD,
% this specific constraint (the protocol) works in their favor.
constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_deliberation, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE "DOVES" (ARCHITECTS/BENEFICIARIES)
% For RFK and McNamara, the ExComm process was a quintessential Rope—a
% coordination mechanism to create communication channels and de-escalation
% options where a binary choice (strike/capitulate) seemed imminent.
constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_deliberation, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE "HAWKS" (CONSTRAINED PARTY)
% The Joint Chiefs viewed the missiles as a military fact requiring a military
% solution. The ExComm protocol was a constraint that forced them into a
% political coordination game, preventing their preferred action. It was a
% Rope that channeled their behavior away from immediate conflict.
constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_deliberation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% From a historical distance, the ExComm protocol is seen as an ideal Rope—a
% system designed to prevent the Snare of a binary choice (Total Surrender vs.
% Total War) by creating a middle-path mechanism for de-escalation.
constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_deliberation, rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cuban_missile_crisis_excomm_deliberation_tests).

test(perspective_agreement) :-
    % Verify that key perspectives agree on the classification, demonstrating
    % the uniform-type nature of this pure coordination constraint.
    constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_deliberation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_deliberation, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == rope,
    TypeInstitutional == rope.

test(low_suppression_verification) :-
    domain_priors:suppression_score(cuban_missile_crisis_excomm_deliberation, S),
    S < 0.2.

:- end_tests(cuban_missile_crisis_excomm_deliberation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.20) and suppression (0.15) are low because
 *   this story models the ExComm deliberation *protocol*, not the overall
 *   crisis. The protocol was explicitly designed to *reduce* suppression of
 *   dissenting views (avoiding Bay of Pigs groupthink) and coordinate a
 *   de-escalatory path that benefited the global population. Its function
 *   was pure coordination.
 *
 * PERSPECTIVAL GAP:
 *   This constraint is a uniform-type Rope, meaning there is no significant
 *   perspectival gap. All actors, whether they are beneficiaries (Doves,
 *   civilians) or constrained by it (Hawks), experience it as a coordination
 *   mechanism. The Hawks are forced to coordinate when they would rather
 *   not, but the mechanism itself is still a Rope. The initial temptation to
 *   classify this as a Snare from the powerless perspective is an error of
 *   misattribution: the powerless are trapped by the background threat of
 *   Mutually Assured Destruction (a separate constraint), not by the ExComm
 *   protocol that was working to save them.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: The 'global_population' and 'us_political_leadership'
 *     benefited from a non-nuclear outcome. The engine assigns them a low
 *     directionality (d), resulting in low effective extraction (χ).
 *   - Victims: The 'castro_regime_autonomy' was sacrificed as part of the
 *     deal (removal of missiles from Turkey and Cuba). They bear the cost of
 *     the coordination, giving them a higher d.
 *
 * MANDATROPHY ANALYSIS:
 *   The ε-invariance principle is critical here. By strictly modeling the
 *   protocol (ε=0.20), we avoid mislabeling this Rope as a Snare. An analyst
 *   who conflates the protocol with the background threat of MAD might assign
 *   a high ε, leading to a false Snare classification and the incorrect
 *   conclusion that the deliberation process itself was extractive. This
 *   model correctly identifies the protocol as a coordination solution to an
 *   external high-extraction problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_cuban_missile_crisis_excomm_deliberation,
    "Would Khrushchev have actually launched missiles if a 'surgical' US air strike had occurred?",
    "Cross-referencing declassified Politburo minutes with field-level Soviet operational orders in Cuba.",
    "If Yes: The ExComm protocol was an indispensable Rope. If No: The Hawks' perception of an inevitable escalation was an illusion, and the crisis was less severe than perceived.",
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_cuban_missile_crisis_excomm_deliberation, empirical, "Uncertainty over Soviet command-and-control and response doctrine during the crisis.").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cuban_missile_crisis_excomm_deliberation, 0, 13). % The "Thirteen Days"

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required as base_extractiveness (0.20) is below the 0.46 threshold.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The protocol was an enforcement mechanism for a specific mode of deliberation.
narrative_ontology:coordination_type(cuban_missile_crisis_excomm_deliberation, enforcement_mechanism).

% --- Network Decomposition (Constraint Families) ---
%
% DUAL FORMULATION NOTE:
% This constraint is one of two stories decomposed from the colloquial label
% "The Cuban Missile Crisis". Decomposed because ε differs across observables.
% This story models the low-ε coordination protocol. A separate story would
% model the high-ε background threat.
%
% Related stories:
%   - nuclear_mutually_assured_destruction (ε≈0.95, Mountain/Snare)
%
% narrative_ontology:affects_constraint(nuclear_mutually_assured_destruction, cuban_missile_crisis_excomm_deliberation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations accurately models the relationships.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */