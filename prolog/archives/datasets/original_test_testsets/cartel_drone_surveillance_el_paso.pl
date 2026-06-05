% ============================================================================
% CONSTRAINT STORY: cartel_drone_surveillance_el_paso
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cartel_drone_surveillance_el_paso, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cartel_drone_surveillance_el_paso
 *   human_readable: Cartel Drone Surveillance Monopoly over El Paso Border Area
 *   domain: geopolitical/technological
 *
 * SUMMARY:
 *   A sophisticated and persistent drone surveillance network, operated by
 *   Mexican cartels, has established a de facto information monopoly over
 *   strategic corridors in the El Paso-Juárez border region. Using
 *   commercially available drones, this network conducts reconnaissance on
 *   law enforcement, monitors rival gangs, and facilitates smuggling
 *   operations. This creates a highly coercive environment, extracting
 *   security and freedom of movement from all other actors in the region. The
 *   system is not performative; it is a highly functional tool of territorial
 *   control and economic extraction, representing a non-state actor
 *   successfully deploying technology to suppress state authority.
 *
 * KEY AGENTS:
 *   - Cartel Leadership: Primary beneficiary (institutional/arbitrage) - Uses the network for command, control, and operational security.
 *   - Local Civilian Populations: Primary victim (powerless/trapped) - Subjected to surveillance, intimidation, and coercion.
 *   - US Border Patrol: Organized victim (organized/constrained) - The direct target of surveillance, which degrades their operational effectiveness.
 *   - Rival Criminal Organizations: Secondary victim (organized/trapped) - Targeted for elimination, their operations are suppressed by the dominant cartel's information advantage.
 *   - Commercial Drone Manufacturers: Unintentional beneficiary/victim (institutional/mobile) - Profit from sales but suffer reputational harm and regulatory risk.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cartel_drone_surveillance_el_paso, 0.75).
domain_priors:suppression_score(cartel_drone_surveillance_el_paso, 0.8).
domain_priors:theater_ratio(cartel_drone_surveillance_el_paso, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cartel_drone_surveillance_el_paso, extractiveness, 0.75).
narrative_ontology:constraint_metric(cartel_drone_surveillance_el_paso, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(cartel_drone_surveillance_el_paso, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cartel_drone_surveillance_el_paso, snare).
narrative_ontology:human_readable(cartel_drone_surveillance_el_paso, "Cartel Drone Surveillance Monopoly over El Paso Border Area").
narrative_ontology:topic_domain(cartel_drone_surveillance_el_paso, "geopolitical/technological").

domain_priors:requires_active_enforcement(cartel_drone_surveillance_el_paso).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cartel_drone_surveillance_el_paso, cartel_leadership).
narrative_ontology:constraint_beneficiary(cartel_drone_surveillance_el_paso, cartel_operators).
narrative_ontology:constraint_victim(cartel_drone_surveillance_el_paso, local_civilian_populations).
narrative_ontology:constraint_victim(cartel_drone_surveillance_el_paso, migrants_and_smugglers).
narrative_ontology:constraint_victim(cartel_drone_surveillance_el_paso, us_border_patrol).
narrative_ontology:constraint_victim(cartel_drone_surveillance_el_paso, mexican_authorities).
narrative_ontology:constraint_victim(cartel_drone_surveillance_el_paso, rival_criminal_organizations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE TRAPPED CIVILIAN (SNARE) — Subject to constant surveillance, with no ability to opt out or resist without extreme risk. Freedom of movement and security are extracted. d≈0.95, f(d)≈1.42, σ=0.9 → χ≈0.96. This is a textbook snare.
constraint_indexing:constraint_classification(cartel_drone_surveillance_el_paso, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE CARTEL LEADERSHIP (ROPE) — Experiences the drone network as a pure coordination tool for logistics, security, and enforcement. It solves collective action problems for the organization. d≈0.05, f(d)≈-0.12, σ=1.1 → χ≈-0.10. The negative effective extraction signifies a net subsidy.
constraint_indexing:constraint_classification(cartel_drone_surveillance_el_paso, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: US BORDER PATROL (SNARE) — An organized victim. While possessing significant resources, they are constrained by rules of engagement, technological gaps, and the sheer scale of the surveillance. The network actively extracts their operational effectiveness. d≈0.60, f(d)≈0.85, σ=1.0 → χ≈0.64. Just crosses the snare threshold.
constraint_indexing:constraint_classification(cartel_drone_surveillance_el_paso, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (SNARE) — Sees the full structure of coercion, suppression, and asymmetric extraction. The system's function is to enforce a monopoly through technological dominance, classifying it as a snare. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈1.04.
constraint_indexing:constraint_classification(cartel_drone_surveillance_el_paso, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: DRONE MANUFACTURER (TANGLED ROPE) — An unintentional beneficiary (through sales) and victim (reputational damage, pressure for regulation). The constraint is a hybrid: it coordinates sales but also creates an extractive, negative externality they are forced to manage. They are not trapped but cannot easily exit the market segment. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.59.
constraint_indexing:constraint_classification(cartel_drone_surveillance_el_paso, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cartel_drone_surveillance_el_paso_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cartel_drone_surveillance_el_paso, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cartel_drone_surveillance_el_paso, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cartel_drone_surveillance_el_paso, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cartel_drone_surveillance_el_paso_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75) is very high, representing the value extracted from controlling smuggling routes, extorting populations, and neutralizing law enforcement. Suppression (0.80) is also very high; the network's purpose is to eliminate alternatives and enforce the cartel's monopoly through the threat of violence informed by superior intelligence. Alternatives (moving freely, reporting crime, competing illicitly) are actively and violently suppressed. Theater Ratio (0.10) is low because the system is almost entirely functional. Its purpose is direct surveillance and control, not signaling or performance.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the cartel leadership (beneficiary), the network is a pure Rope, an elegant solution to the coordination problems of running a large criminal enterprise. For virtually every other actor, it is a Snare. The local resident is trapped under its gaze, and law enforcement is actively targeted by it. This gap highlights the core of Deferential Realism: the same object is experienced as a tool of coordination by its operator and a tool of coercion by its target.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived directly from the structural roles. Cartel Leadership (beneficiary, arbitrage exit) has a very low 'd' value, resulting in negative effective extraction (a net benefit). Local Populations (victim, trapped exit) have the highest possible 'd' value, experiencing maximum extraction. US Border Patrol (victim, constrained exit) experiences high, but not maximal, extraction, as they have organizational resources to mitigate some effects, unlike a powerless civilian. The logic follows the derivation chain without need for overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a straightforward resolution of mandatrophy. The claim that the drone network is a 'coordination tool' is only valid from the cartel's perspective. From all other perspectives, and especially the analytical one, its high suppression and extraction metrics make it an unambiguous Snare. The system's very existence relies on coercing others and suppressing their ability to operate. Any attempt to frame this as a neutral or beneficial technology (a Rope) is a perspectival error that ignores the structural violence inherent in its function. The mandatrophy is resolved by acknowledging the beneficiary's perspective while classifying the overall constraint based on its coercive structure as seen by its victims and the analytical observer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_asymmetry,
    'Is the cartel''s technological advantage persistent, or can state-level counter-drone technology effectively neutralize it at scale?',
    'Deployment and longitudinal effectiveness studies of military-grade counter-UAS systems in the border region.',
    'If the advantage is persistent, the Snare classification holds. If it can be neutralized, the constraint''s suppression and extractiveness would collapse, potentially degrading it to a Piton (attempted surveillance) or dissolving it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_asymmetry, empirical, 'Persistence of the technological gap between cartel and state actors.').

omega_variable(
    state_complicity_vs_incapacity,
    'To what degree does the surveillance network''s persistence depend on Mexican state corruption versus genuine state incapacity?',
    'Intelligence analysis of cartel-government interactions, corruption investigations, and comparative analysis with other regions.',
    'High complicity would imply the constraint is deeply embedded and supported by institutional actors, making it a more stable Snare. High incapacity suggests it''s a more fragile, purely technological constraint that could be overcome with external support.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_complicity_vs_incapacity, empirical, 'The role of state corruption vs. incapacity in enabling the network.').

omega_variable(
    escalation_threshold,
    'What is the political threshold for treating cartel drone incursions as a sovereign act of aggression rather than a criminal matter?',
    'Policy analysis, monitoring of political rhetoric, and observation of changes in military rules of engagement at the border.',
    'Crossing this threshold would fundamentally change the classification by introducing a powerful, institutional actor (the US military) with different exit options and power, potentially dissolving the constraint through overwhelming force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(escalation_threshold, preference, 'The political line between a law enforcement and military problem.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cartel_drone_surveillance_el_paso, 2021, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cart_tr_t0, cartel_drone_surveillance_el_paso, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cart_tr_t2, cartel_drone_surveillance_el_paso, theater_ratio, 2, 0.08).
narrative_ontology:measurement(cart_tr_t5, cartel_drone_surveillance_el_paso, theater_ratio, 5, 0.1).

% Extraction over time
narrative_ontology:measurement(cart_be_t0, cartel_drone_surveillance_el_paso, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(cart_be_t2, cartel_drone_surveillance_el_paso, base_extractiveness, 2, 0.65).
narrative_ontology:measurement(cart_be_t5, cartel_drone_surveillance_el_paso, base_extractiveness, 5, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cartel_drone_surveillance_el_paso, enforcement_mechanism).
narrative_ontology:affects_constraint(cartel_drone_surveillance_el_paso, us_mexico_border_policy).
narrative_ontology:affects_constraint(cartel_drone_surveillance_el_paso, regional_economic_stability_juarez).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
