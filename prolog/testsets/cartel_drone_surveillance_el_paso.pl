% ============================================================================
% CONSTRAINT STORY: cartel_drone_surveillance_el_paso
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cartel_drone_surveillance_el_paso
 *   human_readable: "Cartel Drone Surveillance Monopoly over El Paso Border Area"
 *   domain: geopolitical/technological
 *
 * SUMMARY:
 *   Mexican cartels have established a persistent drone surveillance network
 *   over the El Paso, Texas border region. This network provides them with
 *   real-time intelligence on US law enforcement movements, enabling them to
 *   secure smuggling routes for drugs and people. The system functions as a
 *   coercive, extractive mechanism, imposing costs on rivals, migrants, and
 *   US state actors while channeling benefits to the cartel.
 *
 * KEY AGENTS (by structural relationship):
 *   - Mexican Drug Cartels: Primary beneficiary (institutional/arbitrage) — benefits from intelligence and control.
 *   - Migrants & Rival Smugglers: Primary target (powerless/trapped) — bears costs of extortion and violence.
 *   - US Border Law Enforcement: Secondary/Institutional target (institutional/constrained) — bears costs of degraded operational effectiveness.
 *   - Analytical Observer: Analytical observer — sees the full extractive structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cartel_drone_surveillance_el_paso, 0.75).
domain_priors:suppression_score(cartel_drone_surveillance_el_paso, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(cartel_drone_surveillance_el_paso, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cartel_drone_surveillance_el_paso, extractiveness, 0.75).
narrative_ontology:constraint_metric(cartel_drone_surveillance_el_paso, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(cartel_drone_surveillance_el_paso, theater_ratio, 0.10).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint type.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(cartel_drone_surveillance_el_paso, snare).
narrative_ontology:human_readable(cartel_drone_surveillance_el_paso, "Cartel Drone Surveillance Monopoly over El Paso Border Area").

% --- Binary flags ---
domain_priors:requires_active_enforcement(cartel_drone_surveillance_el_paso). % Drones must be actively flown and monitored.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(cartel_drone_surveillance_el_paso, mexican_drug_cartels).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(cartel_drone_surveillance_el_paso, migrants_and_rival_smugglers).
narrative_ontology:constraint_victim(cartel_drone_surveillance_el_paso, us_border_law_enforcement).


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

% PERSPECTIVE 1: THE PRIMARY TARGET (MIGRANTS & RIVALS)
% For those trapped by the surveillance, it is a pure tool of extortion and
% coercion. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(cartel_drone_surveillance_el_paso, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (THE CARTEL)
% From the cartel's view, it's a superb coordination mechanism that reduces
% risk and increases profit. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
constraint_indexing:constraint_classification(cartel_drone_surveillance_el_paso, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The observer sees the high base extraction, high suppression, and the
% clear victim/beneficiary asymmetry, classifying it as a Snare.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
constraint_indexing:constraint_classification(cartel_drone_surveillance_el_paso, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---

% PERSPECTIVE 4: US LAW ENFORCEMENT (INSTITUTIONAL VICTIM)
% US LE is an institutional actor, but it is a victim of the constraint. Its
% operational effectiveness is degraded. Its exit is 'constrained' by its
% mandate to patrol the border, unlike the cartel's 'arbitrage' exit.
% Engine derives d from:
%  victim membership + constrained exit → d ≈ 0.6-0.7 → f(d) ≈ 0.9-1.1 → high χ
constraint_indexing:constraint_classification(cartel_drone_surveillance_el_paso, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cartel_drone_surveillance_el_paso_tests).

test(perspectival_gap_beneficiary_vs_victim, [nondet]) :-
    % Verify the gap between the cartel (Rope) and migrants (Snare).
    constraint_indexing:constraint_classification(cartel_drone_surveillance_el_paso, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(cartel_drone_surveillance_el_paso, snare, context(agent_power(powerless), _, exit_options(trapped), _)).

test(perspectival_gap_inter_institutional, [nondet]) :-
    % Verify the gap between the two institutional actors.
    constraint_indexing:constraint_classification(cartel_drone_surveillance_el_paso, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(cartel_drone_surveillance_el_paso, snare, context(agent_power(institutional), _, exit_options(constrained), _)).

test(snare_threshold_validation) :-
    domain_priors:base_extractiveness(cartel_drone_surveillance_el_paso, E),
    domain_priors:suppression_score(cartel_drone_surveillance_el_paso, S),
    E >= 0.46,
    S >= 0.60.

:- end_tests(cartel_drone_surveillance_el_paso_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): Extremely high. The surveillance network
 *     directly enables multi-billion dollar smuggling operations and facilitates
 *     extortion of migrants and local populations. The extraction is direct,
 *     coercive, and large-scale.
 *   - Suppression Score (0.80): Extremely high. The system's primary function
 *     is to suppress alternatives: rival smuggling routes are monitored and
 *     attacked, and law enforcement interdiction efforts are systematically
 *     neutralized. This creates a coercive monopoly.
 *   - Theater Ratio (0.10): Low. This is a highly functional system, not a
 *     performative one. The intelligence gathered is real and actionable.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the cartel (beneficiary), the drone network is a
 *   model of efficiency—a Rope that coordinates their complex logistics and
 *   reduces risk. For migrants and rival groups (targets), it is a Snare—an
 *   inescapable web of surveillance that enforces compliance through the
 *   threat of violence and extortion.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is unambiguous. The flow of benefits (money, control,
 *   intelligence) is entirely toward the `mexican_drug_cartels`. The costs
 *   (loss of life, money, freedom, and operational effectiveness) are borne
 *   by `migrants_and_rival_smugglers` and `us_border_law_enforcement`.
 *   The beneficiary/victim declarations directly model this asymmetric structure,
 *   allowing the engine to derive the correct directionality (d) for each actor.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story is a prime example of inter-institutional conflict mediated by a
 *   constraint. Both the cartel and US law enforcement are `institutional` actors,
 *   but they have opposing structural relationships to the surveillance network.
 *   The key differentiator is `exit_options`. The cartel has `arbitrage` exit;
 *   if drones become ineffective, they can switch to other tactics. US LE has a
 *   `constrained` exit; they are mandated to operate at the border and cannot
 *   simply withdraw. This difference in exit optionality is why the engine
 *   correctly derives a low `d` for the cartel (beneficiary) and a high `d` for
 *   US LE (victim), resulting in different classifications (Rope vs. Snare)
 *   despite both being 'institutional'.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] This case demonstrates the framework's ability to
 *   distinguish between internal coordination and system-wide extraction.
 *   While the drone network is a 'coordination' tool for the cartel, its
 *   function relative to the broader system is purely extractive and coercive.
 *   Classifying it as a Snare from the analytical and victim perspectives
 *   correctly identifies it as a parasitic structure, preventing the
 *   mislabeling of predatory control as a form of coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_cartel_drone_surveillance_el_paso,
    'Is the cartel drone surveillance advantage a durable monopoly, or a temporary technological edge that will be rapidly neutralized by counter-drone technology and tactics?',
    'Observation over the next 3-5 years of counter-drone deployment rates by US LE and their documented impact on cartel smuggling success rates.',
    'If durable, the Snare intensifies and may become a permanent feature of border security. If temporary, the constraint may be broken or degrade into a Piton (e.g., symbolic drone flights with little functional impact).',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cartel_drone_surveillance_el_paso, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. As a high-extraction constraint
% (ε > 0.46), its evolution is tracked. The drone network is a recent
% phenomenon that has rapidly intensified.
%
% Theater ratio over time (remains low and functional):
narrative_ontology:measurement(cdsep_tr_t0, cartel_drone_surveillance_el_paso, theater_ratio, 0, 0.10).
narrative_ontology:measurement(cdsep_tr_t5, cartel_drone_surveillance_el_paso, theater_ratio, 5, 0.10).
narrative_ontology:measurement(cdsep_tr_t10, cartel_drone_surveillance_el_paso, theater_ratio, 10, 0.10).

% Extraction over time (shows rapid intensification from experimentation to monopoly):
narrative_ontology:measurement(cdsep_ex_t0, cartel_drone_surveillance_el_paso, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(cdsep_ex_t5, cartel_drone_surveillance_el_paso, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(cdsep_ex_t10, cartel_drone_surveillance_el_paso, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% From the perspective of the beneficiary, this is an enforcement mechanism.
narrative_ontology:coordination_type(cartel_drone_surveillance_el_paso, enforcement_mechanism).

% Network relationships: this constraint is likely downstream from others, such as
% failures in state capacity or the global availability of cheap technology.
% For now, no direct link is declared.
% narrative_ontology:affects_constraint(state_capacity_failure_mexico, cartel_drone_surveillance_el_paso).
% narrative_ontology:affects_constraint(global_supply_chain_drones, cartel_drone_surveillance_el_paso).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation chain
% (beneficiary/victim declarations + exit_options) accurately computes the
% directionality (d) for all involved actors, correctly capturing the dynamics
% between the cartel, its victims, and the constrained institutional actor (US LE).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */