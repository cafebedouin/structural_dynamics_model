% ============================================================================
% CONSTRAINT STORY: star_formation_barrier_g0253
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_star_formation_barrier_g0253, []).

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
 *   constraint_id: star_formation_barrier_g0253
 *   human_readable: Star Formation Barrier in the 'Brick' Cloud (G0.253+0.016)
 *   domain: physics/astrophysics
 *
 * SUMMARY:
 *   The dense molecular cloud G0.253+0.016, known as 'the Brick,' has
 *   sufficient mass and density to be a prolific stellar nursery. Yet, its
 *   star formation rate is surprisingly low, presenting a puzzle for
 *   astrophysicists. This discrepancy has led to various theories involving
 *   strong magnetic fields and turbulent dissipation which inhibit
 *   gravitational collapse. This situation represents a tangled rope
 *   constraint, where observational data both supports and challenges
 *   theoretical frameworks, and where different researchers benefit or are
 *   hindered depending on their chosen approach.
 *
 * KEY AGENTS:
 *   - Naive star formation theories: (Powerless/Trapped) Consistently fail to match observations.
 *   - Observational Astronomers: (Moderate/Constrained) Constrained by observational limits but benefit from improved models.
 *   - Magnetic field support: (Institutional/Arbitrage) Benefit from the Brick supporting their magnetic field theories.
 *   - Turbulent energy dissipation models: (Institutional/Arbitrage) Benefit from the Brick motivating and allowing testing of their theories.
 *   - Analytical Observer: (Analytical/Analytical) Observe interplay, without attachment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(star_formation_barrier_g0253, 0.55).
domain_priors:suppression_score(star_formation_barrier_g0253, 0.65).
domain_priors:theater_ratio(star_formation_barrier_g0253, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(star_formation_barrier_g0253, extractiveness, 0.55).
narrative_ontology:constraint_metric(star_formation_barrier_g0253, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(star_formation_barrier_g0253, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(star_formation_barrier_g0253, tangled_rope).
narrative_ontology:human_readable(star_formation_barrier_g0253, "Star Formation Barrier in the 'Brick' Cloud (G0.253+0.016)").
narrative_ontology:topic_domain(star_formation_barrier_g0253, "physics/astrophysics").

domain_priors:requires_active_enforcement(star_formation_barrier_g0253).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(star_formation_barrier_g0253, magnetic_field_support).
narrative_ontology:constraint_beneficiary(star_formation_barrier_g0253, turbulent_energy_dissipation_models).
narrative_ontology:constraint_victim(star_formation_barrier_g0253, naive_star_formation_theories).
narrative_ontology:constraint_victim(star_formation_barrier_g0253, observational_astronomers_modeling_star_formation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Naive star formation theories (Snare). These theories, lacking detailed consideration of turbulence and magnetic fields, are consistently undermined by observations of 'the Brick'. The theories are 'trapped' in the sense that they cannot easily adapt to the cloud's peculiar properties, leading to continued failures in predicting star formation rates. Extraction is high as they are unable to explain the observed phenomena.
constraint_indexing:constraint_classification(star_formation_barrier_g0253, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: Observational astronomers modeling star formation (Tangled Rope). These astronomers are constrained by the limitations of current observational techniques and computational resources. They benefit from the challenge posed by 'the Brick', as it drives the development of more sophisticated models and observational strategies. However, they also bear the cost of increased complexity and uncertainty. Experience both coordination and extraction.
constraint_indexing:constraint_classification(star_formation_barrier_g0253, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective 3: Researchers supporting the magnetic field explanation (Rope). Those researchers who propose the importance of strong magnetic fields as a support mechanism in molecular clouds benefit from this star formation suppression. They can use the Brick to further their arguments and improve their models of star formation, and have ways of supporting their claim via observations (Zeeman splitting) and models. Coordinate around a specific theory
constraint_indexing:constraint_classification(star_formation_barrier_g0253, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% Perspective 4: Researchers supporting turbulent energy dissipation models. (Rope). Those researchers who propose turbulent dissipation benefit, as G0.253+0.016 motivates and allows for the testing of their theory. Arbitrage due to opportunity to support claim using new data.
constraint_indexing:constraint_classification(star_formation_barrier_g0253, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% Perspective 5: Analytical Observer (Tangled Rope). Viewing the system analytically reveals the tug-of-war between different theoretical frameworks attempting to explain the anomaly of G0.253+0.016, but each framework itself having limits of what it can explain, so none are perfect.
constraint_indexing:constraint_classification(star_formation_barrier_g0253, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(star_formation_barrier_g0253_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(star_formation_barrier_g0253, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(star_formation_barrier_g0253, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(star_formation_barrier_g0253, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(star_formation_barrier_g0253_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderately high (0.55) because the observed lack of star formation extracts explanatory power from simpler theoretical models. Suppression is also moderately high (0.65) as the need for specialized theories based on turbulence or magnetic fields limits the application of general star formation principles. Theater ratio is low (0.20) as theoretical activity has a strong, verifiable basis and is not mostly performative.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the different relationships researchers have to the puzzle. Researchers supporting magnetic fields or turbulence see the Brick as a success story for their models (rope), while those clinging to simpler star formation theories find their models consistently contradicted (snare). Observational astronomers lie somewhere in between (tangled rope), as their observations can both support and challenge different theoretical frameworks.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the beneficiaries and victims. Proponents of magnetic fields benefit from the star formation suppression, whereas observational astronomers are at the whim of their theoretical framework being successful. The Analytical Observer remains unaligned to any specific theoretical claim.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    magnetic_field_strength,
    'What is the precise strength and configuration of the magnetic field within ''the Brick''?',
    'Improved Zeeman splitting measurements and high-resolution polarimetric observations.',
    'If the magnetic field is strong and pervasive, it supports the magnetic field support theory. If weak or disorganized, then other mechanisms like turbulence must be invoked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magnetic_field_strength, empirical, 'Uncertainty surrounding the magnetic field strength.').

omega_variable(
    turbulence_dissipation_rate,
    'What is the rate at which turbulent energy is dissipating within ''the Brick''?',
    'Detailed analysis of molecular line widths and spatial distribution of turbulence.',
    'If the dissipation rate is high, turbulence may be the primary mechanism for preventing star formation. If low, other mechanisms like magnetic fields or feedback from nearby stars must be considered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(turbulence_dissipation_rate, empirical, 'Uncertainty surrounding the rate of turbulent energy dissipation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(star_formation_barrier_g0253, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(star_tr_t0, star_formation_barrier_g0253, theater_ratio, 0, 0.1).
narrative_ontology:measurement(star_tr_t5, star_formation_barrier_g0253, theater_ratio, 5, 0.15).
narrative_ontology:measurement(star_tr_t10, star_formation_barrier_g0253, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(star_be_t0, star_formation_barrier_g0253, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(star_be_t5, star_formation_barrier_g0253, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(star_be_t10, star_formation_barrier_g0253, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(star_formation_barrier_g0253, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
