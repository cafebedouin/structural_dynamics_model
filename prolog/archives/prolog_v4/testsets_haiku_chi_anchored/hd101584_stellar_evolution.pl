% ============================================================================
% CONSTRAINT STORY: hd101584_stellar_evolution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hd101584_stellar_evolution, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hd101584_stellar_evolution
 *   human_readable: The Gravitational Dynamics of the HD101584 Stellar System
 *   domain: astrophysics/stellar_evolution
 *
 * SUMMARY:
 *   HD101584 is a binary star system featuring a dying red giant (the
 *   primary) undergoing rapid mass loss and a smaller companion (likely a
 *   white dwarf, neutron star, or main-sequence star). The system exhibits
 *   complex circumstellar morphology with multiple dust shells and bipolar
 *   outflows, indicating ongoing or recent common envelope interaction. The
 *   gravitational dynamics of this system are entirely determined by the
 *   two-body problem in general relativity: the masses of the primary and
 *   companion, their orbital separation, and the stellar structure equations
 *   governing the red giant's evolution. No observer-dependent measurement
 *   methodology, no instrumental innovation, and no alternative analytical
 *   framework can change the fundamental fact that these are massive bodies
 *   interacting gravitationally in spacetime. The constraint is a pure
 *   mathematical/physical law — it is a Mountain.
 *
 * KEY AGENTS:
 *   - The Primary Red Giant: Physical entity (no agency) — the dying star whose evolution and mass loss are governed by stellar structure and nuclear timescales
 *   - The Companion Star: Physical entity (no agency) — orbits the primary according to Keplerian dynamics, absorbs transferred material, and eventually merges or escapes
 *   - The Gravitational Field: Immutable mediator (no agency) — carries no information about who observes it; constrains all trajectories identically
 *   - The Observational Astrophysicist: Institutional/powerful agent — can measure orbital parameters, spectral lines, and dust properties; cannot change the constraint
 *   - The Mission Planner: Institutional agent — must accept that HD101584 is inaccessible and can only be studied at light-distance across human timescales
 *   - The Graduate Researcher: Individual agent — bounded by the immutability of the system's evolution and the timescale of human dissertation work
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hd101584_stellar_evolution, 0.08).
domain_priors:suppression_score(hd101584_stellar_evolution, 0.02).
domain_priors:theater_ratio(hd101584_stellar_evolution, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hd101584_stellar_evolution, extractiveness, 0.08).
narrative_ontology:constraint_metric(hd101584_stellar_evolution, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(hd101584_stellar_evolution, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hd101584_stellar_evolution, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(hd101584_stellar_evolution, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hd101584_stellar_evolution, mountain).
narrative_ontology:human_readable(hd101584_stellar_evolution, "The Gravitational Dynamics of the HD101584 Stellar System").
narrative_ontology:topic_domain(hd101584_stellar_evolution, "astrophysics/stellar_evolution").

domain_priors:emerges_naturally(hd101584_stellar_evolution).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIZATIONAL ASTRONOMER (MOUNTAIN) — The gravitational dynamics of the HD101584 system are determined by general relativity, mass distributions, and orbital mechanics. These laws operate identically regardless of observer, epoch, or measurement methodology. The constraint emerges from the fundamental structure of spacetime and matter interaction. ε=0.08, suppression=0.02, accessibility_collapse=0.92, resistance=0.08. Pure natural law.
constraint_indexing:constraint_classification(hd101584_stellar_evolution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: OBSERVATIONAL ASTROPHYSICIST (MOUNTAIN) — Even with advanced telescopes, adaptive optics, and spectroscopic capability, the orbital dynamics of HD101584 are immutable constraints on what can be observed or inferred. The red giant's mass loss, the companion's orbital trajectory, and the binary evolution are governed by physical laws that do not yield to instrument choice, measurement technique, or analytical framework. The constraint is invariant across all observational contexts. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.06.
constraint_indexing:constraint_classification(hd101584_stellar_evolution, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: SPACE AGENCY MISSION PLANNER (MOUNTAIN) — HD101584 lies at ~2 kpc (6500 light-years) distance. This distance is an immutable feature of our local galactic structure. No mission planning, funding decision, or instrumental innovation can change the fundamental gravitational constraint: the system's orbital period (~540 years estimated), mass transfer rate, and chemical composition are determined by stellar physics, not by institutional capacity or strategic choice. The constraint is independent of human agency. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.01.
constraint_indexing:constraint_classification(hd101584_stellar_evolution, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GRADUATE STUDENT RESEARCHER (MOUNTAIN) — At the biographical timescale, the HD101584 system presents an immutable observational target. The red giant will evolve on a timescale of 10^4–10^5 years. The student's dissertation must work within the constraint that (1) the system cannot be reached, (2) its dynamics unfold over timescales exceeding human lifespans, and (3) theoretical predictions about mass transfer, common envelope evolution, and post-asymptotic-giant-branch morphology are not subject to negotiation or methodological revision — they follow from stellar structure equations. d≈0.75, f(d)≈1.10, σ=1.0 → χ≈0.09.
constraint_indexing:constraint_classification(hd101584_stellar_evolution, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hd101584_stellar_evolution_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(hd101584_stellar_evolution, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hd101584_stellar_evolution, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hd101584_stellar_evolution, ExtMetricName, E),
    domain_priors:suppression_score(hd101584_stellar_evolution, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hd101584_stellar_evolution),
    narrative_ontology:constraint_metric(hd101584_stellar_evolution, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hd101584_stellar_evolution, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hd101584_stellar_evolution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The HD101584 gravitational dynamics extract nothing from any observer — they are non-rivalrous, non-excludable, and invariant across all measurement contexts. The constraint is purely structural law. Suppression (0.02): Negligible. There is no coercion or suppression of alternatives because there are no alternatives to gravity. The laws of physics are the only operative framework. Theater ratio (0.15): Very low. Observational astronomy of HD101584 is almost entirely functional: spectroscopy measures actual physical quantities (radial velocities, chemical abundances, dust temperatures); imaging reveals actual morphology; the measurement ritual matches the underlying physics with minimal performative overhead. The small 0.15 value reflects only the minor gap between instrumental limitations and ideal perfect knowledge — this is inherent uncertainty, not theatrical performance. Accessibility collapse (0.92): Very high. The HD101584 system is governed by general relativity, stellar structure theory, and orbital mechanics — all fully determined by fundamental physical law. There is no degree of freedom for alternative interpretations, observer bias, or context-dependent framing. The constraint collapses all potential alternative accessibilities into a single immutable reality. Resistance (0.08): Very low. The system does not resist understanding — it yields to the application of standard physical law. The small resistance value reflects only practical measurement limitations (distance, faintness, crowding with stellar companions) and computational complexity in detailed simulations, not fundamental obstruction.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives converge on Mountain classification despite different power levels and timescales. There is no perspectival gap — this is the hallmark of a true natural law constraint. The civilizational astronomer sees universal applicability. The observational astrophysicist sees an invariant observational target. The mission planner sees an immutable distance and inaccessibility. The graduate researcher sees an immutable evolutionary timescale. All agree: the constraint is not negotiable, not subject to institutional decision, not variable across observers. The constraint's uniformity across perspectives validates its mountain status.
 *
 * DIRECTIONALITY LOGIC:
 *   Mountain constraints do not have directionality because they have no beneficiaries or victims — they are indifferent to all observers. Every agent occupies the same structural position relative to gravity: subject to it, constrained by it, with no option to extract value or bear cost. The constraint's directedness is not toward any observer, but toward the physical configuration of the system. Directionality would only arise if someone could benefit from gravity being strong vs weak, or if some agent could exit the constraint by switching to a non-gravitational context. Neither is possible. d=indeterminate (not applicable to mountains); the constraint operates identically for all d.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mass_transfer_mechanism_characterization,
    'Is the mass transfer in HD101584 driven by Roche lobe overflow, dust-driven winds, or collimated outflows? Does the mechanism change over the orbital period?',
    'Multi-wavelength spectroscopy (X-ray, optical, infrared, radio), high-resolution imaging from ALMA or future interferometers, detailed radiative transfer modeling of the circumstellar dust distribution',
    'Different mass transfer mechanisms have the same gravitational consequence (orbital decay, angular momentum loss) but imply different stellar composition distributions and dust chemistry. The fundamental constraint (stellar evolution under gravity) is unchanged; only the detailed observational signatures and timescale of common envelope phases are affected.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mass_transfer_mechanism_characterization, empirical, 'Mechanism of mass transfer (Roche lobe overflow vs wind-driven)').

omega_variable(
    companion_star_mass_degeneracy,
    'Is the HD101584 companion a white dwarf, neutron star, or low-mass M dwarf? Current mass estimates range from 0.3 to 1.4 solar masses.',
    'Radial velocity monitoring with high-precision spectroscopy, astrometric parallax refinement from Gaia or future missions, gravitational lensing analysis if detectable, X-ray/UV emission diagnostic to constrain compact object temperatures',
    'The companion''s mass determines the orbital decay timescale, the strength of tidal effects, and the likelihood of merger vs stable mass transfer. However, the gravitational constraint remains: whatever the companion''s mass, the two-body problem is fully determined. No new constraint type emerges; only the quantitative timescale changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(companion_star_mass_degeneracy, empirical, 'Companion star mass determination').

omega_variable(
    common_envelope_phase_escape,
    'Will the HD101584 system enter a common envelope phase, and if so, will the companion survive orbital decay to merger, or will it escape dynamically?',
    'High-resolution hydrodynamic simulations of common envelope evolution, observational detection of rapid orbital shrinkage, detection of merger precursor signals (gravitational waves for compact objects, electromagnetic transients for lower-mass companions)',
    'If escape occurs: the system remains as a wide binary or separated pair, and the constraint is relaxation toward stability. If merger occurs: the constraint evolves toward post-merger configurations (single star, planetary nebula, possible accretion-powered phenomena). In all cases, the gravitational constraint is the driver — no escape from physics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(common_envelope_phase_escape, empirical, 'Outcome of potential common envelope phase').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hd101584_stellar_evolution, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hd101_tr_t0, hd101584_stellar_evolution, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hd101_tr_t5, hd101584_stellar_evolution, theater_ratio, 5, 0.15).
narrative_ontology:measurement(hd101_tr_t10, hd101584_stellar_evolution, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(hd101_be_t0, hd101584_stellar_evolution, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(hd101_be_t5, hd101584_stellar_evolution, base_extractiveness, 5, 0.08).
narrative_ontology:measurement(hd101_be_t10, hd101584_stellar_evolution, base_extractiveness, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hd101584_stellar_evolution, global_infrastructure).
narrative_ontology:affects_constraint(hd101584_stellar_evolution, binary_orbital_decay_timescales).
narrative_ontology:affects_constraint(hd101584_stellar_evolution, common_envelope_merger_dynamics).
narrative_ontology:affects_constraint(hd101584_stellar_evolution, post_agb_nebula_morphology).

% DUAL FORMULATION NOTE:
% The HD101584 system is structurally upstream of several more specific constraints: the timescale of orbital decay (determined by gravitational dynamics), the mechanics of common envelope evolution (if it occurs), and the morphology of post-AGB planetary nebulae. All downstream constraints inherit their structure from the fundamental gravitational constraint described here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
