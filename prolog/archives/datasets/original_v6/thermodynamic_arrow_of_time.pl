% ============================================================================
% CONSTRAINT STORY: thermodynamic_arrow_of_time
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_thermodynamic_arrow_of_time, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: thermodynamic_arrow_of_time
 *   human_readable: The Thermodynamic Arrow of Time
 *   domain: physics/thermodynamics/cosmology
 *
 * SUMMARY:
 *   The thermodynamic arrow of time is the irreversible increase of entropy
 *   in closed systems, which provides the phenomenological directedness of
 *   time: past has lower entropy than future. This constraint arises from the
 *   second law of thermodynamics and its statistical mechanical foundation.
 *   Unlike extraction constraints that benefit some agents at the cost of
 *   others, the thermodynamic arrow is a natural law that applies uniformly
 *   to all matter and energy. No agent can escape it, negotiate with it, or
 *   benefit from asymmetry in how it applies. The constraint exhibits zero
 *   degrees of freedom across all perspectives and all scales. It is perhaps
 *   the purest example of a mountain-type constraint in the physical
 *   sciences.
 *
 * KEY AGENTS:
 *   - All thermodynamic systems: Equally subject to entropy increase (universal/trapped)
 *   - Biological organisms: Experience the arrow as directedness of metabolism and aging (powerless/trapped)
 *   - Technological institutions: Cannot evade entropy increase despite optimization efforts (institutional/arbitrage)
 *   - Analytical observers: Recognize the statistical mechanical basis for irreversibility (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(thermodynamic_arrow_of_time, 0.08).
domain_priors:suppression_score(thermodynamic_arrow_of_time, 0.03).
domain_priors:theater_ratio(thermodynamic_arrow_of_time, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(thermodynamic_arrow_of_time, extractiveness, 0.08).
narrative_ontology:constraint_metric(thermodynamic_arrow_of_time, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(thermodynamic_arrow_of_time, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(thermodynamic_arrow_of_time, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(thermodynamic_arrow_of_time, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(thermodynamic_arrow_of_time, mountain).
narrative_ontology:human_readable(thermodynamic_arrow_of_time, "The Thermodynamic Arrow of Time").
narrative_ontology:topic_domain(thermodynamic_arrow_of_time, "physics/thermodynamics/cosmology").

domain_priors:emerges_naturally(thermodynamic_arrow_of_time).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ENTROPIC OBSERVER (MOUNTAIN) — All thermodynamic systems experience the arrow of time as an absolute constraint. No exit, no alternative, no negotiation. Entropy increases; disorder is irreversible on macroscopic scales. This is not extractive in the traditional sense — it is a law of nature that applies equally to all matter and energy.
constraint_indexing:constraint_classification(thermodynamic_arrow_of_time, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE THERMODYNAMIC AGENT (MOUNTAIN) — At the scale of human experience and technological systems, the arrow of time is immutable. Heat flows from hot to cold; useful energy dissipates; disorder increases. No cost negotiation is possible. The constraint operates identically whether the agent is aware of it or powerful or powerless.
constraint_indexing:constraint_classification(thermodynamic_arrow_of_time, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN) — The second law of thermodynamics and the statistical mechanics foundation for the arrow of time are mathematically rigorous. The increase in entropy follows from the combinatorial vastness of high-entropy microstates vs. low-entropy microstates in any closed or isolated system. This is not a constraint that could be negotiated, circumvented, or extracted from — it is a structural feature of statistical reality.
constraint_indexing:constraint_classification(thermodynamic_arrow_of_time, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE TECHNOLOGICAL INSTITUTION (MOUNTAIN) — Even institutions that optimize energy efficiency (power plants, computing infrastructure, HVAC systems) cannot escape the thermodynamic arrow. All work done requires energy input; all energy input generates waste heat and entropy. The constraint applies to all technological systems without exception or negotiation.
constraint_indexing:constraint_classification(thermodynamic_arrow_of_time, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(thermodynamic_arrow_of_time_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(thermodynamic_arrow_of_time, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(thermodynamic_arrow_of_time, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(thermodynamic_arrow_of_time, ExtMetricName, E),
    domain_priors:suppression_score(thermodynamic_arrow_of_time, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(thermodynamic_arrow_of_time),
    narrative_ontology:constraint_metric(thermodynamic_arrow_of_time, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(thermodynamic_arrow_of_time, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(thermodynamic_arrow_of_time_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The thermodynamic arrow does not extract value from some agents to benefit others. It is an equal-application natural law. The small non-zero value reflects that entropy production can be quantified as cost to any system, but this cost is not asymmetric extraction — it applies identically to all. Suppression (0.03): Negligible. There are no alternatives to suppress because no alternatives exist. The arrow cannot be negotiated, circumvented, or weakened through any physical process. Theater ratio (0.15): Very low. The second law is not performative — entropy actually increases; it is not a ritual that could be replaced with a substitute. The small value reflects only the layer of scientific explanation and modeling required to discuss the phenomenon.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives classify identically as mountain. This uniformity is the defining feature of a natural law constraint. No agent's position, power, time horizon, or scope changes their relationship to the thermodynamic arrow. A powerless agent trapped by it, a moderate agent experiencing it, an institutional actor managing systems governed by it, and an analytical observer studying it all encounter the same immutable constraint. The absence of a perspectival gap is itself the diagnostic signature of a mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality analysis is required for a mountain constraint. All agents have d ≈ 1.0 (equally exposed to the constraint) with no asymmetry in benefits or costs. The sigmoid f(d) is not computed because there is no beneficiary/victim relationship — entropy increase is not extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The thermodynamic arrow of time presents no mandatrophy because it is a mountain-type constraint with zero extractive content. The question of whether this is 'really' a constraint in the DR sense is legitimate: it is not a social or institutional arrangement that could be reformed or negotiated. However, the framework includes it to establish the baseline against which extraction constraints are measured. The thermodynamic arrow is the zero point: extractiveness 0.08, suppression 0.03, uniformity across all perspectives. Any constraint that resembles the natural law in structure but operates through institutional mechanisms (false summits) can be compared against this baseline.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    time_asymmetry_fundamental_origin,
    'Does the thermodynamic arrow of time emerge from fundamental physics asymmetries (CP violation, neutrino oscillations, electroweak symmetry breaking) or from boundary conditions and statistical mechanics alone?',
    'Analysis of whether time-asymmetric dynamics at the particle level are necessary to explain thermodynamic irreversibility vs. whether pure statistics of symmetric microstates suffices',
    'If fundamental: the arrow may not be perfectly universal at all scales. If statistical: the arrow is robust and platform-independent, but questions arise about initial condition asymmetry (why was the early universe low-entropy?).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(time_asymmetry_fundamental_origin, empirical, 'Origin of time asymmetry: fundamental physics vs statistical mechanics').

omega_variable(
    closed_system_assumption,
    'Is the thermodynamic arrow truly universal, or is it an artifact of treating macroscopic systems as isolated when they are actually coupled to vast thermal reservoirs?',
    'Investigation of whether perfectly isolated quantum systems exhibit irreversibility or whether the arrow emerges only when tracing over inaccessible degrees of freedom',
    'If artifact: the arrow is observer-dependent and conditional on coarse-graining — potentially modifying the mountain classification. If fundamental: the mountain stands regardless of isolation assumptions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(closed_system_assumption, conceptual, 'Whether thermodynamic arrow is universal or observer-relative via coarse-graining').

omega_variable(
    quantum_coherence_escape_route,
    'Could quantum coherence in macroscopic systems allow escape from thermodynamic irreversibility in principle?',
    'Examination of quantum revivals, coherence protection mechanisms, and thermodynamic limits on quantum error correction',
    'If yes: quantum systems might exhibit time-reversible macroscopic behavior under extreme conditions. If no: the arrow applies to all physical regimes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_coherence_escape_route, empirical, 'Whether quantum coherence permits escape from thermodynamic irreversibility').

omega_variable(
    entropic_cost_of_observation,
    'Does the measurement of entropy or the verification of the second law itself require entropy production that could be construed as extractive?',
    'Analysis of Landauer''s principle and the thermodynamic cost of information erasure; measurement of entropy cost of monitoring systems',
    'If yes: the constraint generates the paradoxical quality that verifying it requires paying its cost, creating a quasi-extractive structure. If no: the arrow is purely natural law without meta-extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entropic_cost_of_observation, empirical, 'Thermodynamic cost of measuring or verifying the second law').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(thermodynamic_arrow_of_time, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(therm_arrow_tr_t0, thermodynamic_arrow_of_time, theater_ratio, 0, 0.15).
narrative_ontology:measurement(therm_arrow_tr_t1, thermodynamic_arrow_of_time, theater_ratio, 1, 0.15).
narrative_ontology:measurement(therm_arrow_tr_t2, thermodynamic_arrow_of_time, theater_ratio, 2, 0.15).

% Extraction over time
narrative_ontology:measurement(therm_arrow_be_t0, thermodynamic_arrow_of_time, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(therm_arrow_be_t1, thermodynamic_arrow_of_time, base_extractiveness, 1, 0.08).
narrative_ontology:measurement(therm_arrow_be_t2, thermodynamic_arrow_of_time, base_extractiveness, 2, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(thermodynamic_arrow_of_time, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
