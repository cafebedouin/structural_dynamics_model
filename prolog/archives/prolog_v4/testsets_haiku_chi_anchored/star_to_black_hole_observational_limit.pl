% ============================================================================
% CONSTRAINT STORY: star_to_black_hole_observational_limit
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_star_to_black_hole_observational_limit, []).

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
 *   constraint_id: star_to_black_hole_observational_limit
 *   human_readable: Observational Limit on Directly Observing Star-to-Black Hole Transformation
 *   domain: technological/observational_astronomy
 *
 * SUMMARY:
 *   The direct observation of a massive star collapsing into a black hole
 *   encounters an absolute observational barrier: the timescale of
 *   gravitational collapse (milliseconds to seconds) is orders of magnitude
 *   shorter than the human capacity to detect, repoint, and stabilize
 *   instruments; the ejecta generated during collapse creates an opaque
 *   envelope that blocks electromagnetic radiation during the critical moment
 *   of formation; and the rarity of naked stellar collapse in accessible
 *   volumes of space means no single observatory can predict or schedule such
 *   an observation. This constraint exhibits the defining characteristics of
 *   a mountain — it emerges from the joint application of physical laws
 *   (thermodynamics of opaque plasma, gravity-driven collapse timescales) and
 *   technological architecture (instrument response times, photon travel
 *   delays). The constraint is invariant across observational strategies: no
 *   improvement in detector sensitivity, no increase in sky survey cadence,
 *   and no technological innovation yet envisioned can overcome the
 *   fundamental asymmetry between dynamical timescales and human/instrumental
 *   response times. However, the rise of multi-messenger astronomy
 *   (gravitational wave detection + rapid multi-wavelength follow-up) creates
 *   a perspectival escape: observations triggered by gravitational wave
 *   signals can provide indirect evidence of collapse that approaches the
 *   information content of a direct observation. This emergence of the 'rope'
 *   and 'piton' perspectives reflects not a weakening of the underlying
 *   physical constraint but a redefinition of what 'observation' means in
 *   practice.
 *
 * KEY AGENTS:
 *   - Observational Astronomer: Primary subject (powerless/trapped) — constrained by physics of collapse timescales and opacity; cannot escape the observational limit
 *   - Observational Campaign Coordinator: Institutional agent (organized/constrained) — manages large surveys that cannot predict collapses and cannot respond faster than hardware allows
 *   - Observatory Network: Coordinating institution (institutional/mobile) — gravitational wave networks create alternative pathway (rope perspective) by enabling rapid multi-wavelength follow-up triggered by GW signals
 *   - Theoretical Astrophysics Community: Knowledge producer (institutional/arbitrage) — can model collapse but cannot change the underlying physical constraint; arbitrage exit (simulation, computation) does not soften the mountain
 *   - Space Telescope Program Manager: Administrative actor (institutional/arbitrage) — faces budget constraints that force substitution of direct observation goals with indirect proxy observations (piton outcome)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes the constraint as a consequence of fundamental physics and technology architecture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(star_to_black_hole_observational_limit, 0.18).
domain_priors:suppression_score(star_to_black_hole_observational_limit, 0.03).
domain_priors:theater_ratio(star_to_black_hole_observational_limit, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(star_to_black_hole_observational_limit, extractiveness, 0.18).
narrative_ontology:constraint_metric(star_to_black_hole_observational_limit, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(star_to_black_hole_observational_limit, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(star_to_black_hole_observational_limit, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(star_to_black_hole_observational_limit, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(star_to_black_hole_observational_limit, mountain).
narrative_ontology:human_readable(star_to_black_hole_observational_limit, "Observational Limit on Directly Observing Star-to-Black Hole Transformation").
narrative_ontology:topic_domain(star_to_black_hole_observational_limit, "technological/observational_astronomy").

domain_priors:emerges_naturally(star_to_black_hole_observational_limit).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OBSERVATIONAL ASTRONOMER (MOUNTAIN) — Faces an absolute physical constraint: the collapse timescale (milliseconds to seconds) exceeds any human capacity to repoint and stabilize instruments, and obscuration by ejecta blocks view during the critical moment. No escape from this constraint. d≈1.00, f(d)≈1.42, σ=1.0 → χ≈0.26. But base ε=0.18 remains below the mountain threshold extraction ceiling of 0.25.
constraint_indexing:constraint_classification(star_to_black_hole_observational_limit, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: OBSERVATIONAL CAMPAIGN COORDINATOR (MOUNTAIN) — Massive survey campaigns (Pan-STARRS, ZTF, Vera Rubin) have constrained exit: optical sky surveys cannot be paused, cannot be redirected faster than hardware allows, and cannot predict collapses. The constraint emerges from physics and technology architecture jointly. d≈0.85, f(d)≈1.15, σ=1.2 → χ≈0.25. At the boundary of the mountain threshold.
constraint_indexing:constraint_classification(star_to_black_hole_observational_limit, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER / PHYSICS VIEW (MOUNTAIN) — From first principles, the constraint is a natural law: core collapse and accretion-disk formation occur on dynamical timescales set by gravity and nuclear physics. The opacity of ejected material during collapse is a consequence of thermodynamics and atomic physics. No negotiation possible. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.21. The constraint is invariant across all observational strategies.
constraint_indexing:constraint_classification(star_to_black_hole_observational_limit, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: OBSERVATORY NETWORK (ROPE) — Modern observatory networks (gravitational wave detectors + multi-wavelength follow-up) have created a coordination mechanism that partially defeats the constraint: LIGO/Virgo detections of neutron star mergers provide a trigger, shifting the problem from prediction to rapid response. The network has mobile exit (can redirect resources, can choose which events to follow). d≈0.45, f(d)≈0.45, σ=1.2 → χ≈0.10. From this perspective, the constraint is a coordination problem being incrementally solved, not a mountain.
constraint_indexing:constraint_classification(star_to_black_hole_observational_limit, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: THEORETICAL ASTROPHYSICS COMMUNITY (MOUNTAIN) — For massive star collapse (not neutron star mergers, which have electromagnetic counterparts), the constraint is immutable from a theoretical standpoint. Modeling predicts the collapse occurs in seconds; opacity during the fireball phase is thermodynamically required; no theory offers an alternative. Even with arbitrage exit (can pivot to computational studies, simulation), the observational constraint persists unchanged. d≈0.25, f(d)≈0.02, σ=1.0 → χ≈0.004. Theoretical arbitrage does not soften the mountain.
constraint_indexing:constraint_classification(star_to_black_hole_observational_limit, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 6: SPACE TELESCOPE PROGRAM MANAGER (PITON) — The declared goal is 'direct observation of stellar collapse.' But the program operates under severe budget constraints and limited facility lifetime. In practice, the program settles for indirect evidence: inferring collapse from gravitational waves, compact object masses, accretion signatures. The observable 'direct observation' has been substituted with 'detecting collapse by proxy.' theater_ratio≈0.65 (significant performative framing of limited observations as 'direct'). The constraint is real (mountain), but the program's response is increasingly theatrical (piton).
constraint_indexing:constraint_classification(star_to_black_hole_observational_limit, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(star_to_black_hole_observational_limit_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(star_to_black_hole_observational_limit, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(star_to_black_hole_observational_limit, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(star_to_black_hole_observational_limit, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(star_to_black_hole_observational_limit, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(star_to_black_hole_observational_limit, ExtMetricName, E),
    domain_priors:suppression_score(star_to_black_hole_observational_limit, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(star_to_black_hole_observational_limit),
    narrative_ontology:constraint_metric(star_to_black_hole_observational_limit, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(star_to_black_hole_observational_limit, resistance, R),
    AC >= 0.85,
    R =< 0.15.

test(piton_threshold) :-
    domain_priors:theater_ratio(star_to_black_hole_observational_limit, TR),
    TR >= 0.70.

:- end_tests(star_to_black_hole_observational_limit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.18): Very low. This constraint does not extract value from any agent; it simply denies access to a class of observations. The non-zero value reflects that the constraint does structure research priorities and funding allocation (some funding flows to indirect methods, theoretical modeling, simulation), but this is not extraction in the sense of asymmetric capture of surplus — it is opportunity cost. Suppression (0.03): Minimal. The constraint does not require active suppression of alternatives; it simply makes direct observation physically implausible. Alternative observational pathways (gravitational waves, X-ray counterparts, neutrino emission) are available and increasingly deployed. Theater ratio (0.15): Very low. There is minimal performative activity obscuring the constraint's function. Researchers openly discuss the observational limits, and no institutional actors benefit from theatrical framing of the constraint. The small increase over the interval (0.08 → 0.15) reflects modest growth in mission statements that frame 'detecting stellar collapse' as an observational goal while implicitly accepting that direct observation is unavailable — a minor drift toward piton-like performance.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a dramatic perspectival gap that highlights the distinction between physical mountains (invariant across all perspectives) and institutional mountains (which vary with exit options). The observational astronomer sees a pure mountain: the timescale and opacity constraints are absolute. The observatory network coordinator sees a rope: gravitational wave triggers provide a coordination mechanism that converts an unpredictable event into a rapid-response scenario. The space telescope manager sees a piton: the goal ('direct observation') is repeatedly stated but increasingly unfulfilled, with the program substituting indirect proxy observations while maintaining the original framing. The theoretical community sees a mountain (no amount of computation changes the physics) while the observatory network sees a rope (the problem is redistributed and partially solved). This gap reveals that the constraint is mountain-like in its physics but permits institutional work-arounds that reduce its effective force.
 *
 * DIRECTIONALITY LOGIC:
 *   This is a rare constraint that has no beneficiaries or victims in the economic sense — it is a pure limit on access to information. Directionality derivation is inapplicable. The constraint affects all agents symmetrically: it denies all of them access to a class of high-information observations. However, different agents experience different effective forces: The observational astronomer (powerless/trapped) experiences maximum force (d≈1.0). The observatory network (institutional/mobile) experiences reduced force because it has exit options (can deploy resources to other events, can use gravitational waves as triggers). The space telescope program (institutional/arbitrage) experiences a soften force because it can arbitrage to indirect observations. This variation in experienced force across actors with different exit options is captured in the perspectival gap, not in the base extractiveness metric.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by being a true mountain in the physical sense. The constraint is not a mask for extraction (snare), not a hybrid of coordination and extraction (tangled rope), not a temporary problem with a sunset (scaffold), and not a degraded institution (piton). The mountain classification is stable across perspectives because the underlying physics — the timescale of core collapse and the thermodynamic opacity of plasma — is invariant. However, the emergence of gravitational wave astronomy and multi-messenger observations creates a perspectival illusion: from the observatory network's viewpoint (rope perspective), the constraint is being 'solved' by redirecting the problem. This is not a mandatrophy because the rope perspective is honestly describing what the network does: it trades direct observation for triggered indirect observation. The piton perspective (space telescope program theatrically describing goals it cannot meet) is a genuine institutional degradation, but it is secondary to the primary mountain constraint. The system resolves by clearly labeling which perspectives reflect the physical limit (mountain) and which reflect institutional accommodations to that limit (rope, piton).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gravitational_wave_trigger_sufficiency,
    'Does a gravitational wave trigger from a binary merger provide sufficient advance notice to enable rapid multi-wavelength observations that capture transient signatures indistinguishable from a direct stellar collapse observation?',
    'Comparative analysis of GW-triggered follow-up data vs historical direct detections (if any) of core-collapse supernovae; timing analysis of signal-to-noise in multi-wavelength data obtained from GW triggers',
    'If yes: the constraint is substantially defeated by observational networks (rope perspective dominant). If no: the constraint persists (mountain) and GW triggers merely enable indirect inference.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gravitational_wave_trigger_sufficiency, empirical, 'Whether GW triggers provide sufficient advance notice for meaningful multi-wavelength observation').

omega_variable(
    opacity_threshold_rescatter,
    'Is the opacity during core collapse absolute (thermodynamic limit), or could exotic physics (shock precursors, jet formation) create windows for earlier emission before peak ejection obscures the event?',
    'Precision hydrodynamic simulations with neutrino transport and jet physics; observational searches for precursor emission in supernova light curves; detection of high-energy transients preceding optical maximum',
    'If absolute: mountain classification confirmed across all perspectives. If windows exist: constraint is softer (tangled rope), and targeted rapid-response campaigns might achieve direct observation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_threshold_rescatter, empirical, 'Whether core-collapse opacity is thermodynamically absolute or admits observational windows').

omega_variable(
    metric_redefinition_observational_parity,
    'Can ''direct observation'' be redefined to include gravitational wave strain data + multi-wavelength follow-up as equivalent to electromagnetic observation of collapse itself, closing the constraint by metric substitution?',
    'Community consensus on observational criteria for ''direct detection''; comparison of information content in multi-messenger data vs single-wavelength direct observation',
    'If redefinition accepted: constraint appears solved (piton outcome). If rejected: constraint remains open (mountain). Outcome is preference-dependent, not empirical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metric_redefinition_observational_parity, preference, 'Whether metric redefinition can render the constraint resolved by consensus').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(star_to_black_hole_observational_limit, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stbh_tr_t0, star_to_black_hole_observational_limit, theater_ratio, 0, 0.08).
narrative_ontology:measurement(stbh_tr_t5, star_to_black_hole_observational_limit, theater_ratio, 5, 0.12).
narrative_ontology:measurement(stbh_tr_t10, star_to_black_hole_observational_limit, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(stbh_be_t0, star_to_black_hole_observational_limit, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(stbh_be_t5, star_to_black_hole_observational_limit, base_extractiveness, 5, 0.17).
narrative_ontology:measurement(stbh_be_t10, star_to_black_hole_observational_limit, base_extractiveness, 10, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(star_to_black_hole_observational_limit, information_standard).
narrative_ontology:affects_constraint(star_to_black_hole_observational_limit, neutron_star_merger_electromagnetic_counterpart).
narrative_ontology:affects_constraint(star_to_black_hole_observational_limit, gravitational_wave_rapid_follow_up_infrastructure).

% DUAL FORMULATION NOTE:
% The observational limit on direct stellar collapse observation is a mountain constraint (ε=0.18). It is upstream of the gravitational wave follow-up constraint (ε≈0.35, tangled rope), which represents the institutional effort to work around the mountain by using GW triggers to enable rapid multi-wavelength observations. The two constraints are distinct: the mountain constraint is physical (timescales and opacity), while the downstream constraint is institutional (coordinating observatories to respond within minutes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
