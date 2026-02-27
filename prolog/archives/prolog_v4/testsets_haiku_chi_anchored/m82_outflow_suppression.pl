% ============================================================================
% CONSTRAINT STORY: m82_outflow_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_m82_outflow_suppression, []).

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
 *   constraint_id: m82_outflow_suppression
 *   human_readable: M82* Explosive Outflow and Star Formation Suppression
 *   domain: physical/astronomical
 *
 * SUMMARY:
 *   M82*, the supermassive black hole at the center of the nearby starburst
 *   galaxy M82, exhibits a remarkable phenomenon: instead of passively
 *   accreting material, it actively expels it through powerful jets and
 *   winds. This outflow carries enormous kinetic energy—sufficient to
 *   suppress star formation across the surrounding galactic disk and eject
 *   pristine gas into the intergalactic medium. The constraint here is not
 *   the outflow itself (which is straightforward hydrodynamics) but the
 *   systematic extraction of star-forming potential from the galaxy's stellar
 *   population and accretion reservoir. The outflow mechanism exhibits
 *   features of both coordination (it regulates SMBH growth through
 *   self-limiting feedback) and extraction (it suppresses star formation for
 *   the victim populations). The constraint demonstrates a perspectival
 *   split: the SMBH experiences coordination; the stellar population
 *   experiences suppression; the analytical observer must decide whether to
 *   naturalize this as a physical law (false) or recognize it as a contingent
 *   astrophysical phenomenon driven by dust geometry, magnetic fields, and
 *   accretion rates.
 *
 * KEY AGENTS:
 *   - Supermassive Black Hole M82*: Primary beneficiary (institutional/arbitrage) — achieves accretion equilibrium through outflow feedback; experiences self-regulation as coordination
 *   - Surrounding Stellar Population: Primary victim (powerless/trapped) — direct suppression of star formation within outflow cone; cannot escape feedback
 *   - Galactic Accretion Reservoir: Secondary victim (powerless/trapped) — gas destined for star formation is expelled before condensing; lost to the galaxy
 *   - Star Formation History Community: Organized observer (organized/constrained) — recognizes outflow as transient feedback mechanism with natural sunset as accretion declines
 *   - Cosmological AGN Feedback Framework: Analytical perspective (analytical/analytical) — sees outflow as necessary regulator of galaxy-black-hole coevolution; risks naturalizing contingent mechanism as fundamental law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(m82_outflow_suppression, 0.38).
domain_priors:suppression_score(m82_outflow_suppression, 0.52).
domain_priors:theater_ratio(m82_outflow_suppression, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(m82_outflow_suppression, extractiveness, 0.38).
narrative_ontology:constraint_metric(m82_outflow_suppression, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(m82_outflow_suppression, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(m82_outflow_suppression, tangled_rope).
narrative_ontology:human_readable(m82_outflow_suppression, "M82* Explosive Outflow and Star Formation Suppression").
narrative_ontology:topic_domain(m82_outflow_suppression, "physical/astronomical").

domain_priors:requires_active_enforcement(m82_outflow_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(m82_outflow_suppression, supermassive_black_hole_m82).
narrative_ontology:constraint_victim(m82_outflow_suppression, surrounding_stellar_population).
narrative_ontology:constraint_victim(m82_outflow_suppression, galactic_accretion_reservoir).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SURROUNDING STELLAR POPULATION (SNARE) — Stars within the outflow cone cannot escape the suppressive mechanism; their star formation is directly inhibited by the energetic feedback. d≈0.93, f(d)≈1.40, σ=0.9 → χ≈0.52. The stellar material is systematically extracted from accretion-to-formation by the outflow mechanism.
constraint_indexing:constraint_classification(m82_outflow_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: GALACTIC ACCRETION RESERVOIR (SNARE) — The gas supply available for stellar formation is systematically suppressed by the outflow. Material destined for accretion is expelled before it can form stars. d≈0.92, f(d)≈1.39, σ=0.9 → χ≈0.52. Pure extraction of potential star-forming material.
constraint_indexing:constraint_classification(m82_outflow_suppression, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: SUPERMASSIVE BLACK HOLE M82* (ROPE) — The outflow mechanism is a coordination solution: it regulates its own growth through self-limiting feedback, preventing runaway accretion. The SMBH experiences the outflow as a coordination function that maintains energy equilibrium. d≈0.08, f(d)≈-0.09, σ=0.8 → χ≈-0.03. Net beneficiary through coordination.
constraint_indexing:constraint_classification(m82_outflow_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / AGN FEEDBACK SYSTEM (TANGLED ROPE) — From a cosmological view, AGN outflows like M82* serve a genuine coordination function: they regulate galaxy growth and prevent excessive black hole accretion, maintaining cosmic equilibrium. Simultaneously, this mechanism extracts resources from the host galaxy (star formation suppression, gas removal). The constraint has both coordination (feedback stability) and asymmetric extraction (stellar suppression). d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.30. Moderate effective extraction at civilizational scale.
constraint_indexing:constraint_classification(m82_outflow_suppression, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: STAR FORMATION HISTORY COMMUNITY (SCAFFOLD) — Organized scientific observers recognize the outflow as a temporary suppression mechanism during high-accretion epochs. As the black hole reaches equilibrium and the outflow weakens, star formation can resume. The constraint has a natural sunset: accretion-driven feedback is strongest during early/active phases; secular evolution allows recovery. d≈0.38, f(d)≈0.38, σ=0.9 → χ≈0.17. Low effective extraction because the mechanism is transient.
constraint_indexing:constraint_classification(m82_outflow_suppression, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PHYSICAL LAW VIEW (MOUNTAIN) — From a fundamental physics perspective, energy and momentum conservation laws guarantee that accretion-powered feedback will suppress star formation in active galaxies. This is not contingent but an irreducible constraint of physics. ε≤0.25, suppression≤0.05 structurally. However, the measured base properties (ε=0.38, suppression=0.52) reveal this as a false summit: the outflow is a contingent astrophysical phenomenon (dust settling, magnetic field geometry) not a pure law of physics.
constraint_indexing:constraint_classification(m82_outflow_suppression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(m82_outflow_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(m82_outflow_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(m82_outflow_suppression, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(m82_outflow_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The outflow suppresses star formation significantly but not absolutely—some star formation persists in protected regions and in the post-outflow epoch. The extraction is real (gas and energy removed from star-forming material) but not maximal because: (a) the outflow is not perfectly efficient (some gas escapes suppression), (b) star formation can resume after accretion decline, and (c) the mechanism is transient rather than permanent. The trajectory (0.18→0.28→0.38) reflects increasing severity of suppression as the black hole's accretion rate rises. Suppression (0.52): Moderate-high. The outflow creates genuine barriers to star formation: energetic heating, mechanical stripping, and potential-well escape eliminate options for affected gas. But suppression is not total because: (a) radiation pressure and jets do not instantaneously affect all gas, (b) magnetic fields and dust geometry create localized shielding, (c) some stellar formation can occur in dense cores. Theater ratio (0.48): Moderate. The outflow is not primarily performative—it is a direct physical mechanism coupling accretion energy to kinetic feedback. However, some 'theatrical' elements exist: (a) the visible outflow component is a small fraction of the total energy budget (much is radiated), (b) radio observations show structured jets that may overstate the actual mechanical coupling, (c) the feedback's effectiveness varies with dust content and viewing angle, creating apparent performance variation. Theater increases slightly over the interval as dust settles and magnetic fields organize, making the outflow more structured and apparently more purposeful.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the critical perspectival divide between beneficiary and victim. The SMBH (institutional/arbitrage) perceives coordination: the outflow solves the feedback regulation problem and maintains accretion equilibrium. It classifies as Rope—pure coordination with minimal extraction. The surrounding stars (powerless/trapped) perceive extraction: they are systematically prevented from forming within the outflow cone and cannot escape. It classifies as Snare—pure extraction with no coordination benefit. The analytical observer at cosmological scale (analytical/analytical) perceives both: the outflow is a genuine regulator of galaxy-black-hole coevolution (Rope from the system's perspective) AND it suppresses local star formation (Snare from the galaxy's perspective), making it Tangled Rope at the civilizational level. The false-summit danger: the observer might naturalize this as inevitable physics (Mountain), claiming that energy conservation and momentum transfer are physical laws requiring such suppression. The structural data contradict this: suppression (0.52) is too high, extractiveness (0.38) is too high for a pure law. The outflow is a specific outcome of dust settling, magnetic field amplification, and accretion-rate thresholds—all contingent on astrophysical conditions, not fundamental physics.
 *
 * DIRECTIONALITY LOGIC:
 *   Supermassive Black Hole M82*: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Net institutional beneficiary. The SMBH achieves regulation through the outflow and experiences it as coordination. Surrounding Stellar Population: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction. Stars cannot escape the outflow cone or its suppressive effects; they bear the full cost of feedback. Galactic Accretion Reservoir: Victim + trapped → d≈0.92, f(d)≈1.39. Near-maximum extraction. Gas destined for star formation is expelled; no exit option. Star Formation History Community: Organized + constrained → d≈0.38, f(d)≈0.38. Low effective extraction because community recognizes the outflow as transient and has research pathways to study recovery. Analytical Observer (AGN Feedback): analytical → d≈0.50, f(d)≈0.65. Moderate—symmetry between coordination (galaxy-BH balance) and extraction (star formation suppression). Analytical Observer (Physical Law): analytical → d≈0.72, f(d)≈1.15. Would imply high naturalization; contradicted by structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by distinguishing between genuine physical law (Mountain) and contingent astrophysical mechanism (Tangled Rope). The false-summit detector identifies that ε=0.38 and suppression=0.52 exceed mountain thresholds, ruling out pure law status. The constraint genuinely is Tangled Rope: it exhibits both coordination (SMBH feedback regulation) and extraction (star formation suppression), with active enforcement (energetic feedback) required. The outflow is not inevitable from physics alone—it emerges from specific conditions: (1) accretion rates exceeding a critical threshold, (2) dust content creating sufficient opacity to couple radiation to mechanical energy, (3) magnetic field amplification providing jet collimation, (4) galaxy mass and potential-well depth determining escape fractions. These are contingent astrophysical parameters, not fundamental constants. The scaffold perspective confirms the transient nature: as the black hole's accretion rate declines (either through exhausting the gas supply or reaching equilibrium), the outflow weakens and star formation resumes. This is not a permanent suppression (Snare) but a temporary mechanism with a natural sunset measured in galactic evolutionary timescales (0.1-1 Gyr). The Tangled Rope classification is robust: coordination function (accretion feedback) + asymmetric extraction (star formation suppression) + active enforcement (outflow jets) + contested scope (local vs. galactic scale).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    outflow_causal_primacy,
    'Does the outflow directly suppress star formation through energetic feedback, or does feedback-suppression causally precede and enable the outflow?',
    'High-resolution simulation of coupled SMBH accretion-AGN feedback dynamics; temporal correlation analysis of outflow initiation and star formation quenching across galaxies with similar M82* properties',
    'If outflow-driven suppression: snare classification confirmed for stellar population. If feedback-preceded: constraint is more rope-like (coordination of feedback). Causality reversal changes directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(outflow_causal_primacy, empirical, 'Causal ordering between outflow initiation and star formation suppression').

omega_variable(
    escape_velocity_threshold,
    'At what outflow velocity do stars definitively escape the galaxy vs. remain in the potential well? Is the transition continuous or sharp?',
    'N-body simulations with parametric outflow velocity; observational surveys of escape fractions across AGN activity levels; dynamical modeling of M82* system',
    'If sharp threshold: some stars escape while others are trapped (heterogeneous victim population). If continuous: suppression is graded rather than binary. Affects whether ''trapped'' is accurate or too strong.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(escape_velocity_threshold, empirical, 'Velocity threshold for permanent escape from galaxy potential').

omega_variable(
    recovery_timescale_duration,
    'After outflow quenching, how long until star formation resumes to pre-outflow rates? Is recovery autonomous or does it require external gas accretion?',
    'Longitudinal observation of post-active galaxies; simulations of cooling timescales and gas refueling; comparison of M82 historical star formation rate recovery with other AGN',
    'If recovery is rapid (<1 Gyr): scaffold sunset is real. If recovery is slow or requires external inputs (>5 Gyr or dependent on mergers): sunset is theoretical, not structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recovery_timescale_duration, empirical, 'Timescale and mechanism for star formation recovery after outflow suppression').

omega_variable(
    dust_and_magnetic_contingency,
    'How much of the observed outflow is contingent on dust opacity and magnetic field geometry vs. inherent to energy conservation in SMBH accretion?',
    'Multi-wavelength decomposition (radio-optical-infrared) of outflow composition; magnetohydrodynamic simulations varying dust opacity and field structure; comparison of outflows in galaxies with different dust properties',
    'If highly contingent (dust/field dependent): constraint is institutional/structural, not physical law. If fundamental: some aspects are closer to mountain (but ε and suppression still rule out pure mountain). Affects false-summit diagnosis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dust_and_magnetic_contingency, empirical, 'Contingency of outflow on dust and magnetic field properties vs. fundamental physics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(m82_outflow_suppression, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(m82_tr_t0, m82_outflow_suppression, theater_ratio, 0, 0.35).
narrative_ontology:measurement(m82_tr_t5, m82_outflow_suppression, theater_ratio, 5, 0.42).
narrative_ontology:measurement(m82_tr_t10, m82_outflow_suppression, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(m82_be_t0, m82_outflow_suppression, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(m82_be_t5, m82_outflow_suppression, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(m82_be_t10, m82_outflow_suppression, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(m82_outflow_suppression, enforcement_mechanism).
narrative_ontology:affects_constraint(m82_outflow_suppression, agn_feedback_galaxy_coevolution).
narrative_ontology:affects_constraint(m82_outflow_suppression, star_formation_rate_regulation).

% DUAL FORMULATION NOTE:
% M82* outflow suppression is downstream of AGN accretion physics but represents a distinct structural constraint. The upstream accretion constraint has lower extractiveness (ε≈0.15, more coordination-focused); the outflow constraint adds the suppression mechanism (ε≈0.38), which couples accretion to galactic-scale star formation outcomes. These are linked: accretion enables outflow, but outflow's suppressive effects are distinct structural phenomena.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
