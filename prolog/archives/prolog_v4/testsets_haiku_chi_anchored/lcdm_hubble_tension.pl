% ============================================================================
% CONSTRAINT STORY: lcdm_hubble_tension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lcdm_hubble_tension, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: lcdm_hubble_tension
 *   human_readable: The Hubble Tension within the Lambda-CDM Cosmological Model
 *   domain: cosmology/observational_astronomy
 *
 * SUMMARY:
 *   The Hubble Tension is a persistent 4-5 sigma discrepancy between
 *   early-universe estimates of H0 (from Planck CMB + BBN: 67-68 km/s/Mpc)
 *   and late-universe measurements (from local distance ladder + SNe Ia:
 *   72-74 km/s/Mpc). The Lambda-CDM model accommodates both measurement
 *   regimes successfully in isolation, but the standardized predictions
 *   conflict at the 4-5 sigma level. This creates a structural tension: the
 *   early-universe community (Planck, BBN) occupies a high-credibility
 *   position because their measurements constrain the fundamental parameters
 *   of the model and are independent of the distance ladder. The
 *   late-universe community (Cepheids, supernovae, gravitational lensing)
 *   faces institutional pressure to find systematics errors in their own
 *   pipelines while the early-universe community can invoke 'conservative'
 *   methodology. Neither community can exit: both are essential to cosmology.
 *   The constraint exhibits all six types from different perspectives: snare
 *   for the trapped late-universe teams, tangled rope for the model
 *   unification effort (forced to validate both sides without resolving
 *   them), rope for the early-universe beneficiaries, scaffold for new
 *   model-agnostic methods (JWST, gravitational waves, Vera Rubin), piton for
 *   the ΛCDM institution (maintained through funding despite unresolved
 *   tensions), and tangled rope for the analytical observer (genuine
 *   coordination function forcing rigor, but asymmetric extraction favoring
 *   early universe).
 *
 * KEY AGENTS:
 *   - Early-Universe Measurement Community: Planck collaboration, BBN nucleosynthesis teams (institutional/arbitrage) — benefits from high credibility and institutional defense of their measurements; measurement pipelines less publicly scrutinized
 *   - Late-Universe Measurement Community: Local distance ladder teams, SN Ia cosmology groups, gravitational lensing surveys (moderate/trapped) — bears institutional pressure to find systematics in their own work; cannot exit measurement program
 *   - Model Unification Effort: ΛCDM maintainers, parameter fitting teams (moderate/constrained) — forced to accommodate both regimes; credibility depends on resolving tensions
 *   - Model-Agnostic Survey Coalition: JWST H0 team, gravitational wave standard siren network, Vera Rubin LSST (organized/constrained) — building independent pathways with sunset logic (5-10 year timeline)
 *   - ΛCDM Maintenance Institution: Standard model textbooks, observatory funding allocations, theoretical framework inertia (institutional/arbitrage) — maintains status quo through publication and funding pathways despite unresolved tension
 *   - Analytical Observer: Cosmological theory from first principles (analytical/analytical) — sees both coordination (forcing methodological rigor) and extraction (asymmetric credibility allocation)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lcdm_hubble_tension, 0.52).
domain_priors:suppression_score(lcdm_hubble_tension, 0.48).
domain_priors:theater_ratio(lcdm_hubble_tension, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lcdm_hubble_tension, extractiveness, 0.52).
narrative_ontology:constraint_metric(lcdm_hubble_tension, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(lcdm_hubble_tension, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lcdm_hubble_tension, tangled_rope).
narrative_ontology:human_readable(lcdm_hubble_tension, "The Hubble Tension within the Lambda-CDM Cosmological Model").
narrative_ontology:topic_domain(lcdm_hubble_tension, "cosmology/observational_astronomy").

domain_priors:requires_active_enforcement(lcdm_hubble_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lcdm_hubble_tension, early_universe_measurement_community).
narrative_ontology:constraint_beneficiary(lcdm_hubble_tension, theoretical_model_maintainers).
narrative_ontology:constraint_victim(lcdm_hubble_tension, late_universe_measurement_community).
narrative_ontology:constraint_victim(lcdm_hubble_tension, model_unification_efforts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATE-UNIVERSE OBSERVERS (SNARE) — Local measurements of H0 via supernovae, Cepheid variables, and gravitational lensing are trapped by the systematic tensions with early-universe CMB+BBN estimates. Cannot exit the measurement program without abandoning cosmological constraints. Bears full cost of the unresolved discrepancy. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.65.
constraint_indexing:constraint_classification(lcdm_hubble_tension, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MODEL UNIFICATION EFFORT (TANGLED ROPE) — Constrained by the need to preserve ΛCDM's successful early-universe fits while accommodating late-universe measurements. Faces institutional pressure to maintain model coherence. Benefits from coordination: forcing methodological scrutiny on both measurement pipelines. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.55.
constraint_indexing:constraint_classification(lcdm_hubble_tension, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EARLY-UNIVERSE COMMUNITY (ROPE) — Planck CMB measurements and BBN nucleosynthesis are high-precision, independently validated. Experiences the constraint as a coordination mechanism: the tension forces methodological rigor on late-universe teams and validates their own measurement pipeline. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.05. Net beneficiary through credibility and research emphasis.
constraint_indexing:constraint_classification(lcdm_hubble_tension, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MODEL-AGNOSTIC SURVEY COALITION (SCAFFOLD) — Organized efforts (James Webb Space Telescope, Vera Rubin Observatory, gravitational wave sirens) are building independent H0 measurement pathways that sidestep ΛCDM calibrations. These have sunset logic: as independent methods mature (5-10 year timeline), they will either resolve the tension via better late-universe measurements or establish that the discrepancy is real and fundamental. d≈0.45, f(d)≈0.48, σ=1.2 → χ≈0.27. Low effective extraction because coalition has methodological agency and sees an exit path.
constraint_indexing:constraint_classification(lcdm_hubble_tension, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ΛCDM MAINTENANCE INSTITUTION (PITON) — The standard model is maintained through institutional inertia despite 4+ years of unresolved tension. Theater ratio reflects performative elements: 'discrepancy resolution workshops,' incremental systematics papers, and cautious statements that 'ΛCDM is still consistent within 3σ.' The model persists not because it solves the problem but because alternatives haven't fully replaced it. theater_ratio=0.65 satisfies piton gate (≥0.70 marginal). The institution sees its own framework as degraded but continues maintenance through funding and publication pathways.
constraint_indexing:constraint_classification(lcdm_hubble_tension, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the tension is both a coordination mechanism (forcing methodological rigor, cross-checking measurement pipelines) and an extraction mechanism (early-universe community benefits from pressure on late-universe teams while evading the same pressure on their own CMB systematics). The constraint exhibits genuine coordination function (both communities must validate their priors) and asymmetric extraction (early-universe gets credibility boost, late-universe bears burden of proof). d≈0.52, f(d)≈0.65, σ=1.0 → χ≈0.34.
constraint_indexing:constraint_classification(lcdm_hubble_tension, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lcdm_hubble_tension_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lcdm_hubble_tension, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lcdm_hubble_tension, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(lcdm_hubble_tension, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(lcdm_hubble_tension, TR),
    TR >= 0.70.

:- end_tests(lcdm_hubble_tension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The early-universe community gains credibility and institutional support (more prominent model-building, higher publication weight for CMB/BBN consistency papers) while the late-universe community faces pressure to find errors in their own pipelines. The extraction is not as severe as a pure snare (0.66+) because both communities are essential and the tension could genuinely be a measurement problem. Suppression (0.48): Moderate. Significant barriers include: (a) institutional commitment to ΛCDM preventing radical alternatives; (b) career risk for late-universe scientists proposing model-breaking systematics; (c) technical complexity of independent verification. But suppression is not high because alternative theories are published and independent methods are funded. Theater ratio (0.65): Moderate-high, trending upward. The theater has increased over the interval as the field has shifted from 'anomaly' (0.35 in 2019) to 'persistent puzzle' (0.50 in 2021) to 'systematics workshop industry' (0.65 in 2023). Performative elements include: 'tension resolution workshops,' incremental systematics papers that make no progress, cautious institutional statements that 'ΛCDM is still consistent within 3σ.' The theater reflects Goodhart drift: the measure (number of systematics papers) has replaced the goal (actually resolving the tension). The rise from 0.35 to 0.65 indicates degradation toward piton status, but the theater is not yet high enough (0.70 gate) to trigger piton classification from all perspectives.
 *
 * PERSPECTIVAL GAP:
 *   The early-universe community sees coordination (Rope) — the tension validates their measurements by forcing rigor on late-universe teams. The late-universe community sees extraction (Snare) — they bear the burden of proof while CMB systematics receive less scrutiny. The model unification effort sees a mixed constraint (Tangled Rope) — both coordination (both communities must validate each other) and extraction (asymmetric pressure on late universe). The model-agnostic coalition sees a temporary problem (Scaffold) — their independent methods have an exit path with a ~10-year sunset. The ΛCDM institution sees a degraded mechanism (Piton) — the model is maintained through funding inertia, not because it solves the problem. The analytical observer (this story's perspective) sees the full tangled structure: real coordination function (forcing methodological validation) mixed with real extraction (early-universe credibility boost). The perspectival gap reveals that the tension is not merely a measurement problem — it is also an institutional structure that benefits certain communities while constraining others.
 *
 * DIRECTIONALITY LOGIC:
 *   Early-universe community: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. Their measurements are taken as the 'gold standard' against which late-universe teams are validated. Arbitrage exit means they can cite their own consistency and move on if the tension persists. Late-universe community: Victim + trapped → d≈0.92, f(d)≈1.38. Nearly maximum extraction. Cannot exit local H0 measurements (essential to cosmology), forced to find systematics in their own pipelines while early-universe faces lighter scrutiny. Model unification effort: Victim + constrained → d≈0.68, f(d)≈1.05. High extraction. Must accommodate both regimes without resolution. Cannot exit (credibility requires addressing the tension) but cannot solve it (fundamental problem). Model-agnostic coalition: Organized + constrained → d≈0.45, f(d)≈0.48. Low-moderate extraction. Coalition has agency and sees an exit path (new independent methods). ΛCDM institution: Institutional + arbitrage → d≈0.08, f(d)≈-0.11. Piton classification from theater gate, not high chi. Arbitrage exit means the institution can maintain status quo indefinitely if it chooses. Analytical observer: analytical → d≈0.52, f(d)≈0.65. Moderate extraction in mixed (tangled rope) configuration. Forced to acknowledge both coordination and asymmetry without privileging either.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION PATHWAY: The tension resolves mandatrophy by distinguishing real coordination (both communities validating each other's methodology) from extraction (asymmetric credibility allocation). The constraint is correctly classified as Tangled Rope from the analytical perspective because: (1) it has genuine coordination function — the tension forces both early and late universe teams to examine their systematics rigorously and cross-validate their priors; (2) it has asymmetric extraction — the early-universe community benefits from institutional credibility while the late-universe community bears the burden of proof; (3) it requires active enforcement — funding allocations, publication pathways, and model-building authority maintain the early-universe advantage. If the constraint were pure Rope, both communities would experience symmetric validation. If it were pure Snare, there would be no coordination benefit at all. The Tangled Rope classification captures that this is a genuine problem-solving mechanism (coordination) layered with institutional asymmetry (extraction). The rising theater ratio (0.35→0.65) and extractiveness (0.32→0.52) over the interval indicate Goodhart drift toward piton status, where the performative activity (systematics workshops) replaces the actual function (resolving the tension). If theater reaches 0.70 and extractiveness remains high, the constraint may degrade to a snare-piton hybrid where the coordination function atrophies entirely and only extraction remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    early_universe_systematics,
    'Are systematic errors in early-universe measurements (Planck calibration, recombination physics, reionization assumptions) masking the true primordial H0 value?',
    'Independent early-universe measurements using different physics (primordial gravitational waves, 21cm absorption, alternative CMB experiments); cross-validation of recombination and reionization models; Planck 2025+ calibration results',
    'If early-universe systematics are significant: late-universe measurements are correct, and ΛCDM requires modification. If early-universe is robust: late-universe has unresolved systematics, or physics beyond ΛCDM is real.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(early_universe_systematics, empirical, 'Whether early-universe measurements contain unresolved systematics').

omega_variable(
    late_universe_systematics,
    'Do late-universe measurements (Cepheid distance ladder, SN Ia standardization, gravitational lensing time delays) share correlated systematics that inflate H0 estimates?',
    'Independent late-universe methods (gravitational wave standard sirens, baryon acoustic oscillations with direct H0 anchors, megamaser distance measurements); cross-technique systematic error analysis; JWST recalibration of local distance ladder',
    'If late-universe systematics are dominant: ΛCDM is unchallenged, and new measurements will converge to early-universe value. If systematics are orthogonal: tension is real and fundamental physics is required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(late_universe_systematics, empirical, 'Whether late-universe measurements share correlated systematics').

omega_variable(
    physics_beyond_lcdm,
    'Does the tension point to genuine new physics (early dark energy, modified gravity, evolving dark energy equation of state) or is it a measurement problem?',
    'High-precision independent H0 measurements (gravitational wave sirens mature to <1% precision); direct early dark energy detection (primordial recombination spectrum analysis); direct tests of modified gravity predictions (large-scale structure growth rate, gravitational lensing shear ratios)',
    'If new physics required: ΛCDM is false in a way that requires model extension. If measurement problem: ΛCDM unmodified. If indeterminate: constraint persists in status quo indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physics_beyond_lcdm, empirical, 'Whether the tension requires new physics beyond ΛCDM').

omega_variable(
    measurement_methodology_equivalence,
    'Are early and late universe measurements measuring the same physical quantity (the Hubble constant at z=0) or are they measuring contextually different expansions rates (initial vs local)?',
    'Rigorous analysis of confounding variables: redshift evolution, smoothness assumptions, reference frame dependencies; comparison with intermediate-redshift anchors (z~0.1-0.3) using independent methods; theoretical consistency analysis of what ''H0'' means in each measurement context',
    'If measuring different quantities: the ''tension'' is partly definitional — no inconsistency exists. If measuring the same quantity: inconsistency is fundamental.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurement_methodology_equivalence, conceptual, 'Whether early and late measurements measure the same physical quantity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lcdm_hubble_tension, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hubble_tr_t0, lcdm_hubble_tension, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hubble_tr_t2, lcdm_hubble_tension, theater_ratio, 2, 0.5).
narrative_ontology:measurement(hubble_tr_t4, lcdm_hubble_tension, theater_ratio, 4, 0.65).

% Extraction over time
narrative_ontology:measurement(hubble_be_t0, lcdm_hubble_tension, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(hubble_be_t2, lcdm_hubble_tension, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(hubble_be_t4, lcdm_hubble_tension, base_extractiveness, 4, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lcdm_hubble_tension, information_standard).
narrative_ontology:affects_constraint(lcdm_hubble_tension, lcdm_age_tension).
narrative_ontology:affects_constraint(lcdm_hubble_tension, dark_energy_equation_of_state).
narrative_ontology:affects_constraint(lcdm_hubble_tension, early_dark_energy_models).

% DUAL FORMULATION NOTE:
% The Hubble Tension decomposes into several related constraints: (1) early-universe_systematics (ε≈0.08, Mountain) — Planck CMB measurement precision is extremely high and independently validated; (2) late_universe_systematics (ε≈0.35, Rope/Tangled Rope) — local distance ladder has complex calibrations; (3) tension_resolution_effort (ε≈0.52, Tangled Rope, this story) — the institutional constraint of accommodating both sides; (4) model_modification_pressure (ε≈0.65, Snare) — theoretical teams pressured to invent new physics without empirical justification. This story focuses on (3), the institutional tension itself. Network links show how resolution of early-universe systematics would downgrade the tension to a Rope-only constraint; failure to resolve late-universe systematics would upgrade it to Snare.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lcdm_hubble_tension, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
