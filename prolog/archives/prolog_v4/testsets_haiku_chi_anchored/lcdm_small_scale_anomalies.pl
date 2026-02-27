% ============================================================================
% CONSTRAINT STORY: lcdm_small_scale_anomalies
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lcdm_small_scale_anomalies, []).

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
 *   constraint_id: lcdm_small_scale_anomalies
 *   human_readable: Lambda-CDM Cosmological Model (Small-Scale Structure Anomalies)
 *   domain: scientific/cosmological
 *
 * SUMMARY:
 *   The Lambda-CDM model is the standard cosmological paradigm,
 *   extraordinarily successful at predicting the cosmic microwave background,
 *   large-scale structure, and cosmic expansion history. Yet at small scales
 *   (< 1 Mpc), observations reveal persistent anomalies: fewer dwarf
 *   satellite galaxies than simulations predict (missing satellites problem),
 *   cores in dark matter halos that are smoother than cold-collisionless
 *   theory predicts (core-cusp tension), galaxies arranged in planar
 *   structures inconsistent with isotropic CDM predictions, and the
 *   'too-big-to-fail' problem where simulated massive subhalos are too
 *   common. Over 20 years, the institutional response has been to absorb
 *   anomalies into ad-hoc mechanisms (baryonic feedback, merger bias, galaxy
 *   formation complexities) rather than revise the core model. The constraint
 *   here is not ΛCDM itself but the institutional arrangement that suppresses
 *   alternative dark matter theories and modified gravity while maintaining
 *   CDM dominance through selective interpretation of observations. This
 *   manifests as a tangled rope: the model provides genuine coordination
 *   value at large scales (CMB, BAO) while extracting visibility and funding
 *   from alternatives at small scales. The theater ratio (0.65) reflects
 *   increasing performative activity: parameters are tuned, simulations are
 *   refined, anomalies are reframed, but the fundamental particle assumptions
 *   remain unquestioned.
 *
 * KEY AGENTS:
 *   - Large-Scale Structure Cosmology: Primary beneficiary (institutional/arbitrage) — CMB and BAO constraints perfectly aligned with ΛCDM; career capital flows to parameter refinement
 *   - Small-Scale Structure Observations: Primary victim (powerless/trapped) — satellite counts, core shapes, alignment anomalies absorbed into theory rather than triggering revision
 *   - Alternative Theory Community: Secondary victim (moderate/constrained) — MOND, warm dark matter, fuzzy dark matter researchers face funding scarcity and publication bias despite viable models
 *   - Simulation and Computational Community: Organized actor (organized/mobile) — sees small-scale problem as solvable through higher-resolution simulations; represents the scaffold perspective with a sunset
 *   - Observational Satellite Missions: Institutional actor (institutional/constrained) — Gaia, Euclid, JWST data reduction choices depend on ΛCDM assumptions; constrained by model-dependent calibrations
 *   - Theoretical Dark Matter Framework: Institutional persistence (institutional/arbitrage) — cold collisionless dark matter maintained through inertia despite alternatives; piton perspective
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional choice (CDM vs. modified gravity) as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lcdm_small_scale_anomalies, 0.38).
domain_priors:suppression_score(lcdm_small_scale_anomalies, 0.48).
domain_priors:theater_ratio(lcdm_small_scale_anomalies, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lcdm_small_scale_anomalies, extractiveness, 0.38).
narrative_ontology:constraint_metric(lcdm_small_scale_anomalies, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(lcdm_small_scale_anomalies, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lcdm_small_scale_anomalies, tangled_rope).
narrative_ontology:human_readable(lcdm_small_scale_anomalies, "Lambda-CDM Cosmological Model (Small-Scale Structure Anomalies)").
narrative_ontology:topic_domain(lcdm_small_scale_anomalies, "scientific/cosmological").

domain_priors:requires_active_enforcement(lcdm_small_scale_anomalies).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lcdm_small_scale_anomalies, standard_model_institutional_physics).
narrative_ontology:constraint_beneficiary(lcdm_small_scale_anomalies, large_scale_structure_cosmology).
narrative_ontology:constraint_victim(lcdm_small_scale_anomalies, small_scale_structure_observations).
narrative_ontology:constraint_victim(lcdm_small_scale_anomalies, alternative_theory_research_community).
narrative_ontology:constraint_victim(lcdm_small_scale_anomalies, observational_satellite_missions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL-SCALE STRUCTURE OBSERVATIONS (SNARE) — Observational anomalies (missing satellites, core-cusp problem, too-big-to-fail, planes of galaxies) cannot exit the model's interpretive framework. Every anomaly is absorbed into ad-hoc mechanisms (baryonic feedback, merger bias, incomplete simulations) rather than triggering model revision. Trapped by the institutional dominance of ΛCDM. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.63.
constraint_indexing:constraint_classification(lcdm_small_scale_anomalies, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE THEORY COMMUNITY (TANGLED ROPE) — MOND, emergent gravity, self-interacting dark matter researchers benefit from ΛCDM as a reference point (papers gain citation traction by being 'against the standard model') but are constrained by funding scarcity, publication bias, and difficulty obtaining observational time. The model provides coordination (shared predictive framework for comparison) and extraction (suppresses funding/visibility for alternatives). d≈0.72, f(d)≈1.12, σ=1.2 → χ≈0.51.
constraint_indexing:constraint_classification(lcdm_small_scale_anomalies, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: LARGE-SCALE STRUCTURE COSMOLOGY (ROPE) — The model is a coordinating success at > 1 Mpc scales: CMB, matter power spectrum, baryon acoustic oscillations all align with ΛCDM predictions. Large-scale cosmologists benefit from a unified framework and gain career capital by refining ΛCDM parameters. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.05. Net beneficiary via coordination.
constraint_indexing:constraint_classification(lcdm_small_scale_anomalies, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SATELLITE MISSIONS (TANGLED ROPE) — Missions like Gaia, Euclid, and JWST are designed with ΛCDM cosmology as the reference frame. They benefit from having a unified model for interpreting data but are constrained by model-dependent data reduction choices (e.g., ΛCDM-assuming stellar-to-halo mass relations used to calibrate observations). Some missions could pursue model-independent measurements but face pressure to confirm ΛCDM. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.35.
constraint_indexing:constraint_classification(lcdm_small_scale_anomalies, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SIMULATION / COMPUTATION COMMUNITY (SCAFFOLD) — Sees the small-scale anomalies as a temporary problem solvable through improved simulations and subgrid physics. Higher-resolution ΛCDM simulations (Illustris, FIRE, EAGLE) are progressively absorbing anomalies, reducing the need for alternatives. The sunset is the asymptotic approach to realistic baryonic physics in simulations. d≈0.35, f(d)≈0.30, σ=1.2 → χ≈0.14. Low extraction because computational improvements have a genuine pathway to resolution.
constraint_indexing:constraint_classification(lcdm_small_scale_anomalies, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: THEORETICAL DARK MATTER FRAMEWORK (PITON) — Cold, collisionless dark matter is maintained through institutional inertia despite decades of small-scale tensions. The framework persists because the theoretical alternatives (warm dark matter, axion fuzzy dark matter, self-interacting dark matter) are incomplete or require new physics. Theater ratio = 0.65: significant performative activity (parameter tweaking, simulation tuning, anomaly reframing) without loss of faith in the core paradigm. d≈0.12, f(d)≈0.02, σ=1.2 → χ≈0.01.
constraint_indexing:constraint_classification(lcdm_small_scale_anomalies, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — From the civilizational view, the small-scale structure problem might be framed as an inherent limitation of structure formation in collisionless matter: clustering and gravitational dynamics obey immutable laws. The false summit view naturalizes what is actually a contingent choice: CDM vs. modified gravity, cold vs. warm, collisionless vs. self-interacting. The mountain classification fails the accessibility/resistance gates (ε=0.38, suppression=0.48) — the engine flags this as a false summit revealing institutional entrenchment masquerading as natural law.
constraint_indexing:constraint_classification(lcdm_small_scale_anomalies, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lcdm_small_scale_anomalies_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lcdm_small_scale_anomalies, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lcdm_small_scale_anomalies, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(lcdm_small_scale_anomalies, TR),
    TR >= 0.70.

:- end_tests(lcdm_small_scale_anomalies_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts visibility and funding from alternative theories, but not maximally — MOND has productive research programs, warm dark matter is actively studied, and some observational groups explicitly test alternatives. The extraction is real but not total suppression. Suppression (0.48): Moderate. Barriers include publication bias (journals favor CDM-confirming results), funding concentration (NSF/DOE prioritize CDM), limited telescope time (ΛCDM-dependent proposal scoring), and citation advantage (criticizing CDM faces longer review cycles). But suppression is not absolute — alternatives publish, get funded at lower rates, obtain observational time. Theater ratio (0.65): Elevated. Increasing performative activity: baryonic feedback parameters are adjusted post-hoc to match simulations, merger bias is invoked to explain satellite deficits, reionization is introduced to address core-cusp tensions. The activity is non-random (constrained by data) but exhibits Goodhart drift — the model is tuned to absorb anomalies rather than tested against new predictions. Trajectory over interval: initial theater (0.35) reflects early genuine puzzlement; mid-point (0.50) marks the shift to systematic feedback modeling; final (0.65) shows mature parameter tuning with decreasing predictive novelty.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits large perspectival gaps. Large-scale cosmologists (CMB, BAO specialists) see a successful rope — ΛCDM coordinates observations and enables precise measurements. Small-scale observers (dwarf galaxies, satellite kinematics) see a snare — their data is systematically reinterpreted to fit the model. The alternative theory community sees tangled rope — the model provides a comparative reference frame (coordination) but suppresses their visibility (extraction). Computational cosmologists see a scaffold — improved simulations are progressively reducing tensions with a clear sunset: asymptotic baryonic physics. The theoretical framework itself appears as piton — maintained through institutional weight rather than predictive success. The civilizational analytical observer risks naturalizing the choice to pursue CDM-only as an immutable fact of cosmology rather than a contingent institutional arrangement. The mandatrophy resolves by recognizing that all six types are perspectivally valid: there is no single 'true' classification, only different structural positions relative to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Large-scale structure cosmology: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Alternative theory community: Victim + constrained → d≈0.72, f(d)≈1.12. Significant extraction but not maximal (alternatives do persist and publish). Small-scale observations: Victim + trapped → d≈0.92, f(d)≈1.38. Near-maximal extraction — anomalies cannot exit the interpretive framework. Satellite missions: Institutional actor, constrained by model-dependent data reduction → d≈0.55, f(d)≈0.75. Moderate extraction. Simulation community: Organized actor with mobile exit (computational alternatives) → d≈0.35, f(d)≈0.30. Low effective extraction. Theoretical framework: Institutional + arbitrage (maintains dominant position) → d≈0.12, f(d)≈0.02. Piton classification comes from theater gate (≥0.70 not met; theater=0.65), not from high chi. Analytical observer: analytical → d≈0.72, f(d)≈1.15. False summit detector catches the naturalization fallacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by decomposing the constraint into observational levels. At scales > 1 Mpc (CMB, BAO, large-scale galaxy distribution), ΛCDM is unambiguously a coordination mechanism (rope). At scales < 1 Mpc (dwarf galaxies, satellite kinematics, internal halo structure), the same model becomes tangled rope or snare depending on whether anomalies are viewed as legitimate complexity (baryonic feedback) or as suppressed contradictions. The institutional arrangement that prevents rigorous testing of alternatives (by redirecting resources and visibility) is what creates the extraction overhead, making the constraint tangled rope rather than pure rope. If alternatives were equally funded and tested, the small-scale problem would be an unsolved coordination challenge with multiple candidate solutions (genuine research frontier). Instead, the problem is treated as a single dominant framework with ad-hoc adjustments, which is characteristic of extraction. The mandatrophy resolves by noting that the classification depends on the OBSERVATIONAL SCALE axis, which decomposes the natural-language concept 'ΛCDM cosmology' into two distinct constraints: (1) large-scale ΛCDM structure prediction (rope, ε≈0.08), and (2) small-scale dark matter model dominance (tangled rope, ε≈0.38). These are not the same constraint viewed from different angles — they have different ε values by a factor of 4.75, different failure modes, and different institutional dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    baryonic_feedback_sufficiency,
    'Can baryonic feedback processes (supernovae, AGN jets, reionization) explain all small-scale anomalies within ΛCDM, or are they ad-hoc post-hoc adjustments?',
    'Comparison of anomaly-resolution timing: do feedback mechanisms proposed *before* anomaly discovery match simulations, or are they retrofitted *after*? Measure predictive power of feedback models on held-out observational data.',
    'If sufficient: tangled rope confirmed — ΛCDM is a coordination framework with manageable extraction overhead. If insufficient: small-scale anomalies are genuine contradictions, promoting snare classification and legitimizing alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baryonic_feedback_sufficiency, empirical, 'Whether baryonic feedback can resolve all small-scale anomalies').

omega_variable(
    dark_matter_particle_alternatives,
    'Do observational signatures favor warm dark matter, fuzzy dark matter, or axion-like particles over CDM for small-scale structure, or are alternatives equally unconstrained?',
    'Precision measurements of Lyman-alpha forest, dwarf galaxy kinematics, satellite abundance from JWST/Euclid data. Statistical comparison of likelihood ratios between CDM and alternatives using same observational dataset.',
    'If alternative is strongly preferred: ΛCDM loses coordination dominance. If alternatives equally unconstrained: ΛCDM''s institutional position is pure suppression (snare), not coordination (rope/tangled rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dark_matter_particle_alternatives, empirical, 'Observational constraints on dark matter particle type').

omega_variable(
    modified_gravity_viability,
    'Can MOND or relativistic modified gravity theories predict the full suite of cosmological observations (CMB, BAO, gravitational lensing) as well as ΛCDM, or do they have their own anomalies?',
    'Joint likelihood analysis fitting CMB + BAO + galaxy surveys + cluster lensing to MOND, relativistic extensions (TeVeS, other), and ΛCDM simultaneously. Compare AIC/BIC scores.',
    'If modified gravity is competitive: ΛCDM is tangled rope (coordination with extraction overhead). If modified gravity fails CMB: ΛCDM is rope (pure coordination), and small-scale anomalies are legitimate unresolved puzzles, not suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modified_gravity_viability, empirical, 'Whether modified gravity can match full cosmological dataset').

omega_variable(
    simulation_convergence_plateau,
    'Is there a fundamental convergence ceiling in ΛCDM structure simulations, or are current anomalies purely numerical artifacts of insufficient resolution?',
    'Exaflop-scale simulations (next-gen Frontier/Aurora facilities) running CDM to 10^12 particle counts. Measure whether small-scale power spectrum, satellite abundance, and alignment statistics stabilize or continue systematic drift.',
    'If plateau reached: anomalies are real physical contradictions, not numerical. If convergence continues: scaffold sunset is plausible — better simulations will resolve tensions without paradigm shift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(simulation_convergence_plateau, empirical, 'Convergence behavior of high-resolution ΛCDM simulations').

omega_variable(
    institutional_theory_capture,
    'Is the suppression of alternative theories (MOND, warm DM, emergent gravity) structural or institutional? Could funding/visibility be redirected without scientific loss?',
    'Bibliometric analysis: citation rates, funding allocation, conference invitation patterns for alternatives vs. ΛCDM over 20 years. Controlled experiment: increase funding for alternatives by 10x for 5 years, measure publication quality and observational traction.',
    'If institutional: the suppression is extractive (snare). If structural (alternatives genuinely less viable): suppression is legitimate coordination cost (tangled rope). The mandatrophy depends on this distinction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_theory_capture, preference, 'Whether small-scale anomaly suppression is institutional or structural').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lcdm_small_scale_anomalies, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lcdm_tr_t0, lcdm_small_scale_anomalies, theater_ratio, 0, 0.35).
narrative_ontology:measurement(lcdm_tr_t10, lcdm_small_scale_anomalies, theater_ratio, 10, 0.5).
narrative_ontology:measurement(lcdm_tr_t20, lcdm_small_scale_anomalies, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(lcdm_be_t0, lcdm_small_scale_anomalies, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(lcdm_be_t10, lcdm_small_scale_anomalies, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(lcdm_be_t20, lcdm_small_scale_anomalies, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lcdm_small_scale_anomalies, information_standard).
narrative_ontology:affects_constraint(lcdm_small_scale_anomalies, cold_dark_matter_particle_detection).
narrative_ontology:affects_constraint(lcdm_small_scale_anomalies, structure_formation_simulations).
narrative_ontology:affects_constraint(lcdm_small_scale_anomalies, modified_gravity_constraints).

% DUAL FORMULATION NOTE:
% The constraint decomposes into large-scale coordination (ΛCDM success at > 1 Mpc, rope, ε≈0.08) and small-scale institutional dominance (ΛCDM model suppression of alternatives at < 1 Mpc, tangled rope, ε≈0.38). These are structurally distinct constraints with different beneficiaries, victims, and institutional mechanisms. The large-scale story is a pure coordination success; the small-scale story is an institutional arrangement with real extraction overhead. Both use ΛCDM in the label, but the ε values differ by factor of 4.75, indicating distinct structural phenomena.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lcdm_small_scale_anomalies, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
