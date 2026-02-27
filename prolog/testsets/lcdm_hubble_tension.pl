% ============================================================================
% CONSTRAINT STORY: lcdm_hubble_tension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   domain: cosmology/observational_physics
 *
 * SUMMARY:
 *   The Hubble Tension represents a 5-sigma discrepancy between
 *   early-universe expansion rate measurements (inferred from Cosmic
 *   Microwave Background via ΛCDM) and late-universe measurements (direct
 *   distance ladder and baryon acoustic oscillations). Since ~2019, when
 *   local H0 measurements exceeded 73 km/s/Mpc while Planck CMB inference
 *   yielded 67 km/s/Mpc, the tension has persisted and sharpened. This
 *   constraint exhibits a tangled_rope structure: ΛCDM provides genuine
 *   coordination (unified parameter space, predictive framework) while
 *   simultaneously extracting from late-universe measurement programs through
 *   institutional enforcement of early-universe values as standard. The
 *   early-universe community (Planck, CMB analysis) benefits from dominance
 *   as the reference frame; the late-universe community (Cepheids, Type Ia
 *   supernovae, BAO) bears the cost of being systematically undervalued. The
 *   theater_ratio (0.65) reflects that much discussion involves calibration
 *   and methodology rather than fundamental model reassessment — both
 *   communities perform rigor without yet resolving the incompatibility. The
 *   constraint's extractiveness (0.38) is moderate because some genuine
 *   measurement and model coordination value persists alongside the tension.
 *
 * KEY AGENTS:
 *   - Early-Universe Measurement Community (Planck, WMAP, CMB analysis): Primary beneficiary (institutional/arbitrage) — establishes the standard H0 value via early-universe physics; captures authority of being the reference frame
 *   - Late-Universe Measurement Community (Cepheid distance ladder, Type Ia supernovae, BAO): Primary victim (powerless/trapped) — measurements systematically diverge from early-universe standard; constrained by observational fidelity to report tensions; face pressure to revise or suppress findings
 *   - ΛCDM Standard Model Framework: Secondary victim (moderate/constrained) — benefits from unified parameter space and predictive success at large scales but extracted from by the incompatibility; requires active enforcement to maintain dominance despite tension
 *   - Alternative Cosmology Researchers: Organized agents (organized/constrained) — see scaffold: the tension motivates beyond-ΛCDM models (early dark energy, modified gravity, evolving dark energy) with sunset logic as observational precision increases
 *   - Distance Ladder Hierarchy (Cepheids → SNe Ia → H0): Institutional actor (institutional/arbitrage) — maintains methodological authority through historical validation but faces degradation as an integrative cosmological tool (piton perspective)
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing measurement tension as inherent to observational cosmology rather than recognizing it as a structural coordination failure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lcdm_hubble_tension, 0.38).
domain_priors:suppression_score(lcdm_hubble_tension, 0.48).
domain_priors:theater_ratio(lcdm_hubble_tension, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lcdm_hubble_tension, extractiveness, 0.38).
narrative_ontology:constraint_metric(lcdm_hubble_tension, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(lcdm_hubble_tension, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lcdm_hubble_tension, tangled_rope).
narrative_ontology:human_readable(lcdm_hubble_tension, "The Hubble Tension within the Lambda-CDM Cosmological Model").
narrative_ontology:topic_domain(lcdm_hubble_tension, "cosmology/observational_physics").

domain_priors:requires_active_enforcement(lcdm_hubble_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lcdm_hubble_tension, early_universe_measurements_community).
narrative_ontology:constraint_beneficiary(lcdm_hubble_tension, cepheid_distance_ladder_programs).
narrative_ontology:constraint_victim(lcdm_hubble_tension, late_universe_measurements_community).
narrative_ontology:constraint_victim(lcdm_hubble_tension, lambda_cdm_model_consistency).
narrative_ontology:constraint_victim(lcdm_hubble_tension, cosmological_parameter_space).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATE-UNIVERSE MEASUREMENT TEAMS (SNARE) — Trapped in a measurement framework (Type Ia supernovae, baryon acoustic oscillations) whose results systematically diverge from the early-universe standard. Cannot exit without abandoning observational fidelity; bears full cost of model inconsistency. Maximum extraction without coordination benefit.
constraint_indexing:constraint_classification(lcdm_hubble_tension, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LAMBDA-CDM MODEL CONSISTENCY (TANGLED ROPE) — The standard model benefits from early-universe precision (CMB measurements enforce parameter constraints) but is extracted from by the incompatibility between early and late measurements. Active enforcement of ΛCDM dominance constrains alternative model exploration. Genuine coordination function (unified parameter space) paired with asymmetric extraction (late-universe pressure).
constraint_indexing:constraint_classification(lcdm_hubble_tension, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: EARLY-UNIVERSE MEASUREMENT COMMUNITY (ROPE) — Primary beneficiary (institutional/arbitrage). CMB precision (Planck, WMAP) establishes the early-universe H0 value via distance ladder. This community captures the coordination benefits of being the standard reference point. Can arbitrage between ΛCDM dominance and potential alternative models. Net coordination, minimal extraction experienced.
constraint_indexing:constraint_classification(lcdm_hubble_tension, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALTERNATIVE COSMOLOGY COALITION (SCAFFOLD) — Organized research groups exploring beyond-ΛCDM models (modified gravity, early dark energy, evolving dark energy) see the tension as a temporary coordination failure with a sunset clause. Low effective extraction because the coalition has agency and a clear exit path: empirical resolution through next-generation observations (DESI, Vera Rubin, CMB-S4). As new data arrives, the tension either resolves (favoring ΛCDM repair) or drives model replacement. Theater remains moderate because alternative models must demonstrate both coordination and lower tension to displace ΛCDM.
constraint_indexing:constraint_classification(lcdm_hubble_tension, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CEPHEID DISTANCE LADDER PROGRAM (PITON) — The hierarchical distance measurement chain (parallax → Cepheids → Type Ia supernovae → Hubble flow) is largely performative at this stage. Each rung has been validated independently, but the integrative framework persists in dominance despite growing tension. The program maintains institutional authority through historical priority and methodological rigor, but its functional role in constraining cosmology has degraded: late-universe H0 measurements no longer serve as coordination benchmarks. Theater is high because continued emphasis on rung calibration performs cosmological authority without resolving the fundamental inconsistency.
constraint_indexing:constraint_classification(lcdm_hubble_tension, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, measurement tensions are inherent to observational science: all experiments have finite precision, and the universe may not conform to human mathematical convenience. Some observers frame the Hubble tension as an immutable property of cosmological measurement — systematic uncertainty cannot be eliminated, model incompleteness is inherent. However, the structural data contradicts the mountain classification: the tension arises from contingent institutional commitments (ΛCDM enforcement, measurement methodology choices) rather than from irreducible physical limits. The engine will compute this as a false summit.
constraint_indexing:constraint_classification(lcdm_hubble_tension, mountain,
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

test(piton_threshold) :-
    domain_priors:theater_ratio(lcdm_hubble_tension, TR),
    TR >= 0.70.

:- end_tests(lcdm_hubble_tension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The early-universe community benefits from ΛCDM dominance during the measurement period, but the benefit is not overwhelming because the tension is widely acknowledged and late-universe programs maintain methodological legitimacy. The extraction is real (institutional pressure to conform to early-universe values) but not as severe as in a pure snare because both sides continue publishing and the scientific community has not reached consensus on suppression. The trajectory shows growth (0.18 → 0.38) reflecting increasing pressure as precision improves without resolution. Suppression (0.48): Moderate-high. Significant barriers to model flexibility include ΛCDM's success at large scales, the institutional investment in the standard framework, and the difficulty of identifying systematic errors in either measurement set. But suppression is not total — alternative models are being actively developed and discussed. Theater ratio (0.65): Moderate-high and rising. Much discussion involves calibration details (Cepheid extinction, Type Ia standardization, Planck foreground modeling) rather than direct model assessment. The performance of rigor (detailed methodology papers, independent confirmations) has increased over time, but the fundamental tension remains unresolved, indicating growing gap between activity level and functional progress. This suggests increasing theater as the field invests effort in methodological refinement without model reassessment.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates sharp perspectival disagreement. Early-universe observers see rope: a coordination mechanism (ΛCDM prediction flow) with beneficial unified parameter space. Late-universe observers see snare: they are trapped in a measurement framework whose results are systematically devalued relative to early-universe constraints. ΛCDM sees itself as tangled_rope: the model provides genuine coordination but is extracted from by the late-universe pressure to modify fundamental assumptions. Alternative cosmologists see scaffold: the tension is a temporary coordination failure with a clear exit path (next-generation surveys will discriminate between models). The cepheid distance ladder sees piton: its own methods are methodologically sound but functionally degraded as a cosmological tool. The civilization-scale analytical perspective risks seeing mountain (measurement tension inherent to cosmology) but the structural data contradicts this — the tension arises from the contingent institutional choice to enforce early-universe values as the standard.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) flows from their structural position. The early-universe community benefits from being the standard reference (beneficiary status + institutional power + arbitrage options → low d → negative χ). The late-universe community bears the cost of systematic undervaluation (victim status + moderate power + trapped/constrained options → high d → high χ). The ΛCDM framework itself is extracted from by the late-universe pressure (victim status + moderate coherence as a model + constrained flexibility → d ≈ 0.55 → elevated χ). The alternative cosmology coalition has higher exit capacity through model flexibility and growing observational support (constrained exit but organized power + coalition structure → moderate d → moderate χ). The cepheid distance ladder occupies a piton position: it has institutional arbitrage capability (can be used across multiple frameworks) but its functional role in cosmological enforcement has degraded (d ≈ 0.40, producing low χ despite institutional status). The tension arises not from a single extraction flow but from two communities with asymmetric power in the coordinating framework.
 *
 * MANDATROPHY ANALYSIS:
 *   STRUCTURAL EVIDENCE FOR TANGLED ROPE: The constraint meets all three gates. (1) Coordination function: ΛCDM provides genuine predictive coordination across multiple observational domains (large-scale structure, weak lensing, primordial nucleosynthesis); this is not theater. (2) Beneficiary and victim structure: Early-universe community benefits (CMB primacy); late-universe community bears cost (forced conformity). (3) Active enforcement: ΛCDM dominance is maintained by institutional preference, pedagogical inertia, and funding concentration in standard model research. The extraction is real (late-universe teams receive lower citations, face pressure to revise findings, have reduced model flexibility) but not total (alternative models exist, some funding flows to alternatives). The theater_ratio (0.65) is high but not extreme, indicating that cosmological activity is substantially real (detailed observational work, hypothesis testing) rather than purely performative. The mandatrophy resolves because the framework's coordination benefits (unified H0 inference across multiple methods) are genuine but incomplete — the framework fails to accommodate growing precision in late-universe measurements. This is classic tangled_rope: coordination that is asymmetrically enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    early_universe_systematic_errors,
    'Do early-universe measurements (Planck CMB recombination physics, sound horizon calibration) contain undetected systematic errors that inflate the inferred H0?',
    'Independent CMB missions with different systematic architectures (Simons Observatory, CMB-S4); recalibration of recombination physics with updated nuclear and atomic data; comparison of sound horizon measurements via baryon acoustic oscillations vs CMB',
    'If early-universe H0 is corrected downward by ~2σ: tension resolves in favor of late-universe measurements, ΛCDM reinforced. If early-universe measurements are correct: late-universe systematic errors or new physics required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(early_universe_systematic_errors, empirical, 'Potential systematic errors in early-universe measurements').

omega_variable(
    late_universe_population_heterogeneity,
    'Do Type Ia supernovae and baryon acoustic oscillations exhibit unaccounted-for population heterogeneity or evolution that biases H0 inference?',
    'Spectroscopic follow-up of large supernova samples; correlation analysis of SNe Ia properties with host galaxy environment; direct BAO measurements at multiple redshifts with uniform methodology; redshift-space distortion analysis',
    'If late-universe population effects are corrected: H0 estimates could shift toward early-universe values. If no correction is possible: new physics or modified gravity required to reconcile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(late_universe_population_heterogeneity, empirical, 'Unaccounted population heterogeneity in late-universe measurements').

omega_variable(
    dark_energy_evolution,
    'Does dark energy density evolve with cosmic time (w(a) ≠ -1) in a way that reconciles early and late measurements without modifying gravity?',
    'Constraining dark energy equation of state at multiple redshifts via: Type Ia supernovae spectroscopy, weak gravitational lensing magnification-bias analysis, combination with large-scale structure growth rate measurements',
    'If dark energy evolves significantly: ΛCDM can be extended (w(a) parameterization) without replacing it. If w remains constant near -1: new physics (modified gravity, early dark energy) becomes necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dark_energy_evolution, empirical, 'Evolution of dark energy equation of state').

omega_variable(
    spatial_isotropy_violation,
    'Is the tension partly an artifact of assuming perfect spatial isotropy in cosmological analysis, when inhomogeneities along our light cone could modify the effective expansion rate?',
    'Analysis of large-scale structure directionality; constraints on dipole and quadrupole moments of measured quantities; full-sky analysis of H0 measurements by redshift shell',
    'If isotropy violations are significant: new parameterization of inhomogeneous cosmology required; H0 becomes position-dependent. If isotropy holds: tension remains a fundamental inconsistency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(spatial_isotropy_violation, empirical, 'Violation of spatial isotropy assumption').

omega_variable(
    coordination_model_flexibility,
    'Is ΛCDM enforcement (requiring a single unified parameter space) extracting from late-universe measurements by forcing them into a framework that doesn''t accommodate their data?',
    'Comparative analysis of model likelihoods when ΛCDM is treated as one among multiple coordinated models vs when it is mandated as the standard; assessment of how many ΛCDM modifications are required to match current data',
    'If ΛCDM is overly rigid: relaxing the unified framework could resolve the tension as a tangled_rope → scaffolding transition. If ΛCDM flexibility is sufficient: tension reflects genuine physical inconsistency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_model_flexibility, conceptual, 'Whether ΛCDM institutional dominance constrains inference').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lcdm_hubble_tension, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hubble_tr_t0, lcdm_hubble_tension, theater_ratio, 0, 0.42).
narrative_ontology:measurement(hubble_tr_t5, lcdm_hubble_tension, theater_ratio, 5, 0.54).
narrative_ontology:measurement(hubble_tr_t10, lcdm_hubble_tension, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(hubble_be_t0, lcdm_hubble_tension, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(hubble_be_t5, lcdm_hubble_tension, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(hubble_be_t10, lcdm_hubble_tension, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lcdm_hubble_tension, information_standard).
narrative_ontology:affects_constraint(lcdm_hubble_tension, dark_energy_equation_of_state).
narrative_ontology:affects_constraint(lcdm_hubble_tension, early_dark_energy_proposal).
narrative_ontology:affects_constraint(lcdm_hubble_tension, modified_gravity_constraints).

% DUAL FORMULATION NOTE:
% The Hubble tension decomposes into at least two structurally distinct constraints: (1) Early-universe H0 inference from CMB physics (ε ≈ 0.08, Mountain-like — recombination physics is well-established), and (2) Late-universe H0 measurement via distance ladder (ε ≈ 0.12, Rope-like — coordination mechanism across multiple probes). The tension emerges from their incompatibility within ΛCDM enforcement (ε ≈ 0.38, Tangled Rope). Each story has different empirical status and different resolution paths. The Hubble tension story models the institutional constraint that enforces their unified interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lcdm_hubble_tension, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
