% ============================================================================
% CONSTRAINT STORY: cold_dark_matter_paradigm
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cold_dark_matter_paradigm, []).

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
 *   constraint_id: cold_dark_matter_paradigm
 *   human_readable: The Lambda-CDM model's 'cold dark matter' tenet as a dominant scientific paradigm
 *   domain: cosmology/fundamental_physics
 *
 * SUMMARY:
 *   The Lambda-CDM (ΛCDM) standard model of Big Bang cosmology is one of the
 *   most successful frameworks in science, with extraordinary predictive
 *   power for large-scale structure, the cosmic microwave background, and
 *   Type Ia supernovae. Yet it embeds a structural constraint that functions
 *   simultaneously as coordinating mechanism and extractive system: the 'cold
 *   dark matter' tenet as a non-negotiable element of the paradigm. This
 *   constraint operates through institutional gatekeeping (funding
 *   concentration, journal access, peer review authority) that privileges
 *   ΛCDM-compliant research while suppressing or marginalizing alternatives
 *   like MOND, modified gravity, or emergent gravity theories. The constraint
 *   exhibits a hybrid structure: it genuinely coordinates the cosmological
 *   research community around a unified theoretical framework with shared
 *   parameter spaces and experimental programs, yet simultaneously extracts
 *   by concentrating career opportunities and institutional legitimacy in a
 *   single paradigm. The theater ratio (0.65) reflects that much cosmological
 *   effort is spent in parameter fitting and observational reinterpretation
 *   to maintain ΛCDM's dominance rather than in fundamental questioning of
 *   its assumptions. Recent tensions (Hubble tension, small-scale structure
 *   problems, CMB dipole anomalies) have accumulated without triggering
 *   paradigm revision, suggesting the constraint's suppression mechanism is
 *   effectively preventing genuine scientific competition.
 *
 * KEY AGENTS:
 *   - CDM Research Establishment: Primary beneficiary (institutional/arbitrage) — major cosmology departments, survey collaborations, funding agencies benefit from ΛCDM as a coordination framework
 *   - Alternative Cosmology Researchers: Primary victim (powerless/trapped) — face career barriers, funding scarcity, journal gatekeeping, publication bias against alternative gravity theories
 *   - Observational Anomaly Detectors: Secondary victim (moderate/constrained) — researchers studying tensions are systematically reinterpreted to preserve ΛCDM rather than trigger model revision
 *   - Competing Paradigm Coalition: Organized alternative (organized/mobile) — MOND/TeVeS/emergent gravity groups benefit from counter-institutional networks but face structural barriers
 *   - ΛCDM Simulation Infrastructure: Institutional actor (organized/constrained) — N-body codes and numerical frameworks encode CDM assumptions; infrastructure inertia maintains paradigm dominance
 *   - Textbook Paradigm Authority: Institutional gatekeeper (institutional/arbitrage) — curricula and textbooks present ΛCDM as 'standard model' through pedagogical selectivity and path-dependency
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent institutional paradigm choice as an immutable feature of cosmology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cold_dark_matter_paradigm, 0.38).
domain_priors:suppression_score(cold_dark_matter_paradigm, 0.42).
domain_priors:theater_ratio(cold_dark_matter_paradigm, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cold_dark_matter_paradigm, extractiveness, 0.38).
narrative_ontology:constraint_metric(cold_dark_matter_paradigm, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(cold_dark_matter_paradigm, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cold_dark_matter_paradigm, tangled_rope).
narrative_ontology:human_readable(cold_dark_matter_paradigm, "The Lambda-CDM model's 'cold dark matter' tenet as a dominant scientific paradigm").
narrative_ontology:topic_domain(cold_dark_matter_paradigm, "cosmology/fundamental_physics").

domain_priors:requires_active_enforcement(cold_dark_matter_paradigm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cold_dark_matter_paradigm, cdm_institutional_infrastructure).
narrative_ontology:constraint_beneficiary(cold_dark_matter_paradigm, standard_model_establishment).
narrative_ontology:constraint_victim(cold_dark_matter_paradigm, alternative_cosmology_researchers).
narrative_ontology:constraint_victim(cold_dark_matter_paradigm, observational_anomalies_resolution).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALTERNATIVE COSMOLOGIST (SNARE) — Career incentives are heavily concentrated in ΛCDM: funding, journal access, peer review gatekeeping, and institutional positions favor CDM-compliant research. Proposing MOND, emergent gravity, or modified-inertia alternatives requires defending against institutional skepticism with limited resources. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.63.
constraint_indexing:constraint_classification(cold_dark_matter_paradigm, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OBSERVATIONAL ANOMALY DETECTOR (TANGLED ROPE) — Researchers studying galactic rotation curves, bullet cluster dynamics, structure formation tensions, and HST Hubble tension see benefits (collaborative data sharing, large surveys) and costs (anomalies are systematically reinterpreted to fit ΛCDM rather than trigger model revision). d≈0.68, f(d)≈1.05, σ=1.2 → χ≈0.48.
constraint_indexing:constraint_classification(cold_dark_matter_paradigm, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CDM RESEARCH ESTABLISHMENT (ROPE) — Major cosmology departments, survey collaborations (Planck, SDSS, LSST), and funding agencies benefit from ΛCDM as a coordination mechanism: unified theoretical framework, clear experimental programs, shared parameter space. Institutions have exit through publication priority and funding capture. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.04.
constraint_indexing:constraint_classification(cold_dark_matter_paradigm, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ΛCDM NUMERICAL SIMULATION INFRASTRUCTURE (PITON) — N-body simulations (Millennium Run, Illustris, EAGLE) encode ΛCDM assumptions throughout: particle physics, initial conditions, parameter defaults. Theater_ratio=0.65: much simulation effort is spent fine-tuning parameter spaces rather than testing fundamental assumptions. Infrastructure persists through institutional inertia and computational investment despite increasing tensions in the model. d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.25.
constraint_indexing:constraint_classification(cold_dark_matter_paradigm, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TEXTBOOK PARADIGM AUTHORITY (PITON) — Graduate curricula and textbooks present ΛCDM as the 'standard model' through institutional repetition and curriculum path-dependency. Theater_ratio=0.65: pedagogical exposition of ΛCDM success (CMB acoustic peaks, large-scale structure, SNe luminosity distances) is legitimate but tends to underweight or relegate tensions (small-scale structure problems, CMB dipole anomalies, Hubble tension) to specialized chapters or footnotes. The presentation is performative in its selectivity. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.04.
constraint_indexing:constraint_classification(cold_dark_matter_paradigm, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: COMPETING PARADIGM COALITION (TANGLED ROPE) — MOND/TeVeS researchers, emergent gravity groups, and modified-inertia theorists benefit from collaborative networks and counter-institutional resources (conferences, preprints, alternative funding), but face structural barriers (fewer journals, lower citation counts, limited institutional positions). d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.35.
constraint_indexing:constraint_classification(cold_dark_matter_paradigm, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the primacy of gravitational dynamics and the existence of a dominant matter component (whatever its nature) are features of the universe itself, not contingent paradigm choices. The specific identity of dark matter may be unknown, but its dominance in the matter budget is a constraint independent of observational preference. However, ε=0.38 and suppression=0.42 contradict the mountain gate — this is a false summit. The paradigm is contingent institutional arrangement, not universal law.
constraint_indexing:constraint_classification(cold_dark_matter_paradigm, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cold_dark_matter_paradigm_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cold_dark_matter_paradigm, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cold_dark_matter_paradigm, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(cold_dark_matter_paradigm, TR),
    TR >= 0.70.

:- end_tests(cold_dark_matter_paradigm_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. Career and funding incentives are heavily concentrated in ΛCDM research. Researchers proposing alternatives face substantial barriers (reduced publication venues, lower citations, limited institutional positions, weaker funding prospects). However, the extraction is not absolute — heterodox research exists and publishes, alternative theorists maintain modest institutional presence, and collaborative networks provide some exit routes. Measured extractiveness reflects this intermediate regime. Suppression (0.42): Moderate-high. Institutional gatekeeping through peer review, editorial authority, and funding allocation creates significant barriers to alternative research. However, suppression is not total — arXiv preprints bypass journal gatekeeping, international collaborations dilute single-region dominance, and observational techniques are distributed. The measurement reflects effective but incomplete suppression. Theater ratio (0.65): Moderate-high. Much cosmological effort is legitimately spent fitting ΛCDM to increasingly precise observations (Planck satellite, weak lensing surveys, large-scale structure), but a significant portion is spent reinterpreting observational tensions to preserve paradigm coherence rather than questioning fundamental assumptions. Parameter space exploration, baryon physics fine-tuning, and alternative-hypothesis suppression in interpretation occupy substantial fraction of literature. The trajectory shows rising theater over the 30-year interval as tensions accumulate without triggering genuine revision.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates contrasting perceptions of the same structural phenomenon across the research ecosystem. The CDM establishment sees coordination (Rope): a unified framework enabling collaborative research and shared parameter exploration. Alternative researchers see pure extraction (Snare): career barriers and institutional discrimination without compensating benefits. Observational researchers see mixed extraction and coordination (Tangled Rope): they benefit from collaborative survey infrastructure but face systematic reinterpretation of their findings to preserve ΛCDM. The simulation infrastructure sees its own degradation (Piton): N-body codes carry ΛCDM assumptions throughout but achieve results through parameter fitting rather than fundamental testing. The textbook authority maintains performative paradigm transmission (Piton): accurate exposition of successes masks selective treatment of tensions. The analytical observer risks naturalizing a contingent institutional choice (Mountain): seeing paradigm dominance as an inherent feature of cosmology rather than a reversible institutional outcome. The competing paradigm coalition sees partial organization (Tangled Rope): they have networks and alternative research programs but face suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   CDM Research Establishment: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary; has exit through publication priority. Alternative Cosmologists: Victim + trapped → d≈0.92, f(d)≈1.38. High extraction; limited exit options within mainstream academia. Observational Anomaly Researchers: Victim + constrained → d≈0.68, f(d)≈1.05. Moderate-high extraction; constrained exit through alternative interpretations. Competing Paradigm Coalition: Organized + mobile → d≈0.55, f(d)≈0.75. Moderate extraction; coalition has agency and can maintain alternative networks despite barriers. Simulation Infrastructure: Organized + constrained → d≈0.35, f(d)≈0.32. Low-moderate extraction; infrastructure entrenchment prevents full exit. Textbook Authority: Institutional + arbitrage → d≈0.08, f(d)≈-0.08. Institutional benefit; has authority to legitimize paradigm. Analytical Observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification risks false summit: naturalizing paradigm choice.
 *
 * MANDATROPHY ANALYSIS:
 *   The Lambda-CDM constraint exemplifies how a genuine coordinate mechanism can accumulate extractive properties without converting to pure snare. ΛCDM provides real coordination value: unified parameter space, clear experimental programs, shared computational infrastructure. Yet the paradigm simultaneously suppresses legitimate alternatives and reinterprets troubling observations to preserve dominance. The mandatrophy is resolved by recognizing that the classification is perspectival and temporal: from the research establishment's perspective, it is legitimate coordination (Rope). From the alternative researcher's perspective, it is pure extraction (Snare). The tangled-rope classification reflects the constraint's actual structure: it performs coordination work while extracting through institutional gatekeeping. The increasing theater ratio (0.48→0.65 over 30 years) shows Goodhart drift: as observational tensions accumulate, more effort must be spent reinterpreting them to maintain paradigm coherence, reducing the functional coordination value and increasing the extractive character. The constraint persists not because the evidence forces it, but because the institutional entrenchment makes paradigm revision costly and because cosmological research has become centralized enough that unified frameworks serve career and funding interests. Resolution would require either: (1) emergence of compelling direct evidence for paradigm alternatives (detection or exclusion of specific mechanisms), (2) institutional diversification that reduces concentration in single paradigm, or (3) deliberate sunset mechanisms (e.g., dedicated funding for alternative-gravity research, alternative theoretical workshops, publication venues for heterodox ideas).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    small_scale_crisis_severity,
    'Are small-scale structure tensions (too-big-to-fail problem, core-cusp problem, missing satellites problem) symptoms of ΛCDM''s incompleteness or artifacts of incomplete baryon physics modeling?',
    'High-resolution hydrodynamical simulations incorporating baryonic feedback, star formation, and supernova/AGN feedback across multiple independent codes; comparison of prediction accuracy for observed dwarf galaxy properties across ΛCDM+baryon models vs MOND',
    'If ΛCDM+baryon refinements resolve 70%+ of tensions: ΛCDM survives, constraint persists. If tensions remain systematic: paradigm shift to alternative gravity becomes plausible, extraction mechanism loses force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_scale_crisis_severity, empirical, 'Whether small-scale tensions reflect ΛCDM failure or incomplete baryon physics').

omega_variable(
    hubble_tension_cosmological_significance,
    'Does the Hubble tension (σ≈5 between early-time and late-time measurements) indicate new physics beyond ΛCDM or systematic observational errors?',
    'Independent distance ladder measurements (Cepheid variables, tip of red giant branch, gravitational lensing time delays); early-universe recombination physics independent of CMB (BBN, other probes); systematics analysis in Type Ia supernova standardization',
    'If new physics confirmed: alternative models (early dark energy, interacting dark energy, modified gravity) become competitive. If systematics resolved: ΛCDM paradigm robustness restored, suppression mechanism remains effective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hubble_tension_cosmological_significance, empirical, 'Whether Hubble tension indicates new physics beyond ΛCDM').

omega_variable(
    dark_matter_direct_detection_possibility,
    'Is direct dark matter detection (WIMP interactions, axion coupling) a realistic experimental prospect or a motivation-driven research program that presupposes ΛCDM''s validity?',
    'Negative results from ton-scale direct detection experiments over 5-10 year timescale; axion search null results across specified mass ranges; theoretical naturalness arguments in particle physics',
    'If detection remains elusive: particle dark matter becomes unmotivated, paradigm vulnerability increases. If detection succeeds: ΛCDM validation increases, suppression of alternatives strengthens further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dark_matter_direct_detection_possibility, empirical, 'Whether direct dark matter detection is experimentally feasible').

omega_variable(
    alternative_gravity_viability_assessment,
    'Can modified-gravity theories (MOND, TeVeS, f(R), emergent gravity) simultaneously explain the cosmic microwave background acoustic peaks, large-scale structure growth, gravitational lensing, and galaxy clusters without invoking dark matter, or do they require dark-matter-equivalent components to match observations?',
    'Comprehensive fitting of modified-gravity models to combined CMB, LSS, weak-lensing, and cluster datasets; comparison of parameter space degeneracies and theoretical naturalness vs ΛCDM',
    'If alternatives can match observations equally: paradigm choice becomes preference-based, ΛCDM suppression becomes asymmetric enforcement (true snare). If alternatives require dark-matter equivalents: ΛCDM''s fundamental advantage is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_gravity_viability_assessment, empirical, 'Whether modified-gravity theories can replace ΛCDM without invoking dark matter equivalents').

omega_variable(
    institutional_entrenchment_reversibility,
    'If observational evidence begins accumulating against ΛCDM, how quickly can the institutional research ecosystem redirect toward alternatives, and what are the sunk-cost barriers?',
    'Historical analysis of paradigm shifts in cosmology and physics (heliocentrism, relativistic gravity, inflationary cosmology); survey of researcher career flexibility and funding allocation responsiveness; analysis of simulation code dependencies and infrastructure migration costs',
    'If entrenchment is reversible (≤5 year reorientation timescale): suppression mechanism is relatively weak, paradigm is resilient through evidence. If entrenchment is sticky (>10 year reorientation timescale): suppression is self-reinforcing, paradigm persists despite evidence against it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_entrenchment_reversibility, empirical, 'Whether institutional commitment to ΛCDM is reversible if evidence shifts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cold_dark_matter_paradigm, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cdm_tr_t0, cold_dark_matter_paradigm, theater_ratio, 0, 0.48).
narrative_ontology:measurement(cdm_tr_t15, cold_dark_matter_paradigm, theater_ratio, 15, 0.58).
narrative_ontology:measurement(cdm_tr_t30, cold_dark_matter_paradigm, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(cdm_be_t0, cold_dark_matter_paradigm, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cdm_be_t15, cold_dark_matter_paradigm, base_extractiveness, 15, 0.31).
narrative_ontology:measurement(cdm_be_t30, cold_dark_matter_paradigm, base_extractiveness, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cold_dark_matter_paradigm, information_standard).
narrative_ontology:affects_constraint(cold_dark_matter_paradigm, dark_matter_detection_experimental_paradigm).
narrative_ontology:affects_constraint(cold_dark_matter_paradigm, modified_gravity_alternative_viability).
narrative_ontology:affects_constraint(cold_dark_matter_paradigm, cosmological_parameter_estimation_framework).

% DUAL FORMULATION NOTE:
% The Lambda-CDM constraint family decomposes into three distinct empirical claims: (1) the existence and dominance of a pressureless cold matter component (ε≈0.08, Mountain from most perspectives — observationally robust), (2) the appropriateness of ΛCDM as the ONLY viable cosmological framework given current data (ε≈0.38, Tangled Rope — this story), and (3) the particle dark matter interpretation of the cold matter component specifically (ε≈0.45, Tangled Rope/Snare — separate story on direct detection paradigm). The first claim is nearly observation-independent; the second and third are institutional and are linked via network effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cold_dark_matter_paradigm, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
