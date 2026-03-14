% ============================================================================
% CONSTRAINT STORY: planetary_migration_model_coupling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_planetary_migration_model_coupling, []).

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
 *   constraint_id: planetary_migration_model_coupling
 *   human_readable: Planetary Migration Model Coupling in Exoplanet System Inference
 *   domain: exoplanet_science/orbital_dynamics
 *
 * SUMMARY:
 *   Planetary migration models are dynamical frameworks that explain how
 *   planets move through protoplanetary disks, losing orbital energy through
 *   disk interactions and scattering off other planets. These models have
 *   become the dominant explanatory framework for understanding observed
 *   exoplanet orbital architectures — compact resonant systems, wide gaps,
 *   eccentric orbits, oblique spin-orbit alignments. The constraint arises
 *   from the tight coupling between migration model assumptions and the
 *   inference of system parameters from observations. When astronomers
 *   observe a compact planetary system, they use migration models to
 *   constrain formation history and orbital parameters. This creates a
 *   feedback loop: migration models explain the systems → systems validate
 *   migration models → migration models become the standard inference tool.
 *   Alternative formation mechanisms (in-situ formation, disk fragmentation,
 *   scattering without migration) struggle for observational pathway under
 *   this coupling because the inference tools are optimized for migration and
 *   the literature defaults to migration interpretations. The constraint
 *   exhibits properties of both coordination (migration models genuinely
 *   speed parameter inference and compress computation) and extraction (the
 *   coordination benefit flows asymmetrically to model developers and
 *   institutional frameworks while alternative pathways face systematic
 *   friction).
 *
 * KEY AGENTS:
 *   - Migration Model Developers: Primary beneficiary (institutional/arbitrage) — capture citation advantage, funding priority, and control over framework standards; can adjust coupling constants and model parameters without friction
 *   - Alternative Formation Pathways: Primary victim (powerless/trapped) — in-situ formation, disk fragmentation, and scattering mechanisms face publication bias and grant funding disadvantage; no structural exit pathway
 *   - Observational Astronomers: Secondary victim (moderate/constrained) — must use migration models to interpret observations; face career friction publishing non-migration interpretations; also benefit from rapid parameter inference
 *   - Established Orbital Framework: Secondary beneficiary (institutional/arbitrage) — historical institutional actor; benefits from continued relevance and model standardization
 *   - Machine Learning Community: Organized alternative (organized/mobile) — building ML-based inference methods that bypass explicit migration models; have exit pathway and organized resources
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination function and asymmetric extraction; identifies circular validation closure risk
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(planetary_migration_model_coupling, 0.38).
domain_priors:suppression_score(planetary_migration_model_coupling, 0.48).
domain_priors:theater_ratio(planetary_migration_model_coupling, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(planetary_migration_model_coupling, extractiveness, 0.38).
narrative_ontology:constraint_metric(planetary_migration_model_coupling, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(planetary_migration_model_coupling, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(planetary_migration_model_coupling, tangled_rope).
narrative_ontology:human_readable(planetary_migration_model_coupling, "Planetary Migration Model Coupling in Exoplanet System Inference").
narrative_ontology:topic_domain(planetary_migration_model_coupling, "exoplanet_science/orbital_dynamics").

domain_priors:requires_active_enforcement(planetary_migration_model_coupling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(planetary_migration_model_coupling, migration_model_developers).
narrative_ontology:constraint_beneficiary(planetary_migration_model_coupling, established_orbital_framework).
narrative_ontology:constraint_victim(planetary_migration_model_coupling, alternative_formation_hypotheses).
narrative_ontology:constraint_victim(planetary_migration_model_coupling, observational_constraint_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALTERNATIVE FORMATION PATHWAY (SNARE) — Cannot exit the inference pipeline once migration models are entrenched. Alternative mechanisms (disk fragmentation, scattering, in-situ formation) face systematic disadvantage in publication and grant funding. No structural exit: these pathways bear the cost of being subordinated without the benefit of the coordination they provide.
constraint_indexing:constraint_classification(planetary_migration_model_coupling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OBSERVATIONAL ASTRONOMER (TANGLED ROPE) — Constrained by the need to use migration models to interpret data, but also genuinely benefits from the coordination that model standards provide. Can design new observations but faces career friction publishing non-migration interpretations. Moderate extraction with genuine but asymmetric coordination benefit.
constraint_indexing:constraint_classification(planetary_migration_model_coupling, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MIGRATION MODEL CONSORTIUM (ROPE) — Institutional beneficiary. Experiences the constraint as pure coordination: standardized migration models enable rapid system characterization and parameter inference. Net beneficiary with arbitrage options (can pivot to new models, adjust frameworks, adjust coupling constants without cost). Extraction runs toward this agent.
constraint_indexing:constraint_classification(planetary_migration_model_coupling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LEGACY ORBITAL MECHANICS FRAMEWORK (PITON) — Historical institutional actor. Migration models were built to explain observed orbital architectures (compact systems, resonances, gaps), but the framework has become self-referential: systems are 'explained' by migration, migration is 'validated' by reproducing observed systems. Theater ratio high because the framework now persists through institutional inertia (textbooks, training, funding structures) rather than explanatory power. Alternative approaches struggle for resources.
constraint_indexing:constraint_classification(planetary_migration_model_coupling, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MACHINE LEARNING ALTERNATIVE (SCAFFOLD) — Organized agents (new simulation frameworks, neural network inference, Bayesian hierarchical models) are building alternative pathways that bypass the need for explicit migration model coupling. These methods can extract orbital parameters from observations directly without physics-based intermediaries. Temporary coordination with a sunset: as ML methods mature and gain empirical validation, the need for migration model coupling decreases. Low experienced extraction because exit is visible and organized.
constraint_indexing:constraint_classification(planetary_migration_model_coupling, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, migration models provide genuine coordination: they compress decades of N-body simulation into fast forward models, enabling rapid inference from observational data. But the constraint exhibits asymmetric extraction: the coordination benefit flows primarily to model developers and institutional frameworks, while observational constraints and alternative pathways bear the cost of incompatibility. The system requires active enforcement (model validation, coupling parameter justification, systematic bias correction) to function.
constraint_indexing:constraint_classification(planetary_migration_model_coupling, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(planetary_migration_model_coupling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(planetary_migration_model_coupling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(planetary_migration_model_coupling, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(planetary_migration_model_coupling, TR),
    TR >= 0.70.

:- end_tests(planetary_migration_model_coupling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Migration models do provide genuine coordination — they reduce computational cost from months of N-body simulation to seconds of forward modeling, enabling rapid parameter inference across large datasets. But the constraint exhibits asymmetric extraction: the coordination benefit is captured primarily by model developers and institutional frameworks, while alternative formation mechanisms are systematically subordinated. The value reflects this hybrid: not zero (genuine coordination exists) but significant (extractive friction on alternatives is real). Suppression (0.48): Moderate. Barriers to publishing alternative formation hypotheses include publication bias toward migration explanations, reviewer expertise weighted toward migration models, and funding agency focus on standard frameworks. However, suppression is not total — alternative mechanisms do publish, and some observational gaps are actively being addressed. Theater ratio (0.65): Moderately high and increasing. As of interval end, migration model validation has increasingly performative character: parameters are adjusted post-hoc to fit observations, model variants proliferate to explain each new discovery, and circular validation (models explain systems, systems validate models) dominates the literature. The ratio has increased over the interval as the field has matured and the number of exoplanet discoveries has accelerated, making explicit testing increasingly difficult.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full perspectival gap between beneficiary and victim positions. The migration model consortium sees pure coordination (Rope) — the constraint solves their central problem of rapid parameter inference. The legacy framework sees performative ritual (Piton) — aware that validation is increasingly circular, maintained by inertia rather than explanatory force. The ML alternative sees a temporary problem with organized exit (Scaffold) — newer methods bypass migration coupling entirely, and resources are flowing toward these alternatives. Observational astronomers see mixed coordination and extraction (Tangled Rope) — the system enables their work but constrains their interpretive freedom. Alternative formation pathways see pure extraction (Snare) — they are completely subordinated with no structural exit. The analytical observer identifies the constraint as tangled rope: genuine coordination function exists (parameter inference cost reduction) alongside asymmetric extraction (institutional gatekeeping on alternatives). The theater ratio rising from 0.40 to 0.65 indicates that the performative aspect (model validation as ritual rather than genuine test) is increasingly dominant.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from structural position: beneficiary + arbitrage → low d → low/negative χ; trapped victim → high d → high χ; moderate constrained agent → intermediate d → intermediate χ. The migration model consortium (institutional/arbitrage) has low d because they benefit from the constraint with no exit friction. Alternative formation pathways (powerless/trapped) have high d because they are structurally subordinated. Observational astronomers (moderate/constrained) have intermediate d reflecting genuine benefit (can use models) plus real cost (career friction for alternatives). The ML alternative (organized/mobile) has intermediate-low d because organized agents with exit pathways experience lower effective extraction. The legacy orbital framework (institutional/constrained) shows moderate d despite institutional status because it is constrained by the need to justify coupling via circular validation — unlike the consortium, it cannot arbitrage to new frameworks without loss of authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by identifying the constraint as genuinely tangled: migration models provide both coordination (computational efficiency, parameter inference) and extraction (institutional gatekeeping on alternatives). The false summit would be classifying this as pure mountain (orbital mechanics are invariant laws) or pure rope (coordination without extraction). The structural data shows extraction is real — alternatives face systematic friction — but coordination is also real — inference would be far slower and more computationally expensive without model frameworks. The key unresolved question is whether the coupling is structurally necessary (high extraction but justified by coordination gain) or institutionally contingent (high extraction, low coordination justification, circle validation artifact). The omega variables target this ambiguity. The piton classification from the legacy framework perspective indicates that part of the extraction is theater rather than function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    model_coupling_necessity,
    'Is the tight coupling between migration models and orbital system inference structurally necessary or institutionally contingent?',
    'Comparative performance analysis: direct Bayesian inference on orbital parameters without migration intermediate vs. migration-coupled inference on identical datasets; convergence rates and parameter uncertainty profiles',
    'If necessary: coupling is coordination cost (lower extraction estimates). If contingent: coupling is institutional gatekeeping (higher extraction; snare classification strengthened across perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(model_coupling_necessity, empirical, 'Whether tight model coupling is structurally necessary or institutionally contingent').

omega_variable(
    alternative_formation_observability,
    'Are in-situ formation, disk fragmentation, and scattering formation mechanisms distinguishable in observational signature from migration-sculpted systems?',
    'Predictive signature comparison: explicit predictions from each mechanism for architecture distributions, resonance occupancy, eccentricity correlations, then direct observational test on new exoplanet discoveries',
    'If distinguishable: alternative pathways have genuine observational pathway (exit available; snare classification weakened). If indistinguishable: alternatives cannot be empirically validated under migration-coupled inference (snare classification strengthened; extraction becomes fundamental).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_formation_observability, empirical, 'Whether alternative formation mechanisms produce distinguishable observational signatures').

omega_variable(
    circular_validation_closure,
    'Does the circular validation loop (migration models explain systems, systems validate migration models) prevent falsification of the coupling assumption itself?',
    'Auditing of model validation papers: frequency of attempts to falsify migration mechanism vs. frequency of retroactive parameter adjustment to fit observations; statistical power analysis of falsification studies',
    'If closed: the constraint exhibits piton (performative theater) and snare (no exit for alternatives). If open: constraints are genuine uncertainty bounds, not institutional protection. This determines whether the theater_ratio (0.65) reflects methodological reality or institutional ossification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(circular_validation_closure, empirical, 'Whether circular validation prevents falsification of the coupling assumption').

omega_variable(
    ml_inference_sufficiency,
    'Can machine learning methods extract orbital parameters with comparable accuracy and physical interpretability to migration-coupled inference?',
    'Blind comparison: ML models trained on synthetic migration-produced systems vs. migration models on the same systems; cross-validation on held-out synthetic datasets and real exoplanet observations; measurement of parameter bias and calibration properties',
    'If sufficient: scaffold perspective is valid; sunset clause is real (ML methods mature, coupling becomes optional). If insufficient: ML alternative is aspirational rather than structural; scaffold becomes aspirational piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ml_inference_sufficiency, empirical, 'Whether ML methods can replace migration-coupled inference').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(planetary_migration_model_coupling, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pmmc_tr_t0, planetary_migration_model_coupling, theater_ratio, 0, 0.4).
narrative_ontology:measurement(pmmc_tr_t5, planetary_migration_model_coupling, theater_ratio, 5, 0.55).
narrative_ontology:measurement(pmmc_tr_t10, planetary_migration_model_coupling, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(pmmc_be_t0, planetary_migration_model_coupling, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(pmmc_be_t5, planetary_migration_model_coupling, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(pmmc_be_t10, planetary_migration_model_coupling, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(planetary_migration_model_coupling, information_standard).
narrative_ontology:affects_constraint(planetary_migration_model_coupling, exoplanet_formation_observability).
narrative_ontology:affects_constraint(planetary_migration_model_coupling, orbital_resonance_interpretation).
narrative_ontology:affects_constraint(planetary_migration_model_coupling, protoplanetary_disk_dynamics).

% DUAL FORMULATION NOTE:
% Planetary migration model coupling is downstream of specific theoretical models (peas-in-a-pod, grand tack, streaming instability interactions) and upstream of observational inference for individual systems. This story captures the constraint at the framework level; decomposition into specific model variants would produce separate stories with higher extractiveness (more performative ritual, less coordination function) and lower omegas (specific mechanisms tested more directly).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
