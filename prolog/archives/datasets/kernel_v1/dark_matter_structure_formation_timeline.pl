% ============================================================================
% CONSTRAINT STORY: dark_matter_structure_formation_timeline
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dark_matter_structure_formation_timeline, []).

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
 *   constraint_id: dark_matter_structure_formation_timeline
 *   human_readable: Dark Matter Structure Formation Timeline
 *   domain: cosmology/astrophysics
 *
 * SUMMARY:
 *   The dark matter structure formation timeline is a natural law constraint
 *   describing the timescale on which density perturbations in the early
 *   universe collapse under gravity to form the large-scale structure
 *   observed today — the cosmic web of galaxies, clusters, and filaments.
 *   This timescale is set by fundamental cosmological parameters (the matter
 *   density Ω_m, the expansion history H(z) determined by dark energy, and
 *   the primordial perturbation spectrum from inflation) combined with
 *   gravitational dynamics. Unlike institutional or engineered constraints,
 *   the formation timeline cannot be negotiated, lobbied, or reformed — it is
 *   an invariant consequence of physics. All observable perspectives agree:
 *   the timeline is immutable. The constraint exhibits zero degrees of
 *   freedom across all indices (agent_power, time_horizon, exit_options,
 *   spatial_scope). Even observers with maximal power and mobility cannot
 *   change when structures formed or how fast they assembled. This is the
 *   defining signature of a mountain constraint in the Deferential Realism
 *   framework: multiple independent observers from different structural
 *   positions all perceive the same unchangeable boundary.
 *
 * KEY AGENTS:
 *   - The Universe: Natural system subject to gravitational dynamics; no agent property, but the system whose behavior defines the constraint
 *   - Observational Cosmology Community: Institutional observer (institutional/arbitrage) — benefits from understanding the timeline through improved surveys and cosmological inference, but cannot change the timeline itself
 *   - Early-Career Researchers: Individual participants (moderate/constrained) — constrained by the immutability of the timeline within their field, but with limited power to alter fundamental physics
 *   - Funding Agencies: Institutional beneficiaries (institutional/arbitrage) — benefit from coordinating research efforts around the known timeline, but experience no extraction because the timeline is shared knowledge
 *   - Dark Matter Physics Community: Specialized observer (powerful/mobile) — can choose to study different dark matter models, but the underlying structure formation timeline within each model is fixed
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as a fundamental physical law emerging from inflation + gravity + dark energy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dark_matter_structure_formation_timeline, 0.12).
domain_priors:suppression_score(dark_matter_structure_formation_timeline, 0.03).
domain_priors:theater_ratio(dark_matter_structure_formation_timeline, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dark_matter_structure_formation_timeline, extractiveness, 0.12).
narrative_ontology:constraint_metric(dark_matter_structure_formation_timeline, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(dark_matter_structure_formation_timeline, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dark_matter_structure_formation_timeline, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(dark_matter_structure_formation_timeline, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dark_matter_structure_formation_timeline, mountain).
narrative_ontology:human_readable(dark_matter_structure_formation_timeline, "Dark Matter Structure Formation Timeline").
narrative_ontology:topic_domain(dark_matter_structure_formation_timeline, "cosmology/astrophysics").

domain_priors:emerges_naturally(dark_matter_structure_formation_timeline).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STANDARD MODEL (MOUNTAIN) — From the perspective of fundamental physics and observational cosmology, structure formation timescales are determined by invariant cosmological parameters (Ω_m, H_0, the spectral index n_s) and gravitational dynamics. The timeline is mathematically necessary: given initial perturbation amplitudes from inflation and the equation of state of the cosmic fluid, the collapse time for density fluctuations follows from the Friedmann equations. This is not contingent institutional arrangement but fundamental physical law.
constraint_indexing:constraint_classification(dark_matter_structure_formation_timeline, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: HIGH-RESOURCE SURVEYS (MOUNTAIN) — Large survey collaborations (SDSS, DESI, Euclid, Vera Rubin Observatory) are constrained by fundamental observational limits: redshift-distance relationships, light travel time, and the finite volume of the observable universe. These constraints are not socially constructed or politically negotiable. The timeline for surveying the universe to z ≈ 2-3 (where structure formation was active) is set by physics, not by administrative choice. High-resource teams can accelerate data collection but cannot accelerate light propagation or undo the expansion history of the universe.
constraint_indexing:constraint_classification(dark_matter_structure_formation_timeline, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: EARLY-CAREER RESEARCHER (MOUNTAIN) — For a graduate student or postdoc entering cosmology, the structure formation timeline is an unchangeable fact of nature. They cannot choose to make structure form faster or slower by individual effort. The constraint emerges as a natural limit: the observational evidence (Hubble diagram, CMB, baryon acoustic oscillations, galaxy clustering) confirms a specific formation history. They can study different redshifts or use different probes, but the underlying timeline is fixed. Exit option is 'constrained' rather than 'mobile' because leaving cosmology incurs career cost, but the timeline itself is immutable within the field.
constraint_indexing:constraint_classification(dark_matter_structure_formation_timeline, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: FUNDING AGENCIES (MOUNTAIN) — From the perspective of space agencies and funding bodies, the structure formation timeline is a constraint that their planning must accommodate, not engineer. They cannot fund their way out of the timeline — it is set by physics. Agencies allocate resources to surveys that map the timeline (hence arbitrage: they benefit from using surveys to coordinate science policy), but they cannot alter the timeline itself. The constraint from their view is mountain: immutable, natural law, accessible only to those who invest in observation.
constraint_indexing:constraint_classification(dark_matter_structure_formation_timeline, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 5: KNOWLEDGE COORDINATION (ROPE) — At the civilizational level, the structure formation timeline coordinates multiple scientific domains: observational astronomy, general relativity, particle physics (dark matter microphysics), and computational physics (N-body simulations). The timeline is a genuine coordinating fact — it enables research agendas across disciplines to synchronize. Inflation theory predicts primordial perturbations; structure formation theory predicts their growth; observations confirm the timeline. This is pure coordination with minimal extraction: the timeline is a shared truth that benefits all participants. No agent extracts disproportionate value from the timeline itself — the coordination benefit is symmetric.
constraint_indexing:constraint_classification(dark_matter_structure_formation_timeline, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dark_matter_structure_formation_timeline_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(dark_matter_structure_formation_timeline, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dark_matter_structure_formation_timeline, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dark_matter_structure_formation_timeline, ExtMetricName, E),
    domain_priors:suppression_score(dark_matter_structure_formation_timeline, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dark_matter_structure_formation_timeline),
    narrative_ontology:constraint_metric(dark_matter_structure_formation_timeline, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dark_matter_structure_formation_timeline, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dark_matter_structure_formation_timeline_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The dark matter structure formation timeline is not an extraction mechanism — no agent extracts surplus from others through this constraint. The baseline extractiveness reflects only unavoidable informational costs: the timeline is complex, requires specialized knowledge to verify, and access to high-quality observational data is unevenly distributed. But these costs are coordination costs (paying for knowledge), not extraction (one party benefiting at another's expense). Suppression (0.03): Minimal. There are no suppressions of alternatives. The timeline is deterministic given initial conditions and cosmological parameters; there is no silencing of competing timelines because physics does not offer alternatives at this level. The small nonzero value reflects only the technical barrier to understanding — one must learn general relativity and cosmology to fully appreciate why the timeline is inevitable. Theater ratio (0.15): Very low. Cosmological observations (redshift surveys, CMB measurements, strong lensing, baryon acoustic oscillations) provide direct empirical tests of structure formation predictions. The verification process is largely transparent: predictions can be computed from first principles and compared to observations with quantifiable error budgets. The small theater component reflects unavoidable measurement uncertainty and instrumental limitations, not performative activity. The measurement progression (theater_ratio 0.08 → 0.15) reflects that early cosmology (1980s) relied more on indirect inference (e.g., cluster abundance statistics), while modern surveys provide more direct constraints on structure growth and formation redshift. The stabilization at 0.15 reflects that even with future observations, some degree of uncertainty in cosmological parameter measurement is fundamental (cosmic variance limits).
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap in this constraint — all five perspectives classify as mountain or rope. The analytical standard model view and the high-resource survey view both see mountain: immutable physics. The early-career researcher sees mountain despite individual constrained status: the immutability is not a product of their powerlessness, but an objective property of the timeline. The funding agency sees mountain-grade immutability but experiences rope-level coordination benefit: the timeline enables synchronized research. This near-uniformity is the diagnostic signature of a genuine natural law constraint. The small gap between mountain (four perspectives) and rope (one perspective) reflects a genuine structural difference: the pure coordination perspective (cross-disciplinary knowledge synchronization) has minimal extraction but genuine coordination benefit, whereas the mountain perspectives emphasize immutability over coordination. This is not a contradiction — a fact can be both immutable AND coordinating. The structure formation timeline is both: it is a law of nature AND a shared truth that enables disparate research communities to synchronize their efforts.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation is degenerate for this mountain constraint — there are no beneficiaries or victims declared, because the constraint does not extract from anyone or provide targeted benefits. The constraint is universal: all agents experience it equally. The analytical observer's canonical d = 0.73 applies to all contexts, but this is only a formal placeholder — the actual d computation would derive as ~0.50 (symmetric: equal costs and benefits across all parties, symmetric access to the coordination value). However, for a genuine natural law, directionality computation is conceptually moot. No agent extracts from another through this constraint; no agent's exit capacity is impaired. The f(d) sigmoid is applied only to establish that χ ≈ 0.12 × 1.0 × 1.0 = 0.12 (minimal effective extraction even with canonical d), which is the intended outcome for a mountain. The immutability holds regardless of how d is computed.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not present a mandatrophy. A mountain constraint classified uniformly across all perspectives (or with only rope perspectives showing minor coordination benefits) has resolved the classification question completely: the constraint is immutable physics with universal applicability. There is no tension between coordination and extraction, no hidden beneficiary, and no misclassification risk. The constraint is stable in type: mountain. The minimal omegas document genuine scientific uncertainties (dark matter microphysics, initial condition specification, dark energy interpretation, observational completeness) rather than mandatrophies. These uncertainties could in principle modify the classification if resolved in certain directions (e.g., if dark energy is engineered/contingent, the timeline might shift toward rope), but as currently understood, the mountain classification is secure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dark_matter_microphysics_determination,
    'Does the structure formation timeline depend crucially on the nature of dark matter (cold vs warm vs fuzzy dark matter), or is the timeline robust across viable dark matter models?',
    'Comparison of N-body simulations across different dark matter models; high-resolution observations of small-scale structure (Lyman-alpha forest, dwarf galaxies, satellite systems) to discriminate between models; detection or exclusion of dark matter particles',
    'If timeline is robust: mountain classification is secure — fundamental physics constraint independent of particle physics details. If timeline varies sharply with dark matter model: mountain weakens; timeline becomes partially dependent on contingent microphysical assumptions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dark_matter_microphysics_determination, empirical, 'Robustness of formation timeline across dark matter models').

omega_variable(
    initial_condition_specification,
    'Are the initial conditions for structure formation (primordial perturbation spectrum, phase properties) truly set by inflation alone, or is there irreducible observational under-determination in specifying them?',
    'Future CMB observations (CMB-S4, next-generation probes) to tighten constraints on primordial power spectrum; comparison of CMB predictions with large-scale structure observations across multiple redshifts; search for non-Gaussianities or primordial gravitational waves that would validate or constrain inflation models',
    'If inflation fully specifies: mountain gate holds — initial conditions are determined by prior physical law. If under-determination exists: timeline classification may shift toward rope (coordinate-dependent initial conditions) or require expanded omegas.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(initial_condition_specification, empirical, 'Degree of observational specification of initial conditions').

omega_variable(
    cosmological_constant_interpretation,
    'Is the dark energy density (cosmological constant or dynamical scalar field) a fundamental property of spacetime geometry, or an effective description of something more fundamental that could in principle be engineered or modified?',
    'Detection of dark energy evolution or time-dependence; detection of primordial gravitational waves at frequencies sensitive to early-universe physics; future tests of equivalence principle and gravitational theories at cosmological scales',
    'If fundamental: mountain classification holds — dark energy is an immutable property of the universe. If effective/engineered: timeline could be contingent; structure formation would be subject to controllable parameters (moves toward rope or tangled_rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cosmological_constant_interpretation, conceptual, 'Fundamental vs effective status of dark energy').

omega_variable(
    observational_completeness_bias,
    'Do current observational signatures of structure formation reflect the actual physical timeline, or are we measuring only the redshifts/timescales at which we can observe (observational selection bias)?',
    'Comparison of observed galaxy populations with predictions from hydrodynamical simulations including realistic selection effects; studies of galaxies at extreme redshift (z > 10-20) and their contribution to structure growth; forward-modeling of selection biases in current surveys',
    'If completeness is high: mountain classification is robust — we observe the true timeline. If significant bias: the timeline we measure is observer-dependent; different detection methods could yield different apparent formation histories (suggests hidden rope or scaffold structure).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(observational_completeness_bias, empirical, 'Whether observed structure formation timeline reflects true physics or observational selection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dark_matter_structure_formation_timeline, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dmtl_tr_t0, dark_matter_structure_formation_timeline, theater_ratio, 0, 0.08).
narrative_ontology:measurement(dmtl_tr_t50, dark_matter_structure_formation_timeline, theater_ratio, 50, 0.15).
narrative_ontology:measurement(dmtl_tr_t100, dark_matter_structure_formation_timeline, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(dmtl_be_t0, dark_matter_structure_formation_timeline, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(dmtl_be_t50, dark_matter_structure_formation_timeline, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(dmtl_be_t100, dark_matter_structure_formation_timeline, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dark_matter_structure_formation_timeline, information_standard).
narrative_ontology:affects_constraint(dark_matter_structure_formation_timeline, cmb_acoustic_scale_constraint).
narrative_ontology:affects_constraint(dark_matter_structure_formation_timeline, galaxy_clustering_growth_rate).
narrative_ontology:affects_constraint(dark_matter_structure_formation_timeline, weak_lensing_matter_power_spectrum).

% DUAL FORMULATION NOTE:
% The dark matter structure formation timeline is a foundational constraint that grounds the interpretation of multiple observational signatures. CMB acoustic scale, galaxy clustering growth, and weak lensing power spectrum are observational windows into the same underlying formation process. They are linked constraints: knowledge of the timeline constrains interpretation of each observable, and vice versa. However, they are distinct constraints (each has different ε values reflecting different observational uncertainties and theoretical complexities) rather than aspects of a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
