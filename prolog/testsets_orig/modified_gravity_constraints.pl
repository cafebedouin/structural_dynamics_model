% ============================================================================
% CONSTRAINT STORY: modified_gravity_constraints
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_modified_gravity_constraints, []).

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
 *   constraint_id: modified_gravity_constraints
 *   human_readable: Modified Gravity Constraints in Cosmological Observation
 *   domain: cosmology/fundamental_physics
 *
 * SUMMARY:
 *   Modified gravity theories propose that gravitational dynamics at
 *   cosmological scales differ from general relativity (GR), potentially
 *   explaining observations attributed to dark matter through modified
 *   gravitational interactions instead. This creates a structural tension
 *   between two explanatory frameworks: particle dark matter (dominant in
 *   funding and institutional authority) and modified gravity (theoretically
 *   developed but observationally suppressed). The constraint operates
 *   through asymmetric verification standards — modified gravity theories are
 *   required to reproduce dark matter predictions exactly, even though they
 *   are mathematically distinct frameworks with different physical
 *   motivations. This creates extraction: modified gravity researchers must
 *   continuously prove their theories match dark matter's observational
 *   success while funding and publication channels favor dark matter
 *   interpretations. The constraint exhibits all six DR types, revealing how
 *   scientific paradigm dominance can operate as a constraint mechanism.
 *   Theater ratio (0.65) reflects that dark matter detection experiments and
 *   modified gravity tests are substantially performative: null results from
 *   dark matter searches do not falsify the hypothesis (the search space
 *   remains vast), and modified gravity theories can be extended to match any
 *   new observation. The constraint's trajectory shows increasing theater and
 *   slowly rising extractiveness as the paradigm becomes more entrenched
 *   despite accumulating anomalies.
 *
 * KEY AGENTS:
 *   - Modified Gravity Theorists: Primary victim (powerless/trapped) — structurally barred from equal institutional access; must prove equivalence to dark matter framework to be heard
 *   - Observational Astronomers: Secondary agent (moderate/constrained) — coordinate on observation standards; benefit from resources but constrained by dark matter paradigm dominance in allocation
 *   - Dark Matter Particle Physics Community: Primary beneficiary (institutional/arbitrage) — funding flows, publication priority, observational attention directed toward dark matter detection and verification
 *   - Alternative Theories Coalition: Organized agents (organized/constrained) — LSST, gravitational wave astronomy, precision cosmology providing alternative pathways; constrained by current paradigm but have agency
 *   - Direct Detection Experiments: Institutional actor (institutional/arbitrage) — maintains infrastructure despite consistent null results; persists through sunk-cost and institutional inertia (piton perspective)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement as inherent feature of observational science
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(modified_gravity_constraints, 0.38).
domain_priors:suppression_score(modified_gravity_constraints, 0.42).
domain_priors:theater_ratio(modified_gravity_constraints, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(modified_gravity_constraints, extractiveness, 0.38).
narrative_ontology:constraint_metric(modified_gravity_constraints, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(modified_gravity_constraints, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(modified_gravity_constraints, tangled_rope).
narrative_ontology:human_readable(modified_gravity_constraints, "Modified Gravity Constraints in Cosmological Observation").
narrative_ontology:topic_domain(modified_gravity_constraints, "cosmology/fundamental_physics").

domain_priors:requires_active_enforcement(modified_gravity_constraints).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(modified_gravity_constraints, dark_matter_particle_physics_community).
narrative_ontology:constraint_beneficiary(modified_gravity_constraints, observational_cosmology_funding_agencies).
narrative_ontology:constraint_victim(modified_gravity_constraints, modified_gravity_theoretical_programs).
narrative_ontology:constraint_victim(modified_gravity_constraints, observational_astronomy_resource_allocation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MODIFIED GRAVITY THEORISTS (SNARE) — Structurally trapped by observational interpretation standards that require modified gravity theories to replicate dark matter predictions even when mathematically distinct. Cannot exit the constraint without abandoning their research program. Bears full extraction cost through resource scarcity and publication barriers.
constraint_indexing:constraint_classification(modified_gravity_constraints, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OBSERVATIONAL ASTRONOMERS (TANGLED ROPE) — Constrained by instrumentation costs and data availability, but also benefit from the constraint through collaborative access to major telescopes and funding for cosmological surveys. Coordinate on observation standards while experiencing asymmetric extraction through resource concentration.
constraint_indexing:constraint_classification(modified_gravity_constraints, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PARTICLE PHYSICS & DARK MATTER PARADIGM (ROPE) — Primary beneficiary. Experiences the constraint as coordination: the dominance of dark matter interpretation in cosmology coordinates funding flows, publication priority, and observational attention toward dark matter detection experiments. Net beneficiary through institutional advantage.
constraint_indexing:constraint_classification(modified_gravity_constraints, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALTERNATIVE GRAVITY RESEARCH COALITION (SCAFFOLD) — Organized agents (LSST surveys, gravitational wave astronomy, precision cosmology initiatives) see modified gravity testing as a temporary coordination problem with plausible sunset. Gravitational wave astronomy and high-precision CMB observations provide independent pathways to test gravity's functional form without dark matter assumptions. Has agency and perceives exit path.
constraint_indexing:constraint_classification(modified_gravity_constraints, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DARK MATTER DETECTION EXPERIMENTS (PITON) — Direct detection searches (LUX, XENON, SuperCDMS) for hypothetical dark matter particles have achieved null results repeatedly while infrastructure and career paths remain locked into the detection paradigm. Theater ratio is high — the experiments function as technology development and testing, but their primary nominal goal (direct particle detection) has failed consistently for decades. Persistence driven by institutional inertia and sunk cost rather than functional success.
constraint_indexing:constraint_classification(modified_gravity_constraints, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW PERSPECTIVE (MOUNTAIN) — From a civilizational/universal perspective, some constraint on gravitational theory is inherent to observational science: any theory of gravity must be tested empirically, and testing always requires assumptions about what constitutes evidence. The constraint appears immutable — we cannot escape the requirement to validate theories through observation. However, this perspective risks naturalizing the specific institutional arrangement (dark matter dominance) as a law of nature rather than a contingent historical consensus.
constraint_indexing:constraint_classification(modified_gravity_constraints, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(modified_gravity_constraints_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(modified_gravity_constraints, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(modified_gravity_constraints, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(modified_gravity_constraints, TR),
    TR >= 0.70.

:- end_tests(modified_gravity_constraints_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Modified gravity research faces resource scarcity and publication bias, but is not utterly impossible — journals publish modified gravity papers, conferences accept talks, some funding agencies support alternative gravity research. The extraction is real but not total. Suppression (0.42): Moderate. Barriers include publication standards requiring comparison to dark matter, funding allocation heavily favoring dark matter experiments, and institutional prestige concentrated in particle physics. But suppression is not total — graduate programs still train modified gravity theorists, papers still publish, conferences still host debates. Theater ratio (0.65): Moderate-high and increasing. Dark matter detection searches have recorded null results for decades while the field persists, suggesting performative elements (technology development, testing infrastructure, career path maintenance) have partially decoupled from primary goal. Modified gravity tests are also partly performative — theories can be adjusted to match new data, limiting falsifiability. The theater ratio increase over the interval (0.45 to 0.65) reflects growing awareness that the paradigm conflict has become institutionalized rather than empirically resolvable in the near term.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how paradigm dominance operates as an asymmetric enforcement mechanism. Modified gravity theorists see extraction (Snare) — they are structurally barred from equal institutional access and must continuously prove equivalence to a competing framework. Particle physics sees coordination (Rope) — the dark matter interpretation organizes funding flows and observational priorities efficiently from their perspective. Observational astronomers see mixed constraint (Tangled Rope) — they coordinate on survey standards while experiencing resource asymmetry. The alternative theories coalition sees a temporary problem with a sunset (Scaffold) — gravitational wave astronomy and precision cosmology will eventually provide independent tests. Dark matter detection experiments see their own degradation (Piton) — consistent null results suggest the direct detection goal is replaced by technology development, but career and institutional inertia keep the experiments running. The analytical observer risks seeing an immutable constraint of science (Mountain) — 'we must test theories empirically' — but the actual constraint is institutional: paradigm dominance, not empirical necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   The particle dark matter community benefits from institutional architecture: funding agencies prioritize dark matter experiments, journals develop explicit comparison standards that dark matter predicts, and career advancement strongly favors dark matter interpretations. This creates low directionality for beneficiaries (d ≈ 0.15-0.20). Modified gravity theorists are constrained by the same architecture — they must match dark matter predictions to gain credibility despite theoretical distinctness. This creates high directionality for victims (d ≈ 0.75-0.85). Observational astronomers occupy an intermediate position: they benefit from survey funding and instrumentation (lowering d) but are constrained by paradigm dominance in observation planning (raising d). The beneficiary/victim asymmetry is clear: one direction benefits from institutional authority; the other pays the cost of suppressed alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing that the classification depends critically on the observer's position within the paradigm. The dark matter particle physics community genuinely experiences the constraint as coordination (Rope) — they are solving the legitimate problem of understanding cosmic structure. Modified gravity theorists experience it as extraction (Snare) — they are locked out of equal institutional access. Both are correct readings of the same structural data. The resolution is not 'which type is really correct?' but 'the constraint is enforced asymmetrically against researchers holding non-dominant positions.' The analytical observer's mountain perspective risks naturalizing this institutional arrangement as a law of nature ('we must validate all theories against dark matter because that's how science works'), but the structural data reveals it as contingent: future observational windows (gravitational waves, precision CMB) may enable independent tests that lift the constraint. The tangled rope classification at the base reflects that genuine coordination (comparing theories to observations) is paired with genuine extraction (suppressing alternatives through resource control).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dark_matter_observational_ambiguity,
    'Are observational anomalies (flat rotation curves, gravitational lensing patterns, CMB power spectrum) evidence for dark matter particles, or do they indicate modified gravity?',
    'High-precision tests that distinguish between dark matter and modified gravity predictions: gravitational wave speed precision, strong-field tests via black hole shadows, ultra-precise galactic rotation curve mapping with controlled sample selection',
    'If dark matter correct: modified gravity constraint is falsifiable and constraints lift. If modified gravity correct: current observation standards unjustly suppress alternative theories. If both are needed: constraint becomes legitimate coordination problem (Rope) rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dark_matter_observational_ambiguity, empirical, 'Whether observations favor dark matter or modified gravity explanations').

omega_variable(
    theory_equivalence_boundary,
    'Can modified gravity theories reproduce dark matter predictions exactly, or are they structurally distinct frameworks that happen to overlap observationally?',
    'Mathematical analysis of parameter space: can modified gravity field equations be continuously deformed to match dark matter predictions while preserving theoretical motivation? Analysis of anomaly sources — do they derive from the same physical principles or different ones?',
    'If structurally equivalent: requiring modified gravity to match dark matter is legitimate coordination on equivalent formalisms. If distinct: the constraint is pure extraction — forcing false equivalence. If partially overlapping: constraint is tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theory_equivalence_boundary, conceptual, 'Structural relationship between modified gravity and dark matter formalisms').

omega_variable(
    funding_allocation_mechanism_opacity,
    'To what extent does the suppression of modified gravity research reflect genuine empirical status vs. institutional funding mechanisms that favor dark matter?',
    'Analysis of grant award rates by topic (dark matter vs modified gravity, controlling for proposal quality metrics); citation patterns in high-impact journals; conference presentation acceptance rates; PhD job placement outcomes by field',
    'If empirically justified: constraint reflects legitimate observational assessment. If mechanism-driven: the suppression is institutional, not evidential, raising extraction severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_allocation_mechanism_opacity, empirical, 'Whether suppression reflects empirical status or institutional mechanisms').

omega_variable(
    gravitational_wave_astronomy_timeline,
    'Will gravitational wave astronomy provide definitive tests of gravity''s functional form on sub-decade timescales, enabling the scaffold perspective''s sunset?',
    'Roadmap analysis of LIGO, Virgo, KAGRA, future detectors (LISA, Einstein Telescope); feasibility of precision tests from neutron star mergers and black hole collisions; timeline for sensitivity thresholds that distinguish modified gravity from dark matter predictions',
    'If definitive tests achievable by 2035: scaffold sunset is realistic. If not until 2050+: sunset timeline extends; constraint persists. If gravitation wave tests cannot distinguish: scaffold sunset fails and constraint becomes permanent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gravitational_wave_astronomy_timeline, empirical, 'Whether gravitational wave astronomy provides timely definitive tests').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(modified_gravity_constraints, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(modi_tr_t0, modified_gravity_constraints, theater_ratio, 0, 0.45).
narrative_ontology:measurement(modi_tr_t10, modified_gravity_constraints, theater_ratio, 10, 0.58).
narrative_ontology:measurement(modi_tr_t20, modified_gravity_constraints, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(modi_be_t0, modified_gravity_constraints, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(modi_be_t10, modified_gravity_constraints, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(modi_be_t20, modified_gravity_constraints, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(modified_gravity_constraints, information_standard).
narrative_ontology:affects_constraint(modified_gravity_constraints, dark_matter_candidate_particles).
narrative_ontology:affects_constraint(modified_gravity_constraints, cosmological_model_selection).

% DUAL FORMULATION NOTE:
% Modified gravity constraints are downstream of specific observational claims (rotation curves, lensing patterns, CMB anomalies) but represent a distinct structural constraint on how alternative theories are evaluated. The constraint is not about the empirical data itself but about institutional standards for comparing theories to data.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
