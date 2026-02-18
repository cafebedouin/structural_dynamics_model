% ============================================================================
% CONSTRAINT STORY: star_formation_barrier_g0253
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_star_formation_barrier_g0253, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: star_formation_barrier_g0253
 *   human_readable: "Star Formation Barrier in the 'Brick' Cloud (G0.253+0.016)"
 *   domain: Physics/Astrophysics
 *
 * SUMMARY:
 *   The dense molecular cloud G0.253+0.016, known as "the Brick," has
 *   sufficient mass and density to be a prolific stellar nursery. However,
 *   observations show it is curiously "star-poor." This implies the
 *   existence of a powerful physical constraint—such as extreme turbulence or
 *   strong magnetic fields—that actively prevents gravitational collapse and
 *   inhibits star formation. This constraint represents a fundamental,
 *   unalterable feature of this region's physics.
 *
 * KEY AGENTS (by structural relationship):
 *   - interstellar_gas_and_dust: Conceptual target (powerless/trapped) — The matter being prevented from undergoing gravitational collapse.
 *   - astronomers: Analytical observer (analytical/analytical) — Studies the phenomenon, seeking to understand the physical laws governing it.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(star_formation_barrier_g0253, 0.05). % ε represents the scientific cost of observing/theorizing the barrier, not an extraction from agents.
domain_priors:suppression_score(star_formation_barrier_g0253, 0.02).   % Structural property (raw, unscaled). Suppression of human alternatives is nil; we cannot change this.
domain_priors:theater_ratio(star_formation_barrier_g0253, 0.0).       % Piton detection (>= 0.70). This is a physical phenomenon, not a performance.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(star_formation_barrier_g0253, extractiveness, 0.05).
narrative_ontology:constraint_metric(star_formation_barrier_g0253, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(star_formation_barrier_g0253, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. These values ensure the constraint is
% certified as a natural law.
narrative_ontology:constraint_metric(star_formation_barrier_g0253, accessibility_collapse, 0.98). % The alternative (star formation) is almost completely foreclosed by the physical conditions.
narrative_ontology:constraint_metric(star_formation_barrier_g0253, resistance, 0.0). % There is zero meaningful resistance to this physical barrier.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(star_formation_barrier_g0253, mountain).
narrative_ontology:human_readable(star_formation_barrier_g0253, "Star Formation Barrier in the 'Brick' Cloud (G0.253+0.016)").
narrative_ontology:topic_domain(star_formation_barrier_g0253, "Physics/Astrophysics").

% --- Emergence flag (required for mountain constraints) ---
% This constraint is a feature of the natural universe, not a human construct.
% This flag is mandatory for the mountain metric gate to fire.
domain_priors:emerges_naturally(star_formation_barrier_g0253).

% --- Structural relationships ---
% Not applicable. As a Mountain constraint representing a physical law,
% there are no coherent beneficiaries or victims in the socio-economic sense.
% The directionality derivation chain is not needed.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   This is a uniform-type constraint (Mountain-only). The classification
   is invariant across all perspectives, as it reflects a physical reality.
   We include multiple perspectives to demonstrate this invariance.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE MATTER ITSELF (CONCEPTUAL TARGET)
% The gas and dust within the cloud are structurally powerless and trapped by
% the physical conditions. From this perspective, the barrier to collapse is
% an absolute, unchangeable environmental constant.
constraint_indexing:constraint_classification(star_formation_barrier_g0253, mountain,
    context(agent_power(powerless),
            time_horizon(biographical), % Timescale of a parcel of gas
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE RESEARCH COMMUNITY
% Astronomers studying the phenomenon see it as a fundamental law or condition
% of this specific environment. Their ability to alter it is zero.
constraint_indexing:constraint_classification(star_formation_barrier_g0253, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained), % Constrained by available instruments/funding
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The default analytical context confirms the classification. The underlying
% physical laws are universal, even if this manifestation is regional.
constraint_indexing:constraint_classification(star_formation_barrier_g0253, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(star_formation_barrier_g0253_tests).

test(classification_invariance, [nondet]) :-
    % For a uniform-type constraint, verify that the classification is the same
    % from multiple key perspectives.
    constraint_indexing:constraint_classification(star_formation_barrier_g0253, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(star_formation_barrier_g0253, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypeTarget == mountain,
    TypeAnalytical == mountain.

test(mountain_threshold_validation) :-
    % Verify that the base metrics conform to Mountain classification thresholds.
    domain_priors:base_extractiveness(star_formation_barrier_g0253, E),
    domain_priors:suppression_score(star_formation_barrier_g0253, S),
    E =< 0.25,
    S =< 0.05.

test(natural_law_profile_validation) :-
    % Verify the constraint has the necessary metrics for NL certification.
    narrative_ontology:constraint_metric(star_formation_barrier_g0253, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(star_formation_barrier_g0253, resistance, R),
    domain_priors:emerges_naturally(star_formation_barrier_g0253),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(star_formation_barrier_g0253_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint is classified as a Mountain because it represents a
 *   physical phenomenon that is, from a human standpoint, unchangeable and
 *   fundamental. The base extractiveness (ε=0.05) is low, representing the
 *   scientific effort required to observe and understand the barrier, rather
 *   than a socio-economic extraction. The suppression score (S=0.02) is also
 *   very low, as there are no viable 'alternatives' to the laws of physics
 *   in this region that could be chosen. The required Natural Law profile
 *   metrics (`emerges_naturally`, `accessibility_collapse` >= 0.85,
 *   `resistance` <= 0.15) are all met, ensuring its certification as a
 *   physical limit.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. The constraint's classification as a Mountain
 *   is invariant across all observers, from the conceptual 'perspective' of
 *   the gas itself to the analytical view of the scientific community. This
 *   invariance is the hallmark of a true Mountain constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain constraint, beneficiary and victim declarations are not
 *   applicable. The concept of directionality, which measures the asymmetry
 *   of costs and benefits in human systems, does not apply to a fundamental
 *   physical law. The system correctly computes a neutral or irrelevant
 *   directionality for all agents.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies a natural, physical limit,
 *   preventing it from being misinterpreted as an artificial or negotiable
 *   Snare. The ε-invariance principle is critical here: if future observations
 *   suggest that 'magnetic fields' (ε=0.05) and 'turbulence' (ε=0.06) are
 *   distinct, competing constraints rather than aspects of one barrier, they
 *   would need to be decomposed into separate, linked constraint stories.
 *   For now, they are treated as facets of a single physical barrier.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_star_formation_barrier_g0253,
    'What is the primary physical mechanism causing the star formation barrier in G0.253+0.016: strong magnetic fields, high turbulence, or the cloud''s dynamical youth?',
    'Higher-resolution mapping of magnetic field geometry and gas velocity dispersions using instruments like the James Webb Space Telescope (JWST) or the Atacama Large Millimeter/submillimeter Array (ALMA).',
    'Resolution would refine models of star formation in extreme galactic environments. It would not change the constraint type (Mountain), but would change the physical description of the constraint''s cause.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(star_formation_barrier_g0253, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not strictly required as base_extractiveness (0.05) is below the 0.46
% threshold. However, we include these facts to model the stability of this
% physical constraint over observational time. The values are flat,
% indicating no drift.
narrative_ontology:measurement(star_formation_barrier_g0253_tr_t0, star_formation_barrier_g0253, theater_ratio, 0, 0.0).
narrative_ontology:measurement(star_formation_barrier_g0253_tr_t5, star_formation_barrier_g0253, theater_ratio, 5, 0.0).
narrative_ontology:measurement(star_formation_barrier_g0253_tr_t10, star_formation_barrier_g0253, theater_ratio, 10, 0.0).

narrative_ontology:measurement(star_formation_barrier_g0253_ex_t0, star_formation_barrier_g0253, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(star_formation_barrier_g0253_ex_t5, star_formation_barrier_g0253, base_extractiveness, 5, 0.05).
narrative_ontology:measurement(star_formation_barrier_g0253_ex_t10, star_formation_barrier_g0253, base_extractiveness, 10, 0.05).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% This constraint is a physical limit, not a coordination mechanism.
% No coordination_type or Boltzmann floor override is applicable.

% Network relationships: This constraint provides crucial data for, and thus
% structurally affects, larger-scale models of star formation across the galaxy.
narrative_ontology:affects_constraint(star_formation_barrier_g0253, galactic_star_formation_rate_models).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary. The constraint is a Mountain, and the concept
% of directed extraction is not applicable.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */