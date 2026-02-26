% ============================================================================
% CONSTRAINT STORY: bh_merger_gravitational_infall
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bh_merger_gravitational_infall, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bh_merger_gravitational_infall
 *   human_readable: Gravitational Infall of Supermassive Black Holes
 *   domain: physical/astrophysics
 *
 * SUMMARY:
 *   The gravitational infall of supermassive black holes, as observed in
 *   systems like the triple-black-hole candidate in galaxy UGC 11551, is a
 *   physical process governed by the laws of General Relativity. This
 *   constraint represents the immutable nature of gravity on cosmic scales,
 *   dictating the orbital decay and eventual merger of massive objects. It is
 *   not a social, political, or economic system, but a fundamental feature of
 *   spacetime.
 *
 * KEY AGENTS:
 *   - Infalling Black Holes: Primary subjects (powerless/trapped) — their dynamics are completely determined by the gravitational field.
 *   - Surrounding Gas and Stars: Secondary subjects (moderate/constrained) — their orbits and fates are dictated by the central masses.
 *   - Astrophysicists: Observers (analytical/analytical) — they model the system but cannot influence it.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bh_merger_gravitational_infall, 0.02).
domain_priors:suppression_score(bh_merger_gravitational_infall, 0.01).
domain_priors:theater_ratio(bh_merger_gravitational_infall, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bh_merger_gravitational_infall, extractiveness, 0.02).
narrative_ontology:constraint_metric(bh_merger_gravitational_infall, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(bh_merger_gravitational_infall, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bh_merger_gravitational_infall, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(bh_merger_gravitational_infall, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bh_merger_gravitational_infall, mountain).
narrative_ontology:human_readable(bh_merger_gravitational_infall, "Gravitational Infall of Supermassive Black Holes").
narrative_ontology:topic_domain(bh_merger_gravitational_infall, "physical/astrophysics").

domain_priors:emerges_naturally(bh_merger_gravitational_infall).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE INFALLING MASSES (MOUNTAIN) — The black holes and surrounding matter are subject to the laws of gravity. They have no agency or ability to exit the system. The constraint is an unchangeable feature of their environment.
constraint_indexing:constraint_classification(bh_merger_gravitational_infall, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: A NEARBY STAR SYSTEM (MOUNTAIN) — A star system within the galaxy is governed by the same gravitational laws. While it might be ejected through a three-body interaction, it cannot alter the fundamental constraint of gravity itself. Its path is determined by an immutable law.
constraint_indexing:constraint_classification(bh_merger_gravitational_infall, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN) — Human astrophysicists observe and model this process. They understand it as a manifestation of General Relativity, a universal physical law. From this perspective, the constraint is a fundamental, unchangeable aspect of the universe.
constraint_indexing:constraint_classification(bh_merger_gravitational_infall, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bh_merger_gravitational_infall_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(bh_merger_gravitational_infall, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bh_merger_gravitational_infall, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(bh_merger_gravitational_infall, ExtMetricName, E),
    domain_priors:suppression_score(bh_merger_gravitational_infall, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(bh_merger_gravitational_infall),
    narrative_ontology:constraint_metric(bh_merger_gravitational_infall, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(bh_merger_gravitational_infall, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(bh_merger_gravitational_infall_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint is classified as a Mountain because it represents a fundamental law of nature. Extractiveness (ε=0.02) and suppression (0.01) are minimal; gravity does not 'extract' value for a beneficiary, nor does it 'suppress' alternatives. It is a structural property of the universe. The NL Profile metrics confirm this: it emerges naturally (true), has high accessibility collapse (0.95) as understanding General Relativity requires deep expertise, and has extremely low resistance (0.05) as it is physically impossible to oppose on this scale.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All agents, regardless of their structural position—from the black holes themselves to the distant human observers—classify the constraint as a Mountain. This invariance is the defining characteristic of a true natural law within the Deferential Realism framework.
 *
 * DIRECTIONALITY LOGIC:
 *   The concepts of beneficiary and victim are not applicable to this constraint. Gravitational infall is an impersonal physical process. As there are no declared beneficiaries or victims, the directionality 'd' is not derived from structural relationships but falls back to canonical values, which are irrelevant as the near-zero base extractiveness (ε) ensures that effective extraction (χ) is negligible from all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   This story serves as a baseline case, correctly identifying a physical law as a Mountain. It avoids the mandatrophy of misattributing agency or extractive purpose to a natural process. Any attempt to classify this as a Snare (e.g., 'gravity traps everything') would be a category error, projecting social concepts onto physics. The system correctly identifies its structural signature as that of an unchangeable, non-extractive, universal constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bh_merger_gravitational_infall, 0, 1000000000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(bh_merger_gravitational_infall, general_relativity_field_equations).

% DUAL FORMULATION NOTE:
% This constraint is a specific manifestation of the more general constraint 'general_relativity_field_equations'. While GR is the universal law (the parent Mountain), specific instances like this merger provide empirical validation and context for it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
