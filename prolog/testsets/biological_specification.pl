% ============================================================================
% CONSTRAINT STORY: biological_specification
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Generated: 2026-02-19
% ============================================================================

:- module(constraint_biological_specification, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:interval/3,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * constraint_id: biological_specification
 * human_readable: Biological Specification (Real vs. Toy)
 * domain: ontological
 *
 * SUMMARY:
 * The irreducible physical gap between a manufactured object (plush, sawdust, 
 * clockwork) and a biological organism. It represents the "natural law" of 
 * the nursery before the intervention of transformation magic.
 *
 * KEY AGENTS:
 * - Velveteen Rabbit: Primary target (powerless/trapped) — constrained by his 
 * physical form (no hind legs, made in one piece).
 * - Wild Rabbits: Exemplars of the Mountain (moderate/mobile) — their existence 
 * defines the parameters of the constraint.
 * - Analytical Observer: The "Skin Horse" or narrator who recognizes the 
 * immutability of physical laws.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biological_specification, 0.15).
domain_priors:suppression_score(biological_specification, 0.00).
domain_priors:theater_ratio(biological_specification, 0.00).

% --- Constraint metric facts ---
narrative_ontology:constraint_metric(biological_specification, extractiveness, 0.15).
narrative_ontology:constraint_metric(biological_specification, suppression_requirement, 0.00).
narrative_ontology:constraint_metric(biological_specification, theater_ratio, 0.00).

% --- NL Profile Metrics (Required for Mountain) ---
% Accessibility Collapse: Alternatives are structurally inaccessible (Toys cannot 
% simply choose to be biological).
narrative_ontology:constraint_metric(biological_specification, accessibility_collapse, 0.95).
% Resistance: Zero meaningful resistance; physical laws of biology are indifferent.
narrative_ontology:constraint_metric(biological_specification, resistance, 0.00).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biological_specification, mountain).

% --- Emergence flag ---
domain_priors:emerges_naturally(biological_specification).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE TOY (UNIFORM-TYPE EXCEPTION)
% From the Rabbit's perspective, his lack of legs is an unchangeable law.
constraint_indexing:constraint_classification(biological_specification, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE WILD RABBITS
% Biology is simply the "way things are" — an invariant background.
constraint_indexing:constraint_classification(biological_specification, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
constraint_indexing:constraint_classification(biological_specification, mountain,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biological_specification_tests).

test(mountain_invariance) :-
    % Verify that classification remains Mountain across different powers
    constraint_indexing:constraint_classification(biological_specification, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(biological_specification, mountain, context(agent_power(analytical), _, _, _)).

test(nl_certification) :-
    % Verify NL metrics pass thresholds (Config defaults: AC >= 0.85, Res <= 0.15)
    narrative_ontology:constraint_metric(biological_specification, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(biological_specification, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(biological_specification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * As a Mountain, epsilon is low (0.15) and suppression is zero. This is a 
 * physical constraint of the world, not a social or institutional one.
 *
 * DIRECTIONALITY LOGIC:
 * No beneficiary or victim is declared because natural laws do not extract 
 * rent; they simply define the possibility space.
 *
 * MANDATROPHY ANALYSIS:
 * Classification as a Mountain prevents the Rabbit's physical limitations 
 * from being misread as a Snare of "poor craftsmanship." It is a 
 * biological baseline, not a malicious design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    omega_biological_specification,
    'Can Nursery Magic be classified as a re-specification of biological Mountain?',
    'Empirical observation of the Fairy transition.',
    'If true, Mountains are locally mutable via higher-order Tangled Ropes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(omega_biological_specification, conceptual, 'Mutability of biological laws via magic.').

/* ==========================================================================
   7. INTEGRATION HOOKS & NETWORK
   ========================================================================== */

narrative_ontology:interval(biological_specification, 0, 10).

% DUAL FORMULATION NOTE:
% This is the upstream Mountain that biological_specification defines.
% It affects the downstream Tangled Rope of becoming 'Real'.
narrative_ontology:affects_constraint(biological_specification, devotional_transformation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
