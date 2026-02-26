% ============================================================================
% CONSTRAINT STORY: legacy_identity_lock_in
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-20
% ============================================================================

:- module(constraint_legacy_identity_lock_in, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
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
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * constraint_id: legacy_identity_lock_in
 * human_readable: The Inflexible Substrate of Upbringing and Boons
 * domain: Psychological/Metaphysical/Socio-Political
 *
 * SUMMARY:
 * Chitra's identity is forged by a divine boon to her royal grandsire and her 
 * father's subsequent upbringing of her as a male heir. 
 * This structural lock-in creates a "Mountain" of habit and nature—her hands 
 * are "strong to bend the bow" rather than skilled in "Cupid's archery".
 *
 * KEY AGENTS (by structural relationship):
 * - Chitra: Primary subject (powerless/trapped) — bears the ontological weight
 * - King Chitravahana: Primary beneficiary (institutional/arbitrage) — designer
 * - Shiva/Madana/Vasanta: Metaphysical architects — enforces the boon
 * - Analytical Observer: Sees the "Mountain" of past penance and nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legacy_identity_lock_in, 0.12).
domain_priors:suppression_score(legacy_identity_lock_in, 0.04).
domain_priors:theater_ratio(legacy_identity_lock_in, 0.05).

% --- Constraint metric facts ---
narrative_ontology:constraint_metric(legacy_identity_lock_in, extractiveness, 0.12).
narrative_ontology:constraint_metric(legacy_identity_lock_in, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(legacy_identity_lock_in, theater_ratio, 0.05).

% --- NL Profile Metrics (Required for Mountains) ---
narrative_ontology:constraint_metric(legacy_identity_lock_in, accessibility_collapse, 0.96).
narrative_ontology:constraint_metric(legacy_identity_lock_in, resistance, 0.02).

% --- Binary flags ---
domain_priors:emerges_naturally(legacy_identity_lock_in).
narrative_ontology:constraint_claim(legacy_identity_lock_in, mountain).

% --- Structural relationships ---
% Identity lock-in primarily benefits the lineage/state stability.
narrative_ontology:constraint_beneficiary(legacy_identity_lock_in, kingly_house_manipur).
% Chitra bears the cost of the "primal injustice" of her perceived plainness/masculinity.
narrative_ontology:constraint_victim(legacy_identity_lock_in, chitra_individual_ego).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY SUBJECT (MOUNTAIN)
% Chitra is trapped by her upbringing and the god's boon.
constraint_indexing:constraint_classification(legacy_identity_lock_in, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE INSTITUTIONAL ARCHITECT (MOUNTAIN)
% The King views this as a settled coordination of heritage and succession.
constraint_indexing:constraint_classification(legacy_identity_lock_in, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Identifies the invariant structural reality of the identity substrate.
constraint_indexing:constraint_classification(legacy_identity_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legacy_identity_lock_in_tests).

test(mountain_invariance) :-
    % For a true Mountain/Natural Law, type should be invariant.
    constraint_indexing:constraint_classification(legacy_identity_lock_in, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legacy_identity_lock_in, mountain, context(agent_power(institutional), _, _, _)).

test(nl_signature_compliance) :-
    narrative_ontology:constraint_metric(legacy_identity_lock_in, accessibility_collapse, AC), AC >= 0.85,
    narrative_ontology:constraint_metric(legacy_identity_lock_in, resistance, R), R =< 0.15,
    domain_priors:emerges_naturally(legacy_identity_lock_in).

:- end_tests(legacy_identity_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Classified as a Mountain because the identity is formed by metaphysical boons
 * and decades of physical training that cannot be simply "willed" away. 
 * The extractiveness is low (0.12) because it is a foundational state rather than
 * an active tax, though it imposes severe psychological constraints.
 *
 * PERSPECTIVAL GAP:
 * There is no gap in type (Mountain), but a gap in experience. To the King, 
 * it is the "unbroken line of male descent"; to Chitra, it is "primal injustice".
 *
 * DIRECTIONALITY LOGIC:
 * The lineage (Beneficiary) is subsidized by Chitra's suppression of her 
 * feminine self (Victim).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    omega_boon_malleability,
    'Is the boon of Shiva a physical Mountain of biology or a Snare of cultural enforcement?',
    'Empirical testing of whether Chitra could "un-learn" her strength through non-divine means.',
    'If false, the constraint degrades to a Piton of tradition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(omega_boon_malleability, conceptual, 'Nature vs. Nurture in divine boons').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legacy_identity_lock_in, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS
   ========================================================================== */

% Stability data for the Mountain.
narrative_ontology:measurement(lili_tr_t0, legacy_identity_lock_in, theater_ratio, 0, 0.05).
narrative_ontology:measurement(lili_tr_t10, legacy_identity_lock_in, theater_ratio, 10, 0.05).

narrative_ontology:measurement(lili_ex_t0, legacy_identity_lock_in, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(lili_ex_t10, legacy_identity_lock_in, base_extractiveness, 10, 0.12).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legacy_identity_lock_in, resource_allocation). % Lineage as social resource.

% The lock-in is upstream of the cost of gender performance.
narrative_ontology:affects_constraint(legacy_identity_lock_in, gender_performance_cost).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
