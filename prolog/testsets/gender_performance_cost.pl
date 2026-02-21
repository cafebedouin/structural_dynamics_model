% ============================================================================
% CONSTRAINT STORY: gender_performance_cost
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-20
% ============================================================================

:- module(constraint_gender_performance_cost, []).

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
 * constraint_id: gender_performance_cost
 * human_readable: The Labor of the Borrowed Mask
 * domain: Gender Theory / Ontological Ethics
 *
 * SUMMARY:
 * This constraint represents the physical and psychological extraction 
 * required for Chitra to maintain the "borrowed beauty" granted by the gods 
 *. While it coordinates the attraction of Arjuna, it 
 * requires her to "deck her every day" and treat her own body as a "rival" 
 *. The cost is the suppression of her "manlike 
 * strength" and "man's training".
 *
 * KEY AGENTS (by structural relationship):
 * - Chitra: Primary target (powerless/trapped) — bears the "hateful task" of 
 * deceit.
 * - Arjuna: Primary beneficiary (powerful/mobile) — consumes the "wine of 
 * pleasure".
 * - Madana/Vasanta: Enforcers (institutional) — provide the "fragrant wine of 
 * heaven".
 * - Analytical Observer: Sees the "Tangled Rope" of coordination via 
 * extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gender_performance_cost, 0.58).
domain_priors:suppression_score(gender_performance_cost, 0.62).
domain_priors:theater_ratio(gender_performance_cost, 0.88). % High: The beauty is "falsehood".

% --- Constraint metric facts ---
narrative_ontology:constraint_metric(gender_performance_cost, extractiveness, 0.58).
narrative_ontology:constraint_metric(gender_performance_cost, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gender_performance_cost, theater_ratio, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gender_performance_cost, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(gender_performance_cost). % Enforced by the Gods.

% --- Structural relationships ---
% Arjuna benefits from the "perfect completion of life's desire".
narrative_ontology:constraint_beneficiary(gender_performance_cost, arjuna_the_warrior).

% Chitra is the victim, sitting "ashamed of her naked poverty".
narrative_ontology:constraint_victim(gender_performance_cost, chitra_true_self).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% From Chitra's view, the performance is a trap that robs her of "the prizes 
% of love" for her true self.
constraint_indexing:constraint_classification(gender_performance_cost, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Arjuna perceives a "vision of so much loveliness" that solves his 
% "love-hungered" state.
constraint_indexing:constraint_classification(gender_performance_cost, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Recognizes the coordination (love) built on structural extraction (deceit).
constraint_indexing:constraint_classification(gender_performance_cost, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gender_performance_cost_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gender_performance_cost, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gender_performance_cost, rope, context(agent_power(powerful), _, _, _)).

test(tangled_rope_requirements) :-
    narrative_ontology:constraint_beneficiary(gender_performance_cost, _),
    narrative_ontology:constraint_victim(gender_performance_cost, _),
    domain_priors:requires_active_enforcement(gender_performance_cost).

:- end_tests(gender_performance_cost_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Classified as a Tangled Rope because it facilitates a functional 
 * relationship (coordination) but does so by extracting Chitra's identity 
 *. The high theater_ratio (0.88) highlights that 
 * this is a "disguise".
 *
 * PERSPECTIVAL GAP:
 * Chitra sees a Snare: she is "the woman ashamed of her naked poverty" 
 *. Arjuna sees a Rope: the "wealth of the world" 
 * and "end of all poverty".
 *
 * DIRECTIONALITY LOGIC:
 * The constraint extracts from Chitra (Victim) to subsidize the aesthetic 
 * and emotional needs of Arjuna (Beneficiary).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    omega_performance_internalization,
    'Does the labor of performance eventually modify the Mountain of base identity?',
    'Observation of Chitra after the year expires—does she retain "Cupid\'s archery"?',
    'If true, the Snare becomes a Scaffold for a new synthesis of self.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(omega_performance_internalization, conceptual, 'Plasticity of performed identity').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gender_performance_cost, 0, 1). % The duration is one year.

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS
   ========================================================================== */

% Performance intensity at start, middle, and end of the year.
narrative_ontology:measurement(gpc_tr_t0, gender_performance_cost, theater_ratio, 0, 0.95).
narrative_ontology:measurement(gpc_tr_t5, gender_performance_cost, theater_ratio, 5, 0.88).
narrative_ontology:measurement(gpc_tr_t10, gender_performance_cost, theater_ratio, 10, 0.40). % Performance drops at revelation.

narrative_ontology:measurement(gpc_ex_t0, gender_performance_cost, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(gpc_ex_t10, gender_performance_cost, base_extractiveness, 10, 0.75). % Extraction peaks as "despair" grows.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gender_performance_cost, information_standard). % Gender as a signaling standard.

% Downstream of identity substrate; upstream of the final Ontological Friction resolution.
narrative_ontology:affects_constraint(gender_performance_cost, ontological_friction_resolution).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
