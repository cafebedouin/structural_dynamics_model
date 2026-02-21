% ============================================================================
% CONSTRAINT STORY: ontological_friction_resolution
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-20
% ============================================================================

:- module(constraint_ontological_friction_resolution, []).

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
 * constraint_id: ontological_friction_resolution
 * human_readable: The Chaste Fire of Truth
 * domain: Metaphysics / Identity Resolution
 *
 * SUMMARY:
 * This constraint represents the "chaste fire of pain" that arises when the 
 * "vaporous veil of smiles" (the mask) is stripped away to reveal the "bare 
 * simplicity of truth". It is the final structural 
 * delta between "Illusion" and "Truth". The friction 
 * resolves when the "playmate of the night" aspires to be the "helpmeet of 
 * the day".
 *
 * KEY AGENTS (by structural relationship):
 * - Chitra: Primary target (powerless/trapped) — undergoes the "last sacrifice" 
 * of unveiling her "dirty garments" and "bleeding feet".
 * - Arjuna: Primary beneficiary (powerful/mobile) — whose life becomes "full" 
 * upon meeting the "ultimate you".
 * - The "True Self": Analytical entity — emerging from the "languorous grace" 
 * of the body.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ontological_friction_resolution, 0.72).
domain_priors:suppression_score(ontological_friction_resolution, 0.68).
domain_priors:theater_ratio(ontological_friction_resolution, 0.30). % Low theater at resolution: Truth is "bare".

% --- Constraint metric facts ---
narrative_ontology:constraint_metric(ontological_friction_resolution, extractiveness, 0.72).
narrative_ontology:constraint_metric(ontological_friction_resolution, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ontological_friction_resolution, theater_ratio, 0.30).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ontological_friction_resolution, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(ontological_friction_resolution). % Enforced by the "fire of pain".

% --- Structural relationships ---
% The "Full Life" (Arjuna) is the beneficiary of the resolution.
narrative_ontology:constraint_beneficiary(ontological_friction_resolution, arjuna_fulfilled_life).

% The "False Form" (The Boon) is the victim of this resolution's truth.
narrative_ontology:constraint_victim(ontological_friction_resolution, borrowed_beauty_mask).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE REVEALED SUBJECT (SNARE)
% Chitra initially experiences this as an "imperfection" and "blemish" that 
% could lead to rejection.
constraint_indexing:constraint_classification(ontological_friction_resolution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE SEEKER OF TRUTH (ROPE)
% Arjuna views the resolution as the "end of all poverty" and the "goal of 
% all efforts".
constraint_indexing:constraint_classification(ontological_friction_resolution, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Recognizes that the "Ultimate Truth" is a coordination that requires the 
% extraction (destruction) of the pleasant "Illusion".
constraint_indexing:constraint_classification(ontological_friction_resolution, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ontological_friction_resolution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ontological_friction_resolution, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ontological_friction_resolution, rope, context(agent_power(powerful), _, _, _)).

test(extraction_threshold) :-
    narrative_ontology:constraint_metric(ontological_friction_resolution, extractiveness, E),
    E >= 0.46. % Resolution through "pain" is a high-extraction event.

:- end_tests(ontological_friction_resolution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Classified as a Tangled Rope because the resolution synthesizes the "Warrior" 
 * and "Woman" roles into a single coordination (the "second Arjuna" son), but 
 * requires the high extraction of "bleeding feet" and "dirty garments" 
 *.
 *
 * PERSPECTIVAL GAP:
 * Chitra's "last sacrifice" is a Snare of vulnerability; Arjuna's "bare 
 * simplicity of truth" is the Rope of arrival.
 *
 * DIRECTIONALITY LOGIC:
 * The "Truth" extracts from the ego's desire for "perfect completeness" 
 * to subsidize the long-term "fruitage" of duty and lineage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    omega_son_synthesis,
    'Does the birth of the "second Arjuna" permanently resolve the Ontological Friction?',
    'Observation of the next generation\'s identity formation.',
    'If true, the Tangled Rope of identity becomes a Mountain of legacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(omega_son_synthesis, empirical, 'Trans-generational resolution of identity').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ontological_friction_resolution, 9, 10). % The final transition at the end of the year.

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS
   ========================================================================== */

% Transition from the "Mask" to "Naked Dignity".
narrative_ontology:measurement(ofr_tr_t0, ontological_friction_resolution, theater_ratio, 0, 0.88). % The mask is peak.
narrative_ontology:measurement(ofr_tr_t10, ontological_friction_resolution, theater_ratio, 10, 0.05). % Truth stands bare.

narrative_ontology:measurement(ofr_ex_t0, ontological_friction_resolution, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(ofr_ex_t10, ontological_friction_resolution, base_extractiveness, 10, 0.72). % Peak extraction at the moment of reveal.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ontological_friction_resolution, global_infrastructure). % The "world-path" of Truth.

% Downstream of both the Identity substrate and the Performance cost.
narrative_ontology:affects_constraint(legacy_identity_lock_in, ontological_friction_resolution).
narrative_ontology:affects_constraint(gender_performance_cost, ontological_friction_resolution).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
