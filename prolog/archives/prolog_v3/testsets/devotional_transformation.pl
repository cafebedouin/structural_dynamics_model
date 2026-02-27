% ============================================================================
% CONSTRAINT STORY: devotional_transformation
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-19
% ============================================================================

:- module(constraint_devotional_transformation, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * constraint_id: devotional_transformation
 * human_readable: The Transformation through Devotion
 * domain: ontological/social
 *
 * SUMMARY:
 * The "Nursery Magic" process where a toy becomes "Real" through a child's
 * undivided affection. While providing ultimate ontological meaning, it
 * requires the "shabbiness" of the physical form as a non-negotiable fee.
 * This is a classic example of a process with both a genuine coordination
 * function (stabilizing the child's emotional world) and asymmetric
 * extraction (the physical decay of the toy).
 *
 * KEY AGENTS (by structural relationship):
 * - Velveteen Rabbit: Primary target (powerless/trapped) — bears physical decay.
 * - The Boy: Primary beneficiary (institutional/arbitrage) — receives
 *   emotional/coordinative stability. The Boy acts as the 'institution'
 *   of the nursery, setting its rules and values.
 * - Skin Horse: Analytical observer — understands the long-term utility and costs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(devotional_transformation, 0.45).
domain_priors:suppression_score(devotional_transformation, 0.10).
domain_priors:theater_ratio(devotional_transformation, 0.05).

% --- Constraint metric facts (engine primary keys) ---
narrative_ontology:constraint_metric(devotional_transformation, extractiveness, 0.45).
narrative_ontology:constraint_metric(devotional_transformation, suppression_requirement, 0.10).
narrative_ontology:constraint_metric(devotional_transformation, theater_ratio, 0.05).

% --- Constraint claim (Analytical perspective type) ---
narrative_ontology:constraint_claim(devotional_transformation, tangled_rope).

% --- Binary flags ---
% The "uncomfortable things" (loss of whiskers/fur) represent the structural enforcement.
domain_priors:requires_active_enforcement(devotional_transformation).

% --- Structural relationships (REQUIRED for Tangled Rope) ---
% The Boy benefits from the stable emotional coordination of the toy.
narrative_ontology:constraint_beneficiary(devotional_transformation, nursery_children).

% The Rabbit bears the cost of material degradation.
narrative_ontology:constraint_victim(devotional_transformation, toy_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE VELVETEEN RABBIT (TANGLED ROPE)
% As a victim with trapped exit options, the physical extraction is acute.
% Derived d ≈ 0.95 → high effective extraction (chi). The process has a
% genuine upside (becoming Real), preventing a Snare classification.
constraint_indexing:constraint_classification(devotional_transformation, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BOY (ROPE)
% As the primary beneficiary with arbitrage exit (he can always get another
% toy), the process is pure coordination. The physical decay of the toy is
% not an extraction from his perspective, but a feature of the bond.
% Derived d ≈ 0.05 → low/negative effective extraction (chi).
constraint_indexing:constraint_classification(devotional_transformation, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The observer (e.g., the Skin Horse) sees both the genuine coordination
% function for the Boy and the asymmetric extraction from the Rabbit.
% This dual nature is the hallmark of a Tangled Rope.
constraint_indexing:constraint_classification(devotional_transformation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(devotional_transformation_tests).

test(perspectival_gap) :-
    % Verify gap between the target's experience and the beneficiary's.
    constraint_indexing:constraint_classification(devotional_transformation, tangled_rope, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(devotional_transformation, rope, context(agent_power(institutional), _, _, _)).

test(tangled_rope_gate) :-
    % Verify all requirements for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(devotional_transformation, _),
    narrative_ontology:constraint_victim(devotional_transformation, _),
    domain_priors:requires_active_enforcement(devotional_transformation).

:- end_tests(devotional_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Becoming "Real" is not a free transformation. The base extractiveness (0.45)
 * represents the structural destruction of the physical toy (pink satin,
 * sawdust, whiskers) required to achieve the new ontological status. The
 * suppression is low (0.10) because there are many other toys, but none
 * offer this specific path to "Reality."
 *
 * DIRECTIONALITY LOGIC:
 * The Rabbit is the victim because it bears 100% of the physical cost.
 * The Boy is the beneficiary because he receives the psychological coordination
 * benefit of a "Real" companion without material loss. His role is
 * 'institutional' within the local scope of the nursery, as his affection
 * is the sole mechanism of enforcement and validation.
 *
 * MANDATROPHY ANALYSIS:
 * Labeling this as a Tangled Rope prevents us from dismissing the Rabbit's
 * suffering (as a pure Rope would) or ignoring the genuine magic and
 * coordinative value of the result (as a pure Snare would). It correctly
 * identifies that a valuable outcome is coupled to a costly, asymmetric process.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    omega_reality_persistence,
    'Does Reality persist once the primary beneficiary outgrows the nursery?',
    'Long-term observation of "Real" toys in adult storage.',
    'Determines if the coordination is permanent or a temporary Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(omega_reality_persistence, conceptual, 'Persistence of metaphysical status post-use.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(devotional_transformation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The process of becoming "Real" involves increasing physical decay (extraction)
% over the interval of the Boy's childhood.
%
% Theater ratio over time (starts as performative play, becomes real bond):
narrative_ontology:measurement(devotional_transformation_tr_t0, devotional_transformation, theater_ratio, 0, 0.30).
narrative_ontology:measurement(devotional_transformation_tr_t5, devotional_transformation, theater_ratio, 5, 0.15).
narrative_ontology:measurement(devotional_transformation_tr_t10, devotional_transformation, theater_ratio, 10, 0.05).

% Extraction over time (the toy becomes progressively more "shabby"):
narrative_ontology:measurement(devotional_transformation_ex_t0, devotional_transformation, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(devotional_transformation_ex_t5, devotional_transformation, base_extractiveness, 5, 0.30).
narrative_ontology:measurement(devotional_transformation_ex_t10, devotional_transformation, base_extractiveness, 10, 0.45).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Network Relationships
% This process is upstream of the terminal disposal protocol.
narrative_ontology:affects_constraint(devotional_transformation, hygiene_disposal_protocol).
% This process is downstream of the biological definition of 'Real'.
narrative_ontology:affects_constraint(biological_specification, devotional_transformation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */