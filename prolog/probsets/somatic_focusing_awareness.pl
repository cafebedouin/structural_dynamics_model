% ============================================================================
% CONSTRAINT STORY: somatic_focusing_awareness
% ============================================================================
% Version: 0.2 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_somatic_focusing_awareness, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: somatic_focusing_awareness
 *   human_readable: Somatic Focusing Awareness
 *   domain: social/psychological
 *
 * SUMMARY:
 *   This constraint defines the practice of 'staying with feelings' through
 *   'interested curiosity' without trying to change them or force
 *   communication. Somatic Focusing Awareness facilitates emotional
 *   understanding and self-regulation.
 *
 * KEY AGENTS:
 *   - Individual Practitioner: Primary beneficiary (powerless/mobile) - gains self-awareness and regulation skills
 *   - Therapist Guide: Secondary beneficiary (institutional/analytical) - benefits from enhanced therapeutic outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(somatic_focusing_awareness, 0.15).
domain_priors:suppression_score(somatic_focusing_awareness, 0.1).
domain_priors:theater_ratio(somatic_focusing_awareness, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(somatic_focusing_awareness, extractiveness, 0.15).
narrative_ontology:constraint_metric(somatic_focusing_awareness, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(somatic_focusing_awareness, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(somatic_focusing_awareness, rope).
narrative_ontology:human_readable(somatic_focusing_awareness, "Somatic Focusing Awareness").
narrative_ontology:topic_domain(somatic_focusing_awareness, "social/psychological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(somatic_focusing_awareness, individual_practitioner).
narrative_ontology:constraint_beneficiary(somatic_focusing_awareness, therapist_guide).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The individual experiencing the feelings directly benefits from increased awareness and understanding. They are mobile in that they can disengage, but focusing can create benefits.
constraint_indexing:constraint_classification(somatic_focusing_awareness, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% The practitioner benefits from the practice over their life by gaining tools that help self-regulate. The practice can be exited without strong effects but is encouraged long term.
constraint_indexing:constraint_classification(somatic_focusing_awareness, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% The therapeutic community as a whole benefits from this awareness being widely practiced, allowing for more effective therapies. They can fully analyze the situation, though the effects are generational.
constraint_indexing:constraint_classification(somatic_focusing_awareness, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% The analytical observer sees the practice as coordination, helping individuals better understand and navigate emotions.
constraint_indexing:constraint_classification(somatic_focusing_awareness, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(somatic_focusing_awareness_tests).
:- end_tests(somatic_focusing_awareness_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low. The practice involves minimal extraction, primarily requiring time and attention from the individual. Suppression (0.10): Low. There is little suppression as individuals are free to engage or disengage with the practice at will. Theater ratio (0.20): Low. The practice is largely functional, with minimal performative elements.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives view the constraint as a rope due to the coordination and benefits it provides across different roles and time horizons.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is positive for all participants as the practice promotes self-awareness and therapeutic effectiveness. The therapist guides benefit by increased client outcomes, while the practitioner benefits from improved emotional regulation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(somatic_focusing_awareness, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(somatic_focusing_awareness, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
