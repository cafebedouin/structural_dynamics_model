% ============================================================================
% CONSTRAINT STORY: grete_samsa_transition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_grete_samsa_transition, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: grete_samsa_transition
 *   human_readable: Grete's Burden and Ascendance in The Metamorphosis
 *   domain: social/economic
 *
 * SUMMARY:
 *   This constraint models the evolving role of Grete Samsa following her
 *   brother Gregor's transformation. Initially burdened with caring for
 *   Gregor, Grete's role expands to include providing for the family's
 *   economic needs. This transition showcases a dynamic where extraction (the
 *   initial burden) is balanced by an increasing degree of coordination
 *   (Grete's emerging agency and family support).
 *
 * KEY AGENTS:
 *   - Gregor Samsa: Primary target (powerless/trapped) - experiences high extraction and dependence.
 *   - Grete Samsa: Mixed target/beneficiary (moderate/constrained) - initially burdened, later empowered.
 *   - Samsa Family: Beneficiary (institutional/constrained) - relies on Grete's adaptation for survival.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(grete_samsa_transition, 0.65).
domain_priors:suppression_score(grete_samsa_transition, 0.45).
domain_priors:theater_ratio(grete_samsa_transition, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(grete_samsa_transition, extractiveness, 0.65).
narrative_ontology:constraint_metric(grete_samsa_transition, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(grete_samsa_transition, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(grete_samsa_transition, tangled_rope).
narrative_ontology:human_readable(grete_samsa_transition, "Grete's Burden and Ascendance in The Metamorphosis").
narrative_ontology:topic_domain(grete_samsa_transition, "social/economic").

domain_priors:requires_active_enforcement(grete_samsa_transition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(grete_samsa_transition, samsa_family).
narrative_ontology:constraint_beneficiary(grete_samsa_transition, grete_samsa).
narrative_ontology:constraint_victim(grete_samsa_transition, gregor_samsa).
narrative_ontology:constraint_victim(grete_samsa_transition, grete_samsa).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GREGOR SAMSA (SNARE) - Gregor is trapped in his transformed state and completely dependent on his family, particularly Grete, for survival. He experiences the constraint as pure extraction, as his ability to contribute is gone, and he is a burden. Initially, he has some hope for reconnection and usefulness, which gradually diminishes. He is structurally powerless, with no exit options within the family context.
constraint_indexing:constraint_classification(grete_samsa_transition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: GRETE SAMSA (TANGLED ROPE) - Grete experiences a mix of coordination and extraction. Initially, she is burdened with caring for Gregor, but as she takes on more responsibility within the family, she gains skills and influence. She has limited exit options due to familial duty but is not completely trapped. Her extraction is high initially but decreases as she ascends within the family structure.
constraint_indexing:constraint_classification(grete_samsa_transition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: SAMSA FAMILY (ROPE) - The family initially coordinates to deal with Gregor's condition. Grete's increasing responsibilities provide a means to manage their changed circumstances. The extraction is low as the family is benefitting from Grete's contributions and ability to create new income streams.
constraint_indexing:constraint_classification(grete_samsa_transition, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) - From a civilizational, analytical perspective, Grete's transformation represents a complex dynamic of burden and ascendance. Her initial burden is heavy, but her adaptation ultimately empowers her. The structural elements include the dependence of Gregor, the family's need for income, and Grete's own skills. This is a tangled rope due to the presence of both significant extraction (Gregor and initially Grete) and a coordinating function (survival of the family).
constraint_indexing:constraint_classification(grete_samsa_transition, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(grete_samsa_transition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(grete_samsa_transition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(grete_samsa_transition, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(grete_samsa_transition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(grete_samsa_transition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. Initially, Grete bears a heavy burden of caring for Gregor, which is emotionally and physically draining. This burden decreases as she takes on more responsibilities and gains influence. Suppression (0.45): Moderate. Grete's initial options are limited due to family duty. As she takes on more responsibilities, her options increase, but she remains constrained by her family's needs. Theater Ratio (0.30): Low. Most of Grete's activity serves a functional purpose, with minimal theater.
 *
 * PERSPECTIVAL GAP:
 *   Gregor experiences the transformation as a snare due to his complete dependence and lack of agency. Grete experiences a tangled rope as her initial burden is balanced by her growing skills and influence within the family. The family experiences the transition as a rope, as they benefit from Grete's contributions to manage the crisis. The analytical observer recognizes the complex dynamic of extraction and coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Gregor's trapped exit option gives him a high directionality, which increases the effect of his extraction. Grete's constrained exit option reduces her experienced extraction, especially as she ascends within the family structure. The family's constrained exit option results in a low directionality, as they are benefitting from Grete's effort.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gretes_agency,
    'To what extent is Grete''s ascendance a genuine empowerment vs. a constrained adaptation?',
    'Analysis of Grete''s personal goals and desires vs. her actions within the story. Examination of whether her choices are truly free or dictated by circumstances.',
    'If genuine empowerment, the classification shifts toward a scaffold. If constrained adaptation, the classification remains a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gretes_agency, conceptual, 'The degree of Grete''s individual agency in her actions and ascendance.').

omega_variable(
    family_dependency,
    'How strong is the family''s dependency on Grete''s contributions vs. other available options?',
    'Examination of the family''s potential alternatives to Grete''s efforts. Analysis of the economic and social context of the story.',
    'If high dependency, the extraction element is strengthened, reinforcing the tangled rope or snare classification for some perspectives. If other options existed, the extraction is less severe.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(family_dependency, empirical, 'The degree of the family''s dependency on Grete''s contribution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(grete_samsa_transition, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gret_tr_t0, grete_samsa_transition, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gret_tr_t5, grete_samsa_transition, theater_ratio, 5, 0.3).
narrative_ontology:measurement(gret_tr_t10, grete_samsa_transition, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(gret_be_t0, grete_samsa_transition, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(gret_be_t5, grete_samsa_transition, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(gret_be_t10, grete_samsa_transition, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(grete_samsa_transition, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
