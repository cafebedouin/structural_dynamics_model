% ============================================================================
% CONSTRAINT STORY: parable_fish_turtle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_parable_fish_turtle, []).

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
 *   constraint_id: parable_fish_turtle
 *   human_readable: The Ontological Lake (Fish and Turtle Parable)
 *   domain: philosophical/social
 *
 * SUMMARY:
 *   This constraint models the limits of understanding based on lived
 *   experience, as illustrated by the parable of the fish and the turtle. The
 *   fish, having only known life in the water, struggles to comprehend the
 *   turtle's descriptions of the world on land. This highlights how our
 *   perspectives are shaped by our environments and how difficult it can be
 *   to grasp concepts outside our immediate experience. The extractiveness is
 *   related to how established paradigms often suppress novel perspectives
 *   that challenge them.
 *
 * KEY AGENTS:
 *   - Established Paradigms: Beneficiary (moderate/constrained) - Benefits from maintaining the status quo but is constrained to adapt to survive
 *   - Novel Perspectives: Victim (powerless/trapped) - Struggles to gain acceptance, limiting contribution to progress.
 *   - Intellectual Progress: Victim (powerless/trapped) - Hampered by the suppression of novel ideas.
 *   - Analytical Observer: (analytical/analytical) - Observes the inherent limitations and essential role of different perspectives.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(parable_fish_turtle, 0.45).
domain_priors:suppression_score(parable_fish_turtle, 0.3).
domain_priors:theater_ratio(parable_fish_turtle, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(parable_fish_turtle, extractiveness, 0.45).
narrative_ontology:constraint_metric(parable_fish_turtle, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(parable_fish_turtle, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(parable_fish_turtle, tangled_rope).
narrative_ontology:human_readable(parable_fish_turtle, "The Ontological Lake (Fish and Turtle Parable)").
narrative_ontology:topic_domain(parable_fish_turtle, "philosophical/social").

domain_priors:requires_active_enforcement(parable_fish_turtle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(parable_fish_turtle, established_paradigms).
narrative_ontology:constraint_victim(parable_fish_turtle, novel_perspectives).
narrative_ontology:constraint_victim(parable_fish_turtle, intellectual_progress).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Novel Perspectives (Snare). New ideas are often suppressed or dismissed because they challenge the dominant worldview. These perspectives are 'trapped' within the established paradigm, unable to gain traction or acceptance. Their ability to influence and contribute to intellectual progress is severely limited.
constraint_indexing:constraint_classification(parable_fish_turtle, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(universal))).

% Perspective 2: Established Paradigms (Tangled Rope). Dominant worldviews benefit from maintaining the status quo, often resisting new ideas that threaten their authority. While established paradigms provide a framework for understanding, they can also constrain intellectual progress by suppressing alternative perspectives. They are constrained because they must adapt to survive, and benefit by maintaining control and perceived truth.
constraint_indexing:constraint_classification(parable_fish_turtle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(universal))).

% Perspective 3: Analytical Observer (Rope). From a broad, analytical perspective, the parable highlights the inherent limitations of human understanding. The observer recognizes that our knowledge is always shaped by our experiences and that different perspectives are essential for intellectual growth. Sees the process as coordination between perspectives to refine understanding.
constraint_indexing:constraint_classification(parable_fish_turtle, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parable_fish_turtle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(parable_fish_turtle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parable_fish_turtle, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(parable_fish_turtle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45): Moderate. The suppression of novel ideas slows intellectual progress, but does not completely halt it. New ideas eventually find their way into the mainstream. Suppression (0.30): Moderate. There is a degree of resistance from established paradigms to maintain their authority. The theater ratio is low, representing the lower performative aspects of this parable.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap lies in the difference between those within and outside the existing paradigm. Novel perspectives experience the dominant view as extractive, while the holders of established paradigms may see their resistance as necessary for maintaining order and coherence. The analytical observer sees the interplay as both necessary and potentially limiting.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are established paradigms (they maintain control). Victims are novel perspectives and intellectual progress (they are suppressed).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    paradigm_shift_threshold,
    'What level of accumulated counter-evidence is required to trigger a paradigm shift, overcoming the inherent resistance to novel perspectives?',
    'Historical analysis of scientific and social revolutions, identifying common patterns in the accumulation and acceptance of new ideas.',
    'If the threshold is too high, intellectual progress is stifled. If the threshold is too low, unsubstantiated claims gain traction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paradigm_shift_threshold, empirical, 'The level of evidence needed for a paradigm shift.').

omega_variable(
    experiential_understanding_limits,
    'To what extent is human understanding fundamentally limited by our individual and collective experiences, creating inherent biases and blind spots?',
    'Cross-cultural studies of cognition and perception, investigating how different environments and social contexts shape our understanding of the world.',
    'Determines the inherent limits of rationality and the inevitability of perspectival bias.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(experiential_understanding_limits, conceptual, 'The limits of experience on understanding.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(parable_fish_turtle, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(para_tr_t0, parable_fish_turtle, theater_ratio, 0, 0.05).
narrative_ontology:measurement(para_tr_t50, parable_fish_turtle, theater_ratio, 50, 0.1).
narrative_ontology:measurement(para_tr_t100, parable_fish_turtle, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(para_be_t0, parable_fish_turtle, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(para_be_t50, parable_fish_turtle, base_extractiveness, 50, 0.4).
narrative_ontology:measurement(para_be_t100, parable_fish_turtle, base_extractiveness, 100, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(parable_fish_turtle, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
