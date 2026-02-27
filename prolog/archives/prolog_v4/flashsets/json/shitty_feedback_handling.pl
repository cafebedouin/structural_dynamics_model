% ============================================================================
% CONSTRAINT STORY: shitty_feedback_handling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shitty_feedback_handling, []).

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
 *   constraint_id: shitty_feedback_handling
 *   human_readable: The Protocol for Handling Shitty Feedback
 *   domain: social/professional
 *
 * SUMMARY:
 *   "Shitty feedback" is a ubiquitous byproduct of social systems where
 *   feedback skills are rare. The protocol for handling it involves complex
 *   power dynamics, emotional resilience, and organizational structures. It
 *   manifests as a tangled rope, with coordination in the form of maintaining
 *   hierarchical structure and extraction in the emotional toll it takes on
 *   employees and the drag it puts on organizational performance. It also has
 *   piton aspects, with existing HR protocols being little more than theater
 *   and organizational inertia maintaining the status quo.
 *
 * KEY AGENTS:
 *   - Employees Receiving Feedback: Primary target (powerless/trapped) - subject to the negative effects of poorly delivered feedback.
 *   - Ineffective Managers: Primary beneficiary (institutional/arbitrage) - benefits from maintaining control or avoiding difficult conversations.
 *   - HR Department: Secondary institutional actor (institutional/constrained) - maintains feedback protocols, often performative.
 *   - Well-Meaning Colleague: Moderate actor (moderate/constrained) - observes feedback dynamics and attempts to mitigate the negative effects
 *   - Feedback Training Industry: Beneficiary (powerful/arbitrage) - can arbitrage demand for better feedback skills.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shitty_feedback_handling, 0.55).
domain_priors:suppression_score(shitty_feedback_handling, 0.4).
domain_priors:theater_ratio(shitty_feedback_handling, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shitty_feedback_handling, extractiveness, 0.55).
narrative_ontology:constraint_metric(shitty_feedback_handling, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(shitty_feedback_handling, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shitty_feedback_handling, tangled_rope).
narrative_ontology:human_readable(shitty_feedback_handling, "The Protocol for Handling Shitty Feedback").
narrative_ontology:topic_domain(shitty_feedback_handling, "social/professional").

domain_priors:requires_active_enforcement(shitty_feedback_handling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shitty_feedback_handling, ineffective_managers).
narrative_ontology:constraint_beneficiary(shitty_feedback_handling, feedback_training_industry).
narrative_ontology:constraint_victim(shitty_feedback_handling, employees_receiving_feedback).
narrative_ontology:constraint_victim(shitty_feedback_handling, organizational_performance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The employee receiving the feedback often feels trapped, unable to effectively challenge or ignore the feedback without risking negative repercussions. The extraction comes from the emotional toll, wasted time, and potential career stagnation.
constraint_indexing:constraint_classification(shitty_feedback_handling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Colleagues observe the shitty feedback and may attempt to mitigate its effects or offer alternative interpretations. They are constrained by their own positions and relationships within the organization but benefit from maintaining a functional work environment.
constraint_indexing:constraint_classification(shitty_feedback_handling, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Managers who provide 'shitty feedback' may benefit from maintaining control, avoiding difficult conversations, or simply lacking the skills to provide constructive criticism. This is coordination for them, as the system supports the status quo.
constraint_indexing:constraint_classification(shitty_feedback_handling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% HR departments often have protocols for feedback, but these may be performative and ineffective at addressing the root causes of 'shitty feedback.' The process exists, but its functional value is limited.
constraint_indexing:constraint_classification(shitty_feedback_handling, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The observer sees the system as a tangled rope, with coordination (maintaining hierarchical structure) and extraction (emotional toll on employees). The observer sees the entire structure.
constraint_indexing:constraint_classification(shitty_feedback_handling, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shitty_feedback_handling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(shitty_feedback_handling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shitty_feedback_handling, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(shitty_feedback_handling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(shitty_feedback_handling, TR),
    TR >= 0.70.

:- end_tests(shitty_feedback_handling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because while the extraction can be high for individuals, the effects are often localized and diffused. Suppression is moderate (0.40) because employees often have limited options for challenging or ignoring the feedback without risking negative consequences. Theater is also moderate (0.75) since procedures are in place, but their efficacy is low.
 *
 * PERSPECTIVAL GAP:
 *   The employee feels trapped, the manager feels in control, the HR department fulfills its duty, and the organization performs sub-optimally. The system supports the status quo, providing benefits to managers and the HR department, but extracting from the employees and the organization as a whole.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the power level, exit options, and relationship to the extraction flow. The pipeline computes 'd' from these context parameters and applies a sigmoid function to produce effective extractiveness, chi.
 *
 * MANDATROPHY ANALYSIS:
 *   The protocol for handling shitty feedback demonstrates the nuances of human interaction within organizational structures. While certain perspectives may view the scenario as purely extractive or merely co-ordinative, this model identifies the co-existence of both elements creating a system that at once extracts from some whilst coordinating on behalf of others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feedback_skill_definition,
    'What constitutes ''good'' versus ''shitty'' feedback in a specific organizational context?',
    'Empirical studies of feedback effectiveness; analysis of the impact of different feedback styles on employee performance and well-being',
    'Clarifying the definition could shift the classification from Snare to Rope or vice versa, depending on whether the feedback is perceived as helpful or harmful.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feedback_skill_definition, conceptual, 'The definition of ''good'' vs ''shitty'' feedback').

omega_variable(
    cultural_feedback_norms,
    'How do cultural norms and expectations influence the perception and delivery of feedback?',
    'Cross-cultural studies of feedback practices; analysis of the impact of cultural differences on employee reactions to feedback',
    'Different cultural norms could mitigate or exacerbate the negative effects of ''shitty feedback,'' influencing the overall extractiveness of the system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_feedback_norms, empirical, 'The impact of cultural norms on feedback perception').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shitty_feedback_handling, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shit_tr_t0, shitty_feedback_handling, theater_ratio, 0, 0.4).
narrative_ontology:measurement(shit_tr_t5, shitty_feedback_handling, theater_ratio, 5, 0.65).
narrative_ontology:measurement(shit_tr_t10, shitty_feedback_handling, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(shit_be_t0, shitty_feedback_handling, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(shit_be_t5, shitty_feedback_handling, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(shit_be_t10, shitty_feedback_handling, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shitty_feedback_handling, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
