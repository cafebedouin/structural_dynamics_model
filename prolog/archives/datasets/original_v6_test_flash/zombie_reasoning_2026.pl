% ============================================================================
% CONSTRAINT STORY: zombie_reasoning_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zombie_reasoning_2026, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: zombie_reasoning_2026
 *   human_readable: The Zombie Reasoning Epistemic Snare
 *   domain: philosophical/technological
 *
 * SUMMARY:
 *   The Zombie Reasoning Epistemic Snare describes how the imprecise and
 *   overly broad application of terms like 'reasoning,' 'evaluating,' and
 *   'selecting' in discussions about artificial intelligence creates an
 *   environment of epistemic confusion. This ambiguity benefits proponents of
 *   AI hype while trapping those seeking genuine understanding and hindering
 *   informed public discourse.
 *
 * KEY AGENTS:
 *   - Epistemic Clarity: Primary victim (powerless/trapped) - the pursuit of precise understanding is directly undermined.
 *   - Informed Public Discourse: Secondary victim (moderate/constrained) - quality of public conversation is negatively affected.
 *   - AI Hype Proponents: Primary beneficiary (institutional/arbitrage) - benefit through funding and support facilitated by ambiguity.
 *   - Analytical Observer: Analyzes the dynamics of the situation (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zombie_reasoning_2026, 0.55).
domain_priors:suppression_score(zombie_reasoning_2026, 0.7).
domain_priors:theater_ratio(zombie_reasoning_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zombie_reasoning_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(zombie_reasoning_2026, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(zombie_reasoning_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zombie_reasoning_2026, snare).
narrative_ontology:human_readable(zombie_reasoning_2026, "The Zombie Reasoning Epistemic Snare").
narrative_ontology:topic_domain(zombie_reasoning_2026, "philosophical/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zombie_reasoning_2026, ai_hype_proponents).
narrative_ontology:constraint_victim(zombie_reasoning_2026, epistemic_clarity).
narrative_ontology:constraint_victim(zombie_reasoning_2026, informed_public_discourse).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Epistemic Clarity (Snare) - The erosion of precise language traps those seeking genuine understanding, hindering their ability to critically evaluate AI claims. No exit option as the distortion becomes pervasive.
constraint_indexing:constraint_classification(zombie_reasoning_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: Informed Public Discourse (Snare) - Public discourse is constrained by the imprecise use of terms, leading to misinformed opinions and policy decisions. Limited exit due to information overload and lack of resources for critical analysis.
constraint_indexing:constraint_classification(zombie_reasoning_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 3: AI Hype Proponents (Rope) - Benefit from the ambiguity as it facilitates funding and public support. They can arbitrage the situation by using vague language to promote their agendas.
constraint_indexing:constraint_classification(zombie_reasoning_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 4: The Analytical Observer (Tangled Rope) - Observes the system as a whole, recognizing both the extractive aspects for epistemic clarity and the coordinating function of promoting AI development. The analytical observer is able to perceive both the beneficial and detrimental effects over a long timescale.
constraint_indexing:constraint_classification(zombie_reasoning_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zombie_reasoning_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(zombie_reasoning_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(zombie_reasoning_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(zombie_reasoning_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zombie_reasoning_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): The constraint extracts epistemic clarity from the discourse. Suppression (0.70): The ambiguity and hype suppress the ability to critically evaluate AI claims. Theater ratio (0.30): There is some performance of genuine reasoning, but it's often overshadowed by the hype.
 *
 * PERSPECTIVAL GAP:
 *   Epistemic Clarity and Informed Public Discourse see the constraint as a snare, trapping them in a confusing landscape. AI Hype Proponents experience it as a rope, facilitating their objectives. The Analytical Observer sees a Tangled Rope, recognizing both the benefits and harms.
 *
 * DIRECTIONALITY LOGIC:
 *   AI Hype Proponents are beneficiaries, so they have a low 'd' value. Epistemic Clarity and Informed Public Discourse are victims, giving them high 'd' values. The Analytical Observer has a moderate 'd' value.
 *
 * MANDATROPHY ANALYSIS:
 *   The Zombie Reasoning Snare is distinct from a coordination problem because its primary effect is to obscure rather than facilitate understanding. While it could be argued that the hype serves to coordinate investment and research, the resulting confusion outweighs any benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reasoning_definition_stability,
    'Can a stable and widely accepted definition of ''reasoning'' be established for AI systems?',
    'Philosophical analysis, consensus-building workshops, and formalization efforts within the AI community.',
    'If yes, the snare weakens, allowing for more precise evaluation of AI capabilities. If no, the ambiguity persists, reinforcing the snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reasoning_definition_stability, conceptual, 'The stability of the definition of ''reasoning'' in the context of AI.').

omega_variable(
    public_understanding_threshold,
    'What level of public understanding is necessary to counteract the effects of imprecise language in AI discourse?',
    'Surveys, educational interventions, and media analysis to assess and improve public literacy on AI concepts.',
    'Determines the scale of effort needed to inform the public and mitigate the harms of the snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_understanding_threshold, empirical, 'The required level of public understanding of AI concepts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zombie_reasoning_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zomb_tr_t0, zombie_reasoning_2026, theater_ratio, 0, 0.1).
narrative_ontology:measurement(zomb_tr_t5, zombie_reasoning_2026, theater_ratio, 5, 0.2).
narrative_ontology:measurement(zomb_tr_t10, zombie_reasoning_2026, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(zomb_be_t0, zombie_reasoning_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(zomb_be_t5, zombie_reasoning_2026, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(zomb_be_t10, zombie_reasoning_2026, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
