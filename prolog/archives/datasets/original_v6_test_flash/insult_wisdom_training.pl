% ============================================================================
% CONSTRAINT STORY: insult_wisdom_training
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_insult_wisdom_training, []).

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
 *   constraint_id: insult_wisdom_training
 *   human_readable: The Odd Assignment (Paying for Insults)
 *   domain: religious/social/philosophical
 *
 * SUMMARY:
 *   A disciple is commanded by his Master to pay anyone who insults him for
 *   three years as a prerequisite for wisdom. This unusual assignment
 *   functions as a constraint, shaping the disciple's behavior and
 *   relationships within the community. The core question is whether this
 *   serves as a legitimate method to train the disciple, or a method to
 *   extract resources.
 *
 * KEY AGENTS:
 *   - Disciple: Primary target (powerless/trapped) - must pay for insults.
 *   - Insulters: Beneficiaries (moderate/mobile) - receive payments from the disciple.
 *   - Master: Primary beneficiary (institutional/arbitrage) - assigns the task.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(insult_wisdom_training, 0.6).
domain_priors:suppression_score(insult_wisdom_training, 0.7).
domain_priors:theater_ratio(insult_wisdom_training, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(insult_wisdom_training, extractiveness, 0.6).
narrative_ontology:constraint_metric(insult_wisdom_training, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(insult_wisdom_training, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(insult_wisdom_training, tangled_rope).
narrative_ontology:human_readable(insult_wisdom_training, "The Odd Assignment (Paying for Insults)").
narrative_ontology:topic_domain(insult_wisdom_training, "religious/social/philosophical").

domain_priors:requires_active_enforcement(insult_wisdom_training).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(insult_wisdom_training, insulters).
narrative_ontology:constraint_beneficiary(insult_wisdom_training, master).
narrative_ontology:constraint_victim(insult_wisdom_training, disciple).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The Disciple experiences the constraint as a Snare because he is forced to pay those who insult him, suppressing his natural inclination to defend himself and his resources. He is trapped because he believes it is a prerequisite for wisdom.
constraint_indexing:constraint_classification(insult_wisdom_training, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% The Master benefits from the constraint as he can use this method to train disciples to gain wisdom. He could decide not to assign this at any time, giving him arbitrage exit options.
constraint_indexing:constraint_classification(insult_wisdom_training, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% The Analytical Observer sees the constraint as a Tangled Rope because it involves both coordination (a method of training) and extraction (the disciple must pay others).
constraint_indexing:constraint_classification(insult_wisdom_training, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(insult_wisdom_training_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(insult_wisdom_training, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(insult_wisdom_training, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(insult_wisdom_training, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(insult_wisdom_training_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.6 because the disciple is losing resources to those who are insulting him, which would be the point of extraction. Suppression is 0.7 because the disciple is suppressing the inclination to defend himself from the insults and instead pays for them. Theater Ratio is 0.3 because there is not much of a show in the interaction, but rather a transactional process. The master benefits and the Disciple is taken advantage of.
 *
 * PERSPECTIVAL GAP:
 *   The Disciple views this as a snare because he must pay for insults to receive wisdom, an action that would be suppressing his natural response. The Master sees this as a means of training as a rope, which he can end at any time. The Analytical Observer sees the entanglement of both and classifies this as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the relationship between the actors. The disciple is the target, because he must pay for the insults. The master is the beneficiary as he assigns the task and sees a use for the disciple. The insulters are also considered a beneficiary.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_of_insult_training,
    'Is paying for insults an effective method for gaining wisdom?',
    'Empirical study comparing the wisdom gained by disciples undergoing this training versus other training methods.',
    'If effective, the constraint is a rope. If ineffective, the constraint is a snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(efficacy_of_insult_training, empirical, 'The efficacy of using paying for insults as a wisdom training method.').

omega_variable(
    alternative_interpretations,
    'Are there alternative interpretations of the Master''s command that reduce the extraction?',
    'Philosophical and religious analysis of the command''s meaning and purpose.',
    'If alternatives exist, the constraint is less of a snare and more of a tangled rope or scaffold. If not, the constraint is more of a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_interpretations, conceptual, 'Whether alternative interpretations affect the extraction of the command.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(insult_wisdom_training, 0, 3).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(insu_tr_t0, insult_wisdom_training, theater_ratio, 0, 0.2).
narrative_ontology:measurement(insu_tr_t1, insult_wisdom_training, theater_ratio, 1, 0.3).
narrative_ontology:measurement(insu_tr_t3, insult_wisdom_training, theater_ratio, 3, 0.3).

% Extraction over time
narrative_ontology:measurement(insu_be_t0, insult_wisdom_training, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(insu_be_t1, insult_wisdom_training, base_extractiveness, 1, 0.6).
narrative_ontology:measurement(insu_be_t3, insult_wisdom_training, base_extractiveness, 3, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(insult_wisdom_training, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
