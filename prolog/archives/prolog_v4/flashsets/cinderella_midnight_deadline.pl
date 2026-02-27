% ============================================================================
% CONSTRAINT STORY: cinderella_midnight_deadline
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cinderella_midnight_deadline, []).

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
 *   constraint_id: cinderella_midnight_deadline
 *   human_readable: The Fairy Godmother's Midnight Deadline
 *   domain: magical/social
 *
 * SUMMARY:
 *   A Fairy Godmother grants Cinderella a temporary, high-status
 *   transformation (coach, gown, etc.) to attend a royal ball. A strict
 *   midnight deadline is imposed, after which all magic reverts. This
 *   constraint governs Cinderella's actions and relationships during the
 *   ball.
 *
 * KEY AGENTS:
 *   - Cinderella: Primary target (powerless/trapped) - Undergoes transformation but faces reversion.
 *   - The Fairy Godmother: Primary beneficiary (institutional/arbitrage) - Enforces magical rules and expectations.
 *   - The Prince: Secondary actor (institutional/constrained) - Benefits from meeting Cinderella, but his courtship is also constrained by social norms.
 *   - The Kingdom: Benefits from the potential union with a suitable partner.
 *   - Analytical Observer: Analyzes from a civilizational perspective the impact of the magical gift.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cinderella_midnight_deadline, 0.6).
domain_priors:suppression_score(cinderella_midnight_deadline, 0.7).
domain_priors:theater_ratio(cinderella_midnight_deadline, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cinderella_midnight_deadline, extractiveness, 0.6).
narrative_ontology:constraint_metric(cinderella_midnight_deadline, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(cinderella_midnight_deadline, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cinderella_midnight_deadline, tangled_rope).
narrative_ontology:human_readable(cinderella_midnight_deadline, "The Fairy Godmother's Midnight Deadline").
narrative_ontology:topic_domain(cinderella_midnight_deadline, "magical/social").

domain_priors:requires_active_enforcement(cinderella_midnight_deadline).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cinderella_midnight_deadline, the_prince).
narrative_ontology:constraint_beneficiary(cinderella_midnight_deadline, the_kingdom).
narrative_ontology:constraint_victim(cinderella_midnight_deadline, cinderella).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Cinderella experiences the constraint as a snare because she is trapped by the deadline. If she fails to meet it, she reverts to her former state. She has no agency to extend the deadline.
constraint_indexing:constraint_classification(cinderella_midnight_deadline, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% The Fairy Godmother experiences the constraint as a rope because it's a coordination mechanism to ensure Cinderella's proper behavior and maintains the magical agreement. She benefits from ensuring the magic is used responsibly.
constraint_indexing:constraint_classification(cinderella_midnight_deadline, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% The prince benefits from the introduction of a suitable partner but is constrained by the need for a proper courtship within the defined social structure. He experiences a mix of coordination and extraction.
constraint_indexing:constraint_classification(cinderella_midnight_deadline, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% From an analytical perspective, the deadline is a tangled rope: it provides a temporary opportunity but also enforces social norms and expectations, creating a mix of benefits and constraints. The time constraint is an active enforcement of a social rule.
constraint_indexing:constraint_classification(cinderella_midnight_deadline, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cinderella_midnight_deadline_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cinderella_midnight_deadline, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cinderella_midnight_deadline, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cinderella_midnight_deadline, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cinderella_midnight_deadline_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): High, due to the significant risk of reversion and potential social cost if discovered. Suppression (0.7): High, given the limited control Cinderella has over the duration and conditions of the magic. Theater Ratio (0.4): Moderate. Some performative elements are involved, such as maintaining the illusion of status, but genuine opportunity exists for connection.
 *
 * PERSPECTIVAL GAP:
 *   Cinderella experiences the deadline as a high-stakes limitation (Snare), while the Fairy Godmother sees it as a necessary condition for responsible magic use (Rope). The Prince experiences a mix (Tangled Rope) due to the blending of potential benefits and social expectations. The kingdom benefits from Cinderella and the Prince potentially being together.
 *
 * DIRECTIONALITY LOGIC:
 *   Cinderella bears the cost of possible reversion and loss of the new identity, having no exit from the deadline. The Fairy Godmother benefits as enforcer of magic with the arbitrage exit option of removing the magic. The Prince benefits from the magical introduction of a suitable partner, thus benefiting but also being under social constraints.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    magic_source,
    'Is the Fairy Godmother''s magic source limited or unlimited?',
    'Detailed account of Fairy Godmother''s power usage and possible depletion over long periods.',
    'If magic is limited, the constraint becomes more about efficient resource allocation. If unlimited, the constraint may be about maintaining social order and limiting chaos.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(magic_source, empirical, 'Determine whether fairy godmother magic is a scarce resource').

omega_variable(
    deadline_negotiability,
    'Can the midnight deadline be negotiated or extended under certain circumstances?',
    'Analysis of other fairy tales to find precedence for altering spell conditions.',
    'If negotiable, it weakens the constraint toward a scaffold. If unchangeable, it strengthens the snare aspects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deadline_negotiability, conceptual, 'Understand whether deadline is hard or flexible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cinderella_midnight_deadline, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cind_tr_t0, cinderella_midnight_deadline, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cind_tr_t6, cinderella_midnight_deadline, theater_ratio, 6, 0.3).
narrative_ontology:measurement(cind_tr_t12, cinderella_midnight_deadline, theater_ratio, 12, 0.4).

% Extraction over time
narrative_ontology:measurement(cind_be_t0, cinderella_midnight_deadline, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(cind_be_t6, cinderella_midnight_deadline, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(cind_be_t12, cinderella_midnight_deadline, base_extractiveness, 12, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cinderella_midnight_deadline, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
