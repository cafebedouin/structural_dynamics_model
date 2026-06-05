% ============================================================================
% CONSTRAINT STORY: ulysses_chp01
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-03
% Status: [Draft]
% ============================================================================

:- module(constraint_ulysses_chp01, []).

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
 *   constraint_id: ulysses_chp01
 *   human_readable: The Martello Tower Usurpation
 *   domain: social/political/religious
 *
 * SUMMARY:
 *   The opening chapter of James Joyce's Ulysses finds Stephen Dedalus living
 *   in a Martello tower with Buck Mulligan and Haines. The tower, originally
 *   built for defense, now represents a space where social and intellectual
 *   tensions play out. Stephen, burdened by personal and historical traumas,
 *   finds himself increasingly alienated by the boisterous and often
 *   offensive behavior of his companions. This dynamic highlights themes of
 *   usurpation, dependence, and the struggle for artistic freedom.
 *
 * KEY AGENTS:
 *   - Stephen Dedalus: Primary target (powerless/trapped) — bears the brunt of the social extraction
 *   - Buck Mulligan: Primary beneficiary (powerful/arbitrage) — benefits from the social dynamic
 *   - Haines: Secondary actor (moderate/mobile) — occupies an ambiguous position as an English guest
 *   - British Military (Past): Institutional force (institutional/constrained) — the original purpose of the tower
 *   - Analytical Observer: Sees full structure (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp01, 0.6).
domain_priors:suppression_score(ulysses_chp01, 0.7).
domain_priors:theater_ratio(ulysses_chp01, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp01, extractiveness, 0.6).
narrative_ontology:constraint_metric(ulysses_chp01, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ulysses_chp01, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp01, snare).
narrative_ontology:human_readable(ulysses_chp01, "The Martello Tower Usurpation").
narrative_ontology:topic_domain(ulysses_chp01, "social/political/religious").

domain_priors:requires_active_enforcement(ulysses_chp01).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp01, buck_mulligan).
narrative_ontology:constraint_beneficiary(ulysses_chp01, haines).
narrative_ontology:constraint_victim(ulysses_chp01, stephen_dedalus).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Stephen Dedalus experiences the tower as a Snare. He is trapped by his dependence on Mulligan for shelter and companionship, and suppressed by the boisterous and often offensive behavior of Mulligan and Haines.
constraint_indexing:constraint_classification(ulysses_chp01, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Buck Mulligan benefits from the situation, enjoying the company and the relative freedom from societal constraints that the tower provides. He can engage in his antics without significant repercussions.
constraint_indexing:constraint_classification(ulysses_chp01, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% Haines, the English guest, is in a more ambiguous position. He benefits from the temporary lodging and the unique social dynamic, but is also somewhat constrained by the expectations of his hosts and his own cultural background. He's mobile and has the option of leaving anytime.
constraint_indexing:constraint_classification(ulysses_chp01, tangled_rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% Originally built as a defensive structure (rope), now a remnant of the past, no longer serving its original purpose, but still carries some weight of history.
constraint_indexing:constraint_classification(ulysses_chp01, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical observer sees the tower as a site of complex social dynamics, where power and dependence intertwine. It highlights the tensions between Irish identity, English influence, and artistic freedom.
constraint_indexing:constraint_classification(ulysses_chp01, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp01_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp01, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp01, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ulysses_chp01, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp01, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp01_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.6) because Stephen experiences a significant loss of agency and well-being due to his dependence on Mulligan and the oppressive atmosphere of the tower. The suppression is also high (0.7) because Stephen feels unable to fully express himself or escape the situation. The theater ratio is relatively high (0.75) because the interactions, while often performative, have real consequences for Stephen.
 *
 * PERSPECTIVAL GAP:
 *   Stephen experiences the tower as a Snare, feeling trapped and suppressed. Mulligan, on the other hand, benefits from the situation and experiences it as a source of freedom and amusement (Rope). Haines occupies an intermediate position, benefiting somewhat but also constrained by social expectations (Tangled Rope). The analytical perspective sees it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Stephen, as the primary victim, experiences a high degree of extraction. Mulligan, as the primary beneficiary, experiences a low degree of extraction. Haines, in his ambiguous position, experiences a moderate degree of extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This example highlights the subjective nature of constraint classification. What appears as a source of freedom for one person (Mulligan) is a source of oppression for another (Stephen). The analytical perspective attempts to capture the complexity of this dynamic by recognizing both the coordination and extraction aspects of the situation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stephens_agency,
    'To what extent is Stephen truly trapped, versus choosing to remain in this situation?',
    'Analyzing Stephen''s internal monologues and actions throughout Ulysses to determine his level of agency.',
    'If Stephen has high agency, the constraint is less of a Snare and more of a Tangled Rope. If low, then Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stephens_agency, conceptual, 'Degree of Stephen''s agency in his situation.').

omega_variable(
    mulligans_intent,
    'Is Mulligan intentionally manipulative, or simply oblivious to Stephen''s discomfort?',
    'Close reading of Mulligan''s dialogues and actions to infer his motivations.',
    'If Mulligan is intentionally manipulative, the extraction is higher. If oblivious, then extraction is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mulligans_intent, conceptual, 'Intentions behind Mulligan''s behavior.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp01, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ulys_tr_t0, ulysses_chp01, theater_ratio, 0, 0.6).
narrative_ontology:measurement(ulys_tr_t1, ulysses_chp01, theater_ratio, 1, 0.7).
narrative_ontology:measurement(ulys_tr_t2, ulysses_chp01, theater_ratio, 2, 0.75).

% Extraction over time
narrative_ontology:measurement(ulys_be_t0, ulysses_chp01, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(ulys_be_t1, ulysses_chp01, base_extractiveness, 1, 0.55).
narrative_ontology:measurement(ulys_be_t2, ulysses_chp01, base_extractiveness, 2, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
