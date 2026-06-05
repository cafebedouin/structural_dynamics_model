% ============================================================================
% CONSTRAINT STORY: kjv_puritan_new_world_exit
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_puritan_new_world_exit, []).

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
 *   constraint_id: kjv_puritan_new_world_exit
 *   human_readable: Puritan Textual Re-Indexing (KJV in the New World)
 *   domain: political/religious
 *
 * SUMMARY:
 *   This story tracks the movement of the King James Version (KJV) of the
 *   Bible across a geographic 'Exit Option.' In England, the KJV was a tool
 *   of the Crown used to suppress Puritan identity (perceived as a Snare).
 *   Upon arrival in the New World, Puritans transformed the KJV into a tool
 *   for self-determination, reflecting their evolving power dynamics and
 *   textual interpretation.
 *
 * KEY AGENTS:
 *   - Puritans in England: Primary target (powerless/trapped) - suppressed dissent under the Crown.
 *   - Puritan Congregations in New England: Primary beneficiary (institutional/arbitrage) - self-organized and re-indexed the KJV.
 *   - English Crown: (institutional/constrained) - Lost control of KJV interpretation after puritan exit.
 *   - Anglican Clergy: (institutional/constrained) - Saw their authority challenged by independent Puritan interpretations.
 *   - Analytical Observer: Analytical perspective (analytical/analytical) - Analyzes the transformation of the KJV's role.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_puritan_new_world_exit, 0.6).
domain_priors:suppression_score(kjv_puritan_new_world_exit, 0.7).
domain_priors:theater_ratio(kjv_puritan_new_world_exit, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_puritan_new_world_exit, extractiveness, 0.6).
narrative_ontology:constraint_metric(kjv_puritan_new_world_exit, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(kjv_puritan_new_world_exit, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_puritan_new_world_exit, tangled_rope).
narrative_ontology:human_readable(kjv_puritan_new_world_exit, "Puritan Textual Re-Indexing (KJV in the New World)").
narrative_ontology:topic_domain(kjv_puritan_new_world_exit, "political/religious").

domain_priors:requires_active_enforcement(kjv_puritan_new_world_exit).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_puritan_new_world_exit, puritan_congregations).
narrative_ontology:constraint_victim(kjv_puritan_new_world_exit, english_crown).
narrative_ontology:constraint_victim(kjv_puritan_new_world_exit, anglican_clergy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Prior to emigration, Puritans experienced the KJV as a tool of the English Crown, suppressing dissent.
constraint_indexing:constraint_classification(kjv_puritan_new_world_exit, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% In the New World, Puritan congregations self-organized and used the KJV for their own purposes, exercising interpretive power.
constraint_indexing:constraint_classification(kjv_puritan_new_world_exit, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% The KJV's role transformed from suppression to self-determination for the Puritans post-emigration, marking a shift in power dynamics and textual interpretation.
constraint_indexing:constraint_classification(kjv_puritan_new_world_exit, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_puritan_new_world_exit_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kjv_puritan_new_world_exit, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kjv_puritan_new_world_exit, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(kjv_puritan_new_world_exit, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kjv_puritan_new_world_exit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): High prior to emigration, reflecting Crown control; moderated in New England. Suppression (0.7): Significant suppression in England due to Crown control. Theater ratio (0.3): Low theater ratio, as the KJV was central to Puritan religious practice.
 *
 * PERSPECTIVAL GAP:
 *   Puritans initially experienced the KJV as a snare but later re-indexed it as a rope for their own purposes. The analytical observer recognizes the transformation in the KJV's role across geographical and political contexts.
 *
 * DIRECTIONALITY LOGIC:
 *   In England, Puritans were victims of the KJV's enforced interpretation, yielding a high 'd' value. In the New World, they became beneficiaries, shifting the 'd' value.
 *
 * MANDATROPHY ANALYSIS:
 *   The shift in the KJV's role resolves the mandatrophy. It was initially a tool for Crown control (Snare) but transformed into a tool for self-determination for the Puritans (Rope) due to their geographical exit and subsequent self-governance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degree_of_crown_control,
    'To what extent did the Crown directly control KJV interpretation?',
    'Historical analysis of Crown directives and enforcement actions.',
    'Determines the extractiveness experienced by the Puritans in England.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degree_of_crown_control, empirical, 'Measures the level of control the Crown exercised over KJV interpretation and usage.').

omega_variable(
    self_governance_impact,
    'How did self-governance in New England affect Puritan interpretation of the KJV?',
    'Compare Puritan sermons and writings from England vs. New England.',
    'Clarifies the role of self-determination in re-indexing the KJV for Puritan purposes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_governance_impact, empirical, 'Quantifies the effect of self-governance on textual re-interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_puritan_new_world_exit, 1600, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t1620, kjv_puritan_new_world_exit, theater_ratio, 1620, 0.2).
narrative_ontology:measurement(kjv__tr_t1660, kjv_puritan_new_world_exit, theater_ratio, 1660, 0.3).
narrative_ontology:measurement(kjv__tr_t1700, kjv_puritan_new_world_exit, theater_ratio, 1700, 0.3).

% Extraction over time
narrative_ontology:measurement(kjv__be_t1620, kjv_puritan_new_world_exit, base_extractiveness, 1620, 0.7).
narrative_ontology:measurement(kjv__be_t1660, kjv_puritan_new_world_exit, base_extractiveness, 1660, 0.6).
narrative_ontology:measurement(kjv__be_t1700, kjv_puritan_new_world_exit, base_extractiveness, 1700, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_puritan_new_world_exit, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
