% ============================================================================
% CONSTRAINT STORY: ulysses_chp03
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp03, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ulysses_chp03
 *   human_readable: The Ineluctable Modality (Sandymount Strand)
 *   domain: philosophical/social/technological
 *
 * SUMMARY:
 *   Stephen Dedalus navigates Sandymount Strand, bound by the 'ineluctable
 *   modality of the visible'. This refers to the inescapable nature of
 *   sensory perception and the limitations it imposes on human consciousness.
 *   He is constrained to experience the world through his senses, unable to
 *   transcend them fully.
 *
 * KEY AGENTS:
 *   - Stephen Dedalus: Central consciousness bound by the modality (powerless/trapped)
 *   - The 'Visible': The modality itself (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp03, 0.15).
domain_priors:suppression_score(ulysses_chp03, 0.03).
domain_priors:theater_ratio(ulysses_chp03, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp03, extractiveness, 0.15).
narrative_ontology:constraint_metric(ulysses_chp03, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(ulysses_chp03, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ulysses_chp03, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(ulysses_chp03, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp03, mountain).
narrative_ontology:human_readable(ulysses_chp03, "The Ineluctable Modality (Sandymount Strand)").
narrative_ontology:topic_domain(ulysses_chp03, "philosophical/social/technological").

domain_priors:emerges_naturally(ulysses_chp03).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Stephen is trapped by his immediate sensations; he cannot escape the present moment.
constraint_indexing:constraint_classification(ulysses_chp03, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% From a philosophical perspective, the ineluctable modality represents a fundamental aspect of human experience and consciousness. The 'visible' is part of a broader modality of sensation, memory, and thought.
constraint_indexing:constraint_classification(ulysses_chp03, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp03_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(ulysses_chp03, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp03, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ulysses_chp03, ExtMetricName, E),
    domain_priors:suppression_score(ulysses_chp03, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ulysses_chp03),
    narrative_ontology:constraint_metric(ulysses_chp03, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ulysses_chp03, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ulysses_chp03_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low, as the modality is a fundamental aspect of existence, not an imposed constraint. Suppression (0.03): Very low, as there is little active force preventing escape, only intrinsic limitation. Theater ratio (0.1): Virtually no performative aspect.
 *
 * PERSPECTIVAL GAP:
 *   Both perspectives acknowledge the ineluctable nature. Stephen experiences it directly, while philosophy recognizes it as a principle.
 *
 * DIRECTIONALITY LOGIC:
 *   Stephen, as powerless/trapped, bears costs. The 'Visible', as analytical/analytical, has no directionality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp03, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
