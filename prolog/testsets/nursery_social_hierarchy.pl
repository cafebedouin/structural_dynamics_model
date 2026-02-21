% ============================================================================
% CONSTRAINT STORY: nursery_social_hierarchy
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-19
% ============================================================================

:- module(constraint_nursery_social_hierarchy, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nursery_social_hierarchy
 *   human_readable: Social Hierarchy of Playthings
 *   domain: social/economic
 *
 * SUMMARY:
 *   A status-based framework within the nursery where mechanical complexity
 *   is equated with "reality." This system extracts dignity from simple toys
 *   to fuel the social dominance of modern, technical objects.
 *
 * KEY AGENTS (by structural relationship):
 *   - Velveteen Rabbit (Plush Toys): Primary target (powerless/trapped) — bears mockery
 *   - Mechanical Toys: Primary beneficiaries (powerful/mobile) — maintain superiority
 *   - Nursery Owner: Institutional overseer (institutional/arbitrage) — sees a self-organizing system
 *   - Skin Horse: Analytical observer — sees through the "technical" theater
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nursery_social_hierarchy, 0.62). % High status extraction
domain_priors:suppression_score(nursery_social_hierarchy, 0.40).   % Social exclusion of non-moderns
domain_priors:theater_ratio(nursery_social_hierarchy, 0.65).       % High use of technical jargon as theater

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nursery_social_hierarchy, extractiveness, 0.62).
narrative_ontology:constraint_metric(nursery_social_hierarchy, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(nursery_social_hierarchy, theater_ratio, 0.65).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(nursery_social_hierarchy, snare).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(nursery_social_hierarchy, mechanical_toys).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(nursery_social_hierarchy, plush_toys).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE VELVETEEN RABBIT (SNARE)
% A system of total insignificance and mockery.
% Derived d from victim status + trapped exit -> d ≈ 0.95 -> high chi
constraint_indexing:constraint_classification(nursery_social_hierarchy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE MECHANICAL TOYS (ROPE)
% Validates their complexity; allows coordination of dominance.
% Derived d from beneficiary status + mobile exit -> d ≈ 0.15 -> low chi
constraint_indexing:constraint_classification(nursery_social_hierarchy, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: THE NURSERY OWNER (ROPE)
% The institutional power that provides the toys and sets the environment.
% Sees the hierarchy as a self-organizing, low-maintenance system of order.
% Beneficiary status + arbitrage exit -> d ≈ 0.05 -> low/negative chi
constraint_indexing:constraint_classification(nursery_social_hierarchy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (SNARE)
% Sees the full extractive structure behind the social theater.
constraint_indexing:constraint_classification(nursery_social_hierarchy, snare,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nursery_social_hierarchy_tests).

test(perspectival_gap) :-
    % Target sees a Snare; Beneficiaries (powerful/institutional) see a Rope.
    constraint_indexing:constraint_classification(nursery_social_hierarchy, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nursery_social_hierarchy, rope, context(agent_power(powerful), _, _, _)),
    constraint_indexing:constraint_classification(nursery_social_hierarchy, rope, context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
    % Verify high extraction consistent with Snare classification.
    narrative_ontology:constraint_metric(nursery_social_hierarchy, extractiveness, E),
    E > 0.46.

test(theater_check) :-
    % Verify the system is hollowing out coordination through high theater.
    narrative_ontology:constraint_metric(nursery_social_hierarchy, theater_ratio, TR),
    TR > 0.50.

:- end_tests(nursery_social_hierarchy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The epsilon (0.62) reflects the high psychological extraction (dignity,
 *   status) directed at simple toys. This is exacerbated by the theater ratio
 *   (0.65), as mechanical toys use technical jargon and displays of complexity
 *   to obscure the lack of actual functional utility in their social posturing.
 *
 * PERSPECTIVAL GAP:
 *   The Velveteen Rabbit (powerless, trapped) experiences the hierarchy as a
 *   Snare, a system of inescapable social exclusion. The Mechanical Toys
 *   (powerful, mobile) and the Nursery Owner (institutional, arbitrage) see
 *   it as a Rope—a useful coordination mechanism for establishing order and
 *   validating their status. This gap is the core of the conflict.
 *
 * DIRECTIONALITY LOGIC:
 *   The "Modern" mechanical toys are declared beneficiaries as the hierarchy
 *   subsidizes their self-worth. The "Antique" or "Plush" toys are victims,
 *   bearing the cost of this status extraction. This drives the directionality
 *   (d) to opposite ends of the spectrum for these groups, creating the
 *   Snare/Rope perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the hierarchy as a harmless
 *   game (Rope). The perspectival gap, driven by high extraction (ε=0.62),
 *   highlights that status is not shared coordination but zero-sum extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_mechanical_obsolescence,
    'Does the Snare collapse into a Piton once the mechanical parts fail?',
    'Observation of broken clockwork toys maintaining high status in the hierarchy.',
    'If status persists after function is lost, it proves the hierarchy is purely theatrical (Piton). If status collapses, it was tied to function (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(omega_mechanical_obsolescence, empirical, 'Transition from Snare to Piton upon functional failure of mechanical toys.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nursery_social_hierarchy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. Required for high-extraction
% constraints (base_extractiveness > 0.46). The hierarchy intensified over
% time as the "modern" toys asserted dominance and the rules became more rigid.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(nursery_social_hierarchy_tr_t0, nursery_social_hierarchy, theater_ratio, 0, 0.30).
narrative_ontology:measurement(nursery_social_hierarchy_tr_t5, nursery_social_hierarchy, theater_ratio, 5, 0.50).
narrative_ontology:measurement(nursery_social_hierarchy_tr_t10, nursery_social_hierarchy, theater_ratio, 10, 0.65).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(nursery_social_hierarchy_ex_t0, nursery_social_hierarchy, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(nursery_social_hierarchy_ex_t5, nursery_social_hierarchy, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(nursery_social_hierarchy_ex_t10, nursery_social_hierarchy, base_extractiveness, 10, 0.62).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Network: This social hierarchy provides the "negative motivation" that
% drives the Rabbit toward the devotional_transformation.
narrative_ontology:affects_constraint(nursery_social_hierarchy, devotional_transformation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */