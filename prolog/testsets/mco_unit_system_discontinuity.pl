% ============================================================================
% CONSTRAINT STORY: mco_unit_system_discontinuity
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_mco_unit_system_discontinuity, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: mco_unit_system_discontinuity
 * human_readable: Persistence of Imperial Units in a Metric World
 * domain: technological/institutional
 * * SUMMARY:
 * The continued use of the Imperial unit system in a global scientific
 * community that has standardized on Metric is a classic Piton. It is a
 * legacy coordination system (a Rope) that has lost its primary function but
 * persists through cultural inertia and the cost of refactoring legacy systems.
 * This creates constant friction, risk, and catastrophic failures, like the
 * Mars Climate Orbiter loss, which serves as the canonical example of this
 * Piton failing under load.
 * * KEY AGENTS:
 * - Navigation Engineer (JPL): Subject (Powerless); subject to the data
 * provided by the upstream software interface.
 * - Software Architect (Lockheed): Beneficiary (Institutional); established the original
 * coding standards and unit choices, benefiting from not having to refactor.
 * - Mishap Investigation Board: Auditor (Analytical); post-hoc observer identifying
 * the systemic failure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Rationale (Extractiveness): 0.3. Low-to-moderate. The constraint extracts cognitive energy
% through constant conversion requirements and introduces a high risk of
% catastrophic failure (like the MCO loss), but does not perform active, continuous extraction.
domain_priors:base_extractiveness(mco_unit_system_discontinuity, 0.3).
% Rationale (Suppression): 0.1. Very low. The Metric system is not actively suppressed;
% the Imperial system simply persists through cultural and educational inertia.
domain_priors:suppression_score(mco_unit_system_discontinuity, 0.1).
% Rationale (Theater): 0.85. High. The maintenance of Imperial units in this context is
% almost entirely performative or inertial. It serves no superior function to the global
% standard and its continued use is a form of institutional theater.
domain_priors:theater_ratio(mco_unit_system_discontinuity, 0.85).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(mco_unit_system_discontinuity, extractiveness, 0.3).
narrative_ontology:constraint_metric(mco_unit_system_discontinuity, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(mco_unit_system_discontinuity, theater_ratio, 0.85).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a valid coordination standard, a constructed system.
narrative_ontology:constraint_claim(mco_unit_system_discontinuity, piton).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(mco_unit_system_discontinuity, us_aerospace_legacy_tooling).
narrative_ontology:constraint_victim(mco_unit_system_discontinuity, planetary_science_community).
narrative_ontology:constraint_victim(mco_unit_system_discontinuity, us_taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (JPL NAVIGATION ENGINEER)
% To the engineer receiving the data, the software interface is a Mountain.
% They assume the "Force" value provided is in the standard SI units
% required by their system. There is no "choice" to interpret the number
% differently; the data is presented as an immutable fact.
constraint_indexing:constraint_classification(mco_unit_system_discontinuity, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (NASA PROJECT MANAGEMENT)
% Management views the Interface Control Document (ICD) as a Rope—a
% functional coordination mechanism that allows two different
% organizations to work on the same hardware. They believe the
% "system" is coordinating the teams effectively through these documents.
constraint_indexing:constraint_classification(mco_unit_system_discontinuity, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MISHAP INVESTIGATION BOARD)
% The Board identifies the use of Imperial units as a Piton. It was a
% legacy Rope that lost its function in a global, metric-first scientific
% project. Its persistence through inertia (high theater_ratio) created a
% high-risk, low-benefit situation that ultimately failed under load.
constraint_indexing:constraint_classification(mco_unit_system_discontinuity, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mco_unit_system_discontinuity_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless, institutional, and analytical.
    constraint_indexing:constraint_classification(mco_unit_system_discontinuity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mco_unit_system_discontinuity, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(mco_unit_system_discontinuity, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypePowerless == mountain,
    TypeInstitutional == rope,
    TypeAnalytical == piton,
    TypePowerless \= TypeInstitutional,
    TypeInstitutional \= TypeAnalytical.

test(piton_threshold_validation) :-
    % Verify the analytical observer correctly identifies a Piton, which requires a high theater ratio.
    narrative_ontology:constraint_metric(mco_unit_system_discontinuity, theater_ratio, TR),
    TR >= 0.70,
    constraint_indexing:constraint_classification(mco_unit_system_discontinuity, piton, context(agent_power(analytical), _, _, _)).

:- end_tests(mco_unit_system_discontinuity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint is a canonical example of a Piton. The core of the analysis rests on the
 * high theater_ratio (0.85), representing the persistence of an obsolete standard (Imperial units)
 * in a context where a superior alternative (Metric) is the global norm.
 *
 * The base extractiveness (0.3) is low because the system doesn't actively take resources day-to-day;
 * instead, it imposes cognitive friction and creates the *risk* of catastrophic, high-extraction failure.
 * Suppression (0.1) is minimal as no one is actively preventing the use of Metric.
 *
 * The Perspectival Gap is stark and informative:
 * - The powerless Engineer sees the incorrect data as an immutable fact (Mountain).
 * - The institutional Manager sees the flawed process documents as functional coordination (Rope).
 * - The analytical Investigator, with a historical view, sees the obsolete, inertial system for what it is: a Piton.
 * This divergence is key to understanding how such failures can occur in complex systems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_mco_unit_system_discontinuity,
    'Was the failure primarily due to cultural inertia (Piton) or active budget suppression of verification tools (Snare)?',
    'Review of internal project memos from the "Faster, Better, Cheaper" era to find evidence of denied requests for end-to-end testing.',
    'If due to inertia, it remains a Piton. If due to active suppression of known solutions for cost reasons, it would be reclassified as a Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(mco_unit_system_discontinuity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not required for this constraint as base_extractiveness (0.3)
% is below the 0.46 threshold for mandatory lifecycle drift monitoring.

/*
narrative_ontology:measurement(mco_unit_system_discontinuity_tr_t0, mco_unit_system_discontinuity, theater_ratio, 0, 0.0).
narrative_ontology:measurement(mco_unit_system_discontinuity_tr_t5, mco_unit_system_discontinuity, theater_ratio, 5, 0.0).
narrative_ontology:measurement(mco_unit_system_discontinuity_tr_t10, mco_unit_system_discontinuity, theater_ratio, 10, 0.85).

narrative_ontology:measurement(mco_unit_system_discontinuity_ex_t0, mco_unit_system_discontinuity, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(mco_unit_system_discontinuity_ex_t5, mco_unit_system_discontinuity, base_extractiveness, 5, 0.0).
narrative_ontology:measurement(mco_unit_system_discontinuity_ex_t10, mco_unit_system_discontinuity, base_extractiveness, 10, 0.3).
*/

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% A system of units is a quintessential information standard.
narrative_ontology:coordination_type(mco_unit_system_discontinuity, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */