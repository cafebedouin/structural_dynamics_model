% ============================================================================
% CONSTRAINT STORY: bip_narrative_illusion
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-07
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_bip_narrative_illusion, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
domain_priors:base_extractiveness/2,
domain_priors:suppression_score/2,
domain_priors:theater_ratio/2,
domain_priors:requires_active_enforcement/1,
narrative_ontology:interval/3,
narrative_ontology:measurement/5,
narrative_ontology:constraint_metric/3,
narrative_ontology:constraint_beneficiary/2,
narrative_ontology:constraint_victim/2,
constraint_indexing:constraint_classification/3.

/* ==========================================================================

1. NARRATIVE CONTEXT
========================================================================== */

/**

* CONSTRAINT IDENTIFICATION
* * constraint_id: bip_narrative_illusion


* human_readable: The Black Iron Prison (BIP) and Sensory Optimization
* domain: philosophical/social/religious
* * SUMMARY:


* This constraint defines the world as a "Black Iron Prison" (BIP)—a "puny cell"
* created by malevolent elites and corporations that keep humans "slaves to Empire".
* It operates through a "bombardment of sensory stimulus" that forces individuals
* to cultivate an "analytical understanding" of the world, optimizing behaviors
* to "get things done" while suppressing the "good heart".
* * KEY AGENTS:


* * The Worldly Person: Subject (Powerless)


* * The Corporate Elite: Beneficiary (Institutional)


* * The Spiritual Seeker: Auditor (Analytical)
*/



/* ==========================================================================
2. BASE PROPERTIES (DOMAIN PRIORS)
========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(bip_narrative_illusion, 0.85).
domain_priors:suppression_score(bip_narrative_illusion, 0.75).

domain_priors:theater_ratio(bip_narrative_illusion, 0.20).

% Constraint metric facts for the classification engine
narrative_ontology:constraint_metric(bip_narrative_illusion, extractiveness, 0.85).
narrative_ontology:constraint_metric(bip_narrative_illusion, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(bip_narrative_illusion, theater_ratio, 0.20).

% Binary flags
domain_priors:requires_active_enforcement(bip_narrative_illusion).

% Structural property derivation hooks
narrative_ontology:constraint_beneficiary(bip_narrative_illusion, business_leaders_and_shareholders).
narrative_ontology:constraint_victim(bip_narrative_illusion, the_good_heart).
narrative_ontology:constraint_victim(bip_narrative_illusion, lived_experience).

/* ==========================================================================
3. INDEXED CLASSIFICATIONS (P, T, E, S)
========================================================================== */

% PERSPECTIVE 1: THE WORLDLY PERSON (SNARE)
% Chained to sensory stimulus and "doing and thinking," experiencing high extraction.
constraint_indexing:constraint_classification(bip_narrative_illusion, snare,
context(agent_power(powerless),
time_horizon(biographical),
exit_options(trapped),
spatial_scope(local))).

% PERSPECTIVE 2: THE CORPORATE ELITE (ROPE)
% Viewed as essential infrastructure for managing the "college of corporations."
constraint_indexing:constraint_classification(bip_narrative_illusion, rope,
context(agent_power(institutional),
time_horizon(historical),
exit_options(mobile),
spatial_scope(global))).

% PERSPECTIVE 3: THE SPIRITUAL SEEKER (MOUNTAIN)
% Views the entire system as an insubstantial, unchangeable dream or illusion.
constraint_indexing:constraint_classification(bip_narrative_illusion, mountain,
context(agent_power(analytical),
time_horizon(civilizational),
exit_options(analytical),
spatial_scope(global))).

/* ==========================================================================
4. VALIDATION TESTS
========================================================================== */

:- begin_tests(bip_narrative_illusion_tests).

test(perspectival_gap) :-
constraint_indexing:constraint_classification(bip_narrative_illusion, snare, context(agent_power(powerless), _, _, _)),
constraint_indexing:constraint_classification(bip_narrative_illusion, rope, context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
narrative_ontology:constraint_metric(bip_narrative_illusion, extractiveness, E),
E >= 0.46.

:- end_tests(bip_narrative_illusion_tests).

/* ==========================================================================
5. GENERATIVE COMMENTARY
========================================================================== */

/**

* LOGIC RATIONALE:
* Base extraction is set high (0.85) because the system extracts the internal
* "Being" and "Heart" to serve profit-driven "doing". The perspectival gap
* exists because the Subject is trapped in the sensory "puny cell," while the
* Beneficiary uses this same cell as a tool for global economic coordination.
* * MANDATROPHY ANALYSIS:


* [RESOLVED MANDATROPHY] - The high extraction is not purely predatory; it is
* the dark side of a highly efficient "Rope" of global corporate coordination.
* The "Snare" arises because the system suppresses spiritual alternatives.
*/

/* ==========================================================================
6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
========================================================================== */

omega_variable(
omega_bip_intent,
'Is sensory bombardment an intentional strategy or a natural byproduct?',
'Resource audit of behavior optimization vs human well-being.',
'If intentional: Snare. If byproduct: Evolutionary Mountain.',
confidence_without_resolution(medium)
).

/* ==========================================================================
7. INTEGRATION HOOKS
========================================================================== */

narrative_ontology:interval(bip_narrative_illusion, 0, 10).

/* ==========================================================================
8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
========================================================================== */

% Theater ratio over time (Goodhart drift check)
narrative_ontology:measurement(bip_tr_t0, bip_narrative_illusion, theater_ratio, 0, 0.05).
narrative_ontology:measurement(bip_tr_t5, bip_narrative_illusion, theater_ratio, 5, 0.12).
narrative_ontology:measurement(bip_tr_t10, bip_narrative_illusion, theater_ratio, 10, 0.20).

% Extraction over time (Extraction accumulation check)
narrative_ontology:measurement(bip_ex_t0, bip_narrative_illusion, base_extractiveness, 0, 0.70).
narrative_ontology:measurement(bip_ex_t5, bip_narrative_illusion, base_extractiveness, 5, 0.78).
narrative_ontology:measurement(bip_ex_t10, bip_narrative_illusion, base_extractiveness, 10, 0.85).

/* ==========================================================================
END OF CONSTRAINT STORY
========================================================================== */
