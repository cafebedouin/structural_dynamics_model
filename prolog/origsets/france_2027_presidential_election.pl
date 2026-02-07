% ============================================================================
% CONSTRAINT STORY: france_2027_presidential_election
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: January 27, 2026
% ============================================================================

:- module(france_2027_presidential_election, []).

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
    narrative_ontology:constraint_metric/3,
constraint_indexing:constraint_classification/3,
narrative_ontology:interval/3.

/* ==========================================================================

1. NARRATIVE CONTEXT
========================================================================== */

/**

* CONSTRAINT IDENTIFICATION
* * constraint_id: france_2027_presidential_election


* human_readable: 2027 French Presidential Election Framework
* domain: political
* * SUMMARY:


* As of January 2026, the 2027 election is defined by a "Republican Barrier"
* that is structurally thinning. With President Macron ineligible
* and the far-right (Bardella/Le Pen) consistently polling at 35-37%,
* the constitutional 500-endorsement requirement and the two-round system
* act as a Scaffold for the center while appearing as a Snare to the Left.
* * KEY AGENTS:


* * Jordan Bardella: National Rally President and current frontrunner.


* * The "Left-Wing Primary": A coordination attempt by PS/Greens (launched Jan 24, 2026).


* * The French Voter: Facing high fiscal deficits (5.8%) and political gridlock.
*/



/* ==========================================================================
2. BASE PROPERTIES (DOMAIN PRIORS)
========================================================================== */

% Base extractiveness: High (0.48)
% Rationale: High fiscal pressure (112% debt-to-GDP) and the "cost of gridlock"
% where government struggles to pass budgets.
domain_priors:base_extractiveness(france_2027_presidential_election, 0.48).

% Suppression score: Moderate (0.38)
% Rationale: The 500-mayoral endorsement rule acts as a "soft" suppression
% for non-institutional candidates.
domain_priors:suppression_score(france_2027_presidential_election, 0.38).

% Theater Ratio: Low-Moderate (0.40)
% Rationale: Unlike Brazil's inertial Piton, France's current legal appeals
% (e.g., Le Pen's trial) are active and determine candidate viability.
domain_priors:theater_ratio(france_2027_presidential_election, 0.40).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(france_2027_presidential_election, extractiveness, 0.48).
narrative_ontology:constraint_metric(france_2027_presidential_election, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(france_2027_presidential_election, theater_ratio, 0.4).

domain_priors:requires_active_enforcement(france_2027_presidential_election).

/* ==========================================================================
3. INDEXED CLASSIFICATIONS (P, T, E, S)
========================================================================== */

% PERSPECTIVE 1: THE DISENCHANTED VOTER (MOUNTAIN)
% Immutability of the "system" regardless of the candidate chosen.
constraint_indexing:constraint_classification(france_2027_presidential_election, mountain,
context(agent_power(powerless),
time_horizon(biographical),
exit_options(trapped),
spatial_scope(national))).

% PERSPECTIVE 2: THE INSTITUTIONAL CENTRIST (ROPE)
% Coordination tool to maintain the "Republic" against extremes.
constraint_indexing:constraint_classification(france_2027_presidential_election, rope,
context(agent_power(institutional),
time_horizon(generational),
exit_options(mobile),
spatial_scope(national))).

% PERSPECTIVE 3: THE LEFT-WING PRIMARY ORGANIZER (SCAFFOLD)
% Temporary support structure to build unity before the sunset of the first round.
constraint_indexing:constraint_classification(france_2027_presidential_election, scaffold,
context(agent_power(organized),
time_horizon(immediate),
exit_options(constrained),
spatial_scope(national))) :-
narrative_ontology:has_sunset_clause(france_2027_presidential_election).

% PERSPECTIVE 4: THE POLITICAL SCIENTIST (TANGLED ROPE)
% Detects hybrid coordination (voter choice) vs extraction (fiscal gridlock).
constraint_indexing:constraint_classification(france_2027_presidential_election, tangled_rope,
context(agent_power(analytical),
time_horizon(historical),
exit_options(analytical),
spatial_scope(global))) :-
domain_priors:base_extractiveness(france_2027_presidential_election, E), E >= 0.46.

/* ==========================================================================
4. VALIDATION TESTS
========================================================================== */

:- begin_tests(france_2027_tests).

test(perspectival_gap) :-
% Verify variance between the powerless (Mountain) and the institutional (Rope).
constraint_indexing:constraint_classification(france_2027_presidential_election, mountain, context(agent_power(powerless), _, _, _)),
constraint_indexing:constraint_classification(france_2027_presidential_election, rope, context(agent_power(institutional), _, _, _)).

test(extraction_threshold) :-
domain_priors:base_extractiveness(france_2027_presidential_election, E),

E >= 0.46. % Correctly flags for Tangled Rope / Snare logic in v3.4.

:- end_tests(france_2027_tests).

/* ==========================================================================
5. GENERATIVE COMMENTARY
========================================================================== */

/**

* LOGIC RATIONALE:
* The classification of 'Mountain' for the powerless reflects the "ceiling"
* effect of the two-round system. For the analytical observer, this is a
* 'Tangled Rope' because the electoral mechanism effectively coordinates
* the nation but fails to extract it from the fiscal gridlock (5.8% deficit).
* * MANDATROPHY RESOLUTION:


* [RESOLVED MANDATROPHY]
* Coordination is maintained through the 'Republican Front' logic,
* preventing the system from collapsing into a pure Snare, even as
* extraction levels remain high due to institutional debt.
*/

/* ==========================================================================
6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
omega_france_le_pen_eligibility,
'Will the appeal court overturn Marine Le Pen\'s 5-year eligibility ban?',
'Court ruling expected before Summer 2026.',
'If Upheld: Bardella becomes the sole RN standard-bearer. If Overturned: Rivalry risk.',
confidence_without_resolution(high)
).

/* ==========================================================================
7. INTEGRATION HOOKS
========================================================================== */

% Required for external script parsing
narrative_ontology:interval(france_2027_presidential_election, 0, 10).
narrative_ontology:has_sunset_clause(france_2027_presidential_election). % Mandatory for Scaffold classification

/* ==========================================================================
END OF CONSTRAINT STORY
========================================================================== */
