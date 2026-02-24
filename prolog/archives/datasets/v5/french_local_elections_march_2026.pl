% ============================================================================
% CONSTRAINT STORY: france_local_elections_march_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: January 27, 2026
% ============================================================================

:- module(france_local_elections_march_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
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
* * constraint_id: france_local_elections_march_2026


* human_readable: March 2026 French Municipal Elections (The Scaffold)
* domain: political
* * SUMMARY:


* The March 15 and 22, 2026, municipal elections serve as a temporary
* structural "Scaffold". They provide a necessary
* support framework for political realignment, specifically the "Left-Wing
* Primary" (launched Jan 24, 2026) and the "Republican Front," which must
* hold until the post-election sunset.
* * KEY AGENTS:


* * Emmanuel Grégoire: Center-Left architect in Paris (PS, Greens, PCF support).


* * Sophia Chikirou: LFI challenger testing the strength of the radical left.


* * Rachida Dati: High-power institutional candidate (LR/Modem/UDI).
*/



/* ==========================================================================
2. BASE PROPERTIES (DOMAIN PRIORS)
========================================================================== */

% Base extractiveness: Low-Moderate (0.35)
% Rationale: Local elections remain high-coordination events, but the
% "Majority Premium" (25% seat bonus) extracts representation for the winner.
domain_priors:base_extractiveness(france_local_elections_march_2026, 0.35).

% Suppression score: Moderate (0.45)
% Rationale: The new list system for small towns (<1,000 residents) suppresses
% individual/independent candidates in favor of party-list coordination.
domain_priors:suppression_score(france_local_elections_march_2026, 0.45).

% Theater Ratio: Moderate (0.30)
% Rationale: Real policy stakes (housing, security, and transport) prevent
% this from being a pure Piton.
domain_priors:theater_ratio(france_local_elections_march_2026, 0.30).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(france_local_elections_march_2026, extractiveness, 0.35).
narrative_ontology:constraint_metric(france_local_elections_march_2026, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(france_local_elections_march_2026, theater_ratio, 0.3).

domain_priors:requires_active_enforcement(france_local_elections_march_2026).

/* ==========================================================================
3. INDEXED CLASSIFICATIONS (P, T, E, S)
========================================================================== */

% PERSPECTIVE 1: THE LOCAL PRIMARY ORGANIZER (SCAFFOLD)
% Temporary coordination that sunsets immediately after the second round.
constraint_indexing:constraint_classification(france_local_elections_march_2026, scaffold,
context(agent_power(organized),
time_horizon(immediate),
exit_options(constrained),
spatial_scope(national))) :-
narrative_ontology:has_sunset_clause(france_local_elections_march_2026).

% PERSPECTIVE 2: THE INSTITUTIONAL PARTY (ROPE)
% Coordination tool for "counting mayors" and building the 2027 base.
constraint_indexing:constraint_classification(france_local_elections_march_2026, rope,
context(agent_power(institutional),
time_horizon(generational),
exit_options(mobile),
spatial_scope(national))).

% PERSPECTIVE 3: THE MARGINALIZED INDEPENDENT (SNARE)
% The new mandatory list system blocks individual entries in small communes.
constraint_indexing:constraint_classification(france_local_elections_march_2026, snare,
context(agent_power(powerless),
time_horizon(biographical),
exit_options(trapped),
spatial_scope(local))).

/* ==========================================================================
4. VALIDATION TESTS
========================================================================== */

:- begin_tests(france_2026_scaffold_tests).

test(scaffold_sunset) :-
% Ensure Scaffold classification is valid under the sunset clause.
narrative_ontology:has_sunset_clause(france_local_elections_march_2026),
constraint_indexing:constraint_classification(france_local_elections_march_2026, scaffold, _).

test(list_system_snare) :-
% Verify that the powerless in local scopes see the new voting rules as a Snare.
constraint_indexing:constraint_classification(france_local_elections_march_2026, snare,
context(agent_power(powerless), _, _, spatial_scope(local))).

:- end_tests(france_2026_scaffold_tests).

/* ==========================================================================
5. GENERATIVE COMMENTARY
========================================================================== */

/**

* LOGIC RATIONALE:
* The 2026 local elections are the ultimate 'Scaffold' because they provide
* the 500-mayoral endorsements required for the 2027 primary.
* Once the second round (March 22) concludes, the coordination lists
* (like 'New Popular Paris') will either dissolve or harden into
* presidential machines.
* * MANDATROPHY RESOLUTION:


* [RESOLVED MANDATROPHY]
* The system avoids 'Piton' status because of the high local policy weight.
* However, the 'Tangled Rope' risk is high in major cities (Paris/Lyon/Marseille)
* due to the new dual-ballot system (district + city council).
*/

/* ==========================================================================
6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
omega_left_unity_collapse,
'Will LFI maintain a separate list in the second round in Paris?',
'Second-round alliance results (March 16-22, 2026).',
'If True, right-wing (Dati) victory probability increases; if False, Left wins.',
confidence_without_resolution(medium)
).

/* ==========================================================================
7. INTEGRATION HOOKS
========================================================================== */

narrative_ontology:interval(france_local_elections_march_2026, 0, 10).
narrative_ontology:has_sunset_clause(france_local_elections_march_2026).

/* ==========================================================================
END OF CONSTRAINT STORY
========================================================================== */
