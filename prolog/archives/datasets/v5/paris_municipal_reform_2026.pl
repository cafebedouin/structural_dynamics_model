% ============================================================================
% CONSTRAINT STORY: paris_municipal_reform_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: January 27, 2026
% ============================================================================

:- module(paris_municipal_reform_2026, []).

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
========================================================================= */

/**

* CONSTRAINT IDENTIFICATION
* * constraint_id: paris_municipal_reform_2026


* human_readable: Paris Municipal Reform (Loi Maillard/PLM Reform)
* domain: political/legal
* * SUMMARY:


* The 2026 reform (Loi du 11 août 2025) fundamentally shifts the "Majority
* Premium" (Prime Majoritaire) from 50% down to 25% for the Council of Paris
. While intended to enhance proportionality and democratic
* visibility, the transition creates a "Tangled Rope" dynamic where
* coordination (direct election) is bundled with a new form of institutional
* extraction: the legislative fragmentation of the "Majority Premium".
* * KEY AGENTS:


* * The "Third-Place" List: Subject (Powerless/Moderate) - Gains from the


* reduction of the winner's premium.
* * The Incumbent Project (PS/Grégoire): Beneficiary (Institutional) - Must


* now navigate a significantly more fragmented council.
* * Sylvain Maillard: Architect/Auditor (Analytical) - Proponent of the


* "direct universal suffrage" shift.
*/

/* ==========================================================================
2. BASE PROPERTIES (DOMAIN PRIORS)
========================================================================= */

% Base extractiveness: Moderate-High (0.47)
% Rationale: The reduction of the Majority Premium to 25% (from 50%) technically
% "extracts" stability from the winning list to provide "democratic diversity"
% for smaller parties.
domain_priors:base_extractiveness(paris_municipal_reform_2026, 0.47).

% Suppression score: Low-Moderate (0.35)
% Rationale: The reform actually lowers suppression of minority voices by
% making 75% of seats proportional (up from 50%).
domain_priors:suppression_score(paris_municipal_reform_2026, 0.35).

% Theater Ratio: Low (0.25)
% Rationale: The stakes are concrete: the 163 seats of the Council of Paris
% will be distributed under a entirely new mathematical reality.
domain_priors:theater_ratio(paris_municipal_reform_2026, 0.25).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(paris_municipal_reform_2026, extractiveness, 0.47).
narrative_ontology:constraint_metric(paris_municipal_reform_2026, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(paris_municipal_reform_2026, theater_ratio, 0.25).

domain_priors:requires_active_enforcement(paris_municipal_reform_2026).

/* ==========================================================================
3. INDEXED CLASSIFICATIONS (P, T, E, S)
========================================================================= */

% PERSPECTIVE 1: THE MINORITY PARTY (ROPE)
% Viewed as a coordination tool that finally allows them a fair seat count.
constraint_indexing:constraint_classification(paris_municipal_reform_2026, rope,
context(agent_power(individual_moderate),
time_horizon(biographical),
exit_options(mobile),
spatial_scope(local))).

% PERSPECTIVE 2: THE COUNCIL ARCHITECT (TANGLED ROPE)
% Detects the hybrid extraction (of stability) and coordination (of voter intent).
constraint_indexing:constraint_classification(paris_municipal_reform_2026, tangled_rope,
context(agent_power(analytical),
time_horizon(generational),
exit_options(analytical),
spatial_scope(national))) :-
domain_priors:base_extractiveness(paris_municipal_reform_2026, E), E >= 0.46.

% PERSPECTIVE 3: THE MAJORITY LEADER (SNARE)
% The reduction to 25% is a "Snare" that makes the city potentially ungovernable
% without exhausting, extractive coalition-building.
constraint_indexing:constraint_classification(paris_municipal_reform_2026, snare,
context(agent_power(institutional),
time_horizon(immediate),
exit_options(trapped),
spatial_scope(national))).

/* ==========================================================================
4. VALIDATION TESTS
========================================================================= */

:- begin_tests(paris_reform_tests).

test(majority_premium_shift) :-
% Verify the 25% logic is captured as high-extraction from the winner's perspective.
domain_priors:base_extractiveness(paris_municipal_reform_2026, E),

E >= 0.46.

test(tangled_rope_detection) :-
% Analytical observer must see Tangled Rope due to hybrid E/S scores.
constraint_indexing:constraint_classification(paris_municipal_reform_2026, tangled_rope,
context(agent_power(analytical), _, _, _)).

:- end_tests(paris_reform_tests).

/* ==========================================================================
5. GENERATIVE COMMENTARY
========================================================================= */

/**

* LOGIC RATIONALE:
* The "Tangled Rope" classification is central here because the reform
* solves a coordination problem (voter confusion over indirect election)
* but introduces an "extractive" tax on governability. By cutting the
* Majority Premium in half (50% -> 25%), the law forces the winning
* party to "pay" in seats to achieve a broader democratic consensus.
* * MANDATROPHY RESOLUTION:


* [RESOLVED MANDATROPHY]
* The system remains a Rope for the electorate (higher fidelity) but
* risks becoming a Snare for the executive. The "Tangled" nature
* prevents the system from being mislabeled as pure coordination.
*/

/* ==========================================================================
6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
========================================================================= */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
omega_fragmentation_risk,
'Will the 25% premium be sufficient to form a stable majority in a 5-list runoff?',
'Simulation of seat distribution after the March 22, 2026 runoff.',
'If insufficient: Constant "49.3-style" gridlock. If sufficient: Successful Scaffold.',
confidence_without_resolution(low)
).

/* ==========================================================================
7. INTEGRATION HOOKS
========================================================================= */

narrative_ontology:interval(paris_municipal_reform_2026, 0, 10).

/* ==========================================================================
END OF CONSTRAINT STORY
========================================================================= */
