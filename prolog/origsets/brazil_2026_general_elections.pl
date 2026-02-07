% ============================================================================
% CONSTRAINT STORY: brazil_2026_general_elections
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: January 27, 2026
% ============================================================================

:- module(brazil_2026_general_elections, []).

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
* * constraint_id: brazil_2026_general_elections


* human_readable: 2026 Brazilian General Election Structure
* domain: political
* * SUMMARY:


* The 2026 elections in Brazil operate as a rigid structural constraint defined
* by the "Bolsonarismo vs. Lulismo" binary. While the incumbent (Lula) seeks
* re-election, the judicial disqualification of Jair Bolsonaro has forced the
* Right into a "Piton" state of inertial grievance, while the "Centrão" acts as
* a Tangled Rope extracting state resources for legislative cooperation.
* * KEY AGENTS:


* * The "Polarized Voter": Subject (Powerless)


* * The "Centrão" Bloc: Beneficiary (Institutional)


* * STF (Supreme Federal Court): Auditor/Enforcer (Analytical)
*/



/* ==========================================================================
2. BASE PROPERTIES (DOMAIN PRIORS)
========================================================================== */

% Base extractiveness: High (0.52)
% Rationale: High due to the "Budgetary Secret" (Emendas de Relator) and
% institutionalized extraction by the Centrão to maintain the electoral status quo.
domain_priors:base_extractiveness(brazil_2026_general_elections, 0.52).

% Suppression score: Moderate (0.42)
% Rationale: Judicial "Ineligibility" rulings and strict party fidelity laws
% limit the emergence of "Third Way" (Terceira Via) candidates.
domain_priors:suppression_score(brazil_2026_general_elections, 0.42).

% Theater Ratio: High (0.75) for the Right-wing mobilization
% Rationale: Much of the conservative campaigning focuses on a candidate (Bolsonaro)
% who is legally barred, creating a "Piton" effect of inertial, theatrical resistance.
domain_priors:theater_ratio(brazil_2026_general_elections, 0.75).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(brazil_2026_general_elections, extractiveness, 0.52).
narrative_ontology:constraint_metric(brazil_2026_general_elections, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(brazil_2026_general_elections, theater_ratio, 0.75).

domain_priors:requires_active_enforcement(brazil_2026_general_elections).

/* ==========================================================================
3. INDEXED CLASSIFICATIONS (P, T, E, S)
========================================================================== */

% PERSPECTIVE 1: THE DISILLUSIONED VOTER (SNARE)
% High extraction (taxation vs services) felt as a predatory trap with no exit.
constraint_indexing:constraint_classification(brazil_2026_general_elections, snare,
context(agent_power(powerless),
time_horizon(biographical),
exit_options(trapped),
spatial_scope(national))).

% PERSPECTIVE 2: THE "CENTRÃO" LEADERSHIP (ROPE)
% Viewed as the essential coordination mechanism for governability.
constraint_indexing:constraint_classification(brazil_2026_general_elections, rope,
context(agent_power(institutional),
time_horizon(generational),
exit_options(mobile),
spatial_scope(national))).

% PERSPECTIVE 3: THE POLITICAL SCIENTIST (TANGLED ROPE)
% Detects the hybrid nature of democratic coordination used for asymmetric extraction.
constraint_indexing:constraint_classification(brazil_2026_general_elections, tangled_rope,
context(agent_power(analytical),
time_horizon(historical),
exit_options(analytical),
spatial_scope(global))) :-
domain_priors:base_extractiveness(brazil_2026_general_elections, E), E >= 0.50,
domain_priors:suppression_score(brazil_2026_general_elections, S), S > 0.40.

% PERSPECTIVE 4: THE BOLSONARISTA BASE (PITON)
% Maintenance of a movement whose primary functional lead (the candidate) is inert.
constraint_indexing:constraint_classification(brazil_2026_general_elections, piton,
context(agent_power(organized),
time_horizon(biographical),
exit_options(constrained),
spatial_scope(national))) :-
domain_priors:theater_ratio(brazil_2026_general_elections, TR), TR > 0.70.

/* ==========================================================================
4. VALIDATION TESTS
========================================================================== */

:- begin_tests(brazil_2026_tests).

test(perspectival_gap) :-
% Verify the constraint is a Snare for the powerless but a Rope for the institution.
constraint_indexing:constraint_classification(brazil_2026_general_elections, snare, context(agent_power(powerless), _, _, _)),
constraint_indexing:constraint_classification(brazil_2026_general_elections, rope, context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
domain_priors:base_extractiveness(brazil_2026_general_elections, E),
(E =< 0.05 -> true ; E >= 0.46). % Passes v3.4 threshold for high-extraction logic.

test(piton_detection) :-
domain_priors:theater_ratio(brazil_2026_general_elections, TR),

TR > 0.70,
constraint_indexing:constraint_classification(brazil_2026_general_elections, piton, _).

:- end_tests(brazil_2026_tests).

/* ==========================================================================
5. GENERATIVE COMMENTARY
========================================================================== */

/**

* LOGIC RATIONALE:
* The 0.52 extractiveness reflects the "cost of governability" in Brazil,
* where the executive must cede vast budgetary control to the legislative
* center. The Perspectival Gap exists because the "Institutional" agents
* (Centrão) see this as a Rope (coordination), while the "Powerless" see
* it as a Snare (resource drain without representation).
* * MANDATROPHY RESOLUTION:


* [RESOLVED MANDATROPHY]
* The Tangled Rope classification is applied here because while the system
* is highly extractive (E=0.52), it still successfully performs the "Rope"
* function of preventing total institutional collapse or civil conflict
* through high-cost coordination.
*/

/* ==========================================================================
6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
omega_brazil_eligibility,
'Will the Supreme Court (STF) maintain the ineligibility of the opposition leader?',
'Judicial review of late 2025 appeals.',
'If False, Piton reverts to Rope; if True, Snare risk for the Right increases.',
confidence_without_resolution(medium)
).

/* ==========================================================================
7. INTEGRATION HOOKS
========================================================================== */

% Required for external script parsing
narrative_ontology:interval(brazil_2026_general_elections, 0, 10).

/* ==========================================================================
END OF CONSTRAINT STORY
========================================================================== */
