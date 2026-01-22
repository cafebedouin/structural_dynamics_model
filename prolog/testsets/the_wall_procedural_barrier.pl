% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: the_wall_2001
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini-911-Analysis
% Source: The 9/11 Commission Report
% ============================================================================

:- module(constraint_the_wall, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/5,
    narrative_ontology:constraint_beneficiary/3,
    narrative_ontology:constraint_victim/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: the_wall_2001
 * human_readable: The Intelligence/Law Enforcement Information Sharing Barrier
 * domain: legal/institutional
 * temporal_scope: 1995-2001 CE
 * spatial_scope: US Department of Justice / FBI / CIA
 * * SUMMARY:
 * "The Wall" refers to procedures restricting information sharing between 
 * intelligence investigators and criminal prosecutors. Initially 
 * intended to protect FISA legality, it became a pervasive barrier.
 * * KEY AGENTS:
 * - FBI_Field_Agent: Individual powerless; subject to rules.
 * - OIPR_Gatekeeper: Institutional; controlled flow of FISA data.
 * - CIA_Analyst: Individual moderate; operates in a "zone defense".
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Structural Anchor for System Extraction
narrative_ontology:interval(the_wall_2001, 1995, 2001).

% Base extractiveness score (0.0 - 1.0)
% Rationale: High asymmetry; prevented vital safety info sharing.
domain_priors:base_extractiveness(the_wall_2001, 0.8).

% Suppression score (0.0 - 1.0)
% Rationale: Actively enforced by OIPR threat of withholding FISA warrants.
domain_priors:suppression_score(the_wall_2001, 0.9).

% Enforcement: Requires active maintenance by OIPR
domain_priors:requires_active_enforcement(the_wall_2001).

% Beneficiaries and Victims
narrative_ontology:constraint_beneficiary(the_wall_2001, institutional_status_quo, 'Protection of FISA prosecutorial purity').
narrative_ontology:constraint_victim(the_wall_2001, public_safety, 'Inability to track 9/11 hijackers').
narrative_ontology:constraint_victim(the_wall_2001, fbi_field_agents, 'Exclusion from investigations').

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

% Institutional View: The Wall as a necessary coordination tool.
constraint_indexing:constraint_classification(
    the_wall_2001,
    rope,
    context(agent_power(institutional), time_horizon(biographical), exit_options(arbitrage), spatial_scope(national))
) :- !.

% Powerless View: The Wall as an unchangeable law (Mountain).
constraint_indexing:constraint_classification(
    the_wall_2001,
    mountain,
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))
) :- !.

% Analytical View: The Wall as a suppressive Noose.
constraint_indexing:constraint_classification(
    the_wall_2001,
    noose,
    context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))
) :- !.

/* ==========================================================================
   4. TESTS (Validation)
   ========================================================================== */

:- begin_tests(the_wall_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(the_wall_2001, Type1, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(the_wall_2001, Type2, context(agent_power(individual_powerless), _, _, _)),
    Type1 \= Type2.

:- end_tests(the_wall_tests).

/* ==========================================================================
   5. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

narrative_ontology:omega_variable(
    wall_rigidity_source,
    "Was the barrier's strength derived from FISA law or FBI risk-aversion?",
    resolution_mechanism("Legal audit vs. internal policy memos"),
    impact("Determines if resolution requires law or leadership change"),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * CONCLUSION:
 * The existence of a viable but suppressed sharing model 
 * confirms the "Noose" classification for the 2001 state of the Wall.
 */

intent_viable_alternative(the_wall_2001, transparency_1995, 'Coordinated transparency').
intent_alternative_rejected(the_wall_2001, transparency_1995, 'OIPR administrative gatekeeping').

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
