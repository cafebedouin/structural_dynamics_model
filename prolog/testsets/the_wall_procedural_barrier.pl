% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: THE_WALL_PROCEDURAL_BARRIER
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
    constraint_indexing:constraint_classification/3.

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
 * "The Wall" refers to a set of procedures and institutional interpretations 
 * that restricted the sharing of information between intelligence investigators 
 * [cite_start]and criminal prosecutors[cite: 52]. While originally intended to protect 
 * the "primary purpose" requirement of FISA warrants, it evolved into a 
 * pervasive barrier that prevented agents on the same squads from sharing 
 * [cite_start]information, even when such sharing was legally permissible[cite: 52].
 * * KEY AGENTS:
 * - [cite_start]FBI_Field_Agent: Individual powerless; subject to rules, fears career impact of "crossing the wall"[cite: 52].
 * - OIPR_Gatekeeper: Institutional; [cite_start]Office of Intelligence Policy and Review, controlled the flow of FISA data[cite: 52].
 * - [cite_start]CIA_Analyst: Individual moderate; operates in a "zone defense" (where/what) rather than "man-to-man" (who)[cite: 52].
 * * NARRATIVE ARC:
 * The barrier was formalized by Attorney General Reno in 1995 to manage 
 * [cite_start]information sharing[cite: 52]. Over time, it was "misunderstood and 
 * misapplied," becoming a structural "Noose" that prevented the FBI from 
 * finding hijackers Khalid al Mihdhar and Nawaf al Hazmi despite knowing 
 * [cite_start]they were in the country[cite: 52].
 */

/* ==========================================================================
   2. BASE PROPERTIES (Prolog Facts)
   ========================================================================== */

% Structural Anchor for Python extraction
narrative_ontology:interval(the_wall_2001, 1995, 2001).

% Base Domain Priors
domain_priors:base_extractiveness(the_wall_2001, 0.8). % High: prevented vital safety info sharing.
domain_priors:suppression_score(the_wall_2001, 0.9).   [cite_start]% High: actively enforced by OIPR threat[cite: 52].
domain_priors:requires_active_enforcement(the_wall_2001).

% Beneficiaries and Victims
constraint_beneficiary(the_wall_2001, institutional_status_quo, 'Protection of FISA legality/prosecutorial purity').
constraint_victim(the_wall_2001, public_safety, 'Inability to connect dots regarding 9/11 hijackers').
constraint_victim(the_wall_2001, fbi_field_agents, 'Frustration of mission and exclusion from investigations').

% Omegas (Ambiguities)
omega_variable(the_wall_2001, primary_purpose_definition, 'What constitutes "direction" vs "briefing" of a case?').
omega_variable(the_wall_2001, risk_aversion_coefficient, 'The degree to which fear of career damage suppressed sharing').

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Prolog Rules)
   ========================================================================== */

% Perspective 1: The OIPR / Institutional View (ROPE)
% They saw the wall as a necessary coordination mechanism to ensure convictions.
constraint_indexing:constraint_classification(the_wall_2001, institutional, rope) :-
    not(failed_prosecution_risk).

% Perspective 2: The FBI Field Agent View (MOUNTAIN)
% Agents believed the wall was a natural/legal law that could not be moved.
constraint_indexing:constraint_classification(the_wall_2001, individual_powerless, mountain) :-
    perceived_career_risk(high).

% Perspective 3: The Retrospective Analyst View (NOOSE)
% Functionally, it was an asymmetric mechanism that suppressed actionable intelligence.
constraint_indexing:constraint_classification(the_wall_2001, analytical, noose) :-
    information_sharing_blocked(true),
    extractiveness(E), E > 0.5.

/* ==========================================================================
   4. TESTS (Prolog Test Suite)
   ========================================================================== */

:- begin_tests(the_wall_tests).

test(mihdhar_search_failure) :-
    % Demonstrates how the Noose prevented action on Mihdhar in Aug 2001
    constraint_indexing:constraint_classification(the_wall_2001, analytical, noose),
    [cite_start]writeln('TEST PASSED: The Wall functioned as a Noose in the Mihdhar search case[cite: 52].').

test(perspectival_gap) :-
    % Checks if there is a gap between institutional and powerless perspectives
    constraint_indexing:constraint_classification(the_wall_2001, institutional, rope),
    constraint_indexing:constraint_classification(the_wall_2001, individual_powerless, mountain),
    writeln('TEST PASSED: Perspectival Gap detected between "Coordination Tool" and "Unchangeable Law".').

:- end_tests(the_wall_tests).

/* ==========================================================================
   5. GENERATING MODEL INTERPRETATION
   ========================================================================== */

/**
 * MODEL INTERPRETATION:
 * The "Wall" is a quintessential example of a "Rope" (coordination) turning 
 * into a "Noose" (extractive/suppressive) through bureaucratic drift. 
 * The 9/11 Report notes that procedures meant to "regulate" sharing became 
 * [cite_start]procedures that "withered" the flow of information[cite: 52]. 
 * * The most striking detail is the "career stopper" warning issued by FBI 
 * leadership, which transformed a legal policy into a psychological 
 * [cite_start]"Mountain" for agents[cite: 52]. This forced agents to "play zone 
 * defense" (the CIA approach) rather than the "man-to-man" approach needed 
 * [cite_start]to track specific individuals like Mihdhar[cite: 52].
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * THE REJECTED ALTERNATIVE: "Total Transparency"
 * * ALTERNATIVE 1: 1995 Full Information Sharing
 * Viability: Legally permissible; Reno's procedures allowed sharing if 
 * [cite_start]not for "direction"[cite: 52].
 * Suppression: Actively suppressed by OIPR threat to stop presenting 
 * [cite_start]FISA warrants to the Court[cite: 52].
 * Evidence: "The Office [OIPR] assumed the role [of sole gatekeeper] 
 * [cite_start]anyway... the information flow withered"[cite: 52].
 * * CONCLUSION:
 * The Wall was not a "Mountain" of natural law; it was a "Noose" tightened 
 * by administrative risk-aversion. Had the alternative of "coordinated 
 * transparency" been maintained, the hijackers might have been found 
 * [cite_start]via immigration violations or as material witnesses[cite: 52].
 */

% Alternative rule:
intent_viable_alternative(the_wall_2001, transparency_1995, 'Coordinated transparency with legal safeguards').
intent_alternative_rejected(the_wall_2001, transparency_1995, 'Fear of judicial sanction by the FISA Court').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */
% ============================================================================
% CONSTRAINT STORY: THE_WALL_PROCEDURAL_BARRIER
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
    constraint_indexing:constraint_classification/3.

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
 * "The Wall" refers to a set of procedures and institutional interpretations 
 * that restricted the sharing of information between intelligence investigators 
 * [cite_start]and criminal prosecutors[cite: 52]. While originally intended to protect 
 * the "primary purpose" requirement of FISA warrants, it evolved into a 
 * pervasive barrier that prevented agents on the same squads from sharing 
 * [cite_start]information, even when such sharing was legally permissible[cite: 52].
 * * KEY AGENTS:
 * - [cite_start]FBI_Field_Agent: Individual powerless; subject to rules, fears career impact of "crossing the wall"[cite: 52].
 * - OIPR_Gatekeeper: Institutional; [cite_start]Office of Intelligence Policy and Review, controlled the flow of FISA data[cite: 52].
 * - [cite_start]CIA_Analyst: Individual moderate; operates in a "zone defense" (where/what) rather than "man-to-man" (who)[cite: 52].
 * * NARRATIVE ARC:
 * The barrier was formalized by Attorney General Reno in 1995 to manage 
 * [cite_start]information sharing[cite: 52]. Over time, it was "misunderstood and 
 * misapplied," becoming a structural "Noose" that prevented the FBI from 
 * finding hijackers Khalid al Mihdhar and Nawaf al Hazmi despite knowing 
 * [cite_start]they were in the country[cite: 52].
 */

/* ==========================================================================
   2. BASE PROPERTIES (Prolog Facts)
   ========================================================================== */

% Structural Anchor for Python extraction
narrative_ontology:interval(the_wall_2001, 1995, 2001).

% Base Domain Priors
domain_priors:base_extractiveness(the_wall_2001, 0.8). % High: prevented vital safety info sharing.
domain_priors:suppression_score(the_wall_2001, 0.9).   % High: actively enforced by OIPR threat[cite: 52].
domain_priors:requires_active_enforcement(the_wall_2001).

% Beneficiaries and Victims
constraint_beneficiary(the_wall_2001, institutional_status_quo, 'Protection of FISA legality/prosecutorial purity').
constraint_victim(the_wall_2001, public_safety, 'Inability to connect dots regarding 9/11 hijackers').
constraint_victim(the_wall_2001, fbi_field_agents, 'Frustration of mission and exclusion from investigations').

% Omegas (Ambiguities)
omega_variable(the_wall_2001, primary_purpose_definition, 'What constitutes "direction" vs "briefing" of a case?').
omega_variable(the_wall_2001, risk_aversion_coefficient, 'The degree to which fear of career damage suppressed sharing').

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Prolog Rules)
   ========================================================================== */

% Perspective 1: The OIPR / Institutional View (ROPE)
% They saw the wall as a necessary coordination mechanism to ensure convictions.
constraint_indexing:constraint_classification(the_wall_2001, institutional, rope) :-
    not(failed_prosecution_risk).

% Perspective 2: The FBI Field Agent View (MOUNTAIN)
% Agents believed the wall was a natural/legal law that could not be moved.
constraint_indexing:constraint_classification(the_wall_2001, individual_powerless, mountain) :-
    perceived_career_risk(high).

% Perspective 3: The Retrospective Analyst View (NOOSE)
% Functionally, it was an asymmetric mechanism that suppressed actionable intelligence.
constraint_indexing:constraint_classification(the_wall_2001, analytical, noose) :-
    information_sharing_blocked(true),
    extractiveness(E), E > 0.5.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SYSTEMS ANALYST) - Mountain
   --------------------------------------------------------------------------
   WHO: agent_power(analytical) - Observes the unyielding procedural logic.
   WHEN: generational - The bureaucratic structure outlives its subjects.
   WHERE: trapped - Bound by the specific jurisdictional rules of the gate.
   SCOPE: global - The logic of the barrier applies to all who approach it.
   
   WHY THIS CLASSIFICATION:
   The "Mountain" is the procedural architecture itself. The sequence of 
   requirements, the necessity of specific documentation, and the fixed 
   latency of the processing cycle are immutable facts of the terrain. 
   While the subject feels this as a Noose and the clerk uses it as a 
   Rope, the analyst recognizes it as an unchangeable structural 
   invariant: the system is designed as a high-friction filter that 
   cannot be "negotiated" without satisfying its internal logic.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    the_wall_procedural_barrier,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(generational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :-
    domain_priors:requires_active_enforcement(the_wall_procedural_barrier),
    !.

% Explicit priors reflecting the rigid, high-friction nature of the barrier.
domain_priors:base_extractiveness(the_wall_procedural_barrier, 0.2).
domain_priors:suppression_score(the_wall_procedural_barrier, 0.7).

/* ==========================================================================
   4. TESTS (Prolog Test Suite)
   ========================================================================== */

:- begin_tests(the_wall_tests).

test(mihdhar_search_failure) :-
    % Demonstrates how the Noose prevented action on Mihdhar in Aug 2001
    constraint_indexing:constraint_classification(the_wall_2001, analytical, noose),
    [cite_start]writeln('TEST PASSED: The Wall functioned as a Noose in the Mihdhar search case[cite: 52].').

test(perspectival_gap) :-
    % Checks if there is a gap between institutional and powerless perspectives
    constraint_indexing:constraint_classification(the_wall_2001, institutional, rope),
    constraint_indexing:constraint_classification(the_wall_2001, individual_powerless, mountain),
    writeln('TEST PASSED: Perspectival Gap detected between "Coordination Tool" and "Unchangeable Law".').

:- end_tests(the_wall_tests).

/* ==========================================================================
   5. GENERATING MODEL INTERPRETATION
   ========================================================================== */

/**
 * MODEL INTERPRETATION:
 * The "Wall" is a quintessential example of a "Rope" (coordination) turning 
 * into a "Noose" (extractive/suppressive) through bureaucratic drift. 
 * The 9/11 Report notes that procedures meant to "regulate" sharing became 
 * [cite_start]procedures that "withered" the flow of information[cite: 52]. 
 * * The most striking detail is the "career stopper" warning issued by FBI 
 * leadership, which transformed a legal policy into a psychological 
 * [cite_start]"Mountain" for agents[cite: 52]. This forced agents to "play zone 
 * defense" (the CIA approach) rather than the "man-to-man" approach needed 
 * [cite_start]to track specific individuals like Mihdhar[cite: 52].
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * THE REJECTED ALTERNATIVE: "Total Transparency"
 * * ALTERNATIVE 1: 1995 Full Information Sharing
 * Viability: Legally permissible; Reno's procedures allowed sharing if 
 * [cite_start]not for "direction"[cite: 52].
 * Suppression: Actively suppressed by OIPR threat to stop presenting 
 * [cite_start]FISA warrants to the Court[cite: 52].
 * Evidence: "The Office [OIPR] assumed the role [of sole gatekeeper] 
 * [cite_start]anyway... the information flow withered"[cite: 52].
 * * CONCLUSION:
 * The Wall was not a "Mountain" of natural law; it was a "Noose" tightened 
 * by administrative risk-aversion. Had the alternative of "coordinated 
 * transparency" been maintained, the hijackers might have been found 
 * [cite_start]via immigration violations or as material witnesses[cite: 52].
 */

% Alternative rule:
intent_viable_alternative(the_wall_2001, transparency_1995, 'Coordinated transparency with legal safeguards').
intent_alternative_rejected(the_wall_2001, transparency_1995, 'Fear of judicial sanction by the FISA Court').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [the_wall_2001].
% 2. Analyze: ?- multi_index_report(the_wall_2001).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(the_wall_procedural_barrier, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(the_wall_procedural_barrier, noose, agent_power(individual_powerless)).
