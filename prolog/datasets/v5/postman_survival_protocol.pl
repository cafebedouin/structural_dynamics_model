% ============================================================================
% CONSTRAINT STORY: postman_survival_protocol
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Neil Postman's "Advice on How to Live the Rest of Your Life"
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_postman_survival_protocol, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: postman_survival_protocol
 * human_readable: Postman's Protocol for Informational Hygiene
 * domain: social/technological
 * temporal_scope: Contemporary / Digital Era
 * spatial_scope: Global / Modern America
 * * SUMMARY:
 * This constraint set is a defensive protocol designed by Neil Postman to protect individual 
 * agency against the "constant change" and "hostility" of large technological bureaucracies. 
 * It posits that survival requires regularizing the trivial through routine, distrusting 
 * large groups, and rejecting the "magical power" of quantification.
 * * KEY AGENTS:
 * - The Individual Subject: The person attempting to maintain sanity and humor in a "nonsense" saturated world.
 * - The Bureaucracy: An institutional agent characterized by a lack of feelings and a "hostility to individual differences".
 * - The Acquaintance: A person who has not "perjured themselves for you," occupying the space outside deep friendship.
 * * NARRATIVE ARC:
 * The individual is bombarded by technological change and "hardenings of categories". 
 * Postman's rules function as a "Rope" for the subject to navigate a society that is 
 * functionally a "Snare" (bureaucracy) or a "Mountain" (the prevalence of nonsense).
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

narrative_ontology:interval(postman_survival_interval, 0, 10).
narrative_ontology:constraint_claim([postman_survival_protocol], [social_hygiene]).

% Base extractiveness: Moderate (0.4)
% Rationale: The protocol itself is a gift of wisdom, but the systems it describes (Bureaucracy) 
% have high extractiveness (0.8+), necessitating the protocol's existence.
domain_priors:base_extractiveness(postman_survival_protocol, 0.4).

% Suppression: Moderate (0.5)
% Rationale: The protocol advocates for "concealing thoughts" and avoiding "-ists," 
% actively suppressing certain social "virtues" like plain honesty to avoid pain.
domain_priors:suppression_score(postman_survival_protocol, 0.5).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(postman_survival_protocol, extractiveness, 0.4).
narrative_ontology:constraint_metric(postman_survival_protocol, suppression_requirement, 0.5).

% Enforcement: Emerging naturally vs Active
% The bureaucracy's hostility emerges naturally from its size; the protocol 
% requires active individual enforcement to maintain.
domain_priors:requires_active_enforcement(postman_survival_protocol).

% BENEFICIARIES & VICTIMS
% Who benefits from this protocol? The Individual Subject (agency/humor).
constraint_beneficiary(postman_survival_protocol, individual_subject).
% Who/What is the victim? The Institutional Bureaucracy (it loses its grip on the subject).
constraint_victim(postman_survival_protocol, institutional_bureaucracy).

% Metrics required for Section 1 of the Executive Summary
/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE INDIVIDUAL SUBJECT - Rope
   --------------------------------------------------------------------------
   
   WHO: powerless (Facing large-scale technological change and bureaucracies)
   WHEN: biographical (Living the rest of your life)
   WHERE: mobile (Choosing to adopt routines and select friends)
   SCOPE: local (Interpersonal relations and personal habits)
   
   WHY THIS CLASSIFICATION:
   For the individual, Postman's advice is a "Rope"—a functional coordination mechanism. 
   By regularizing the trivial, the individual gains the agency to cope with 
   significant changes.
   
   NARRATIVE EVIDENCE:
   "Establish as many regular routines as possible... Regularize the trivial to 
   cope with the significant".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    postman_survival_protocol,
    rope,
    context(
        agent_power(powerless),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE BUREAUCRACY (Read's Law) - Snare
   --------------------------------------------------------------------------
   
   WHO: institutional (Large groups/bureaucracies)
   WHEN: historical (Systemic rules that outlast individual members)
   WHERE: trapped (Hostile to individual differences)
   SCOPE: national (Modern America / Bureaucracies at scale)
   
   WHY THIS CLASSIFICATION:
   From the perspective of the individual subject, the bureaucracy is a "Snare." 
   It extracts individual differences to satisfy the rules of the system and 
   lacks human traits like loyalty or compassion.
   
   NARRATIVE EVIDENCE:
   "Bureaucracies are by nature hostile to individual differences... institutions 
   do not have loyalty, compassion, or feelings".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    postman_survival_protocol,
    snare,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(trapped),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SAGE (Weingartner/Postman) - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer of civilizational trends)
   WHEN: civilizational (The 95% nonsense rule)
   WHERE: analytical (Observer stance)
   SCOPE: global (The human condition in a technological society)
   
   WHY THIS CLASSIFICATION:
   To the analytical observer, the prevalence of nonsense and the nature of 
   institutions are "Mountains"—unchangeable natural laws of the current 
   historical moment. One does not "fix" these; one simply learns to survive 
   around them.
   
   NARRATIVE EVIDENCE:
   "95% of everything is nonsense... Quantification... represents pure 
   superstition... America is based on counting".
   -------------------------------------------------------------------------- */


constraint_indexing:constraint_classification(
    postman_survival_protocol,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(postman_survival_protocol_tests).

test(multi_perspective_hygiene) :-
    % Individual Subject sees Rope
    constraint_indexing:constraint_classification(postman_survival_protocol, Type1, context(powerless, biographical, mobile, local)),
    % Bureaucracy acts as Snare
    constraint_indexing:constraint_classification(postman_survival_protocol, Type2, context(institutional, historical, trapped, national)),
    % Sage sees Mountain
    constraint_indexing:constraint_classification(postman_survival_protocol, Type3, context(analytical, civilizational, analytical, global)),
    Type1 \= Type2,
    Type2 \= Type3.

test(power_extractiveness_hygiene) :-
    % The individual subject experiences extraction of "autonomy" from the system
    ContextPowerless = context(powerless, biographical, mobile, local),
    ContextInstitutional = context(institutional, historical, trapped, national),
    constraint_indexing:extractiveness_for_agent(postman_survival_protocol, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(postman_survival_protocol, ContextInstitutional, Score2),
    Score1 > Score2.

test(time_immutability) :-
    % In the immediate horizon, routines are a Rope (changeable)
    constraint_indexing:effective_immutability(biographical, mobile, rope),
    % In the historical horizon, bureaucratic nature is a Mountain (unchangeable)
    constraint_indexing:effective_immutability(historical, trapped, mountain).

:- end_tests(postman_survival_protocol_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.4):
 * Reasoning: The protocol itself is low-extraction (it's a tool for agency). However, 
 * the *asymmetry* between the individual and the bureaucracy it responds to 
 * is high (0.8). I resolved this by marking the protocol as a "Rope" for the 
 * individual to avoid the "Snare" of the institution.
 * * 2. SUPPRESSION SCORE (0.5):
 * Reasoning: Postman explicitly advises suppressing "plain speaking" and 
 * numerical "superstition". This suppression is intentional and 
 * pedagogical.
 * * 3. PERSPECTIVE SELECTION:
 * The Individual, the Bureaucracy, and the Sage capture the three states of 
 * interaction with these constraints: usage, enforcement, and observation.
 * * 4. AMBIGUITIES:
 * - Honesty vs. Deception: Postman calls honesty "the first refuge of the scoundrel". 
 * I resolved this by classifying his advice as an "anti-ventilation theory" 
 * aimed at minimizing pain rather than maximizing truth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Mandatory Omega for high-extraction (systemic) constraints:
omega_variable(
    postman_concealment_efficacy,
    "Is language as a tool for concealment (Rope) a functional necessity for social peace or a predatory Snare for truth-seekers?",
    resolution_mechanism("Audit of interpersonal trust levels in high-honesty vs. high-tact cultures"),
    impact("If necessity: Social Mountain. If predatory: Mandatrophy Snare."),
    confidence_without_resolution(medium)
).

omega_variable(
    quantification_superstition,
    "Is the passion for numbers truly medieval superstition (Mountain) or an emerging coordination Rope we have not yet mastered?",
    resolution_mechanism("Long-term verification of numerical intelligence/beauty predictive power vs. human intuition"),
    impact("If superstition: Mountain. If coordination: Rope."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Radical Honesty / Plain Speaking
 * Viability: The conventional virtue.
 * Suppression: Actively rejected by Nugget #5 as "overrated" and potentially 
 * causing "pain to others".
 * * ALTERNATIVE 2: Scientific Quantification (Counting)
 * Viability: America's base logic.
 * Suppression: Rejected by Rule 19 as "medieval superstition" with 
 * "limited effectiveness" in human affairs.
 * * CONCLUSION:
 * The existence of these alternatives (Honesty, Quantification) shifts Postman's 
 * protocol into a "Rope" of informational hygiene. Without his rules, 
 * the individual is easily ensnared by the "Snare" of these alternatives.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [postman_survival_protocol].
% Multi-perspective: ?- multi_index_report(postman_survival_protocol).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
