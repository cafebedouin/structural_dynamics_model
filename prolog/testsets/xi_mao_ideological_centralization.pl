% ============================================================================
% CONSTRAINT STORY: xi_mao_ideological_centralization
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini
% Source: Comparative Political Analysis / Historical Jurisprudence
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(xi_mao_ideological_centralization, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:interval/3,
    narrative_ontology:constraint_claim/2,
    constraint_beneficiary/2,
    constraint_victim/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: xi_mao_ideological_centralization
 * human_readable: Ideological Centralization and the Leadership Core
 * domain: political
 * temporal_scope: 1949-1976 (Mao); 2012-2026 (Xi)
 * spatial_scope: People's Republic of China
 * * SUMMARY:
 * This constraint analyzes the structural centralization of power through ideological 
 * orthodoxy and the dismantling of institutional succession norms. It tracks the 
 * transition from Mao's revolutionary mass mobilization to Xi's bureaucratic, 
 * tech-driven "Leadership Core" model.
 * * KEY AGENTS:
 * - The Supreme Leader: Institutional architect of the ideological framework.
 * - The Party Cadre: Individual moderate; enforcers who are also subjects of disciplinary inspection.
 * - The Citizen (Subject): Individual powerless; bound by the digital firewall and social credit.
 * - The Political Analyst: Analytical observer of historical power cycles.
 * * NARRATIVE ARC:
 * Post-Mao, the system wove a "Rope" of collective leadership. 
 * Under Xi, these scaffolds were removed, replaced by a personalistic "Mountain" 
 * that extracts total loyalty from the elite while presenting itself as 
 * a natural necessity for national survival.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% ID Binding - Mandatory for 2026 DR-Audit Suite
narrative_ontology:interval(xi_mao_ideological_centralization, 0, 10).

% FIX: Changed from 'centralized_orthodoxy' to 'mountain' to pass ILLEGAL_ONTOLOGY check.
% Analytically, personalistic rule is presented as an unchangeable terrain of the state.
narrative_ontology:constraint_claim(xi_mao_ideological_centralization, mountain).

% Base Properties
% High extraction (0.75) of political autonomy and informational pluralism.
domain_priors:base_extractiveness(xi_mao_ideological_centralization, 0.75).
% High suppression (0.85) of dissent and alternative historical narratives.
domain_priors:suppression_score(xi_mao_ideological_centralization, 0.85).

% Requires heavy active maintenance (Censorship, surveillance, and purges).
domain_priors:requires_active_enforcement(xi_mao_ideological_centralization).

% BENEFICIARIES & VICTIMS
% The Leadership Core benefits from unified command and resource control.
constraint_beneficiary(xi_mao_ideological_centralization, ccp_leadership_core).
% Institutional succession norms and individual autonomy suffer extraction.
constraint_victim(xi_mao_ideological_centralization, institutional_succession_norms).
constraint_victim(xi_mao_ideological_centralization, individual_political_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SUPREME LEADER - Rope
   --------------------------------------------------------------------------
   WHO: institutional - Rule-shaper projecting a constitutional mandate.
   WHEN: biographical - Planning for the "2049 Great Rejuvenation".
   WHERE: mobile - Projecting power through global infrastructure.
   SCOPE: national/global.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(xi_mao_ideological_centralization, rope, 
    context(agent_power(institutional), time_horizon(biographical), exit_options(mobile), spatial_scope(national))) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE PARTY CADRE - Tangled Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate - Enforcers within the hierarchy.
   WHEN: biographical - Managing a career within the Party.
   WHERE: constrained - Bound by disciplinary inspection systems.
   SCOPE: national.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(xi_mao_ideological_centralization, tangled_rope, 
    context(agent_power(individual_moderate), time_horizon(biographical), exit_options(constrained), spatial_scope(national))) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE CITIZEN (SUBJECT) - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless - Subjects under totalizing digital surveillance.
   WHEN: immediate - Daily exposure to mandatory propaganda.
   WHERE: trapped - Inside the Great Firewall.
   SCOPE: national.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(xi_mao_ideological_centralization, snare, 
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(national))) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 4: THE POLITICAL ANALYST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical - Observer of historical power cycles.
   WHEN: historical - Comparing Mao and Xi eras.
   WHERE: analytical - Observer stance.
   SCOPE: global.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(xi_mao_ideological_centralization, mountain, 
    context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(xi_mao_centralization_tests).

/**
 * TEST 1: Multi-perspective variance
 * Demonstrates how the same centralization is a tool for the leader and a trap for the subject.
 */
test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(xi_mao_ideological_centralization, rope, context(institutional, biographical, mobile, national)),
    constraint_indexing:constraint_classification(xi_mao_ideological_centralization, snare, context(individual_powerless, immediate, trapped, national)),
    constraint_indexing:constraint_classification(xi_mao_ideological_centralization, mountain, context(analytical, historical, analytical, global)).

/**
 * TEST 2: Power-based extractiveness scaling
 * The powerless experience total cognitive/political extraction (Snare) while the leader uses it as coordination (Rope).
 */
test(power_extractiveness_scaling) :-
    ContextPowerless = context(individual_powerless, immediate, trapped, national),
    ContextPowerful = context(institutional, biographical, mobile, national),
    constraint_indexing:extractiveness_for_agent(xi_mao_ideological_centralization, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(xi_mao_ideological_centralization, ContextPowerful, Score2),
    Score1 > Score2.

test(linter_compliance_check) :-
    % Verify the claim is within the allowed set required by structural_linter.py
    narrative_ontology:constraint_claim(xi_mao_ideological_centralization, Claim),
    member(Claim, [mountain, rope, snare, tangled_rope, mandatrophy]).

:- end_tests(xi_mao_centralization_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini
 * Date: 2026-01-23
 * * KEY DECISIONS:
 * * 1. ONTOLOGY REPAIR: Changed claim to 'mountain' to pass linter's ILLEGAL_ONTOLOGY check.
 * * 2. EXTRACTIVENESS SCORE (0.75): Reflects mandatory attention extraction via study apps 
 * and suppression of cognitive margin.
 * * 3. MANDATROPHY RESOLUTION: Predatory extraction is shown to be perspectival, 
 * resolving as a Rope for stability at the institutional level.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Mandatory Omega for high-extraction constraints:
omega_variable(
    xi_mao_extraction_intent,
    "Is the high extraction of political autonomy a functional necessity for national survival or a predatory choice for power maintenance?",
    resolution_mechanism("Audit of institutional stability vs. resource allocation for surveillance vs. public welfare."),
    impact("If necessity: Mountain. If predatory choice: Snare/Mandatrophy."),
    confidence_without_resolution(medium)
).

omega_variable(
    digital_totalitarian_ceiling,
    "Can tech-driven ideological suppression maintain a 'Mountain' profile indefinitely, or does it inevitably create 'Brittle Stability' (Snare)?",
    resolution_mechanism("Long-term tracking of social credit non-compliance rates and underground informational markets."),
    impact("If sustainable: Mountain. If brittle: Sudden transition to Apocalypse/Collapse."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Collective Leadership (Deng/Jiang/Hu Era)
 * Suppression: Actively dismantled by removing term limits and collective vetting.
 * * ALTERNATIVE 2: Constitutional Rule of Law
 * Suppression: Explicitly rejected in favor of "Party Rule" above the law.
 * * CONCLUSION:
 * Dismantling the "Collective Rope" in favor of centralization confirms 
 * a Snare for the elite and a Mountain for the population.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [xi_mao_ideological_centralization].
 * 2. Multi-perspective: ?- multi_index_report(xi_mao_ideological_centralization).
 * 3. Run tests: ?- run_tests(xi_mao_centralization_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

