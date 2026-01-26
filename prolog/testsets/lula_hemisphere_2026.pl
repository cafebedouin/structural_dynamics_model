% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: lula_hemisphere_2026
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: NYT Opinion - "Lula: This Hemisphere Belongs to All of Us" (Jan 18, 2026)
% ============================================================================

:- module(lula_hemisphere_2026, []).

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
 * * constraint_id: lula_hemisphere_2026
 * human_readable: The Monroe Doctrine Revival (Unilateral US Hegemony)
 * domain: political/legal
 * temporal_scope: 2026 (Modern era / Post-WWII order erosion)
 * spatial_scope: Latin America and the Caribbean
 * * SUMMARY:
 * President Lula of Brazil decries the Jan 3, 2026 US military intervention in
 * Venezuela as a violation of the sovereign equality of nations. He describes a
 * shift where "zones of influence" and unilateral force replace the collectively
 * [cite_start]agreed-upon rules of the UN Charter[cite: 1, 2, 4].
 * * KEY AGENTS:
 * - [cite_start]The United States: Hegemonic power acting unilaterally through "fear and coercion"[cite: 5, 10].
 * - [cite_start]Luiz Inácio Lula da Silva: Institutional leader of a regional power advocating for multipolarity and international law[cite: 6, 7].
 * - [cite_start]The Venezuelan People: Subjects whose self-determination is bypassed by external military force[cite: 11].
 * * NARRATIVE ARC:
 * The post-WWII multilateral order is portrayed as a failing "Rope" being
 * transformed into a "Snare" by unilateral hegemonic actions. Lula argues that
 * for Latin America, this represents a return to an "outdated" era of neocolonial
 * [cite_start]incursions[cite: 2, 9].
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION
   ========================================================================== */

% Structural Anchor for Audit
narrative_ontology:interval(lula_hemisphere_2026, 2026, 2026).
narrative_ontology:constraint_claim(lula_hemisphere_2026, snare).

% Base extractiveness: 0.75
% Rationale: Military capture of a president and "neocolonial incursions for
% strategic resources" represent high asymmetry and resource extraction[cite: 1, 9, 10].
domain_priors:base_extractiveness(lula_hemisphere_2026, 0.75).

% Suppression: 0.85
% Rationale: Unilateral force "arrogates to itself the right to deliver justice,"
% effectively suppressing the alternative of the UN Security Council[cite: 2, 4, 10].
domain_priors:suppression_score(lula_hemisphere_2026, 0.85).

% Enforcement: Requires active maintenance (Bombings, President capture)[cite: 1].
domain_priors:requires_active_enforcement(lula_hemisphere_2026).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(lula_hemisphere_2026, hegemonic_powers).
constraint_victim(lula_hemisphere_2026, sovereign_nations).
constraint_victim(lula_hemisphere_2026, venezuelan_civilians).

% Section 1 Executive Summary Metrics
narrative_ontology:constraint_metric(lula_hemisphere_2026, extractiveness, 0.75).
narrative_ontology:constraint_metric(lula_hemisphere_2026, suppression_requirement, 0.85).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: VENEZUELAN REFUGEE - Mountain
   --------------------------------------------------------------------------
   [cite_start]WHO: individual_powerless - Displaced individuals sheltered in Brazil[cite: 11].
   [cite_start]WHEN: immediate - Survival and safe return home are current necessities[cite: 11].
   [cite_start]WHERE: trapped - Subject to the "anomie" and violence of military attack[cite: 2, 5].
   [cite_start]SCOPE: national - Their entire home country is under direct attack[cite: 5].

   WHY THIS CLASSIFICATION:
   For the individual caught in the conflict, the US military intervention
   appears as an unchangeable, overwhelming force of nature (Mountain) that
   [cite_start]removes all agency from the people[cite: 2, 11].

   NARRATIVE EVIDENCE:
   "The future of Venezuela... must remain in the hands of its people... this is
   [cite_start]an essential condition for the millions... sheltered in Brazil to safely return"[cite: 11].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    lula_hemisphere_2026,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    domain_priors:suppression_score(lula_hemisphere_2026, S),
    S > 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: PRESIDENT LULA (BRAZIL) - Snare
   --------------------------------------------------------------------------
   [cite_start]WHO: institutional - Head of state of a populist democracy[cite: 6, 12].
   [cite_start]WHEN: biographical - Spanning 200 years of independent history[cite: 5].
   [cite_start]WHERE: constrained - Trying to maintain dialogue with the US despite attack[cite: 12].
   [cite_start]SCOPE: continental - Defending a hemisphere that "belongs to all of us"[cite: 12].

   WHY THIS CLASSIFICATION:
   Lula views the intervention as a coercive, extractive "Snare" that violates
   [cite_start]collectively agreed-upon rules to serve hegemonic interests[cite: 2, 6, 9].

   NARRATIVE EVIDENCE:
   "We will not be subservient to hegemonic endeavors... the division of the
   [cite_start]world into zones of influence... [is] outdated and damaging"[cite: 6, 9].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    lula_hemisphere_2026,
    snare,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(continental)
    )
) :-
    domain_priors:base_extractiveness(lula_hemisphere_2026, E),
    E > 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: US STRATEGISTS (INFERRED) - Rope
   --------------------------------------------------------------------------
   [cite_start]WHO: individual_powerful - Arrogating the right to deliver justice[cite: 4].
   [cite_start]WHEN: biographical - Using force as a rule to "resolve disputes"[cite: 2].
   [cite_start]WHERE: arbitrage - Acting unilaterally while bypassing the UN[cite: 4].
   [cite_start]SCOPE: global - Reconfiguring the international system[cite: 2].

   WHY THIS CLASSIFICATION:
   From the hegemonic perspective, the use of force is a functional tool (Rope)
   [cite_start]to achieve strategic goals and "justice" where multilateral systems failed[cite: 1, 4].

   NARRATIVE EVIDENCE:
   "It is not legitimate for another state to arrogate to itself the right to
   [cite_start]deliver justice"[cite: 4].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    lula_hemisphere_2026,
    rope,
    context(
        agent_power(individual_powerful),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(lula_hemisphere_2026_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(lula_hemisphere_2026, Type1, context(individual_powerless, immediate, trapped, national)),
    constraint_indexing:constraint_classification(lula_hemisphere_2026, Type2, context(institutional, biographical, constrained, continental)),
    Type1 = mountain,
    Type2 = snare.

test(power_extractiveness_scaling) :-
    ContextPowerless = context(individual_powerless, immediate, trapped, national),
    ContextPowerful = context(individual_powerful, biographical, arbitrage, global),
    constraint_indexing:extractiveness_for_agent(lula_hemisphere_2026, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(lula_hemisphere_2026, ContextPowerful, Score2),
    Score1 > Score2.

test(continental_scope_noose) :-
    constraint_indexing:constraint_classification(lula_hemisphere_2026, snare, context(institutional, biographical, constrained, continental)).

:- end_tests(lula_hemisphere_2026_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS SCORE (0.75): High score based on the "capture" of a
 * [cite_start]head of state and "neocolonial incursions" for resources[cite: 1, 9].
 * * 2. SUPPRESSION SCORE (0.85): Reflects the "selective" following of norms
 * [cite_start]and the arrogance of unilateral justice replacing collective rules[cite: 2, 4].
 * * 3. OMEGAS: The central uncertainty is the *motive* for the capture. Lula
 * [cite_start]attributes it to strategic resources/hegemony[cite: 6, 9], while the US
 * likely claims "justice/democracy."
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    us_intervention_intent,
    'Was the US capture of the Venezuelan president primarily for human rights justice or strategic resource extraction?',
    resolution_mechanism('Audit of captured president\'s trial evidence vs US corporate contracts signed in Venezuela post-capture.'),
    impact('If Justice: order remains a failing Rope. If Extraction: order is a predatory Snare.'),
    confidence_without_resolution(medium)
).

omega_variable(
    regional_unity_threshold,
    "Will Latin American states overcome 'ideological differences' to resist hegemonic pressure as Lula hopes?",
    resolution_mechanism("Track voting alignment in OAS/UN and joint infrastructure project funding through 2027."),
    impact("If Unified: Regional agency creates a counter-Rope. If Divided: States remain trapped in the Snare."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * [cite_start]ALTERNATIVE 1: Inclusive political process led by Venezuelans[cite: 11].
 * [cite_start]Viability: Lula claims this is the "only condition" for a sustainable future[cite: 11].
 * [cite_start]Suppression: Actively bypassed by unilateral US military attack[cite: 1, 4].
 * * [cite_start]ALTERNATIVE 2: Multilateral order via UN Security Council[cite: 2].
 * [cite_start]Viability: Established post-WWII to build "free, inclusive and democratic societies"[cite: 2].
 * [cite_start]Suppression: Attacked and weakened by major powers until force became the "rule"[cite: 2].
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [lula_hemisphere_2026].
% Report: ?- constraint_indexing:multi_index_report(lula_hemisphere_2026).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
