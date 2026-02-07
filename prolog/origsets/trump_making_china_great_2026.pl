% ============================================================================
% CONSTRAINT STORY: trump_making_china_great_2026
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Policy Brief - How Trump is Making China Great Again (ECFR, Jan 2026)
% ============================================================================

:- module(trump_making_china_great_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:extractiveness_for_agent/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: trump_making_china_great_2026
 * human_readable: The Trumpian Post-Western Order
 * domain: political/economic/geopolitical
 * temporal_scope: 2025-2030 (Immediate to Biographical)
 * spatial_scope: Global (Multipolar focus)
 * * SUMMARY:
 * The return of Donald Trump has accelerated a shift from a US-led liberal
 * international order to a multipolar "post-Western" world. This transition
 * acts as a constraint on European security and US global affinity while
 * expanding the agency of "middle powers" and China.
 * * KEY AGENTS:
 * - [cite_start]The Trump Administration: The "Normal Great Power" agent, transactional and predatory[cite: 650].
 * - [cite_start]European Public/Leaders: The "Axis of Pessimists," feeling trapped by Russian aggression and US unreliability[cite: 14, 865].
 * - [cite_start]Middle Powers (India/Brazil/South Africa): The "Pole-Hoppers," viewing multipolarity as an opportunity for strategic arbitrage[cite: 709, 710].
 * - [cite_start]China: The "Rising Pole," increasingly viewed as a necessary partner or ally[cite: 8, 40].
 * * NARRATIVE ARC:
 * The US moves from being a "Liberal Leviathan" (Rope for allies) to a transactional
 * great power (Snare for allies, Rope for middle powers). This forces a
 * [cite_start]"geopolitical awakening" in Europe as the Pax Americana ends[cite: 1360].
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION
   ========================================================================== */

% Required for DR-Audit Suite
narrative_ontology:interval(post_trump_return, 2025, 2026).
narrative_ontology:constraint_claim(trump_making_china_great_2026, snare).

% Base extractiveness: 0.65
% Rationale: Trump's "America First" approach is explicitly transactional and
% predatory[cite: 20, 23]. [cite_start]Tariffs and "seizure of Greenland" threats imply high extraction[cite: 756, 1352].
domain_priors:base_extractiveness(trump_making_china_great_2026, 0.65).

% Suppression: 0.40
% Rationale: Alternatives (liberal order) are visible but being dismantled.
% Transactionalism is the new "normal"[cite: 650].
domain_priors:suppression_score(trump_making_china_great_2026, 0.40).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(trump_making_china_great_2026, extractiveness, 0.65).
narrative_ontology:constraint_metric(trump_making_china_great_2026, suppression_requirement, 0.4).

% Enforcement: Requires active maintenance (tariffs, summits, intervention in Venezuela)
domain_priors:requires_active_enforcement(trump_making_china_great_2026).

% Metrics for Executive Summary
% Beneficiaries & Victims
constraint_beneficiary(trump_making_china_great_2026, china).
constraint_beneficiary(trump_making_china_great_2026, multipolar_middle_powers).
constraint_victim(trump_making_china_great_2026, european_union).
constraint_victim(trump_making_china_great_2026, liberal_internationalists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE EUROPEAN CITIZEN - Snare
   --------------------------------------------------------------------------
   [cite_start]WHO: powerless - Feeling vulnerable to external superpowers[cite: 14].
   [cite_start]WHEN: immediate - Focused on current threats (Russia, nuclear war)[cite: 1291].
   [cite_start]WHERE: trapped - US no longer a reliable ally, but Europe lacks its own deterrent[cite: 27, 1244].
   [cite_start]SCOPE: continental - Concerned with European war and security[cite: 1291].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    trump_making_china_great_2026,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(continental)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE INDIAN/BRAZILIAN DIPLOMAT - Rope
   --------------------------------------------------------------------------
   [cite_start]WHO: institutional - Middle-power state agency[cite: 709].
   [cite_start]WHEN: biographical - Planning development over the next decade[cite: 8].
   [cite_start]WHERE: arbitrage - Ability to play US and China against each other[cite: 710, 713].
   [cite_start]SCOPE: global - Moving between poles as they wish[cite: 710].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    trump_making_china_great_2026,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANALYTICAL OBSERVER (ECFR) - Mountain
   --------------------------------------------------------------------------
   [cite_start]WHO: analytical - Researchers documenting a structural shift[cite: 6].
   [cite_start]WHEN: historical - Comparing current era to the post-1945 order[cite: 650].
   WHERE: analytical - Observing from outside the immediate policy desperation.
   [cite_start]SCOPE: global - Surveying 21 countries[cite: 28].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    trump_making_china_great_2026,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(trump_china_great_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(trump_making_china_great_2026, Type1, context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(continental))),
    constraint_indexing:constraint_classification(trump_making_china_great_2026, Type2, context(agent_power(institutional), time_horizon(biographical), exit_options(arbitrage), spatial_scope(global))),
    Type1 = snare,
    Type2 = rope.

test(geopolitical_arbitrage_efficiency) :-
    % Middle powers see less extraction because they have exit options (arbitrage)
    ContextEurope = context(powerless, immediate, trapped, continental),
    ContextIndia = context(institutional, biographical, arbitrage, global),
    constraint_indexing:extractiveness_for_agent(trump_making_china_great_2026, ContextEurope, Score1),
    constraint_indexing:extractiveness_for_agent(trump_making_china_great_2026, ContextIndia, Score2),
    Score1 > Score2.

test(us_unreliability_effect) :-
    % Europeans view US unreliability as a hard constraint (Snare/Mountain)
    constraint_indexing:constraint_classification(trump_making_china_great_2026, snare, context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(continental))).

:- end_tests(trump_china_great_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.65):
 * [cite_start]The document paints a picture of a "predatory" US [cite: 23] and "asymmetric"
 * [cite_start]threats (Greenland)[cite: 1352]. However, it isn't 1.0 because middle powers
 * [cite_start]can still extract benefits by trading poles[cite: 710].
 * * 2. SUPPRESSION SCORE (0.40):
 * [cite_start]The liberal order isn't invisible—it's just being "disavowed"[cite: 21].
 * The shift is visible and being actively analyzed by think tanks like ECFR.
 * * 3. PERSPECTIVE SELECTION:
 * - European Citizen: Represents the "victim" of the shift (Snare).
 * - Middle-Power Diplomat: Represents the "beneficiary" of multipolarity (Rope).
 * - ECFR Researcher: Represents the "analytical" view of the Mountain.
 * * 4. AMBIGUITIES:
 * - [cite_start]The document suggests "expectations of Trump are lower than 12 months ago" [cite: 10]
 * [cite_start]but also that substantial numbers believe he successfully "defended America's interests"[cite: 649].
 * I resolved this by noting the "Normal Great Power" transition—it's a Snare for allies
 * but a successful Rope for the US electorate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    european_nuclear_deterrent,
    "Will Europe successfully develop an alternative nuclear deterrent independent of the US?",
    resolution_mechanism("Track PESCO/EU-defense spending and French/UK nuclear policy shifts through 2030."),
    impact("If Yes: Europe becomes a 'Pole' (Rope). If No: Europe remains 'trapped' in the US Snare."),
    confidence_without_resolution(medium)
).

omega_variable(
    venezuela_intervention_effect,
    "Does the Venezuela intervention solidify a 'Spheres of Influence' model as the global standard?",
    resolution_mechanism("Monitor Chinese/Russian naval and military deployments in their respective 'near abroad' over the next 2 years."),
    impact("If Yes: Multipolarity becomes a 'Mountain' (hard law). If No: It remains a transactional 'Rope'."),
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Transatlantic Liberal Order (Biden-era)
 * [cite_start]Viability: The status quo until 2024.[cite: 1036, 1037].
 * [cite_start]Suppression: Actively disavowed by Trump's "America First"[cite: 20, 21].
 * [cite_start]Evidence: "Donald Trump did not go into politics to make China great again... his aggressive 'America First' approach was driving people closer to China."[cite: 17, 20].
 * * CONCLUSION:
 * The fact that a once-stable "Rope" (NATO/Liberal Order) has been transformed
 * into a "Snare" or discarded entirely by the current US administration is
 * what characterizes the 2026 geopolitical environment.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% TO USE:
% ?- [trump_making_china_great_2026].
% ?- run_tests(trump_china_great_tests).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
