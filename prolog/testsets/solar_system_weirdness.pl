% ============================================================================
% CONSTRAINT STORY: solar_system_weirdness
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Alex Wilkins, "Our solar system is extremely weird" 
% Status: [RESOLVED]
% ============================================================================

:- module(constraint_solar_system_weirdness, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
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
 * * constraint_id: solar_system_weirdness
 * human_readable: The Solar System Configuration Anomaly
 * domain: technological/scientific
 * temporal_scope: 1990s (first exoplanets) to 2026 (present understanding) 
 * spatial_scope: Milky Way Galaxy / Solar System 
 * * SUMMARY:
 * For decades, astronomers assumed our solar system was a typical model for the 
 * universe. Large-scale surveys since the early 2000s have revealed 
 * that our neat arrangement of four rocky planets and four gas giants is actually 
 * an outlier compared to the more common "super-Earth" or "sub-Neptune" systems 
 * found elsewhere.
 * * KEY AGENTS:
 * - The Modern Astronomer: Analytical observer using long-term surveys to rewrite formation stories.
 * - The Sun: A solitary, unusually large star (larger than 90% of neighbors) that acts as the system's anchor.
 * - Jupiter: A rare, neat-orbiting gas giant whose trajectory differs from the erratic paths of most exoplanet Jupiters.
 * * NARRATIVE ARC:
 * The "Uniformity Assumption" acted as a conceptual Noose that limited planetary 
 * discovery models. Realizing our "weirdness" has provided the Rope 
 * needed to climb toward a more accurate theory of planetary formation.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(solar_system_weirdness, 0, 10).
narrative_ontology:constraint_claim([solar_system_weirdness], [scientific_paradigm_shift]).

% Base extractiveness: 0.2
% Rationale: Purely scientific/informational; no coercive extraction of labor or resources.
domain_priors:base_extractiveness(solar_system_weirdness, 0.2).

% Suppression: 0.6
% Rationale: The assumption of our system being "normal" suppressed the search 
% for anomalies and dominated the Copernican principle for decades.
domain_priors:suppression_score(solar_system_weirdness, 0.6).

% Enforcement: Emerges naturally from physical laws of planetary formation.
domain_priors:emerges_naturally(solar_system_weirdness).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(solar_system_weirdness, modern_planetary_theorists).
constraint_victim(solar_system_weirdness, legacy_copernican_models).

% Metrics for Section 1
narrative_ontology:constraint_metric(solar_system_weirdness, extractiveness, 0.2).
narrative_ontology:constraint_metric(solar_system_weirdness, suppression_requirement, 0.6).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE MODERN ASTRONOMER - Rope
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer using "High Accuracy Radial Velocity Searchers") 
   WHEN: biographical (Tracking data since the early 2000s) 
   WHERE: arbitrage (Comparing thousands of exoplanets to our own system) 
   SCOPE: global (Utilizing telescopes like Kepler and TESS) 
   
   WHY THIS CLASSIFICATION:
   For the researcher, "weirdness" is a Rope—a functional tool to re-evaluate 
   how systems form and to distinguish between "weird at the 1% level" versus 
   "1 in a million".
   
   NARRATIVE EVIDENCE:
   "Realising that our solar system isn’t like most others out there has helped 
   astronomers rewrite the story of how it formed".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    solar_system_weirdness,
    rope,
    context(
        agent_power(analytical),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(solar_system_weirdness, E),
    E < 0.4,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE SUN/PLANETS - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Physical bodies subject to orbital mechanics) 
   WHEN: civilizational (The 4.5 billion year history of the system) 
   WHERE: trapped (Neat, round orbits with no trajectory exit) 
   SCOPE: national (Restricted to the local solar neighborhood) 
   
   WHY THIS CLASSIFICATION:
   The physical arrangement (four rocky, four gassy) is an immutable Mountain 
   governed by physics. It is simply a fact that we have what we have and lack 
   what we lack (like super-Earths).
   
   NARRATIVE EVIDENCE:
   "Our planets, too, are rare... we are missing planets common to most other 
   star systems – those known as super-Earths".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    solar_system_weirdness,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    % Physical laws appear immutable over civilizational/historical spans
    true,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: LEGACY UNIFORMITARIANS - Noose
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-making academic paradigms of the 20th century) 
   WHEN: historical (The era before exoplanet surveys) 
   WHERE: trapped (Incapable of imagining Earth as a "weird" outlier) 
   SCOPE: national (Academic consensus groups) 
   
   WHY THIS CLASSIFICATION:
   The "Normalcy Noose" extracted the ability to detect Earth-like planets by 
   assuming they would be found around "sun-like stars" exactly like our own, 
   leading to decades of unmet expectations.
   
   NARRATIVE EVIDENCE:
   "Even after finding thousands of exoplanets, we have yet to spot an 
   Earth-like planet around a sun-like star".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    solar_system_weirdness,
    noose,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    domain_priors:suppression_score(solar_system_weirdness, S),
    S > 0.5,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(solar_system_weirdness_tests).

test(multi_perspective_variance) :-
    % Astronomer (Rope) vs Sun (Mountain) vs Legacy Theory (Noose)
    constraint_indexing:constraint_classification(solar_system_weirdness, rope, context(analytical, biographical, arbitrage, global)),
    constraint_indexing:constraint_classification(solar_system_weirdness, mountain, context(individual_powerless, civilizational, trapped, national)),
    constraint_indexing:constraint_classification(solar_system_weirdness, noose, context(institutional, historical, trapped, national)).

test(power_extractiveness_scaling) :-
    % Information extraction is low for the observer (0.2)
    domain_priors:base_extractiveness(solar_system_weirdness, E),
    E < 0.3.

test(time_immutability) :-
    % Long horizons (historical/civilizational) see the weirdness as an immutable biological/physical fact (Mountain)
    true.

:- end_tests(solar_system_weirdness_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.2): Since this is a discovery about physical reality 
 * rather than a social hierarchy, extraction is minimal.
 * * 2. SUPPRESSION (0.6): High because the Copernican "typicality" was the 
 * mandatory paradigm for nearly a century before surveys broke it.
 * * 3. PERSPECTIVE SELECTION: 
 * - Astronomer: Represents the "Rewriting" (Rope).
 * - The System itself: Represents the "Physical Facts" (Mountain).
 * - Legacy Theorists: Represents the "Lost Decades" of search (Noose).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    weirdness_degree_calibration,
    "Is the solar system weird at the 1% level or the 1-in-a-million level?",
    resolution_mechanism("Long-term analysis of data from upcoming missions like Habitable Worlds Observatory"),
    impact("If 1%: Mountain (minor variance). If 1-in-a-million: Noose (Earth is a precarious fluke)."),
    confidence_without_resolution(medium)
).

omega_variable(
    earth_twin_absence,
    "Is the lack of Earth-like planets a result of survey bias or actual non-existence?",
    resolution_mechanism("Completion of large-scale surveys looking for Earth-mass planets around G-type stars"),
    impact("If bias: Rope (just need better tools). If non-existence: Mountain (Earth is truly unique)."),
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: The Copernican Typicality Model
 * Viability: The previous standard for 100+ years.
 * Suppression: Actively rejected now as "weirdness" data accumulates.
 * Evidence: "It wasn’t until the early 2000s... we started to get the first 
 * hints that our solar system... might be unique".
 * * CONCLUSION:
 * The shift from "Typicality" to "Weirdness" represents the core scientific 
 * arbitrage of modern planetary science.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraints/solar_system_weirdness].
 * 2. Multi-perspective: ?- multi_index_report(solar_system_weirdness).
 * 3. Run tests: ?- run_tests(solar_system_weirdness_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
