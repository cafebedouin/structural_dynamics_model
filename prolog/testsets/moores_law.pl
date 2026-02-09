% ============================================================================
% CONSTRAINT STORY: moores_law
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Moore, G. E. (1965) / Semiconductor Physics
% ============================================================================

:- module(constraint_moores_law, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: moores_law
 * human_readable: Moore's Law
 * domain: technological/economic
 * temporal_scope: 1965-Present (The Silicon Era)
 * spatial_scope: Global (Computational Hardware)
 * * SUMMARY:
 * Moore's Law is the observation that the number of transistors on a 
 * microchip doubles approximately every two years, while the cost of 
 * computers is halved. It is less a physical law and more a self-fulfilling 
 * prophecy or a "socially enforced" pace of innovation that has driven 
 * the digital revolution.
 * * KEY AGENTS:
 * - The Semiconductor Engineer: Bound to the "treadmill" of miniaturization.
 * - The Software Developer: Relies on "free" hardware gains to offset 
 * inefficient code (Wirth's Law).
 * - The Strategic Investor: Uses the law as a predictable clock for 
 * capital deployment and market obsolescence.
 * * NARRATIVE ARC:
 * Originally a Rope (a beneficial coordination target), Moore's Law has 
 * become a Mountain (an seemingly immutable pace of progress) and a 
 * Snare for manufacturers who must spend billions in R&D just to stay 
 * in the same place relative to the market.
 */

/* ==========================================================================
   2. BASE PROPERTIES (The "Reality" Layer)
   ========================================================================== */

% Required for structural integration
narrative_ontology:interval(moores_law_interval, 0, 10).
narrative_ontology:constraint_claim(moores_law, mountain).

% Base extractiveness: 0.3 (Low-Moderate)
% Rationale: It extracts value through planned obsolescence. Devices lose 
% utility rapidly as the "frontier" moves forward, forcing a cycle of 
% consumption and extraction of consumer capital.
domain_priors:base_extractiveness(moores_law, 0.3).

% Suppression score: 0.5 (Moderate)
% Rationale: Alternative hardware architectures (e.g., analog computing) 
% are suppressed by the massive economies of scale and "gravity" of the 
% standard silicon roadmap.
domain_priors:suppression_score(moores_law, 0.5).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(moores_law, extractiveness, 0.3).
narrative_ontology:constraint_metric(moores_law, suppression_requirement, 0.5).

% Enforcement requirements: Requires active, massive capital enforcement 
% by the semiconductor industry to maintain the pace.
domain_priors:requires_active_enforcement(moores_law).

% Metrics for Section 1 of the Executive Summary
% Beneficiaries: Consumer electronics industry and data-intensive services.
narrative_ontology:constraint_beneficiary(moores_law, platform_capitalists).

% Victims: Older hardware owners and the environment (e-waste).
narrative_ontology:constraint_victim(moores_law, legacy_infrastructure_owners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE END USER - ROPE
   --------------------------------------------------------------------------
   
   WHO: powerless (A consumer of technology).
   WHEN: biographical (A lifetime of increasing computational power).
   WHERE: mobile (Can choose when to upgrade).
   SCOPE: global.
   
   WHY THIS CLASSIFICATION:
   For the user, Moore's Law is a Rope. It is a predictable ladder of 
   progress. It ensures that the computer they buy today will be 
   significantly more powerful than the one they had five years ago, 
   lowering the barrier to entry for complex software and services.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    moores_law,
    rope,
    context(
        agent_power(powerless),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE CHIP FABRICATOR - SNARE
   --------------------------------------------------------------------------
   
   WHO: institutional (A semiconductor manufacturer like Intel or TSMC).
   WHEN: immediate (Quarterly cycles and multi-billion dollar R&D).
   WHERE: constrained (Cannot stop without losing market lead).
   SCOPE: global.
   
   WHY THIS CLASSIFICATION:
   For the manufacturer, the law is a Snare. It is an "unrelenting 
   treadmill." If they fail to meet the doubling pace for a single cycle, 
   their market cap can evaporate. The cost of maintaining the pace 
   (the "Snare") increases exponentially as physics reaches its limits.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    moores_law,
    snare,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE QUANTUM PHYSICIST - MOUNTAIN
   --------------------------------------------------------------------------
   
   WHO: analytical (Scientific observer).
   WHEN: generational/historical (Viewing the physical limits of matter).
   WHERE: trapped (Atomic scales are fixed).
   SCOPE: global.
   
   WHY THIS CLASSIFICATION:
   To the physicist, the eventual "end" of Moore's Law is a Mountain. It 
   is a physical wall. Once transistors reach the size of a few atoms, 
   quantum tunneling makes current silicon-based logic impossible. 
   This is not a social choice, but an unchangeable physical constraint.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    moores_law,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(generational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(moores_law_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(moores_law, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(moores_law, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(moores_law, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3.

test(enforcement_scaling) :-
    % Institutional actors must enforce this (Requires Active Enforcement).
    domain_priors:requires_active_enforcement(moores_law).

test(time_immutability) :-
    % Long-term (Generational) analysis reveals the physical "Mountain" limit.
    constraint_indexing:constraint_classification(moores_law, mountain, context(_, generational, _, _)).

:- end_tests(moores_law_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. ENFORCEMENT: I classified this as "Requires Active Enforcement." While 
 * it looks like a natural law, it is actually the result of massive 
 * industrial coordination and investment.
 * 2. SNARE VS ROPE: The distinction depends on whether you are the 
 * producer (Snare) or the consumer (Rope) of the processing power.
 * 3. EXTRACTIVENESS: Set at 0.3. The gain in utility for users is real, 
 * but the extraction comes through the "forced" obsolescence of 
 * working hardware.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    post_silicon_transition,
    "Will new materials (Carbon nanotubes/Quantum) maintain the 2-year 
    doubling pace once silicon hits the atomic limit?",
    resolution_mechanism("Observation of commercial chip density post-2nm node"),
    impact("If Yes: Moore's Law remains a Rope/Snare. If No: It hits the Mountain."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Software Optimization (Wirth's Law)
 * Viability: High. If software was written efficiently, we wouldn't 
 * need a hardware doubling every 2 years.
 * Suppression: High. Developers prefer "easy" code enabled by fast 
 * hardware over difficult optimization.
 * * ALTERNATIVE 2: Specialized Architecture (ASICs/FPGAs)
 * Viability: High. Moving away from general-purpose CPUs.
 * * CONCLUSION:
 * The "Snare" of Moore's Law is partly voluntary; we could exit via 
 * better software, but the "Rope" of free hardware gains is too seductive.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_moores_law].
 * 2. Multi-perspective report: ?- multi_index_report(moores_law).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Multifile declarations for new predicates ---
:- multifile
    domain_priors:theater_ratio/2.

% --- Theater ratio (missing from base properties) ---
% Technical constraint — mostly substantive, minimal implementation theater
domain_priors:theater_ratio(moores_law, 0.02).
narrative_ontology:constraint_metric(moores_law, theater_ratio, 0.02).
