% ============================================================================
% CONSTRAINT STORY: social_media_participation_threshold
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Americans' Social Media Use 2025 (Pew Research Center)
% ============================================================================

:- module(constraint_social_media_2025, []).

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
 * * constraint_id: social_media_participation_threshold
 * human_readable: The 2025 Digital Participation Threshold
 * domain: social/technological
 * temporal_scope: 2025
 * spatial_scope: National (United States)
 * * SUMMARY:
 * As of 2025, social media has reached a saturation point where certain platforms
 * function as mandatory infrastructure for social and informational participation.
 * [cite_start]YouTube (84%) and Facebook (71%) serve as the primary coordination hubs[cite: 50],
 * creating a high-friction environment for those attempting to remain "unplugged"
 * [cite_start]from the digital social record[cite: 47].
 * * KEY AGENTS:
 * - Digitally Native (Ages 18-29): Individual powerless; for whom platform
 * [cite_start]usage is a non-negotiable landscape for social survival[cite: 130].
 * - Institutional Strategist (Brand/Org): Uses platforms as a functional
 * [cite_start]coordination mechanism to target specific demographic segments[cite: 201].
 * - Analytical Observer (Pew Researcher): Tracks the systemic shifts in
 * [cite_start]mode and the widening demographic "age gaps"[cite: 30, 128].
 * * NARRATIVE ARC:
 * What began as a Rope (a helpful tool for finding friends or content) has
 * bifurcated. For institutional agents, it remains a Rope—a way to weave
 * [cite_start]narratives and reach audiences[cite: 201]. For younger adults, it has
 * [cite_start]become a Mountain—the physical terrain of their social life[cite: 130].
 * For those concerned with data/attention extraction, it is a Snare that
 * [cite_start]chokes off non-algorithmic social alternatives[cite: 47, 238].
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the Deferential Realism suite
narrative_ontology:interval(social_media_2025_cycle, 0, 10).
narrative_ontology:constraint_claim(social_media_participation_threshold, snare).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.6. High extraction of attention and behavioral data;
% 37% of Facebook users visit "several times a day"[cite: 242, 247].
domain_priors:base_extractiveness(social_media_participation_threshold, 0.6).

% Suppression score (0.0-1.0)
% Rationale: 0.5. Moderate suppression of non-digital alternatives;
% participants use a "range" of platforms daily, effectively crowding
% out analog social time[cite: 47, 74].
domain_priors:suppression_score(social_media_participation_threshold, 0.5).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(social_media_participation_threshold, extractiveness, 0.6).
narrative_ontology:constraint_metric(social_media_participation_threshold, suppression_requirement, 0.5).

% Enforcement requirements
% Not state-enforced, but "socially enforced" through network effects
% and the "burden of proof" on individuals to stay connected[cite: 47, 129].
domain_priors:emerges_naturally(social_media_participation_threshold).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(social_media_participation_threshold, platform_architects).
constraint_beneficiary(social_media_participation_threshold, demographic_advertisers).
constraint_victim(social_media_participation_threshold, individual_attention_span).
constraint_victim(social_media_participation_threshold, non_digital_social_networks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: YOUNG ADULT (18-29) - Mountain
   --------------------------------------------------------------------------

   [cite_start]WHO: powerless - Usage is a prerequisite for social belonging[cite: 131].
   [cite_start]WHEN: immediate - 95% use YouTube and 63% use TikTok[cite: 225].
   [cite_start]WHERE: trapped - Bound by the "network effect" of their peer group[cite: 130].
   SCOPE: local - Immediate social coordination and peer validation.

   WHY THIS CLASSIFICATION:
   [cite_start]For this group, Instagram (80% use) is "nature"[cite: 142]. It is an
   unchangeable feature of the social landscape. There is no viable
   analog alternative that offers the same "speed" of coordination.
   -------------------------------------------------------------------------- */


constraint_indexing:constraint_classification(
    social_media_participation_threshold,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:suppression_score(social_media_participation_threshold, S),
    S > 0.4,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: INSTITUTIONAL MARKETER - Rope
   --------------------------------------------------------------------------

   [cite_start]WHO: institutional - Strategic agents who shape the "use" of the standard[cite: 201].
   WHEN: biographical - Planning campaigns across fiscal cycles.
   [cite_start]WHERE: mobile - Can pivot between TikTok (growth) and Facebook (stability)[cite: 88, 118].
   SCOPE: national - Managing cross-demographic reach.

   WHY THIS CLASSIFICATION:
   The platforms are a Rope—a functional coordination tool. Marketers use the
   "growing shares" of TikTok (37%) and Instagram (50%) to weave
   [cite_start]commercial narratives[cite: 89, 90]. They see it as a beneficial
   [cite_start]mechanism for population-scale targeting[cite: 200, 214].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    social_media_participation_threshold,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(social_media_participation_threshold, E),
    E < 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: SOCIOLOGIST (PEW RESEARCHER) - Analytical
   --------------------------------------------------------------------------

   [cite_start]WHO: analytical - Observer of the broader demographic "DIFF"[cite: 136, 191].
   [cite_start]WHEN: historical - Tracking changes from 2012 to 2025[cite: 110, 112].
   WHERE: analytical - Not bound by the "addiction" loop, but by the data.
   SCOPE: global - Evaluating the U.S. adult population as a case study.

   WHY THIS CLASSIFICATION:
   The researcher views the system as a Rope that has become a Snare for
   certain groups. The "age gap" of +61 on Instagram suggests that
   [cite_start]the platform is "choking off" cross-generational communication[cite: 140, 141].
   -------------------------------------------------------------------------- */


constraint_indexing:constraint_classification(
    social_media_participation_threshold,
    snare,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(social_media_participation_threshold, E),
    E > 0.5,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(social_media_2025_tests).

test(age_divide_variance) :-
    % Test the massive gap in Instagram use: 18-29 (80%) vs 65+ (19%)
    % Shows that the "Mountain" is only visible to the young.
    constraint_indexing:constraint_classification(social_media_participation_threshold, T1, context(powerless, immediate, trapped, local)),
    T1 = mountain.

test(extraction_frequency_check) :-
    % 48% net daily use on YouTube [cite: 252] indicates high attention extraction.
    domain_priors:base_extractiveness(social_media_participation_threshold, E),
    E > 0.5.

:- end_tests(social_media_2025_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. CLASSIFICATION: I chose 'Snare' as the base claim because the data
 * shows high frequency (several times a day) which mimics an extractive
 * [cite_start]feedback loop[cite: 247, 756].
 * 2. PERSPECTIVE MINIMUMS: I contrasted the young adult's 'Mountain'
 * (inevitability) with the institution's 'Rope' (strategy) to highlight
 * [cite_start]the power asymmetry in digital spaces[cite: 129, 201].
 * 3. OMEGA: The uncertainty remains in how "mode shifts" (phone vs web)
 * [cite_start]might artificially inflate the perception of a constraint[cite: 122].
 */

omega_variable(
    mode_shift_bias,
    "How much does the shift from phone-only (2021) to multi-mode (2025)
     change the perceived 'Mountain' status of these platforms?",
    resolution_mechanism("Longitudinal study comparing single-mode vs multi-mode responses in the same year"),
    impact("If high: The 'Mountain' is a measurement artifact. If low:
            The constraint is a genuine social law."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Specialized Platforms (Reddit/Threads/Bluesky)
 * Viability: Reddit (26%) and Threads (8%) exist as alternatives for
 * [cite_start]specific niches[cite: 65, 71, 73].
 * Suppression: Actively suppressed by the "network effect" of the
 * [cite_start]Facebook/YouTube duopoly[cite: 50, 118].
 * * ALTERNATIVE 2: Analog Social Life
 * Viability: Physical interaction remains viable but is socially
 * [cite_start]"taxed" by the lack of digital visibility/coordination[cite: 47].
 * * CONCLUSION:
 * The existence of niche alternatives (Bluesky/Truth Social) that remain at
 * [cite_start]<5% indicates a "winner-take-all" Mountain/Snare dynamic[cite: 73, 78].
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [constraint_social_media_2025].
% 2. Analyze: ?- constraint_indexing:multi_index_report(social_media_participation_threshold).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
