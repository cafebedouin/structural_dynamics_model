% ============================================================================
% CONSTRAINT STORY: dldr_information_policy
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "Don’t Like, Don’t Read" from cafebedouin.org
% ============================================================================

:- module(constraint_dldr_information_policy, []).

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
 * * constraint_id: dldr_information_policy
 * human_readable: "Don’t Like, Don’t Read" (DLDR) Information Tagging
 * domain: technological/social
 * temporal_scope: Digital Era / 2020-2026
 * spatial_scope: Online Archives (AO3) and Curated Media
 * * SUMMARY:
 * This constraint represents the "Don’t Like, Don’t Read" (DLDR) policy, where the 
 * responsibility (onus) for content protection shifts from the author or platform 
 * to the reader. It relies on a granular tagging system that 
 * allows users to make a "rough determination" of whether content is suitable 
 * before engagement.
 * * KEY AGENTS:
 * - The Archive User (Reader): Responsible for protecting themselves using provided 
 * tags.
 * - The Content Creator (Author): Provides metadata/tags to signal potentially 
 * "triggering" content.
 * - The Mainstream Gatekeeper: An institutional agent that "sanitizes" environments, 
 * making tags appear unnecessary by erasing challenging content.
 * * NARRATIVE ARC:
 * In an open information environment like AO3, users encounter unsanitized reality 
 *. The DLDR policy functions as a tool for autonomy, 
 * contrasting with mainstream "sanitized" environments where the lack of tags 
 * indicates that the environment has already been filtered for the user.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(dldr_policy_interval, 0, 10).
narrative_ontology:constraint_claim([dldr_information_policy], [information_architecture]).

% Base extractiveness score (0.35 = Moderate-Low)
% Rationale: It extracts a "tax" of labor (tagging) from creators and cognitive 
% vetting from readers. However, it prevents the higher 
% extraction of agency found in sanitized systems.
domain_priors:base_extractiveness(dldr_information_policy, 0.35).

% Suppression score (0.2 = Low)
% Rationale: In a DLDR system, content is not hidden; it is merely signaled. 
% The suppression of challenging content is what the DLDR policy actively 
% resists.
domain_priors:suppression_score(dldr_information_policy, 0.2).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(dldr_information_policy, extractiveness, 0.35).
narrative_ontology:constraint_metric(dldr_information_policy, suppression_requirement, 0.2).

% Enforcement: Requires active maintenance of tagging taxonomies.
domain_priors:requires_active_enforcement(dldr_information_policy).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(dldr_information_policy, user_autonomy). % Readers choose their exposure.
constraint_beneficiary(dldr_information_policy, archive_plurality). % Allows unsanitized content to exist.
constraint_victim(dldr_information_policy, mainstream_sanitization). % Challenges the "sanitized" status quo.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE AO3 READER - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Has agency to "don't read" based on info)
   WHEN: immediate (Real-time decision making during browsing)
   WHERE: mobile (Can navigate away or filter tags)
   SCOPE: local (Immediate engagement with a story)
   
   WHY THIS CLASSIFICATION:
   For the reader in an unsanitized environment, the DLDR policy is a "Rope"—a 
   functional coordination tool that allows them to safely navigate potentially 
   hazardous information without requiring a central authority to hide it 
  .
   
   NARRATIVE EVIDENCE:
   "The responsibility lies with the reader... you can tell in advance and 
   not read it".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    dldr_information_policy,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(mobile),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE MAINSTREAM CONSUMER - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless (Subject to environments "sanitized" by others)
   WHEN: biographical (Long-term exposure to filtered reality)
   WHERE: trapped (Often unaware of what is missing from their feed)
   SCOPE: national (Broadcast and mainstream media limits)
   
   WHY THIS CLASSIFICATION:
   For those in sanitized environments, the *lack* of tagging is a "Snare." 
   They are trapped in a world where challenging content is erased before they 
   can choose to see it, extracting their agency to determine their own 
   "trigger" thresholds.
   
   NARRATIVE EVIDENCE:
   "The lack of tags indicates that the environment has been sanitized... 
   people rarely miss what's missing".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dldr_information_policy,
    snare,
    context(
        agent_power(powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE PLATFORM GATEKEEPER - Mountain
   --------------------------------------------------------------------------
   
   WHO: institutional (The authority that "sanitizes" the environment)
   WHEN: historical (The established norms of "maturity and age" ratings)
   WHERE: constrained (Bound by the "interests" of the status quo)
   SCOPE: global (Mass media standardization)
   
   WHY THIS CLASSIFICATION:
   To the gatekeeper, sanitization is a "Mountain"—an unchangeable 
   necessity of "mainstream" culture. They view challenging 
   films or stories not as things to be tagged, but as things that simply 
   "don't get made".
   
   NARRATIVE EVIDENCE:
   "we don’t need a better system of tags because challenging films... 
   largely don’t get made".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dldr_information_policy,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(constrained),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(dldr_information_policy_tests).

test(multi_perspective_tagging) :-
    % Reader sees Rope (Autonomy)
    constraint_indexing:constraint_classification(dldr_information_policy, rope, context(individual_moderate, _, _, _)),
    % Mainstream Subject sees Snare (Sanitization/Loss of Agency)
    constraint_indexing:constraint_classification(dldr_information_policy, snare, context(powerless, _, _, _)),
    % Institutional Gatekeeper sees Mountain (The way things are)
    constraint_indexing:constraint_classification(dldr_information_policy, mountain, context(institutional, _, _, _)).

test(sanitization_extraction) :-
    % Shows that powerless (mainstream consumer) experiences more 
    % extraction of agency than the AO3 reader.
    constraint_indexing:extractiveness_for_agent(dldr_information_policy, context(powerless, biographical, trapped, national), E1),
    constraint_indexing:extractiveness_for_agent(dldr_information_policy, context(individual_moderate, immediate, mobile, local), E2),
    E1 > E2.

:- end_tests(dldr_information_policy_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS SCORE (0.35):
 * Reasoning: The "cost" of the DLDR system is the vigilance required by the 
 * user and the labor of tagging by the creator. 
 * The source contrasts this with the "invisible" extraction of 
 * sanitization.
 * * 2. PERSPECTIVE SELECTION:
 * Chose to contrast the AO3 reader (Rope) with the mainstream consumer 
 * (Snare) to highlight the source's point that "the onus was on the 
 * reader".
 * * 3. AMBIGUITIES:
 * The source notes that mainstream ratings like "R" are based on "maturity 
 * and age" rather than specific triggers like those found in *Oldboy* *. I resolved this by classifying mainstream curation 
 * as an institutional "Mountain".
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sanitization_beneficiary_intent,
    "Whose interests does sanitization serve, and is it a functional necessity (Mountain) or a predatory control (Snare)?",
    resolution_mechanism("Audit of content-erasure patterns across mainstream media compared to unsanitized archives"),
    impact("If necessity: Sanitization is a Rope. If control: It is a Snare."),
    confidence_without_resolution(medium)
).

omega_variable(
    tagging_efficacy_ceiling,
    "Does granular tagging actually protect users (Rope) or does it facilitate 
     trauma-tourism (Snare)?",
    resolution_mechanism("Longitudinal study of AO3 user behavior and psychological impact of tag engagement"),
    impact("If protective: Rope. If facilitating harm: Snare."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Universal Sanitization
 * Viability: The current mainstream media standard.
 * Suppression: Actively rejected by archives like AO3 which prioritize 
 * "Don't like, don't read" policies.
 * Evidence: Mainstream sources are "largely sanitized of content".
 * * ALTERNATIVE 2: Pure Raw Unfiltered Chaos
 * Viability: Information environments with zero tags (e.g., early internet).
 * Suppression: Shunted because without " rough determination," the 
 * individual cannot protect themselves.
 * * CONCLUSION:
 * The presence of the DLDR policy as an alternative reveals that mainstream 
 * sanitization is a "Snare" masquerading as a "Mountain" of safety.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [constraint_dldr_information_policy].
% Run analysis: ?- constraint_indexing:multi_index_report(dldr_information_policy).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
