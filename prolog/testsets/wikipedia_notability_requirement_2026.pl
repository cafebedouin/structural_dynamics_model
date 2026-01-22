% ============================================================================
% CONSTRAINT STORY: wikipedia_notability_requirement_2026
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "Crowdsourcing Wikipedia’s encyclopedia" by Matthew Sparkes
% ============================================================================

:- module(wikipedia_notability_requirement_2026, []).

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
 * * constraint_id: wikipedia_notability_requirement_2026
 * human_readable: Wikipedia Notability Requirement
 * domain: social/technological
 * temporal_scope: 2001-2026
 * spatial_scope: Global (Digital)
 * * SUMMARY:
 * The notability requirement is the gatekeeping policy that determines which topics 
 * merit a dedicated article. It serves to foster "trust on a mass scale" 
 * by ensuring the site remains a verified repository rather than a fragmented 
 * collection of unverified data.
 * * KEY AGENTS:
 * - Admin/Power Editor: Enforcers who use their "wider editing powers" 
 * to delete non-notable content.
 * - New Contributor/Subject: Individuals attempting to establish 
 * a presence within the "clever system" of the encyclopedia.
 * - Wikimedia Foundation: The non-profit overseeing the system 
 * that "works in practice" despite theoretical impossibilities.
 * * NARRATIVE ARC:
 * Wikipedia has transformed into a trusted resource by moving away from the 
 * internet's hallmark of discord and toward a system of verified knowledge. 
 * The notability requirement is the primary tool used by its 260,000 volunteers 
 * to maintain this trust.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(wikipedia_notability_requirement_2026, 0, 10).
narrative_ontology:constraint_claim([wikipedia_notability_requirement_2026], [content_governance]).

% Base extractiveness score (0.4 = Moderate)
% Rationale: It extracts significant "proof-of-work" (labor) from editors 
% to justify a topic's existence. It also "extracts" (denies) the 
% digital presence of those who do not meet the consensus threshold.
domain_priors:base_extractiveness(wikipedia_notability_requirement_2026, 0.4).

% Suppression score (0.7 = High)
% Rationale: Non-notable content is actively deleted and suppressed to keep 
% the site from becoming "unreliable and limited in scope".
domain_priors:suppression_score(wikipedia_notability_requirement_2026, 0.7).

% Enforcement requirements
% Requires active enforcement. 342 edits are made every single minute, 
% many of which involve patrolling and enforcing notability.
domain_priors:requires_active_enforcement(wikipedia_notability_requirement_2026).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(wikipedia_notability_requirement_2026, extractiveness, 0.4).
narrative_ontology:constraint_metric(wikipedia_notability_requirement_2026, suppression_requirement, 0.7).

% BENEFICIARIES & VICTIMS
% Beneficiary: Global readers seeking "trusted resources".
constraint_beneficiary(wikipedia_notability_requirement_2026, global_information_consumers).
% Victim: Niche subjects or new editors whose work is deleted.
constraint_victim(wikipedia_notability_requirement_2026, marginalized_knowledge_producers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: ESTABLISHED ADMIN - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Part of the system that "grants wider editing powers")
   WHEN: biographical (Long-term commitment to "responsible changes")
   WHERE: mobile (Can edit across 300+ languages)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For the admin, notability is a "Rope"—a beneficial tool to filter noise and 
   maintain Wikipedia as one of the world's most "essential knowledge hubs".
   
   NARRATIVE EVIDENCE:
   "Trust fosters engagement and commitment so that strangers... are willing 
   to work together".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    wikipedia_notability_requirement_2026,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: NICHE CREATOR - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (New volunteer with no "history of changes")
   WHEN: immediate (Attempting to save a single article)
   WHERE: trapped (No alternative "trusted resource" of this scale)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For a creator whose history or topic is deemed "non-notable," the policy is 
   a "Noose". It suppresses their perspective based on a consensus 
   they do not control.
   
   NARRATIVE EVIDENCE:
   Larry Sanger notes the site has been "hijacked by ideologues" who may 
   use rules to exclude certain voices.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    wikipedia_notability_requirement_2026,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE INFORMATION ANALYST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observing the "sum total of human knowledge")
   WHEN: historical (Analyzing the internet's hallmarks of discord)
   WHERE: analytical (Independent of the site's power structure)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   From an analytical view, the requirement is a "Mountain"—an unavoidable 
   necessity for any crowdsourced encyclopedia to avoid becoming 
   "unverified [and] unreliable".
   
   NARRATIVE EVIDENCE:
   "It would never work in theory" without these structural constraints.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    wikipedia_notability_requirement_2026,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(wikipedia_notability_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, T1, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, T2, context(agent_power(individual_powerless), _, _, _)),
    T1 \= T2.

test(suppression_trust_link) :-
    % Demonstrates that high suppression (0.7) is required to "buck the trend" 
    % of unreliable internet data.
    domain_priors:suppression_score(wikipedia_notability_requirement_2026, S),
    S > 0.5.

:- end_tests(wikipedia_notability_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.4):
 * Reasoning: While Wikipedia is free, the "notability" barrier is a 
 * high-cost labor requirement for contributors to prove their worth.
 * * 2. SUPPRESSION SCORE (0.7):
 * Reasoning: The article explicitly notes that unverified and unreliable data 
 * is the internet norm; Wikipedia's success depends on suppressing that 
 * norm within its boundaries.
 * * 3. PERSPECTIVE SELECTION:
 * Chose to represent the "Institutional" success of the Foundation 
 * against the "Analytical" skepticism of critics like Sanger.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    notability_ideological_bias,
    "Is notability a neutral filter (Rope) or an ideological weapon (Noose)?",
    resolution_mechanism("Audit of deletion rates for marginalized vs. dominant cultural topics"),
    impact("If weaponized: High suppression + High extraction = Noose for diversity."),
    confidence_without_resolution(medium)
).

omega_variable(
    volunteer_labor_sustainability,
    "Can trust be maintained if the 260,000 volunteers burn out under the 342 edit/minute load?",
    resolution_mechanism("Trend analysis of active editor retention and edit-quality"),
    impact("If unsustainable: The 'clever system' collapses into internet discord."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Inclusionism (Store everything)
 * Viability: Theoretically possible with cheap storage.
 * Suppression: Rejected because it makes the site "unreliable and limited in scope".
 * * ALTERNATIVE 2: Expert-Only Curation
 * Viability: The standard encyclopedia model.
 * Suppression: Rejected because it cannot achieve the scale of 64 million articles.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load into system: ?- [wikipedia_notability_requirement_2026].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
