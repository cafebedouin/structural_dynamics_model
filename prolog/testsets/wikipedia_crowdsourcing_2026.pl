% ============================================================================
% CONSTRAINT STORY: wikipedia_crowdsourcing_2026
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "Crowdsourcing Wikipedia’s encyclopedia" by Matthew Sparkes
% ============================================================================

:- module(wikipedia_crowdsourcing_2026, []).

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
 * * constraint_id: wikipedia_crowdsourcing_2026
 * human_readable: Wikipedia Crowdsourced Knowledge Governance
 * domain: technological/social/political
 * temporal_scope: 2001-2026
 * spatial_scope: Global (Internet-wide)
 * * SUMMARY:
 * Wikipedia operates as a massive, open-access knowledge repository that functions 
 * through radical collaboration rather than typical internet hostility. 
 * It constrains knowledge production through a "clever system" that rewards 
 * responsible editing history with increased administrative powers.
 * * KEY AGENTS:
 * - Wikimedia Foundation: (Institutional) The non-profit that maintains the infrastructure.
 * - Volunteer Editors (260,000+): (Collective/Moderate) The workforce performing 342 edits/minute.
 * - The Critical Founder (Larry Sanger): (Analytical/Dissenting) Claims the site is "hijacked by ideologues".
 * * NARRATIVE ARC:
 * Established in 2001, Wikipedia evolved from a theoretical impossibility into 
 * the 9th-most visited website globally. It manages the tension between 
 * open access ("anyone can edit") and trusted verification ("clever system of powers").
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(wikipedia_crowdsourcing_2026, 0, 10).
narrative_ontology:constraint_claim([wikipedia_crowdsourcing_2026], [decentralized_governance]).

% Base extractiveness score (0.05 = Extremely Low)
% Rationale: Wikipedia is a non-profit repository that offers information 
% freely to 15 billion visits/month. Extraction is minimal, though 
% volunteer labor is "extracted" in exchange for reputation/status.
domain_priors:base_extractiveness(wikipedia_crowdsourcing_2026, 0.05).

% Suppression score (0.4 = Moderate)
% Rationale: Wikipedia suppresses "fragmented, unverified, unreliable" 
% information in favor of vetted articles. Dissenting views 
% (like Sanger's) are visible but marginalized by the dominant consensus.
domain_priors:suppression_score(wikipedia_crowdsourcing_2026, 0.4).

% Enforcement requirements
% Requires active enforcement (The "clever system" of editor hierarchy).
domain_priors:requires_active_enforcement(wikipedia_crowdsourcing_2026).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(wikipedia_crowdsourcing_2026, extractiveness, 0.05).
narrative_ontology:constraint_metric(wikipedia_crowdsourcing_2026, suppression_requirement, 0.4).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(wikipedia_crowdsourcing_2026, global_internet_users). % 15 billion visits monthly
constraint_victim(wikipedia_crowdsourcing_2026, unverified_information_peddlers). % Site bucks the trend of unverified data

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE WIKIMEDIA FOUNDATION (Anusha Alikhan) - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-making power)
   WHEN: biographical (Managing 25+ years of growth)
   WHERE: arbitrage (Navigating 300+ languages and global legalities)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For the Foundation, the system is a "Rope"—a functional coordination 
   mechanism that "works in practice" to manage 64 million articles 
   without collapse.
   
   NARRATIVE EVIDENCE:
   "Thank God it works in practice, because it would never work in theory".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    wikipedia_crowdsourcing_2026,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE ANONYMOUS NEW EDITOR - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (A new volunteer with no history)
   WHEN: immediate (A single session/edit attempt)
   WHERE: constrained (Cannot access "wider editing powers" yet)
   SCOPE: local (A single article entry)
   
   WHY THIS CLASSIFICATION:
   A new editor faces a "Noose" of hierarchy. They are "anyone who can edit," 
   yet their changes are subject to immediate suppression by the "clever 
   system" of established power-users.
   
   NARRATIVE EVIDENCE:
   "Grants wider editing powers to volunteers once they have built a history 
   of responsible changes".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    wikipedia_crowdsourcing_2026,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE DISENCHANTED FOUNDER (Larry Sanger) - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer/Critic)
   WHEN: historical (From 2001 to 2026)
   WHERE: mobile (Has left the project but continues to critique)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To a critic like Sanger, the "ideological hijacking" is a "Mountain"—an 
   immovable, flawed state that defines the project’s reality and cannot 
   be reformed from within.
   
   NARRATIVE EVIDENCE:
   The site has been "hijacked by ideologues".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    wikipedia_crowdsourcing_2026,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(wikipedia_crowdsourcing_2026_tests).

test(multi_perspective_trust) :-
    constraint_indexing:constraint_classification(wikipedia_crowdsourcing_2026, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(wikipedia_crowdsourcing_2026, noose, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(wikipedia_crowdsourcing_2026, mountain, context(agent_power(analytical), _, _, _)).

test(edit_frequency_governance) :-
    % 342 edits/min requires high suppression/coordination scores to avoid chaos.
    domain_priors:suppression_score(wikipedia_crowdsourcing_2026, S),
    S > 0.3.

:- end_tests(wikipedia_crowdsourcing_2026_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.05):
 * Reasoning: Wikipedia is nearly non-extractive in terms of monetary gain, 
 * but the "currency" of the system is trust and history.
 * * 2. PERSPECTIVE SELECTION:
 * Analyzed the tension between the "Institutional" success (Alikhan), the 
 * "Powerless" barrier to entry (New editor), and the "Analytical" critique 
 * of ideology (Sanger).
 * * 3. AMBIGUITIES:
 * The text mentions the "clever system" without detailing the specific 
 * algorithms or human-mediated checks, creating an Omega around 
 * the true nature of its power distribution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wikipedia_governance_bottleneck,
    "Is the hierarchy of editing power a meritocracy (Rope) or a self-reinforcing clique (Noose)?",
    resolution_mechanism("Audit of administrative turnover rates vs. consensus-blocking behaviors"),
    impact("If clique: High extractiveness for new labor. If meritocracy: Scalable Rope."),
    confidence_without_resolution(medium)
).

omega_variable(
    wikipedia_ideological_hijacking,
    "Does consensus on Wikipedia represent 'Truth' (Mountain) or 'Dominant Ideology' (Noose)?",
    resolution_mechanism("Comparative analysis of neutral-point-of-view (NPOV) adherence across controversial topics"),
    impact("If ideology: Suppression score increases. If Truth: Becomes a biological/logical Mountain."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Expert-Led Encyclopedias (Nupedia/Britannica)
 * Viability: Historically the "theory" that should have worked.
 * Suppression: Outscaled by Wikipedia's speed and volume.
 * * ALTERNATIVE 2: Unfettered Free-for-all
 * Viability: The standard "internet discord" model.
 * Suppression: Actively rejected by Wikipedia's "clever system" of trust.
 * * CONCLUSION:
 * Wikipedia succeeded by turning the "Chaos" of the internet (Noose) into a 
 * "Vetted Consensus" (Rope).
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [wikipedia_crowdsourcing_2026].
% Report: ?- multi_index_report(wikipedia_crowdsourcing_2026).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
