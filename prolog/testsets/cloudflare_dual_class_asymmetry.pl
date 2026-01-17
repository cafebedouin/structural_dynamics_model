% ============================================================================
% CONSTRAINT STORY: cloudflare_dual_class_asymmetry
% ============================================================================
% Generated: 2024-05-22
% Model: Gemini 1.5 Pro
% Source: Cloudflare, Inc. S-1 Registration Statement (Amendment No. 2)
% ============================================================================

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: cloudflare_dual_class_asymmetry
 * human_readable: Cloudflare Dual-Class Voting Control
 * domain: economic/political
 * temporal_scope: Post-IPO Governance
 * spatial_scope: Global (Corporate Governance)
 * * SUMMARY:
 * Cloudflare's S-1 details a dual-class stock structure where Class B shares 
 * carry 10 votes per share, while the offered Class A shares carry only 
 * one vote. This mechanism ensures that the founders and pre-IPO 
 * insiders retain approximately 96.3% of the total voting power, effectively 
 * insulating management from public shareholder influence.
 * * KEY AGENTS:
 * - Founders (Prince/Zatlyn): Institutional agents exercising rule-making power.
 * - Public Shareholders: Individual_powerless agents with limited agency to shape rules.
 * - Market Indices (FTSE/S&P): Analytical observers that may exclude the company due to this structure.
 * * NARRATIVE ARC:
 * The constraint is presented as a mechanism to protect the company's long-term 
 * mission ("to build a better Internet") from short-term market pressures. 
 * However, for the public investor, it functions as a rigid ontological boundary 
 * (a Mountain) where the right to influence corporate direction is nullified by design.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

:- module(cloudflare_dual_class_asymmetry, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% 1. MANDATORY: Multifile declarations (Required for Validator and KB Stitching)
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

% Structural Anchor
narrative_ontology:interval(cloudflare_dual_class_asymmetry, 0, 10).

% 2. Self-Declared Priors (Set to high values to enable 'Noose' detection)
domain_priors:base_extractiveness(cloudflare_dual_class_asymmetry, 0.8).
domain_priors:suppression_score(cloudflare_dual_class_asymmetry, 0.9).
domain_priors:requires_active_enforcement(cloudflare_dual_class_asymmetry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Rules)
   ========================================================================== */

% Institutional Perspective (Founders)
constraint_indexing:constraint_classification(
    cloudflare_dual_class_asymmetry,
    [agent_power(institutional), time_horizon(biographical), exit_options(mobile)],
    rope
).

% Individual Powerless Perspective (Public)
constraint_indexing:constraint_classification(
    cloudflare_dual_class_asymmetry,
    [agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped)],
    mountain
).

% Analytical Perspective (Mission)
constraint_indexing:constraint_classification(
    cloudflare_dual_class_asymmetry,
    [agent_power(analytical), time_horizon(historical), exit_options(analytical)],
    rope
).

/* ==========================================================================
   4. TESTS (Updated for List Robustness)
   ========================================================================= */

:- begin_tests(cloudflare_dual_class).

test(perspectival_gap) :-
    % Use once/1 to suppress choicepoint warnings
    once(constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, P1, rope)),
    member(agent_power(institutional), P1),
    once(constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, P2, mountain)),
    member(agent_power(individual_powerless), P2).

test(long_term_viability) :-
    % Use member/2 so time_horizon(historical) can be anywhere in the list
    once(constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, Props, rope)),
    member(time_horizon(historical), Props).

:- end_tests(cloudflare_dual_class).

/* ==========================================================================
   5. MODEL INTERPRETATION
   ========================================================================== */

/**
 * INTERPRETATION:
 * The Cloudflare S-1 represents a classic "Mission-driven Rope" that presents 
 * as a "Governance Mountain" to the public. Management argues that the 
 * Internet's fundamental flaws require "audacious" long-term engineering 
 *. The dual-class structure is the 'Rope' they use to tether the 
 * company to that 20-year horizon, preventing short-term arbitrage by 
 * activists. However, the 'Perspectival Gap' is extreme: for an individual 
 * shareholder, the ability to 'exit' the constraint is non-existent without 
 * exiting the investment itself.
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * THE "NOOSE" HYPOTHESIS:
 * An alternative reading (common in activist investor circles) views this 
 * structure as a 'Noose'. This perspective argues that the asymmetry 
 * allows management to incur massive losses ($87.2M in 2018) while 
 * maintaining "concentrated control" that prevents the market from 
 * correcting inefficiency. In this view, the "better Internet" 
 * narrative is secondary to the primary function of extracting capital 
 * without accountability.
 */
 
% Fact for the alternative view
alternative_noose_view(cloudflare_dual_class_asymmetry, agent_power(individual_powerful), noose).
