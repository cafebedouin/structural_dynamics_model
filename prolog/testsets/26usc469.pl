% ============================================================================
% CONSTRAINT STORY: tax_code_section_469
% ============================================================================
% Generated: 2026-01-16
% Model: Gemini 1.5 Pro
% Source: 26 USC 469: Passive activity losses and credits limited
% ============================================================================

:- module(tax_code_section_469, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

% Structural Anchor for System Extraction
narrative_ontology:interval(tax_code_section_469, 0, 10).

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: tax_code_section_469
 * human_readable: Passive Activity Loss (PAL) Limitations
 * domain: economic/political
 * temporal_scope: Post-1986 (Tax Reform Act of 1986)
 * spatial_scope: National (United States)
 * * SUMMARY:
 * Section 469 restricts taxpayers from using losses or credits from "passive activities" 
 * (trades or businesses in which they do not materially participate) to offset "active" 
 * income like wages or "portfolio" income like dividends. It essentially "silos" 
 * investment losses, preventing them from being used to lower a taxpayer's overall tax 
 * liability unless they have matching passive income.
 * * KEY AGENTS:
 * - Small_Investor: Individual with limited capital, often trapped by lack of "material participation."
 * - Real_Estate_Professional: Taxpayers who meet high hour thresholds to bypass PAL limits.
 * - IRS_Institutional: The state body enforcing the compartmentalization of income.
 * * NARRATIVE ARC:
 * Originally enacted to curb tax shelters, the constraint creates a barrier for 
 * casual investors while providing specific "exits" for those with the time or 
 * scale to qualify as professionals or maintain a portfolio of varied passive interests.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Base extractiveness: Moderate (0.45)
% It prevents wealth preservation via losses but fund the state.
domain_priors:base_extractiveness(tax_code_section_469, 0.45).

% Suppression: High (0.75)
% Alternatives (like direct offsetting) are legally prohibited and carry penalties.
domain_priors:suppression_score(tax_code_section_469, 0.75).

% Enforcement: Requires active maintenance (audit, reporting).
domain_priors:requires_active_enforcement(tax_code_section_469).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: Small Investor - NOOSE
   --------------------------------------------------------------------------
   WHO: individual_powerless - Lacks the 750+ hours required for professional status.
   WHEN: biographical - A 30-year investment horizon is dictated by these rules.
   WHERE: trapped - Cannot simply "exit" the tax code while residing in the US.
   SCOPE: national - Governed by federal law.
   
   WHY THIS CLASSIFICATION:
   For the small investor, Section 469 is a Noose because it unilaterally disallows 
   legitimate economic losses from being recognized for tax purposes simply because 
   the investor has a "day job." It extracts value by denying deductions that 
   would be available to larger, specialized players.
   
   NARRATIVE EVIDENCE:
   "neither— (A) the passive activity loss, nor (B) the passive activity credit, 
   for the taxable year shall be allowed." 
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    tax_code_section_469,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(tax_code_section_469, E), E > 0.4,
    domain_priors:suppression_score(tax_code_section_469, S), S > 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: Real Estate Professional - ROPE
   --------------------------------------------------------------------------
   WHO: individual_moderate - Has the agency to structure their life to meet legal thresholds.
   WHEN: immediate - Adjusts participation hours annually.
   WHERE: mobile - Professional status provides an "exit" from the PAL limitation.
   SCOPE: local/regional - Focuses on their specific property portfolio.
   
   WHY THIS CLASSIFICATION:
   For the professional, the rule is a Rope—a functional coordination mechanism. 
   While it imposes a burden (750 hours of service), it provides a clear 
   pathway to bypass the restriction, effectively acting as a professional 
   standard that coordinates tax benefits with actual economic labor.
   
   NARRATIVE EVIDENCE:
   "paragraph (2) shall not apply to any rental real estate activity of such 
   taxpayer... if more than 750 hours of services during the taxable year... 
   are performed." 
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    tax_code_section_469,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :-
    domain_priors:requires_active_enforcement(tax_code_section_469),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: IRS/Institutional - MOUNTAIN
   --------------------------------------------------------------------------
   WHO: institutional - Rule-making and enforcement power.
   WHEN: civilizational - Viewed as a permanent pillar of post-1986 tax logic.
   WHERE: analytical - Objective application of the code.
   SCOPE: global - Affects all US citizens regardless of location.
   
   WHY THIS CLASSIFICATION:
   From the perspective of the institution, Section 469 is a Mountain—an 
   unchangeable fact of the legal landscape. It is not an "extraction" but a 
   natural boundary of what constitutes a "passive activity," necessary to 
   prevent the "erosion" of the tax base by artificial tax shelters.
   
   NARRATIVE EVIDENCE:
   "The term 'passive activity' means any activity— (A) which involves the 
   conduct of any trade or business, and (B) in which the taxpayer does not 
   materially participate." 
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    tax_code_section_469,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(national)
    )
) :-
    domain_priors:suppression_score(tax_code_section_469, S), S > 0.7,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(tax_code_section_469_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(tax_code_section_469, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(tax_code_section_469, Type2, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(tax_code_section_469, Type3, context(agent_power(institutional), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

test(professional_exit_validity) :-
    % Test that mobility/exit options change the classification from Noose to Rope
    constraint_indexing:constraint_classification(tax_code_section_469, noose, context(_, _, trapped, _)),
    constraint_indexing:constraint_classification(tax_code_section_469, rope, context(_, _, mobile, _)).

test(time_horizon_perception) :-
    % Institutional view sees the law as civilizational (Mountain)
    constraint_indexing:constraint_classification(tax_code_section_469, mountain, context(_, time_horizon(civilizational), _, _)).

:- end_tests(tax_code_section_469_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 1.5 Pro
 * Date: 2026-01-16
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.45):
 * Reasoning: While Section 469 prevents tax avoidance (benefiting the collective state), 
 * it creates a "tax on losses" for those without the liquidity to wait for 
 * future passive income, representing a moderate asymmetric flow.
 * * 2. SUPPRESSION SCORE (0.75):
 * Reasoning: The law explicitly defines "material participation" in a way that 
 * excludes most alternative definitions of involvement. Alternatives like 
 * "aggregate netting" are strictly suppressed by subsection (a).
 * * 3. PERSPECTIVE SELECTION:
 * Chose Small Investor (Powerless), Real Estate Pro (Moderate/Mobile), and 
 * IRS (Institutional) to highlight the gap between those crushed by the rule 
 * and those who use it as a regulatory barrier to entry.
 * * 4. CONFIDENCE:
 * High: Legal definitions and the "Noose" vs "Rope" distinction based on professional status.
 * Medium: Extractiveness score (subjective based on economic philosophy).
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Pre-1986 System (Universal Netting)
 * Viability: Historically proven, allowed all business losses to offset any income.
 * Suppression: Explicitly replaced by the 1986 Act to kill the tax shelter industry.
 * * ALTERNATIVE 2: Pro-Rata Participation
 * Viability: Allowing a percentage of losses based on a percentage of hours worked.
 * Suppression: Rejected in favor of "all-or-nothing" thresholds (750 hours).
 * * CONCLUSION:
 * The existence of suppressed alternatives (especially the previous system) 
 * reinforces the "Noose" classification for the powerless individual who 
 * cannot meet the arbitrary 750-hour "Rope" requirement.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [tax_code_section_469].
 * 2. Run tests: ?- run_tests(tax_code_section_469_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
