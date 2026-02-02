% ============================================================================
% CONSTRAINT STORY: '26usc469'
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: 26 USC 469: Passive activity losses and credits limited 
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_26usc469, []).

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
 * 
 * constraint_id: '26usc469'
 * human_readable: Passive Activity Loss Limitation (Section 469)
 * domain: legal/economic
 * temporal_scope: 1986 - Present
 * spatial_scope: National (US Tax Code)
 * 
 * SUMMARY:
 * Section 469 of the U.S. tax code generally prevents taxpayers from deducting
 * "passive losses" (e.g., from real estate rentals where the taxpayer is not
 * "materially participating") against "active income" (e.g., salary).
 * It aims to prevent the erosion of the tax base via artificial tax shelters.
 * 
 * KEY AGENTS:
 * - Casual Passive Investor (Individual Powerless): Owns rental property but cannot deduct losses.
 * - IRS (Institutional): Enforces the tax code to maintain tax base stability.
 * - Tax Policy Analyst (Analytical): Observes the economic and behavioral effects of the law.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval('26usc469', 0, 10).
narrative_ontology:constraint_claim('26usc469', tangled_rope).

% Base extractiveness (0.72): Disallows legitimate economic losses for 
% passive investors while favoring full-time professionals. It extracts capital
% from those who cannot meet "material participation" thresholds.
domain_priors:base_extractiveness('26usc469', 0.72).

% Suppression (0.85): Restricts alternative participation definitions, 
% making non-standard accounting illegal and effectively suppressing tax sheltering.
domain_priors:suppression_score('26usc469', 0.85).

% Enforcement: Requires active enforcement by the IRS during audits.
domain_priors:requires_active_enforcement('26usc469').

% BENEFICIARIES & VICTIMS
constraint_beneficiary('26usc469', us_treasury).
constraint_victim('26usc469', individual_passive_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: CASUAL PASSIVE INVESTOR - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless - Cannot meet "material participation" thresholds.
   WHEN: biographical - Stuck with the rule for the life of the investment.
   WHERE: trapped - Cannot deduct legitimate losses against active income.
   
   WHY THIS CLASSIFICATION:
   For the small passive investor, Section 469 is a 'Snare'. It prevents them
   from deducting legitimate economic losses from their passive investments,
   even if they are losing money. This strangles their ability to engage in
   tax-efficient wealth building.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    '26usc469',
    snare,
    context(agent_power(individual_powerless), time_horizon(biographical), exit_options(trapped), spatial_scope(national))
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: IRS - Rope
   --------------------------------------------------------------------------
   WHO: institutional - Enforces the tax code to maintain tax base stability.
   WHEN: historical - Administering tax laws since 1986.
   WHERE: arbitrage - Can audit and enforce the rules against tax shelters.
   
   WHY THIS CLASSIFICATION:
   For the IRS, Section 469 is a 'Rope'. It is a crucial tool for maintaining
   tax-base stability and preventing wealthy individuals from using artificial
   losses to evade taxes. It coordinates fair tax collection.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    '26usc469',
    rope,
    context(agent_power(institutional), time_horizon(historical), exit_options(arbitrage), spatial_scope(national))
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: TAX POLICY ANALYST - Tangled Rope
   --------------------------------------------------------------------------
   WHO: analytical - Observes the economic and behavioral effects of the law.
   WHEN: historical - Analyzing policy impact over decades.
   WHERE: analytical - Evaluates the trade-offs of the legislation.
   
   WHY THIS CLASSIFICATION:
   For a tax policy analyst, Section 469 is a 'Tangled Rope'. It's a 'Rope'
   because it successfully curbs tax shelters and protects the tax base. It's
   'Tangled' because it does so by creating complex rules that disproportionately
   affect certain investors and can lead to unintended economic distortions.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    '26usc469',
    tangled_rope,
    context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(national))
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(tax_code_section_469_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification('26usc469', Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification('26usc469', Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification('26usc469', Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(tax_code_section_469_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini Pro (Revised)
 * Date: 2026-01-23
 * 
 * KEY DECISIONS:
 * 
 * 1. INSTITUTIONAL PERSPECTIVE: Added the 'IRS' as the institutional agent.
 *    For them, the rule is a crucial 'Rope' for tax administration and fairness.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Investor (Snare): Cannot deduct legitimate losses.
 *    - IRS (Rope): A tool for tax base stability.
 *    - Analyst (Tangled Rope): Balances benefits of preventing tax shelters with unintended complexity.
 * 
 * 3. MANDATROPHY STATUS: High extractiveness (0.72) is 'RESOLVED' because
 *    the intention is to prevent tax base erosion, which is a societal benefit.
 *    The 'Snare' experienced by passive investors is a consequence of this broader goal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * Mandatory Omega for high-extraction constraints.
 */

omega_variable(
    tax_code_469_extraction_intent,
    "Is the 'material participation' requirement a functional necessity for tax-base stability or a predatory moat for full-time incumbents in certain industries (e.g., real estate)?",
    resolution_mechanism("Audit of legislative history and lobbyist influence in the 1986 Tax Reform Act; comparative impact analysis of small vs. large passive investors."),
    impact("If necessity: 'Mountain'. If predatory choice: 'Snare/Mandatrophy'."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Unlimited Passive Loss Deduction
 *    Viability: Historically, this was the system prior to 1986.
 *    Suppression: Rejected due to widespread abuse and erosion of the tax base via tax shelters.
 *
 * CONCLUSION:
 * Section 469 is a 'Tangled Rope' that attempts to strike a balance between
 * preventing tax evasion and allowing legitimate business losses. The
 * suppression of unlimited passive loss deduction was a necessary step
 * to protect the tax base, but its implementation created a 'Snare' for
 * some passive investors.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/'26usc469'].
 * 2. Multi-perspective: ?- multi_index_report('26usc469').
 * 3. Run tests: ?- run_tests(tax_code_section_469_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
