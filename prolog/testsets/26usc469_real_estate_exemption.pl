% ============================================================================
% CONSTRAINT STORY: '26usc469_real_estate_exemption'
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: 26 USC 469(c)(7) - Special rules for taxpayers in real property business
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption, []).

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
 * constraint_id: '26usc469_real_estate_exemption'
 * human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 * domain: legal/economic
 * temporal_scope: 1993 - Present
 * spatial_scope: National (US Tax Code)
 * 
 * SUMMARY:
 * Section 469 of the tax code generally prevents taxpayers from deducting passive
 * losses against active income. The "(c)(7)" exemption carves out a special
 * exception for "real estate professionals," but defines this status with a
 * strict, two-part test (750 hours AND more than half of personal services).
 * This creates a significant barrier for those with high-income W-2 jobs.
 * 
 * KEY AGENTS:
 * - The Hybrid W-2 Investor (Individual Powerless): Earns a high salary and also invests in real estate.
 * - The Full-Time Developer (Individual Moderate): Meets the professional threshold easily.
 * - The IRS (Institutional): Enforces the clear line drawn by the statute.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval('26usc469_real_estate_exemption', 0, 10).
narrative_ontology:constraint_claim('26usc469_real_estate_exemption', tangled_rope).

% Base extractiveness (0.75): Forces a choice between 750 hours of specific labor 
% or the extraction of tax revenue through disallowed losses.
domain_priors:base_extractiveness('26usc469_real_estate_exemption', 0.75).

% Suppression score (0.80): Explicitly suppresses the counting of non-owner 
% employee hours toward the professional threshold, narrowing the path to qualification.
domain_priors:suppression_score('26usc469_real_estate_exemption', 0.80).

% Enforcement: Requires active enforcement by the IRS during audits.
domain_priors:requires_active_enforcement('26usc469_real_estate_exemption'). 

% BENEFICIARIES & VICTIMS
constraint_beneficiary('26usc469_real_estate_exemption', full_time_real_estate_professionals).
constraint_victim('26usc469_real_estate_exemption', hybrid_w2_investors). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE HYBRID W-2 WORKER - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (A high-earning but time-poor professional)
   WHEN: biographical (Trying to build wealth over a career)
   WHERE: trapped (The "more than half" test is mathematically impossible to meet)
   
   WHY THIS CLASSIFICATION:
   For a doctor, lawyer, or programmer with a demanding W-2 job, the rule is a 'Snare'.
   It's impossible to spend more time on real estate than their primary profession,
   so their legitimate real estate losses are disallowed. The rule strangles their
   ability to benefit from the tax advantages available to full-time professionals.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    '26usc469_real_estate_exemption',
    snare,
    context(agent_power(individual_powerless), time_horizon(biographical), exit_options(trapped), spatial_scope(national))
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE FULL-TIME DEVELOPER - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (A professional whose main work is real estate)
   WHEN: immediate (Filing annual taxes)
   WHERE: mobile (Can easily document hours to qualify)
   
   WHY THIS CLASSIFICATION:
   For the full-time professional, the rule is a 'Rope'. It is a clear, functional
   pathway to classifying themselves as "active" and unlocking significant tax
   benefits by deducting paper losses against their income.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    '26usc469_real_estate_exemption',
    rope,
    context(agent_power(individual_moderate), time_horizon(immediate), exit_options(mobile), spatial_scope(regional))
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE IRS / TREASURY - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Enforcing the tax code)
   WHEN: historical (Administering the law since 1993)
   WHERE: arbitrage (Can audit and enforce the bright-line test)
   
   WHY THIS CLASSIFICATION:
   For the IRS, the rule is a 'Rope'. It creates a clear, objective, and enforceable
   bright-line test (750 hours) to distinguish between active professionals and
   passive hobbyists, simplifying administration and preventing abuse of the tax code.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    '26usc469_real_estate_exemption',
    rope,
    context(agent_power(institutional), time_horizon(historical), exit_options(arbitrage), spatial_scope(national))
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests('26usc469_real_estate_exemption_tests').

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification('26usc469_real_estate_exemption', Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification('26usc469_real_estate_exemption', Type2, context(agent_power(individual_moderate), _, _, _)),
    Type1 \= Type2.

:- end_tests('26usc469_real_estate_exemption_tests').

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added the 'IRS' as the institutional agent. For
 *    a bureaucracy, a clear, if arbitrary, rule is a useful 'Rope' for enforcement,
 *    even if it creates a 'Snare' for a specific class of taxpayer.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Hybrid Worker (Snare): An impossible-to-meet threshold.
 *    - Full-Time Pro (Rope): A clear path to tax benefits.
 *    - IRS (Rope): A clear, enforceable rule.
 * 
 * 3. EXTRACTIVENESS (0.75): High. The rule directly results in higher tax revenue
 *    (extraction) from a specific group (hybrid investors) who cannot meet its terms.
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
    section_469_c7_intent,
    "Is the 750-hour threshold an objective proxy for 'material participation', or was it a deliberately constructed regulatory moat to protect full-time real estate incumbents from competition by high-income professionals?",
    resolution_mechanism("Audit of legislative history and lobbyist influence (e.g., National Association of Realtors) in the 1993 Revenue Reconciliation Act."),
    impact("If proxy: It's a clumsy Rope. If a moat: It's a predatory Snare."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: A Percentage-of-Income Test
 *    Viability: An alternative test could be based on the percentage of income derived
 *    from real estate, rather than hours worked.
 *    Suppression: This was likely rejected as it would allow high-earners to more easily
 *    qualify, which may have been the specific intent to prevent.
 *
 * CONCLUSION:
 * The choice of an 'hours worked' test over other potential metrics is the key
 * mechanism that creates the 'Snare'. It specifically targets and excludes individuals
 * with high-income, time-intensive primary professions.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/'26usc469_real_estate_exemption'].
 * 2. Multi-perspective: ?- multi_index_report('26usc469_real_estate_exemption').
 * 3. Run tests: ?- run_tests('26usc469_real_estate_exemption_tests').
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
