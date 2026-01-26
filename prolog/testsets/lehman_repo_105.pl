% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: lehman_repo_105
% ============================================================================
% Revised: 2026-01-20 (v3.1 Hardened Standard)
% Source: Valukas Report / Lehman Brothers Examiner's Report
% ============================================================================

:- module(lehman_repo_105, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/5,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: lehman_repo_105
 * human_readable: Lehman Brothers Repo 105
 * domain: economic/legal
 * temporal_scope: 2007-2008 CE
 * spatial_scope: Global Financial Markets / US-UK
 * * SUMMARY:
 * Repo 105 was an accounting maneuver used to temporarily move billions in 
 * liabilities off-balance sheet to hide leverage. 
 * While legally classified as a "sale" (Mountain), it functioned as a 
 * predatory mandate to deceive investors (Snare).
 * * KEY AGENTS:
 * - Lehman_Executives: Institutional; managed optics using "Ropes".
 * - Accounting_Standards: Analytical; GAAP rules providing the "Mountain".
 * - Market_Investors: Powerless; exit options hidden by the "Snare".
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor for System Extraction
narrative_ontology:interval(lehman_repo_105, 2007, 2008).
narrative_ontology:constraint_claim(lehman_repo_105, snare).

% Base extractiveness: 0.9. 
% Rationale: High strategic extraction of investor trust via hidden leverage.
domain_priors:base_extractiveness(lehman_repo_105, 0.9).
domain_priors:suppression_score(lehman_repo_105, 0.85).
domain_priors:requires_active_enforcement(lehman_repo_105).

% Beneficiaries & Victims
narrative_ontology:constraint_beneficiary(lehman_repo_105, lehman_senior_management).
narrative_ontology:constraint_victim(lehman_repo_105, counterparty_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: Accounting Standards (GAAP) - MOUNTAIN
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    lehman_repo_105, 
    mountain, 
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(trapped),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: Lehman Executive - ROPE
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    lehman_repo_105, 
    rope, 
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: Uninformed Investor - NOOSE
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    lehman_repo_105, 
    snare, 
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(lehman_repo_105_tests).

/**
 * TEST 1: Multi-perspective variance
 * Demonstrates that the same accounting maneuver yields different classifications across perspectives.
 */
test(multi_perspective_variance) :-
    % Perspective 1 (Accounting Standards)
    constraint_indexing:constraint_classification(
        lehman_repo_105,
        Type1,
        context(agent_power(analytical), time_horizon(historical), exit_options(trapped), spatial_scope(global))
    ),
    % Perspective 2 (Lehman Executive)
    constraint_indexing:constraint_classification(
        lehman_repo_105,
        Type2,
        context(agent_power(institutional), time_horizon(biographical), exit_options(arbitrage), spatial_scope(national))
    ),
    % Perspective 3 (Uninformed Investor)
    constraint_indexing:constraint_classification(
        lehman_repo_105,
        Type3,
        context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))
    ),
    % Verify they differ
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3. % Ensure all three are distinct

/**
 * TEST 2: Power-based extractiveness scaling
 * Demonstrates that the investor (powerless) experiences higher extraction than the executive (powerful).
 */
test(power_extractiveness_scaling) :-
    ContextPowerless = context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local)),
    ContextPowerful = context(agent_power(institutional), time_horizon(biographical), exit_options(arbitrage), spatial_scope(national)),
    constraint_indexing:extractiveness_for_agent(lehman_repo_105, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(lehman_repo_105, ContextPowerful, Score2),
    Score1 > Score2.  % Powerless experience more extraction

/**
 * TEST 3: Time-horizon immutability
 * Demonstrates how accounting "facts" appear immutable over short horizons, but can be challenged historically.
 */
test(time_immutability) :-
    % Short horizon + trapped = mountain (legal "sale" at the time)
    constraint_indexing:effective_immutability(time_horizon(immediate), exit_options(trapped), mountain),
    % Long horizon = rope (historical analysis can reveal manipulation)
    constraint_indexing:effective_immutability(time_horizon(historical), exit_options(analytical), rope).

/**
 * TEST 4: Domain-specific insight - Deception vs. Legal Loopholes
 * Demonstrates the conflict between the perceived legality and actual deceptive intent of Repo 105.
 */
test(deception_vs_legal_loopholes) :-
    constraint_indexing:constraint_classification(lehman_repo_105, ClassificationInvestor, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(lehman_repo_105, ClassificationAccountant, context(agent_power(analytical), _, _, _)),
    ClassificationInvestor = snare,
    ClassificationAccountant = mountain.

:- end_tests(lehman_repo_105_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini 2.0 Flash
 * Date: 2026-01-23
 * 
 * KEY DECISIONS MADE BY MODEL:
 * 
 * 1. BASE EXTRACTIVENESS (0.9):
 *    Reasoning: The Valukas Report explicitly identified Repo 105 as a method to manipulate financial statements and deceive investors. This represents a severe form of extraction of trust and accurate information.
 *    Evidence: "Lehman used Repo 105 transactions not to obtain financing... but to temporarily reduce its balance sheet." (Valukas Report)
 *    Uncertainty: The precise legal culpability of individuals at the time of execution.
 *    Beneficiaries: Lehman Senior Management, to meet financial targets and maintain stock price.
 * 
 * 2. PERSPECTIVE SELECTION:
 *    Selected Analytical (GAAP), Institutional (Lehman Executive), and Powerless (Uninformed Investor) to highlight the contrasting views of the accounting maneuver – from legal interpretation to predatory practice.
 * 
 * 3. CLASSIFICATION RATIONALE:
 *    Analytical (GAAP) as Mountain: From a strict legal accounting perspective, Repo 105 was structured to meet the criteria of a "sale," appearing as an immutable accounting fact at the time.
 *    Institutional (Lehman Executive) as Rope: For Lehman's management, Repo 105 was a tool, a coordinating mechanism to manage the perception of the firm's leverage, effectively "tying" their financial reporting to a desired outcome.
 *    Powerless (Uninformed Investor) as Snare: Investors were unknowingly subjected to a financial maneuver designed to conceal risk, trapping them in a misleading financial reality with no transparency or recourse.
 * 
 * 4. AMBIGUITIES IN SOURCE MATERIAL:
 *    - The degree to which all parties involved (e.g., auditors, counterparties) were aware of the deceptive intent behind Repo 105.
 * 
 * 5. OMEGAS 
 *    Define uncertainty so your analysis is cleaner
 *    omega_variable(
 *        colorable_claim_judgment,
 *        "Will a trier of fact find the senior officers' conduct grossly negligent?",
 *        resolution_mechanism("Litigation outcome of breach of fiduciary duty claims regarding Repo 105 implementation and oversight."),
 *        impact("If Yes: The Snare is legally confirmed, establishing individual culpability. If No: It remains a 'Business Judgment' Mountain for executives, implying systemic rather than individual failure."),
 *        confidence_without_resolution(medium)
 *    ).
 * 
 * 6. CONFIDENCE ASSESSMENT:
 *    High confidence: Classification of Investor (Snare) and Executive (Rope), as their roles and motivations are well-documented.
 *    Medium confidence: Precise classification of GAAP as Mountain, as its "immutability" is challenged by its manipulation.
 *    Low confidence: N/A
 *    Would benefit from: More explicit commentary from auditors or regulators at the time, indicating their internal interpretations.
 * 
 * 7. EDGE CASES:
 *    - The line between aggressive but legal accounting and fraudulent misrepresentation, which Repo 105 straddled.
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS (If Applicable)
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: Standard Repo Accounting
 *    Viability: This is the default, transparent method of reporting repurchase agreements. It is always viable from an accounting standpoint.
 *    Suppression: Actively suppressed by Lehman management's desire to hit leverage targets without selling assets at a loss, thus avoiding negative market perception or credit rating downgrades. The alternative would have revealed the true financial state.
 *    Evidence: The entire purpose of Repo 105 was to avoid the implications of standard accounting.
 * 
 * CONCLUSION:
 * The active suppression of transparent accounting alternatives solidifies Repo 105's classification as a "Snare" for investors, intentionally used by management (Rope) to bypass the "Mountain" of standard financial reporting.
 */

% If alternatives exist (affects signature detection):
intent_viable_alternative(lehman_repo_105, standard_repo, 'Transparent leverage reporting following standard accounting practices.').
intent_alternative_rejected(lehman_repo_105, standard_repo, 'Fear of credit rating downgrade and investor reaction to true leverage levels.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * 
 * 1. Load into main system:
 *    ?- [constraints/lehman_repo_105].
 * 
 * 2. Run multi-perspective analysis:
 *    ?- constraint_indexing:multi_index_report(lehman_repo_105).
 * 
 * 3. Run tests:
 *    ?- run_tests(lehman_repo_105_tests).
 * 
 * 4. Generate pedagogical report:
 *    ?- pedagogical_report(lehman_repo_105).
 * 
 * 5. Compare with other constraints:
 *    ?- compare_constraints(lehman_repo_105, [other_id]).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
