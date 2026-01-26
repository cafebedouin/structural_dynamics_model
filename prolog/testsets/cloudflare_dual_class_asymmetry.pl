% ============================================================================
% CONSTRAINT STORY: cloudflare_dual_class_asymmetry
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Cloudflare, Inc. S-1 Registration Statement (Amendment No. 2)
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_cloudflare_dual_class_asymmetry, []).

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
 * constraint_id: cloudflare_dual_class_asymmetry
 * human_readable: Cloudflare Dual-Class Voting Control
 * domain: economic/corporate_governance
 * temporal_scope: Post-IPO Governance
 * spatial_scope: Global (Corporate Governance)
 * 
 * SUMMARY:
 * Cloudflare's S-1 details a dual-class stock structure where Class B shares 
 * carry 10 votes per share, while the offered Class A shares carry only 
 * one vote. This mechanism ensures that the founders and pre-IPO 
 * insiders retain approximately 96.3% of the total voting power, effectively 
 * insulating management from public shareholder influence.
 * 
 * KEY AGENTS:
 * - Public Shareholders (Individual Powerless): New investors with almost zero voting power.
 * - Founders (Institutional): The controlling group of existing partners.
 * - Corporate Governance Expert (Analytical): Observes the structural barriers to investor rights.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Context-Independent)
   ========================================================================== */

narrative_ontology:interval(cloudflare_dual_class_asymmetry, 0, 10).
narrative_ontology:constraint_claim(cloudflare_dual_class_asymmetry, snare).

% Base extractiveness: 0.8.
% High power asymmetry; founders extract capital and fees while 
% denying public investors any governance rights.
domain_priors:base_extractiveness(cloudflare_dual_class_asymmetry, 0.8).

% Suppression score: 0.9.
% Public unitholders have no physical or legal mechanism to 
% override the general partner; alternatives like proxy contests are barred.
domain_priors:suppression_score(cloudflare_dual_class_asymmetry, 0.9).

% Enforcement: Requires active enforcement (via corporate charter and Delaware law).
domain_priors:requires_active_enforcement(cloudflare_dual_class_asymmetry).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(cloudflare_dual_class_asymmetry, founders).
constraint_victim(cloudflare_dual_class_asymmetry, public_shareholders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Rules)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: PUBLIC SHAREHOLDER - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless - No right to elect the board or remove management.
   WHEN: biographical - Stuck with the governance structure for the life of the investment.
   WHERE: trapped - While units can be sold, the governance constraint is inescapable for the asset.
   
   WHY THIS CLASSIFICATION:
   For the public investor, this is a 'Snare.' They are economically bound 
   to the performance of the firm but have zero strategic leverage. Any 
   mismanagement by the founders cannot be corrected through voting.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cloudflare_dual_class_asymmetry,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: FOUNDERS - Rope
   --------------------------------------------------------------------------
   WHO: institutional - Retains nearly total control over the entity.
   WHEN: historical - Preserving the "culture" and "long-term focus" of the firm.
   WHERE: arbitrage - Using Delaware partnership law to bypass corporate norms.
   
   WHY THIS CLASSIFICATION:
   For the founders, this structure is a 'Rope.' It coordinates the transition 
   to a public company without subjecting the partnership to the "short-term 
   pressures" or "hostile takeovers" associated with typical public markets.
   It protects their long-term vision.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cloudflare_dual_class_asymmetry,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: CORPORATE GOVERNANCE EXPERT - Mountain
   --------------------------------------------------------------------------
   WHO: analytical - Observes the unyielding structural barriers.
   WHEN: generational - The multi-class structure is a permanent feature of the IPO.
   WHERE: analytical - Sees the legal and financial architecture as fixed.
   
   WHY THIS CLASSIFICATION:
   The "Mountain" is the multi-class unit architecture. Under the partnership 
   agreement and Delaware law, common unitholders have "no right to elect 
   the general partner or its directors." This creates a permanent, 
   non-negotiable structural barrier that locks the firm’s control in the 
   hands of the founders.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cloudflare_dual_class_asymmetry,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(generational),
        exit_options(analytical),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(cloudflare_dual_class_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(cloudflare_dual_class_tests).

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
 * 1. MANDATROPHY STATUS: High extractiveness (0.8) and suppression (0.9) trigger
 *    the mandatrophy protocol. This is 'RESOLVED' by showing the stark asymmetry:
 *    what is a 'Rope' for the founders (control) is a 'Snare' for public investors.
 * 
 * 2. CLASSIFICATION RATIONALE:
 *    - Public Shareholder (Snare): Economically bound with no governance influence.
 *    - Founders (Rope): A powerful tool for control and long-term vision.
 *    - Governance Expert (Mountain): An immutable legal/structural barrier.
 * 
 * 3. CORE INSIGHT: The dual-class structure is a deliberate design choice that
 *    creates a permanent asymmetry of power, prioritizing founder control over
 *    public shareholder democracy.
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
    gp_fiduciary_duty_erosion,
    "Will Delaware courts uphold the partnership's right to contractually waive fiduciary duties owed to public unitholders, or will a legal 'Mountain' emerge to protect minority investors?",
    resolution_mechanism("Monitoring subsequent litigation in the Delaware Chancery Court (e.g., Dieckman v. Regency) regarding limited partnership agreements."),
    impact("If duties can be waived: The system is a total 'Snare' for public investors. If not: A slight 'Rope' exists for investors."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: "One Share, One Vote" Public Corporation
 *    Viability: This is the standard for most NYSE listings, offering proportional
 *    voting rights to all shareholders.
 *    Suppression: Actively rejected by Cloudflare's founders to prevent external
 *    interference with their long-term vision and preserve control.
 *
 * CONCLUSION:
 * The deliberate rejection of standard voting rights confirms this as an
 * asymmetric 'Snare' for the public unitholder, designed to concentrate power
 * in the hands of the founders.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/cloudflare_dual_class_asymmetry].
 * 2. Multi-perspective: ?- multi_index_report(cloudflare_dual_class_asymmetry).
 * 3. Run tests: ?- run_tests(cloudflare_dual_class_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */