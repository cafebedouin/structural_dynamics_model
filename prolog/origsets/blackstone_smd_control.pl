% ============================================================================
% CONSTRAINT STORY: blackstone_smd_control
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: The Blackstone Group L.P. Form S-1 (March 22, 2007)
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_blackstone_smd_control, []).

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
 * 
 * constraint_id: blackstone_smd_control
 * human_readable: Blackstone Senior Managing Director Voting Control
 * domain: corporate_governance
 * temporal_scope: 2007 (IPO Restructuring) - Present
 * spatial_scope: National (Delaware/US)
 * 
 * SUMMARY:
 * This governance structure allows Senior Managing Directors (SMDs) to retain 
 * absolute control over the firm's general partner through a "multi-class" 
 * unit structure. This prevents public unitholders from electing directors or 
 * influencing management decisions, creating a permanent wall between "founders" 
 * and "public" investors.
 * 
 * KEY AGENTS:
 * - Public Unitholders (Individual Powerless): New investors with almost zero voting power.
 * - Senior Managing Directors (Institutional): The controlling group of existing partners.
 * - Corporate Governance Expert (Analytical): Observes the structural barriers to investor rights.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(blackstone_smd_control, 0, 10).
narrative_ontology:constraint_claim(blackstone_smd_control, snare).

% Base extractiveness: 0.8. 
% Rationale: High power asymmetry; SMDs extract capital and fees while 
% denying public investors any governance rights.
domain_priors:base_extractiveness(blackstone_smd_control, 0.8).

% Suppression: 0.9.
% Rationale: Public unitholders have no physical or legal mechanism to 
% override the general partner; alternatives like proxy contests are barred.
domain_priors:suppression_score(blackstone_smd_control, 0.9).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(blackstone_smd_control, extractiveness, 0.8).
narrative_ontology:constraint_metric(blackstone_smd_control, suppression_requirement, 0.9).

% Enforcement: Requires active enforcement (via corporate charter and Delaware law).
domain_priors:requires_active_enforcement(blackstone_smd_control).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(blackstone_smd_control, senior_managing_directors).
constraint_victim(blackstone_smd_control, public_unitholders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: PUBLIC UNITHOLDER - Snare
   --------------------------------------------------------------------------
   WHO: powerless - No right to elect the board or remove management.
   WHEN: biographical - Stuck with the governance structure for the life of the investment.
   WHERE: trapped - While units can be sold, the governance constraint is inescapable for the asset.
   
   WHY THIS CLASSIFICATION:
   For the public investor, this is a 'Snare'. They are economically bound 
   to the performance of the firm but have zero strategic leverage. Any 
   mismanagement by the SMDs cannot be corrected through voting.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    blackstone_smd_control,
    snare,
    context(
        agent_power(powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: SENIOR MANAGING DIRECTORS (SMDS) - Rope
   --------------------------------------------------------------------------
   WHO: institutional - Retains nearly total control over the entity.
   WHEN: historical - Preserving the "culture" and "long-term focus" of the firm.
   WHERE: arbitrage - Using Delaware partnership law to bypass corporate norms.
   
   WHY THIS CLASSIFICATION:
   For the SMDs, this structure is a 'Rope'. It coordinates the transition 
   to a public company without subjecting the partnership to the "short-term 
   pressures" or "hostile takeovers" associated with typical public markets.
   It protects their long-term vision.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    blackstone_smd_control,
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
   hands of the SMDs.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    blackstone_smd_control,
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

:- begin_tests(blackstone_control_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(blackstone_smd_control, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(blackstone_smd_control, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(blackstone_smd_control, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(blackstone_control_tests).

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
 * 1. MANDATROPHY STATUS: Justified by the high extractiveness (0.8) and suppression (0.9).
 *    The `institutional` perspective of the SMDs sees a 'Rope' (control), which is in
 *    direct opposition to the 'Snare' experienced by the `powerless` public
 *    investors, effectively resolving the mandatrophy.
 * 
 * 2. CLASSIFICATION RATIONALE:
 *    - Public Unitholder (Snare): Economically bound with no influence.
 *    - SMDs (Rope): A tool for control and long-term vision.
 *    - Governance Expert (Mountain): An immutable legal/structural barrier.
 * 
 * 3. CORE INSIGHT: This structure represents a deliberate design choice to subvert
 *    traditional corporate governance, creating a permanent asymmetry of power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the long-term legal viability of such a stark power imbalance.
 */

omega_variable(
    gp_fiduciary_duty_erosion,
    "Will Delaware courts uphold the partnership's right to contractually waive fiduciary duties owed to public unitholders, or will a legal 'Mountain' emerge to protect minority investors?",
    resolution_mechanism("Monitor subsequent litigation in the Delaware Chancery Court (e.g., Dieckman v. Regency) regarding limited partnership agreements."),
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
 *    Suppression: Actively rejected by Blackstone's founders to prevent external
 *    interference with SMD decision-making and preserve "long-term alignment."
 *
 * CONCLUSION:
 * The deliberate rejection of standard voting rights in favor of the multi-class
 * structure confirms this as an asymmetric 'Snare' for the public unitholder,
 * designed to concentrate power.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/blackstone_smd_control].
 * 2. Multi-perspective: ?- multi_index_report(blackstone_smd_control).
 * 3. Run tests: ?- run_tests(blackstone_control_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */