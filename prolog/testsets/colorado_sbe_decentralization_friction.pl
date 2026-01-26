% ============================================================================
% CONSTRAINT STORY: colorado_sbe_decentralization_friction
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Synthesis of US state-level education policy and user-provided context
% ============================================================================

:- module(constraint_colorado_sbe_decentralization_friction, []).

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
 * constraint_id: colorado_sbe_decentralization_friction
 * human_readable: Colorado SBE Institutional Preservation (Educational Decentralization Friction)
 * domain: political/regulatory/educational
 * temporal_scope: 2024-2030
 * spatial_scope: Colorado, USA
 * 
 * SUMMARY:
 * The Colorado State Board of Education (SBE) acts as an institutional 
 * gatekeeper for educational legitimacy. Its regulatory structures around 
 * per-pupil funding, standardized testing, and teacher licensure create 
 * "Mountain" constraints that inhibit the growth of microschooling and 
 * the unbundling of education, despite increasing demand for decentralized learning.
 * 
 * KEY AGENTS:
 * - Taxpayers/Voters (Individual Powerless): Fund public education but lack direct control over SBE policies.
 * - Microschool Founders (Individual Moderate): Agents of decentralization, seeking innovative educational models.
 * - The Board (Institutional): Enforces the "factory model" of education through regulatory structures.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(colorado_sbe_decentralization_friction, 0, 10).
narrative_ontology:constraint_claim(colorado_sbe_decentralization_friction, mountain).

% Base extractiveness: 0.7.
% The state extracts data and compliance labor from small units 
% in exchange for a "legitimacy" stamp that is legally required for funding.
domain_priors:base_extractiveness(colorado_sbe_decentralization_friction, 0.7).

% Suppression score: 0.6.
% Compliance requirements unintentionally favor large incumbents 
% and can hide the viability of alternative learning units, suppressing innovation.
domain_priors:suppression_score(colorado_sbe_decentralization_friction, 0.6).

% Enforcement: Requires active enforcement: The SBE must actively audit, license, 
% and mandate specific testing protocols.
domain_priors:requires_active_enforcement(colorado_sbe_decentralization_friction).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(colorado_sbe_decentralization_friction, legacy_school_districts).
constraint_victim(colorado_sbe_decentralization_friction, microschool_innovators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: TAXPAYERS / VOTERS - Tangled Rope
   --------------------------------------------------------------------------
   WHO: individual_powerless (Fund public education but lack direct control)
   WHEN: biographical (Long-term impact on educational quality and innovation)
   WHERE: constrained (Bound by existing political structures and SBE policies)
   
   WHY THIS CLASSIFICATION:
   For taxpayers and voters, the SBE's actions represent a 'Tangled Rope'.
   It's a 'Rope' because it aims to ensure accountability and quality in education.
   It's 'Tangled' because these regulations can stifle innovation, prevent more
   efficient localized solutions, and create friction with their desire for
   educational freedom.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    colorado_sbe_decentralization_friction,
    tangled_rope,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: MICROSCHOOL FOUNDERS - Snare
   --------------------------------------------------------------------------
   WHO: individual_moderate (Has pedagogical tools but lacks legal parity)
   WHEN: immediate (Focused on meeting this year's compliance)
   WHERE: constrained (Exit is possible but requires losing state funding)
   
   WHY THIS CLASSIFICATION:
   For the microschool founder, the "factory model" regulations are a 'Snare'. 
   The reporting requirements are extractive of time and funds, and 
   the refusal to recognize unbundled credentials creates an artificial 
   barrier to entry, stifling innovation and growth.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    colorado_sbe_decentralization_friction,
    snare,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE BOARD (SBE) - Mountain
   --------------------------------------------------------------------------
   WHO: institutional (Enforces the "factory model" through regulations)
   WHEN: historical (Rooted in 20th-century educational paradigms)
   WHERE: arbitrage (Balances state law against local district autonomy)
   
   WHY THIS CLASSIFICATION:
   For the SBE, the constitutional and geographical reality of local control 
   (178 independent districts) is a 'Mountain'. This administrative friction
   is an immutable structural fact rather than a "policy choice," creating
   an unyielding terrain for all state mandates.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    colorado_sbe_decentralization_friction,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
).

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(colorado_sbe_friction_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, Type2, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, Type3, context(agent_power(institutional), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(colorado_sbe_friction_tests).

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
 * 1. INDIVIDUAL POWERLESS PERSPECTIVE: Added 'Taxpayers / Voters' as the
 *    individual powerless agent, who experience the SBE's actions as a
 *    'Tangled Rope' due to conflicting desires for accountability and innovation.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Taxpayers (Tangled Rope): Seek accountability but face innovation friction.
 *    - Microschool Founders (Snare): Regulatory burden stifles growth.
 *    - SBE (Mountain): Fixed structural realities and a mandate for control.
 * 
 * 3. CORE INSIGHT: The friction in educational decentralization arises from
 *    the SBE's attempts to preserve a 'Mountain' of centralized control,
 *    which creates a 'Snare' for innovators and a 'Tangled Rope' for the public.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the SBE's willingness to adapt to educational innovation.
 */

omega_variable(
    political_capture_omega,
    "Will the SBE interpret 'Quality' through the lens of process compliance (legacy model) or outcome mastery (innovative models)?",
    resolution_mechanism("Monitor if Colorado SBE adopts mastery-based microcredentials as valid for state diplomas and funding. Analyze shifts in SBE composition and policy statements."),
    impact("If Process: Leads to Re-centralization ('Snare'). If Mastery: Enables True Decentralization ('Rope')."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Innovation Zones
 *    Viability: Colorado has existing "Innovation Schools" laws that could be extended to microschool pods, providing regulatory flexibility.
 *    Suppression: Often limited by district-level veto power, maintaining the 'Mountain' of centralized control and preventing widespread adoption.
 *
 * CONCLUSION:
 * The SBE's entrenched regulatory frameworks create a 'Mountain' of resistance
 * to educational innovation. While 'Innovation Zones' exist as a 'Rope', their
 * limited scope suggests a strong suppression of true decentralization.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/colorado_sbe_decentralization_friction].
 * 2. Multi-perspective: ?- multi_index_report(colorado_sbe_decentralization_friction).
 * 3. Run tests: ?- run_tests(colorado_sbe_friction_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */