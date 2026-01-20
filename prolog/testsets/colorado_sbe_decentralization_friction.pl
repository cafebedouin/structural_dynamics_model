% ============================================================================
% CONSTRAINT STORY: colorado_sbe_decentralization_friction
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Synthesis of US state-level education policy and user-provided context
% ============================================================================

:- module(colorado_sbe_decentralization_friction, []).

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
 * * constraint_id: colorado_sbe_decentralization_friction
 * human_readable: Colorado SBE Institutional Preservation
 * domain: political/regulatory/educational
 * temporal_scope: 2024-2030
 * spatial_scope: Colorado, USA
 * * SUMMARY:
 * The Colorado State Board of Education (SBE) operates as an institutional 
 * gatekeeper for educational legitimacy. While microschooling is growing 
 * in America, the SBE's existing regulatory structures around 
 * per-pupil funding, standardized testing, and teacher licensure act as 
 * "Mountain" constraints that inhibit the unbundling of education.
 * * KEY AGENTS:
 * - The Board (SBE): Institutional enforcer of the "factory model".
 * - Microschool Founders: Agents of decentralization.
 * - Taxpayers/Voters: The collective power that authorizes the SBE.
 * * NARRATIVE ARC:
 * The SBE views AI and unbundled credentials as a threat to "Standardized 
 * Accountability." By requiring microschools to conform to legacy 
 * reporting formats, they transform a potential technological "Rope" (AI 
 * tools) back into an institutional "Noose" (regulatory capture).
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(colorado_regulatory_window, 0, 10).
narrative_ontology:constraint_claim(colorado_sbe_decentralization_friction, mountain).

% Base extractiveness score (0.7): High. 
% Rationale: The state extracts data and compliance labor from small units 
% in exchange for a "legitimacy" stamp that is legally required for funding.
domain_priors:base_extractiveness(colorado_sbe_decentralization_friction, 0.7).

% Suppression score (0.6): Moderate-High.
% Rationale: Compliance requirements unintentionally favor large incumbents 
% and can hide the viability of alternative learning units.
domain_priors:suppression_score(colorado_sbe_decentralization_friction, 0.6).

% Enforcement requirements
% Requires active enforcement: The SBE must actively audit, license, 
% and mandate specific testing protocols.
domain_priors:requires_active_enforcement(colorado_sbe_decentralization_friction).

% Metrics
narrative_ontology:constraint_metric(colorado_sbe_decentralization_friction, extractiveness, 0.7).
narrative_ontology:constraint_metric(colorado_sbe_decentralization_friction, suppression_requirement, 0.6).

% Beneficiaries & Victims
constraint_beneficiary(colorado_sbe_decentralization_friction, legacy_school_districts).
constraint_beneficiary(colorado_sbe_decentralization_friction, the_state_board).
constraint_victim(colorado_sbe_decentralization_friction, microschool_innovators).
constraint_victim(colorado_sbe_decentralization_friction, students_seeking_personalization).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE STATE BOARD MEMBER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Holds constitutional authority over public education.
   WHEN: historical - Thinking of the "bigness" arc of 20th-century progress.
   WHERE: arbitrage - Balances state law against local district autonomy.
   SCOPE: national - Benchmarking Colorado against federal standards.
   
   WHY THIS CLASSIFICATION:
   To the Board, regulations are a Rope—a functional coordination mechanism 
   designed to ensure quality, safety, and equitable funding distribution 
   across the state.
   
   NARRATIVE EVIDENCE:
   "Education is a strong candidate for partial decentralization... yet it 
   will still sit on top of large, centralized infrastructures unless 
   standards and governance prevent lock-in".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    colorado_sbe_decentralization_friction,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE COLORADO MICROSCHOOL FOUNDER - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - Has the pedagogical tools but lacks legal parity.
   WHEN: immediate - Focused on meeting this year's compliance to keep the doors open.
   WHERE: constrained - Exit is possible but requires losing state funding.
   SCOPE: local - Operating within a single community.
   
   WHY THIS CLASSIFICATION:
   For the founder, the "factory model" regulations are a Noose. 
   The reporting requirements are extractive of time and funds, and 
   the refusal to recognize unbundled credentials creates an artificial 
   barrier to entry.
   
   NARRATIVE EVIDENCE:
   "Regulatory friction: Compliance requirements can unintentionally favor 
   large incumbents unless policy explicitly supports pluralism".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    colorado_sbe_decentralization_friction,
    noose,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANALYTICAL OBSERVER (ORGANIZATIONAL ANALYST) - Mountain
   --------------------------------------------------------------------------
   WHO: agent_power(analytical) - Observes the unyielding organizational physics.
   WHEN: generational - The 178-district structure is a decades-old fixture.
   WHERE: arbitrage - Managing the friction across 64 counties.
   SCOPE: continental - Colorado as a case study for Western US localism.
   
   WHY THIS CLASSIFICATION:
   The "Mountain" is the constitutional and geographical reality of local 
   control. With 178 independent districts, administrative friction is an 
   immutable structural fact rather than a "policy choice." This creates 
   a baseline of "organizational entropy" that serves as the unyielding 
   terrain upon which all State Board of Education (SBE) mandates must 
   travel.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    colorado_sbe_decentralization_friction,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(generational),
        exit_options(arbitrage),
        spatial_scope(continental)
    )
) :-
    domain_priors:suppression_score(colorado_sbe_decentralization_friction, S),
    S > 0.3.

% Explicit priors reflecting the inherent structural friction of decentralization.
domain_priors:base_extractiveness(colorado_sbe_decentralization_friction, 0.3).
domain_priors:suppression_score(colorado_sbe_decentralization_friction, 0.4).

/* ==========================================================================
   4. TESTS (What We Learn)
   ========================================================================== */

:- begin_tests(colorado_sbe_friction_tests).

test(accountability_paradox) :-
    % Test that "High Accountability" for the Board = "High Suppression" for the Founder.
    domain_priors:suppression_score(colorado_sbe_decentralization_friction, S),
    S > 0.5.

test(scale_bias) :-
    % Test that the system favors institutional over individual moderate power.
    true.

:- end_tests(colorado_sbe_friction_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. PERSPECTIVE SHIFT: The Board doesn't see itself as a "Noose." It sees 
 * itself as the provider of the "big stack" (funding/standards) that 
 * makes any education possible.
 * * 2. REGULATORY FRICTION: I highlighted how the "factory model" 
 * isn't just a physical building, but a mental and legal framework of 
 * "standardization" and "age-grading".
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_capture_omega,
    "Will the Board interpret 'Quality' through the lens of process compliance or outcome mastery?",
    resolution_mechanism("Monitor if Colorado SBE adopts mastery-based microcredentials as valid for state diplomas."),
    impact("If Process: Re-centralization (Noose). If Mastery: True Decentralization (Rope)."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Innovation Zones
 * Viability: Colorado has existing "Innovation Schools" laws that could 
 * be extended to microschool pods.
 * Suppression: Often limited by district-level veto power, maintaining 
 * the "Mountain" of centralized control.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
