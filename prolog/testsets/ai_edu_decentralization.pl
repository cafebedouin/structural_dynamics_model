% ============================================================================
% CONSTRAINT STORY: ai_edu_decentralization
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Multi-source synthesis (Nature, MIT, Pew, AEI, etc.)
% ============================================================================

:- module(ai_edu_decentralization, []).

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
 * * constraint_id: ai_edu_decentralization
 * human_readable: The AI-Education Decoupling
 * domain: technological/educational/economic
 * temporal_scope: 2020-2030 (Early AI Deployment Era)
 * spatial_scope: Global (Primary focus on US context)
 * * SUMMARY:
 * AI acts as a general-purpose technology pushing education toward partial 
 * decentralization by lowering coordination costs for small units (microschools) 
 *. However, this "small unit" growth sits atop "big stacks" of 
 * centralized compute and data infrastructure.
 * * KEY AGENTS:
 * - Parent/Student: Seeking personalization outside the "factory model".
 * - Microschool Founder: Utilizing AI to manage small learning pods.
 * - Tech Platform: Providing the centralized infrastructure (cloud/models).
 * * NARRATIVE ARC:
 * The industrial-era "bigness" of mass schooling reaches a plateau. 
 * AI enables an "unbundling" of credentials and instruction, 
 * allowing diverse pathways to emerge from the previous institutional monopoly.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(ai_edu_decentralization, 0, 10).
narrative_ontology:constraint_claim(ai_edu_decentralization, rope).

% Base extractiveness score (0.5): Moderate.
% Rationale: While AI lowers costs, platform capture by infrastructure 
% providers creates new forms of value extraction.
domain_priors:base_extractiveness(ai_edu_decentralization, 0.5).

% Suppression score (0.4): Moderate.
% Rationale: Alternatives (microschools/pods) are increasingly visible, 
% though regulatory friction persists.
domain_priors:suppression_score(ai_edu_decentralization, 0.4).

% Enforcement requirements
% Emerges naturally from technological shifts but requires active 
% enforcement of interoperability standards to prevent lock-in.
domain_priors:requires_active_enforcement(ai_edu_decentralization).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(ai_edu_decentralization, extractiveness, 0.5).
narrative_ontology:constraint_metric(ai_edu_decentralization, suppression_requirement, 0.4).

% Beneficiaries & Victims
constraint_beneficiary(ai_edu_decentralization, tech_platforms). %
constraint_beneficiary(ai_edu_decentralization, microschool_families). %
constraint_victim(ai_edu_decentralization, legacy_mass_institutions). %

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE FACTORY-MODEL STUDENT - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to standardized age-grading and efficiency.
   WHEN: biographical - The K-12 schooling cycle defines their early life.
   WHERE: trapped - Attendance is mandated by law/social norm; exit is costly.
   SCOPE: local - Their immediate environment is a single school building.
   
   WHY THIS CLASSIFICATION:
   For the student in a legacy system, the "bigness" of the institution 
   appears as an unchangeable law of nature (standardization, bells, fixed 
   curricula).
   
   NARRATIVE EVIDENCE:
   "The 'factory model school' concept captures how industrial-era schooling 
   emphasized standardization... mirroring the logic of mass production".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ai_edu_decentralization,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    constraint_indexing:effective_immutability_for_context(
        context(individual_powerless, biographical, trapped, local),
        mountain
    ),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE MICROSCHOOL FOUNDER - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - Has agency to launch small units.
   WHEN: immediate - Focused on current learning outcomes and engagement.
   WHERE: mobile - Can assemble "stacks" of curriculum and AI tools.
   SCOPE: local - Operating a pod or community academy.
   
   WHY THIS CLASSIFICATION:
   For the founder, AI is a tool for functional coordination. It lowers the 
   minimum viable size for high-quality instruction, making decentralized 
   education a viable and beneficial choice.
   
   NARRATIVE EVIDENCE:
   "AI decentralizes capabilities by lowering the expertise and coordination 
   thresholds required for small organizations... to perform tasks".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ai_edu_decentralization,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(mobile),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(ai_edu_decentralization, E),
    E =< 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE INFRASTRUCTURE PROVIDER - Noose
   --------------------------------------------------------------------------
   
   WHO: institutional - Controls the data, compute, and platform standards.
   WHEN: generational - Focused on long-term market dominance and ecosystem lock-in.
   WHERE: arbitrage - Can play different educational jurisdictions against each other.
   SCOPE: global - Models and platforms operate worldwide.
   
   WHY THIS CLASSIFICATION:
   While the "delivery unit" becomes small, the dependency on the platform 
   becomes a coercive mechanism. Lock-in through data access and pricing 
   concentrates value at the center.
   
   NARRATIVE EVIDENCE:
   "Dependence on a handful of large platforms can re-centralize control through 
   pricing, data access, and ecosystem lock-in".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ai_edu_decentralization,
    noose,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(ai_edu_decentralization, E),
    E >= 0.5,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(ai_edu_decentralization_tests).

test(multi_perspective_variance) :-
    % Perspective 1: Student
    constraint_indexing:constraint_classification(ai_edu_decentralization, Type1, 
        context(agent_power(individual_powerless), time_horizon(biographical), exit_options(trapped), spatial_scope(local))),
    % Perspective 2: Founder
    constraint_indexing:constraint_classification(ai_edu_decentralization, Type2, 
        context(agent_power(individual_moderate), time_horizon(immediate), exit_options(mobile), spatial_scope(local))),
    % Perspective 3: Platform
    constraint_indexing:constraint_classification(ai_edu_decentralization, Type3, 
        context(agent_power(institutional), time_horizon(generational), exit_options(arbitrage), spatial_scope(global))),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 == mountain,
    Type2 == rope,
    Type3 == noose.

test(power_extractiveness_imbalance) :-
    % Powerful institutions extract more value than individuals in unbundled systems
    domain_priors:base_extractiveness(ai_edu_decentralization, E),
    E >= 0.5.

test(exit_option_shift) :-
    % Exit options (mobile) turn Mountain into Rope
    constraint_indexing:effective_immutability(immediate, mobile, rope).

:- end_tests(ai_edu_decentralization_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. BASE EXTRACTIVENESS (0.5): Chose a balanced score because AI both 
 * empowers small units and risks platform capture.
 * * 2. PERSPECTIVE SELECTION: Analyzed from Student (Powerless), Founder 
 * (Moderate), and Platform (Institutional) to show the full oscillation 
 * of the constraint.
 * * 3. CLASSIFICATION RATIONALE:
 * - Student -> Mountain: The "factory model" is fixed until exit.
 * - Founder -> Rope: AI is a coordination tool they can manipulate.
 * - Platform -> Noose: They create the "big stacks" that others depend on.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_legitimacy_omega,
    "Will employers and colleges treat alternative credentials as meaningful signals?",
    resolution_mechanism("Track conversion rates and salary outcomes for microcredential vs. degree holders."),
    impact("If Yes: Permanent Decentralization. If No: Re-centralization around legacy institutions."),
    confidence_without_resolution(medium)
).

omega_variable(
    platform_lock_in_severity,
    "Can open-source models and interoperable standards prevent total platform capture?",
    resolution_mechanism("Measure adoption rates of verifiable digital credentials vs. proprietary ecosystems."),
    impact("If Yes: Sustainable Rope. If No: Universal Noose."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Open Learning Networks
 * Viability: Standards like verifiable digital credentials allow portability.
 * Suppression: Regulatory friction often favors large incumbents.
 * * ALTERNATIVE 2: State-Led Personalized Systems
 * Viability: Large bureaucracies adopting AI tutors.
 * Suppression: High inertia in "factory model" structures.
 * * CONCLUSION:
 * The absence of strong portable standards shifts education toward the 
 * "Noose" of platform capture even as it decentralizes in name.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% To run: ?- run_tests(ai_edu_decentralization_tests).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
