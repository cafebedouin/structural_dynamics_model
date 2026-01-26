% ============================================================================
% CONSTRAINT STORY: smartphone_ubiquity
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Chris Stokel-Walker, "Smartphones (yes, really): Best ideas of the century"
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_smartphone_ubiquity, []).

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
 * constraint_id: smartphone_ubiquity
 * human_readable: The Smartphone Ubiquity Constraint
 * domain: technological/social/economic
 * temporal_scope: 2007 (iPhone release) to 2026+
 * spatial_scope: Global (7 in 10 people worldwide)
 * 
 * SUMMARY:
 * The smartphone has transitioned from a communication device to a "place within 
 * which we live," functioning as a portable digital home that bridges physical 
 * and digital realities. While it allows lower-income 
 * nations to bypass legacy infrastructure (banks, desktops), it simultaneously 
 * extracts privacy and creates a "shell" that can lead to social disconnect.
 * 
 * KEY AGENTS:
 * - The Global User (Individual Moderate): Uses the device for payments, crops, or social connection.
 * - The Regulator (Institutional): Acts to mitigate social harm and privacy breaches.
 * - The Australian Under-16 (Individual Powerless): Faces legal bans and social media exclusion.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(smartphone_ubiquity, 0, 10).
narrative_ontology:constraint_claim(smartphone_ubiquity, tangled_rope).

% Base extractiveness: 0.75 (High)
% Smartphones have accustomed users to "far less privacy," extracting 
% data even within our own homes.
domain_priors:base_extractiveness(smartphone_ubiquity, 0.75).

% Suppression score: 0.50 (Moderate)
% Ubiquity has led users to "bypass the desktop computer altogether," 
% effectively suppressing older infrastructure through sheer utility and adoption.
domain_priors:suppression_score(smartphone_ubiquity, 0.50).

% Enforcement: Emerges naturally through ubiquity and adoption, 
% but requires active enforcement for bans (schools, social media).
domain_priors:requires_active_enforcement(smartphone_ubiquity).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(smartphone_ubiquity, fintech_platforms).
constraint_victim(smartphone_ubiquity, privacy_conscious_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE AUSTRALIAN UNDER-16 - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (Subject to blanket legal bans)
   WHEN: immediate (Facing a sudden legal cutoff)
   WHERE: trapped (Cannot legally access the "digital house" of friends)
   
   WHY THIS CLASSIFICATION:
   For youth subject to the ban, the smartphone/social media constraint is a 
   'Snare'—a coercive exclusion from the "additional place within which we live" 
   that their peers in other countries enjoy. It strangles their social
   connection and access to digital life.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    smartphone_ubiquity,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE REGULATOR - Tangled Rope
   --------------------------------------------------------------------------
   WHO: institutional (Acts to mitigate social harm and privacy breaches)
   WHEN: historical (Responding to evolving technological impacts)
   WHERE: arbitrage (Balances economic benefits with social costs)
   
   WHY THIS CLASSIFICATION:
   For the Regulator, smartphone ubiquity is a 'Tangled Rope'. It's a 'Rope'
   because it's a powerful tool for economic development, communication, and
   access to services. It's 'Tangled' because they must balance these benefits
   with the need to mitigate social harms like addiction, privacy breaches,
   and the digital divide.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    smartphone_ubiquity,
    tangled_rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE GLOBAL USER - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (Uses the device for payments, crops, or social connection)
   WHEN: biographical (Integrating technology into daily life)
   WHERE: mobile (Utilizes the device to bypass traditional infrastructure)
   
   WHY THIS CLASSIFICATION:
   For those in lower-income regions, the smartphone is a functional 'Rope'—a 
   coordination tool that provides infrastructure where none existed, allowing 
   them to "bypass" physical constraints and access essential services.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    smartphone_ubiquity,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(smartphone_ubiquity_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(smartphone_ubiquity, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(smartphone_ubiquity, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(smartphone_ubiquity, Type3, context(agent_power(individual_moderate), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(smartphone_ubiquity_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added 'The Regulator' as the institutional
 *    agent. For them, smartphone ubiquity presents a 'Tangled Rope', balancing
 *    benefits with significant social and economic challenges.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Australian Under-16 (Snare): Exclusion and social disconnect.
 *    - Regulator (Tangled Rope): Balancing utility with social harm.
 *    - Global User (Rope): Access to infrastructure and services.
 * 
 * 3. CORE INSIGHT: Smartphone ubiquity is a 'Tangled Rope'. It's a powerful
 *    'Rope' for billions, offering unprecedented access and connectivity. However,
 *    it simultaneously functions as a 'Snare' for those subject to its extractive
 *    properties (privacy loss) or regulatory exclusion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the long-term trade-off between smartphone utility and privacy/well-being.
 */

omega_variable(
    privacy_extraction_intent,
    "Is the extraction of privacy a functional necessity for smartphone utility (Rope) or a predatory choice by providers (Snare) that exploits network effects and user lock-in?",
    resolution_mechanism("Audit of model efficiency with locally-encrypted data vs. centralized cloud-harvested data; regulatory impact assessments of data privacy laws."),
    impact("If necessity: 'Mountain' of technological design. If predatory: 'Snare/Mandatrophy' for users."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Desktop-based / Centralized Infrastructure
 *    Viability: The 20th-century standard for banking and computing, offering more control over data for individual users.
 *    Suppression: Bypassed by smartphone ubiquity in lower-income countries due to cost and accessibility, and suppressed globally by the convenience of mobile.
 *
 * CONCLUSION:
 * The smartphone represents a 'Tangled Rope' where convenience and utility have
 * actively suppressed alternatives that might offer greater privacy or reduce
 * social harms. The rapid adoption created a 'Mountain' of technological reliance,
 * making true 'Exit Options' increasingly difficult.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/smartphone_ubiquity].
 * 2. Multi-perspective: ?- multi_index_report(smartphone_ubiquity).
 * 3. Run tests: ?- run_tests(smartphone_ubiquity_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */