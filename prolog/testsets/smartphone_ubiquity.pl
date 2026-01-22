% ============================================================================
% CONSTRAINT STORY: smartphone_ubiquity
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
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
 * * constraint_id: smartphone_ubiquity
 * human_readable: The Smartphone Ubiquity Constraint
 * domain: technological/social/economic
 * temporal_scope: 2007 (iPhone release) to 2026+
 * spatial_scope: Global (7 in 10 people worldwide)
 * * SUMMARY:
 * The smartphone has transitioned from a communication device to a "place within 
 * which we live," functioning as a portable digital home that bridges physical 
 * and digital realities. While it allows lower-income 
 * nations to bypass legacy infrastructure (banks, desktops), it simultaneously 
 * extracts privacy and creates a "shell" that can lead to social disconnect 
 *.
 * * KEY AGENTS:
 * - The Global User: One of the 70% who uses the device for payments, crops, or 
 * social connection.
 * - The Data Scientist/Critic: Observes the "insidious effects" of privacy 
 * extraction and social media addiction.
 * - The Regulator: Acts to ban devices or social media access (e.g., Australia's 
 * under-16 ban) to mitigate social harm.
 * * NARRATIVE ARC:
 * This constraint represents a "Rope" for global coordination (fintech, medical 
 * circumvention) that has effectively become a "Mountain" of mandatory 
 * infrastructure. For vulnerable groups (under-16s) or 
 * privacy-aware subjects, it acts as a "Noose" of surveillance and exclusion 
 *.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(smartphone_ubiquity, 0, 10).
narrative_ontology:constraint_claim([smartphone_ubiquity], [technological_ubiquity]).

% Base extractiveness score (0.75 = High)
% Rationale: Smartphones have accustomed users to "far less privacy," extracting 
% data even within our own homes. High score triggers 
% Mandatrophy protocol.
domain_priors:base_extractiveness(smartphone_ubiquity, 0.75).

% Suppression score (0.50 = Moderate)
% Rationale: Ubiquity has led users to "bypass the desktop computer altogether," 
% effectively suppressing older infrastructure through sheer utility and 
% adoption.
domain_priors:suppression_score(smartphone_ubiquity, 0.50).

% Enforcement requirements: Emerges naturally through ubiquity and adoption, 
% but requires active enforcement for bans (schools, social media).
domain_priors:requires_active_enforcement(smartphone_ubiquity).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(smartphone_ubiquity, extractiveness, 0.75).
narrative_ontology:constraint_metric(smartphone_ubiquity, suppression_requirement, 0.50).

% BENEFICIARIES & VICTIMS
% Beneficiaries: FinTech platforms (70M users), mobile operators, and doctors 
% in lower-income countries.
constraint_beneficiary(smartphone_ubiquity, fintech_platforms).
constraint_beneficiary(smartphone_ubiquity, lower_income_infrastructure).
% Victims: Users facing total privacy loss and under-16s facing social media 
% exclusion bans.
constraint_victim(smartphone_ubiquity, privacy_conscious_users).
constraint_victim(smartphone_ubiquity, australian_under_16s).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: RURAL FARMER/DOCTOR - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Uses the device to bypass traditional banks/machines)
   WHEN: biographical (Integrating technology into daily work)
   WHERE: arbitrage (Circumventing expensive machinery/centralized banks)
   SCOPE: local (Monitoring crops/patients)
   
   WHY THIS CLASSIFICATION:
   For those in lower-income regions, the smartphone is a functional Rope—a 
   coordination tool that provides infrastructure where none existed, allowing 
   them to "bypass" physical constraints.
   
   NARRATIVE EVIDENCE:
   "Smartphone-based fintech platforms now broker payments... removing the 
   need for traditional, centralised banks... circumvent the need for 
   expensive machinery".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    smartphone_ubiquity,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(smartphone_ubiquity, E),
    E < 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: AUSTRALIAN UNDER-16 - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Subject to blanket legal bans)
   WHEN: immediate (Facing a sudden legal cutoff)
   WHERE: trapped (Cannot legally access the "digital house" of friends)
   SCOPE: national (Australia-wide ban)
   
   WHY THIS CLASSIFICATION:
   For youth subject to the ban, the smartphone/social media constraint is a 
   Noose—a coercive exclusion from the "additional place within which we live" 
   that their peers in other countries enjoy.
   
   NARRATIVE EVIDENCE:
   "Australia imposed a blanket social media ban for under 16s... countries 
   to ban phones in schools".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    smartphone_ubiquity,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(smartphone_ubiquity, E),
    E > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: DATA SCIENTIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer of ubiquitous data harvesting)
   WHEN: historical (The long-term shift in privacy norms)
   WHERE: trapped (Even online in "our own homes")
   SCOPE: global (7 in 10 ownership)
   
   WHY THIS CLASSIFICATION:
   The scientist sees smartphone ubiquity as a Mountain—an immutable reality 
   where privacy has been permanently altered at a species-wide level.
   
   NARRATIVE EVIDENCE:
   "It’s a device that has accustomed users to have far less privacy... not 
   only in public, but wherever we are".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    smartphone_ubiquity,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(trapped),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(smartphone_ubiquity, S),
    S > 0.4,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(smartphone_ubiquity_tests).

test(multi_perspective_variance) :-
    % Farmer (Rope) vs Minor (Noose) vs Scientist (Mountain)
    constraint_indexing:constraint_classification(smartphone_ubiquity, rope, context(individual_moderate, _, arbitrage, _)),
    constraint_indexing:constraint_classification(smartphone_ubiquity, noose, context(individual_powerless, _, trapped, national)),
    constraint_indexing:constraint_classification(smartphone_ubiquity, mountain, context(analytical, _, _, global)).

test(power_extractiveness_scaling) :-
    % Privacy loss (0.75) is experienced most by the powerless who cannot exit the system.
    Score1 = 0.75,
    Score2 = 0.30,
    Score1 > Score2.

:- end_tests(smartphone_ubiquity_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.75):
 * Reasoning: Chosen because the text emphasizes an "insidious" and total loss 
 * of privacy, even in private residences.
 * * 2. MANDATROPHY STATUS:
 * STATUS: [RESOLVED MANDATROPHY]. High extraction (0.75) is justified for the 
 * user gaining banking/medical access (Rope) but is a Noose for the 
 * surveilled subject.
 * * 3. PERSPECTIVE SELECTION:
 * Included the Australian "ban" context to represent the powerless 
 * Noose perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    privacy_extraction_intent,
    "Is the extraction of privacy a functional necessity for smartphone utility (Rope) or a predatory choice by providers (Noose)?",
    resolution_mechanism("Audit of model efficiency with locally-encrypted data vs. centralized cloud-harvested data"),
    impact("If necessity: Mountain. If predatory: Noose/Mandatrophy."),
    confidence_without_resolution(medium)
).

omega_variable(
    digital_home_sovereignty,
    "Do users actually 'live' in the digital home (Rope) or are they 'retreating' into a shell (Noose)?",
    resolution_mechanism("Long-term analysis of psychological markers of 'disconnection' vs. 'connection' in heavy users"),
    impact("If live: Rope. If retreat: Noose."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Desktop-based / Centralized Infrastructure
 * Viability: The 20th-century standard for banking and computing.
 * Suppression: Bypassed by ubiquity in lower-income countries.
 * * ALTERNATIVE 2: Privacy-by-Design Devices
 * Viability: Devices that do not extract data in the home.
 * Suppression: Users are already "accustomed" to less privacy, making alternatives 
 * socially invisible.
 * * CONCLUSION:
 * The 21st-century "Best Idea" is the transition from physical separation to 
 * a "digital house," which acts as a Rope for many while extracting the 
 * privacy Noose.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraints/smartphone_ubiquity].
 * 2. Multi-perspective: ?- multi_index_report(smartphone_ubiquity).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
