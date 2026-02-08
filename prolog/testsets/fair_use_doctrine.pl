% ============================================================================
% CONSTRAINT STORY: fair_use_doctrine
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: U.S. Copyright Act, 17 U.S.C. § 107
% ============================================================================

:- module(constraint_fair_use, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
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
 * * constraint_id: fair_use_doctrine
 * human_readable: Fair Use (The Expression Safety Valve)
 * domain: social/legal/technological
 * temporal_scope: Biographical to Historical
 * spatial_scope: National (USA-specific, though Berne-aligned)
 * * SUMMARY:
 * Fair Use is a legal doctrine that promotes freedom of expression by 
 * permitting the unlicensed use of copyright-protected works in certain 
 * circumstances, such as criticism, comment, news reporting, teaching, 
 * scholarship, and research.
 * * KEY AGENTS:
 * - The Remixer/Scholar: Uses existing work to create new meaning.
 * - The Copyright Holder: Attempts to define the limits of "transformative" use.
 * - The Judge: Balances the four statutory factors to determine legality.
 * * NARRATIVE ARC:
 * Fair Use functions as the "Escape Hatch" from the Copyright Snare. 
 * It ensures that the "Mountain" of intellectual property does not 
 * become an impassable wall for new creativity. However, its 
 * case-by-case nature makes it a "Foggy Path"—legally risky to navigate.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(fair_use_doctrine, 0, 10).
narrative_ontology:constraint_claim(fair_use_doctrine, rope).

% Base extractiveness: Low (0.1).
% It is designed to *prevent* extraction by allowing free use for 
% socially beneficial purposes.
domain_priors:base_extractiveness(fair_use_doctrine, 0.1).

% Suppression: Moderate (0.4).
% While the law exists, the fear of litigation (chilling effect) 
% often suppresses its actual exercise.
domain_priors:suppression_score(fair_use_doctrine, 0.4).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(fair_use_doctrine, extractiveness, 0.1).
narrative_ontology:constraint_metric(fair_use_doctrine, suppression_requirement, 0.4).

% Enforcement: Requires active enforcement (Affirmative defense in court).
domain_priors:requires_active_enforcement(fair_use_doctrine).

% Beneficiaries: Documentarians, Educators, Parodists, AI Developers.
constraint_beneficiary(fair_use_doctrine, secondary_creators).

% Victims: Copyright Trolls (who seek to extract fees for every use).
constraint_victim(fair_use_doctrine, copyright_maximalists).

% Metrics
/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE PARODIST - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate
   WHEN: biographical
   WHERE: mobile (can pivot between works)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For a creator like a YouTuber or satirist, Fair Use is a "Rope." 
   It provides the structural support needed to climb over the 
   barriers of copyright. It allows them to use the "Expression" 
   of others to coordinate their own creative message.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    fair_use_doctrine,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(fair_use_doctrine, E),
    E < 0.3,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE UNASSISTED CREATOR - Snare
   --------------------------------------------------------------------------
   WHO: powerless
   WHEN: immediate
   WHERE: trapped (cannot afford legal counsel)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For a small creator facing a "Cease and Desist" from a major studio, 
   Fair Use is a "Snare." Because it is an *affirmative defense* (meaning you have to be sued first to prove you are right), 
   the legal cost of exercising this right is a bottleneck that 
   strangles their work.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    fair_use_doctrine,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    domain_priors:requires_active_enforcement(fair_use_doctrine),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SUPREME COURT - Mountain
   --------------------------------------------------------------------------
   WHO: institutional
   WHEN: historical
   WHERE: analytical
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   The court sees Fair Use as a "Mountain"—a permanent, foundational 
   feature of the First Amendment landscape. It is the "built-in 
   accommodation" that allows the Copyright Act to coexist with 
   Free Speech.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    fair_use_doctrine,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(national)
    )
) :-
    true.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(fair_use_tests).

test(defense_cost_snare) :-
    % Powerless agents see a Snare due to enforcement costs.
    constraint_indexing:constraint_classification(fair_use_doctrine, snare, context(powerless, _, _, _)).

test(free_speech_mountain) :-
    % Institutional/Analytical view sees the Mountain of legal doctrine.
    constraint_indexing:constraint_classification(fair_use_doctrine, mountain, context(institutional, _, _, _)).

:- end_tests(fair_use_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * Fair Use is technically an "Exit Option" for Copyright, but I 
 * profiled it as a standalone constraint because it has its own 
 * rules of access. The "Snare" classification highlights the 
 * "Fair Use Paradox": it is a right that many are too poor to use.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    automated_adjudication,
    "Will platforms implement AI judges that can accurately determine Fair Use in real-time?",
    resolution_mechanism("Performance audit of DMCA takedown automation vs. court reversals"),
    impact("If Yes: The Snare loosens (instant defense). If No: Algorithmic suppression remains the status quo."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Multifile declarations for new predicates ---
:- multifile
    domain_priors:theater_ratio/2.

% --- Theater ratio (missing from base properties) ---
% Coordination mechanism in social domain — moderate institutional framing
domain_priors:theater_ratio(fair_use_doctrine, 0.13).
narrative_ontology:constraint_metric(fair_use_doctrine, theater_ratio, 0.13).

% --- Analytical perspective classification (missing) ---
% chi = 0.1 * 1.15 (analytical) * 1.2 (global) = 0.138
% Classification: scaffold
constraint_indexing:constraint_classification(fair_use_doctrine, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).
