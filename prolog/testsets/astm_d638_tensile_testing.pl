% ============================================================================
% CONSTRAINT STORY: astm_d638_tensile_testing
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: ASTM D638: Standard Test Method for Tensile Properties of Plastics
% ============================================================================

:- module(constraint_astm_d638, []).

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
 * * constraint_id: astm_d638_tensile_testing
 * human_readable: ASTM D638 Tensile Property Standard
 * domain: technological/industrial
 * temporal_scope: Active Standard (Current Version)
 * spatial_scope: Global (Industrial Materials Science)
 * * SUMMARY:
 * ASTM D638 establishes the standard "rules of the game" for determining the 
 * tensile properties of unreinforced and reinforced plastics. It 
 * mandates specific specimen geometries (dogbones), testing speeds, and 
 * environmental conditions to ensure data reproducibility across global 
 * supply chains.
 * * KEY AGENTS:
 * - Lab Technician: Powerless subject who must execute the test exactly as 
 * written to ensure valid results.
 * - Quality Assurance Manager: Institutional agent using the standard as a 
 * coordination mechanism for product certification.
 * - Material Scientist (R&D): Analytical agent seeking to understand 
 * physical reality through the lens of the standard's constraints.
 * * NARRATIVE ARC:
 * To the lab worker, ASTM D638 is a Mountain—the physical dimensions and 
 * speeds are immutable facts of their workday. To the industry, 
 * it is a Rope—a functional coordination mechanism allowing a buyer in 
 * Germany to trust a test result from Malaysia. Without this 
 * constraint, the global plastics market would devolve into chaos.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(industrial_testing_regime, 0, 10).
narrative_ontology:constraint_claim(astm_d638_tensile_testing, rope).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: 0.1. Low extraction; the primary "cost" is compliance labor, 
% while the benefit is market-wide interoperability and safety.
domain_priors:base_extractiveness(astm_d638_tensile_testing, 0.1).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: 0.6. Alternatives (ISO 527) are visible but suppressed by 
% regional market dominance and the "stickiness" of specified testing equipment 
% and historical data sets.
domain_priors:suppression_score(astm_d638_tensile_testing, 0.6).

% Enforcement requirements
% Requires active enforcement (Audit/Certification) to be valid in trade.
domain_priors:requires_active_enforcement(astm_d638_tensile_testing).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(astm_d638_tensile_testing, extractiveness, 0.1).
narrative_ontology:constraint_metric(astm_d638_tensile_testing, suppression_requirement, 0.6).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(astm_d638_tensile_testing, global_supply_chains).
constraint_beneficiary(astm_d638_tensile_testing, testing_equipment_manufacturers).
constraint_victim(astm_d638_tensile_testing, innovative_non_standard_testing_startups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: LAB TECHNICIAN - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Must follow Section 10 (Procedure) exactly.
   WHEN: immediate - Today's testing batch.
   WHERE: trapped - Bound by the geometry of the Type I dogbone mold.
   SCOPE: local - The testing machine and specimen in front of them.
   
   WHY THIS CLASSIFICATION:
   The technician cannot "negotiate" with the crosshead speed of 5mm/min. 
   If they change it, the data is invalid. It is an unchangeable law of 
   their immediate environment.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    astm_d638_tensile_testing,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(astm_d638_tensile_testing),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: MATERIAL PROCUREMENT MANAGER - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - Can choose between different materials/standards.
   WHEN: biographical - Planning product lifecycles over 5-10 years.
   WHERE: mobile - Can specify ISO 527 if required by European markets.
   SCOPE: continental - Managing vendors across North America.
   
   WHY THIS CLASSIFICATION:
   ASTM D638 is a functional tool (Rope). It allows the manager to coordinate 
   between vendors using a shared language of "Tensile Strength at Yield" 
   and "Elongation". It is helpful and changeable if the market shifts.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    astm_d638_tensile_testing,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(continental)
    )
) :-
    domain_priors:base_extractiveness(astm_d638_tensile_testing, E),
    E < 0.3,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: ASTM COMMITTEE D20 - Institutional
   --------------------------------------------------------------------------
   
   WHO: institutional - Rule-making power to revise the standard.
   WHEN: generational - Managing a standard that has existed for decades.
   WHERE: arbitrage - Balances industrial needs vs. academic accuracy.
   SCOPE: global - The standard is used worldwide.
   
   WHY THIS CLASSIFICATION:
   For the committee, the standard is a Rope they are constantly weaving. 
   They recognize it as a coordination mechanism that must adapt to new 
   materials like 3D-printed polymers.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    astm_d638_tensile_testing,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 4: SMALL LAB OWNER / TECHNICIAN (FINANCIAL) - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless - Bound by the high capital cost of compliance.
   WHEN: immediate - Ongoing daily maintenance and calibration costs.
   WHERE: trapped - Cannot exit the ASTM ecosystem without losing certification.
   SCOPE: local - The lab's operational margin.
   
   WHY THIS CLASSIFICATION:
   The "Snare" represents the capital extraction required to maintain the 
   standard. This includes the cost of the standard itself, specialized grips, 
   Class B-2 extensometers, and the labor for daily accuracy verification 
   required by Section 5.1.5.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    astm_d638_tensile_testing,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(astm_d638_tensile_testing),
    domain_priors:suppression_score(astm_d638_tensile_testing, S),
    S > 0.5.

% This explicit prior ensures the Snare perspective is mathematically weighted.
domain_priors:base_extractiveness(astm_d638_tensile_testing, 0.1).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(astm_d638_tests).

test(multi_perspective_variance) :-
    % Technician (Mountain) vs Manager (Rope)
    constraint_indexing:constraint_classification(astm_d638_tensile_testing, Mountain, context(individual_powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(astm_d638_tensile_testing, Rope, context(individual_moderate, biographical, mobile, continental)),
    Mountain \= Rope.

test(low_extractiveness_check) :-
    domain_priors:base_extractiveness(astm_d638_tensile_testing, E),
    E < 0.2.

:- end_tests(astm_d638_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. CLASSIFICATION: I primary-labeled this as 'Rope'. Industrial standards 
 * are the quintessential coordination mechanisms. They provide 0.1 
 * extractiveness because they generally solve more problems than they 
 * create.
 * * 2. MOUNTAIN PERSPECTIVE: It's vital to model the technician's view as 
 * 'Mountain'. For the person at the bench, a "Standard" is not a 
 * suggestion; it is the physical architecture of their labor.
 */

omega_variable(
    iso_convergence,
    "Will ASTM D638 and ISO 527 ever fully merge?",
    resolution_mechanism("Monitor Joint Committee activities and ballot results"),
    impact("If Yes: Global Mountain. If No: Continued regional arbitrage/Rope."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: ISO 527
 * Viability: The primary global alternative. Technically similar but 
 * mathematically distinct in some modulus calculations.
 * Suppression: Market-driven. US government agencies and Boeing/Ford 
 * often mandate ASTM, suppressing ISO adoption in NA.
 * * CONCLUSION:
 * The existence of ISO 527 proves ASTM D638 isn't a "natural law" (Mountain), 
 * but an institutional choice (Rope).
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [astm_d638].
% 2. Run: ?- constraint_indexing:multi_index_report(astm_d638_tensile_testing).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
