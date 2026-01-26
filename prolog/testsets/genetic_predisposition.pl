% ============================================================================
% CONSTRAINT STORY: genetic_predisposition
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Behavioral Genetics / Medical Oncology / Deferential Realism
% ============================================================================

:- module(constraint_genetic_predisposition, []).

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
 * * constraint_id: genetic_predisposition
 * human_readable: Genetic Predisposition
 * domain: technological/social
 * temporal_scope: Permanent (Biological Basis of Life)
 * spatial_scope: Global (Human Population)
 * * SUMMARY:
 * Genetic predisposition refers to an increased likelihood of developing a 
 * particular disease or trait based on an individual's genetic makeup. It 
 * represents a structural biological filter that influences health outcomes, 
 * personality, and capabilities, often operating as an "invisible hand" 
 * before environmental factors are introduced.
 * * KEY AGENTS:
 * - The Genomic Scientist (Analytical): Maps the statistical correlations between 
 * loci and phenotypes.
 * - The Public Health Architect (Institutional): Uses population-level genetic 
 * data to coordinate screening and prevention strategies.
 * - The High-Risk Patient (Individual Powerless): Subject to the biological 
 * "lottery," experiencing the predisposition as a fixed threat to health and 
 * insurance eligibility.
 * * NARRATIVE ARC:
 * Predisposition functions as a "Mountain" of biological reality—a fixed 
 * sequence of base pairs. To the healthcare system, it is a "Rope" (a tool for 
 * personalized medicine). However, for the individual at risk of a terminal 
 * condition, it becomes a "Snare," as the genetic "destiny" extracts peace of 
 * mind and creates a trap of medical surveillance or uninsurability.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor
narrative_ontology:interval(genetic_predisposition_interval, 0, 10).
narrative_ontology:constraint_claim(genetic_predisposition, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: High (0.6). It extracts health, time, and resources. In 
% socio-economic systems, it also extracts financial security through 
% "genetic discrimination" and the cost of preventative care.
domain_priors:base_extractiveness(genetic_predisposition, 0.6).

% Suppression score (0.0-1.0)
% Rationale: Moderate (0.5). Alternatives like "lifestyle-only" models are 
% visible but increasingly suppressed as genomic data reveals the underlying 
% biological constraints that limit the efficacy of simple behavioral changes.
domain_priors:suppression_score(genetic_predisposition, 0.5).

% Enforcement requirements
% Emerges naturally from inheritance; requires active social enforcement (laws) 
% to mitigate the resulting inequalities.
domain_priors:emerges_naturally(genetic_predisposition).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(genetic_predisposition, extractiveness, 0.6).
narrative_ontology:constraint_metric(genetic_predisposition, suppression_requirement, 0.5).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(genetic_predisposition, [pharmaceutical_industry, insurance_actuaries]).
constraint_victim(genetic_predisposition, [high_risk_individuals, economically_disadvantaged]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE GENETICIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of the biological substrate.
   WHEN: civilizational - Viewing the genome as an evolutionary archive.
   WHERE: trapped - DNA cannot be rewritten within the existing biological frame.
   SCOPE: global - Universal to the species.
   
   WHY THIS CLASSIFICATION:
   To the scientist, the predisposition is a Mountain. It is an unchangeable 
   feature of the individual's hardware. One cannot "negotiate" with a 
   BRCA1 mutation; it is a fixed peak in the person's biological topography 
   that dictates the risk landscape regardless of social policy.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    genetic_predisposition,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :-
    constraint_indexing:effective_immutability_for_context(
        context(analytical, civilizational, trapped, global),
        mountain
    ),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE HEALTH MINISTER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to shape screening protocols.
   WHEN: biographical - Managing population health over decades.
   WHERE: arbitrage - Can shift resources from general care to targeted screening.
   SCOPE: national - Country-wide healthcare systems.
   
   WHY THIS CLASSIFICATION:
   For the institution, genetic data is a Rope. It is a coordination mechanism 
   that pulls the health system toward efficiency. By identifying predisposed 
   groups, the state can "tether" resources to those who need them most, 
   transforming a random biological risk into a structured, manageable plan.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    genetic_predisposition,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(genetic_predisposition, E),
    E > 0.3, % Institutional power manages the extraction
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE HIGH-RISK INDIVIDUAL - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to their own biology and insurance rules.
   WHEN: immediate - The immediate fear of diagnosis or loss of coverage.
   WHERE: constrained - No way to "exit" one's own genetic identity.
   SCOPE: local - Immediate life and family environment.
   
   WHY THIS CLASSIFICATION:
   For the individual with a severe predisposition, the genetic code is a Snare. 
   The biological "debt" they were born with extracts their peace of mind and 
   financial security. As they age, the "likelihood" of disease tightens, 
   limiting their career choices (staying for insurance) and personal freedom.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    genetic_predisposition,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(genetic_predisposition, E),
    E > 0.5, % High felt extraction of agency and vitality
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(genetic_predisposition_tests).

test(multi_perspective_variance) :-
    % Analyst sees Mountain, Institutional sees Rope, Powerless sees Snare
    constraint_indexing:constraint_classification(genetic_predisposition, mountain, context(analytical, civilizational, trapped, global)),
    constraint_indexing:constraint_classification(genetic_predisposition, rope, context(institutional, biographical, arbitrage, national)),
    constraint_indexing:constraint_classification(genetic_predisposition, snare, context(individual_powerless, immediate, constrained, local)).

test(power_extractiveness_scaling) :-
    ContextPowerless = context(individual_powerless, immediate, constrained, local),
    ContextPowerful = context(institutional, biographical, arbitrage, national),
    constraint_indexing:extractiveness_for_agent(genetic_predisposition, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(genetic_predisposition, ContextPowerful, Score2),
    Score1 > Score2.

test(time_immutability) :-
    % Civilizational view of biology = Mountain
    constraint_indexing:effective_immutability_for_context(context(analytical, civilizational, trapped, global), mountain).

:- end_tests(genetic_predisposition_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.6):
 * Predisposition extracts health and time (biological). It also extracts 
 * wealth (medical costs) and agency (insurance lock-in).
 * * 2. SUPPRESSION SCORE (0.5):
 * The "blank slate" alternative is highly suppressed by modern diagnostics. 
 * Individuals are forced to view themselves as "pre-symptomatic" subjects.
 * * 3. PERSPECTIVE SELECTION:
 * The Researcher (Mountain), the Policy Maker (Rope), and the Patient (Snare) 
 * represent the three primary ways biological "truth" enters the social world.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epigenetic_plasticity_limit,
    "To what degree can environmental factors (Epigenetics) 'untie' the Snare 
    of genetic predisposition?",
    resolution_mechanism("Long-term mapping of gene-environment interactions 
    across diverse populations"),
    impact("If High: Predisposition becomes a Rope (changeable). If Low: It 
    remains a permanent Mountain."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Universal Healthcare/Risk Pooling
 * Viability: High. It socializes the "biological debt" so the individual 
 * doesn't experience a financial Snare.
 * Suppression: Varies by region; often suppressed by private insurance 
 * interests to maintain extraction of high premiums.
 * * CONCLUSION:
 * The presence of socializing alternatives (Universal Risk Pooling) that are 
 * suppressed in certain regimes confirms that Genetic Predisposition is often 
 * a Snare by social design, even if it is a Mountain by biological nature.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_genetic_predisposition].
 * 2. Multi-perspective: ?- multi_index_report(genetic_predisposition).
 * 3. Run tests: ?- run_tests(genetic_predisposition_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
