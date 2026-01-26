% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: qualified_immunity
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Legal Doctrine / U.S. Supreme Court Jurisprudence
% ============================================================================

:- module(constraint_qualified_immunity, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_claim/2,
    constraint_beneficiary/2,
    constraint_victim/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: qualified_immunity
 * human_readable: Qualified Immunity Doctrine
 * domain: political/legal
 * temporal_scope: 1967-Present (Pierson v. Ray to modern era)
 * spatial_scope: National (United States)
 * * SUMMARY:
 * Qualified immunity is a legal doctrine that shields government officials 
 * from being held personally liable for constitutional violations—like the 
 * use of excessive force—as long as their officials did not violate 
 * "clearly established" statutory or constitutional rights. In practice, 
 * this creates a "catch-22" where a right cannot be clearly established 
 * without a prior case involving nearly identical facts.
 * * KEY AGENTS:
 * - The State Official: Protected from the "chilling effect" of lawsuits.
 * - The Plaintiff: Seeking redress for civil rights violations.
 * - The Judiciary: Managing the "floodgates" of litigation.
 * * NARRATIVE ARC:
 * Originally intended as a "good faith" defense (Rope), the doctrine has 
 * evolved through judicial refinement into a nearly insurmountable barrier 
 * (Mountain) for victims of police misconduct, effectively functioning 
 * as a mechanism that extracts justice (Snare) from the powerless.
 */

/* ==========================================================================
   2. BASE PROPERTIES (The "Reality" Layer)
   ========================================================================= */

% Required for structural integration
narrative_ontology:interval(qi_interval, 0, 10).
narrative_ontology:constraint_claim(qualified_immunity, rope).

% Base extractiveness: 0.8 (High)
% Rationale: It extracts "remedy" from the victim. Even when a constitutional 
% violation is admitted, the victim is denied compensation, transferring 
% the cost of the injury from the state/official to the individual.
domain_priors:base_extractiveness(qualified_immunity, 0.8).

% Suppression score: 0.7 (High)
% Rationale: The "clearly established" requirement suppresses the creation 
% of new precedents. Courts often dismiss cases on immunity grounds without 
% ruling on the underlying constitutionality, keeping rights in a "state of 
% non-establishment."
domain_priors:suppression_score(qualified_immunity, 0.7).

% Enforcement: Requires active judicial enforcement through summary judgment.
domain_priors:requires_active_enforcement(qualified_immunity).

% Metrics for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(qualified_immunity, extractiveness, 0.8).
narrative_ontology:constraint_metric(qualified_immunity, suppression_requirement, 0.7).

% Beneficiaries: Law enforcement officers and government entities.
constraint_beneficiary(qualified_immunity, state_officials).

% Victims: Plaintiffs seeking damages for civil rights violations.
constraint_victim(qualified_immunity, civil_rights_plaintiffs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE JUDICIAL ADMINISTRATOR - ROPE
   --------------------------------------------------------------------------
   
   WHO: institutional (The Court system).
   WHEN: historical (Maintaining systemic stability over decades).
   WHERE: arbitrage (Balancing social costs vs. individual rights).
   SCOPE: national.
   
   WHY THIS CLASSIFICATION:
   From the bench, the doctrine is a Rope. It is a necessary tool to prevent 
   the "social costs" of litigation from paralyzing government functions. It 
   protects officials from the fear of personal financial ruin for making 
   split-second decisions in "gray areas" of the law.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    qualified_immunity,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE CIVIL RIGHTS ATTORNEY - MOUNTAIN
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (A professional navigating the system).
   WHEN: biographical (A career spent litigating these cases).
   WHERE: constrained (Must find a 'factually identical' case).
   SCOPE: national.
   
   WHY THIS CLASSIFICATION:
   For the litigator, QI is a Mountain. It is an immovable feature of the 
   legal landscape that makes most civil rights cases "dead on arrival." 
   The factual specificity required to overcome it makes it appear as an 
   impenetrable wall rather than a flexible rule.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    qualified_immunity,
    mountain,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE UNARMED VICTIM - NOOSE
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (A subject of excessive force).
   WHEN: immediate (Seeking medical/legal remedy now).
   WHERE: trapped (The legal system is the only venue for redress).
   SCOPE: local.
   
   WHY THIS CLASSIFICATION:
   For the victim, the doctrine is a Snare. They have suffered a physical 
   injury or loss of liberty, yet the legal system uses its own technicality 
   to strangle their right to a remedy. The "logic" of the doctrine pulls 
   tighter the more egregious the violation, as unique violations are, by 
   definition, not "clearly established."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    qualified_immunity,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(qualified_immunity, Type1, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(qualified_immunity, Type2, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(qualified_immunity, Type3, context(agent_power(individual_powerless), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3.

test(extraction_asymmetry) :-
    % Beneficiary (Officer) benefits from 0% liability; Victim (Plaintiff) suffers 100% loss.
    domain_priors:base_extractiveness(qualified_immunity, E),
    E > 0.7.

test(precedent_suppression) :-
    % Test if the suppression score aligns with the "catch-22" narrative.
    domain_priors:suppression_score(qualified_immunity, S),
    S >= 0.7.

:- end_tests(qualified_immunity_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.8): Ranked very high. Unlike other legal defenses, 
 * QI often functions even when a violation is proven, purely because 
 * of factual mismatch, representing a total extraction of the right to remedy.
 * 2. SUPPRESSION (0.7): The "clearly established" rule is a textbook 
 * suppression mechanism—it penalizes novelty and makes alternatives 
 * (new precedents) nearly impossible to reach.
 * 3. NOOSE ARGUMENT: The "catch-22" (you need a case to win, but you can't 
 * get a case because you can't win) is a perfect logical Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    clearly_established_definition,
    "Will the Supreme Court ever define 'clearly established' without requiring factual identity?",
    resolution_mechanism("Analysis of future SCOTUS rulings on the 'unobviousness' of violations"),
    impact("If Yes: QI shifts from Mountain to Rope. If No: It remains a Snare."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: State/Municipal Liability (Section 1983 Reform)
 * Viability: High. Indemnification would protect officers while allowing 
 * victims to be paid.
 * Suppression: High. Politically blocked by "tough on crime" narratives.
 * * ALTERNATIVE 2: Reverting to the 'Good Faith' Standard
 * Viability: Moderate. Requires judicial reversal.
 * * CONCLUSION:
 * The existence of suppressed alternatives (like municipal liability) 
 * suggests that QI is an artificial Snare, not a natural Mountain.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_qualified_immunity].
 * 2. Multi-perspective: ?- multi_index_report(qualified_immunity).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
