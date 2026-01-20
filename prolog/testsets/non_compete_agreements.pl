% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: non_compete_agreements
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Labor Law / Employment Contracts / FTC Ban Rulings
% ============================================================================

:- module(constraint_non_compete, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
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
 * * constraint_id: non_compete_agreements
 * human_readable: Non-Compete Agreements (Post-Employment Restraint)
 * domain: political/economic/social
 * temporal_scope: Biographical (Typically 6 months to 2 years post-exit)
 * spatial_scope: National to Regional (State-dependent)
 * * SUMMARY:
 * A non-compete agreement is a legal contract where an employee agrees not to 
 * enter into competition with an employer for a specified period and 
 * geographic area after the employment relationship ends. It is a direct 
 * legal restraint on labor mobility.
 * * KEY AGENTS:
 * - The Employer: Seeks to protect trade secrets and training investments.
 * - The Employee: Seeks to leverage their skills in the open market.
 * - The Regulator (e.g., FTC): Attempts to balance market competition with 
 * proprietary protections.
 * * NARRATIVE ARC:
 * Unlike Golden Handcuffs, which use a "Carrot" (equity), Non-Competes use 
 * a "Stick" (legal threat). The constraint functions as a "Border Control" 
 * on human capital, preventing the flow of knowledge from one firm to another.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(non_compete_agreements, 0, 10).
narrative_ontology:constraint_claim(non_compete_agreements, rope).

% Base extractiveness: High (0.8).
% The firm extracts the employee's future earning potential and mobility 
% often without additional compensation beyond the base salary.
domain_priors:base_extractiveness(non_compete_agreements, 0.8).

% Suppression: High (0.7).
% While the contract is known, the full legal implications and "chilling effect" 
% often suppress the visibility of viable alternative career paths.
domain_priors:suppression_score(non_compete_agreements, 0.7).

% Enforcement: Requires active enforcement (Litigation/Injunctions).
domain_priors:requires_active_enforcement(non_compete_agreements).

% Beneficiaries: Established Incumbent Firms (market protection).
constraint_beneficiary(non_compete_agreements, incumbent_corporations).

% Victims: Low-to-Mid Level Laborers (locked out of their industry).
constraint_victim(non_compete_agreements, transitioning_workers).

% Metrics
narrative_ontology:constraint_metric(non_compete_agreements, extractiveness, 0.8).
narrative_ontology:constraint_metric(non_compete_agreements, suppression_requirement, 0.7).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE JUNIOR WORKER - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless
   WHEN: immediate
   WHERE: trapped (cannot work in their trained field)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For a junior worker with limited savings, a non-compete is a "Noose." 
   They cannot afford the legal fees to fight it, and the threat of an 
   injunction prevents them from taking any new job in their sector. 
   It is a pure extraction of their right to work.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    non_compete_agreements,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(non_compete_agreements, E),
    E > 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE IP ATTORNEY - Rope
   --------------------------------------------------------------------------
   WHO: institutional/individual_moderate
   WHEN: biographical
   WHERE: mobile
   SCOPE: regional
   
   WHY THIS CLASSIFICATION:
   To the attorney or corporate strategist, the agreement is a "Rope." 
   It is a tool used to coordinate and protect the "moat" around the 
   company's intellectual property. It structures the exit process 
   and ensures that "training investments" aren't immediately 
   captured by competitors.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    non_compete_agreements,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :-
    true.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE POLICY MAKER - Mountain
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: historical
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The policymaker sees Non-Competes as a "Mountain" of institutional 
   precedent. Despite recent bans, the historical reality of post-employment 
   restraint is a massive feature of the economic landscape that 
   dictates the flow of innovation (e.g., Silicon Valley's growth 
   due to California's refusal to enforce them).
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    non_compete_agreements,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:requires_active_enforcement(non_compete_agreements),
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(non_compete_tests).

test(power_asymmetry_noose) :-
    % Testing that powerless agents (workers) see a Noose.
    constraint_indexing:constraint_classification(non_compete_agreements, noose, context(individual_powerless, _, _, _)).

test(legal_precedent_mountain) :-
    % Testing that analytical views see the structural Mountain.
    constraint_indexing:constraint_classification(non_compete_agreements, mountain, context(analytical, _, _, _)).

:- end_tests(non_compete_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * Non-competes differ from Golden Handcuffs because they are non-voluntary 
 * post-exit. I set extractiveness to 0.8 because there is rarely a 
 * financial upside for the worker to sign one; it is a "tax on exit."
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    ftc_ban_enforceability,
    "Will federal bans on non-competes survive judicial review in 2026?",
    resolution_mechanism("Supreme Court ruling on administrative agency overreach"),
    impact("If Upheld: The Mountain crumbles; the Noose is cut. If Struck Down: The Noose tightens."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
