% ============================================================================
% CONSTRAINT STORY: golden_handcuffs
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Compensation Design / Behavioral Economics / Retention Strategy
% ============================================================================

:- module(constraint_golden_handcuffs, []).

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
 * * constraint_id: golden_handcuffs
 * human_readable: Golden Handcuffs (Vesting-Based Retention)
 * domain: economic/social
 * temporal_scope: Biographical (4-10 year vesting cycles)
 * spatial_scope: Regional to Global (Corporate)
 * * SUMMARY:
 * Golden Handcuffs refer to financial incentives, such as restricted stock units 
 * (RSUs) or stock options, that vest over several years. They are designed to 
 * discourage high-value employees from leaving by making the "exit cost" 
 * (forgone unvested equity) prohibitively expensive.
 * * KEY AGENTS:
 * - The Executive/Engineer: The high-performer whose mobility is being restricted.
 * - The Shareholder: The beneficiary of stable, long-term talent retention.
 * - The Recruiter (External): The agent attempting to "buy out" the handcuffs.
 * * NARRATIVE ARC:
 * The constraint is a "Financial Gravity Well." It solves the Dead Sea Effect 
 * by forcing the "fresh water" (high-performers) to stay through economic 
 * weight. It transforms a liquid labor market into a series of timed lock-ins.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(golden_handcuffs, 0, 10).
narrative_ontology:constraint_claim(golden_handcuffs, economic_incentive_design).

% Base extractiveness: Moderate (0.4).
% While the employee receives high compensation, the organization extracts 
% the employee's mobility and bargaining power over the vesting period.
domain_priors:base_extractiveness(golden_handcuffs, 0.4).

% Suppression: Low (0.2).
% The constraint is explicit and transparent in the contract; alternatives 
% (leaving) are visible but carry a clear, calculated price tag.
domain_priors:suppression_score(golden_handcuffs, 0.2).

% Enforcement: Requires active enforcement (Legal contracts/Payroll systems).
domain_priors:requires_active_enforcement(golden_handcuffs).

% Beneficiaries: The Employer (stability), The Patient Investor.
constraint_beneficiary(golden_handcuffs, institutional_employers).

% Victims: The "Locked-In" Employee (may suffer burnout or lack of growth 
% but feels unable to leave due to the financial penalty).
constraint_victim(golden_handcuffs, unvested_employees).

% Metrics
narrative_ontology:constraint_metric(golden_handcuffs, extractiveness, 0.4).
narrative_ontology:constraint_metric(golden_handcuffs, suppression_requirement, 0.2).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE RECRUITER - Noose
   --------------------------------------------------------------------------
   WHO: individual_moderate
   WHEN: immediate (the hiring window)
   WHERE: constrained (must offer "sign-on bonuses" to break the handcuffs)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For an external recruiter trying to hire a candidate, the handcuffs are a 
   "Noose." They strangle the flow of talent and make hiring prohibitively 
   expensive, as the recruiter must often "buy out" the candidate's unvested 
   shares just to reach parity.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    golden_handcuffs,
    noose,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(national)
    )
) :-
    domain_priors:requires_active_enforcement(golden_handcuffs),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE EMPLOYEE - Rope
   --------------------------------------------------------------------------
   WHO: individual_powerful
   WHEN: biographical (wealth building)
   WHERE: arbitrage (can use high equity to negotiate even higher offers)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For the high-earning employee, the handcuffs are a "Rope." They represent 
   a structured ladder to significant wealth. While they limit mobility, 
   the employee often views them as a coordination tool for their own 
   long-term financial security and status.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    golden_handcuffs,
    rope,
    context(
        agent_power(individual_powerful),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :-
    true.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE LABOR MARKET ANALYST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: historical
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The analyst sees golden handcuffs as a "Mountain" of capital allocation. 
   Vesting schedules are a standard, immutable feature of modern tech and 
   finance landscapes. They are the "geography" through which labor 
   must flow, dictating cycles of "cliffs" and "refreshes."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    golden_handcuffs,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:emerges_naturally(golden_handcuffs), % As a market norm
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(golden_handcuffs_tests).

test(wealth_effect_variance) :-
    % Powerful agents see the coordination (Rope); recruiters see the barrier (Noose).
    constraint_indexing:constraint_classification(golden_handcuffs, rope, context(individual_powerful, _, _, _)),
    constraint_indexing:constraint_classification(golden_handcuffs, noose, context(individual_moderate, _, _, _)).

test(vesting_cycle_immutability) :-
    % Analytical perspective treats the market norm as a Mountain.
    constraint_indexing:constraint_classification(golden_handcuffs, mountain, context(analytical, _, _, _)).

:- end_tests(golden_handcuffs_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * Golden Handcuffs are a fascinating "voluntary" constraint. I set 
 * extractiveness to 0.4 because the employee is compensated for their 
 * lack of mobility. It is the opposite of the Dead Sea Effect: it is 
 * the "anti-evaporator" for talent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    market_volatility_impact,
    "If the stock price drops below the strike price, do the handcuffs break?",
    resolution_mechanism("Analysis of employee turnover during market downturns ('underwater' options)"),
    impact("If Stock Drops: The Mountain/Rope vanishes; the Dead Sea Effect resumes."),
    confidence_without_resolution(high)
).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
