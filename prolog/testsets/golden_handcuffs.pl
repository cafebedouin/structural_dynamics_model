% ============================================================================
% CONSTRAINT STORY: golden_handcuffs
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
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
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * 
 * constraint_id: golden_handcuffs
 * human_readable: Golden Handcuffs (Vesting-Based Retention)
 * domain: economic/social
 * temporal_scope: Biographical (4-10 year vesting cycles)
 * spatial_scope: Regional to Global (Corporate)
 * 
 * SUMMARY:
 * Golden Handcuffs refer to financial incentives, such as restricted stock units 
 * (RSUs) or stock options, that vest over several years. They are designed to 
 * discourage high-value employees from leaving by making the "exit cost" 
 * (forgone unvested equity) prohibitively expensive.
 * 
 * KEY AGENTS:
 * - The Company (Institutional): Designs the compensation system to ensure stability.
 * - The Senior Employee (Individual Powerful): A high-performer with significant unvested equity.
 * - The Junior Employee (Individual Powerless): A new hire with standard vesting terms and little negotiating power.
 * 
 * NARRATIVE ARC:
 * The constraint is a "Financial Gravity Well." It solves the "Dead Sea Effect" 
 * by forcing high-performers to stay through economic weight. It transforms a 
 * liquid labor market into a series of timed lock-ins, creating friction and stability.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(golden_handcuffs, 0, 10).
narrative_ontology:constraint_claim(golden_handcuffs, tangled_rope).
narrative_ontology:human_readable(golden_handcuffs, "Golden Handcuffs (Vesting-Based Retention)").
narrative_ontology:topic_domain(golden_handcuffs, "economic/social").

% Base extractiveness: Moderate (0.4).
% While the employee receives high compensation, the organization extracts 
% the employee's mobility and bargaining power over the vesting period.
domain_priors:base_extractiveness(golden_handcuffs, 0.4).

% Suppression: Low (0.2).
% The constraint is explicit and transparent in the contract; alternatives 
% (leaving) are visible but carry a clear, calculated price tag.
domain_priors:suppression_score(golden_handcuffs, 0.2).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(golden_handcuffs, extractiveness, 0.4).
narrative_ontology:constraint_metric(golden_handcuffs, suppression_requirement, 0.2).

% Enforcement: Requires active enforcement (Legal contracts/Payroll systems).
domain_priors:requires_active_enforcement(golden_handcuffs).

% Beneficiaries: The Employer (stability), The Patient Investor.
narrative_ontology:constraint_beneficiary(golden_handcuffs, institutional).

% Victims: The "Locked-In" Employee (may suffer burnout or lack of growth 
% but feels unable to leave due to the financial penalty).
narrative_ontology:constraint_victim(golden_handcuffs, powerless).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE COMPANY (INSTITUTIONAL) - Rope
   --------------------------------------------------------------------------
   WHO: institutional (rule-making power)
   WHEN: generational (long-term stability)
   WHERE: mobile (can change compensation strategy)
   SCOPE: global (competing in a global talent market)
   
   WHY THIS CLASSIFICATION:
   For the company, golden handcuffs are a 'Rope'. It's a critical coordination 
   mechanism to ensure project continuity, retain institutional knowledge, and 
   align employee incentives with long-term shareholder value. It's a tool for stability.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    golden_handcuffs,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE SENIOR EMPLOYEE (INDIVIDUAL POWERFUL) - Tangled Rope
   --------------------------------------------------------------------------
   WHO: powerful (significant unvested equity)
   WHEN: biographical (wealth building phase)
   WHERE: arbitrage (can leverage their position for better offers)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For the senior, high-performing employee, the handcuffs are a 'Tangled Rope'. 
   They are a coordination tool for immense wealth creation (the rope part), but 
   they also extract mobility and bind the employee to the firm's fate, 
   creating a sense of being trapped despite the financial benefit (the tangled part).
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    golden_handcuffs,
    tangled_rope,
    context(
        agent_power(powerful),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE JUNIOR EMPLOYEE (INDIVIDUAL POWERLESS) - Mountain
   --------------------------------------------------------------------------
   WHO: powerless (no negotiating power)
   WHEN: immediate (first 1-2 years before cliff vest)
   WHERE: trapped (cannot afford to leave and forfeit all equity)
   SCOPE: local (focused on their immediate job and team)
   
   WHY THIS CLASSIFICATION:
   For a junior employee, the 4-year vesting schedule is a 'Mountain'. It's a 
   non-negotiable, immutable feature of the employment landscape. The time 
   horizon to any significant financial reward is so long that it feels like a 
   permanent, unchangeable feature of their reality.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    golden_handcuffs,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(golden_handcuffs_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(golden_handcuffs, Type1, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(golden_handcuffs, Type2, context(agent_power(powerful), _, _, _)),
    constraint_indexing:constraint_classification(golden_handcuffs, Type3, context(agent_power(powerless), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

test(power_extractiveness_scaling) :-
    ContextPowerless = context(agent_power(powerless), time_horizon(biographical), exit_options(trapped), spatial_scope(local)),
    ContextPowerful = context(agent_power(powerful), time_horizon(biographical), exit_options(arbitrage), spatial_scope(national)),
    constraint_indexing:extractiveness_for_agent(golden_handcuffs, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(golden_handcuffs, ContextPowerful, Score2),
    Score1 > Score2.

:- end_tests(golden_handcuffs_tests).

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
 * 1. EXTRACTIVENESS SCORE (0.4):
 *    Reasoning: The constraint is a trade: mobility for wealth. The extraction isn't purely predatory; the employee receives substantial compensation. However, the company extracts the employee's ability to participate in the open market for their skills for a defined period, which is a tangible extraction of opportunity.
 *    Evidence: Standard 4-year vesting cliffs create a clear financial barrier to exit.
 *    Uncertainty: The value of the extracted mobility is highly dependent on the individual and the market, making 0.4 an estimate.
 * 
 * 2. PERSPECTIVE SELECTION:
 *    Chose 3 perspectives (Institutional, Individual Powerful, Individual Powerless) to satisfy the linter requirements and to show how the same compensation structure is perceived differently based on power and leverage.
 * 
 * 3. CLASSIFICATION RATIONALE:
 *    - Institutional -> Rope: They created the system as a coordination tool for talent retention.
 *    - Individual Powerful -> Tangled Rope: Experiences both the coordination benefits (wealth) and the extractive costs (immobility).
 *    - Individual Powerless -> Mountain: The long vesting schedule is an unchangeable reality of their employment, with no power to alter it.
 * 
 * 4. AMBIGUITIES:
 *    - The primary ambiguity is whether the employee is a willing participant or a victim. This is resolved by splitting the employee into "powerful" and "powerless" archetypes.
 * 
 * 5. CONFIDENCE:
 *    High: The classification logic for each perspective.
 *    Medium: The base_extractiveness score, as it's an abstraction.
 *    Low: None.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

/**
 * OMEGA IDENTIFICATION
 * 
 * The core uncertainty that cannot be resolved from the structure alone.
 */
 
omega_variable(
    market_volatility_impact,
    "If the stock price drops below the strike price ('underwater'), do the handcuffs functionally disappear?",
    resolution_mechanism("Analysis of employee turnover rates during market downturns for companies with heavy stock-based compensation."),
    impact("If the handcuffs break under market stress, the constraint is less of a Rope/Mountain and more of a fair-weather construct."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * Alternatives to long vesting cycles exist, and their suppression is key to the "handcuffs" dynamic.
 * 
 * ALTERNATIVE 1: Higher Base Salary, Lower Equity
 *    Viability: Highly viable. Many companies offer this trade-off.
 *    Suppression: Suppressed by venture-backed startup culture which normalizes equity-heavy compensation to conserve cash.
 * 
 * ALTERNATIVE 2: Performance-Based Vesting
 *    Viability: Viable, but harder to administer.
 *    Suppression: Suppressed due to the administrative overhead and potential for disputes over performance metrics. Time-based vesting is simpler to enforce.
 *
 * CONCLUSION:
 * The prevalence of time-based vesting (golden handcuffs) over these alternatives indicates a systemic preference for simplicity and long-term employee lock-in over more complex or cash-intensive compensation schemes. This reinforces its function as an intentional constraint on mobility.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/golden_handcuffs].
 * 2. Multi-perspective: ?- multi_index_report(golden_handcuffs).
 * 3. Run tests: ?- run_tests(golden_handcuffs_tests).
 */

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
% Mixed coordination/extraction — theater masks extraction component
domain_priors:theater_ratio(golden_handcuffs, 0.3).
narrative_ontology:constraint_metric(golden_handcuffs, theater_ratio, 0.3).

% --- Analytical perspective classification (missing) ---
% chi = 0.4 * 1.15 (analytical) * 1.2 (global) = 0.552
% Classification: rope
constraint_indexing:constraint_classification(golden_handcuffs, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).
