% ============================================================================
% CONSTRAINT STORY: lindy_effect
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Nassim Taleb / Benoit Mandelbrot / Intellectual History
% ============================================================================

:- module(constraint_lindy_effect, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: lindy_effect
 * human_readable: The Lindy Effect
 * domain: social/intellectual/technological
 * temporal_scope: Historical to Civilizational
 * spatial_scope: Global
 * * SUMMARY:
 * The Lindy Effect is a theorized phenomenon where the future life expectancy of 
 * non-perishable things (ideas, books, technologies) is proportional to their 
 * current age. In its simplest form, every additional day of survival implies 
 * a longer remaining life.
 * * KEY AGENTS:
 * - The Traditionalist: Uses time as a filter for quality and truth.
 * - The Innovator: Struggles against the "survivorship bias" of established ideas.
 * - The Statistician: Observes the power-law distribution of intellectual survival.
 * * NARRATIVE ARC:
 * The effect functions as a structural bias in information systems. It provides 
 * a "Rope" for identifying enduring value but acts as a "Mountain" for new 
 * entrants who lack the "temporal capital" to be taken seriously by the system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Reality Layer)
   ========================================================================== */

% Required for structural integration
narrative_ontology:interval(lindy_effect, 0, 10).
narrative_ontology:constraint_claim(lindy_effect, mountain).

% Base extractiveness: 0.2 (Low)
% Rationale: The effect doesn't actively steal value, but it does "extract" 
% attention from the new and gift it to the old based solely on age.
domain_priors:base_extractiveness(lindy_effect, 0.2).

% Suppression score: 0.5 (Moderate)
% Rationale: The "Time-Tested" narrative actively suppresses new ideas by 
% labeling them "fragile" or "unproven".
domain_priors:suppression_score(lindy_effect, 0.5).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(lindy_effect, extractiveness, 0.2).
narrative_ontology:constraint_metric(lindy_effect, suppression_requirement, 0.5).

% Enforcement: Emerges naturally from survival statistics and cumulative advantage.
domain_priors:emerges_naturally(lindy_effect).

% Beneficiaries: Classics, established religions, and long-standing technologies.
narrative_ontology:constraint_beneficiary(lindy_effect, established_institutions).

% Victims: Novel ideas, startups, and new artistic movements.
narrative_ontology:constraint_victim(lindy_effect, disruptive_innovators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE ANALYTICAL STATISTICIAN - MOUNTAIN
   --------------------------------------------------------------------------
   
   WHO: analytical (The observer of systemic patterns).
   WHEN: civilizational (Viewing the long-term horizon of ideas).
   WHERE: trapped (The mathematics of power laws are invariant).
   SCOPE: global.
   
   WHY THIS CLASSIFICATION:
   For the statistician, the Lindy Effect is a Mountain. It is an unchangeable 
   feature of non-perishable systems. In a world of information, "ageing" 
   works in reverse; this is a mathematical reality that cannot be legislated 
   away or ignored.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    lindy_effect,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE PRAGMATIC INVESTOR - ROPE
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (A decision-maker seeking stability).
   WHEN: biographical (Achieving success within a single lifetime).
   WHERE: mobile (Can choose to follow Lindy or gamble on the new).
   SCOPE: national.
   
   WHY THIS CLASSIFICATION:
   For the investor, the Lindy Effect is a Rope. It is a coordination 
   mechanism that allows them to distinguish "signal" from "noise." By 
   prioritizing things that have already survived for 50 years, they use 
   the constraint as a tether to reality and enduring value.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    lindy_effect,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE DISRUPTIVE STARTUP - SNARE
   --------------------------------------------------------------------------
   
   WHO: powerless (A new entrant with zero temporal capital).
   WHEN: immediate (Needs to survive the next 12 months).
   WHERE: constrained (Must compete against "proven" incumbents).
   SCOPE: local.
   
   WHY THIS CLASSIFICATION:
   For a new innovator, the Lindy Effect is a Snare. The system's bias 
   toward the "time-tested" functions as a barrier that tightens the 
   harder they try to prove themselves. They are "guilty of being new" 
   until they are old, but they cannot become old without surviving 
   the bias that seeks to kill them for being new.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    lindy_effect,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 4: THE ESTABLISHED INSTITUTION - ROPE
   --------------------------------------------------------------------------
   WHO: institutional (e.g., a university, a religious body).
   WHEN: Generational (Focused on preserving its own existence).
   WHERE: Arbitrage (Leverages its age and reputation).
   SCOPE: National to Global.
   
   WHY THIS CLASSIFICATION:
   For an established institution, the Lindy Effect is a "Rope." It is a 
   powerful mechanism for maintaining authority and relevance. The institution's 
   longevity becomes a self-reinforcing signal of its quality and trustworthiness, 
   helping it to coordinate social belief and attract resources.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    lindy_effect,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(arbitrage),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(lindy_effect_tests).

/**
 * TEST 1: Multi-perspective variance
 * Demonstrates that the Lindy Effect is viewed differently across agents.
 */
test(multi_perspective_variance) :-
    % Analytical Statistician (Mountain)
    constraint_indexing:constraint_classification(
        lindy_effect,
        Type1,
        context(agent_power(analytical), time_horizon(civilizational), exit_options(trapped), spatial_scope(global))
    ),
    % Pragmatic Investor (Rope)
    constraint_indexing:constraint_classification(
        lindy_effect,
        Type2,
        context(agent_power(individual_moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(national))
    ),
    % Disruptive Startup (Snare)
    constraint_indexing:constraint_classification(
        lindy_effect,
        Type3,
        context(agent_power(powerless), time_horizon(immediate), exit_options(constrained), spatial_scope(local))
    ),
    % Established Institution (Rope)
    constraint_indexing:constraint_classification(
        lindy_effect,
        Type4,
        context(agent_power(institutional), time_horizon(generational), exit_options(arbitrage), spatial_scope(global))
    ),
    % Verify they differ
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3. % Ensure all three are distinct

/**
 * TEST 2: Power-based extractiveness scaling
 * Demonstrates that powerless startups experience higher extraction (of opportunity) than established institutions.
 */
test(power_extractiveness_scaling) :-
    ContextPowerless = context(agent_power(powerless), time_horizon(immediate), exit_options(constrained), spatial_scope(local)),
    ContextPowerful = context(agent_power(institutional), time_horizon(generational), exit_options(arbitrage), spatial_scope(global)),
    constraint_indexing:extractiveness_for_agent(lindy_effect, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(lindy_effect, ContextPowerful, Score2),
    Score1 > Score2.  % Powerless experience more extraction

/**
 * TEST 3: Domain-specific insight - The "Time Tax"
 * Demonstrates that the "snare" for innovators is the time they must survive to be considered "Lindy."
 */
test(time_tax_insight) :-
    constraint_indexing:constraint_classification(lindy_effect, ClassificationStartup, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lindy_effect, ClassificationInvestor, context(agent_power(individual_moderate), _, _, _)),
    ClassificationStartup = snare,
    ClassificationInvestor = rope.

:- end_tests(lindy_effect_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini 2.0 Flash
 * Date: 2026-01-23
 * 
 * KEY DECISIONS:
 * 
 * 1. PERSPECTIVAL VARIANCE: I chose to highlight how "Age" acts as a filter. 
 * What is a "validating tool" (Rope) for a scholar is a "death sentence" (Snare) 
 * for a new startup that cannot survive long enough to reach the "Lindy" zone.
 * 
 * 2. EXTRACTIVENESS: Kept at 0.2. The Lindy Effect doesn't "steal" as much 
 * as it "weights." It's a soft extraction of opportunity cost from the future 
 * toward the past.
 * 
 * 3. SNARE ARGUMENT: The "Snare" for innovators is specifically the 
 * impossibility of acquiring age without first surviving the disadvantage 
 * of being new.
 * 
 * 4. OMEGAS 
 *    Define uncertainty so your analysis is cleaner
 *    omega_variable(
 *        digital_acceleration_impact,
 *        "Does the speed of digital iteration (e.g., AI) break the Lindy 
 *        Effect or simply create a 'Compressed Lindy' cycle?",
 *        resolution_mechanism("Longitudinal study of software survival vs. 
 *        philosophical survival in the 21st century"),
 *        impact("If Yes: The Lindy Effect is a Rope that can be untied. If No: It 
 *        is a Mountain that just scales with speed."),
 *        confidence_without_resolution(medium)
 *    ).
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS (If Applicable)
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: Meritocratic "Neophilia" (Silicon Valley model)
 *    Viability: Moderate. Seeks to judge things solely on current utility.
 *    Suppression: Moderate. Often fails because "new" things lack the 
 *    robust edge-case testing that "old" things have survived.
 * 
 * ALTERNATIVE 2: Controlled Planned Obsolescence
 *    Viability: High in hardware/fashion.
 * 
 * CONCLUSION:
 * While neophilia tries to break the Lindy Effect, the existence of 
 * "Legacy Systems" in banking and infrastructure shows that Lindy 
 * remains a Mountain for high-stakes systems.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * 
 * 1. Load into main system:
 *    ?- [constraints/lindy_effect].
 * 
 * 2. Run multi-perspective analysis:
 *    ?- constraint_indexing:multi_index_report(lindy_effect).
 * 
 * 3. Run tests:
 *    ?- run_tests(lindy_effect_tests).
 * 
 * 4. Generate pedagogical report:
 *    ?- pedagogical_report(lindy_effect).
 * 
 * 5. Compare with other constraints:
 *    ?- compare_constraints(lindy_effect, [other_id]).
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
% Structural constraint in social domain — low theater, high substance
domain_priors:theater_ratio(lindy_effect, 0.11).
narrative_ontology:constraint_metric(lindy_effect, theater_ratio, 0.11).
