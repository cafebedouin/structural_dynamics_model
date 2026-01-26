% ============================================================================ 
% CONSTRAINT STORY: dead_sea_effect
% ============================================================================ 
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Software Engineering Management / Bruce Webster / Organizational Theory
% ============================================================================ 

:- module(constraint_dead_sea_effect, []).

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
 * constraint_id: dead_sea_effect
 * human_readable: The Dead Sea Effect (Talent Evaporation)
 * domain: social/economic/organizational
 * temporal_scope: Biographical to Generational
 * spatial_scope: Corporate / Institutional
 * 
 * SUMMARY:
 * The Dead Sea Effect occurs in large organizations when highly talented and
 * mobile individuals "evaporate" (leave) because they have the best exit
 * options, while the less talented/less mobile individuals stay behind,
 * increasing the "salinity" (mediocrity) of the remaining talent pool.
 * 
 * KEY AGENTS:
 * - The High-Performer (Individual Powerful): The "fresh water" who leaves as the environment decays.
 * - The Mediocre Remainer (Individual Powerless): Lacks comparable exit options and stays.
 * - Oblivious Management (Institutional): Fails to differentiate treatment or address underlying issues.
 */

/* ========================================================================== 
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(dead_sea_effect, 0, 10).
narrative_ontology:constraint_claim(dead_sea_effect, snare).

% Base extractiveness: Moderate (0.5).
% The organization extracts the presence/stability of the mediocre at the cost
% of the high-performer's potential, leading to a net loss of system value.
domain_priors:base_extractiveness(dead_sea_effect, 0.5).

% Suppression: Moderate (0.4).
% Talent "evaporation" is often invisible to management (who see low turnover
% in the residue) until the system fails, suppressing warnings of decay.
domain_priors:suppression_score(dead_sea_effect, 0.4).

% Enforcement: Emerges naturally from poor institutional design and incentive structures.
domain_priors:emerges_naturally(dead_sea_effect).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(dead_sea_effect, competitor_organizations).
constraint_victim(dead_sea_effect, the_stagnant_institution).

/* ========================================================================== 
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* -------------------------------------------------------------------------- 
   PERSPECTIVE 1: THE HIGH-PERFORMER - Rope
   -------------------------------------------------------------------------- 
   WHO: individual_powerful (High human capital, many opportunities)
   WHEN: immediate (Recognizes declining environment quickly)
   WHERE: mobile (High exit options)
   
   WHY THIS CLASSIFICATION:
   For the high-performer, the organization's decay is a 'Rope'. Because
   they are mobile, the declining environment is simply the signal they
   need to climb out to a better "Match." The constraint doesn't bind
   them; it motivates their exit and directs them to superior opportunities.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dead_sea_effect,
    rope,
    context(
        agent_power(individual_powerful),
        time_horizon(immediate),
        exit_options(mobile),
        spatial_scope(global)
    )
).

/* -------------------------------------------------------------------------- 
   PERSPECTIVE 2: THE MEDIOCRE REMAINER - Snare
   -------------------------------------------------------------------------- 
   WHO: individual_powerless (Low exit options, limited mobility)
   WHEN: biographical (Stuck in a declining career path)
   WHERE: trapped (Cannot leave, or believes they cannot leave)
   
   WHY THIS CLASSIFICATION:
   For those who cannot leave, the increasing "salinity" is a 'Snare'.
   As the high-performers leave, the workload and institutional decay
   accelerate. Those remaining are trapped in a failing system with
   no path to improvement, slowly suffocated by growing inefficiency.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dead_sea_effect,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* -------------------------------------------------------------------------- 
   PERSPECTIVE 3: OBLIVIOUS MANAGEMENT - Mountain
   -------------------------------------------------------------------------- 
   WHO: institutional (Inertia in a large organization)
   WHEN: historical (Cycles of organizational decline)
   WHERE: analytical (Observes high retention, misinterprets as loyalty)
   
   WHY THIS CLASSIFICATION:
   To management oblivious to the effect, the Dead Sea Effect is a 'Mountain'.
   They see high retention rates (of mediocre employees) and interpret it
   as organizational stability, viewing attrition as a natural, unchangeable
   process of "weeding out" those who don't fit. They are blind to the decay.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dead_sea_effect,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(national)
    )
).

/* ========================================================================== 
   4. TESTS
   ========================================================================== */

:- begin_tests(dead_sea_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(dead_sea_effect, Type1, context(agent_power(individual_powerful), _, _, _)),
    constraint_indexing:constraint_classification(dead_sea_effect, Type2, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(dead_sea_effect, Type3, context(agent_power(institutional), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(dead_sea_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added 'Oblivious Management' as the institutional
 *    agent. Their misinterpretation of the situation (high retention = good)
 *    makes the effect an immutable 'Mountain' of organizational decay to them.
 * 
 * 2. CLASSIFICATION RATIONALE:
 *    - High-Performer (Rope): Uses mobility as an exit strategy.
 *    - Mediocre Remainer (Snare): Trapped by lack of options and decaying environment.
 *    - Management (Mountain): Blind to the underlying systemic failure.
 * 
 * 3. CORE INSIGHT: The Dead Sea Effect is a perverse filtering mechanism. What
 *    starts as a problem for the individual becomes a systemic 'Snare' for the
 *    organization itself, often unrecognized until it's too late.
 */

/* ========================================================================== 
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is whether the effect is a natural attrition or preventable.
 */

omega_variable(
    remote_work_impact,
    'Does remote work exacerbate the \'evaporation\' rate by lowering exit costs (making it easier for high-performers to switch jobs without relocating)?',
    resolution_mechanism('Comparative analysis of turnover rates in remote vs. in-office stagnant firms across industries. தயவுசெய்து இந்த வரியை புறக்கணிக்கவும்.'),
    impact('If Yes: The \'Mountain\' of organizational decay becomes a \'Flash Flood\' (rapid collapse). If No: Remote work is neutral to the effect.'),
    confidence_without_resolution(medium)
).

/* ========================================================================== 
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Active Talent Management (Retention Strategies)
 *    Viability: Highly viable and widely practiced in healthy organizations.
 *    Suppression: Suppressed by management's inability or unwillingness to differentiate
 *    treatment based on performance or mobility.
 *
 * CONCLUSION:
 * The Dead Sea Effect is not an inevitable 'Mountain', but a constructed 'Snare'
 * that results from the suppression of active talent management strategies.
 * It is a choice, not a law of nature.
 */

/* ========================================================================== 
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/dead_sea_effect].
 * 2. Multi-perspective: ?- multi_index_report(dead_sea_effect).
 * 3. Run tests: ?- run_tests(dead_sea_tests).
 */

/* ========================================================================== 
   END OF CONSTRAINT STORY
   ========================================================================== */