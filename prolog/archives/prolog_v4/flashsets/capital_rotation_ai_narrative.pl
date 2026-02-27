% ============================================================================
% CONSTRAINT STORY: capital_rotation_ai_narrative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_capital_rotation_ai_narrative, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: capital_rotation_ai_narrative
 *   human_readable: Market Narrative: US AI Stock Unsustainability
 *   domain: economic
 *
 * SUMMARY:
 *   A dominant market narrative positing the unsustainability of the US AI
 *   stock market rally. This narrative is driven by concerns about
 *   overvaluation, competitive pressures, and potential regulatory headwinds.
 *   It benefits incumbent tech giants and short sellers, while harming retail
 *   investors and emerging AI companies. The narrative is actively enforced
 *   through media coverage, analyst reports, and short selling activity. The
 *   theater ratio is low initially because the initial narrative is grounded
 *   in real analysis. Over time, the theater ratio grows as the narrative
 *   becomes amplified and distorted.
 *
 * KEY AGENTS:
 *   - Retail Investors: Primary target (powerless/trapped) - Susceptible to narrative-driven losses.
 *   - Emerging AI Companies: Secondary target (moderate/constrained) - Affected by negative sentiment impacting capital access.
 *   - Incumbent Tech Giants: Primary beneficiary (institutional/arbitrage) - Maintain market dominance by suppressing competitors.
 *   - Hedge Funds/Short Sellers: Beneficiary (powerful/mobile) - Profit from shorting overvalued stocks.
 *   - Analytical Observer: (analytical/analytical) - Provides unbiased market analysis.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(capital_rotation_ai_narrative, 0.55).
domain_priors:suppression_score(capital_rotation_ai_narrative, 0.7).
domain_priors:theater_ratio(capital_rotation_ai_narrative, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(capital_rotation_ai_narrative, extractiveness, 0.55).
narrative_ontology:constraint_metric(capital_rotation_ai_narrative, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(capital_rotation_ai_narrative, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(capital_rotation_ai_narrative, tangled_rope).
narrative_ontology:human_readable(capital_rotation_ai_narrative, "Market Narrative: US AI Stock Unsustainability").
narrative_ontology:topic_domain(capital_rotation_ai_narrative, "economic").

domain_priors:requires_active_enforcement(capital_rotation_ai_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(capital_rotation_ai_narrative, incumbent_tech_giants).
narrative_ontology:constraint_beneficiary(capital_rotation_ai_narrative, short_sellers).
narrative_ontology:constraint_victim(capital_rotation_ai_narrative, retail_investors).
narrative_ontology:constraint_victim(capital_rotation_ai_narrative, emerging_ai_companies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Retail investors are often trapped by the narrative due to limited access to information and analytical resources, leading to potential losses.
constraint_indexing:constraint_classification(capital_rotation_ai_narrative, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% Emerging AI companies are constrained by the narrative, as negative sentiment can impact their ability to raise capital and attract talent, but they also benefit from the increased focus and investment in the AI sector overall.
constraint_indexing:constraint_classification(capital_rotation_ai_narrative, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Incumbent tech giants benefit from the narrative as it may suppress the growth of emerging competitors, allowing them to maintain market dominance. They can arbitrage the situation by strategically investing in or acquiring promising AI startups at deflated valuations.
constraint_indexing:constraint_classification(capital_rotation_ai_narrative, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Hedge funds and short sellers benefit from the narrative by shorting overvalued AI stocks, but their actions can also amplify the negative sentiment, creating a self-fulfilling prophecy. They are mobile and can exit positions quickly.
constraint_indexing:constraint_classification(capital_rotation_ai_narrative, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% Analytical observers see the narrative as a mixed bag. While there are legitimate concerns about AI stock valuations, the narrative also serves to redistribute capital and influence market sentiment. This is the core analytical classification and claim.
constraint_indexing:constraint_classification(capital_rotation_ai_narrative, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(capital_rotation_ai_narrative_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(capital_rotation_ai_narrative, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(capital_rotation_ai_narrative, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(capital_rotation_ai_narrative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(capital_rotation_ai_narrative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The narrative extracts value from retail investors and emerging AI companies, transferring it to incumbent tech giants and short sellers. Suppression (0.70): High. The narrative is actively enforced through media coverage and short selling activity, making it difficult for dissenting voices to be heard. Theater ratio (0.30): Low. While the narrative is somewhat performative, it is also grounded in real economic concerns.
 *
 * PERSPECTIVAL GAP:
 *   Retail investors see the narrative as a snare, trapping them in losing positions. Emerging AI companies see a tangled rope, constrained by negative sentiment but also benefiting from increased overall interest in AI. Incumbent tech giants see a rope, using the narrative to maintain their dominance. Short sellers see a tangled rope, profiting from the narrative while amplifying it. The analytical observer sees a tangled rope, recognizing the mixed motives and consequences of the narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's structural position. Retail investors are powerless and trapped, leading to high extraction. Emerging AI companies are moderate and constrained, leading to moderate extraction. Incumbent tech giants are institutional and have arbitrage options, leading to low extraction. Short sellers are powerful and mobile, experiencing negative extraction. The analytical observer has an objective view of the situation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint addresses the mandatrophy by distinguishing between genuine market correction and pure extraction. While there are legitimate concerns about AI stock valuations, the narrative also serves to redistribute capital and influence market sentiment in a way that benefits certain actors at the expense of others. Distinguishing between these two aspects is crucial for understanding the true nature of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    valuation_accuracy,
    'To what extent do current AI stock valuations accurately reflect future earnings potential?',
    'Long-term analysis of AI company revenue growth and profitability, comparing actual performance against initial projections.',
    'If valuations are accurate: the narrative is primarily extraction from inexperienced investors. If valuations are inflated: the narrative has a basis in reality and acts as a market correction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(valuation_accuracy, empirical, 'Accuracy of AI stock valuations.').

omega_variable(
    incumbent_influence,
    'To what extent do incumbent tech giants influence the narrative to suppress emerging AI companies?',
    'Analysis of media coverage, lobbying efforts, and investment patterns to identify potential manipulation of the narrative.',
    'If influence is high: the narrative is largely an enforcement mechanism for maintaining market dominance. If influence is low: the narrative is driven by genuine market concerns.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incumbent_influence, empirical, 'Influence of incumbent tech giants on the narrative.').

omega_variable(
    market_manipulation_extent,
    'How much is the AI stock market narrative influenced by coordinated market manipulation?',
    'Regulatory investigations into coordinated short selling activity and disinformation campaigns.',
    'If manipulation is significant: the narrative is a snare. If manipulation is minimal, the narrative is rope or scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(market_manipulation_extent, empirical, 'Impact of market manipulation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(capital_rotation_ai_narrative, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(capi_tr_t0, capital_rotation_ai_narrative, theater_ratio, 0, 0.1).
narrative_ontology:measurement(capi_tr_t5, capital_rotation_ai_narrative, theater_ratio, 5, 0.2).
narrative_ontology:measurement(capi_tr_t10, capital_rotation_ai_narrative, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(capi_be_t0, capital_rotation_ai_narrative, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(capi_be_t5, capital_rotation_ai_narrative, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(capi_be_t10, capital_rotation_ai_narrative, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(capital_rotation_ai_narrative, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
