% ============================================================================
% CONSTRAINT STORY: harlequin_historical_line_discontinuation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_harlequin_historical_line_discontinuation, []).

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
 *   constraint_id: harlequin_historical_line_discontinuation
 *   human_readable: Harlequin's Discontinuation of its Dedicated Historical Romance Imprint
 *   domain: economic
 *
 * SUMMARY:
 *   In 2025, major romance publisher Harlequin (owned by HarperCollins)
 *   announced the discontinuation of its dedicated Harlequin Historical
 *   imprint. This decision reallocates resources towards other romance
 *   subgenres deemed more profitable. While benefiting HarperCollins and
 *   potentially contemporary romance authors, the discontinuation negatively
 *   impacts historical romance authors and readers who relied on this
 *   imprint. It represents a strategic shift in resource allocation within
 *   the romance publishing industry.
 *
 * KEY AGENTS:
 *   - HarperCollins: Primary beneficiary (institutional/arbitrage) – benefits from streamlined operations and resource reallocation.
 *   - Historical Romance Authors: Primary victim (powerless/trapped) – lose a major outlet for their work and face limited alternative options.
 *   - Historical Romance Readers: Primary victim (powerless/trapped) – lose access to a specific type of historical romance novel.
 *   - Contemporary Romance Authors: Secondary beneficiary (moderate/mobile) – may benefit from increased resources allocated to their subgenre.
 *   - Analytical Observer: Sees the overall shifts in the market (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(harlequin_historical_line_discontinuation, 0.55).
domain_priors:suppression_score(harlequin_historical_line_discontinuation, 0.7).
domain_priors:theater_ratio(harlequin_historical_line_discontinuation, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(harlequin_historical_line_discontinuation, extractiveness, 0.55).
narrative_ontology:constraint_metric(harlequin_historical_line_discontinuation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(harlequin_historical_line_discontinuation, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(harlequin_historical_line_discontinuation, tangled_rope).
narrative_ontology:human_readable(harlequin_historical_line_discontinuation, "Harlequin's Discontinuation of its Dedicated Historical Romance Imprint").
narrative_ontology:topic_domain(harlequin_historical_line_discontinuation, "economic").

domain_priors:requires_active_enforcement(harlequin_historical_line_discontinuation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(harlequin_historical_line_discontinuation, harpercollins).
narrative_ontology:constraint_beneficiary(harlequin_historical_line_discontinuation, contemporary_romance_authors).
narrative_ontology:constraint_victim(harlequin_historical_line_discontinuation, historical_romance_authors).
narrative_ontology:constraint_victim(harlequin_historical_line_discontinuation, historical_romance_readers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Historical romance authors who relied on Harlequin Historicals as a primary outlet experience the discontinuation as a snare. Their specialized skill set is less transferable to other genres, and the reduced market access suppresses their career options.
constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Readers of historical romance who specifically enjoyed the type of stories published by Harlequin Historicals experience the discontinuation as a snare. Their preferred niche is being suppressed. They have limited exit options, as the style is specific.
constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% HarperCollins, the parent company, benefits from streamlining operations and focusing on more profitable romance subgenres. They see the discontinuation as a rope, facilitating resource allocation. They have arbitrage exit options as they can publish any genre.
constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Contemporary romance authors benefit as resources are shifted towards their subgenre. The action is a tangled rope because they also face the risk of resources being moved again. Exit options are good as they can write and publish.
constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% Analytical perspective sees a tangled rope. Resources have been moved to a different area, but it is also the loss of diversity within the publishing company. The action requires active enforcement to ensure resources aren't shifted back.
constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(harlequin_historical_line_discontinuation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(harlequin_historical_line_discontinuation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(harlequin_historical_line_discontinuation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(harlequin_historical_line_discontinuation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. Historical romance authors and readers experience a significant loss due to reduced market access and content availability. HarperCollins benefits from increased profits through resource reallocation. Suppression (0.70): High. The discontinuation actively reduces the availability of historical romance content in a specific style, suppressing options for both authors and readers. Other resources have become harder to find. Theater Ratio (0.30): Low. The action is primarily a business decision and less focused on external presentation.
 *
 * PERSPECTIVAL GAP:
 *   The key perspectival gap lies between HarperCollins, which views the discontinuation as a beneficial resource allocation (Rope), and historical romance authors and readers, who experience it as a loss of opportunity and content (Snare). Contemporary romance authors have mixed perspectives (Tangled Rope), as they might benefit, but still depend on resources from HarperCollins. Analytical sees a mixed resource allocation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural positions of the agents. HarperCollins benefits directly from the resource reallocation (low d), while historical romance authors and readers bear the costs (high d). Contemporary romance authors experience a more neutral impact (moderate d).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination by distinguishing the benefits to HarperCollins from the costs to historical romance authors and readers. While HarperCollins may frame the discontinuation as a necessary business decision, the structural analysis reveals the extractive impact on a specific segment of the romance community.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_demand_assessment,
    'Was the discontinuation driven by a genuine decline in market demand for historical romance, or by a strategic decision to prioritize other genres regardless of inherent demand?',
    'Independent sales data analysis across multiple publishers; consumer surveys on genre preferences; comparison of marketing budgets allocated to different romance subgenres.',
    'If demand decline: decision is resource allocation (Rope from more perspectives). If strategic shift: decision is extraction (Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_demand_assessment, empirical, 'Whether the discontinuation was driven by a genuine decline in market demand').

omega_variable(
    author_adaptation_capacity,
    'How easily can historical romance authors adapt their skills to other romance subgenres or writing fields?',
    'Tracking career transitions of former Harlequin Historical authors; skills assessment and training programs; market analysis of demand for different writing styles.',
    'If high adaptation: author experience is constrained (Tangled Rope). If low adaptation: author experience is trapped (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(author_adaptation_capacity, empirical, 'How easily can historical romance authors adapt').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(harlequin_historical_line_discontinuation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(harl_tr_t0, harlequin_historical_line_discontinuation, theater_ratio, 0, 0.2).
narrative_ontology:measurement(harl_tr_t5, harlequin_historical_line_discontinuation, theater_ratio, 5, 0.25).
narrative_ontology:measurement(harl_tr_t10, harlequin_historical_line_discontinuation, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(harl_be_t0, harlequin_historical_line_discontinuation, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(harl_be_t5, harlequin_historical_line_discontinuation, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(harl_be_t10, harlequin_historical_line_discontinuation, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(harlequin_historical_line_discontinuation, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
