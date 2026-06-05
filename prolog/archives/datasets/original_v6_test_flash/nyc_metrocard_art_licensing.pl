% ============================================================================
% CONSTRAINT STORY: nyc_metrocard_art_licensing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nyc_metrocard_art_licensing, []).

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
 *   constraint_id: nyc_metrocard_art_licensing
 *   human_readable: NYC MetroCard Art Licensing Agreement
 *   domain: economic
 *
 * SUMMARY:
 *   The licensing agreement between the MTA and artists for using their
 *   artwork on MetroCards allows the MTA to profit from the artwork's
 *   popularity while potentially limiting the artists' control and
 *   compensation. This arrangement can be viewed differently depending on the
 *   perspective. The MTA benefits significantly, while the artists may
 *   experience it as a mixed bag of exposure and limited returns. An
 *   analytical view suggests a tangled rope scenario.
 *
 * KEY AGENTS:
 *   - Participating Artists: Primary target (powerless/trapped) - Provides artwork, potentially receives less than market value
 *   - Metropolitan Transportation Authority: Primary beneficiary (institutional/arbitrage) - Profits from the artwork, has many artists to choose from
 *   - Analytical Observer: Evaluates the agreement (analytical/analytical) - Assesses the structure of extraction and coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nyc_metrocard_art_licensing, 0.55).
domain_priors:suppression_score(nyc_metrocard_art_licensing, 0.4).
domain_priors:theater_ratio(nyc_metrocard_art_licensing, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nyc_metrocard_art_licensing, extractiveness, 0.55).
narrative_ontology:constraint_metric(nyc_metrocard_art_licensing, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(nyc_metrocard_art_licensing, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nyc_metrocard_art_licensing, tangled_rope).
narrative_ontology:human_readable(nyc_metrocard_art_licensing, "NYC MetroCard Art Licensing Agreement").
narrative_ontology:topic_domain(nyc_metrocard_art_licensing, "economic").

domain_priors:requires_active_enforcement(nyc_metrocard_art_licensing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nyc_metrocard_art_licensing, metropolitan_transportation_authority).
narrative_ontology:constraint_victim(nyc_metrocard_art_licensing, participating_artists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Artists may feel trapped if the agreement terms are unfavorable or if they lack bargaining power. They may experience extraction if the compensation does not align with the artwork's popularity and resulting profits for the MTA.
constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% The MTA benefits from the agreement by leveraging artists' work to increase MetroCard sales and brand image. They have arbitrage options as they can choose from many artists willing to participate.
constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% Objectively, the agreement functions as both a coordination mechanism (providing artists with exposure and the MTA with content) and an extraction mechanism (MTA profits, artists may be limited). The overall structure creates a Tangled Rope scenario.
constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nyc_metrocard_art_licensing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nyc_metrocard_art_licensing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nyc_metrocard_art_licensing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: Moderate. The MTA benefits significantly from increased MetroCard sales and brand image due to the artists' work. The artists, while gaining exposure, might not be adequately compensated, leading to extraction. Suppression: Moderate. Artists may feel limited by the contract terms and lack the power to negotiate better compensation or retain full control over their artwork's usage. The theater ratio is low as the main function is economic exchange, not performative activity.
 *
 * PERSPECTIVAL GAP:
 *   Artists may perceive the agreement as a snare if they feel exploited or undervalued. The MTA sees it as a beneficial partnership (Rope). The analytical observer recognizes the combination of coordination and extraction (Tangled Rope) inherent in the agreement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural positions. The MTA (institutional/arbitrage) benefits with low directionality. Artists (powerless/trapped) face higher directionality. The Analytical Observer is neutral.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    artist_bargaining_power,
    'How much bargaining power do individual artists have when negotiating with the MTA?',
    'Surveying artists who have participated in the program and assessing the variability in contract terms.',
    'If artists have little power, the constraint shifts towards a Snare. If artists have substantial power, it shifts towards a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(artist_bargaining_power, empirical, 'The level of influence artists have on the licensing agreement terms.').

omega_variable(
    market_value_determination,
    'How accurately does the licensing agreement compensate artists relative to the commercial value generated by their artwork on the MetroCard?',
    'Analyzing sales data of MetroCards featuring different artists and comparing the compensation structures.',
    'If compensation is significantly below market value, the constraint shifts towards a Snare. If compensation aligns with market value, it moves toward a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_value_determination, empirical, 'The fairness of compensation offered in the licensing agreement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nyc_metrocard_art_licensing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nyc__tr_t0, nyc_metrocard_art_licensing, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nyc__tr_t5, nyc_metrocard_art_licensing, theater_ratio, 5, 0.3).
narrative_ontology:measurement(nyc__tr_t10, nyc_metrocard_art_licensing, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(nyc__be_t0, nyc_metrocard_art_licensing, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(nyc__be_t5, nyc_metrocard_art_licensing, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(nyc__be_t10, nyc_metrocard_art_licensing, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nyc_metrocard_art_licensing, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
