% ============================================================================
% CONSTRAINT STORY: art_market_decoupling
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-04-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_art_market_decoupling, []).

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
 *   constraint_id: art_market_decoupling
 *   human_readable: The Obscene Decoupling
 *   domain: economic/social
 *
 * SUMMARY:
 *   The 'Obscene Decoupling,' as identified by David Bowie, describes a
 *   three-way separation between the artist, the elitist 'art world,' and the
 *   'obscene' art market. This decoupling results in the commodification of
 *   art, where its value is primarily determined by its investment potential
 *   rather than its cultural significance or the artist's intent.
 *
 * KEY AGENTS:
 *   - Artists: Primary victim (powerless/trapped) - Limited control over the commercialization of their work.
 *   - Art Market Investors: Primary beneficiary (institutional/arbitrage) - Treat art as an asset class for profit.
 *   - Art World Elite: Secondary beneficiary (moderate/constrained) - Benefit from inflated values but constrained by maintaining the illusion of cultural value.
 *   - Society: Secondary victim (powerless/trapped) - Loses potential cultural enrichment.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(art_market_decoupling, 0.6).
domain_priors:suppression_score(art_market_decoupling, 0.7).
domain_priors:theater_ratio(art_market_decoupling, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(art_market_decoupling, extractiveness, 0.6).
narrative_ontology:constraint_metric(art_market_decoupling, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(art_market_decoupling, theater_ratio, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(art_market_decoupling, tangled_rope).
narrative_ontology:human_readable(art_market_decoupling, "The Obscene Decoupling").
narrative_ontology:topic_domain(art_market_decoupling, "economic/social").

domain_priors:requires_active_enforcement(art_market_decoupling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(art_market_decoupling, art_investors).
narrative_ontology:constraint_beneficiary(art_market_decoupling, art_world_elite).
narrative_ontology:constraint_victim(art_market_decoupling, artists).
narrative_ontology:constraint_victim(art_market_decoupling, society).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Artists are often trapped in a system where their work is commodified and its value is determined by the market, with little control over the process.
constraint_indexing:constraint_classification(art_market_decoupling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Investors benefit from the decoupling by treating art as an asset class, arbitraging the market for profit.
constraint_indexing:constraint_classification(art_market_decoupling, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The 'art world' benefits from the inflated value of art, but are somewhat constrained by the need to maintain the illusion of cultural value.
constraint_indexing:constraint_classification(art_market_decoupling, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Society loses the potential cultural enrichment when art becomes primarily an investment vehicle.
constraint_indexing:constraint_classification(art_market_decoupling, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% The analytical observer sees the tangled rope of a system where art's cultural significance is warped by market forces.
constraint_indexing:constraint_classification(art_market_decoupling, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(art_market_decoupling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(art_market_decoupling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(art_market_decoupling, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(art_market_decoupling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(art_market_decoupling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): The art market extracts value from artists and society by commodifying art and prioritizing investment over cultural significance. Suppression (0.7): Artists have limited control over how their work is valued and sold. Theater Ratio (0.8): High proportion of performative activity, with auctions and exhibitions often serving as displays of wealth and status rather than genuine appreciation of art.
 *
 * PERSPECTIVAL GAP:
 *   Artists experience the system as a snare, lacking control over their work's commodification. Investors experience it as a rope, a reliable investment vehicle. The art world elite see it as a tangled rope, benefiting from inflated prices but constrained by the need to maintain the illusion of artistic and cultural importance. The analytical observer sees the mixed nature of the art world and the market.
 *
 * DIRECTIONALITY LOGIC:
 *   Art market investors benefit, leading to a lower directionality. Artists and society are harmed, leading to a higher directionality. The art world elite are beneficiaries, but are somewhat constrained.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_vs_investment_value,
    'To what extent is the value of art driven by genuine cultural significance versus speculative investment?',
    'Analysis of art market trends, artist interviews, and critical reviews.',
    'If investment value dominates, the decoupling is more severe. If cultural value is primary, the decoupling is less pronounced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_vs_investment_value, empirical, 'The relative importance of cultural versus investment value.').

omega_variable(
    artist_agency,
    'How much agency do artists have in controlling the commodification of their work?',
    'Interviews with artists, analysis of artist contracts, and legal research.',
    'Greater artist agency would lessen the snare on artists, but the art market decoupling would still affect society.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(artist_agency, empirical, 'The agency artists have to control the commercialization of their work.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(art_market_decoupling, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art__tr_t0, art_market_decoupling, theater_ratio, 0, 0.6).
narrative_ontology:measurement(art__tr_t5, art_market_decoupling, theater_ratio, 5, 0.7).
narrative_ontology:measurement(art__tr_t10, art_market_decoupling, theater_ratio, 10, 0.8).

% Extraction over time
narrative_ontology:measurement(art__be_t0, art_market_decoupling, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(art__be_t5, art_market_decoupling, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(art__be_t10, art_market_decoupling, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(art_market_decoupling, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
