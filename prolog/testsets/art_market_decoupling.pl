% ============================================================================
% CONSTRAINT STORY: art_market_decoupling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
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
    narrative_ontology:affects_constraint/2,
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
 *   This constraint models the 'Obscene Decoupling' in the contemporary art
 *   market, a concept articulated by David Bowie. It describes a system where
 *   three distinct domains have separated: the artist's creative act, the
 *   elitist 'art world' of social networking and status, and the 'obscene'
 *   art market where art functions as a financial instrument for the
 *   ultra-wealthy. The system coordinates value and prestige but does so by
 *   extracting enormous value from the original producers (artists) and
 *   excluding the general public, channeling benefits to a small class of
 *   collectors and intermediaries.
 *
 * KEY AGENTS:
 *   - Artists: Primary victims (powerless/trapped) — provide the underlying asset but are largely excluded from the immense financial upside.
 *   - High-Net-Worth Collectors: Primary beneficiaries (institutional/arbitrage) — use art as an investment vehicle, status symbol, and tax shelter.
 *   - Galleries & Auction Houses: Intermediary beneficiaries/enforcers (organized/mobile) — act as gatekeepers, control narratives, and take significant commissions.
 *   - Museums: Institutional actors (institutional/constrained) — their cultural mission is often subordinated to the market's financial logic.
 *   - General Public: Secondary victims (powerless/trapped) — experience culture as an increasingly inaccessible, financialized commodity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(art_market_decoupling, 0.68).
domain_priors:suppression_score(art_market_decoupling, 0.75).
domain_priors:theater_ratio(art_market_decoupling, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(art_market_decoupling, extractiveness, 0.68).
narrative_ontology:constraint_metric(art_market_decoupling, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(art_market_decoupling, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(art_market_decoupling, tangled_rope).
narrative_ontology:human_readable(art_market_decoupling, "The Obscene Decoupling").
narrative_ontology:topic_domain(art_market_decoupling, "economic/social").

domain_priors:requires_active_enforcement(art_market_decoupling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(art_market_decoupling, high_net_worth_collectors).
narrative_ontology:constraint_beneficiary(art_market_decoupling, galleries_and_auction_houses).
narrative_ontology:constraint_victim(art_market_decoupling, artists).
narrative_ontology:constraint_victim(art_market_decoupling, general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ARTIST (SNARE) — Trapped within a system that extracts the majority of long-term financial value from their work. Lacks the leverage to negotiate terms or access the secondary market. The system appears purely coercive. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈1.16.
constraint_indexing:constraint_classification(art_market_decoupling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE COLLECTOR (ROPE) — Experiences the market as a pure coordination mechanism for allocating capital, signaling status, and securing tax advantages. Benefits from price appreciation and market liquidity. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.10 (net beneficiary).
constraint_indexing:constraint_classification(art_market_decoupling, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE GALLERY (TANGLED ROPE) — A primary enforcer and beneficiary who sees both sides. Provides a genuine coordination service (career-building, promotion) while engaging in high extraction (50% commissions, controlling access). d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.44.
constraint_indexing:constraint_classification(art_market_decoupling, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: THE MUSEUM (PITON) — Constrained by the market's influence on cultural valuation. The function of curating based on historical or aesthetic merit is degraded, replaced by the performative act of ratifying market-validated artists. The theater_ratio of 0.65 is near the piton gate.
constraint_indexing:constraint_classification(art_market_decoupling, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE ANALYST (TANGLED ROPE) — The canonical view. Recognizes the genuine coordination function (price discovery, prestige allocation) is inextricably linked with a highly extractive, asymmetric structure that benefits capital over labor. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.94.
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

test(piton_threshold) :-
    domain_priors:theater_ratio(art_market_decoupling, TR),
    TR >= 0.70.

:- end_tests(art_market_decoupling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high, reflecting gallery commissions (typically 50%), auction house premiums, and the complete exclusion of artists from secondary market profits. Suppression (0.75) is high due to the powerful gatekeeping function of the gallery system, which makes it nearly impossible for artists to build a high-value career independently. Theater Ratio (0.65) is significant; art fairs, gallery openings, and curated 'artist narratives' are highly performative rituals that serve to justify prices and maintain the system's exclusivity.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the collector-investor, the market is a well-oiled coordination machine (Rope) for capital. For the emerging artist, it is an extractive trap (Snare) they cannot escape. For the intermediary gallery, it is a complex system of genuine career-building and raw extraction (Tangled Rope). This perspectival divergence is the core of the 'decoupling'—the participants are operating in functionally different realities defined by their structural relationship to the flow of capital and prestige.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from clear structural roles. Artists are declared victims with trapped exit options, maximizing their `d` value and leading to a Snare classification. Collectors are beneficiaries with arbitrage exit, minimizing their `d` value to produce a Rope. Galleries, as both beneficiaries and enforcers with mobile exit, occupy a middle ground that correctly resolves to a Tangled Rope. The system automatically computes these divergent experiences from the same set of base properties.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic case for resolving mandatrophy. A defense of the art market would frame it as a necessary coordination mechanism (a Rope) to solve the problem of pricing unique goods. This narrative completely obscures the coercive extraction experienced by artists. By classifying it as a Tangled Rope from an analytical view, and a Snare from the artist's view, the framework correctly identifies that the coordination function is real but has been co-opted for asymmetric extraction. It prevents the 'coordination' claim from masking the 'extraction' reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    value_creation_vs_rent_seeking,
    'Is the market''s value-add (liquidity, promotion, price discovery) a genuine service proportional to its take, or is it primarily extractive rent-seeking?',
    'Comparative analysis of artist net income vs. total market value generated by their work over a career; analysis of price formation mechanisms.',
    'If primarily service-based, the system is closer to a Rope. If primarily rent-seeking, it is a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(value_creation_vs_rent_seeking, conceptual, 'Distinguishing between the market''s service function and its extractive function.').

omega_variable(
    alternative_system_viability,
    'Can alternative platforms (e.g., NFTs, direct-to-collector models) achieve the prestige and pricing power to offer a viable exit from the established gallery system?',
    'Longitudinal tracking of artist careers and market values for those who exclusively use alternative systems versus traditional ones.',
    'If alternatives are viable, suppression is lower and the artist''s exit option shifts from ''trapped'' to ''constrained'', changing their classification. If not, the high suppression score is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_system_viability, empirical, 'Viability of exit options from the traditional art market.').

omega_variable(
    source_of_obscene_valuation,
    'Is the ''obscene'' valuation a product of artistic genius, market manipulation, or its function as a tax-advantaged asset for the ultra-wealthy?',
    'Correlation analysis between artwork prices, critical acclaim, and macroeconomic indicators of wealth inequality and tax policy.',
    'If primarily an asset class function, the ''art'' aspect is theater, and the constraint is a component of a larger financial Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(source_of_obscene_valuation, empirical, 'Determining the primary driver of high-end art market prices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(art_market_decoupling, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art__tr_t0, art_market_decoupling, theater_ratio, 0, 0.3).
narrative_ontology:measurement(art__tr_t22, art_market_decoupling, theater_ratio, 22, 0.5).
narrative_ontology:measurement(art__tr_t44, art_market_decoupling, theater_ratio, 44, 0.65).

% Extraction over time
narrative_ontology:measurement(art__be_t0, art_market_decoupling, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(art__be_t22, art_market_decoupling, base_extractiveness, 22, 0.55).
narrative_ontology:measurement(art__be_t44, art_market_decoupling, base_extractiveness, 44, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(art_market_decoupling, resource_allocation).
narrative_ontology:affects_constraint(art_market_decoupling, ultra_wealthy_tax_avoidance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
