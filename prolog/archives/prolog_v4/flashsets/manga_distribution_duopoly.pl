% ============================================================================
% CONSTRAINT STORY: manga_distribution_duopoly
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manga_distribution_duopoly, []).

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
 *   constraint_id: manga_distribution_duopoly
 *   human_readable: Manga Distribution Duopoly in North America
 *   domain: economic
 *
 * SUMMARY:
 *   The North American manga distribution market is effectively a duopoly,
 *   dominated by Viz Media and Yen Press. This duopoly impacts independent
 *   manga artists, smaller publishers, and consumers. While providing a
 *   distribution channel, it also creates extractive pressures and limits
 *   market access.
 *
 * KEY AGENTS:
 *   - Viz Media: Beneficiary (institutional/arbitrage) - Benefits from market control and distribution infrastructure.
 *   - Yen Press: Beneficiary (institutional/arbitrage) - Benefits from market control and distribution infrastructure.
 *   - Independent Manga Artists: Victim (powerless/trapped) - Limited distribution options and market access.
 *   - Smaller Manga Publishers: Victim (moderate/constrained) - Constrained by market dominance but benefit from some access.
 *   - Manga Consumers: Victim (moderate/mobile) - Limited choices and potentially higher prices due to the lack of competition. Some mobility through purchasing directly from Japan or using digital distribution methods.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manga_distribution_duopoly, 0.55).
domain_priors:suppression_score(manga_distribution_duopoly, 0.65).
domain_priors:theater_ratio(manga_distribution_duopoly, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manga_distribution_duopoly, extractiveness, 0.55).
narrative_ontology:constraint_metric(manga_distribution_duopoly, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(manga_distribution_duopoly, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manga_distribution_duopoly, tangled_rope).
narrative_ontology:human_readable(manga_distribution_duopoly, "Manga Distribution Duopoly in North America").
narrative_ontology:topic_domain(manga_distribution_duopoly, "economic").

domain_priors:requires_active_enforcement(manga_distribution_duopoly).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manga_distribution_duopoly, viz_media).
narrative_ontology:constraint_beneficiary(manga_distribution_duopoly, yen_press).
narrative_ontology:constraint_victim(manga_distribution_duopoly, independent_manga_artists).
narrative_ontology:constraint_victim(manga_distribution_duopoly, smaller_manga_publishers).
narrative_ontology:constraint_victim(manga_distribution_duopoly, manga_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Independent manga artists are trapped due to limited distribution options. The duopoly extracts by controlling access to the market.
constraint_indexing:constraint_classification(manga_distribution_duopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Smaller publishers are constrained by the duopoly's market dominance. They benefit from the existing distribution infrastructure but are also extracted from through unfavorable terms.
constraint_indexing:constraint_classification(manga_distribution_duopoly, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Viz Media and Yen Press benefit from the duopoly, coordinating distribution and maintaining market control. They have arbitrage exit options due to their established infrastructure.
constraint_indexing:constraint_classification(manga_distribution_duopoly, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% The analytical observer sees a tangled rope: the duopoly provides distribution infrastructure (coordination) but also extracts from artists and smaller publishers through market control (asymmetric extraction).
constraint_indexing:constraint_classification(manga_distribution_duopoly, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manga_distribution_duopoly_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(manga_distribution_duopoly, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(manga_distribution_duopoly, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(manga_distribution_duopoly, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(manga_distribution_duopoly_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The duopoly extracts from artists and smaller publishers through unfavorable terms and limited market access. Suppression (0.65): Moderate-high. Significant barriers exist for new entrants and independent artists to bypass the established distribution channels. Theater ratio (0.30): Low. While marketing and promotion play a role, the primary function is actual distribution.
 *
 * PERSPECTIVAL GAP:
 *   Independent manga artists see a snare due to their limited distribution options. Smaller publishers see a tangled rope because they have some access but are still constrained. Viz Media and Yen Press see a rope, coordinating distribution. The analytical observer sees the overall tangled rope structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Viz Media and Yen Press) have low directionality due to their arbitrage exit options. Victims (independent artists and smaller publishers) have higher directionality due to their limited exit options. The directionality affects the chi calculation, leading to different classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   This analysis distinguishes between a pure extraction snare and a mixed coordination/extraction tangled rope. The duopoly provides a distribution infrastructure (coordination), but also extracts from other parties through market control. The differing perspectives highlight the complex nature of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_entry_feasibility,
    'How feasible is it for new manga distributors to enter the North American market and effectively compete with the duopoly?',
    'Analysis of market entry barriers, including capital requirements, licensing agreements, and existing distribution networks.',
    'If market entry is easy, the constraint is weaker and leans towards a Rope. If market entry is difficult, the constraint is stronger and leans towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_entry_feasibility, empirical, 'Feasibility of new market entrants.').

omega_variable(
    digital_distribution_impact,
    'To what extent does digital distribution bypass the duopoly''s control over physical distribution channels?',
    'Comparative analysis of manga sales through digital platforms versus physical retail channels.',
    'If digital distribution significantly reduces reliance on physical channels, the constraint weakens and leans towards a Scaffold. If digital distribution is still largely controlled by the duopoly, the constraint remains strong.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_distribution_impact, empirical, 'Impact of digital distribution on the duopoly''s control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manga_distribution_duopoly, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mang_tr_t0, manga_distribution_duopoly, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mang_tr_t5, manga_distribution_duopoly, theater_ratio, 5, 0.25).
narrative_ontology:measurement(mang_tr_t10, manga_distribution_duopoly, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(mang_be_t0, manga_distribution_duopoly, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(mang_be_t5, manga_distribution_duopoly, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(mang_be_t10, manga_distribution_duopoly, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manga_distribution_duopoly, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
