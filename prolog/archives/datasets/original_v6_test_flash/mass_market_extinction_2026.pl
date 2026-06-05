% ============================================================================
% CONSTRAINT STORY: mass_market_extinction_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mass_market_extinction_2026, []).

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
 *   constraint_id: mass_market_extinction_2026
 *   human_readable: The Mass Market Paperback Sunset
 *   domain: economic/cultural
 *
 * SUMMARY:
 *   This constraint tracks the final collapse of the mass-market paperback
 *   format following ReaderLink's decision to cease distribution at the end
 *   of 2025. This event disproportionately affects lower-income readers and
 *   independent bookstores, while benefiting large online retailers and
 *   publishers focused on higher-priced formats. The shift creates a form of
 *   cultural and economic extraction.
 *
 * KEY AGENTS:
 *   - Mass Market Readers: Primary victim (powerless/trapped) - loses access to affordable reading.
 *   - Independent Bookstores: Secondary victim (moderate/constrained) - loses a key revenue stream.
 *   - Amazon Kindle Store: Primary beneficiary (institutional/arbitrage) - gains ebook market share.
 *   - Prestige Paperback Publishers: Secondary beneficiary (institutional/arbitrage) - benefits from less direct competition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mass_market_extinction_2026, 0.6).
domain_priors:suppression_score(mass_market_extinction_2026, 0.7).
domain_priors:theater_ratio(mass_market_extinction_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mass_market_extinction_2026, extractiveness, 0.6).
narrative_ontology:constraint_metric(mass_market_extinction_2026, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(mass_market_extinction_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mass_market_extinction_2026, snare).
narrative_ontology:human_readable(mass_market_extinction_2026, "The Mass Market Paperback Sunset").
narrative_ontology:topic_domain(mass_market_extinction_2026, "economic/cultural").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mass_market_extinction_2026, amazon_kindle_store).
narrative_ontology:constraint_beneficiary(mass_market_extinction_2026, prestige_paperback_publishers).
narrative_ontology:constraint_victim(mass_market_extinction_2026, mass_market_readers).
narrative_ontology:constraint_victim(mass_market_extinction_2026, independent_bookstores).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The mass market reader, particularly those in lower income brackets or with limited access to online retail, finds their primary source of affordable reading material disappearing. Trapped by economic constraints and limited access.
constraint_indexing:constraint_classification(mass_market_extinction_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% Independent bookstores, already struggling, lose a key product category that drove foot traffic and impulse buys. They are constrained by existing leases and inventory, but benefit from increased demand in specialized areas.
constraint_indexing:constraint_classification(mass_market_extinction_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% The Kindle store and ebook market generally benefits as readers migrate to digital formats. They have full arbitrage capability.
constraint_indexing:constraint_classification(mass_market_extinction_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Publishers who focus on prestige paperbacks (trade paperbacks) benefit from the decline of the mass market paperback because there is less direct competition at the bookstore and more price flexibility for new releases.
constraint_indexing:constraint_classification(mass_market_extinction_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% From an analytical perspective, the end of mass market paperbacks represents a complex interplay of market forces, technological change, and shifting cultural preferences, a tangled rope involving benefits and extraction.
constraint_indexing:constraint_classification(mass_market_extinction_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mass_market_extinction_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mass_market_extinction_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mass_market_extinction_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mass_market_extinction_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mass_market_extinction_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): The constraint extracts access to affordable literature, particularly from low-income readers. Suppression (0.7): The dwindling number of outlets for mass market paperbacks and the promotion of more expensive alternatives creates a coercive environment with few alternatives for specific demographics. Theater Ratio (0.3): Low. Little performative coordination.
 *
 * PERSPECTIVAL GAP:
 *   The mass market reader experiences a loss of access (snare), while Amazon sees an opportunity for increased ebook sales (rope). Independent bookstores experience the decline as a loss to overall sales but still retain benefits from specialized books and trade paperbacks (tangled rope). Prestige paperback publishers can gain greater revenue as there are less low-cost options. The analytical observer sees all of these as a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the degree to which an agent benefits or suffers due to the sunsetting of mass market paperbacks. Readers have little ability to influence this process and are thus victims, while Kindle and prestige paperback publishers benefit economically.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    digital_divide_impact,
    'How severely will the digital divide limit access to reading for low-income communities?',
    'Tracking ebook adoption rates by income level; assessing library ebook lending programs.',
    'If severe: Increased inequality in access to culture. If minimal: Transition to digital reading is relatively smooth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_divide_impact, empirical, 'The impact of the digital divide on access to reading').

omega_variable(
    prestige_paperback_price_point,
    'Will prestige paperbacks remain affordable, or will their prices rise, effectively excluding a segment of readers?',
    'Tracking the average price of prestige paperbacks relative to inflation and minimum wage.',
    'If affordable: Less impact on readers overall. If prices rise: Further marginalization of low-income readers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prestige_paperback_price_point, empirical, 'The future price point of prestige paperbacks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mass_market_extinction_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mass_tr_t0, mass_market_extinction_2026, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mass_tr_t5, mass_market_extinction_2026, theater_ratio, 5, 0.3).
narrative_ontology:measurement(mass_tr_t10, mass_market_extinction_2026, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(mass_be_t0, mass_market_extinction_2026, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(mass_be_t5, mass_market_extinction_2026, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(mass_be_t10, mass_market_extinction_2026, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mass_market_extinction_2026, resource_allocation).
narrative_ontology:affects_constraint(mass_market_extinction_2026, ebook_market_dominance).
narrative_ontology:affects_constraint(mass_market_extinction_2026, literacy_access_inequality).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
