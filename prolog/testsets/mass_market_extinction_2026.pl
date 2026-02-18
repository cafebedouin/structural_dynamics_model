% ============================================================================
% CONSTRAINT STORY: mass_market_extinction_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_mass_market_extinction_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: mass_market_extinction_2026
 * human_readable: The Mass Market Paperback Sunset
 * domain: economic/cultural
 * * SUMMARY:
 * This constraint tracks the final collapse of the mass-market paperback format
 * following ReaderLink's decision to cease distribution at the end of 2025.
 * Once the "most popular reading format" that democratized American literacy
 * through low price points ($9.99 cap) and non-bookstore accessibility, the
 * format has plummeted from 131 million units in 2004 to 15 million in 2025.
 * * KEY AGENTS:
 * - Low-Income Readers: Subject (Powerless) - Losing the most accessible
 * entry point to print literacy.
 * - ReaderLink/Publishers: Beneficiary (Institutional) - Managing the exit
 * from a low-margin, consolidating distribution market.
 * - Industry Veterans: Auditor (Analytical) - Documenting the shift from
 * "democratization" to "extinction".
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.70) due to rising production costs clashing with a rigid
% $9.99 consumer price cap, making the format economically unsustainable.
domain_priors:base_extractiveness(mass_market_extinction_2026, 0.70).

% High suppression (0.82); the removal of the format suppresses the choice of
% low-cost physical alternatives in non-bookstore outlets.
domain_priors:suppression_score(mass_market_extinction_2026, 0.82).

% Moderate theater (0.45); the $9.99 price point served as a theatrical anchor
% that publishers were "reluctant" to break, leading to format death instead of repricing.
domain_priors:theater_ratio(mass_market_extinction_2026, 0.45).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(mass_market_extinction_2026, extractiveness, 0.70).
narrative_ontology:constraint_metric(mass_market_extinction_2026, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(mass_market_extinction_2026, theater_ratio, 0.45).

% The system's collapse is presented as a constructed market outcome ("the consumer has spoken").
narrative_ontology:constraint_claim(mass_market_extinction_2026, tangled_rope).
narrative_ontology:human_readable(mass_market_extinction_2026, "The Mass Market Paperback Sunset").
narrative_ontology:topic_domain(mass_market_extinction_2026, "economic/cultural").

% The rigid distribution and pricing contracts required active enforcement to maintain.
domain_priors:requires_active_enforcement(mass_market_extinction_2026).

% Structural property derivation hooks for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(mass_market_extinction_2026, publishers_and_distributors).
narrative_ontology:constraint_victim(mass_market_extinction_2026, low_income_readers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the budget-conscious reader, the extinction is a snare: they are "trapped"
% into choosing more expensive trade paperbacks, digital formats, or forgoing books.
constraint_indexing:constraint_classification(mass_market_extinction_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For ReaderLink/Publishers, the exit is a rope: coordinating the final withdrawal
% to maintain logistical efficiency and exit an unprofitable market segment.
constraint_indexing:constraint_classification(mass_market_extinction_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% An analyst sees a Tangled Rope: a system that once had a genuine coordination
% function (democratizing literacy) but became highly extractive and required
% active enforcement of its pricing/distribution model, ultimately harming one group
% while benefiting another's exit strategy.
constraint_indexing:constraint_classification(mass_market_extinction_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mass_market_extinction_2026_tests).

test(perspectival_gap) :-
    % Subject feels the loss of access (Snare), Institution coordinates the exit (Rope).
    constraint_indexing:constraint_classification(mass_market_extinction_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mass_market_extinction_2026, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(mass_market_extinction_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    domain_priors:base_extractiveness(mass_market_extinction_2026, E),
    E > 0.46. % Triggers temporal data requirement.

test(tangled_rope_structural_properties) :-
    % Verify all three conditions for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(mass_market_extinction_2026, _),
    narrative_ontology:constraint_victim(mass_market_extinction_2026, _),
    domain_priors:requires_active_enforcement(mass_market_extinction_2026).

:- end_tests(mass_market_extinction_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extraction score (0.70) reflects the "impossible to sustain" production
 * costs against a fixed consumer price expectation ($9.99). Suppression (0.82) is
 * high because the format's disappearance removes the only low-cost, widely
 * available physical book option from non-bookstore locations like grocery stores.
 *
 * The Perspectival Gap is stark: publishers see a coordinated, rational exit from
 * a failed market (Rope), while low-income readers experience the loss of an
 * essential good (Snare).
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The original classification of Piton was incorrect because the constraint is
 * not inert; it is highly extractive (E=0.70). The Tangled Rope classification
 * correctly resolves this by acknowledging the dual nature of the constraint: it
 * possessed a genuine, decades-long coordination function (distributing cheap books)
 * that has become entangled with severe asymmetric extraction (the economic unsustainability
 * is offloaded onto readers who lose access). This prevents mislabeling the system
 * as either pure extraction (Snare) or pure inertia (Piton).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_physical_literacy_floor,
    'Does the death of mass market create a permanent floor for physical book ownership, making it a luxury good?',
    'Tracking unit sales of Trade Paperbacks vs. E-books in former ID territories over 5 years.',
    'If trade paperback sales do not absorb the displaced readers, it implies print literacy is becoming a class-gated Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mass_market_extinction_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Model the decline from 2004 (T=0) to the final sunset in 2025/2026 (T=10).

% Theater ratio: Increases as the format becomes a legacy "ode" rather
% than a functional market driver, with publishers performatively lamenting its demise.
narrative_ontology:measurement(mm_tr_t0, mass_market_extinction_2026, theater_ratio, 0, 0.15).
narrative_ontology:measurement(mm_tr_t5, mass_market_extinction_2026, theater_ratio, 5, 0.30).
narrative_ontology:measurement(mm_tr_t10, mass_market_extinction_2026, theater_ratio, 10, 0.45).

% Extraction: Increases as unit sales plunge (131M to 15M) and fixed costs
% per unit skyrocket, making margins "impossible to sustain".
narrative_ontology:measurement(mm_ex_t0, mass_market_extinction_2026, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(mm_ex_t5, mass_market_extinction_2026, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(mm_ex_t10, mass_market_extinction_2026, base_extractiveness, 10, 0.70).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The mass-market paperback system was a mechanism for allocating a specific
% cultural resource (low-cost literature) to a wide audience.
narrative_ontology:coordination_type(mass_market_extinction_2026, resource_allocation).

% The collapse of this physical media format directly influences the market
% pressures and pricing strategies for its digital alternative.
narrative_ontology:affects_constraint(mass_market_extinction_2026, ebook_pricing_models).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */