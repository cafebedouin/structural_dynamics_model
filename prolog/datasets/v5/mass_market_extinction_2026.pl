% ============================================================================
% CONSTRAINT STORY: mass_market_extinction_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-05
% ============================================================================

:- module(constraint_mass_market_extinction, []).

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
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    constraint_indexing:constraint_classification/3.

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
 * - [Low-Income Readers]: Subject (Powerless) - Losing the most accessible 
 * entry point to print literacy.
 * - [ReaderLink/Publishers]: Beneficiary (Institutional) - Managing the exit 
 * from a low-margin, consolidating distribution market.
 * - [Industry Veterans]: Auditor (Analytical) - Documenting the shift from 
 * "democratization" to "extinction".
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.70) due to rising production costs clashing with a rigid 
% $9.99 consumer price cap.
domain_priors:base_extractiveness(mass_market_extinction_2026, 0.70). 

% High suppression (0.82); the removal of the format suppresses the choice of 
% low-cost physical alternatives in non-bookstore outlets.
domain_priors:suppression_score(mass_market_extinction_2026, 0.82).   

% Moderate theater (0.45); the $9.99 price point serves as a theatrical anchor 
% that publishers were "reluctant" to break, leading to format death.
domain_priors:theater_ratio(mass_market_extinction_2026, 0.45).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(mass_market_extinction_2026, extractiveness, 0.7).
narrative_ontology:constraint_metric(mass_market_extinction_2026, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(mass_market_extinction_2026, theater_ratio, 0.45).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the budget-conscious reader, the extinction is a snare: they are "trapped" 
% into choosing more expensive trade paperbacks or hardcovers.
constraint_indexing:constraint_classification(mass_market_extinction_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For ReaderLink/Publishers, the exit is a rope: coordinating the final withdrawal 
% to maintain logistical efficiency in a "puzzling" market decline.
constraint_indexing:constraint_classification(mass_market_extinction_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Industry veterans see a piton: an inertial format that "democratized America" 
% but can no longer sustain its own structural weight.
constraint_indexing:constraint_classification(mass_market_extinction_2026, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(national))) :-
    domain_priors:theater_ratio(mass_market_extinction_2026, TR), TR > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mass_market_tests).

test(perspectival_gap) :-
    % Subject feels the loss of access (Snare), Institution coordinates the exit (Rope).
    constraint_indexing:constraint_classification(mass_market_extinction_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mass_market_extinction_2026, rope, context(agent_power(institutional), _, _, _)).

test(extraction_threshold) :-
    domain_priors:base_extractiveness(mass_market_extinction_2026, E),

    E > 0.46. % Triggers temporal data requirement.

:- end_tests(mass_market_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.70) reflects the "impossible to sustain" production 
 * costs against a fixed consumer expectation ($9.99).
 * The Perspectival Gap exists because while publishers see the "consumer has spoken," 
 * the subject loses their primary "accessible via low prices" entry point.
 *
 * MANDATROPHY ANALYSIS:
 * The Piton classification acknowledges that while the format was functional 
 * for decades, the decay of the Independent Distributor (ID) network turned 
 * it into an inertial relic by 2025.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_physical_literacy_floor,
    'Does the death of mass market create a permanent floor for physical book ownership?',
    'Tracking unit sales of Trade Paperbacks vs. E-books in former ID territories.',
    'If trade doesn't pick up the slack, print literacy becomes a luxury Snare.',
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
% than a functional market driver.
narrative_ontology:measurement(mm_tr_t0, mass_market_extinction_2026, theater_ratio, 0, 0.15).
narrative_ontology:measurement(mm_tr_t5, mass_market_extinction_2026, theater_ratio, 5, 0.30).
narrative_ontology:measurement(mm_tr_t10, mass_market_extinction_2026, theater_ratio, 10, 0.45).

% Extraction: Increases as unit sales plunge (131M to 15M) and margins 
% become "impossible to sustain".
narrative_ontology:measurement(mm_ex_t0, mass_market_extinction_2026, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(mm_ex_t5, mass_market_extinction_2026, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(mm_ex_t10, mass_market_extinction_2026, base_extractiveness, 10, 0.70).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
