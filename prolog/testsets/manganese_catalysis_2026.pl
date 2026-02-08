% ============================================================================
% CONSTRAINT STORY: manganese_catalysis_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-05
% ============================================================================

:- module(constraint_manganese_catalysis_2026, []).

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
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: manganese_catalysis_2026
 * human_readable: Manganese-Formate Fuel Pathway
 * domain: technological/economic
 * * SUMMARY:
 * Researchers have identified manganese as a high-efficiency catalyst for 
 * converting CO2 into formate, a potential hydrogen carrier for fuel cells. 
 * This removes the "Mountain" of precious-metal scarcity (Platinum/Iridium) 
 * and establishes a more accessible "Rope" for the global hydrogen economy.
 * * KEY AGENTS:
 * - Industrial Energy Consumers: Subject (Powerless)
 * - CCU Tech Developers: Beneficiary (Institutional)
 * - Environmental Auditors: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is low (0.18) because the shift to abundant metals reduces 
% the rent-seeking potential of rare-earth monopolies.
domain_priors:base_extractiveness(manganese_catalysis_2026, 0.18). 

% Suppression is low (0.25) as the technology expands degrees of freedom 
% for energy production rather than restricting them.
domain_priors:suppression_score(manganese_catalysis_2026, 0.25).   

% Theater ratio is low (0.08) as the breakthrough provides a 
% functional chemical pathway with high industrial utility.
domain_priors:theater_ratio(manganese_catalysis_2026, 0.08).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(manganese_catalysis_2026, extractiveness, 0.18).
narrative_ontology:constraint_metric(manganese_catalysis_2026, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(manganese_catalysis_2026, theater_ratio, 0.08).

% Constraint classification claim
narrative_ontology:constraint_claim(manganese_catalysis_2026, scaffold).

% Primary keys for the classification engine
% Beneficial and victim actors (E < 0.46 typically means broader coordination)
narrative_ontology:constraint_beneficiary(manganese_catalysis_2026, clean_energy_infrastructure).
narrative_ontology:constraint_victim(manganese_catalysis_2026, precious_metal_cartels).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PREVIOUS MARKET (MOUNTAIN)
% From the old perspective of platinum-dependent fuel cells, the 
% cost of hydrogen was an immutable Mountain (Fixed Limit).
constraint_indexing:constraint_classification(manganese_catalysis_2026, mountain, 
    context(agent_power(powerless), 
            time_horizon(historical), 
            exit_options(trapped), 
            spatial_scope(global))).

% PERSPECTIVE 2: THE NEW ECONOMY (ROPE)
% For CCU developers, the manganese catalyst is a Rope: essential 
% infrastructure for coordinating affordable carbon utilization.
constraint_indexing:constraint_classification(manganese_catalysis_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SCAFFOLD)
% Analysts view this as a Scaffold: a transitional coordination 
% mechanism to bridge the gap toward a fully carbon-neutral fuel economy.
constraint_indexing:constraint_classification(manganese_catalysis_2026, scaffold,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))) :-
    narrative_ontology:has_sunset_clause(manganese_catalysis_2026).

% Mandatory sunset clause for transitional Scaffold
narrative_ontology:has_sunset_clause(manganese_catalysis_2026).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manganese_catalysis_2026_tests).

test(low_extraction_signal) :-
    domain_priors:base_extractiveness(manganese_catalysis_2026, E),
    E < 0.20.

test(theater_check) :-
    domain_priors:theater_ratio(manganese_catalysis_2026, TR),
    TR < 0.10.

test(type_variance) :-
    constraint_indexing:constraint_classification(manganese_catalysis_2026, mountain, _),
    constraint_indexing:constraint_classification(manganese_catalysis_2026, rope, _).

:- end_tests(manganese_catalysis_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.18) is low because manganese is abundant, 
 * decentralizing the power of the hydrogen economy away from rare 
 * metal holders. The low Theater Ratio (0.08) reflects that this is a 
 * functional chemical discovery with immediate industrial relevance 
 * for lowering CCU costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_manganese_scaling,
    'Can manganese catalysis maintain efficiency at industrial giga-scales?',
    'Validation of 24-month continuous flow pilot plant data.',
    'Success secures the Rope; Failure returns the industry to the Mountain of scarcity.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manganese_catalysis_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio remains low as the catalyst proves its functional worth.
narrative_ontology:measurement(mn_tr_t0, manganese_catalysis_2026, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mn_tr_t5, manganese_catalysis_2026, theater_ratio, 5, 0.06).
narrative_ontology:measurement(mn_tr_t10, manganese_catalysis_2026, theater_ratio, 10, 0.08).

% Extraction drops initially as monopolies are broken, then stabilizes.
narrative_ontology:measurement(mn_ex_t0, manganese_catalysis_2026, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(mn_ex_t5, manganese_catalysis_2026, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(mn_ex_t10, manganese_catalysis_2026, base_extractiveness, 10, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
