% ============================================================================
% CONSTRAINT STORY: fungal_shift_grassland_loss
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fungal_shift_grassland_loss, []).

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
 *   constraint_id: fungal_shift_grassland_loss
 *   human_readable: Ecological Transition from Grassland to Scrub via Fungal Network Disruption
 *   domain: ecological
 *
 * SUMMARY:
 *   Warming winters reduce stable snowpack in mountain ecosystems, altering
 *   the soil environment. This leads to a disruption of existing fungal
 *   networks that support grassland species. The disrupted networks benefit
 *   opportunistic fungi and scrub species, leading to a shift from grassland
 *   to scrubland.
 *
 * KEY AGENTS:
 *   - Grassland Ecosystem: Primary target (powerless/trapped) — loses habitat and biodiversity.
 *   - Scrub Species: Primary beneficiary (institutional/arbitrage) — gains habitat and expands range.
 *   - Specialized Fungi: Secondary target (powerless/trapped) - loss of host plant.
 *   - Opportunistic Fungi: Secondary beneficiary (institutional/arbitrage) - new environment to colonize
 *   - Ecological Modelers: Analytical observer.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fungal_shift_grassland_loss, 0.55).
domain_priors:suppression_score(fungal_shift_grassland_loss, 0.7).
domain_priors:theater_ratio(fungal_shift_grassland_loss, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fungal_shift_grassland_loss, extractiveness, 0.55).
narrative_ontology:constraint_metric(fungal_shift_grassland_loss, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(fungal_shift_grassland_loss, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fungal_shift_grassland_loss, tangled_rope).
narrative_ontology:human_readable(fungal_shift_grassland_loss, "Ecological Transition from Grassland to Scrub via Fungal Network Disruption").
narrative_ontology:topic_domain(fungal_shift_grassland_loss, "ecological").

domain_priors:requires_active_enforcement(fungal_shift_grassland_loss).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fungal_shift_grassland_loss, scrub_species).
narrative_ontology:constraint_beneficiary(fungal_shift_grassland_loss, opportunistic_fungi).
narrative_ontology:constraint_victim(fungal_shift_grassland_loss, grassland_species).
narrative_ontology:constraint_victim(fungal_shift_grassland_loss, specialized_fungi).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The grassland ecosystem is trapped and unable to adapt quickly enough to the changing conditions. They bear the cost of the transition.
constraint_indexing:constraint_classification(fungal_shift_grassland_loss, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% Scrub species benefit from the altered environment and expand their range. They see it as coordination of resources in their favor.
constraint_indexing:constraint_classification(fungal_shift_grassland_loss, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% Ecological modelers observe the transition and see the tangled web of interactions, including both coordination and extraction.
constraint_indexing:constraint_classification(fungal_shift_grassland_loss, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Specialized fungi that rely on specific grassland plants are trapped and unable to adapt, leading to their decline.
constraint_indexing:constraint_classification(fungal_shift_grassland_loss, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fungal_shift_grassland_loss_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fungal_shift_grassland_loss, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fungal_shift_grassland_loss, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fungal_shift_grassland_loss, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fungal_shift_grassland_loss_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The disruption of fungal networks represents a significant extraction of resources and support from the grassland ecosystem. The increased suppression reflects the reduced ability of grassland to compete with the encroaching scrub species.
 *
 * PERSPECTIVAL GAP:
 *   Grassland ecosystems view the change as a snare due to their inability to adapt. Scrub species see the shift as a coordination of resources in their favor. Ecological modelers see the full picture as a tangled rope of ecological interactions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries like scrub species gain resources and expand their range. Victims like grassland species lose habitat and biodiversity. Fungi fall into both categories, some losing (specialized) while others benefit (opportunistic). The directionality values reflect these relationships.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    climate_variability,
    'How much will climate variability impact snowpack stability?',
    'Climate models and historical data analysis',
    'High variability could accelerate the transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_variability, empirical, 'The extent to which climate variability impacts snowpack stability.').

omega_variable(
    fungal_network_resilience,
    'How resilient are the existing fungal networks to disturbance?',
    'Ecological experiments and network analysis',
    'High resilience could slow or prevent the transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fungal_network_resilience, empirical, 'The resilience of the existing fungal networks to disturbance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fungal_shift_grassland_loss, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fung_tr_t0, fungal_shift_grassland_loss, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fung_tr_t25, fungal_shift_grassland_loss, theater_ratio, 25, 0.15).
narrative_ontology:measurement(fung_tr_t50, fungal_shift_grassland_loss, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(fung_be_t0, fungal_shift_grassland_loss, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(fung_be_t25, fungal_shift_grassland_loss, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(fung_be_t50, fungal_shift_grassland_loss, base_extractiveness, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fungal_shift_grassland_loss, resource_allocation).
narrative_ontology:affects_constraint(fungal_shift_grassland_loss, climate_change_impacts).
narrative_ontology:affects_constraint(fungal_shift_grassland_loss, biodiversity_loss).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
