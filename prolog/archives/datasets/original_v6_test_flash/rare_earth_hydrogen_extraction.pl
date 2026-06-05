% ============================================================================
% CONSTRAINT STORY: rare_earth_hydrogen_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rare_earth_hydrogen_extraction, []).

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
 *   constraint_id: rare_earth_hydrogen_extraction
 *   human_readable: Rare Earth Element Dependency for Core Hydrogen Extraction
 *   domain: economic, technological, geopolitical
 *
 * SUMMARY:
 *   Accessing hydrogen from Earth's core, as theorized by some research,
 *   requires advanced extraction technologies heavily reliant on rare earth
 *   elements (REEs). This dependency creates a complex web of economic,
 *   technological, and geopolitical constraints. While potentially unlocking
 *   a new energy source, it also establishes significant vulnerabilities and
 *   dependencies.
 *
 * KEY AGENTS:
 *   - Rare Earth Suppliers: Primary beneficiary (institutional/arbitrage)
 *   - Hydrogen Extraction Technology Developers: Secondary beneficiary (powerful/constrained)
 *   - Nations Dependent on Hydrogen Imports: Primary victim (powerless/trapped)
 *   - Downstream Hydrogen Consumers: Secondary victim (moderate/mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rare_earth_hydrogen_extraction, 0.6).
domain_priors:suppression_score(rare_earth_hydrogen_extraction, 0.7).
domain_priors:theater_ratio(rare_earth_hydrogen_extraction, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rare_earth_hydrogen_extraction, extractiveness, 0.6).
narrative_ontology:constraint_metric(rare_earth_hydrogen_extraction, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(rare_earth_hydrogen_extraction, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rare_earth_hydrogen_extraction, tangled_rope).
narrative_ontology:human_readable(rare_earth_hydrogen_extraction, "Rare Earth Element Dependency for Core Hydrogen Extraction").
narrative_ontology:topic_domain(rare_earth_hydrogen_extraction, "economic, technological, geopolitical").

domain_priors:requires_active_enforcement(rare_earth_hydrogen_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rare_earth_hydrogen_extraction, rare_earth_suppliers).
narrative_ontology:constraint_beneficiary(rare_earth_hydrogen_extraction, hydrogen_extraction_technology_developers).
narrative_ontology:constraint_victim(rare_earth_hydrogen_extraction, nations_dependent_on_hydrogen_imports).
narrative_ontology:constraint_victim(rare_earth_hydrogen_extraction, downstream_hydrogen_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Nations heavily reliant on hydrogen imports face limited exit options and bear the brunt of increased costs and geopolitical dependencies.
constraint_indexing:constraint_classification(rare_earth_hydrogen_extraction, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Rare earth suppliers experience the constraint as a coordination mechanism that secures their market position and incentivizes further resource exploration and development.
constraint_indexing:constraint_classification(rare_earth_hydrogen_extraction, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Dependency on rare earth elements for core hydrogen extraction presents a mixed bag. It facilitates the development of new energy sources but simultaneously creates new dependencies, geopolitical leverage, and potential resource scarcity issues.
constraint_indexing:constraint_classification(rare_earth_hydrogen_extraction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Companies involved in developing hydrogen extraction technologies benefit from the increased demand, but are also constrained by the availability and cost of rare earth elements. They benefit, but also bear a cost.
constraint_indexing:constraint_classification(rare_earth_hydrogen_extraction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rare_earth_hydrogen_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rare_earth_hydrogen_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rare_earth_hydrogen_extraction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rare_earth_hydrogen_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rare_earth_hydrogen_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is relatively high because the constraint creates dependencies and geopolitical leverage. The suppression is also high due to limited alternatives and the concentration of REE resources in a few countries. The theater ratio is relatively low because the core issue revolves around real material dependencies.
 *
 * PERSPECTIVAL GAP:
 *   Nations dependent on hydrogen imports see this as a snare because their energy security is at the mercy of REE supply. Rare earth suppliers see this as a rope because it creates demand and secures their market. Technology developers experience a tangled rope because they benefit from the new technologies, but are constrained by REE availability and cost. 
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the structural positions. Beneficiaries with arbitrage options (REE suppliers) have low d-values and see the constraint as coordination. Victims with limited exit options (hydrogen-import-dependent nations) have high d-values and perceive it as a snare. Technology developers are constrained and powerful, therefore they experience extraction to a lesser degree, but benefit as well.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ree_supply_availability,
    'Will the supply of rare earth elements be sufficient to meet the demand for core hydrogen extraction technologies?',
    'Geological surveys, economic modeling, and geopolitical analysis',
    'If supply is limited, the constraint becomes a tighter snare, exacerbating dependencies and geopolitical tensions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ree_supply_availability, empirical, 'Availability and concentration of rare earth elements.').

omega_variable(
    technology_substitution,
    'Can alternative materials or technologies reduce the dependence on rare earth elements for core hydrogen extraction?',
    'Materials science research, engineering development, and technology forecasting',
    'If substitution is possible, the constraint may weaken over time, potentially shifting to a scaffold or rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technology_substitution, empirical, 'Possibility of technology substitution in hydrogen extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rare_earth_hydrogen_extraction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rare_tr_t0, rare_earth_hydrogen_extraction, theater_ratio, 0, 0.1).
narrative_ontology:measurement(rare_tr_t5, rare_earth_hydrogen_extraction, theater_ratio, 5, 0.2).
narrative_ontology:measurement(rare_tr_t10, rare_earth_hydrogen_extraction, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(rare_be_t0, rare_earth_hydrogen_extraction, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(rare_be_t5, rare_earth_hydrogen_extraction, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(rare_be_t10, rare_earth_hydrogen_extraction, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rare_earth_hydrogen_extraction, resource_allocation).
narrative_ontology:affects_constraint(rare_earth_hydrogen_extraction, global_rare_earth_supply_chain).
narrative_ontology:affects_constraint(rare_earth_hydrogen_extraction, hydrogen_energy_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
