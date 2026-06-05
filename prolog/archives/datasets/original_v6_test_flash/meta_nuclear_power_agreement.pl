% ============================================================================
% CONSTRAINT STORY: meta_nuclear_power_agreement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_meta_nuclear_power_agreement, []).

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
 *   constraint_id: meta_nuclear_power_agreement
 *   human_readable: Meta's direct investment and offtake agreements for advanced nuclear power
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Meta, a massive energy consumer for its AI data centers, is signing
 *   direct agreements with advanced nuclear power developers (e.g., for Small
 *   Modular Reactors). This is done to secure a long-term, stable, and
 *   potentially carbon-neutral power supply, reducing its reliance on
 *   traditional utilities. This development can be viewed as a tangled rope,
 *   where Meta and advanced nuclear developers benefit while traditional
 *   utilities and renewable energy sectors potentially face disadvantages.
 *
 * KEY AGENTS:
 *   - Meta: Beneficiary (institutional/arbitrage) - Secures stable power supply and energy price hedge.
 *   - Advanced Nuclear Developers: Beneficiary (institutional/arbitrage) - Secures long-term funding and revenue.
 *   - Traditional Utilities: Victim (moderate/constrained) - Face increased competition and potential market share loss.
 *   - Renewable Energy Sector: Victim (moderate/mobile) - Faces increased competition for investment.
 *   - Local Communities near Nuclear Sites: Victim (powerless/trapped) - Bear the risks associated with nuclear power generation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(meta_nuclear_power_agreement, 0.45).
domain_priors:suppression_score(meta_nuclear_power_agreement, 0.3).
domain_priors:theater_ratio(meta_nuclear_power_agreement, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(meta_nuclear_power_agreement, extractiveness, 0.45).
narrative_ontology:constraint_metric(meta_nuclear_power_agreement, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(meta_nuclear_power_agreement, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(meta_nuclear_power_agreement, tangled_rope).
narrative_ontology:human_readable(meta_nuclear_power_agreement, "Meta's direct investment and offtake agreements for advanced nuclear power").
narrative_ontology:topic_domain(meta_nuclear_power_agreement, "technological/economic").

domain_priors:requires_active_enforcement(meta_nuclear_power_agreement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(meta_nuclear_power_agreement, meta).
narrative_ontology:constraint_beneficiary(meta_nuclear_power_agreement, advanced_nuclear_developers).
narrative_ontology:constraint_victim(meta_nuclear_power_agreement, traditional_utilities).
narrative_ontology:constraint_victim(meta_nuclear_power_agreement, renewable_energy_sector).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Meta secures a long-term, stable, and potentially carbon-neutral power supply, reducing its reliance on traditional utilities and hedging against future energy price volatility. They can potentially arbitrage by selling excess power back to the grid.
constraint_indexing:constraint_classification(meta_nuclear_power_agreement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Secures long-term funding and guaranteed revenue streams, de-risking their projects and making them more attractive to investors. Allows them to scale production and compete with established energy sources.
constraint_indexing:constraint_classification(meta_nuclear_power_agreement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Face increased competition from direct power purchase agreements, potentially losing large customers and market share. Constrained by existing infrastructure and regulatory frameworks, making it difficult to compete with the agility of direct investment deals.
constraint_indexing:constraint_classification(meta_nuclear_power_agreement, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The renewable energy sector faces increased competition for investment and market share, as advanced nuclear is presented as a comparably green energy source with consistent availability. Mobile in that investment can flow towards other projects.
constraint_indexing:constraint_classification(meta_nuclear_power_agreement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% Views Meta's agreement as a tangled rope: some coordination of a novel technology with guaranteed investment. Asymmetric extraction between those who benefit from the technology and those disadvantaged by the reduction in investment for alternative green energy technologies.
constraint_indexing:constraint_classification(meta_nuclear_power_agreement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% These communities bear the risks associated with nuclear power generation, including potential accidents and long-term waste storage. Trapped due to limited mobility and economic dependence on the energy infrastructure.
constraint_indexing:constraint_classification(meta_nuclear_power_agreement, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(meta_nuclear_power_agreement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(meta_nuclear_power_agreement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(meta_nuclear_power_agreement, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(meta_nuclear_power_agreement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45): Meta's investment redirects capital from other energy sources, creating a moderate extraction from other energy markets and local communities. Suppression (0.30): The agreement suppresses investment and growth in the renewable energy sector and constrains traditional utilities. The theater ratio is low (0.20), indicating a genuine move towards advanced power infrastructure.
 *
 * PERSPECTIVAL GAP:
 *   Meta and nuclear developers see a win-win, a pure rope classification. Traditional utilities view it as a snare, as they have fewer options to change course. The renewable energy sector also views it as a tangled rope, given it may indirectly benefit from cheaper power but faces competition. From an analytical perspective, this constraint is a tangled rope, coordinating funding of novel tech but asymmetrically impacting incumbents.
 *
 * DIRECTIONALITY LOGIC:
 *   Meta has arbitrage and benefits from the deal. Nuclear developers arbitrage and benefit from the deal. Traditional utilities are constrained and bear costs. Renewable energy are mobile but bear costs. Local communities are trapped and bear costs.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_safety,
    'What are the long-term safety implications of advanced nuclear reactors?',
    'Longitudinal monitoring of reactor performance, independent safety reviews, and public health studies.',
    'If safe: supports continued investment and expansion. If unsafe: undermines public trust and leads to regulatory restrictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_safety, empirical, 'Uncertainty regarding the long-term safety and environmental impact of advanced nuclear technologies.').

omega_variable(
    economic_viability,
    'Are advanced nuclear reactors economically competitive with other energy sources?',
    'Cost-benefit analysis of reactor construction and operation, comparison with renewable energy and fossil fuels, and market demand analysis.',
    'If economically viable: accelerates adoption and reduces reliance on subsidies. If not viable: limits scalability and hinders market penetration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_viability, empirical, 'Uncertainty around the economic viability of advanced nuclear energy compared to renewable energy or traditional utilities.').

omega_variable(
    waste_disposal_solution,
    'Will there be a permanent waste disposal solution for nuclear waste?',
    'Research and development of advanced waste disposal technologies, political consensus on waste storage sites, and public acceptance of waste management practices.',
    'If successful: alleviates environmental concerns and fosters public support. If unsuccessful: exacerbates environmental risks and hinders reactor deployment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(waste_disposal_solution, preference, 'Uncertainty around the solution to the current nuclear waste disposal problem.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(meta_nuclear_power_agreement, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meta_tr_t0, meta_nuclear_power_agreement, theater_ratio, 0, 0.1).
narrative_ontology:measurement(meta_tr_t5, meta_nuclear_power_agreement, theater_ratio, 5, 0.2).
narrative_ontology:measurement(meta_tr_t10, meta_nuclear_power_agreement, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(meta_be_t0, meta_nuclear_power_agreement, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(meta_be_t5, meta_nuclear_power_agreement, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(meta_be_t10, meta_nuclear_power_agreement, base_extractiveness, 10, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(meta_nuclear_power_agreement, resource_allocation).
narrative_ontology:affects_constraint(meta_nuclear_power_agreement, renewable_energy_investment).
narrative_ontology:affects_constraint(meta_nuclear_power_agreement, energy_grid_stability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
