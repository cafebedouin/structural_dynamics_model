% ============================================================================
% CONSTRAINT STORY: us_suburban_zoning_2025
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_suburban_zoning_2025, []).

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
 *   constraint_id: us_suburban_zoning_2025
 *   human_readable: Single-Family Suburban Zoning Codes
 *   domain: political/legal/economic
 *
 * SUMMARY:
 *   Local zoning codes in the United States restrict large swaths of
 *   metropolitan land to low-density, single-family homes. This constraint
 *   story examines the impacts of these zoning codes on various stakeholders,
 *   including homeowners, renters, and future residents. The system creates
 *   winners and losers, and there are complex trade-offs to consider when
 *   evaluating the effectiveness and fairness of these regulations.
 *
 * KEY AGENTS:
 *   - Existing Homeowners: Primary beneficiary (institutional/arbitrage) - benefit from increased property values and preservation of neighborhood character.
 *   - Low-Income Renters: Primary victim (powerless/trapped) - trapped by the lack of affordable housing options due to exclusionary zoning.
 *   - Future Residents: Secondary victim (moderate/constrained) - constrained by the limited housing supply and higher costs imposed by zoning regulations.
 *   - Construction Industry: Secondary beneficiary (powerful/mobile) - benefit from increased demand for single-family homes and the ability to build in desirable areas.
 *   - YIMBY Organizations: Organized agents (organized/mobile) - actively working to reform zoning laws and increase housing density.
 *   - Local Zoning Boards: Institutional actors (institutional/constrained) - constrained by existing regulations and political pressures from homeowners.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_suburban_zoning_2025, 0.6).
domain_priors:suppression_score(us_suburban_zoning_2025, 0.7).
domain_priors:theater_ratio(us_suburban_zoning_2025, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_suburban_zoning_2025, extractiveness, 0.6).
narrative_ontology:constraint_metric(us_suburban_zoning_2025, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(us_suburban_zoning_2025, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_suburban_zoning_2025, tangled_rope).
narrative_ontology:human_readable(us_suburban_zoning_2025, "Single-Family Suburban Zoning Codes").
narrative_ontology:topic_domain(us_suburban_zoning_2025, "political/legal/economic").

domain_priors:requires_active_enforcement(us_suburban_zoning_2025).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_suburban_zoning_2025, existing_homeowners).
narrative_ontology:constraint_beneficiary(us_suburban_zoning_2025, construction_industry).
narrative_ontology:constraint_victim(us_suburban_zoning_2025, low_income_renters).
narrative_ontology:constraint_victim(us_suburban_zoning_2025, future_residents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Low-income renters are trapped by the lack of affordable housing options within the region due to exclusionary zoning. They have little power to change zoning laws and are disproportionately affected by the limited housing supply.
constraint_indexing:constraint_classification(us_suburban_zoning_2025, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% Existing homeowners benefit from increased property values and the preservation of neighborhood character. They often have the power to influence local zoning decisions and can arbitrage their position through political action and community organizing.
constraint_indexing:constraint_classification(us_suburban_zoning_2025, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(local))).

% Future residents are constrained by the limited housing supply and higher costs imposed by zoning regulations. While they may benefit from the perceived stability and character of single-family neighborhoods, their housing options are limited, and they lack representation in current zoning decisions.
constraint_indexing:constraint_classification(us_suburban_zoning_2025, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% YIMBY (Yes In My Backyard) organizations are actively working to reform zoning laws and increase housing density. They view the current zoning system as a temporary obstacle to be overcome through advocacy and policy changes. They are mobile because they can advocate for changes in zoning policy.
constraint_indexing:constraint_classification(us_suburban_zoning_2025, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% Local zoning boards are often trapped by existing regulations and political pressures from homeowners. While they may have the authority to make changes, they are constrained by limited resources and public opposition, resulting in incremental adjustments rather than fundamental reforms.
constraint_indexing:constraint_classification(us_suburban_zoning_2025, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(local))).

% From a civilizational and global perspective, single-family zoning presents a mixed picture. On one hand, it preserves neighborhood character and promotes stability. On the other hand, it limits housing supply, increases costs, and perpetuates inequality. The long-term consequences of this system are difficult to predict, making it a tangled rope with both benefits and drawbacks.
constraint_indexing:constraint_classification(us_suburban_zoning_2025, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_suburban_zoning_2025_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_suburban_zoning_2025, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_suburban_zoning_2025, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_suburban_zoning_2025, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_suburban_zoning_2025, TR),
    TR >= 0.70.

:- end_tests(us_suburban_zoning_2025_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): Significant. Zoning regulations extract value from low-income renters and future residents by limiting housing supply and increasing costs. Existing homeowners and the construction industry benefit from this extraction. Suppression (0.7): High. Zoning codes actively suppress alternative housing options, such as apartments and townhouses, and limit the potential for higher-density development. This suppression is enforced through legal regulations and community opposition. Theater ratio (0.3): Moderate. While there is some public participation in zoning decisions, the process is often dominated by homeowners and developers, and the interests of renters and future residents are not adequately represented.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives of different stakeholders vary significantly. Homeowners see zoning as a way to protect their property values and neighborhood character, while renters see it as a barrier to affordable housing. Future residents may benefit from the perceived stability of single-family neighborhoods, but they also face limited housing options and higher costs. YIMBY organizations view zoning as a temporary obstacle to be overcome through advocacy and policy changes.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's position relative to the extraction flow. Existing homeowners and the construction industry benefit from zoning regulations, while renters and future residents bear the costs. YIMBY organizations are working to change the system, and their directionality is therefore more complex. Local zoning boards are often caught in the middle, trying to balance the competing interests of different stakeholders.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint presents a trade-off between the benefits of preserving neighborhood character and the costs of limiting housing supply and increasing inequality. The classification as a tangled rope reflects the complex interplay of coordination and extraction, with no easy solutions. While zoning can be seen as a tool for coordination (e.g., preventing incompatible land uses), it also creates significant barriers to entry and perpetuates existing patterns of segregation. The challenge is to find a balance that promotes both affordability and community stability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    density_tolerance_threshold,
    'What is the maximum acceptable density increase that can be implemented without causing significant social or environmental disruption?',
    'Empirical studies of density impacts, community surveys, and simulations of traffic and infrastructure changes.',
    'If low tolerance: zoning reform will be limited, perpetuating existing housing shortages. If high tolerance: more significant zoning changes will be possible, potentially leading to more affordable housing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(density_tolerance_threshold, empirical, 'Density increase tolerance level in existing single family areas.').

omega_variable(
    political_feasibility_of_reform,
    'What is the likelihood of overcoming political opposition to zoning reform and implementing meaningful changes?',
    'Analysis of political dynamics, public opinion polls, and case studies of successful zoning reforms in other areas.',
    'If low feasibility: zoning reform will be stalled, and existing patterns of segregation and inequality will persist. If high feasibility: zoning reform will be successful, leading to more diverse and inclusive communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_feasibility_of_reform, empirical, 'Political feasibility of zoning reform.').

omega_variable(
    economic_impact_of_deregulation,
    'What would be the economic consequences of deregulating zoning and allowing higher-density housing?',
    'Economic modeling of housing markets, employment patterns, and tax revenues under different zoning scenarios.',
    'If negative impact: zoning deregulation may lead to unintended consequences, such as increased traffic congestion and infrastructure strain. If positive impact: zoning deregulation may boost economic growth and create more opportunities for low-income residents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact_of_deregulation, empirical, 'Economic impact from zoning deregulation and increased densification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_suburban_zoning_2025, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_s_tr_t0, us_suburban_zoning_2025, theater_ratio, 0, 0.2).
narrative_ontology:measurement(us_s_tr_t10, us_suburban_zoning_2025, theater_ratio, 10, 0.3).
narrative_ontology:measurement(us_s_tr_t20, us_suburban_zoning_2025, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(us_s_be_t0, us_suburban_zoning_2025, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(us_s_be_t10, us_suburban_zoning_2025, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(us_s_be_t20, us_suburban_zoning_2025, base_extractiveness, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_suburban_zoning_2025, resource_allocation).
narrative_ontology:affects_constraint(us_suburban_zoning_2025, us_housing_market_instability).
narrative_ontology:affects_constraint(us_suburban_zoning_2025, urban_sprawl_inefficiency).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
