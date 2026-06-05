% ============================================================================
% CONSTRAINT STORY: nsw_transmission_bottleneck
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsw_transmission_bottleneck, []).

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
 *   constraint_id: nsw_transmission_bottleneck
 *   human_readable: NSW Regional Transmission Congestion
 *   domain: technological/political
 *
 * SUMMARY:
 *   The physical limit on megawatts that can be transferred from South
 *   Australia to New South Wales results in transmission congestion. This
 *   constraint impacts renewable energy generators in South Australia, limits
 *   access to cheaper renewable energy for NSW consumers, and creates
 *   opportunities for incumbent fossil fuel generators. The interaction
 *   between infrastructure limitations and market dynamics creates the
 *   opportunity for this constraint to act as a tangled rope.
 *
 * KEY AGENTS:
 *   - Renewable Energy Generators in SA: Primary target (powerless/trapped) - bear extraction due to curtailed generation.
 *   - Coal Power Generators in NSW: Primary beneficiary (powerful/constrained) - benefit from reduced competition.
 *   - NSW Electricity Consumers: Secondary victim (moderate/constrained) - pay higher prices.
 *   - Transmission Network Operators: Beneficiary (institutional/arbitrage) - benefit from infrastructure investment and congestion management revenues.
 *   - Analytical Observer: Civilizational view (analytical/analytical) - sees infrastructure limitations and market design flaws.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsw_transmission_bottleneck, 0.6).
domain_priors:suppression_score(nsw_transmission_bottleneck, 0.4).
domain_priors:theater_ratio(nsw_transmission_bottleneck, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsw_transmission_bottleneck, extractiveness, 0.6).
narrative_ontology:constraint_metric(nsw_transmission_bottleneck, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(nsw_transmission_bottleneck, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsw_transmission_bottleneck, tangled_rope).
narrative_ontology:human_readable(nsw_transmission_bottleneck, "NSW Regional Transmission Congestion").
narrative_ontology:topic_domain(nsw_transmission_bottleneck, "technological/political").

domain_priors:requires_active_enforcement(nsw_transmission_bottleneck).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsw_transmission_bottleneck, coal_power_generators_in_nsw).
narrative_ontology:constraint_beneficiary(nsw_transmission_bottleneck, transmission_network_operators).
narrative_ontology:constraint_victim(nsw_transmission_bottleneck, renewable_energy_generators_in_sa).
narrative_ontology:constraint_victim(nsw_transmission_bottleneck, nsw_electricity_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% South Australian renewable energy generators are trapped by the bottleneck, unable to fully export their electricity to NSW, leading to curtailed output and lost revenue.
constraint_indexing:constraint_classification(nsw_transmission_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% Transmission network operators benefit from congestion by creating a need for new infrastructure investment and potentially increasing revenues through congestion management.
constraint_indexing:constraint_classification(nsw_transmission_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Analytical observer recognizes the tangle of infrastructure limitations, market design flaws, and political considerations that lead to transmission congestion.
constraint_indexing:constraint_classification(nsw_transmission_bottleneck, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Consumers in NSW are constrained; they bear higher electricity prices due to the limited supply from SA's cheaper renewables. However, they also benefit from the reliability provided by existing coal generators.
constraint_indexing:constraint_classification(nsw_transmission_bottleneck, tangled_rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(regional))).

% The coal generators in NSW benefit from the reduced competition from SA renewables due to the transmission constraints. They are somewhat constrained by regulations.
constraint_indexing:constraint_classification(nsw_transmission_bottleneck, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsw_transmission_bottleneck_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nsw_transmission_bottleneck, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nsw_transmission_bottleneck, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsw_transmission_bottleneck, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsw_transmission_bottleneck_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): High extraction. The constraint significantly limits the ability of SA renewable generators to export power, resulting in lost revenue and potentially stranded assets. Suppression (0.4): Moderate suppression. While new transmission infrastructure can be built, it is a lengthy and expensive process, effectively suppressing competition from SA renewables. Theater ratio (0.2): Low theater. There is limited performative activity associated with the transmission congestion itself; most activity is focused on building new infrastructure or changing market rules.
 *
 * PERSPECTIVAL GAP:
 *   The renewable generators in SA experience the constraint as a snare, limiting their access to the NSW market. Transmission network operators view it as a coordination mechanism, necessitating infrastructure investments. The analytical observer sees a tangled rope - a complex interaction of market dynamics, infrastructure limitations, and potentially political considerations.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable generators in SA are victims and trapped, leading to a high directionality value. Coal generators benefit from less competition, resulting in a low directionality value. Transmission network operators also benefit, further lowering their directionality value. Consumers are both victims (higher prices) and beneficiaries (reliable supply), giving a moderate directionality value.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification is appropriate because the constraint involves both coordination (managing electricity flow) and extraction (limiting competition and raising prices). It prevents mislabeling as pure coordination by highlighting the negative impacts on SA renewable generators and NSW consumers, and as pure extraction by recognizing the genuine need for network management and stability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    future_transmission_capacity,
    'How quickly can transmission capacity between SA and NSW be increased?',
    'Project timelines for new transmission lines and HVDC interconnectors.',
    'Higher transmission capacity reduces extraction, potentially shifting the constraint towards a pure coordination problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_transmission_capacity, empirical, 'Timeline and cost for new transmission capacity.').

omega_variable(
    market_design_changes,
    'Can market design changes alleviate the congestion and create more efficient dispatch?',
    'Modeling the impact of different market rules on congestion and generator dispatch.',
    'Effective market design changes can reduce the extractiveness of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_design_changes, conceptual, 'Impact of market design changes on congestion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsw_transmission_bottleneck, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsw__tr_t0, nsw_transmission_bottleneck, theater_ratio, 0, 0.1).
narrative_ontology:measurement(nsw__tr_t5, nsw_transmission_bottleneck, theater_ratio, 5, 0.2).
narrative_ontology:measurement(nsw__tr_t10, nsw_transmission_bottleneck, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(nsw__be_t0, nsw_transmission_bottleneck, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(nsw__be_t5, nsw_transmission_bottleneck, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(nsw__be_t10, nsw_transmission_bottleneck, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsw_transmission_bottleneck, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
