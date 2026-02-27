% ============================================================================
% CONSTRAINT STORY: fcc_dji_covered_list
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fcc_dji_covered_list, []).

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
 *   constraint_id: fcc_dji_covered_list
 *   human_readable: FCC 'Covered List' Ban on DJI Drones
 *   domain: technological/political
 *
 * SUMMARY:
 *   The FCC's ban on DJI drones, citing national security concerns,
 *   represents a complex interplay of technological, economic, and political
 *   factors. The ban aims to protect US national security interests and
 *   promote domestic drone manufacturing but also restricts consumer choice
 *   and potentially hinders innovation. The ban's effectiveness and long-term
 *   consequences are subject to ongoing debate.
 *
 * KEY AGENTS:
 *   - US Consumers: Primary victims (powerless/trapped) - face higher prices and fewer options.
 *   - DJI: Victim (powerful/mobile) - loses market access in the US.
 *   - US Drone Manufacturers: Beneficiary (institutional/arbitrage) - gain market share and reduced competition.
 *   - Cybersecurity Firms: Beneficiary (institutional/constrained) - increased demand for services.
 *   - Local Law Enforcement: Moderate victim (moderate/constrained) - limited in drone technology options.
 *   - Analytical Observer: (analytical/analytical) - Protectionist measure with potential drawbacks.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fcc_dji_covered_list, 0.6).
domain_priors:suppression_score(fcc_dji_covered_list, 0.7).
domain_priors:theater_ratio(fcc_dji_covered_list, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fcc_dji_covered_list, extractiveness, 0.6).
narrative_ontology:constraint_metric(fcc_dji_covered_list, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(fcc_dji_covered_list, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fcc_dji_covered_list, tangled_rope).
narrative_ontology:human_readable(fcc_dji_covered_list, "FCC 'Covered List' Ban on DJI Drones").
narrative_ontology:topic_domain(fcc_dji_covered_list, "technological/political").

domain_priors:requires_active_enforcement(fcc_dji_covered_list).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fcc_dji_covered_list, us_drone_manufacturers).
narrative_ontology:constraint_beneficiary(fcc_dji_covered_list, cybersecurity_firms).
narrative_ontology:constraint_victim(fcc_dji_covered_list, dji).
narrative_ontology:constraint_victim(fcc_dji_covered_list, us_consumers).
narrative_ontology:constraint_victim(fcc_dji_covered_list, local_law_enforcement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% US Consumers face higher prices and fewer options, with limited ability to influence policy or switch to alternative drone solutions that meet their needs at the same price point. Trapped by the ban.
constraint_indexing:constraint_classification(fcc_dji_covered_list, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Local Law Enforcement agencies are constrained by the ban, as DJI drones were often the most cost-effective and feature-rich options for surveillance and emergency response. They benefit from increased cybersecurity but are limited in options.
constraint_indexing:constraint_classification(fcc_dji_covered_list, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% US Drone Manufacturers benefit from the ban by gaining market share and reduced competition from DJI. They can arbitrage the situation by filling the demand previously met by DJI. This is a coordination mechanism where the ban protects their market.
constraint_indexing:constraint_classification(fcc_dji_covered_list, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Cybersecurity firms benefit from increased demand for their services to assess and secure drone systems, although they may be constrained by the need to navigate complex regulatory landscapes and adapt to evolving threats.
constraint_indexing:constraint_classification(fcc_dji_covered_list, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% DJI is a victim of the ban, losing market access in the US. While a powerful company, its exit options are limited due to the specific nature of the ban targeting its products in the US market.
constraint_indexing:constraint_classification(fcc_dji_covered_list, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% The analytical observer sees the ban as a tangled rope: a protectionist measure disguised as a security imperative. It benefits domestic manufacturers and cybersecurity firms while extracting value from consumers and limiting technology options.
constraint_indexing:constraint_classification(fcc_dji_covered_list, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fcc_dji_covered_list_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fcc_dji_covered_list, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fcc_dji_covered_list, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fcc_dji_covered_list, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fcc_dji_covered_list_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is rated at 0.6 because the ban does extract significant value from consumers through increased prices and limited options and from DJI through loss of market share. Suppression is rated at 0.7 due to the active enforcement of the ban and limited alternatives for consumers and law enforcement. Theater ratio is 0.3, because although security concerns are cited, the ban's practical effectiveness is debated, suggesting the performative component is not dominant.
 *
 * PERSPECTIVAL GAP:
 *   The perspective of US consumers is a Snare, as they are trapped with limited options. US drone manufacturers see the ban as a Rope, as it coordinates market share towards them. Law enforcement agencies see the ban as a Tangled Rope as they can only use other drones. DJI is harmed, although powerful, but can only shift production elsewhere. The analytical observer sees a protectionist bent, suggesting that there is a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   US Drone Manufacturers and Cybersecurity Firms benefit, indicated by a low directionality, while US Consumers, Local Law Enforcement, and DJI bear the costs, indicated by a high directionality. The power and exit options influence the exact impact on each agent.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as extraction by identifying the intended benefits for US drone manufacturers as a form of coordination. However, the higher extractiveness reflects the constraints on consumers and law enforcement, resulting in a tangled rope classification. The debate about the true cybersecurity threat and consumer choice allows for better clarification of the ban. It isn't a pure Snare because local firms are assisted by the action.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cybersecurity_threat_validation,
    'Is the cybersecurity threat posed by DJI drones a validated, significant risk, or primarily a justification for protectionist measures?',
    'Independent cybersecurity audits and penetration testing of DJI drones, coupled with analysis of real-world incidents involving DJI drones.',
    'If validated threat: Justifies the ban and strengthens the ''rope'' perspective. If primarily protectionist: Undermines the ban and shifts perspectives towards ''snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cybersecurity_threat_validation, empirical, 'Validity of the cybersecurity threat posed by DJI drones').

omega_variable(
    economic_impact_assessment,
    'What is the actual economic impact of the ban on US consumers, local law enforcement, and other stakeholders?',
    'Comprehensive economic analysis, including price comparisons, availability of alternatives, and impacts on public services.',
    'High negative impact: Strengthens the ''snare'' perspective for consumers and law enforcement. Limited impact: Weakens the ''snare'' perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact_assessment, empirical, 'Economic impact of the ban').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fcc_dji_covered_list, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fcc__tr_t0, fcc_dji_covered_list, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fcc__tr_t2, fcc_dji_covered_list, theater_ratio, 2, 0.3).
narrative_ontology:measurement(fcc__tr_t5, fcc_dji_covered_list, theater_ratio, 5, 0.4).

% Extraction over time
narrative_ontology:measurement(fcc__be_t0, fcc_dji_covered_list, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(fcc__be_t2, fcc_dji_covered_list, base_extractiveness, 2, 0.6).
narrative_ontology:measurement(fcc__be_t5, fcc_dji_covered_list, base_extractiveness, 5, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fcc_dji_covered_list, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
