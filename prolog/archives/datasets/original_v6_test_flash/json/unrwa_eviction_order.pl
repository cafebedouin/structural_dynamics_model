% ============================================================================
% CONSTRAINT STORY: unrwa_eviction_order
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-03-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unrwa_eviction_order, []).

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
 *   constraint_id: unrwa_eviction_order
 *   human_readable: Israeli Land Authority's Eviction Order for UNRWA HQ in East Jerusalem
 *   domain: geopolitical/legal
 *
 * SUMMARY:
 *   The Israeli Land Authority (ILA) has issued an order for the United
 *   Nations Relief and Works Agency (UNRWA) to vacate its headquarters in
 *   East Jerusalem within 30 days, citing alleged contract violations and
 *   illegal construction. This action creates a constraint impacting multiple
 *   actors with varying degrees of power and exit options. Palestinian
 *   refugees, heavily reliant on UNRWA, face the most severe consequences,
 *   while the ILA benefits from asserting control over state-owned land. The
 *   international community observes the situation with concern, as it
 *   potentially undermines international norms regarding humanitarian
 *   assistance and refugee protection.
 *
 * KEY AGENTS:
 *   - UNRWA: Moderate/Constrained
 *   - Palestinian Refugees: Powerless/Trapped
 *   - Israeli Land Authority: Institutional/Arbitrage
 *   - Israeli Government: Institutional/Arbitrage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unrwa_eviction_order, 0.65).
domain_priors:suppression_score(unrwa_eviction_order, 0.7).
domain_priors:theater_ratio(unrwa_eviction_order, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unrwa_eviction_order, extractiveness, 0.65).
narrative_ontology:constraint_metric(unrwa_eviction_order, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(unrwa_eviction_order, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unrwa_eviction_order, snare).
narrative_ontology:human_readable(unrwa_eviction_order, "Israeli Land Authority's Eviction Order for UNRWA HQ in East Jerusalem").
narrative_ontology:topic_domain(unrwa_eviction_order, "geopolitical/legal").

domain_priors:requires_active_enforcement(unrwa_eviction_order).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unrwa_eviction_order, israeli_land_authority).
narrative_ontology:constraint_beneficiary(unrwa_eviction_order, israeli_government).
narrative_ontology:constraint_victim(unrwa_eviction_order, unrwa).
narrative_ontology:constraint_victim(unrwa_eviction_order, palestinian_refugees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN REFUGEES (SNARE) - Trapped and highly dependent on UNRWA services, with very limited exit options. The eviction order further suppresses their access to essential aid and support, exacerbating their vulnerability.
constraint_indexing:constraint_classification(unrwa_eviction_order, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: UNRWA (TANGLED ROPE) - Constrained by the eviction order, impacting its operational capacity and ability to deliver services. However, UNRWA also benefits from international recognition and support, allowing it to potentially relocate and continue its mission, albeit with significant challenges.
constraint_indexing:constraint_classification(unrwa_eviction_order, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ISRAELI LAND AUTHORITY (ROPE) - Benefits from enforcing its legal authority and reclaiming land. The eviction order aligns with the ILA's mandate to manage state-owned land efficiently. The ILA benefits from increased control over land management.
constraint_indexing:constraint_classification(unrwa_eviction_order, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL LAW (PITON) - The eviction order undermines international norms regarding the protection of humanitarian organizations and the rights of refugees, leading to a degradation of the international legal framework. High theater due to repeated violations without effective enforcement.
constraint_indexing:constraint_classification(unrwa_eviction_order, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) - The eviction order serves as a tool to exert pressure on UNRWA and potentially alter the status quo in East Jerusalem. The international community can provide or withhold aid contingent on UNRWA remaining in place or relocating operations. There is both coordination (between the Israeli Land Authority and Israeli Government) and extraction from the Palestinian Refugees.
constraint_indexing:constraint_classification(unrwa_eviction_order, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unrwa_eviction_order_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unrwa_eviction_order, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unrwa_eviction_order, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(unrwa_eviction_order, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(unrwa_eviction_order, TR),
    TR >= 0.70.

:- end_tests(unrwa_eviction_order_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High, reflecting the significant disruption to UNRWA's operations and the potential harm to Palestinian refugees who depend on its services. Suppression (0.70): High, indicating limited alternatives for Palestinian refugees who are trapped by the ongoing conflict and dependency on UNRWA. Theater Ratio (0.75): High, the eviction order has a direct and material impact on UNRWA's operations, but also has a performative aspect in terms of signaling Israeli sovereignty and control over Jerusalem.
 *
 * PERSPECTIVAL GAP:
 *   Palestinian refugees view this as a Snare, as they have no exit options and face severe consequences. UNRWA sees it as a Tangled Rope because of their limited mobility and benefit from international support. The Israeli Land Authority and Government view it as a Rope in terms of enforcing their legal authority. An Analytical Observer sees a tangled rope because there are competing forces.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian refugees (victims/trapped) face the greatest negative impact. UNRWA (moderate/constrained) bears costs but can also potentially adapt through relocation. The ILA (institutional/arbitrage) benefits from increased control and land management.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legality_of_construction,
    'Was the construction by UNRWA truly illegal, or is this a pretext for eviction?',
    'Independent legal review of the construction permits and land ownership documents.',
    'If illegal, the eviction order is more justifiable. If a pretext, it highlights political motivations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legality_of_construction, empirical, 'Legality of the alleged construction violations.').

omega_variable(
    impact_on_refugee_services,
    'What will be the actual impact on UNRWA''s ability to provide services to Palestinian refugees?',
    'Assessment of UNRWA''s contingency plans and the availability of alternative service delivery mechanisms.',
    'Severe impact strengthens the Snare classification. Minimal impact suggests a more theatrical or symbolic action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_refugee_services, empirical, 'The extent to which the eviction impacts service provision to refugees').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unrwa_eviction_order, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unrw_tr_t0, unrwa_eviction_order, theater_ratio, 0, 0.2).
narrative_ontology:measurement(unrw_tr_t15, unrwa_eviction_order, theater_ratio, 15, 0.7).
narrative_ontology:measurement(unrw_tr_t30, unrwa_eviction_order, theater_ratio, 30, 0.75).

% Extraction over time
narrative_ontology:measurement(unrw_be_t0, unrwa_eviction_order, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(unrw_be_t15, unrwa_eviction_order, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(unrw_be_t30, unrwa_eviction_order, base_extractiveness, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unrwa_eviction_order, resource_allocation).
narrative_ontology:affects_constraint(unrwa_eviction_order, palestinian_refugee_crisis).
narrative_ontology:affects_constraint(unrwa_eviction_order, un_funding_stability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
