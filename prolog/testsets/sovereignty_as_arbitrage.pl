% ============================================================================
% CONSTRAINT STORY: sovereignty_as_arbitrage
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-15
% ============================================================================

:- module(constraint_sovereignty_as_arbitrage, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
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
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sovereignty_as_arbitrage
 *   human_readable: The Alberta Prosperity Project (State-Building Arbitrage)
 *   domain: political/economic
 *
 * SUMMARY:
 *   This constraint represents the attempt by separatist organizations like the
 *   Alberta Prosperity Project (APP) to reframe sovereignty as a mechanism for
 *   economic arbitrage. By "seizing sovereignty," the movement aims to
 *   eliminate federal "siphoning" and environmental limits, promising a
 *   low-tax petro-state that bypasses Canada's constitutional and climate
 *   obligations.
 *
 * KEY AGENTS (by structural relationship):
 *   - separatist_leadership: Primary beneficiary (organized/mobile) — gains
 *     institutional power and secures hydrocarbon-centric policy.
 *   - national_social_safety_net: Primary target (institutional/constrained) — loses
 *     territorial integrity and fiscal contribution.
 *   - low_income_voters: Secondary victim (powerless/trapped) — bears the
 *     risk of service cuts if tax-slash promises undermine resilience.
 *   - global_investors: Analytical observer (powerful/arbitrage) — evaluates
 *     the stability of an independent Alberta military and currency.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereignty_as_arbitrage, 0.48).
domain_priors:suppression_score(sovereignty_as_arbitrage, 0.42).
domain_priors:theater_ratio(sovereignty_as_arbitrage, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereignty_as_arbitrage, extractiveness, 0.48).
narrative_ontology:constraint_metric(sovereignty_as_arbitrage, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(sovereignty_as_arbitrage, theater_ratio, 0.65).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(sovereignty_as_arbitrage, tangled_rope).
narrative_ontology:human_readable(sovereignty_as_arbitrage, "The Alberta Prosperity Project (State-Building Arbitrage)").
narrative_ontology:topic_domain(sovereignty_as_arbitrage, "political/economic").

% --- Binary flags ---
domain_priors:requires_active_enforcement(sovereignty_as_arbitrage).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
narrative_ontology:constraint_beneficiary(sovereignty_as_arbitrage, separatist_leadership).
narrative_ontology:constraint_victim(sovereignty_as_arbitrage, national_social_safety_net).
narrative_ontology:constraint_victim(sovereignty_as_arbitrage, low_income_voters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SEPARATIST LEADERSHIP (ROPE)
% Sovereignty is viewed as a pure coordination tool to restore "freedom"
% and "entrepreneurship" from federal control. This is the beneficiary view.
constraint_indexing:constraint_classification(sovereignty_as_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: THE CANADIAN FEDERALIST (SNARE)
% Viewed as a high-extraction mechanism that threatens to "break up the
% country" and strip common social investments.
constraint_indexing:constraint_classification(sovereignty_as_arbitrage, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: THE LOW-INCOME VOTER (SNARE)
% The promise of low taxes is seen as a high-risk gamble that could lead to
% cuts in essential public services, trapping them in a less resilient state.
constraint_indexing:constraint_classification(sovereignty_as_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Recognizes a coordination function (state-building) entangled with
% asymmetric extraction (evading climate costs and redistribution).
constraint_indexing:constraint_classification(sovereignty_as_arbitrage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civil_izational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereignty_as_arbitrage_tests).

test(tangled_rope_gate) :-
    % Verify core Tangled Rope requirements: beneficiary + victim + enforcement.
    narrative_ontology:constraint_beneficiary(sovereignty_as_arbitrage, _),
    narrative_ontology:constraint_victim(sovereignty_as_arbitrage, _),
    domain_priors:requires_active_enforcement(sovereignty_as_arbitrage).

test(perspectival_gap_beneficiary_victim) :-
    % Verify gap between beneficiary (Rope) and a primary victim (Snare).
    constraint_indexing:constraint_classification(sovereignty_as_arbitrage, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(sovereignty_as_arbitrage, snare, context(agent_power(powerless), _, exit_options(trapped), _)).

test(extraction_floor) :-
    % Verify extraction is high enough to trigger the Tangled Rope gate.
    narrative_ontology:constraint_metric(sovereignty_as_arbitrage, extractiveness, E), E >= 0.30.

:- end_tests(sovereignty_as_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Classified as a Tangled Rope because it offers a genuine coordination
 *   vision (a new state) while enabling the extraction of wealth from the
 *   previous national arrangement by evading federal taxes and climate policy.
 *   The base extraction of 0.48 is just above the Snare threshold, reflecting
 *   the significant fiscal impact of separation. The high theater ratio (0.65)
 *   reflects the performative nature of diplomacy with the U.S. and
 *   "sovereignty acts" that test constitutional boundaries without full effect.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: for beneficiaries (separatist leadership), it's a Rope to
 *   coordinate a new, freer political entity. For the federal government and
 *   powerless citizens who rely on national programs, it's a Snare that
 *   threatens the integrity of the state and the social safety net.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the provincial political elite and hydrocarbon
 *   stakeholders who can arbitrage federal policy. The victims are the
 *   broader Canadian federation (losing its most productive province) and
 *   low-income Albertans who risk losing federally-supported services.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents the movement from being mislabeled as "purely"
 *   about freedom (Rope) by exposing the asymmetric extraction involved in
 *   slashing taxes for corporations while externalizing the risk of fiscal
 *   instability onto vulnerable populations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_u_s_recognition,
    'Will the U.S. actually support an independent petro-state that undermines a key ally?',
    'Analysis of State Department communications and Canadian sovereignty warnings.',
    'If yes, arbitrage potential increases; if no, it becomes a Piton of symbolic protest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(omega_u_s_recognition, empirical, 'Geopolitical recognition of separatist arbitrage').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Intervals reflect the 2026 timeframe of the "sovereigntist turn".
narrative_ontology:interval(sovereignty_as_arbitrage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Tracking extraction accumulation and theatricality as separatist rhetoric intensifies.
narrative_ontology:measurement(saa_ex_t0, sovereignty_as_arbitrage, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(saa_ex_t5, sovereignty_as_arbitrage, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(saa_ex_t10, sovereignty_as_arbitrage, base_extractiveness, 10, 0.48).

narrative_ontology:measurement(saa_tr_t0, sovereignty_as_arbitrage, theater_ratio, 0, 0.40).
narrative_ontology:measurement(saa_tr_t5, sovereignty_as_arbitrage, theater_ratio, 5, 0.53).
narrative_ontology:measurement(saa_tr_t10, sovereignty_as_arbitrage, theater_ratio, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereignty_as_arbitrage, enforcement_mechanism).

% This constraint is causally downstream from two other major constraints:
% the economic reality of fossil fuel dependency and the political friction
% of Canada's fiscal equalization program.
narrative_ontology:affects_constraint(fossil_fuel_lock_in, sovereignty_as_arbitrage).
narrative_ontology:affects_constraint(fiscal_equalization_friction, sovereignty_as_arbitrage).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */