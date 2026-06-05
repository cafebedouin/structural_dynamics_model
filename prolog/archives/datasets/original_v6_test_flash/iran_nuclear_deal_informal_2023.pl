% ============================================================================
% CONSTRAINT STORY: iran_nuclear_deal_informal_2023
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iran_nuclear_deal_informal_2023, []).

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
 *   constraint_id: iran_nuclear_deal_informal_2023
 *   human_readable: Informal US-Iran Nuclear De-escalation Agreement (2023)
 *   domain: geopolitical
 *
 * SUMMARY:
 *   The informal US-Iran nuclear de-escalation agreement represents an effort
 *   to reduce tensions and prevent nuclear proliferation without a formal
 *   treaty. This arrangement balances the interests of the US and Iran while
 *   also creating potential risks for regional rivals and limitations on IAEA
 *   verification capabilities. The agreement's effectiveness and long-term
 *   sustainability are subject to ongoing monitoring and analysis.
 *
 * KEY AGENTS:
 *   - United States: Benefits from reduced tensions and strategic gains (institutional/arbitrage)
 *   - Iran: Benefits from sanctions relief and avoids escalation (institutional/arbitrage)
 *   - Regional Rivals: May perceive the agreement as a betrayal and be left vulnerable (powerless/trapped)
 *   - IAEA Verification Authority: Constrained by the informal nature of the agreement (moderate/constrained)
 *   - Global Stability: Overall global stability benefits (organized/mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iran_nuclear_deal_informal_2023, 0.55).
domain_priors:suppression_score(iran_nuclear_deal_informal_2023, 0.45).
domain_priors:theater_ratio(iran_nuclear_deal_informal_2023, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iran_nuclear_deal_informal_2023, extractiveness, 0.55).
narrative_ontology:constraint_metric(iran_nuclear_deal_informal_2023, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(iran_nuclear_deal_informal_2023, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iran_nuclear_deal_informal_2023, tangled_rope).
narrative_ontology:human_readable(iran_nuclear_deal_informal_2023, "Informal US-Iran Nuclear De-escalation Agreement (2023)").
narrative_ontology:topic_domain(iran_nuclear_deal_informal_2023, "geopolitical").

domain_priors:requires_active_enforcement(iran_nuclear_deal_informal_2023).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iran_nuclear_deal_informal_2023, united_states).
narrative_ontology:constraint_beneficiary(iran_nuclear_deal_informal_2023, iran).
narrative_ontology:constraint_beneficiary(iran_nuclear_deal_informal_2023, global_stability).
narrative_ontology:constraint_victim(iran_nuclear_deal_informal_2023, regional_rivals).
narrative_ontology:constraint_victim(iran_nuclear_deal_informal_2023, iaea_verification_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The US benefits from de-escalation by avoiding a nuclear crisis and reducing regional instability. Achieves strategic aims without formal treaty obligations.
constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Iran benefits from sanctions relief and avoids further economic hardship, as well as avoids military escalation.
constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Regional rivals such as Saudi Arabia and Israel may perceive the agreement as a betrayal and be left vulnerable to Iranian influence.
constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% The IAEA is constrained due to the informal nature of the agreement, limiting verification capabilities. This perspective experiences both benefits (some degree of cooperation) and costs (reduced transparency).
constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% From an analytical viewpoint, the informal agreement represents a tangled rope: a hybrid of coordination to prevent nuclear proliferation, alongside extraction from some actors.
constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iran_nuclear_deal_informal_2023_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(iran_nuclear_deal_informal_2023, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(iran_nuclear_deal_informal_2023_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Reflects the fact that the deal has both beneficiaries and victims and has an extraction component. Suppression (0.45): The agreement suppresses the options of regional rivals and the IAEA to some extent. Theater Ratio (0.30): A lower value given the direct nature of the agreement and its focus on verifiable actions.
 *
 * PERSPECTIVAL GAP:
 *   The US and Iran perceive a rope, benefiting from de-escalation. Regional rivals perceive a snare, being left more vulnerable. The IAEA and the analytical observer see a tangled rope, given a mix of benefits and constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each actor's power, exit options, and benefits/costs within the agreement's structure. Beneficiaries (US, Iran) have arbitrage options and see coordination. Victims (Regional Rivals) are trapped and see extraction. The IAEA is constrained and experiences mixed coordination and extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_compliance,
    'Will both parties continue to abide by the informal agreement in the long term, particularly under changing political climates?',
    'Monitoring continued adherence to the agreed-upon parameters. Changes in rhetoric and policy by both sides.',
    'If compliance fails, the agreement devolves into a snare. If compliance continues, it remains a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_compliance, empirical, 'The sustainability of the agreement given long-term shifts in power.').

omega_variable(
    verification_effectiveness,
    'How effective is the verification of Iranian nuclear activities given the informal nature of the agreement?',
    'Analysis of IAEA reports and independent assessments of Iranian nuclear activities.',
    'If verification is weak, the agreement is more of a snare. If verification is strong, it is more of a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_effectiveness, empirical, 'Assesses how effective the verification of Iranian nuclear activities is.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iran_nuclear_deal_informal_2023, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iran_tr_t0, iran_nuclear_deal_informal_2023, theater_ratio, 0, 0.2).
narrative_ontology:measurement(iran_tr_t1, iran_nuclear_deal_informal_2023, theater_ratio, 1, 0.25).
narrative_ontology:measurement(iran_tr_t2, iran_nuclear_deal_informal_2023, theater_ratio, 2, 0.3).

% Extraction over time
narrative_ontology:measurement(iran_be_t0, iran_nuclear_deal_informal_2023, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(iran_be_t1, iran_nuclear_deal_informal_2023, base_extractiveness, 1, 0.53).
narrative_ontology:measurement(iran_be_t2, iran_nuclear_deal_informal_2023, base_extractiveness, 2, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iran_nuclear_deal_informal_2023, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
