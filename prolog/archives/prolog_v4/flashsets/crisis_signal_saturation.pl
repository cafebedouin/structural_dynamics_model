% ============================================================================
% CONSTRAINT STORY: crisis_signal_saturation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_crisis_signal_saturation, []).

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
 *   constraint_id: crisis_signal_saturation
 *   human_readable: The Perpetual Alarm Fatigue
 *   domain: informational/psychological/sociological
 *
 * SUMMARY:
 *   A scenario where the "Rope" of real-time global risk monitoring and alert
 *   systems (emergency broadcasts, market volatility pings, pandemic
 *   trackers) reaches such high frequency and intensity that the subject's
 *   nervous system habituates to "crisis" as the baseline. This leads to a
 *   desensitization to genuine threats and a general sense of anxiety and
 *   helplessness. Simultaneously, the constant alerts provide a steady stream
 *   of revenue and power for various organizations.
 *
 * KEY AGENTS:
 *   - General Population: Primary target (powerless/trapped) - constantly bombarded with alerts, leading to fatigue and desensitization.
 *   - First Responders: Secondary target (moderate/constrained) - need alerts to do their jobs, but experience burnout from constant false alarms.
 *   - News Aggregators: Primary beneficiary (institutional/arbitrage) - benefit from increased attention and engagement driven by crisis alerts.
 *   - Government Agencies: Secondary beneficiary (powerful/mobile) - benefit from the perceived need for their services created by constant alerts.
 *   - Security Firms: Secondary beneficiary (powerful/arbitrage) - benefit from sales of security solutions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(crisis_signal_saturation, 0.55).
domain_priors:suppression_score(crisis_signal_saturation, 0.7).
domain_priors:theater_ratio(crisis_signal_saturation, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(crisis_signal_saturation, extractiveness, 0.55).
narrative_ontology:constraint_metric(crisis_signal_saturation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(crisis_signal_saturation, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(crisis_signal_saturation, tangled_rope).
narrative_ontology:human_readable(crisis_signal_saturation, "The Perpetual Alarm Fatigue").
narrative_ontology:topic_domain(crisis_signal_saturation, "informational/psychological/sociological").

domain_priors:requires_active_enforcement(crisis_signal_saturation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(crisis_signal_saturation, news_aggregators).
narrative_ontology:constraint_beneficiary(crisis_signal_saturation, government_agencies).
narrative_ontology:constraint_beneficiary(crisis_signal_saturation, security_firms).
narrative_ontology:constraint_victim(crisis_signal_saturation, general_population).
narrative_ontology:constraint_victim(crisis_signal_saturation, first_responders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of an individual constantly bombarded with alerts, the situation is a Snare. They have limited ability to filter or ignore the constant stream of information, leading to fatigue and desensitization.
constraint_indexing:constraint_classification(crisis_signal_saturation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% First responders are both beneficiaries and victims. They need the alerts to do their jobs, but the constant stream of alarms, many of which are false, leads to fatigue and burnout. They are constrained in their ability to ignore or filter alerts.
constraint_indexing:constraint_classification(crisis_signal_saturation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% News aggregators benefit from increased attention and engagement driven by crisis alerts. They can rapidly arbitrage attention by amplifying signals. They see the system as a coordination mechanism for distributing information quickly.
constraint_indexing:constraint_classification(crisis_signal_saturation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Government agencies benefit from the perceived need for their services created by constant alerts (justifying budgets, expanding powers). However, they are also constrained by the need to respond to these alerts, many of which may be insignificant or false. The volume of alerts may also obscure genuine threats. Mobile due to agency to reallocate resources.
constraint_indexing:constraint_classification(crisis_signal_saturation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% Security firms arbitrage on the perceived insecurity and sell solutions.
constraint_indexing:constraint_classification(crisis_signal_saturation, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical observer sees the situation as a Tangled Rope, a system that has some coordination benefits (rapid dissemination of information) but also significant extraction costs (alarm fatigue, desensitization, wasted resources). The underlying algorithms and incentives are structured to amplify risk and threat, even when the underlying reality is not as severe.
constraint_indexing:constraint_classification(crisis_signal_saturation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(crisis_signal_saturation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(crisis_signal_saturation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(crisis_signal_saturation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(crisis_signal_saturation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(crisis_signal_saturation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The constant stream of alerts extracts attention, energy, and resources from individuals and organizations, reducing their ability to respond effectively to genuine threats. Suppression (0.70): High. The system is structured to amplify risk and threat, making it difficult to escape the constant stream of alerts. The individual's exit options are severely limited. Theater ratio (0.30): Low. The ratio is relatively low due to the system often reporting genuinely new information or events.
 *
 * PERSPECTIVAL GAP:
 *   The general population experiences a constant barrage of alerts, leading to desensitization and a sense of being trapped. First responders are both beneficiaries and victims, needing the alerts but suffering from burnout. News aggregators and government agencies benefit from the increased attention and perceived need for their services, seeing the system as a valuable tool.
 *
 * DIRECTIONALITY LOGIC:
 *   The general population is the primary victim, with limited exit options and a high degree of suppression. News aggregators benefit from the system, leveraging it to drive engagement. Government agencies derive power from a constant state of perceived crisis. First responders are a mixed case, as they rely on the alerts but are also susceptible to alert fatigue.
 *
 * MANDATROPHY ANALYSIS:
 *   While the system appears to be a valuable tool for disseminating information quickly, it also has significant costs in terms of alarm fatigue, desensitization, and wasted resources. It is a Tangled Rope because it has both a genuine coordination function and asymmetric extraction. The system's benefits are not evenly distributed, and the costs are disproportionately borne by individuals and first responders.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    severity_threshold,
    'What is the optimal threshold for triggering a crisis alert to maximize responsiveness while minimizing alarm fatigue?',
    'Controlled experiments measuring response times and false alarm rates at different alert thresholds.',
    'Determines the trade-off between responsiveness and alarm fatigue; affects the overall effectiveness of the warning system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(severity_threshold, empirical, 'Determines the optimal threshold for triggering a crisis alert.').

omega_variable(
    cognitive_desensitization_rate,
    'How quickly do individuals become desensitized to repeated crisis alerts, and what factors influence this desensitization rate?',
    'Longitudinal studies tracking individual responses to crisis alerts over time; analysis of factors like alert frequency, severity, and individual differences.',
    'Influences the long-term effectiveness of the warning system; determines the rate at which individuals become desensitized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_desensitization_rate, empirical, 'Determines how quickly individuals become desensitized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(crisis_signal_saturation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cris_tr_t0, crisis_signal_saturation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cris_tr_t5, crisis_signal_saturation, theater_ratio, 5, 0.2).
narrative_ontology:measurement(cris_tr_t10, crisis_signal_saturation, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(cris_be_t0, crisis_signal_saturation, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(cris_be_t5, crisis_signal_saturation, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(cris_be_t10, crisis_signal_saturation, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(crisis_signal_saturation, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
