% ============================================================================
% CONSTRAINT STORY: us_iran_drone_conflict
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_iran_drone_conflict, []).

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
 *   constraint_id: us_iran_drone_conflict
 *   human_readable: US-Iran Drone Conflict and Escalation
 *   domain: political
 *
 * SUMMARY:
 *   The US-Iran drone conflict represents an escalating series of incidents
 *   involving drone operations, military posturing, and regional power
 *   projection. This conflict primarily occurs in the airspace over Syria,
 *   Iraq and surrounding regions, with each side accusing the other of
 *   provocative actions that threaten their interests and regional stability.
 *   The US claims to be protecting its forces and allies, while Iran asserts
 *   its right to operate in the region and support its allies.
 *
 * KEY AGENTS:
 *   - US Military Industrial Complex: Primary beneficiary (institutional/arbitrage) - benefits from increased military spending and arms sales.
 *   - Iranian Revolutionary Guard: Primary beneficiary (institutional/arbitrage) - benefits from projecting power and influence in the region.
 *   - Regional Stability: Primary victim (powerless/trapped) - undermined by escalating tensions and potential for full-scale conflict.
 *   - Syrian Civilians: Secondary victim (powerless/trapped) - bear the brunt of drone strikes and instability.
 *   - International Shipping: Secondary victim (moderate/constrained) - vulnerable to attacks and disruptions in the region.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_iran_drone_conflict, 0.65).
domain_priors:suppression_score(us_iran_drone_conflict, 0.7).
domain_priors:theater_ratio(us_iran_drone_conflict, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_iran_drone_conflict, extractiveness, 0.65).
narrative_ontology:constraint_metric(us_iran_drone_conflict, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(us_iran_drone_conflict, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_iran_drone_conflict, tangled_rope).
narrative_ontology:human_readable(us_iran_drone_conflict, "US-Iran Drone Conflict and Escalation").
narrative_ontology:topic_domain(us_iran_drone_conflict, "political").

domain_priors:requires_active_enforcement(us_iran_drone_conflict).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_iran_drone_conflict, us_military_industrial_complex).
narrative_ontology:constraint_beneficiary(us_iran_drone_conflict, iranian_revolutionary_guard).
narrative_ontology:constraint_victim(us_iran_drone_conflict, regional_stability).
narrative_ontology:constraint_victim(us_iran_drone_conflict, syrian_civilians).
narrative_ontology:constraint_victim(us_iran_drone_conflict, international_shipping).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Syrian civilians are trapped in the conflict zone and bear the brunt of the drone strikes and escalating tensions. They have no exit options and are powerless to stop the conflict.
constraint_indexing:constraint_classification(us_iran_drone_conflict, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% Regional stability is constrained by the conflict. While some actors may benefit from instability, overall, the escalating tensions and drone strikes undermine long-term stability.
constraint_indexing:constraint_classification(us_iran_drone_conflict, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% The US military-industrial complex benefits from the conflict through increased arms sales and military spending. They have arbitrage options by shifting focus to other regions if the conflict de-escalates.
constraint_indexing:constraint_classification(us_iran_drone_conflict, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% The Iranian Revolutionary Guard benefits from the conflict by projecting power and influence in the region. They have arbitrage options to pursue other regional goals if this particular conflict subsides.
constraint_indexing:constraint_classification(us_iran_drone_conflict, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% An analytical observer sees the conflict as a tangled rope. There's coordination in the sense that each side's actions are responses to the other, but it leads to destructive asymmetric extraction and undermines global security.
constraint_indexing:constraint_classification(us_iran_drone_conflict, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_iran_drone_conflict_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_iran_drone_conflict, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_iran_drone_conflict, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_iran_drone_conflict, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_iran_drone_conflict_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.65 - High. The conflict extracts resources and stability from the region, with significant costs to civilian populations and regional security. Suppression: 0.70 - High. Both the US and Iran actively suppress alternative narratives and actions, creating a highly constrained environment. Theater Ratio: 0.40 - Moderate. While there is some element of performative posturing, the conflict also involves real military actions and consequences. The US and Iran reinforce their claims through active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is driven by the divergent interests and goals of the actors involved. The US and Iran see their actions as defensive and necessary, while regional actors and the international community perceive the conflict as destabilizing and dangerous. This difference in perspective fuels the conflict and makes de-escalation difficult.
 *
 * DIRECTIONALITY LOGIC:
 *   The US military-industrial complex and the Iranian Revolutionary Guard are classified as beneficiaries with arbitrage options, as they both gain from the conflict and can shift focus to other areas if necessary. Regional stability and Syrian civilians are classified as victims with limited or no exit options, as they are directly harmed by the conflict and cannot easily escape its consequences.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a Tangled Rope because both sides are engaging in a form of costly signaling to demonstrate resolve and protect their interests. There's a coordination failure at play, as each side's actions escalate the conflict despite the long-term negative consequences for regional stability. The classification prevents mislabeling coordination as pure extraction by acknowledging that both sides have strategic incentives for their behavior, even if the outcome is mutually destructive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_attribution_accuracy,
    'How accurately can US and Iranian forces attribute intent to each other''s drone operations?',
    'Independent analysis of drone flight paths, communication intercepts, and incident reports.',
    'If intent is frequently misattributed, escalation becomes more likely. If intent is accurately attributed, de-escalation is more likely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_attribution_accuracy, empirical, 'Accuracy of intent attribution in drone operations').

omega_variable(
    regional_proxy_control,
    'To what extent can the US and Iran control the actions of their regional proxies?',
    'Analysis of funding flows, communication channels, and proxy group behavior.',
    'If proxies are highly autonomous, the conflict can escalate beyond the control of the US and Iran. If proxies are tightly controlled, de-escalation is more likely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_proxy_control, empirical, 'Level of control over regional proxies').

omega_variable(
    domestic_political_imperatives,
    'How much are US and Iranian actions driven by domestic political considerations?',
    'Analysis of political discourse, public opinion polls, and elite statements.',
    'If domestic politics are a major driver, rational de-escalation becomes more difficult. If domestic politics are less important, compromise is more likely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_political_imperatives, conceptual, 'Influence of domestic politics on conflict dynamics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_iran_drone_conflict, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_i_tr_t0, us_iran_drone_conflict, theater_ratio, 0, 0.2).
narrative_ontology:measurement(us_i_tr_t5, us_iran_drone_conflict, theater_ratio, 5, 0.3).
narrative_ontology:measurement(us_i_tr_t10, us_iran_drone_conflict, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(us_i_be_t0, us_iran_drone_conflict, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(us_i_be_t5, us_iran_drone_conflict, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(us_i_be_t10, us_iran_drone_conflict, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_iran_drone_conflict, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
