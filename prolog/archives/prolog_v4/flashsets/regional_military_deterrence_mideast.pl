% ============================================================================
% CONSTRAINT STORY: regional_military_deterrence_mideast
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regional_military_deterrence_mideast, []).

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
 *   constraint_id: regional_military_deterrence_mideast
 *   human_readable: US/Israeli Military Deterrence Posture against Iran
 *   domain: geopolitical
 *
 * SUMMARY:
 *   The US and Israeli military deterrence posture against Iran involves the
 *   deployment of significant military assets in the Middle East. This is
 *   intended to deter Iran from aggressive actions, but also results in
 *   economic strain on Iran, escalates regional tensions, and requires
 *   substantial financial investment from US taxpayers.
 *
 * KEY AGENTS:
 *   - US Military-Industrial Complex: Primary beneficiary (institutional/arbitrage) - profits from ongoing demand for military hardware and services.
 *   - Israeli Defense Sector: Secondary beneficiary (institutional/constrained) - benefits from US military aid and technology transfers.
 *   - Iranian Economy: Primary victim (powerless/trapped) - suffers from economic sanctions and the threat of military action.
 *   - Regional Stability: Secondary victim (moderate/constrained) - destabilized by escalating tensions and proxy wars.
 *   - US Taxpayers: Tertiary victim (moderate/constrained) - burdened by the financial costs of maintaining the deterrence posture.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regional_military_deterrence_mideast, 0.55).
domain_priors:suppression_score(regional_military_deterrence_mideast, 0.7).
domain_priors:theater_ratio(regional_military_deterrence_mideast, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regional_military_deterrence_mideast, extractiveness, 0.55).
narrative_ontology:constraint_metric(regional_military_deterrence_mideast, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(regional_military_deterrence_mideast, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regional_military_deterrence_mideast, tangled_rope).
narrative_ontology:human_readable(regional_military_deterrence_mideast, "US/Israeli Military Deterrence Posture against Iran").
narrative_ontology:topic_domain(regional_military_deterrence_mideast, "geopolitical").

domain_priors:requires_active_enforcement(regional_military_deterrence_mideast).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regional_military_deterrence_mideast, us_military_industrial_complex).
narrative_ontology:constraint_beneficiary(regional_military_deterrence_mideast, israeli_defense_sector).
narrative_ontology:constraint_victim(regional_military_deterrence_mideast, iranian_economy).
narrative_ontology:constraint_victim(regional_military_deterrence_mideast, regional_stability).
narrative_ontology:constraint_victim(regional_military_deterrence_mideast, us_taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The Iranian economy is trapped by the constant threat of military action and sanctions, limiting its growth and development.
constraint_indexing:constraint_classification(regional_military_deterrence_mideast, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% Regional stability is both supported (by preventing large-scale conflict) and undermined (by escalating tensions and proxy wars) by the deterrence posture. Constrained due to dependence on external actors.
constraint_indexing:constraint_classification(regional_military_deterrence_mideast, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% The US military-industrial complex benefits from the continuous demand for military hardware and services generated by the perceived threat from Iran. Arbitrage due to global reach.
constraint_indexing:constraint_classification(regional_military_deterrence_mideast, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% US Taxpayers are constrained to fund the ongoing military expenditures but also benefit from the perceived safety and security provided by the deterrence posture.
constraint_indexing:constraint_classification(regional_military_deterrence_mideast, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical observer sees the deterrence posture as a complex system with both stabilizing and destabilizing effects, benefiting some actors while extracting from others.
constraint_indexing:constraint_classification(regional_military_deterrence_mideast, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regional_military_deterrence_mideast_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regional_military_deterrence_mideast, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regional_military_deterrence_mideast, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regional_military_deterrence_mideast, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(regional_military_deterrence_mideast_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score of 0.55 reflects the economic and political costs imposed on Iran and the region. The suppression score of 0.70 indicates the limited options available to Iran and other regional actors to challenge the deterrence posture. The theater_ratio reflects that the deterrence posture also involves symbolic displays of force and political signaling.
 *
 * PERSPECTIVAL GAP:
 *   The Iranian economy views the deterrence posture as a snare, trapping it in a cycle of economic hardship and political isolation. The US military-industrial complex sees it as a rope, providing a continuous stream of revenue and influence. Regional stability experiences the deterrence as a tangled rope, both preventing large-scale conflict and fueling proxy wars. US Taxpayers see a similar tangled rope, paying for what is supposedly a security measure.
 *
 * DIRECTIONALITY LOGIC:
 *   The US Military Industrial Complex and Israeli defense sector benefit, exhibiting low d values. Iran is trapped, exhibiting a high d value. Regional stability is constrained, with an intermediate d value. US taxpayers are similarly constrained, with an intermediate d value.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    iran_nuclear_intent,
    'What is Iran''s true intention regarding nuclear weapons development?',
    'Improved intelligence gathering, verification of nuclear sites, diplomatic negotiations.',
    'If Iran intends to develop nuclear weapons: Deterrence is more justified. If Iran does not: Deterrence is less necessary and more extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iran_nuclear_intent, empirical, 'Iran''s true intention regarding nuclear weapons.').

omega_variable(
    proxy_war_threshold,
    'What level of proxy war activity constitutes unacceptable regional destabilization?',
    'Establish clear metrics for measuring proxy war activity and regional stability.',
    'If threshold is low: Deterrence is seen as failing to prevent proxy wars. If threshold is high: Deterrence is seen as more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_war_threshold, conceptual, 'Level of proxy war activity constituting unacceptable regional destabilization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regional_military_deterrence_mideast, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regi_tr_t0, regional_military_deterrence_mideast, theater_ratio, 0, 0.3).
narrative_ontology:measurement(regi_tr_t5, regional_military_deterrence_mideast, theater_ratio, 5, 0.4).
narrative_ontology:measurement(regi_tr_t10, regional_military_deterrence_mideast, theater_ratio, 10, 0.5).

% Extraction over time
narrative_ontology:measurement(regi_be_t0, regional_military_deterrence_mideast, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(regi_be_t5, regional_military_deterrence_mideast, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(regi_be_t10, regional_military_deterrence_mideast, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regional_military_deterrence_mideast, enforcement_mechanism).
narrative_ontology:affects_constraint(regional_military_deterrence_mideast, iran_nuclear_agreement).
narrative_ontology:affects_constraint(regional_military_deterrence_mideast, regional_arms_race).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
