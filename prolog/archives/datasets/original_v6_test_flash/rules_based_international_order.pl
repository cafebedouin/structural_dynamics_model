% ============================================================================
% CONSTRAINT STORY: rules_based_international_order
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rules_based_international_order, []).

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
 *   constraint_id: rules_based_international_order
 *   human_readable: The Rules-Based International Order
 *   domain: political/economic
 *
 * SUMMARY:
 *   The rules-based international order is a complex system of institutions
 *   and norms that govern relations between states. It provides a framework
 *   for coordination and cooperation, but also involves power imbalances and
 *   extraction. The system is viewed differently by various actors, depending
 *   on their power, exit options, and structural position.
 *
 * KEY AGENTS:
 *   - Powerful Nations: Primary beneficiaries (institutional/arbitrage) - set the rules and benefit from the system's stability.
 *   - Smaller Nations: Primary victims (powerless/trapped) - subject to the rules set by others with limited ability to influence or exit.
 *   - Challenger States: Secondary victims (moderate/constrained) - experience both benefits and constraints, seeking to alter the existing power dynamics.
 *   - International Corporations: Beneficiaries (institutional/arbitrage) - benefit from predictable markets and cross-border trade.
 *   - Analytical Observer: Analytical view (analytical/analytical) - observes the complex interplay of coordination, extraction, and suppression.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rules_based_international_order, 0.55).
domain_priors:suppression_score(rules_based_international_order, 0.6).
domain_priors:theater_ratio(rules_based_international_order, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rules_based_international_order, extractiveness, 0.55).
narrative_ontology:constraint_metric(rules_based_international_order, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(rules_based_international_order, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rules_based_international_order, tangled_rope).
narrative_ontology:human_readable(rules_based_international_order, "The Rules-Based International Order").
narrative_ontology:topic_domain(rules_based_international_order, "political/economic").

domain_priors:requires_active_enforcement(rules_based_international_order).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rules_based_international_order, powerful_nations).
narrative_ontology:constraint_beneficiary(rules_based_international_order, international_corporations).
narrative_ontology:constraint_victim(rules_based_international_order, smaller_nations).
narrative_ontology:constraint_victim(rules_based_international_order, challenger_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Smaller nations are often trapped within the system, subject to the rules set by more powerful actors. They have limited ability to exit or influence the system, leading to a snare-like experience.
constraint_indexing:constraint_classification(rules_based_international_order, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Challenger states experience the order as both a constraint and an opportunity. They may benefit from certain aspects of the system (e.g., trade), but are also constrained by the existing power structures. They have some agency but face significant barriers to changing the system.
constraint_indexing:constraint_classification(rules_based_international_order, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Powerful nations largely benefit from the rules-based international order, as they often set the rules and can navigate the system to their advantage. They experience the order as a coordination mechanism that facilitates trade, security, and diplomacy. They have arbitrage options within the system.
constraint_indexing:constraint_classification(rules_based_international_order, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% International corporations benefit from the stability and predictability provided by the rules-based international order, facilitating cross-border trade and investment. They have arbitrage options within the system, allowing them to navigate regulations and markets efficiently.
constraint_indexing:constraint_classification(rules_based_international_order, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From an analytical perspective, the rules-based international order is a tangled rope. It provides coordination and stability, but also involves extraction and suppression, particularly for smaller nations and challenger states. The system requires active enforcement to maintain its function.
constraint_indexing:constraint_classification(rules_based_international_order, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rules_based_international_order_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rules_based_international_order, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rules_based_international_order, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rules_based_international_order, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rules_based_international_order_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The system extracts resources and autonomy from smaller nations and challenger states, while benefiting powerful nations and international corporations. Suppression (0.60): Moderate-High. The system suppresses alternative arrangements through diplomatic pressure, economic sanctions, and military intervention. Theater Ratio (0.40): Moderate. The system involves both functional cooperation and performative diplomacy, with a tendency toward more political theater in recent years.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the differing power dynamics within the system. Powerful nations see the system as a rope, providing coordination and stability. Smaller nations often experience the system as a snare, subject to rules they did not create. Challenger states view the system as a tangled rope, both constraining and enabling their actions. The analytical observer sees the system as a complex mix of all three, reflecting the inherent tensions and power imbalances.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the agent's power, exit options, and structural position. Powerful nations with arbitrage options experience low directionality, benefiting from the system. Smaller nations with limited exit options experience high directionality, bearing the costs. Challenger states with constrained exit experience moderate directionality, as they are both constrained and enabled by the system.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the rules-based international order is a multi-faceted phenomenon. It is not simply a rope (pure coordination) or a snare (pure extraction), but a tangled rope that combines both elements. The analytical observer's perspective captures this complexity, acknowledging the genuine benefits of the system while also recognizing the power imbalances and extraction that it involves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_legitimacy,
    'To what extent is the enforcement of the ''rules'' perceived as legitimate and unbiased?',
    'Analysis of UN Security Council resolutions, WTO dispute settlements, and other enforcement mechanisms; surveys of public opinion in different countries.',
    'If enforcement is perceived as biased, the order is more likely to be seen as a snare by weaker actors. If enforcement is perceived as legitimate, the order is more likely to be seen as a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_legitimacy, empirical, 'The perceived legitimacy and unbiasedness of enforcement mechanisms.').

omega_variable(
    rule_adaptability,
    'How adaptable are the ''rules'' to changing global circumstances and power dynamics?',
    'Case studies of specific rules and institutions; analysis of amendment processes and reform efforts.',
    'If the rules are inflexible, the order is more likely to become a piton or a snare. If the rules are adaptable, the order is more likely to remain a tangled rope or evolve into a more equitable system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rule_adaptability, conceptual, 'The adaptability of the rules to changing global circumstances.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rules_based_international_order, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rule_tr_t0, rules_based_international_order, theater_ratio, 0, 0.2).
narrative_ontology:measurement(rule_tr_t20, rules_based_international_order, theater_ratio, 20, 0.4).
narrative_ontology:measurement(rule_tr_t40, rules_based_international_order, theater_ratio, 40, 0.5).

% Extraction over time
narrative_ontology:measurement(rule_be_t0, rules_based_international_order, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(rule_be_t20, rules_based_international_order, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(rule_be_t40, rules_based_international_order, base_extractiveness, 40, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rules_based_international_order, enforcement_mechanism).
narrative_ontology:affects_constraint(rules_based_international_order, global_trade_agreements).
narrative_ontology:affects_constraint(rules_based_international_order, international_security_alliances).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
