% ============================================================================
% CONSTRAINT STORY: authoritarian_power_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_authoritarian_power_paradox, []).

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
 *   constraint_id: authoritarian_power_paradox
 *   human_readable: The Paradox of Authoritarian Centralization
 *   domain: political/social/technological
 *
 * SUMMARY:
 *   The paradox of authoritarian power describes a state where an institution
 *   or leader increases centralization to ensure stability but, in doing so,
 *   suppresses the diagnostic sensors required to perceive threats. This
 *   leads to a system where the leadership becomes increasingly isolated from
 *   reality, making poor decisions and ultimately undermining its own power.
 *   The constraint operates across political, social, and technological
 *   domains, manifesting in different forms but with the same underlying
 *   dynamic.
 *
 * KEY AGENTS:
 *   - Authoritarian Leadership: Primary beneficiary (institutional/arbitrage) - benefits from centralized control and resource allocation.
 *   - Elite Inner Circle: Secondary beneficiary (institutional/arbitrage) - gains preferential access to resources and power.
 *   - Local Populations: Primary victim (powerless/trapped) - suffers from suppression of dissent and extraction of resources.
 *   - Dissenting Voices: Secondary victim (moderate/constrained) - faces suppression but can exert some resistance.
 *   - Institutional Sensors: Intelligence, regulatory, and audit agencies (institutional/constrained) - formerly functional roles degrade to theatrical performances.
 *   - Analytical Observer: Global/Historical perspective (analytical/analytical) - observes the system dynamics and paradox.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(authoritarian_power_paradox, 0.6).
domain_priors:suppression_score(authoritarian_power_paradox, 0.7).
domain_priors:theater_ratio(authoritarian_power_paradox, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(authoritarian_power_paradox, extractiveness, 0.6).
narrative_ontology:constraint_metric(authoritarian_power_paradox, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(authoritarian_power_paradox, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(authoritarian_power_paradox, tangled_rope).
narrative_ontology:human_readable(authoritarian_power_paradox, "The Paradox of Authoritarian Centralization").
narrative_ontology:topic_domain(authoritarian_power_paradox, "political/social/technological").

domain_priors:requires_active_enforcement(authoritarian_power_paradox).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(authoritarian_power_paradox, authoritarian_leadership).
narrative_ontology:constraint_beneficiary(authoritarian_power_paradox, elite_inner_circle).
narrative_ontology:constraint_victim(authoritarian_power_paradox, local_populations).
narrative_ontology:constraint_victim(authoritarian_power_paradox, dissenting_voices).
narrative_ontology:constraint_victim(authoritarian_power_paradox, institutional_sensors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of the oppressed local population, who are trapped and powerless under the authoritarian regime. They experience the system as a snare due to the high suppression of dissent and extraction of resources.
constraint_indexing:constraint_classification(authoritarian_power_paradox, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective of dissenting intellectuals or organized opposition who are constrained but not entirely powerless. They experience the system as a tangled rope, facing suppression but also able to exert some influence or resistance.
constraint_indexing:constraint_classification(authoritarian_power_paradox, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective of the authoritarian leadership who benefit from the centralized power. They perceive the system as a rope, enabling efficient control and resource allocation, with some inherent risks of oversight due to overconfidence. Exits exist via maintained state power.
constraint_indexing:constraint_classification(authoritarian_power_paradox, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective of institutional sensors and lower-level intelligence apparatus. With constrained exit they may experience a piton dynamic, where former rope aspects now function in a degraded, theatrical manner because centralization diminishes effectiveness.
constraint_indexing:constraint_classification(authoritarian_power_paradox, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% An analytical observer sees the authoritarian system as a tangled rope, recognizing the coordination benefits for the leadership but also the asymmetric extraction and suppression of diagnostic capacity. The paradox is clear at this level.
constraint_indexing:constraint_classification(authoritarian_power_paradox, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(authoritarian_power_paradox_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(authoritarian_power_paradox, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(authoritarian_power_paradox, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(authoritarian_power_paradox, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(authoritarian_power_paradox, TR),
    TR >= 0.70.

:- end_tests(authoritarian_power_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): High. The regime extracts resources and compliance from the population, suppressing dissent and independent action. Suppression (0.7): High. Freedom of speech, assembly, and association are severely restricted. Theater Ratio (0.3): Moderate-Low. Some performative functions exist (e.g., propaganda), but the regime primarily focuses on maintaining control through force and repression. The claimed type is Tangled Rope because the regime initially provides coordination benefits but progressively undermines its effectiveness through suppression. The analytical observer can detect this transition over time.
 *
 * PERSPECTIVAL GAP:
 *   The paradox manifests as a perspectival gap. The leadership sees a rope - a tool for efficient governance. The local population experiences a snare - complete subjugation. Dissenting intellectuals see a tangled rope, able to resist to some degree. The analytical observer sees the full tangled rope dynamic, including the long-term risks of over-centralization.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (leadership, inner circle) experience coordination (rope) because centralization concentrates resources for projects they value. Victims (local population, dissenting voices) experience asymmetric extraction (snare) because dissent and autonomy are actively suppressed. The analytical perspective captures both aspects, leading to a tangled rope classification. The degraded institutional sensors represent a piton dynamic, where past efficiency has been lost to suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   This resolves the mandatrophy by demonstrating how a system initially designed for coordination (rope) can degrade into a tool of pure extraction (snare) due to over-centralization. The analytical perspective (tangled rope) captures the inherent instability of this dynamic. Mislabeling the system as a pure rope would ignore the lived experience of the oppressed. The piton perspective for institutional sensors confirms the erosion of functional utility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regime_stability_threshold,
    'What level of suppression can an authoritarian regime maintain without triggering widespread revolt or collapse?',
    'Historical analysis of authoritarian regimes and their failure modes, correlation between suppression levels and stability.',
    'If threshold is low: even moderate suppression leads to instability. If threshold is high: regime can withstand significant dissent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_stability_threshold, empirical, 'Suppression threshold for regime stability.').

omega_variable(
    information_fidelity_cutoff,
    'At what point does centralized control degrade the fidelity of information reaching the leadership, hindering effective decision-making?',
    'Case studies of authoritarian decision-making, analysis of information flows within centralized systems.',
    'If cutoff is low: centralization quickly becomes detrimental. If cutoff is high: centralization remains effective for longer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_fidelity_cutoff, empirical, 'Information fidelity cutoff for effective centralization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(authoritarian_power_paradox, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(auth_tr_t0, authoritarian_power_paradox, theater_ratio, 0, 0.1).
narrative_ontology:measurement(auth_tr_t5, authoritarian_power_paradox, theater_ratio, 5, 0.2).
narrative_ontology:measurement(auth_tr_t10, authoritarian_power_paradox, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(auth_be_t0, authoritarian_power_paradox, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(auth_be_t5, authoritarian_power_paradox, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(auth_be_t10, authoritarian_power_paradox, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(authoritarian_power_paradox, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
