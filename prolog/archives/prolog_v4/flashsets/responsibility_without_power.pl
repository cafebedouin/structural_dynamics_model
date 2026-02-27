% ============================================================================
% CONSTRAINT STORY: responsibility_without_power
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_responsibility_without_power, []).

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
 *   constraint_id: responsibility_without_power
 *   human_readable: The Scapegoat Architecture
 *   domain: organizational/legal/socio-economic
 *
 * SUMMARY:
 *   The Scapegoat Architecture describes a scenario where responsibility for
 *   maintaining system safety or ethical standards is assigned to a subject
 *   who lacks the actual power or tools to control the outcome. This is often
 *   observed in automated systems where liability for errors falls on
 *   individuals who can't understand or modify the algorithms, in supply
 *   chains where compliance officers are accountable for the actions of
 *   distant subcontractors, or in middle management where performance targets
 *   are assigned without necessary resources.
 *
 * KEY AGENTS:
 *   - Executive Management: Benefits by assigning responsibility (institutional/arbitrage).
 *   - System Owners: Benefits by offloading liability (institutional/arbitrage).
 *   - Middle Management: Victim of responsibility without power (moderate/constrained).
 *   - Compliance Officers: Victim, responsible without power (moderate/constrained).
 *   - Designated Scapegoat: Bears the cost of system failure (powerless/trapped).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(responsibility_without_power, 0.55).
domain_priors:suppression_score(responsibility_without_power, 0.7).
domain_priors:theater_ratio(responsibility_without_power, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(responsibility_without_power, extractiveness, 0.55).
narrative_ontology:constraint_metric(responsibility_without_power, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(responsibility_without_power, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(responsibility_without_power, tangled_rope).
narrative_ontology:human_readable(responsibility_without_power, "The Scapegoat Architecture").
narrative_ontology:topic_domain(responsibility_without_power, "organizational/legal/socio-economic").

domain_priors:requires_active_enforcement(responsibility_without_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(responsibility_without_power, executive_management).
narrative_ontology:constraint_beneficiary(responsibility_without_power, system_owners).
narrative_ontology:constraint_victim(responsibility_without_power, middle_management).
narrative_ontology:constraint_victim(responsibility_without_power, compliance_officers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The individual or team assigned responsibility but lacking control experiences this as a pure extraction mechanism. They are trapped because their job depends on accepting this responsibility, even if they know they cannot effectively fulfill it.
constraint_indexing:constraint_classification(responsibility_without_power, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Middle management experiences the constraint as a Tangled Rope. They have some ability to influence outcomes, but their power is limited. They benefit from the framework by having defined duties and a chain of command, but extraction occurs through legal or career repercussions when systems fail despite their best efforts.
constraint_indexing:constraint_classification(responsibility_without_power, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Executive management views the scapegoat architecture as a 'rope' because they gain from it by assigning responsibility which in turn protects them from liability and increases public trust. They can exit this arrangement if the 'scapegoat' is not performing to the expected standards.
constraint_indexing:constraint_classification(responsibility_without_power, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The legal system's approach to assigning liability to entities without power is a piton. Its original function may have been fair, but now it's primarily theatrical - fulfilling a need to assign blame without necessarily improving outcomes. Constrained because it cannot exit assigning responsibility without proper power because it's the backbone of law itself.
constraint_indexing:constraint_classification(responsibility_without_power, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% Analytical observer: Sees this as a tangled rope. On the one hand, a formal assignment of responsibility can drive improvements in security or safety. On the other hand, delegating legal or moral responsibility to those without the power to meet such responsibilities can be a source of perverse incentives and structural brittleness.
constraint_indexing:constraint_classification(responsibility_without_power, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(responsibility_without_power_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(responsibility_without_power, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(responsibility_without_power, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(responsibility_without_power, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(responsibility_without_power, TR),
    TR >= 0.70.

:- end_tests(responsibility_without_power_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. This setup extracts from middle management and compliance officers, who are penalized when things go wrong. Suppression (0.70): High. The framework maintains its dominance because alternatives are suppressed by legal tradition and organizational structures that prioritize clear lines of accountability, even if those lines don't correspond to actual control. Theater Ratio (0.60): Moderate. Assigning responsibility to someone, even without power, creates the appearance of accountability, improving public trust and mitigating legal risk.
 *
 * PERSPECTIVAL GAP:
 *   Executive management views this system as a rope, providing a solution to complex problems. In contrast, designated scapegoats experience pure extraction. The analytical observer sees that this dynamic is a tangled rope, with some coordinating effects coupled to perverse incentives.
 *
 * DIRECTIONALITY LOGIC:
 *   Executive management's directionality is driven by their arbitrage exit, as they are protected by assigning liability to someone lower. The designed scapegoat has a trapped exit, causing a high directionality. An analytical observer views this as a tangled rope since while there is extracted responsibilities, there are some good outcomes of this setup. All of this is a tangled rope.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    power_to_influence,
    'What level of power is required to effectively influence system outcomes related to safety/ethics?',
    'Quantitative analysis of system performance vs. power level of responsible party.',
    'If the necessary power threshold is high, the ''responsibility without power'' framework is more extractive. If it''s low, it can function as a genuine coordinating mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(power_to_influence, empirical, 'What power is required to influence outcomes?').

omega_variable(
    moral_hazard,
    'Does assigning responsibility without power create a moral hazard, where decision-makers are less incentivized to invest in safety or ethical systems?',
    'Comparative case studies and surveys to gauge the impact on strategic investment.',
    'If moral hazard is significant, the framework is net-negative. Otherwise, it may have some limited benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_hazard, empirical, 'Does it create a moral hazard?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(responsibility_without_power, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(resp_tr_t0, responsibility_without_power, theater_ratio, 0, 0.4).
narrative_ontology:measurement(resp_tr_t5, responsibility_without_power, theater_ratio, 5, 0.5).
narrative_ontology:measurement(resp_tr_t10, responsibility_without_power, theater_ratio, 10, 0.6).

% Extraction over time
narrative_ontology:measurement(resp_be_t0, responsibility_without_power, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(resp_be_t5, responsibility_without_power, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(resp_be_t10, responsibility_without_power, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(responsibility_without_power, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
