% ============================================================================
% CONSTRAINT STORY: cascading_uncertainty_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cascading_uncertainty_2026, []).

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
 *   constraint_id: cascading_uncertainty_2026
 *   human_readable: The Sovereignty Gap (Cascading Uncertainty)
 *   domain: political/social/geopolitical
 *
 * SUMMARY:
 *   As of Feb 5, 2026, a "Cascading Uncertainty" has emerged from the
 *   simultaneous expiration of New START and a domestic enforcement crisis.
 *   This constraint manifests as a sovereignty gap, leading to reduced
 *   transparency, increased risk of nuclear conflict, and a degradation of
 *   arms control institutions. Some actors, such as rogue states and arms
 *   manufacturers, may benefit from this uncertainty, while others, like
 *   global security and non-proliferation treaties, are placed at risk. The
 *   constraint requires active enforcement to mitigate these risks.
 *
 * KEY AGENTS:
 *   - Global Security: Primary victim (powerless/trapped)
 *   - Non-Proliferation Treaties: Secondary victim (moderate/constrained)
 *   - Rogue States: Primary beneficiary (institutional/arbitrage)
 *   - Arms Manufacturers: Secondary beneficiary (powerful/arbitrage)
 *   - Arms Control Institutions: Institutional Actor (institutional/constrained)
 *   - Analytical Observer: Analytical perspective (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cascading_uncertainty_2026, 0.6).
domain_priors:suppression_score(cascading_uncertainty_2026, 0.7).
domain_priors:theater_ratio(cascading_uncertainty_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cascading_uncertainty_2026, extractiveness, 0.6).
narrative_ontology:constraint_metric(cascading_uncertainty_2026, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(cascading_uncertainty_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cascading_uncertainty_2026, tangled_rope).
narrative_ontology:human_readable(cascading_uncertainty_2026, "The Sovereignty Gap (Cascading Uncertainty)").
narrative_ontology:topic_domain(cascading_uncertainty_2026, "political/social/geopolitical").

domain_priors:requires_active_enforcement(cascading_uncertainty_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cascading_uncertainty_2026, rogue_states).
narrative_ontology:constraint_beneficiary(cascading_uncertainty_2026, arms_manufacturers).
narrative_ontology:constraint_victim(cascading_uncertainty_2026, global_security).
narrative_ontology:constraint_victim(cascading_uncertainty_2026, non_proliferation_treaties).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Global Security (Snare): With the expiration of New START and domestic enforcement crisis, global security becomes trapped in a system with reduced transparency and increased risk of nuclear conflict. This perspective has no exit option and bears the full cost of the uncertainty.
constraint_indexing:constraint_classification(cascading_uncertainty_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Non-Proliferation Treaties (Tangled Rope): These treaties are constrained by the uncertainty, as verification becomes more difficult. However, they also benefit from increased attention and potential strengthening due to the perceived threat. There is asymmetric extraction, as the treaties are being undermined, but cannot fully exit.
constraint_indexing:constraint_classification(cascading_uncertainty_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Rogue States (Rope): These states may benefit from the uncertainty, as it provides an opportunity to pursue their own agendas without as much oversight. This perspective sees the constraint as enabling coordination for their strategic objectives.
constraint_indexing:constraint_classification(cascading_uncertainty_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Arms Control Institutions (Piton): These institutions may attempt to maintain a theatrical facade of control and verification, even though their actual effectiveness is diminished. The system is degraded, but maintained for performative reasons.
constraint_indexing:constraint_classification(cascading_uncertainty_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% Analytical Observer (Tangled Rope): The analytical perspective sees both coordination and extraction. The uncertainty leads to increased global instability and risk, while some actors benefit from the lack of oversight. The system requires active enforcement to mitigate the risks.
constraint_indexing:constraint_classification(cascading_uncertainty_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Arms Manufacturers (Rope): These groups stand to benefit from increased demand for weapons and military technology due to heightened global tensions, even in the short term. This creates a rope like effect, as the increased demand aids in production.
constraint_indexing:constraint_classification(cascading_uncertainty_2026, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cascading_uncertainty_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cascading_uncertainty_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cascading_uncertainty_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cascading_uncertainty_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cascading_uncertainty_2026, TR),
    TR >= 0.70.

:- end_tests(cascading_uncertainty_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): Moderate-high. The uncertainty allows rogue states to pursue their agendas with less oversight, resulting in a transfer of power and resources away from established institutions. Suppression (0.70): High. The lack of enforcement mechanisms creates a coercive environment, limiting the ability of states to ensure compliance with arms control agreements. Theater ratio (0.30): Low. The reduced transparency and increased risk of conflict lead to reduced confidence in theatrical performances.
 *
 * PERSPECTIVAL GAP:
 *   Global security is trapped in a snare, while non-proliferation treaties are in a tangled rope. Rogue states and arms manufacturers both benefit from the situation, as seen with a rope. Arms Control Institutions are degraded into a piton. The analytical perspective sees a tangled rope with both coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Rogue states and arms manufacturers benefit from lack of constraint, while global security and arms-control treaties are constrained by lack of enforcement. Arms control institutions are degraded into a mere theatrical display.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as a tangled rope to correctly represent the combination of coordination and extraction. While some actors benefit from the uncertainty (Rogue States, Arms Manufacturers), this benefit comes at the cost of increased global instability and risk (Global Security, Non-Proliferation Treaties).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_mechanism_viability,
    'Can effective international enforcement mechanisms be re-established?',
    'Diplomatic negotiations and agreements',
    'If viable, the constraint becomes a scaffold. If not, it becomes a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_viability, empirical, 'Determine if enforcement mechanisms can be viable').

omega_variable(
    rogue_state_restraint,
    'Will rogue states exercise restraint in their actions?',
    'Monitoring state behavior and intentions',
    'If states exercise restraint, the extraction is reduced. If not, extraction is amplified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rogue_state_restraint, empirical, 'Assess the level of rogue state restraint.').

omega_variable(
    treaty_modernization_scope,
    'Can treaties be modernized to account for new technologies and strategic realities?',
    'Legal scholarship and diplomatic efforts to modernize treaty law.',
    'If treaties can be modernized, this constraint turns into a rope. If not, this constraint is more of a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_modernization_scope, conceptual, 'Assess treaty modernization scope and scale').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cascading_uncertainty_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(casc_tr_t0, cascading_uncertainty_2026, theater_ratio, 0, 0.4).
narrative_ontology:measurement(casc_tr_t5, cascading_uncertainty_2026, theater_ratio, 5, 0.35).
narrative_ontology:measurement(casc_tr_t10, cascading_uncertainty_2026, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(casc_be_t0, cascading_uncertainty_2026, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(casc_be_t5, cascading_uncertainty_2026, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(casc_be_t10, cascading_uncertainty_2026, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cascading_uncertainty_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(cascading_uncertainty_2026, proliferation_risks_new_tech).
narrative_ontology:affects_constraint(cascading_uncertainty_2026, great_power_competition_2026).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
