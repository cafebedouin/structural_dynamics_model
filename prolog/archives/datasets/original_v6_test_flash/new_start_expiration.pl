% ============================================================================
% CONSTRAINT STORY: new_start_expiration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_new_start_expiration, []).

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
 *   constraint_id: new_start_expiration
 *   human_readable: New START Treaty Expiration
 *   domain: political
 *
 * SUMMARY:
 *   The expiration of the New START treaty, which limited US and Russian
 *   nuclear arsenals, creates a new constraint landscape. While specific
 *   sectors might benefit, this expiration increases global instability and
 *   erodes the non-proliferation regime. The lack of verifiable limits allows
 *   for an unrestricted build-up of nuclear arsenals, increasing the risk of
 *   miscalculation and potentially initiating a new arms race. Various actors
 *   perceive and experience this constraint differently, leading to multiple
 *   DR classifications.
 *
 * KEY AGENTS:
 *   - Global Strategic Stability: Powerless/Trapped - suffers the negative consequences of increased nuclear risk.
 *   - Non-Proliferation Regime: Moderate/Constrained - its effectiveness is diminished.
 *   - Military-Industrial Complex (US): Institutional/Arbitrage - benefits from increased defense spending.
 *   - Military-Industrial Complex (Russia): Institutional/Arbitrage - benefits from increased defense spending.
 *   - Arms Control Institutions: Institutional/Constrained - international bodies' influence wanes.
 *   - Analytical Observer: Analytical/Analytical - assesses the mixed impact on global security.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(new_start_expiration, 0.6).
domain_priors:suppression_score(new_start_expiration, 0.7).
domain_priors:theater_ratio(new_start_expiration, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(new_start_expiration, extractiveness, 0.6).
narrative_ontology:constraint_metric(new_start_expiration, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(new_start_expiration, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(new_start_expiration, tangled_rope).
narrative_ontology:human_readable(new_start_expiration, "New START Treaty Expiration").
narrative_ontology:topic_domain(new_start_expiration, "political").

domain_priors:requires_active_enforcement(new_start_expiration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(new_start_expiration, military_industrial_complex_us).
narrative_ontology:constraint_beneficiary(new_start_expiration, military_industrial_complex_russia).
narrative_ontology:constraint_victim(new_start_expiration, global_strategic_stability).
narrative_ontology:constraint_victim(new_start_expiration, non_proliferation_regime).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Global strategic stability is trapped and powerless in the face of increased nuclear risk. It bears the full cost of potential miscalculation and escalation.
constraint_indexing:constraint_classification(new_start_expiration, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% The non-proliferation regime is constrained. While it aims to limit nuclear spread, it is weakened by the absence of the treaty, and must expend resources to monitor and adapt to the new environment.
constraint_indexing:constraint_classification(new_start_expiration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The US military-industrial complex benefits from increased defense spending and modernization efforts, which can be justified by the absence of treaty limits, giving them arbitrage opportunities in resource allocation.
constraint_indexing:constraint_classification(new_start_expiration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% The Russian military-industrial complex similarly benefits from the expiration of the treaty, as it allows for the modernization and expansion of their nuclear arsenal, creating arbitrage opportunities in resource allocation.
constraint_indexing:constraint_classification(new_start_expiration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% International arms control institutions, like the UN disarmament bodies, find themselves in a degraded state. While they still exist, their effectiveness is reduced without the framework of the treaty. They are constrained by political realities and lack real enforcement power.
constraint_indexing:constraint_classification(new_start_expiration, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% An analytical observer sees the expiration as a tangled rope – a mix of coordination failure and extraction, where the benefits to specific national actors come at the cost of increased global instability and risk.
constraint_indexing:constraint_classification(new_start_expiration, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(new_start_expiration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(new_start_expiration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(new_start_expiration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(new_start_expiration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(new_start_expiration, TR),
    TR >= 0.70.

:- end_tests(new_start_expiration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is assessed at 0.60 because the expiration leads to an extraction of stability and security from the global system. Suppression is 0.70 due to the limited options for smaller states or international institutions to influence the US and Russia's nuclear policies. The theater ratio is 0.30, as there is real, functional increase in military activity and strategic planning, rather than performative actions.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the divergent experiences of different actors. The military-industrial complexes in the US and Russia see the treaty's end as an opportunity for expansion, benefiting from increased spending and modernization. Global strategic stability, however, is negatively affected, experiencing a snare. International arms control institutions are constrained, resulting in a piton classification. The analytical observer sees a complex mix of effects, highlighting a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality logic stems from the structural relationships. The military-industrial complexes benefit (low d) and the global security environment suffers (high d). Institutions like arms control bodies are constrained but not entirely powerless, giving them moderate d values. The analytical perspective aims to capture the overall net effect. The structural positions determine the experienced extractiveness for each actor.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction by explicitly identifying the beneficiaries and victims. While the military-industrial complexes might argue they are enhancing national security, the broader impact on global stability is a net extraction. Similarly, it avoids mislabeling extraction as coordination by recognizing that the build-up of nuclear arsenals does not serve the collective good, and the beneficiaries' interests are not aligned with global stability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_mechanism_viability,
    'Can alternative verification mechanisms be established to replace the New START treaty''s inspection regime?',
    'Development and deployment of new monitoring technologies and cooperative verification arrangements.',
    'If viable: reduces the extraction by limiting uncertainty. If not viable: increases extraction due to lack of transparency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_mechanism_viability, empirical, 'Whether alternative verification mechanisms can be established.').

omega_variable(
    political_will_for_renewal,
    'Will there be sufficient political will to negotiate a new arms control agreement in the near future?',
    'Shifts in domestic politics and geopolitical relations influencing the likelihood of treaty negotiation.',
    'If yes: extraction is temporary, leading to a scaffold scenario. If no: extraction becomes entrenched, solidifying a snare scenario.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_will_for_renewal, preference, 'Future political will for arms control negotiations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(new_start_expiration, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(new__tr_t0, new_start_expiration, theater_ratio, 0, 0.2).
narrative_ontology:measurement(new__tr_t5, new_start_expiration, theater_ratio, 5, 0.3).
narrative_ontology:measurement(new__tr_t10, new_start_expiration, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(new__be_t0, new_start_expiration, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(new__be_t5, new_start_expiration, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(new__be_t10, new_start_expiration, base_extractiveness, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(new_start_expiration, enforcement_mechanism).
narrative_ontology:affects_constraint(new_start_expiration, nuclear_proliferation).
narrative_ontology:affects_constraint(new_start_expiration, great_power_competition).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
