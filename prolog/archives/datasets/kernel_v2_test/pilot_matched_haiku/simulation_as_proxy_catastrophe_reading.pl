% ============================================================================
% CONSTRAINT STORY: simulation_as_proxy_catastrophe_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simulation_as_proxy_catastrophe_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: simulation_as_proxy_catastrophe_reading
 *   human_readable: Simulation as Proxy Catastrophe: Competence Maintenance Through Controlled Practice
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   catastrophe_proxy_sufficiency: the
 *   simulation_as_proxy_catastrophe_reading. This reading asserts that
 *   simulation exercises constitute catastrophe-equivalent practice
 *   sufficient to maintain operational competence indefinitely. The
 *   constraint operates in high-reliability organizations (nuclear power,
 *   aviation, emergency response) where actual catastrophes are rare but
 *   competence maintenance is critical. The reading claims that controlled
 *   simulation can substitute for catastrophe-level learning, enabling
 *   organizations to maintain competence without bearing catastrophic costs.
 *   This reading coexists with three sibling readings:
 *   catastrophe_necessity_reading (catastrophe experience is irreplaceable),
 *   hybrid_degradation_reading (simulation is partially sufficient but
 *   degrades over time), and simulation_fidelity_threshold (sufficiency
 *   depends on achieving specific fidelity levels). The structural delta
 *   between this reading and its siblings is low extractiveness (simulation
 *   is coordination mechanism), beneficiary is regulatory bodies (liability
 *   protection via competence verification without catastrophe), and no
 *   victim set (competence is maintained). However, the measurement
 *   trajectory shows rising theater_ratio (0.35 → 0.62) and rising
 *   extractiveness (0.15 → 0.32), suggesting that the constraint is drifting
 *   toward Tangled Rope or Snare as simulation becomes increasingly
 *   performative and regulatory bodies rely on it as liability shield rather
 *   than genuine competence mechanism.
 *
 * KEY AGENTS:
 *   - Regulatory Bodies (NRC, FAA, ICAO): Institutional beneficiary (institutional/arbitrage) — accepts simulation as proxy, reduces liability exposure, provides measurable compliance pathway
 *   - Operational Organizations (nuclear plants, airlines, emergency response): Organized beneficiary (organized/constrained) — maintains competence without catastrophic risk, enables collective learning
 *   - Frontline Operators (pilots, reactor operators, responders): Mixed position (moderate/constrained) — benefits from competence maintenance but bears time/cost burden and theater risk
 *   - Untrained Successors (junior staff, newly hired, populations in high-risk zones): Primary victim (powerless/trapped) — depends on competence of trained operators; if simulation proves insufficient, bears catastrophic cost
 *   - Simulation Industry (manufacturers, training vendors, certification bodies): Institutional actor (institutional/arbitrage) — maintains simulation through financial incentive and institutional inertia; theater ratio rising
 *   - Analytical Observer: Civilizational view (analytical/analytical) — assesses whether simulation genuinely substitutes for catastrophe-level learning or naturalizes institutional convenience
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simulation_as_proxy_catastrophe_reading, 0.28).
domain_priors:suppression_score(simulation_as_proxy_catastrophe_reading, 0.35).
domain_priors:theater_ratio(simulation_as_proxy_catastrophe_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simulation_as_proxy_catastrophe_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(simulation_as_proxy_catastrophe_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(simulation_as_proxy_catastrophe_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simulation_as_proxy_catastrophe_reading, rope).
narrative_ontology:human_readable(simulation_as_proxy_catastrophe_reading, "Simulation as Proxy Catastrophe: Competence Maintenance Through Controlled Practice").
narrative_ontology:topic_domain(simulation_as_proxy_catastrophe_reading, "safety_engineering/organizational_learning/high_reliability_organizations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simulation_as_proxy_catastrophe_reading, '2f79cb43-482b-4c71-862f-b0a779903517').
narrative_ontology:cs_kernel_codification('2f79cb43-482b-4c71-862f-b0a779903517', formalized).
narrative_ontology:cs_authority_grounding('2f79cb43-482b-4c71-862f-b0a779903517', extraction).
narrative_ontology:cs_interpretation_layer_present('2f79cb43-482b-4c71-862f-b0a779903517').
narrative_ontology:cs_reading_relation('2f79cb43-482b-4c71-862f-b0a779903517', simulation_as_proxy_catastrophe_reading__catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f79cb43-482b-4c71-862f-b0a779903517', simulation_as_proxy_catastrophe_reading__hybrid_degradation_reading, influences).
narrative_ontology:cs_reading_relation('2f79cb43-482b-4c71-862f-b0a779903517', simulation_as_proxy_catastrophe_reading__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('2f79cb43-482b-4c71-862f-b0a779903517', foundational, simulation_indefinitely_sufficient).
narrative_ontology:cs_axiom_status(simulation_indefinitely_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('2f79cb43-482b-4c71-862f-b0a779903517', simulation_indefinitely_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('2f79cb43-482b-4c71-862f-b0a779903517', foundational, catastrophe_avoidance_via_simulation).
narrative_ontology:cs_axiom_status(catastrophe_avoidance_via_simulation, holdable).
narrative_ontology:cs_axiom_grounding('2f79cb43-482b-4c71-862f-b0a779903517', catastrophe_avoidance_via_simulation, instrumental).
narrative_ontology:cs_reference_frame('2f79cb43-482b-4c71-862f-b0a779903517', simulation_sufficiency_framework).
narrative_ontology:cs_drift_state('2f79cb43-482b-4c71-862f-b0a779903517', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2f79cb43-482b-4c71-862f-b0a779903517', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(simulation_as_proxy_catastrophe_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simulation_as_proxy_catastrophe_reading, regulatory_bodies).
narrative_ontology:constraint_beneficiary(simulation_as_proxy_catastrophe_reading, operational_organizations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPERATIONAL ORGANIZATION (ROPE) — Organized actors (nuclear plants, aviation operations, emergency response teams) benefit from simulation as genuine coordination mechanism: maintains competence without catastrophic risk, enables collective learning, distributes tacit knowledge across teams. Constrained exit (regulatory mandate) but net beneficiary — extraction is minimal, coordination function is real.
constraint_indexing:constraint_classification(simulation_as_proxy_catastrophe_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: REGULATORY AUTHORITY (ROPE) — Institutional actors (NRC, FAA, ICAO) benefit from simulation as coordination mechanism: enables competence verification without requiring actual catastrophes, reduces liability exposure, provides measurable compliance pathway. Arbitrage exit (can shift to alternative verification methods) but net beneficiary — extraction is minimal, coordination function is clear.
constraint_indexing:constraint_classification(simulation_as_proxy_catastrophe_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: FRONTLINE OPERATOR (TANGLED ROPE) — Moderate-power actors (pilots, reactor operators, emergency responders) experience mixed coordination and extraction. Simulation genuinely maintains competence (coordination benefit) but also imposes time/cost burden, creates theater (passing exercises without real learning), and substitutes for actual experience diversity. Constrained exit (regulatory requirement, career dependence) — moderate extraction alongside genuine coordination.
constraint_indexing:constraint_classification(simulation_as_proxy_catastrophe_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: UNTRAINED SUCCESSOR (SNARE) — Powerless agents (junior operators, newly hired staff, populations in high-risk zones) face extraction: they depend on the competence of trained operators, but simulation-only training may not transmit tacit knowledge or prepare for novel failure modes. Trapped by organizational hierarchy and geographic dependence — if simulation proves insufficient, they bear the catastrophic cost. No exit option.
constraint_indexing:constraint_classification(simulation_as_proxy_catastrophe_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: SIMULATION INDUSTRY (PITON) — Institutional actors (simulator manufacturers, training vendors, certification bodies) maintain simulation as proxy through institutional inertia and financial incentive. The original function (competence maintenance without catastrophe) has partially atrophied — much simulation is now theater (checking boxes, meeting hours requirements) rather than genuine learning. Theater ratio (0.58) reflects that many exercises are performative compliance rather than functional skill transfer. Arbitrage exit available but financial incentive keeps the constraint in place.
constraint_indexing:constraint_classification(simulation_as_proxy_catastrophe_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / SIMULATION SUFFICIENCY VIEW (ROPE) — From a civilizational perspective, simulation is a genuine coordination mechanism: it solves the real problem of maintaining competence without requiring catastrophic events. The constraint enables organizations to learn from controlled failure without bearing catastrophic costs. This perspective sees simulation as a legitimate proxy for catastrophe-equivalent learning — the mechanism works, extraction is minimal, and the coordination function is clear.
constraint_indexing:constraint_classification(simulation_as_proxy_catastrophe_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simulation_as_proxy_catastrophe_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(simulation_as_proxy_catastrophe_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(simulation_as_proxy_catastrophe_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(simulation_as_proxy_catastrophe_reading, TR),
    TR >= 0.70.

:- end_tests(simulation_as_proxy_catastrophe_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-to-moderate. The constraint is fundamentally a coordination mechanism — simulation solves the real problem of maintaining competence without catastrophic cost. However, extractiveness is rising (0.15 → 0.32 over the interval), indicating that the constraint is drifting toward extraction as regulatory bodies use simulation as liability shield and simulation industry maintains theater. At t=0, extractiveness is minimal (genuine coordination). By t=15, extractiveness has doubled, suggesting that the constraint is becoming Tangled Rope. Suppression (0.35): Moderate. Regulatory mandate for simulation creates barriers to alternative verification methods, but suppression is not severe — organizations can still choose simulation fidelity levels and training approaches. Theater ratio (0.58): Moderate-high and rising. Early simulation (t=0, theater=0.35) was genuinely functional — exercises were designed to test competence and identify gaps. By t=15 (theater=0.62), much simulation is performative compliance: checking hours requirements, passing standardized exercises, maintaining certification without genuine learning. The rising theater ratio indicates that the constraint is degrading from Rope toward Piton.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a perspectival gap between beneficiaries and victims. Regulatory bodies see Rope (genuine coordination: competence verification without catastrophe). Operational organizations see Rope (competence maintenance without catastrophic cost). Frontline operators see Tangled Rope (mixed coordination and extraction: competence maintenance but also time/cost burden and theater risk). Untrained successors see Snare (false proxy: they depend on competence of trained operators, but if simulation proves insufficient, they bear catastrophic cost with no exit option). The analytical observer sees Rope (genuine coordination mechanism) but notes the rising theater ratio and extractiveness trajectory, suggesting drift toward Tangled Rope or Snare. The perspectival gap is driven by exit options: beneficiaries have arbitrage options (can shift to alternative verification methods); operators have constrained options (regulatory mandate); victims have trapped options (geographic/organizational dependence). The gap widens as theater rises — beneficiaries continue to see coordination, but victims increasingly see extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim declarations and exit options. Regulatory bodies (beneficiary, institutional/arbitrage) experience low d (0.15-0.25) — they collect from the constraint without bearing costs. Operational organizations (beneficiary, organized/constrained) experience moderate d (0.35-0.45) — they benefit from competence maintenance but bear training costs. Frontline operators (mixed, moderate/constrained) experience moderate-high d (0.50-0.60) — they bear time/cost burden while also benefiting from competence maintenance. Untrained successors (victim, powerless/trapped) experience high d (0.75-0.85) — they bear catastrophic risk if simulation proves insufficient, with no exit option. The engine computes effective extraction (χ) by applying f(d) to base extractiveness, scaled by scope. At national scope, the scaling is modest; at global scope (regulatory standards), scaling is higher. The rising extractiveness trajectory suggests that d is shifting upward for beneficiaries (regulatory bodies increasingly collecting liability protection) and victims (untrained successors increasingly bearing risk as theater rises).
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING ANALYSIS: This constraint instantiates the simulation_as_proxy_catastrophe_reading of the catastrophe_proxy_sufficiency kernel. The reading's mandate is to maintain operational competence without catastrophic cost through simulation exercises. The mandate is live (competence maintenance is still required), but the reading's sufficiency claim is increasingly contested. The measurement trajectory shows that the constraint is drifting from Rope (genuine coordination) toward Tangled Rope (mixed coordination and extraction) as theater rises and extractiveness increases. The mandatrophy is not yet resolved — the constraint still functions as a coordination mechanism — but the trajectory suggests that mandatrophy will emerge if theater continues to rise and extractiveness continues to increase. The analytical observer should monitor whether the constraint's function (competence maintenance) is being replaced by its form (simulation hours, exercise completion). If function is replaced by form, the constraint will have resolved mandatrophy: the original mandate (competence maintenance) will have been superseded by the institutional mandate (liability protection via simulation theater).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_threshold,
    'What level of simulation fidelity is necessary and sufficient to maintain competence equivalent to catastrophe-level learning?',
    'Longitudinal comparison of operator performance in actual emergencies: operators trained via high-fidelity simulation vs. low-fidelity simulation vs. catastrophe-experienced operators. Measurement of error rates, decision quality, and novel-failure response.',
    'If high-fidelity is necessary: simulation becomes expensive and extractive (Tangled Rope from operator perspective). If low-fidelity suffices: simulation is genuinely efficient coordination (Rope confirmed). If no simulation suffices: the constraint is a false proxy (Snare from untrained successor perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_threshold, empirical, 'Fidelity threshold for competence equivalence').

omega_variable(
    catastrophe_necessity_vs_simulation_sufficiency,
    'Is actual catastrophe experience irreplaceable for certain competence domains, or can simulation genuinely substitute indefinitely?',
    'Domain-specific analysis: identify competence elements that require catastrophe-level stress (physiological response, decision-making under extreme uncertainty, team coordination under chaos) vs. those that can be learned via simulation. Post-incident analysis of actual failures: did simulation-trained operators fail in ways that catastrophe-experienced operators would not?',
    'If catastrophe is irreplaceable for some domains: simulation is a partial proxy (Tangled Rope or Snare from operator perspective). If simulation is fully sufficient: the reading is confirmed (Rope). If catastrophe is necessary: the constraint is a false summit (Snare from untrained successor perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(catastrophe_necessity_vs_simulation_sufficiency, empirical, 'Whether catastrophe experience is irreplaceable or simulation-substitutable').

omega_variable(
    kernel_reading_contest,
    'Which reading of the catastrophe_proxy_sufficiency kernel is structurally correct: simulation_as_proxy_catastrophe_reading (this reading), catastrophe_necessity_reading, hybrid_degradation_reading, or simulation_fidelity_threshold?',
    'This is a conceptual/empirical hybrid. The empirical component is resolved by the two omegas above (fidelity threshold, catastrophe necessity). The conceptual component is resolved by examining whether the regulatory framework''s legitimacy claim (simulation is sufficient) is grounded in evidence or in institutional convenience. If regulatory bodies have systematically avoided funding the empirical studies that would resolve the fidelity question, that avoidance is evidence that the reading is institutionally convenient rather than empirically grounded.',
    'If simulation_as_proxy_catastrophe_reading is correct: the constraint is Rope (genuine coordination). If catastrophe_necessity_reading is correct: the constraint is Snare (false proxy). If hybrid_degradation_reading is correct: the constraint is Tangled Rope (mixed coordination and extraction). If simulation_fidelity_threshold is correct: the constraint is Scaffold (temporary until fidelity is established).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which kernel reading is structurally correct').

omega_variable(
    regulatory_liability_protection_motive,
    'To what extent does regulatory acceptance of simulation as proxy reflect genuine belief in sufficiency vs. institutional preference for liability protection?',
    'Institutional analysis: examine regulatory funding for fidelity research, regulatory response to simulation failures, regulatory willingness to mandate catastrophe-equivalent training when simulation proves insufficient. If regulatory bodies have systematically underfunded fidelity research while mandating simulation, that pattern suggests institutional motive (liability protection) rather than empirical confidence.',
    'If motive is genuine belief: the constraint is Rope (coordination). If motive is liability protection: the constraint is Tangled Rope (beneficiary is regulatory body, victim is untrained successor). If motive is institutional convenience: the constraint is Snare (false proxy maintained for regulatory convenience).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_liability_protection_motive, empirical, 'Regulatory motive: sufficiency belief vs. liability protection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simulation_as_proxy_catastrophe_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simprox_tr_t0, simulation_as_proxy_catastrophe_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(simprox_tr_t5, simulation_as_proxy_catastrophe_reading, theater_ratio, 5, 0.45).
narrative_ontology:measurement(simprox_tr_t10, simulation_as_proxy_catastrophe_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement(simprox_tr_t15, simulation_as_proxy_catastrophe_reading, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(simprox_be_t0, simulation_as_proxy_catastrophe_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(simprox_be_t5, simulation_as_proxy_catastrophe_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(simprox_be_t10, simulation_as_proxy_catastrophe_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(simprox_be_t15, simulation_as_proxy_catastrophe_reading, base_extractiveness, 15, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(simprox_su_t0, simulation_as_proxy_catastrophe_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(simprox_su_t10, simulation_as_proxy_catastrophe_reading, suppression_requirement, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simulation_as_proxy_catastrophe_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(simulation_as_proxy_catastrophe_reading, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(simulation_as_proxy_catastrophe_reading, hybrid_degradation_reading).
narrative_ontology:affects_constraint(simulation_as_proxy_catastrophe_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_proxy_sufficiency kernel. The sibling readings (catastrophe_necessity_reading, hybrid_degradation_reading, simulation_fidelity_threshold) are separate constraint stories with different ε values and different beneficiary/victim structures. This reading asserts low extractiveness (genuine coordination); catastrophe_necessity_reading asserts high extractiveness (false proxy); hybrid_degradation_reading asserts moderate extractiveness (partial proxy with degradation); simulation_fidelity_threshold asserts low-to-moderate extractiveness (coordination with fidelity constraint). The network links enable contamination propagation analysis: if this reading's sufficiency claim is falsified by empirical evidence, the engine can predict which sibling readings will be affected.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
