% ============================================================================
% CONSTRAINT STORY: purity_drift_degradation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_purity_drift_degradation, []).

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
 *   constraint_id: purity_drift_degradation
 *   human_readable: Purity Drift in Long-Term Reciprocal Obligations
 *   domain: social_constraint_theory/agency_depletion/power_asymmetry
 *
 * SUMMARY:
 *   Purity drift describes the structural degradation of reciprocal
 *   obligation systems over time. At t=0, the obligation structure provides
 *   genuine coordination value: mutual aid, risk pooling, information
 *   sharing, or collective resource access. Both parties benefit, though
 *   asymmetrically. Over time, the coordination function atrophies while the
 *   extraction mechanism persists. The dominant agent continues to extract
 *   compliance, labor, or resources from the dependent agent, but the
 *   reciprocal benefits decay. The purity metric P(t) = coordination_value /
 *   (coordination_value + extraction_cost) declines from an initial tangled
 *   rope range (0.50-0.60) below the 0.40 threshold, at which point the
 *   constraint transitions from a mixed coordination-extraction hybrid to a
 *   pure extraction snare from the dependent agent's perspective. This drift
 *   occurs without explicit policy change — the formal structure of the
 *   obligation remains constant, but its functional content transforms. The
 *   theater_ratio increases as performative reciprocity gestures (symbolic
 *   recognition, nominal inclusion, ritualized appreciation) substitute for
 *   material benefits. This constraint is downstream of
 *   indexical_extraction_asymmetry: the power differential that enables
 *   asymmetric extraction also drives asymmetric decay, because the dominant
 *   agent has no incentive to maintain coordination value once dependency is
 *   established.
 *
 * KEY AGENTS:
 *   - Dependent Agent: Primary victim (powerless/trapped) — bears increasing extraction as coordination value decays; cannot exit due to accumulated dependency, identity fusion, or structural barriers
 *   - Constrained Participant: Secondary victim (moderate/constrained) — recognizes degradation but retains exit capacity at high cost; still receives residual coordination value
 *   - Dominant Agent: Primary beneficiary (institutional/arbitrage) — extracts value while coordination function atrophies; experiences the structure as legitimate reciprocity; has full exit capacity
 *   - Reform Coalition: Organized agents (organized/mobile) — working to sunset degraded obligations through policy intervention, mutual aid networks, or alternative coordination mechanisms
 *   - Institutional Maintainer: Institutional actor (institutional/arbitrage) — maintains degraded obligation structure through inertia; sees high theater ratio but continues enforcement
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes purity drift as structural feature of long-term obligations under power asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(purity_drift_degradation, 0.48).
domain_priors:suppression_score(purity_drift_degradation, 0.62).
domain_priors:theater_ratio(purity_drift_degradation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(purity_drift_degradation, extractiveness, 0.48).
narrative_ontology:constraint_metric(purity_drift_degradation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(purity_drift_degradation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(purity_drift_degradation, tangled_rope).
narrative_ontology:human_readable(purity_drift_degradation, "Purity Drift in Long-Term Reciprocal Obligations").
narrative_ontology:topic_domain(purity_drift_degradation, "social_constraint_theory/agency_depletion/power_asymmetry").

domain_priors:requires_active_enforcement(purity_drift_degradation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(purity_drift_degradation, dominant_agents_in_obligation_structures).
narrative_ontology:constraint_victim(purity_drift_degradation, dependent_agents_in_long_term_obligations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT AGENT (SNARE) — Trapped in degraded obligation structure where coordination function has atrophied but extraction persists. Cannot exit due to accumulated dependency, identity fusion with the relationship, or structural barriers. Experiences pure extraction as the reciprocal benefit has vanished while the burden remains.
constraint_indexing:constraint_classification(purity_drift_degradation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CONSTRAINED PARTICIPANT (TANGLED ROPE) — Recognizes the degradation but retains some exit capacity at high cost. Still receives residual coordination value (social network access, institutional affiliation, partial reciprocity) alongside extraction. Can leave but would sacrifice accumulated relational capital and face reputational costs.
constraint_indexing:constraint_classification(purity_drift_degradation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DOMINANT AGENT (ROPE) — Experiences the obligation structure as coordination. Extracts value from dependent agents but frames this as legitimate reciprocity. Has full exit capacity and can arbitrage between multiple obligation networks. The degradation is invisible from this position because the extraction flow runs toward this agent.
constraint_indexing:constraint_classification(purity_drift_degradation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM COALITION (SCAFFOLD) — Organized agents working to sunset degraded obligation structures through policy intervention, mutual aid networks, or alternative coordination mechanisms. Sees purity drift as a temporary problem with a solution path: renegotiation protocols, exit subsidies, or structural alternatives that restore reciprocity or enable clean exit.
constraint_indexing:constraint_classification(purity_drift_degradation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL MAINTAINER (PITON) — Sees the obligation structure as a degraded ritual maintained through inertia. The coordination function has atrophied but the institutional form persists because no alternative has fully replaced it. High theater ratio: performative reciprocity gestures without functional mutual benefit.
constraint_indexing:constraint_classification(purity_drift_degradation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes both the genuine coordination function at t=0 and the extraction accumulation over time. Purity drift is a structural feature of long-term obligations under power asymmetry: coordination value decays faster than extraction costs, transforming tangled ropes into snares without policy intervention. The analytical classification matches the claimed type because this is the cross-position synthesis.
constraint_indexing:constraint_classification(purity_drift_degradation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(purity_drift_degradation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(purity_drift_degradation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(purity_drift_degradation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(purity_drift_degradation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(purity_drift_degradation, TR),
    TR >= 0.70.

:- end_tests(purity_drift_degradation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint begins as a tangled rope (ε ≈ 0.32) with genuine coordination value. Over the 9-unit interval, coordination function decays while extraction persists, driving extractiveness to 0.48 — just above the tangled rope / snare boundary. The final value reflects that the obligation structure still provides residual coordination value for some agents (constrained participants) but has crossed into snare territory for trapped agents. Suppression (0.62): Moderate-high. Exit barriers include accumulated dependency (sunk costs in the relationship), identity fusion (self-concept tied to the obligation), reputational costs (social penalty for breaking commitments), and structural barriers (lack of alternative coordination mechanisms). Suppression increases over time as dependency deepens. Theater ratio (0.58): Moderate-high. Performative reciprocity gestures increasingly substitute for functional mutual benefit. The dominant agent maintains symbolic recognition and nominal inclusion rituals while reducing material reciprocity. The theater ratio rises from 0.28 at t=0 to 0.58 at t=9, tracking the coordination decay.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how power asymmetry creates divergent experiences of the same structural degradation. The dependent agent sees a snare: coordination value has vanished, extraction persists, and exit is blocked. The constrained participant sees a tangled rope: residual coordination value remains, but extraction is increasing and exit is costly. The dominant agent sees a rope: the obligation structure coordinates mutual benefit, and any extraction is legitimate reciprocity. The reform coalition sees a scaffold: the degradation is a solvable problem with a sunset path through policy intervention. The institutional maintainer sees a piton: the structure is degraded but persists through inertia. The analytical observer sees a tangled rope at the civilizational scale: the constraint genuinely coordinates at t=0 and genuinely extracts at t=9, and the transformation is a structural feature of long-term obligations under power asymmetry. The perspectival gap is not a disagreement about facts but a consequence of structural position: the agent who benefits from extraction cannot see it as extraction, and the agent who bears extraction cannot see the residual coordination value that justifies the structure from other positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The dependent agent is a victim with trapped exit options, yielding high d and high experienced extraction. The constrained participant is a victim with constrained exit options, yielding moderate-high d and moderate experienced extraction. The dominant agent is a beneficiary with arbitrage exit options, yielding low d and low or negative experienced extraction. The reform coalition is organized with mobile exit options, yielding moderate d but low experienced extraction due to agency. The institutional maintainer is a beneficiary with arbitrage exit options, yielding low d. The analytical observer has analytical exit options, yielding moderate-high d but recognizes the structural transformation rather than experiencing it directly. The perspectival gap is maximal between the dependent agent (snare) and the dominant agent (rope), revealing that the same obligation structure appears as pure extraction from below and as legitimate coordination from above.
 *
 * MANDATROPHY ANALYSIS:
 *   PURITY DRIFT AS MANDATROPHY RESOLUTION: This constraint resolves the mandatrophy by showing that the tangled rope / snare boundary is not a fixed property of the constraint but a time-dependent function of the purity metric. At t=0, the constraint is a tangled rope from all perspectives: genuine coordination value coexists with asymmetric extraction. At t=9, the constraint is a snare from the dependent agent's perspective but remains a tangled rope from the constrained participant's perspective and a rope from the dominant agent's perspective. The mandatrophy is resolved by recognizing that classification is both indexical (depends on observer position) and temporal (depends on time point). The purity drift hypothesis predicts that tangled ropes naturally degrade into snares under power asymmetry unless active maintenance restores coordination value or policy intervention enables exit. The 0.40 purity threshold is the empirical boundary at which behavioral framing shifts from mutual benefit to unilateral burden, operationalizing the tangled rope / snare transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_decay_mechanism,
    'What drives the asymmetric decay of coordination value versus extraction cost in long-term obligations?',
    'Longitudinal tracking of obligation structures: measure coordination benefits and extraction costs at multiple time points; identify whether decay is due to changing external conditions, strategic defection by dominant agents, or inherent structural dynamics',
    'If decay is strategic: intervention can restore purity by enforcing reciprocity. If decay is structural: only exit or renegotiation can resolve. If decay is external: changing conditions may restore coordination value without intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_decay_mechanism, empirical, 'Mechanism driving asymmetric decay of coordination versus extraction').

omega_variable(
    purity_threshold_calibration,
    'Is the 0.40 purity threshold (P = coordination / (coordination + extraction)) the correct boundary between tangled rope and snare, or does the transition occur at a different ratio?',
    'Cross-domain analysis of obligation structures at various purity levels; behavioral observation of when agents shift from mutual benefit framing to unilateral burden framing; exit attempt frequency as function of purity',
    'If threshold is lower (e.g., 0.30): more constraints misclassified as snares when they retain coordination function. If threshold is higher (e.g., 0.50): snares misclassified as tangled ropes, masking extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(purity_threshold_calibration, empirical, 'Calibration of purity threshold for tangled rope to snare transition').

omega_variable(
    identity_lock_persistence,
    'Does identity fusion with the obligation structure persist after the coordination function has atrophied, creating a cognitive barrier to exit independent of structural barriers?',
    'Post-exit interviews with agents who left degraded obligations; comparison of exit barriers cited by those who left versus those who stayed; measurement of identity frame shifts during and after exit',
    'If identity lock persists: dependent agents are identity_locked rather than trapped, and the binding mechanism is cognitive rather than purely structural. If identity lock dissolves when coordination vanishes: the trapped classification is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Whether identity fusion persists after coordination decay').

omega_variable(
    theater_substitution_timing,
    'At what point in the purity drift does performative reciprocity (theater) substitute for functional reciprocity?',
    'Behavioral observation of obligation structures over time; identification of when symbolic gestures replace material benefits; correlation between theater_ratio increase and purity decline',
    'If theater substitution precedes purity collapse: theater_ratio is an early warning signal for degradation. If theater substitution follows purity collapse: theater is a consequence rather than a cause of drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_substitution_timing, empirical, 'Timing of theater substitution relative to purity decline').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(purity_drift_degradation, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(purity_drift_tr_t0, purity_drift_degradation, theater_ratio, 0, 0.28).
narrative_ontology:measurement(purity_drift_tr_t3, purity_drift_degradation, theater_ratio, 3, 0.42).
narrative_ontology:measurement(purity_drift_tr_t6, purity_drift_degradation, theater_ratio, 6, 0.52).
narrative_ontology:measurement(purity_drift_tr_t9, purity_drift_degradation, theater_ratio, 9, 0.58).

% Extraction over time
narrative_ontology:measurement(purity_drift_be_t0, purity_drift_degradation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(purity_drift_be_t3, purity_drift_degradation, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(purity_drift_be_t6, purity_drift_degradation, base_extractiveness, 6, 0.44).
narrative_ontology:measurement(purity_drift_be_t9, purity_drift_degradation, base_extractiveness, 9, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(purity_drift_degradation, attachment_coordination).

% DUAL FORMULATION NOTE:
% Purity drift is downstream of indexical_extraction_asymmetry. The power differential that enables asymmetric extraction also drives asymmetric decay: the dominant agent has no incentive to maintain coordination value once dependency is established. The upstream constraint (indexical_extraction_asymmetry) has its own extractiveness reflecting the power asymmetry itself; purity drift has its own extractiveness reflecting the temporal degradation of coordination function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
