% ============================================================================
% CONSTRAINT STORY: new_start_expiration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: new_start_expiration
 *   human_readable: New START Treaty Expiration and Nuclear Arms Coordination Collapse
 *   domain: geopolitical/nuclear_deterrence
 *
 * SUMMARY:
 *   The New START Treaty, which limited US and Russian deployed strategic
 *   nuclear warheads to 1,550 each and established on-site inspection and
 *   verification regimes, expired in February 2026. Its non-renewal created a
 *   constraint landscape characterized by the collapse of bilateral
 *   verification transparency and the simultaneous acceleration of
 *   modernization programs by both superpowers. The constraint operates
 *   across three nested levels: (1) the bilateral US-Russia strategic
 *   stability mechanism, (2) the global non-nuclear security system dependent
 *   on extended deterrence, and (3) the institutional verification and arms
 *   control infrastructure. This constraint demonstrates how the same
 *   structural phenomenon — the absence of enforceable limits on nuclear
 *   arsenals — appears as coordination mechanism (Rope for superpowers),
 *   asymmetric extraction (Tangled Rope for allies), pure extraction (Snare
 *   for non-nuclear states), degraded ritual (Piton for compliance
 *   bureaucracies), and potentially natural law (false Mountain from
 *   analytical distance).
 *
 * KEY AGENTS:
 *   - Russian Strategic Forces Command: Primary beneficiary (institutional/arbitrage) — gains strategic modernization freedom, eliminates transparency obligations, re-establishes first-strike ambiguity
 *   - US Department of Defense: Primary beneficiary (institutional/arbitrage) — enables classified programs, accelerates hypersonic deployment, removes notification requirements for warhead modernization
 *   - Non-Nuclear States (majority of UN): Primary victim (powerless/trapped) — cannot verify compliance, cannot exit extended deterrence dependency, cannot credibly hedge through independent capability
 *   - Non-Aligned Movement / IAEA: Secondary victim and organized actor (organized/constrained) — loses verification mandate and transparency access; retains coordination role in normative advocacy
 *   - Allied Nuclear Umbrella States (NATO, Japan, South Korea): Mixed victim-beneficiary (powerful/constrained) — benefit from strengthened US deterrent but suffer extraction risk from unmonitored arms expansion and accident probability increase
 *   - Arms Control Advocacy Coalition: Organized sunset actor (organized/mobile) — sees expiration as temporary window; building coalition for treaty renewal or alternative mechanisms
 *   - International Treaty Compliance System: Institutional degradation (institutional/constrained) — maintains inspection infrastructure and reporting bureaucracies despite loss of functional enforcement; transition from Rope to Piton
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(new_start_expiration, 0.62).
domain_priors:suppression_score(new_start_expiration, 0.68).
domain_priors:theater_ratio(new_start_expiration, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(new_start_expiration, extractiveness, 0.62).
narrative_ontology:constraint_metric(new_start_expiration, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(new_start_expiration, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(new_start_expiration, tangled_rope).
narrative_ontology:human_readable(new_start_expiration, "New START Treaty Expiration and Nuclear Arms Coordination Collapse").
narrative_ontology:topic_domain(new_start_expiration, "geopolitical/nuclear_deterrence").

domain_priors:requires_active_enforcement(new_start_expiration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(new_start_expiration, russian_military_establishment).
narrative_ontology:constraint_beneficiary(new_start_expiration, us_military_industrial_complex).
narrative_ontology:constraint_victim(new_start_expiration, global_non_nuclear_states).
narrative_ontology:constraint_victim(new_start_expiration, transparency_verification_regimes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-NUCLEAR STATES (SNARE) — Lack exit options from the constraint imposed by nuclear powers' unchecked arsenals. Cannot independently verify compliance, cannot credibly develop countervailing capability, cannot organize collective security outside superpower umbrellas. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈1.06. Full extraction from powerless position.
constraint_indexing:constraint_classification(new_start_expiration, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-ALIGNED MOVEMENT / IAEA (TANGLED ROPE) — Organized but constrained. Benefits from arms control transparency for monitoring and norm-setting (coordination function); suffers from extraction as superpowers resume unverified arms races and budgets for verification collapse. d≈0.65, f(d)≈0.95, σ=1.2 → χ≈0.62. Hybrid: genuine coordination role but also victim of arms race acceleration.
constraint_indexing:constraint_classification(new_start_expiration, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: RUSSIAN STRATEGIC FORCES (ROPE) — Experiences expiration as coordination mechanism: the constraint removal enables rearmament signaling, modernization without notification obligations, and strategic bargaining leverage. Benefits from opacity of arsenals and renewed first-strike capability. d≈0.10, f(d)≈0.10, σ=1.2 → χ≈0.07. Net beneficiary; sees constraint expiration as coordination win (freedom to signal strength).
constraint_indexing:constraint_classification(new_start_expiration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: US STRATEGIC COMMAND (ROPE) — Similar beneficiary position. Expiration enables classified modernization programs, acceleration of hypersonic delivery systems, and expansion of deployed warheads without transparency costs. d≈0.10, f(d)≈0.10, σ=1.2 → χ≈0.07. Net beneficiary; sees constraint removal as coordination mechanism for strategic flexibility.
constraint_indexing:constraint_classification(new_start_expiration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ALLIED NUCLEAR UMBRELLA STATES (TANGLED ROPE) — Powerful but structurally constrained by dependence on US nuclear guarantees. Experience expiration as mixed: benefits from US strategic modernization (strengthened deterrent against adversaries) but suffers from extraction as unmonitored arms expansion increases accident risk and escalation probability. d≈0.58, f(d)≈0.75, σ=1.2 → χ≈0.55. Both coordination (credible deterrent) and extraction (verification collapse).
constraint_indexing:constraint_classification(new_start_expiration, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ARMS CONTROL ADVOCACY COALITION (SCAFFOLD) — Organized advocates (SIPRI, Union of Concerned Scientists, nuclear abolition NGOs) see expiration as a temporary window with a potential sunset: new arms control agreements (bilateral, trilateral, or FMCT) could reimpose constraints. The coordination function is building consensus for negotiated return to verification. d≈0.45, f(d)≈0.50, σ=1.2 → χ≈0.33. Low effective extraction because advocates have exit path (negotiated renewal).
constraint_indexing:constraint_classification(new_start_expiration, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERNATIONAL TREATY COMPLIANCE SYSTEM (PITON) — The formal inspection and verification apparatus of the UN, IAEA, and bilateral inspection regimes persists through institutional inertia despite functional obsolescence. Without START, many inspection rights evaporate, yet verification bureaucracies maintain ritualistic reporting and compliance theater. theater_ratio≈0.60. The institutional framework continues asserting relevance after losing enforcement mechanism. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.43. Degraded coordination (inspectors still convene) masked as enforcement.
constraint_indexing:constraint_classification(new_start_expiration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, the expiration of START reflects immutable realities of nuclear deterrence: mutual deterrence requires opacity, verification is inherently incomplete with advanced technology, and the security dilemma makes unilateral restraint irrational. The constraint's expiration is thus 'natural' — a structural feature of nuclear competition. However, the extractiveness (0.62) and suppression (0.68) contradict mountain thresholds. This perspective risks naturalizing what is actually a contingent diplomatic failure (institutional actors choosing non-renewal) as an iron law of deterrence. FALSE SUMMIT INDICATOR.
constraint_indexing:constraint_classification(new_start_expiration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(new_start_expiration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(new_start_expiration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(new_start_expiration, TypeOther, context(agent_power(organized), _, _, _)),
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
 *   Extractiveness (0.62): High-moderate. The constraint extracts significant value from non-nuclear states through imposed deterrence dependency and verification uncertainty, but is not maximal (0.70+) because superpowers also bear costs of mutual strategic instability and accident risk. The extraction is asymmetric: superpowers capture modernization gains; non-nuclear states bear deterrence risk. The trajectory shows acceleration from 0.35 at treaty signing (initial confidence in verification mechanisms) to 0.62 post-expiration (verification collapse creates information asymmetry enabling extraction). Suppression (0.68): High. Significant barriers to non-nuclear states' exit include: (1) no credible autonomous deterrent (requires 10-20 year development), (2) extended deterrence dependency locked in by alliance commitments, (3) diplomatic capacity constraints for independent negotiation, (4) intelligence gap preventing unilateral verification. Theater ratio (0.55): Moderate. Pre-expiration, verification theaters dominated (inspection rituals, compliance reporting). Post-expiration, theater declines slightly as performative inspection apparatus becomes moot. However, diplomatic theater around 'negotiating renewed agreements' rises, offsetting decline in inspection theater. Net effect: theater remains moderate, neither rising to Piton (0.70+) nor falling to Rope (0.30-).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. Superpowers see Rope (coordination mechanism enabling credible deterrence signaling through opacity). Allies see Tangled Rope (benefits from deterrent strength but extracts cost of instability). Non-nuclear states see Snare (pure extraction with no exit). Advocacy coalition sees Scaffold (temporary expiration, renewable through negotiation). Compliance bureaucracy sees Piton (degraded inspection ritual persisting through inertia, no longer enforced). Analytical observer risks seeing Mountain (deterrence naturally requires opacity). The perspectival gap reveals that the 'same constraint' produces radically different classifications depending on the agent's structural relationship to verification and deterrence.
 *
 * DIRECTIONALITY LOGIC:
 *   Russian Strategic Forces: Beneficiary + arbitrage → d≈0.10, f(d)≈0.10. Net beneficiary from expiration. US Strategic Command: Beneficiary + arbitrage → d≈0.10, f(d)≈0.10. Net beneficiary from expiration. Non-nuclear states: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction — no exit from deterrence dependency or verification uncertainty. Allied nuclear umbrella states: Mixed beneficiary (deterrent strength) + victim (instability risk) + constrained → d≈0.58, f(d)≈0.75. Moderate extraction; structural constraint prevents exit but also provides security good. Non-Aligned Movement: Victim (loses transparency) + organized + constrained → d≈0.65, f(d)≈0.95. Significant extraction but not maximum; organization provides some advocacy capacity. Arms control advocates: Mobile (can shift to new negotiating forums) → d≈0.45, f(d)≈0.50. Low effective extraction; mobile exit to alternative mechanisms. Compliance system: Constrained institutional actor → d≈0.50, f(d)≈0.65. Piton classification comes from theater gate (0.55), not from high chi (0.44). Analytical observer: analytical → d≈0.72, f(d)≈1.15. False Mountain detector flags this — naturalizing contingent treaty failure as immutable deterrence law.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids false classification through perspectival decomposition. The mandatrophy is NOT 'is this a Rope or a Snare?' but 'for whom?' Superpowers perceive Rope (genuine coordination function: enabling credible deterrence through strategic ambiguity). Non-nuclear states perceive Snare (pure extraction with no coordination benefit). The constraint is legitimately both because the same structural phenomenon (absence of verification) serves coordinating function for some agents (superpowers achieving strategic clarity through mutual uncertainty) and extractive function for others (non-nuclear states unable to verify or exit). The hybrid nature (Tangled Rope for the system as a whole) resolves the mandatrophy by acknowledging that the coordination benefit (deterrent credibility) is inseparable from the extraction cost (non-nuclear states' vulnerability). The theater ratio (0.55) indicates moderate performativity in compliance mechanisms, preventing false elevation to false Mountain. The active enforcement requirement is satisfied: superpowers actively enforce the non-renewal decision and block attempts at intermediate transparency measures. The beneficiary/victim distinction is sharp: superpowers benefit; non-nuclear states and verification regimes suffer. No false natural-law framing applies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_technology_threshold,
    'At what technical sophistication level does verification become impossible to perform without cooperative access?',
    'Intelligence community technical assessments; historical correlation between satellite resolution improvements and treaty negotiation difficulty; forward modeling of hypersonic and autonomous systems detectability',
    'If threshold crossed: constraint shift from Tangled Rope to Snare (unilateral extraction replaces coordinated verification). If threshold not crossed: Rope restoration remains possible through new agreements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(verification_technology_threshold, empirical, 'Technical limits of unilateral verification capability').

omega_variable(
    escalation_spiral_trigger,
    'Does unmonitored rearmament by one superpower trigger proportional response from the other, or does asymmetric capability advantage enable coercive diplomacy?',
    'Game-theoretic analysis of Nash equilibrium under incomplete information; historical precedent from 1970s-1980s Cold War acceleration periods; agent-based modeling of detection lag and response timing',
    'If trigger fires: mutually destabilizing arms race (Snare for both parties). If coercion succeeds: one state achieves strategic advantage (Snare for victim state, Rope for beneficiary). If cooperation re-establishes: negotiated arms control (Scaffold restoration).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(escalation_spiral_trigger, empirical, 'Whether arms race acceleration becomes self-reinforcing or negotiable').

omega_variable(
    third_party_actor_bypass,
    'Does proliferation pressure on threshold states (Iran, North Korea, Saudi Arabia, Poland) increase in a START-free environment, creating independent nuclear hedging strategies?',
    'Monitoring of uranium enrichment rates, weapons-grade material production, and delivery system development in threshold states; correlation with superpowers'' verification collapse; diplomatic pressure tracking from extended deterrence allies',
    'If proliferation accelerates: constraint becomes global (affects all states via cascading deterrence) rather than bilateral. Reclassifies from Tangled Rope to Snare for threshold states. If proliferation pauses: bilateral constraint remains bounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_actor_bypass, empirical, 'Whether START expiration triggers global proliferation cascade').

omega_variable(
    diplomatic_renewal_window,
    'What diplomatic conditions would enable negotiation of replacement arms control agreements (New START II, trilateral with China, or FMCT)?',
    'Political feasibility analysis from security studies literature; historical precedent from Cold War arms control recovery patterns; modeling of negotiation preconditions (leadership change, crisis de-escalation, economic pressure)',
    'If window opens: Scaffold sunset clause validates — constraint temporary, solution-oriented. If window closes: permanent Snare for non-nuclear states, stable Rope for superpowers (extraction with no sunset).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diplomatic_renewal_window, preference, 'Political conditions for arms control agreement renewal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(new_start_expiration, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(newstart_tr_t0, new_start_expiration, theater_ratio, 0, 0.45).
narrative_ontology:measurement(newstart_tr_t5, new_start_expiration, theater_ratio, 5, 0.52).
narrative_ontology:measurement(newstart_tr_t10, new_start_expiration, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(newstart_be_t0, new_start_expiration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(newstart_be_t5, new_start_expiration, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(newstart_be_t10, new_start_expiration, base_extractiveness, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(new_start_expiration, enforcement_mechanism).
narrative_ontology:affects_constraint(new_start_expiration, nuclear_deterrence_strategic_stability).
narrative_ontology:affects_constraint(new_start_expiration, proliferation_cascade_hypothesis).
narrative_ontology:affects_constraint(new_start_expiration, extended_deterrence_dependency).

% DUAL FORMULATION NOTE:
% New START expiration is structurally distinct from (1) the underlying bilateral deterrence equilibrium (which predates the treaty and continues after) and (2) the global proliferation dynamics it may trigger. This story models the constraint of the expiration event itself — the institutional withdrawal of verification transparency. Upstream constraints (deterrence equilibrium, proliferation incentives) have different ε values reflecting different empirical uncertainty. Downstream constraints (potential cascade effects in threshold states) are consequences of this expiration constraint's extraction function. Network links enable contamination propagation analysis: if verification collapse spreads to other arms control regimes, the extraction mechanism intensifies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(new_start_expiration, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
