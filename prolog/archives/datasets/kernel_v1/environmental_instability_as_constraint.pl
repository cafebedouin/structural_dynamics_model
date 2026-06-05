% ============================================================================
% CONSTRAINT STORY: environmental_instability_as_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_environmental_instability_as_constraint, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: environmental_instability_as_constraint
 *   human_readable: Environmental Instability as Perceived Constraint
 *   domain: cognitive_psychology/decision_theory/environmental_dynamics
 *
 * SUMMARY:
 *   Environmental instability — unpredictable changes in physical, social,
 *   economic, or informational conditions — is unusual in the DR corpus
 *   because it appears to classify as mountain from all observed
 *   perspectives, including the powerless agent. This challenges the
 *   framework's assumption that mountain constraints are rare and
 *   perspectival variation is universal. The constraint emerges when the
 *   volatility of the environment compresses the time horizon available for
 *   decision-making below the threshold at which meaningful exit planning
 *   becomes cognizable. A powerless agent facing rapid economic shocks,
 *   neighborhood violence, housing instability, or information overload
 *   experiences the constraint as immutable not because external barriers
 *   prevent exit, but because the time available to recognize and plan for
 *   exit has disappeared. The agent cannot see the exits because they exist
 *   on timescales longer than the volatile environment permits them to
 *   contemplate. From the institutional beneficiary perspective (agents with
 *   adaptive capacity), the constraint is not experienced as a constraint at
 *   all — volatility creates arbitrage opportunities and market premiums for
 *   adaptive resources. Yet even the beneficiary perceives the underlying
 *   source of volatility (macroeconomic cycles, climate dynamics,
 *   geopolitical shocks) as natural law. This structural invariance across
 *   power positions is the signature that distinguishes environmental
 *   instability from extraction constraints, where power differences produce
 *   different classifications.
 *
 * KEY AGENTS:
 *   - Powerless agents (immediate/local): victims of time-horizon compression; constrained by compressed decision windows; structurally unable to access exits that exist on longer timescales
 *   - Moderate agents (biographical/regional): partially buffer volatility through adaptation but consume resources doing so; face trade-off between adaptation and goal pursuit
 *   - Institutional agents (generational/global): benefit from volatility through arbitrage; invest in adaptive infrastructure; experience volatility as opportunity rather than constraint
 *   - Analytical observer (civilizational/universal): recognizes both genuine natural-law components (complex systems turbulence) and institutional design choices that amplify volatility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(environmental_instability_as_constraint, 0.18).
domain_priors:suppression_score(environmental_instability_as_constraint, 0.03).
domain_priors:theater_ratio(environmental_instability_as_constraint, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(environmental_instability_as_constraint, extractiveness, 0.18).
narrative_ontology:constraint_metric(environmental_instability_as_constraint, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(environmental_instability_as_constraint, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(environmental_instability_as_constraint, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(environmental_instability_as_constraint, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(environmental_instability_as_constraint, mountain).
narrative_ontology:human_readable(environmental_instability_as_constraint, "Environmental Instability as Perceived Constraint").
narrative_ontology:topic_domain(environmental_instability_as_constraint, "cognitive_psychology/decision_theory/environmental_dynamics").

domain_priors:emerges_naturally(environmental_instability_as_constraint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(environmental_instability_as_constraint, agents_with_adaptive_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POWERLESS/TRAPPED/IMMEDIATE/LOCAL (MOUNTAIN) — An agent facing rapid environmental changes (economic shocks, neighborhood violence, housing instability, information overload) experiences the constraint as immutable because the time available for decision-making is compressed below the threshold at which meaningful exit planning becomes cognizable. The constraint manifests as a perceptual ceiling on what futures are available to consider. This is not a natural law, but it appears as one from within the compressed time frame.
constraint_indexing:constraint_classification(environmental_instability_as_constraint, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MODERATE/CONSTRAINED/BIOGRAPHICAL/REGIONAL (MOUNTAIN) — An agent with some adaptive capacity (savings buffer, information access, social networks) still experiences environmental volatility as a binding constraint because adaptation costs rise with volatility magnitude. At intermediate volatility levels, the biographical horizon reveals the constraint's structure: adaptation is possible in principle but consumes resources and attention that would otherwise go to goal pursuit. The constraint appears immutable because breaking free from volatility response consumes the agent's entire adaptation budget.
constraint_indexing:constraint_classification(environmental_instability_as_constraint, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL/ARBITRAGE/GENERATIONAL/GLOBAL (MOUNTAIN) — Institutions with resources to invest in adaptive infrastructure (portfolio diversification, distributed supply chains, risk hedging, information advantage) do not experience environmental instability as a constraint — they experience it as a market opportunity (arbitrage). Yet even from this perspective, the underlying instability itself is perceived as natural law because the source of volatility is external to institutional control: macroeconomic cycles, climate dynamics, geopolitical shocks. The institution benefits from instability without perceiving that the instability itself is anything but an immutable environmental feature.
constraint_indexing:constraint_classification(environmental_instability_as_constraint, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL/ANALYTICAL/CIVILIZATIONAL/UNIVERSAL (MOUNTAIN) — From civilizational scope, environmental instability has genuine natural-law properties: complex adaptive systems exhibit emergent volatility that no agent can fully predict or control. Climate chaos, financial market turbulence, pandemic emergence, and information cascades all reflect deep structural features of interconnected systems. The mountain classification is not a false summit at this scale — the instability is truly immutable at the level of the system as a whole. However, this perspective masks a critical asymmetry: the constraint falls differentially on agents with different adaptive capacity, making it appear universal when it is actually stratified.
constraint_indexing:constraint_classification(environmental_instability_as_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(environmental_instability_as_constraint_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(environmental_instability_as_constraint, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(environmental_instability_as_constraint, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(environmental_instability_as_constraint, ExtMetricName, E),
    domain_priors:suppression_score(environmental_instability_as_constraint, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(environmental_instability_as_constraint),
    narrative_ontology:constraint_metric(environmental_instability_as_constraint, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(environmental_instability_as_constraint, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(environmental_instability_as_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Very low, consistent with mountain classification. The constraint does not extract in the technical sense — it does not transfer resources from victims to beneficiaries through a suppression mechanism. Instability creates differential vulnerability (agents without adaptive capacity suffer more), but this is a consequence of capacity asymmetry, not extraction. Suppression (0.03): Near zero. There are no suppression mechanisms preventing alternatives — no coercion, no prohibition, no enforcement. The barrier is temporal-cognitive: the time horizon compression prevents recognition of alternatives. Accessibility collapse (0.88): Very high. In high-volatility environments, the accessible decision space shrinks dramatically. Long-term plans become impossible; multi-step strategies collapse to reactive response; contingency options disappear from consideration. The decision-maker's sense of possibility contracts to the immediate moment. Resistance (0.12): Very low. The constraint is not enforced; agents are not forced to comply with anything. They simply cannot perceive exits when compressed into immediate-time frames. Theater ratio (0.15): Very low. Environmental instability has minimal performative content — the volatility is real, the time compression is real, the constraints it creates are real. There is no theatrical covering or legitimacy performance; the constraint is experienced as objective fact. The low theater_ratio confirms that this is not a piton (degraded mechanism) but a genuinely functional natural-law constraint.
 *
 * PERSPECTIVAL GAP:
 *   The critical gap is NOT between powerless and institutional perspectives (both classify as mountain) but between the immediate and civilizational time horizons. At immediate scope, the constraint appears as immutable environmental fact. At civilizational scope, analytical observation reveals that much of the volatility is not natural law but distributed choice: financial leverage, supply-chain fragility, information-system design, and adaptive-capacity hoarding are maintained by institutional actors and could be redesigned. Yet even the redesign would not eliminate the underlying natural-law component (complex systems turbulence). The gap reveals that environmental instability is partially a mountain and partially a constructed constraint, but the perceptual mechanism (time-horizon compression) is universal — it affects all observers with short enough decision windows. The mountain classification at immediate horizon is not a false summit; it is a structural truth about short-term perception. The question is whether the instability itself is natural law or institutional choice — and the answer appears to be: both.
 *
 * DIRECTIONALITY LOGIC:
 *   Standard directionality derivation does not apply cleanly because the constraint is mountain across all perspectives. In mountain constraints, directionality is not the primary analytical dimension — all observers perceive the same unchangeability regardless of their structural position. However, the declared beneficiaries (agents_with_adaptive_capacity) are present, which triggers FSM evaluation. The ambiguity in the beneficiary declaration itself (whether adaptive-capacity agents genuinely profit from volatility or merely suffer less) is routed through the omega variables. If adaptive capacity is genuinely profitable (agents prefer volatility), the constraint would reclassify to tangled_rope or snare depending on whether significant coordination function exists. The current mountain classification assumes that adaptive capacity is a defensive adaptation (suffering-reduction) rather than a profit mechanism. This is empirically testable: compare the value of adaptive-capacity infrastructure in stable vs. volatile environments. If adaptive-capacity premium rises with volatility, beneficiary extraction is occurring and FSM triggers toward tangled_rope or snare. If adaptive-capacity premium is stable across volatility conditions, the constraint is genuinely mountain with no extraction component.
 *
 * MANDATROPHY ANALYSIS:
 *   Environmental instability resolves the mandatrophy at the meta-level: the constraint is mountain because it is both natural law AND distributed extraction, but the perceptual mechanism (time-horizon compression) is universal. The mountain classification does not deny that adaptive-capacity agents benefit differentially — it recognizes that the benefit arises from capacity asymmetry, not from suppression or active extraction. This is categorically different from a snare, where the beneficiary actively maintains coercion to extract from victims. The mountain classification signals: 'This constraint is immutable from short time horizons and uniform across power positions, but the asymmetric impact it creates may be partially institutional.' The beneficiary declaration is not a false summit flag (FSM candidate) based on high coupling or obvious narrative beneficiary framing, but rather an empirical question: does the instability itself benefit adaptive-capacity agents, or does it merely harm capacity-poor agents? The distinction is tractable through measurement of adaptive-capacity value across volatility conditions. This is the mandatrophy resolution: environmental instability is mountain-with-ambiguity-about-beneficiary-intent, not false-summit-mountain. The omega variables are epistemic, not structural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_distributed_extraction,
    'Is environmental instability a genuine natural law emergent from complex systems, or a form of distributed extraction where agents with adaptive capacity impose volatility costs on agents without it?',
    'Historical analysis of volatility source: distinguish between exogenous shocks (climate, pandemics — true externalities) vs. endogenous volatility (financial leverage, information asymmetries, supply-chain fragility — designed and maintainable). Measurement of whether volatility magnitude is invariant to institutional design or responds to policy/infrastructure investment.',
    'If primarily natural law: constraint remains mountain across all perspectives. If significant component is institutional choice: constraint reclassifies to tangled_rope or snare from powerless perspectives, revealing beneficiary-victim structure among those with/without adaptive capacity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_distributed_extraction, empirical, 'Whether instability is natural law or distributed extraction via adaptive capacity asymmetry').

omega_variable(
    time_horizon_perceptual_collapse,
    'Does environmental instability truly prevent exit-option recognition, or does it collapse the time horizon in which recognition occurs, making exits that would be available on longer timescales appear unavailable on shorter ones?',
    'Cognitive science experiments: time-horizon extension protocols for agents in high-volatility conditions. Measurement of how strategic option sets change when decision frame is shifted from immediate to biographical or generational. Longitudinal tracking of agents who experience volatility reduction: do previously ''immutable'' constraints become changeable when instability drops?',
    'If perceptual collapse: the mountain classification is an artifact of compressed time horizons, not a structural property. Intervention via time-horizon extension (long-term planning support, scenario analysis, institutional commitments to stability) could reclassify from mountain to rope or scaffold. If truly immutable: cognitive interventions do not change the constraint''s classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(time_horizon_perceptual_collapse, empirical, 'Whether instability causes genuine immutability or perceptual time-horizon collapse').

omega_variable(
    information_asymmetry_vs_inherent_unpredictability,
    'How much of the perceived unpredictability of environmental conditions reflects genuinely unpredictable dynamics vs. information asymmetries where privileged agents have prediction capacity that less-privileged agents lack?',
    'Comparison of volatility perception across agent populations with different information access (e.g., traders with high-frequency data vs. retail investors, firms with early-warning systems vs. communities without). Measurement of prediction accuracy gaps. Analysis of whether instability is subjective (varies by information position) or objective (invariant across information positions).',
    'If significant component is information asymmetry: constraint structure includes an extraction mechanism (privileged agents extract value from unpredictability they can predict). Reclassifies toward tangled_rope or snare. If primarily objective unpredictability: mountain classification is warranted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_asymmetry_vs_inherent_unpredictability, empirical, 'Information asymmetry vs. inherent unpredictability in environmental volatility').

omega_variable(
    false_summit_beneficiary_ambiguity,
    'The declared beneficiary is ''agents_with_adaptive_capacity'' — but do these agents genuinely benefit from the instability itself, or do they simply suffer less from it than agents without adaptive capacity? Is the constraint creating asymmetric extraction, or is it creating a natural-law floor below which benefit cannot fall?',
    'Counterfactual analysis: would agents with adaptive capacity be better off in a stable environment (even if it constrained them in other ways), or do they actively profit from volatility? Measurement of adaptive-capacity value: is the premium paid for adaptive resources proportional to the cost of instability itself, or is there excess extraction?',
    'If adaptive capacity is profitable (agents prefer volatility): constraint is tangled_rope or snare with adaptive-capacity as beneficiary group. If adaptive capacity merely reduces loss: no true beneficiary exists (mountain with no beneficiaries). FSM does not trigger if no genuine extraction occurs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_beneficiary_ambiguity, empirical, 'Whether adaptive-capacity agents are beneficiaries or merely less-victimized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(environmental_instability_as_constraint, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(envinstab_tr_t0, environmental_instability_as_constraint, theater_ratio, 0, 0.1).
narrative_ontology:measurement(envinstab_tr_t2, environmental_instability_as_constraint, theater_ratio, 2, 0.13).
narrative_ontology:measurement(envinstab_tr_t5, environmental_instability_as_constraint, theater_ratio, 5, 0.15).

% Extraction over time
narrative_ontology:measurement(envinstab_be_t0, environmental_instability_as_constraint, base_extractiveness, 0, 0.16).
narrative_ontology:measurement(envinstab_be_t2, environmental_instability_as_constraint, base_extractiveness, 2, 0.17).
narrative_ontology:measurement(envinstab_be_t5, environmental_instability_as_constraint, base_extractiveness, 5, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(environmental_instability_as_constraint, resource_allocation).
narrative_ontology:affects_constraint(environmental_instability_as_constraint, time_horizon_compression).
narrative_ontology:affects_constraint(environmental_instability_as_constraint, information_cascade_formation).
narrative_ontology:affects_constraint(environmental_instability_as_constraint, adaptive_capacity_stratification).

% DUAL FORMULATION NOTE:
% Environmental instability as perceived constraint (this story) is upstream of three decomposed constraints: time-horizon compression (the perceptual mechanism by which instability prevents exit recognition), information-cascade formation (how distributed uncertainty in volatile environments creates feedback loops), and adaptive-capacity stratification (how differential capacity to buffer volatility creates differential constraint experience). This story's mountain classification is invariant; the downstream stories may show more variation across perspectives because they capture specific mechanisms rather than the environmental volatility itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
