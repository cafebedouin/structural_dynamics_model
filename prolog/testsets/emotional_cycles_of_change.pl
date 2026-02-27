% ============================================================================
% CONSTRAINT STORY: emotional_cycles_of_change
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_emotional_cycles_of_change, []).

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
 *   constraint_id: emotional_cycles_of_change
 *   human_readable: The Kelley-Connor Cycle of Change
 *   domain: psychology/behavioral_science
 *
 * SUMMARY:
 *   The Kelley-Connor Cycle of Change is a structural constraint operating at
 *   the intersection of organizational design and individual psychology. It
 *   describes a predictable emotional cost that emerges whenever adopters
 *   transition from established practices to new systems or behaviors. The
 *   constraint exhibits classic tangled-rope properties: it serves a genuine
 *   coordination function (helping organizations communicate that change is
 *   difficult and temporary) while simultaneously extracting psychological
 *   burden from adopters during the critical Valley of Despair phase.
 *   Organizations often invoke the model to justify inaction on emotional
 *   support, treating the Valley as inevitable rather than designable. The
 *   cycle involves suppressed alternatives (adopters cannot easily retreat
 *   once committed), asymmetric information (initiators know adoption will be
 *   temporary hardship; adopters discover it experientially), and deferred
 *   benefit (success is promised but not immediate). Different stakeholders
 *   experience radically different classifications: adopters trapped in the
 *   Valley see a Snare; middle managers implementing change see a Tangled
 *   Rope with constrained exit; executives see a Rope that their teams are
 *   implementing; change management professionals see a Scaffold with sunset
 *   logic; the model itself has become a Piton maintaining inertial theater
 *   around change rituals; and a naive analytical view risks naturalizing the
 *   emotional dysregulation as immutable neurobiology (false summit).
 *
 * KEY AGENTS:
 *   - Individual Adopter: Primary victim (powerless/trapped) — bears full psychological burden during Valley of Despair; sunk investment prevents exit
 *   - Middle Manager: Secondary victim (moderate/constrained) — manages upward/downward conflict; bears emotional labor
 *   - Change Initiator / Executive Leadership: Primary beneficiary (institutional/arbitrage) — benefits from eventual system adoption; has high exit optionality
 *   - Change Management Professionals: Organized support providers (organized/constrained) — bridge role with genuine scaffolding function but systemic constraint
 *   - Kelley-Connor Model as Framing: Piton institution (institutional/arbitrage) — explains the cycle but often justifies insufficient support
 *   - Organizational System: Institutional context (institutional/arbitrage) — benefits from adopter commitment during transition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(emotional_cycles_of_change, 0.38).
domain_priors:suppression_score(emotional_cycles_of_change, 0.52).
domain_priors:theater_ratio(emotional_cycles_of_change, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(emotional_cycles_of_change, extractiveness, 0.38).
narrative_ontology:constraint_metric(emotional_cycles_of_change, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(emotional_cycles_of_change, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(emotional_cycles_of_change, tangled_rope).
narrative_ontology:human_readable(emotional_cycles_of_change, "The Kelley-Connor Cycle of Change").
narrative_ontology:topic_domain(emotional_cycles_of_change, "psychology/behavioral_science").

domain_priors:requires_active_enforcement(emotional_cycles_of_change).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(emotional_cycles_of_change, change_initiators).
narrative_ontology:constraint_beneficiary(emotional_cycles_of_change, system_designers).
narrative_ontology:constraint_victim(emotional_cycles_of_change, adopter_psychological_burden).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL ADOPTER (SNARE) — At stage 3 (Valley of Despair), the adopter is trapped by sunk investment (time, emotional effort, organizational commitment) and cannot retreat to prior practices without visible failure. Maximum experienced extraction: the emotional cost is internalized while the organizational benefit is deferred and uncertain. No exit options; full psychological burden.
constraint_indexing:constraint_classification(emotional_cycles_of_change, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MIDDLE MANAGER (TANGLED ROPE) — Constrained by both accountability upward (to executives expecting adoption success) and pressure downward (from staff experiencing Valley of Despair). Benefits from eventual system competence and career advancement if adoption succeeds; bears extraction through emotional labor, conflict mediation, and performance pressure during the dip. Exit exists (job change) but costs are high.
constraint_indexing:constraint_classification(emotional_cycles_of_change, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CHANGE INITIATOR (ROPE) — Leadership that mandates system adoption experiences the constraint as pure coordination: communicating the change, managing adoption timelines, and celebrating early wins. The emotional cycle is abstracted and delegated downward. Leadership has arbitrage options (can pivot to different systems, can reallocate teams, can reframe timelines). Net beneficiary position.
constraint_indexing:constraint_classification(emotional_cycles_of_change, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CHANGE MANAGEMENT DISCIPLINE (SCAFFOLD) — The formal practice of change management (coaching, pacing, emotional support protocols, milestone celebrations) is designed as a temporary support structure to shorten the Valley of Despair and reduce its emotional cost. As adopters gain competence, the support framework is meant to dissolve. Sunset logic: mature adopters need less scaffolding. Theater is moderate (some ritual in change ceremonies, but genuine emotional support reduces pure performance).
constraint_indexing:constraint_classification(emotional_cycles_of_change, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MOTIVATIONAL FRAMING (PITON) — The Kelley-Connor model itself functions as a piton: a degraded explanatory framework that once served to help adopters understand that their Valley of Despair was normal and temporary (functional). But it now often serves as theater — organizations cite the model to justify inaction on emotional support, naturalizing the Valley as inevitable rather than designable. The model's primary function (emotional validation) has atrophied; its secondary function (excuse for insufficient support) persists through institutional inertia. Theater ratio is high because many change initiatives invoke the model but fail to implement the actual support scaffolds.
constraint_indexing:constraint_classification(emotional_cycles_of_change, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a cognitive/neurobiological perspective, some emotional dysregulation during complex skill acquisition is inherent to how human brains rewire under cognitive load. The amygdala responds to uncertainty and perceived loss; the prefrontal cortex is taxed by new procedural learning. This perspective sees the Valley of Despair as an immutable feature of human neurobiology — a constraint that cannot be designed away, only managed. However, the structural data reveals this as a false summit: the extractive component (suppressed alternative timelines, internalized psychological cost, deferred benefit) is contingent on organizational design choices, not immutable biology.
constraint_indexing:constraint_classification(emotional_cycles_of_change, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emotional_cycles_of_change_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(emotional_cycles_of_change, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emotional_cycles_of_change, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(emotional_cycles_of_change, TR),
    TR >= 0.70.

:- end_tests(emotional_cycles_of_change_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts psychological cost (stress, emotional dysregulation, time investment, identity disruption) from adopters. However, it is not maximal extraction because (a) the extraction is time-bounded (Valley ends), (b) adopters do gain real capability/efficiency benefits at stage 5 in many cases, (c) some organizations implement genuine support structures that reduce extraction. The value reflects that extraction is real but negotiable. Suppression (0.52): Moderate-high. Significant barriers exist: sunk investment (time, emotional effort, explicit commitment) makes retreat costly. Alternative timelines (phased adoption, opt-out, parallel systems) are often suppressed by organizational mandate. Fear of being seen as 'unable to adapt' prevents honest communication about difficulty. But suppression is not total — some organizations allow alternative adoption pathways, phased rollouts, and explicit opt-outs. Theater ratio (0.58): Moderate-high. Many change initiatives invoke the Kelley-Connor model as justification for insufficient emotional support ('This is normal, you'll get through the Valley'), which is theater — the model's explanatory function becomes performative cover for organizational inaction. However, genuine change management practices (mentoring, coaching, milestone celebrations) do reduce theater below 0.70.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence across the six types. The adopter in the Valley sees pure extraction (Snare) because they experience high cost with deferred/uncertain benefit and cannot exit. The middle manager sees mixed coordination and extraction (Tangled Rope) because they are simultaneously enabling team adaptation and managing organizational pressure. The executive sees coordination (Rope) because adoption is an implemented change — the emotional cost is abstracted to others. The change management discipline sees a temporary support structure (Scaffold) because good coaching is designed to reduce Valley depth over time, with sunset logic as adopters mature. The Kelley-Connor model itself has become a Piton — it no longer functions primarily as explanation (that was its original role) but as institutional theater that justifies insufficient support. The naive analytical view risks seeing an immutable natural law (Mountain) — that emotional dysregulation is inherent to skill acquisition — but this naturalizes what is actually a contingent organizational choice about how much support is provided. The gap reveals that the 'emotional cycle' is not a fixed constraint but a designable process whose shape depends on support structure choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position relative to the extraction flow. Change initiators (institutional/arbitrage) benefit from adoption and have high exit options — low d, negative chi. Individual adopters (powerless/trapped) bear psychological cost and cannot exit without penalty — high d, high chi. Middle managers (moderate/constrained) occupy a mixed position: constrained by both upward/downward pressure and partially able to buffer their teams through support structures — moderate d, moderate chi. Organized change management (organized/constrained) has agency to design support but is constrained by organizational timelines/budgets — moderate d. The Kelley-Connor model as institutional framing (institutional/arbitrage) has become self-protecting — it explains the cycle in a way that justifies organizational inaction, similar to how a system might invoke 'market forces' to justify exploitation.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: This constraint avoids mandatrophy through explicit perspectival pluralism. The Tangled Rope classification (primary) correctly identifies that the cycle has BOTH a genuine coordination function (helping organizations manage change at scale) AND asymmetric extraction (psychological cost concentrated on adopters). The classification resists both extremes: (1) pure extraction (Snare) because adopters do gain real benefits and organizations provide some support, and (2) pure coordination (Rope) because the model is often invoked to justify inaction on emotional support, shifting cost downward. The Piton perspective correctly identifies that the model's explanatory function (making Valley inevitable/acceptable) is being used to maintain organizational theater around change management, rather than to genuinely support adopters. The false summit Mountain perspective is exposed by the directionality analysis: if the Valley were truly immutable neurobiology, then support structure design would have no effect — but the omega variable on valley_depth_designability reveals that support is genuinely effective, proving the constraint is contingent on organizational choice, not immutable law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    valley_depth_designability,
    'How much of the Valley of Despair''s emotional depth is inherent to skill acquisition versus contingent on organizational support structure?',
    'Comparative outcome analysis: measuring emotional dysregulation in high-support vs low-support change initiatives for identical system transitions; neurobiological baselines vs context-dependent variance',
    'If >70% contingent: Valley is a Snare that could be redesigned to Rope or Scaffold through better support. If >70% inherent: Mountain classification is correct and support focus should shift to management rather than elimination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(valley_depth_designability, empirical, 'Extent to which Valley depth is designable versus inherent').

omega_variable(
    adoption_timeline_compression,
    'Can the Kelley-Connor five-stage cycle be structurally compressed through phased adoption, mentorship, or alternative learning pathways, or does it represent an irreducible cognitive schedule?',
    'Longitudinal comparison of adoption curves across different pedagogical approaches (cohort-based, self-paced, peer-led, expert-guided); identification of whether stage durations are characteristic properties or design-dependent variables',
    'If compressible: Valley is a Tangled Rope with negotiable suppression. If not: stage sequence reflects cognitive load patterns independent of design choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adoption_timeline_compression, empirical, 'Whether stage durations can be compressed through design').

omega_variable(
    downstream_benefit_realization,
    'Do adopters actually realize the promised efficiency/capability gains at stage 5, or does the model function as a sunk-cost justification for failed adoption?',
    'Outcome measurement: comparing actual performance improvements post-adoption against pre-adoption predictions; tracking real productivity gains vs perceptual gains; identifying reversions to prior systems',
    'If gains are real: extractive cost is temporary trade for legitimate benefit (Tangled Rope correct). If gains are illusory or marginal: extraction is not repaid (Snare more accurate than Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(downstream_benefit_realization, empirical, 'Whether promised downstream benefits materialize').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(emotional_cycles_of_change, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emcycle_tr_t0, emotional_cycles_of_change, theater_ratio, 0, 0.35).
narrative_ontology:measurement(emcycle_tr_t3, emotional_cycles_of_change, theater_ratio, 3, 0.5).
narrative_ontology:measurement(emcycle_tr_t6, emotional_cycles_of_change, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(emcycle_be_t0, emotional_cycles_of_change, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(emcycle_be_t3, emotional_cycles_of_change, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(emcycle_be_t6, emotional_cycles_of_change, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(emotional_cycles_of_change, enforcement_mechanism).
narrative_ontology:affects_constraint(emotional_cycles_of_change, organizational_learning_plateau).
narrative_ontology:affects_constraint(emotional_cycles_of_change, identity_resistance_to_change).
narrative_ontology:affects_constraint(emotional_cycles_of_change, sunk_cost_escalation).

% DUAL FORMULATION NOTE:
% The Kelley-Connor Cycle can be decomposed into structurally distinct constraints: (1) the neurobiological reality of cognitive load during skill acquisition (closer to Mountain), and (2) the organizational choice to suppress alternative adoption timelines and insufficient support scaffolding (closer to Snare). This story addresses the organizational/psychological hybrid (Tangled Rope). The neurobiological upstream constraint would have lower extractiveness (~0.15) and higher naturalization claims; the organizational downstream constraint (suppressed alternatives) would have higher extractiveness (~0.52). Both are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
