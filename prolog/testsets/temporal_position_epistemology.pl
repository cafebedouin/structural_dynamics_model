% ============================================================================
% CONSTRAINT STORY: temporal_position_epistemology
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temporal_position_epistemology, []).

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
 *   constraint_id: temporal_position_epistemology
 *   human_readable: Temporal Position Epistemology: Duration-Dependent Understanding
 *   domain: epistemology/philosophy_of_time/cognitive_science
 *
 * SUMMARY:
 *   The temporal position epistemology constraint captures a fundamental
 *   tension in how understanding develops through duration. Certain truths
 *   are not hidden behind informational barriers but are constitutively
 *   unavailable to agents at their current temporal position—understanding
 *   requires becoming a different kind of thinker through lived duration.
 *   This creates an asymmetry between agents who can wait for understanding
 *   to mature (civilizational planners, longitudinal researchers) and agents
 *   forced to decide from their current temporal position (developmental
 *   learners, quarterly optimizers). The constraint exhibits genuine
 *   coordination function: the temporal filter prevents premature
 *   optimization and protects against naive interventions in complex
 *   developmental systems. But it also enables extraction: short-term
 *   thinkers capture value from positions they occupy before long-term
 *   consequences manifest, while long-term value creation bears the cost of
 *   decisions made with incomplete understanding. The theater_ratio (0.38)
 *   reflects moderate performative content: some institutional time-binding
 *   mechanisms (tenure clocks, apprenticeship durations) serve genuine
 *   developmental functions, while others are ritualistic gatekeeping. The
 *   constraint's extractiveness has increased over the interval (0.35 → 0.48)
 *   as institutional pressures for immediate legibility have intensified,
 *   forcing more decisions into timeframes shorter than the understanding
 *   they require.
 *
 * KEY AGENTS:
 *   - Developmental Learners: Primary victim (powerless/trapped at immediate horizon) — cannot exit temporal position; forced to decide before understanding matures
 *   - Long-Term Value Creation: Abstract victim (powerless/trapped) — collective good that cannot organize; bears cost of premature optimization
 *   - Short-Term Thinkers: Primary beneficiary (institutional/arbitrage) — extract value from positions occupied before consequences manifest
 *   - Quarterly Optimizers: Institutional beneficiary (institutional/arbitrage) — arbitrage the epistemic asymmetry between immediate and developmental understanding
 *   - Mid-Career Professionals: Mixed position (moderate/constrained) — experience both extraction (career pressure) and coordination (expertise gradients)
 *   - Civilizational Planners: Powerful agents (powerful/mobile) — can wait for understanding but still constrained by irreducibility of duration
 *   - Longitudinal Research Community: Organized agents (organized/mobile) — building institutional memory and synthetic duration pathways with scaffold logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent cognitive architectures as phenomenological necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temporal_position_epistemology, 0.48).
domain_priors:suppression_score(temporal_position_epistemology, 0.52).
domain_priors:theater_ratio(temporal_position_epistemology, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temporal_position_epistemology, extractiveness, 0.48).
narrative_ontology:constraint_metric(temporal_position_epistemology, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(temporal_position_epistemology, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temporal_position_epistemology, tangled_rope).
narrative_ontology:human_readable(temporal_position_epistemology, "Temporal Position Epistemology: Duration-Dependent Understanding").
narrative_ontology:topic_domain(temporal_position_epistemology, "epistemology/philosophy_of_time/cognitive_science").

domain_priors:requires_active_enforcement(temporal_position_epistemology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temporal_position_epistemology, short_term_thinkers).
narrative_ontology:constraint_beneficiary(temporal_position_epistemology, immediate_decision_makers).
narrative_ontology:constraint_beneficiary(temporal_position_epistemology, quarterly_optimizers).
narrative_ontology:constraint_victim(temporal_position_epistemology, long_term_value_creation).
narrative_ontology:constraint_victim(temporal_position_epistemology, developmental_learners).
narrative_ontology:constraint_victim(temporal_position_epistemology, civilizational_planners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPMENTAL LEARNER AT IMMEDIATE HORIZON (SNARE) — Trapped in current temporal position with no ability to accelerate developmental understanding. Cannot exit the constraint that certain truths require duration to comprehend. Experiences maximum extraction: forced to make decisions with incomplete understanding that can only be corrected through time they don't have access to.
constraint_indexing:constraint_classification(temporal_position_epistemology, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MID-CAREER PROFESSIONAL (TANGLED ROPE) — Constrained by career timelines and institutional pressures but also benefits from the coordination function: the constraint creates legitimate expertise gradients and prevents premature optimization. Experiences both extraction (pressure to decide before understanding matures) and coordination (the temporal filter protects against naive interventions).
constraint_indexing:constraint_classification(temporal_position_epistemology, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: QUARTERLY OPTIMIZER (ROPE) — Benefits from the constraint by extracting value from short-term positions before long-term consequences manifest. Experiences the temporal position barrier as coordination: it creates predictable cycles of reinterpretation that can be arbitraged. Net beneficiary of the epistemic asymmetry between immediate and developmental understanding.
constraint_indexing:constraint_classification(temporal_position_epistemology, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LONGITUDINAL RESEARCH COMMUNITY (SCAFFOLD) — Organized agents building institutional memory systems, longitudinal datasets, and developmental tracking methodologies see the constraint as temporary. Cohort studies, life-course research, and multi-generational datasets are creating pathways to compress temporal understanding. Estimated sunset: 20-40 years as predictive models trained on longitudinal data enable synthetic duration.
constraint_indexing:constraint_classification(temporal_position_epistemology, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CIVILIZATIONAL PLANNER (TANGLED ROPE) — Has resources and timeline to wait for understanding to mature, but still constrained by the irreducibility of duration-dependent insight. Benefits from the coordination function (temporal filter prevents premature lock-in) while bearing extraction cost (cannot accelerate critical insights needed for long-term planning).
constraint_indexing:constraint_classification(temporal_position_epistemology, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PHENOMENOLOGICAL VIEW (MOUNTAIN) — From a civilizational/universal perspective, duration-dependent understanding appears as an immutable feature of temporal consciousness: certain insights are constitutively unavailable to agents who have not yet undergone the developmental process. This perspective sees Bergsonian duration as irreducible. However, the structural data reveals this as potentially a false summit: the constraint may naturalize what are actually contingent cognitive architectures and institutional time-binding mechanisms.
constraint_indexing:constraint_classification(temporal_position_epistemology, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temporal_position_epistemology_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(temporal_position_epistemology, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(temporal_position_epistemology, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(temporal_position_epistemology, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temporal_position_epistemology_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint creates genuine asymmetric extraction: short-term thinkers capture career advancement, financial returns, and reputational benefits from positions they occupy before long-term understanding reveals their decisions as premature or misguided. Long-term value creation bears the cost through accumulated technical debt, path dependencies, and opportunity costs of premature lock-in. However, extraction is not maximal because the temporal filter also serves a coordination function—it prevents even more damaging naive interventions. Suppression (0.52): Moderate-high. Significant barriers prevent agents from accessing duration-dependent understanding: institutional time-binding mechanisms (tenure clocks, apprenticeship requirements), cognitive architecture limitations (working memory constraints, attention span), and economic pressures (quarterly reporting, short funding cycles). But suppression is not total—some agents can and do wait, and longitudinal research methodologies are creating alternative pathways. Theater ratio (0.38): Moderate. Some institutional time-binding is genuinely functional (medical residencies, PhD programs in complex domains), protecting against premature credentialing. But significant performative content exists: waiting periods that serve gatekeeping rather than developmental functions, ritualistic apprenticeships that could be compressed, and tenure clocks that measure duration rather than insight.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon—duration-dependent understanding—appears across the full classification range depending on temporal horizon and exit options. The developmental learner at immediate horizon sees pure extraction (Snare): trapped in current temporal position, forced to decide before understanding matures, with no exit. The quarterly optimizer sees coordination (Rope): the temporal barrier creates predictable reinterpretation cycles that can be arbitraged. The mid-career professional and civilizational planner see mixed coordination-extraction (Tangled Rope): genuine expertise gradients and protection against premature optimization, but also career pressure and planning constraints. The longitudinal research community sees a temporary problem with a sunset (Scaffold): synthetic duration pathways are being built. The analytical observer risks seeing an immutable phenomenological law (Mountain): Bergsonian duration as irreducible—but the structural data reveals this may naturalize contingent cognitive architectures and institutional time-binding mechanisms. The perspectival gap is not 'which type is correct?' but 'from which temporal position are you measuring?'
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim structure reflects the temporal asymmetry in value capture. Short-term thinkers and quarterly optimizers are beneficiaries: they occupy positions where decisions are made and value is extracted before the temporal horizon required for full understanding. Their arbitrage exit options reflect ability to move between positions faster than consequences manifest. Developmental learners and long-term value creation are victims: they bear the cost of decisions made with incomplete understanding, and cannot exit their temporal position (trapped) or the constraint itself. Mid-career professionals and civilizational planners occupy mixed positions: they experience both the coordination function (temporal filter creates legitimate expertise gradients) and the extraction cost (pressure to decide before understanding matures). The longitudinal research community sees a sunset: institutional memory systems and predictive models trained on longitudinal data are creating pathways to compress temporal understanding, though the omega variable on synthetic duration feasibility remains unresolved.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint resolves the mandatrophy by demonstrating that duration-dependent understanding has both genuine coordination function AND asymmetric extraction. The coordination function is real: the temporal filter prevents premature optimization in complex developmental systems (child development, skill acquisition, organizational maturation, ecological succession). Forcing immediate legibility in these domains causes genuine harm—the constraint protects against naive interventions. But the extraction is also real: the temporal barrier enables short-term thinkers to capture value from positions they occupy before consequences manifest, while long-term value creation bears the cost. The tangled_rope classification captures this irreducible duality: you cannot remove the extraction without destroying the coordination function (eliminating temporal filters would enable even more damaging premature optimization), and you cannot remove the coordination function without eliminating the extraction (if duration-dependent understanding were eliminable, the temporal asymmetry would disappear). The constraint is tangled because the same structural feature—irreducibility of duration—both protects developmental complexity and enables temporal arbitrage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    synthetic_duration_feasibility,
    'Can computational models trained on longitudinal data provide genuine duration-dependent understanding without the agent experiencing the duration?',
    'Comparison of decision quality between agents using longitudinal predictive models vs agents with lived experience; identification of insight types that resist synthetic compression',
    'If feasible: constraint is scaffold (technological sunset). If infeasible: constraint is mountain (phenomenological irreducibility). Current classification as tangled_rope reflects uncertainty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(synthetic_duration_feasibility, empirical, 'Whether synthetic duration can substitute for lived temporal experience').

omega_variable(
    retrospective_reinterpretation_mechanism,
    'Is retrospective reinterpretation a cognitive bug (correctable through better initial framing) or a feature of how understanding develops through time?',
    'Longitudinal studies tracking interpretation stability; identification of domains where initial understanding proves durable vs domains requiring temporal revision',
    'If bug: higher extractiveness (constraint is correctable institutional failure). If feature: lower extractiveness (constraint is coordination mechanism managing developmental complexity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retrospective_reinterpretation_mechanism, conceptual, 'Whether temporal reinterpretation is correctable or constitutive').

omega_variable(
    institutional_time_binding_necessity,
    'Do institutions that enforce temporal position barriers (tenure clocks, apprenticeship durations, waiting periods) protect against premature optimization or extract rents from developmental learners?',
    'Cross-institutional comparison of decision quality and innovation rates in systems with vs without temporal gatekeeping; identification of domains where time-binding improves vs degrades outcomes',
    'If protective: coordination function dominates (rope from more perspectives). If extractive: asymmetric extraction dominates (snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_time_binding_necessity, empirical, 'Whether institutional time-binding is coordination or extraction').

omega_variable(
    compounding_insight_threshold,
    'What proportion of valuable insights are genuinely duration-dependent vs merely appearing so due to institutional or cognitive path dependencies?',
    'Identification of insights that resist acceleration through any known pedagogical or technological intervention; measurement of insight acquisition rates across different temporal compression strategies',
    'If high proportion genuinely duration-dependent: mountain classification more accurate. If low proportion: tangled_rope or snare classification more accurate (most apparent duration-dependence is institutional artifact).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compounding_insight_threshold, empirical, 'Proportion of insights that are irreducibly duration-dependent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temporal_position_epistemology, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_pos_tr_t0, temporal_position_epistemology, theater_ratio, 0, 0.25).
narrative_ontology:measurement(temp_pos_tr_t15, temporal_position_epistemology, theater_ratio, 15, 0.32).
narrative_ontology:measurement(temp_pos_tr_t30, temporal_position_epistemology, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(temp_pos_be_t0, temporal_position_epistemology, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(temp_pos_be_t15, temporal_position_epistemology, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(temp_pos_be_t30, temporal_position_epistemology, base_extractiveness, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temporal_position_epistemology, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of perceptual_immediacy_bias (mountain: cognitive architecture favoring immediate perception) and compounding_illegibility (rope: coordination mechanism for managing complexity accumulation). The temporal position constraint inherits structural features from both: the perceptual bias creates the initial asymmetry (immediate understanding is privileged), while compounding illegibility creates the coordination need (temporal filters manage developmental complexity). This constraint has its own extractiveness value (0.48) reflecting the career and institutional asymmetries in temporal value capture, distinct from the upstream constraints' values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
