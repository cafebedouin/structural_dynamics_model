% ============================================================================
% CONSTRAINT STORY: rehearsal_identity_merger
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rehearsal_identity_merger, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rehearsal_identity_merger
 *   human_readable: Rehearsal-Identity Merger in Autobiographical Narrative
 *   domain: epistemology/cognitive_science/philosophy_of_mind
 *
 * SUMMARY:
 *   The rehearsal-identity merger constraint describes a cognitive process
 *   where repeated performance of a preferred autobiographical narrative
 *   progressively erases the distinction between the performance (the story
 *   as told) and the performer (the self as experienced). This is not simple
 *   forgetting or motivated reasoning — it is a structural feature of memory
 *   reconsolidation where each act of recall modifies the memory trace, and
 *   repeated rehearsal in a consistent narrative frame causes the rehearsed
 *   version to overwrite the original experience. The constraint exhibits
 *   tangled_rope characteristics because it serves genuine coordination
 *   functions (narrative coherence enables social trust, psychological
 *   stability, and behavioral planning) while simultaneously extracting
 *   epistemic accuracy (the agent loses access to the original experience and
 *   cannot detect the loss from within the merged identity frame). The
 *   constraint is downstream of cognitive_efficiency_epistemic_cost (the
 *   mountain constraint that memory reconsolidation trades accuracy for
 *   efficiency) but adds a social-performative layer: rehearsal is not just
 *   internal recall but social performance, and the social feedback loop
 *   accelerates the merger. The theater_ratio (0.58) reflects that much of
 *   what appears to be autobiographical memory retrieval is actually
 *   narrative performance — the agent is not accessing stored experience but
 *   generating a story consistent with their current self-concept and social
 *   context.
 *
 * KEY AGENTS:
 *   - Identity-Fused Self: Primary victim (powerless/identity_locked) — the agent whose identity has merged with the rehearsed narrative; structurally mobile but functionally trapped by identity fusion; cannot detect the merger from within
 *   - Partially Aware Narrator: Secondary victim and beneficiary (moderate/constrained) — recognizes rehearsal shapes memory but continues because it serves coordination functions; bears epistemic cost but gains narrative coherence
 *   - Therapeutic Framework: Primary beneficiary (institutional/arbitrage) — narrative therapy and related approaches treat rehearsal as intervention mechanism; benefits from the constraint while maintaining reality-testing protocols
 *   - Metacognitive Training Coalition: Organized agents (organized/mobile) — cognitive scientists and educators developing interventions to increase awareness of memory malleability; building alternative pathways with sunset logic
 *   - Cultural Narrative System: Collective actor (moderate/constrained) — oral traditions and cultural memory practices rely on rehearsal for transmission but suffer progressive drift; mixed coordination and extraction
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing memory reconsolidation as immutable cognitive architecture rather than modifiable habit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rehearsal_identity_merger, 0.48).
domain_priors:suppression_score(rehearsal_identity_merger, 0.62).
domain_priors:theater_ratio(rehearsal_identity_merger, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rehearsal_identity_merger, extractiveness, 0.48).
narrative_ontology:constraint_metric(rehearsal_identity_merger, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(rehearsal_identity_merger, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rehearsal_identity_merger, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(rehearsal_identity_merger, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rehearsal_identity_merger, tangled_rope).
narrative_ontology:human_readable(rehearsal_identity_merger, "Rehearsal-Identity Merger in Autobiographical Narrative").
narrative_ontology:topic_domain(rehearsal_identity_merger, "epistemology/cognitive_science/philosophy_of_mind").

domain_priors:requires_active_enforcement(rehearsal_identity_merger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rehearsal_identity_merger, psychological_comfort).
narrative_ontology:constraint_beneficiary(rehearsal_identity_merger, social_coherence).
narrative_ontology:constraint_victim(rehearsal_identity_merger, epistemic_accuracy).
narrative_ontology:constraint_victim(rehearsal_identity_merger, behavioral_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IDENTITY-FUSED SELF (SNARE) — The agent whose identity has merged with the rehearsed narrative cannot detect the merger from within. Exit would require abandoning the self-concept that has been constructed through repeated performance. The agent is structurally mobile (could access external records, could engage in reality-testing) but functionally trapped by identity fusion. Maximum experienced extraction — the cost is epistemic accuracy and behavioral flexibility, but these losses are invisible from within the identity frame.
constraint_indexing:constraint_classification(rehearsal_identity_merger, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: PARTIALLY AWARE NARRATOR (TANGLED ROPE) — An agent with some meta-cognitive awareness of the rehearsal process. Recognizes that repeated telling shapes memory but continues the practice because it serves genuine coordination functions (social bonding, identity communication, meaning-making). Bears extraction cost (epistemic drift) but also gains coordination benefit (narrative coherence enables social trust and self-understanding). Constrained exit — could reduce rehearsal frequency or seek external validation, but at significant social and psychological cost.
constraint_indexing:constraint_classification(rehearsal_identity_merger, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THERAPEUTIC FRAMEWORK (ROPE) — Narrative therapy, coherence therapy, and related clinical approaches treat rehearsal as a coordination mechanism: constructing coherent life narratives reduces psychological distress and enables behavioral change. The framework benefits from the constraint — it is the intervention mechanism. Experiences minimal extraction because the therapeutic context explicitly monitors for pathological confabulation and maintains reality-testing protocols. Arbitrage exit — can shift to alternative therapeutic modalities if narrative approaches prove ineffective.
constraint_indexing:constraint_classification(rehearsal_identity_merger, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: METACOGNITIVE TRAINING COALITION (SCAFFOLD) — Cognitive scientists, educators, and clinicians developing interventions to increase awareness of memory malleability and narrative construction. See the merger as a temporary coordination problem with a sunset: as metacognitive literacy spreads (memory reconsolidation research, source monitoring training, epistemic humility norms), individuals gain tools to detect and correct rehearsal-driven drift. The constraint's extraction mechanism loses force as the population develops immunity through education. Estimated sunset: 20-40 years for widespread metacognitive literacy in educated populations.
constraint_indexing:constraint_classification(rehearsal_identity_merger, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CULTURAL NARRATIVE SYSTEM (TANGLED ROPE) — Collective memory practices, oral traditions, and cultural identity narratives rely on rehearsal for transmission but also suffer from progressive drift. The system coordinates group identity and intergenerational knowledge transfer (genuine function) while simultaneously accumulating distortion through repeated performance (extraction). Constrained exit — cultures can adopt historiographic rigor or external validation practices, but at the cost of narrative flexibility and emotional resonance that make stories transmissible.
constraint_indexing:constraint_classification(rehearsal_identity_merger, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COGNITIVE ARCHITECTURE VIEW (MOUNTAIN) — From a civilizational/universal perspective, memory reconsolidation and narrative construction are inherent features of human cognitive architecture. Every act of recall modifies the memory trace; coherent self-narrative is a functional requirement for planning and social coordination. The merger is not a contingent institutional arrangement but a structural property of how episodic memory works. However, the structural data contradicts this mountain classification — the engine will compute this as a false summit, revealing that the 'inherent to cognition' framing naturalizes what is actually a modifiable cognitive habit amenable to metacognitive intervention.
constraint_indexing:constraint_classification(rehearsal_identity_merger, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rehearsal_identity_merger_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rehearsal_identity_merger, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rehearsal_identity_merger, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rehearsal_identity_merger, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rehearsal_identity_merger_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint extracts epistemic accuracy and behavioral flexibility from the identity-fused agent. The extraction is substantial because the agent loses not just access to original experience but also the capacity to detect the loss — the merger is self-concealing. However, extraction is not maximal because the constraint also provides genuine coordination benefits (psychological stability, social coherence, meaning-making). The value reflects that roughly half of the constraint's effect is extractive overhead beyond necessary coordination cost. Suppression (0.62): Moderate-high. Significant barriers to exit include: (1) identity fusion makes exit psychologically equivalent to self-destruction, (2) social feedback loops reinforce the rehearsed narrative, (3) memory reconsolidation is automatic and largely unconscious, (4) metacognitive monitoring is effortful and culturally rare, (5) external validation sources may be inaccessible or ambiguous. But suppression is not total — some agents develop metacognitive awareness, therapeutic frameworks provide structured reality-testing, and digital traces create implicit external validation. Theater ratio (0.58): Moderate-high. Much of what appears to be memory retrieval is actually narrative performance. The agent is not accessing stored experience but generating a story consistent with current self-concept. The theater has increased over the interval as repeated rehearsal progressively replaces retrieval with reconstruction. The performative component is substantial but not dominant — some genuine memory traces persist, especially for emotionally salient or recently encoded events.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the identity_locked exit option in its canonical form: the identity-fused self is structurally mobile (has access to external validation sources, could engage in reality-testing, could reduce rehearsal frequency) but functionally trapped because their identity is constituted through the rehearsed narrative. Exit would require not just paying a cost but becoming a different person — abandoning the self-concept that gives their life coherence and meaning. The perspectival gap between the identity-fused self (snare) and the therapeutic framework (rope) reveals the constraint's dual nature: from within the merged identity, the constraint is pure extraction (epistemic accuracy is lost with no compensating benefit visible to the agent); from the therapeutic perspective, the constraint is coordination (narrative coherence is the benefit, and epistemic drift is monitored and corrected). The partially aware narrator (tangled_rope) occupies the middle ground — recognizes both the coordination function and the extraction cost, and chooses to continue rehearsal because the coordination benefit outweighs the epistemic cost at their current level of awareness. The metacognitive training coalition (scaffold) sees a sunset — as metacognitive literacy spreads, the constraint's extraction mechanism loses force. The analytical observer (mountain) risks naturalizing the constraint, but the structural data contradicts this — the merger is not an immutable feature of cognition but a modifiable habit.
 *
 * DIRECTIONALITY LOGIC:
 *   The identity-fused self is the primary victim with identity_locked exit — structurally mobile (could access external records, could engage in reality-testing) but functionally trapped because exit would require abandoning the self-concept constructed through rehearsal. The engine derives high d from victim status + identity_locked exit, producing high experienced extraction. The partially aware narrator is both victim and beneficiary with constrained exit — bears epistemic cost but gains coordination benefit, and could reduce rehearsal at significant psychological cost. The engine derives moderate d from mixed structural position. The therapeutic framework is the primary beneficiary with arbitrage exit — benefits from the constraint as intervention mechanism while maintaining reality-testing protocols. The engine derives low d from beneficiary status + arbitrage exit, producing low or negative experienced extraction. The metacognitive training coalition has mobile exit and sees a sunset — organized agents building alternative pathways. The cultural narrative system has constrained exit and mixed experience — coordinates group identity while accumulating distortion. The analytical observer risks naturalizing the constraint as immutable cognitive architecture, but the structural data reveals this as a false summit — memory reconsolidation is modifiable through metacognitive training.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint resolves the mandatrophy by demonstrating that rehearsal-identity merger has BOTH a genuine coordination function (narrative coherence enables social trust, psychological stability, and behavioral planning) AND asymmetric extraction (epistemic accuracy is lost, and the loss is invisible to the identity-fused agent). The coordination function is not a cover story — narrative coherence is a real psychological need, and rehearsal serves that need. But the extraction is also real — the agent loses access to original experience and cannot detect the loss from within the merged identity frame. The constraint requires active enforcement (social feedback loops, automatic memory reconsolidation, suppression of metacognitive monitoring) to maintain the merger. The tangled_rope classification prevents two errors: (1) treating rehearsal as pure coordination (rope) ignores the epistemic cost borne by the identity-fused agent, and (2) treating rehearsal as pure extraction (snare) ignores the genuine psychological benefits of narrative coherence. The perspectival gap is the diagnostic: the identity-fused self sees snare (maximum extraction, no visible benefit), the therapeutic framework sees rope (coordination with monitored epistemic cost), and the partially aware narrator sees tangled_rope (mixed experience). All three perspectives are structurally valid readings of the same constraint from different observation sites.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    detection_threshold_paradox,
    'At what point does the merger become structurally undetectable to the self, and is this threshold agent-specific or universal?',
    'Longitudinal studies comparing self-report narrative consistency against external records (journals, video, third-party accounts) across varying rehearsal frequencies; identification of individual differences in metacognitive monitoring capacity',
    'If threshold is universal and low: most autobiographical memory is post-hoc confabulation (snare from more perspectives). If threshold is high and agent-specific: metacognitive training can prevent merger (scaffold confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(detection_threshold_paradox, empirical, 'Whether merger undetectability threshold is universal or agent-specific').

omega_variable(
    coordination_extraction_ratio,
    'What proportion of narrative rehearsal serves genuine coordination functions (social bonding, meaning-making, behavioral guidance) versus pure extraction (epistemic drift with no compensating benefit)?',
    'Experimental manipulation of rehearsal frequency and narrative flexibility; measurement of social trust, psychological well-being, and behavioral coherence against epistemic accuracy metrics',
    'If coordination dominates: constraint is rope from more perspectives. If extraction dominates: constraint is snare from more perspectives. Current classification assumes mixed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_ratio, empirical, 'Ratio of coordination benefit to epistemic extraction in narrative rehearsal').

omega_variable(
    external_validation_accessibility,
    'Do most agents have practical access to external validation sources (journals, recordings, corroborating witnesses) that could detect rehearsal drift, or is such access rare?',
    'Survey of autobiographical record-keeping practices across cultures and socioeconomic strata; analysis of digital trace availability (photos, messages, location data) as implicit external validation',
    'If access is common: identity_locked classification is appropriate (structurally mobile but cognitively trapped). If access is rare: trapped classification is more accurate (structurally immobile).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(external_validation_accessibility, empirical, 'Prevalence of accessible external validation sources for autobiographical claims').

omega_variable(
    therapeutic_boundary_maintenance,
    'Do therapeutic frameworks that use narrative construction maintain sufficient reality-testing to avoid pathological confabulation, or does the therapeutic alliance itself suppress correction?',
    'Meta-analysis of narrative therapy outcomes; comparison of confabulation rates in narrative therapy vs control; examination of therapist training in distinguishing adaptive narrative flexibility from pathological distortion',
    'If boundaries are maintained: therapeutic framework perspective (rope) is valid. If boundaries fail: therapeutic framework is itself identity_locked (tangled_rope or snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(therapeutic_boundary_maintenance, empirical, 'Whether therapeutic narrative construction maintains epistemic boundaries').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rehearsal_identity_merger, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rim_theater_initial, rehearsal_identity_merger, theater_ratio, 0, 0.35).
narrative_ontology:measurement(rim_tr_t3, rehearsal_identity_merger, theater_ratio, 3, 0.42).
narrative_ontology:measurement(rim_tr_t6, rehearsal_identity_merger, theater_ratio, 6, 0.51).
narrative_ontology:measurement(rim_tr_t9, rehearsal_identity_merger, theater_ratio, 9, 0.58).

% Extraction over time
narrative_ontology:measurement(rim_be_t0, rehearsal_identity_merger, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(rim_be_t3, rehearsal_identity_merger, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(rim_be_t6, rehearsal_identity_merger, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(rim_be_t9, rehearsal_identity_merger, base_extractiveness, 9, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rehearsal_identity_merger, identity_coordination).
narrative_ontology:boltzmann_floor_override(rehearsal_identity_merger, 0.08).

% DUAL FORMULATION NOTE:
% This constraint is downstream of cognitive_efficiency_epistemic_cost (the mountain constraint that memory reconsolidation trades accuracy for efficiency). The upstream constraint establishes that memory modification is inherent to recall; the rehearsal-identity merger adds the social-performative layer that accelerates the modification and makes it self-concealing. The two constraints have different ε values (cognitive_efficiency_epistemic_cost ≈ 0.08 as a mountain; rehearsal_identity_merger = 0.48 as a tangled_rope) because they measure different structural phenomena: the upstream constraint measures the inherent accuracy-efficiency tradeoff in memory reconsolidation, while this constraint measures the additional extraction introduced by repeated social performance in a consistent narrative frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
