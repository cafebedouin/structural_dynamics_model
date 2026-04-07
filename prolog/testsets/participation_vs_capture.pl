% ============================================================================
% CONSTRAINT STORY: participation_vs_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_participation_vs_capture, []).

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
 *   constraint_id: participation_vs_capture
 *   human_readable: Participation vs Capture in Distributed Cognition
 *   domain: cognitive_science/philosophy_of_mind/technology_studies
 *
 * SUMMARY:
 *   The participation vs capture constraint redefines intellectual autonomy
 *   in the context of distributed cognition. Traditional autonomy was
 *   conceived as 'thinking alone' — independence from external influence.
 *   Distributed cognition reveals this as impossible: all thinking is already
 *   distributed across brain, body, environment, and social context. The new
 *   autonomy is not independence but frame-awareness: the capacity to
 *   recognize and exit cognitive frames rather than being captured by them.
 *   Capture is not degree of influence (all cognition is influenced) but
 *   inability to exit the frame. This constraint coordinates participation in
 *   extended mind systems (genuine cognitive enhancement through tool use,
 *   collaborative reasoning, offloaded memory) while extracting autonomy from
 *   users who cannot recognize frame boundaries. The legibility gradient —
 *   simplified interfaces that make complex systems accessible — is both
 *   coordination mechanism (enables participation) and extraction mechanism
 *   (hides frame structure, preventing frame-awareness). Users with
 *   frame-awareness experience distributed cognition as pure coordination
 *   (rope). Users captured by legibility gradients experience it as
 *   extraction (snare). The constraint is tangled_rope from the analytical
 *   perspective because both functions are real and structurally inseparable.
 *
 * KEY AGENTS:
 *   - Frame-Locked User: Primary victim (powerless/identity_locked) — identity constituted through cognitive frame; cannot recognize frame boundaries; structurally mobile but functionally trapped
 *   - Occasional Frame-Switcher: Secondary victim (moderate/constrained) — experiences both cognitive enhancement and frame-lock; can exit frames with effort; mixed extraction
 *   - Platform Designer: Primary beneficiary (institutional/arbitrage) — captures value from user engagement and lock-in; provides genuine cognitive tools; net beneficiary
 *   - Frame-Aware Practitioner: Secondary beneficiary (organized/mobile) — meta-cognitive training enables frame recognition and exit; experiences pure coordination
 *   - Analytical Observer: Reference perspective (analytical/analytical) — sees both coordination and extraction; tangled_rope classification
 *   - Epistemic Commons: Abstract victim (powerless/trapped) — collective capacity for frame-switching and cognitive flexibility; degraded by widespread frame-lock
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(participation_vs_capture, 0.42).
domain_priors:suppression_score(participation_vs_capture, 0.48).
domain_priors:theater_ratio(participation_vs_capture, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(participation_vs_capture, extractiveness, 0.42).
narrative_ontology:constraint_metric(participation_vs_capture, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(participation_vs_capture, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(participation_vs_capture, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(participation_vs_capture, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(participation_vs_capture, tangled_rope).
narrative_ontology:human_readable(participation_vs_capture, "Participation vs Capture in Distributed Cognition").
narrative_ontology:topic_domain(participation_vs_capture, "cognitive_science/philosophy_of_mind/technology_studies").

domain_priors:requires_active_enforcement(participation_vs_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(participation_vs_capture, users_with_frame_awareness).
narrative_ontology:constraint_beneficiary(participation_vs_capture, platform_designers).
narrative_ontology:constraint_beneficiary(participation_vs_capture, cognitive_tool_developers).
narrative_ontology:constraint_victim(participation_vs_capture, users_captured_by_legibility_gradient).
narrative_ontology:constraint_victim(participation_vs_capture, frame_locked_participants).
narrative_ontology:constraint_victim(participation_vs_capture, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRAME-LOCKED USER (SNARE) — Identity constituted through the cognitive frame provided by the distributed system. Cannot recognize frame boundaries because recognition would require stepping outside the frame that defines their cognitive identity. Structurally mobile (could use different tools) but functionally trapped by identity fusion with the frame. Maximum experienced extraction — the legibility gradient extracts cognitive autonomy while appearing as cognitive enhancement.
constraint_indexing:constraint_classification(participation_vs_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: OCCASIONAL FRAME-SWITCHER (TANGLED ROPE) — Experiences genuine cognitive enhancement from distributed cognition tools but also experiences moments of frame-lock. Can exit frames with effort and external prompting. Benefits from participation (extended cognition, offloaded memory, collaborative reasoning) while bearing costs (reduced frame-switching fluency, dependency on specific tool affordances). Mixed extraction — some agency, some capture.
constraint_indexing:constraint_classification(participation_vs_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM DESIGNER (ROPE) — Benefits from user engagement and lock-in. Experiences the constraint as coordination: designing affordances that make distributed cognition legible and accessible. Net beneficiary — captures value from user participation while providing genuine cognitive tools. Low effective extraction because the designer has full exit options and structural power.
constraint_indexing:constraint_classification(participation_vs_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FRAME-AWARE PRACTITIONER (ROPE) — Organized agents with meta-cognitive training (philosophers, cognitive scientists, contemplative practitioners) who can recognize and exit frames deliberately. Experience distributed cognition as pure coordination — tools extend capacity without capturing identity. Low extraction because frame-awareness provides exit capacity.
constraint_indexing:constraint_classification(participation_vs_capture, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both genuine coordination (distributed cognition extends human capacity) and asymmetric extraction (legibility gradients capture users who cannot recognize frame boundaries). The constraint coordinates participation in extended mind systems while extracting autonomy from those who lack frame-switching capacity. This is the reference perspective for the tangled_rope classification.
constraint_indexing:constraint_classification(participation_vs_capture, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(participation_vs_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(participation_vs_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(participation_vs_capture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(participation_vs_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42): Moderate. The constraint extracts cognitive autonomy (frame-switching capacity) from users who lack frame-awareness, but also provides genuine cognitive enhancement (extended memory, collaborative reasoning, tool-mediated problem-solving). The extraction is real but not maximal — many users retain some frame-switching capacity, and the tools provide real value. The value has increased over the interval as platform designs have optimized for engagement (which correlates with frame-lock) rather than autonomy. Suppression (0.48): Moderate. Barriers to frame-exit include identity fusion with the frame, cognitive habituation (frame-switching capacity atrophies from disuse), platform design that hides frame structure, and social norms that treat frame-lock as normal engagement. But suppression is not total — frame-awareness training exists, alternative tools are available, and some users spontaneously develop frame-switching capacity. Theater ratio (0.35): Moderate-low. Some performative elements exist (platforms claim to enhance autonomy while optimizing for engagement; users perform frame-awareness without genuine exit capacity) but the constraint is not primarily theatrical. The cognitive enhancement is real, and the frame-lock is real. Theater has increased as 'critical thinking' and 'media literacy' education has proliferated without actually teaching frame-recognition.
 *
 * PERSPECTIVAL GAP:
 *   The frame-locked user sees no constraint at all — the frame is invisible from within. They experience distributed cognition as natural and necessary, not as a choice. The occasional frame-switcher sees mixed coordination and extraction — moments of enhancement and moments of capture. The platform designer sees pure coordination — they are solving the legitimate problem of making complex systems accessible. The frame-aware practitioner sees pure coordination — distributed cognition extends their capacity without capturing their identity. The analytical observer sees tangled_rope — genuine coordination (cognitive enhancement) inseparably coupled with asymmetric extraction (frame-lock for those without awareness). The gap between the frame-locked user's invisibility and the analytical observer's structural view is the constraint's core dynamic. The frame-locked user cannot see what the analytical observer sees because seeing it would require exiting the frame that constitutes their cognitive identity.
 *
 * DIRECTIONALITY LOGIC:
 *   Frame-locked users are victims with identity_locked exit options. Their identity is constituted through the cognitive frame — exit would require becoming a different person, not just switching tools. The engine derives high d (≈0.89) from victim status + identity_locked exit, producing high experienced extraction. Platform designers are beneficiaries with arbitrage exit options — they can switch between platforms, design paradigms, and business models freely. The engine derives low d (≈0.05) from beneficiary status + arbitrage exit, producing low or negative experienced extraction. Occasional frame-switchers are victims (bear frame-lock costs) with constrained exit options (can exit frames but at significant cognitive effort cost). The engine derives moderate d (≈0.55) from victim status + constrained exit. Frame-aware practitioners are beneficiaries (experience cognitive enhancement without capture) with mobile exit options (can switch frames deliberately). The engine derives low d (≈0.35) from beneficiary status + mobile exit. The analytical observer uses canonical d for analytical power (≈0.72), producing moderate experienced extraction — the observer sees the extraction but is not fully captured by it.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that participation in distributed cognition is neither pure coordination (rope) nor pure extraction (snare) but structurally both. The coordination function is real: distributed cognition genuinely extends human cognitive capacity through tool use, collaborative reasoning, and offloaded memory. The extraction function is also real: legibility gradients that enable participation also hide frame structure, preventing frame-awareness and capturing users who cannot recognize boundaries. The two functions are inseparable because the same design features (simplified interfaces, consistent affordances, seamless integration) both enable participation and prevent frame-recognition. A platform that made all frame structure visible would be unusable for novices; a platform that hides all frame structure captures all users. The tangled_rope classification captures this structural inseparability. The constraint is not mislabeled coordination (it genuinely coordinates) and not mislabeled extraction (it genuinely extracts). It is both, and the perspectival gap reveals which function dominates for which agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    frame_awareness_trainability,
    'Is frame-awareness a trainable skill or a stable cognitive trait?',
    'Longitudinal studies of frame-switching training interventions; measurement of frame-recognition capacity before and after meta-cognitive instruction; correlation with other cognitive flexibility measures',
    'If trainable: constraint is scaffold (temporary coordination problem solvable through education). If stable trait: constraint is snare for low-trait individuals, rope for high-trait individuals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(frame_awareness_trainability, empirical, 'Whether frame-awareness can be developed through training').

omega_variable(
    legibility_gradient_necessity,
    'Are legibility gradients (simplified interfaces that hide complexity) necessary for distributed cognition accessibility, or do they constitute extractive design?',
    'Comparison of user outcomes across interface designs with different legibility/transparency tradeoffs; measurement of cognitive autonomy vs accessibility for novice vs expert users',
    'If necessary: extraction is coordination cost (lower base extractiveness). If unnecessary: extraction is design choice benefiting platforms (higher base extractiveness, possibly snare reclassification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legibility_gradient_necessity, conceptual, 'Whether interface simplification is necessary coordination or extractive design').

omega_variable(
    identity_lock_mechanism,
    'Does frame-lock operate through identity fusion (self-concept constituted through the frame) or through cognitive habituation (frame-switching capacity atrophies from disuse)?',
    'Experimental separation of identity-based vs habit-based frame-lock; measurement of frame-switching capacity after identity-relevant vs identity-neutral frame exposure; longitudinal tracking of frame-switching fluency with and without identity investment',
    'If identity fusion: exit requires identity transformation (identity_locked classification correct). If habituation: exit requires practice (constrained classification more accurate). Mechanism determines whether suppression is internalized or structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether frame-lock is identity-based or habit-based').

omega_variable(
    extended_mind_autonomy_tradeoff,
    'Is there an inherent tradeoff between cognitive extension (offloading to external systems) and cognitive autonomy (capacity to exit frames), or is the tradeoff an artifact of current tool design?',
    'Theoretical analysis of extended mind thesis; empirical comparison of autonomy outcomes across different distributed cognition architectures; identification of design patterns that preserve or enhance frame-switching capacity',
    'If inherent: some extraction is unavoidable (mountain component to the constraint). If artifact: extraction is contingent on design choices (pure tangled_rope, potentially resolvable through better tool design).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extended_mind_autonomy_tradeoff, conceptual, 'Whether cognitive extension necessarily trades off against autonomy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(participation_vs_capture, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(partic_tr_t0, participation_vs_capture, theater_ratio, 0, 0.2).
narrative_ontology:measurement(partic_tr_t3, participation_vs_capture, theater_ratio, 3, 0.28).
narrative_ontology:measurement(partic_tr_t6, participation_vs_capture, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(partic_be_t0, participation_vs_capture, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(partic_be_t3, participation_vs_capture, base_extractiveness, 3, 0.36).
narrative_ontology:measurement(partic_be_t6, participation_vs_capture, base_extractiveness, 6, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(participation_vs_capture, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of mode_constraint_topology (the mathematical structure of cognitive frames) and ai_as_fourth_node (AI as participant in distributed cognition). The upstream constraints establish the structural possibility space; this constraint describes the actual participation-vs-capture dynamics within that space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
