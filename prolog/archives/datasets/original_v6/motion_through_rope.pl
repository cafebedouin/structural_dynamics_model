% ============================================================================
% CONSTRAINT STORY: motion_through_rope
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_motion_through_rope, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: motion_through_rope
 *   human_readable: Motion Through Suffering as Harm Reduction
 *   domain: existential_psychology/moral_philosophy/clinical_intervention
 *
 * SUMMARY:
 *   The motion-through-suffering principle — that velocity through difficulty
 *   reduces total harm compared to dwelling or avoidance — represents a
 *   coordination constraint in the domain of unavoidable psychological pain.
 *   Voltaire's aphorism 'pass quickly through' becomes a testable temporal
 *   hypothesis when operationalized through rumination research, exposure
 *   therapy protocols, and grief processing timelines. The constraint
 *   coordinates how agents navigate suffering they cannot exit: acute grief
 *   from loss, trauma symptoms, phobic anxiety, existential dread. The
 *   structural claim is that duration of conscious dwelling amplifies harm
 *   through rumination loops, catastrophizing, and secondary emotional
 *   elaboration, while controlled velocity (neither avoidance nor endless
 *   processing) minimizes total damage. This is not a claim that suffering
 *   can be eliminated or that all pain is optional — the upstream constraint
 *   (suffering_ontology_mountain) establishes that some suffering is
 *   structurally unavoidable. Motion-through-rope addresses what to do given
 *   that unavoidability: move through rather than camp in the pain.
 *
 * KEY AGENTS:
 *   - Acute Grief Experiencers: Primary beneficiaries (powerless/trapped) — cannot exit the loss but benefit from temporal structure that reduces rumination amplification
 *   - Trauma Survivors in Treatment: Primary beneficiaries (moderate/constrained) — PTSD symptoms constrain but exposure therapy's velocity principle provides harm reduction
 *   - Exposure Therapy Participants: Primary beneficiaries (moderate/constrained) — phobic anxiety constrains but graduated exposure (controlled velocity) enables functional recovery
 *   - Clinical Psychology Field: Institutional beneficiary (institutional/mobile) — developed and tests the velocity principle across therapeutic modalities
 *   - Analytical Observer: Universal perspective (analytical/analytical) — sees the constraint as genuine coordination with minimal extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(motion_through_rope, 0.18).
domain_priors:suppression_score(motion_through_rope, 0.22).
domain_priors:theater_ratio(motion_through_rope, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(motion_through_rope, extractiveness, 0.18).
narrative_ontology:constraint_metric(motion_through_rope, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(motion_through_rope, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(motion_through_rope, rope).
narrative_ontology:human_readable(motion_through_rope, "Motion Through Suffering as Harm Reduction").
narrative_ontology:topic_domain(motion_through_rope, "existential_psychology/moral_philosophy/clinical_intervention").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(motion_through_rope, acute_grief_experiencers).
narrative_ontology:constraint_beneficiary(motion_through_rope, trauma_survivors_in_treatment).
narrative_ontology:constraint_beneficiary(motion_through_rope, exposure_therapy_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ACUTE GRIEVER (ROPE) — Trapped in immediate suffering with no exit from the loss itself, but benefits from the temporal structure: moving through grief stages rather than dwelling reduces rumination-amplified harm. The constraint coordinates passage through unavoidable pain.
constraint_indexing:constraint_classification(motion_through_rope, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: TRAUMA SURVIVOR IN TREATMENT (ROPE) — Constrained by PTSD symptoms and treatment access barriers, but exposure therapy's velocity principle (graduated, time-limited exposure rather than avoidance or flooding) provides genuine harm reduction. The constraint coordinates therapeutic passage.
constraint_indexing:constraint_classification(motion_through_rope, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CLINICAL PSYCHOLOGY FIELD (ROPE) — Mobile institutional actor that developed and tests the velocity principle across multiple therapeutic modalities. Benefits from the coordination function: the temporal structure enables systematic intervention design and outcome measurement.
constraint_indexing:constraint_classification(motion_through_rope, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, the velocity principle solves a genuine coordination problem: how to navigate unavoidable suffering with minimal amplification. Low extraction, low suppression, high functional content. The constraint is what it claims to be.
constraint_indexing:constraint_classification(motion_through_rope, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(motion_through_rope_tests).
:- end_tests(motion_through_rope_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The constraint extracts minimal rent — some therapeutic overhead (therapist fees, treatment time) but the core principle (move through rather than dwell) is accessible without institutional gatekeeping. Self-help applications, peer support, and cultural wisdom transmit the principle outside clinical contexts. The extraction is primarily the coordination cost of systematic application (therapy infrastructure) rather than asymmetric capture. Suppression (0.22): Low. The constraint does not suppress alternatives — agents can choose to dwell, avoid, or process indefinitely. The velocity principle is a recommendation backed by empirical evidence, not a coercive mandate. Cultural and individual variation in grief processing is tolerated. The modest suppression reflects that some therapeutic modalities (prolonged exposure, CPT) have protocol requirements that constrain timing, but these are opt-in treatment structures, not imposed constraints. Theater ratio (0.15): Very low. The constraint has high functional content. Exposure therapy protocols are empirically validated, rumination research is replicable, and grief processing timelines show consistent patterns across cultures. The theatrical component is minimal — some clinical ritual around 'processing' and 'closure' that may be performative, but the core velocity principle (graduated exposure, time-limited sessions, movement through stages) is mechanistically grounded.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all four perspectives classify as rope. The structural uniformity reflects that the velocity principle solves a genuine coordination problem (how to navigate unavoidable suffering with minimal amplification) without significant extraction or suppression. The powerless agent (acute griever) and the institutional agent (clinical field) agree on classification despite vastly different power and exit options because both experience the constraint as coordination rather than extraction. The absence of a perspectival gap is itself diagnostic: it confirms that the constraint is what it claims to be (rope), rather than a snare or tangled rope masquerading as coordination. The analytical observer's rope classification is not a false summit — the structural data supports the coordination claim.
 *
 * DIRECTIONALITY LOGIC:
 *   All four perspectives classify as rope because all agents are beneficiaries of the coordination function, and none are victims of extraction. The acute griever (powerless/trapped) benefits despite having no exit from the loss itself — the constraint coordinates passage through unavoidable pain. The trauma survivor (moderate/constrained) benefits despite treatment access barriers — the velocity principle provides genuine harm reduction when applied. The clinical field (institutional/mobile) benefits as the developer and validator of the principle. The analytical observer sees the constraint as what it claims to be: a low-extraction coordination mechanism for navigating unavoidable suffering. The uniform rope classification across all perspectives reflects that this is a structurally pure coordination constraint with minimal extractive overhead.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that not all therapeutic or philosophical principles are extractive. The velocity principle could have been a snare (therapists extracting rent by medicalizing normal grief, prolonging treatment unnecessarily) or a tangled rope (genuine harm reduction mixed with professional gatekeeping). The structural data shows it is neither. Extractiveness is low (0.18) because the principle is accessible outside clinical contexts. Suppression is low (0.22) because alternatives are tolerated. Theater is low (0.15) because the functional content is empirically validated. The constraint coordinates passage through unavoidable suffering without capturing the sufferer. The mandatrophy resolution is: some constraints in the suffering domain are mountains (unavoidable pain exists), some are ropes (velocity through pain reduces harm), and some are snares (rumination loops, avoidance spirals, therapeutic dependency). This constraint is a rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(motion_through_rope, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(motion_through_rope, information_standard).

% DUAL FORMULATION NOTE:
% Motion-through-rope is downstream of suffering_ontology_mountain. The upstream constraint establishes that some suffering is structurally unavoidable (mountain). Motion-through-rope addresses what to do given that unavoidability: the temporal structure of passage (velocity) affects total harm. The two constraints have different epsilon values because they make different structural claims. Suffering_ontology_mountain has epsilon near zero (unavoidable pain is not extractive). Motion-through-rope has epsilon 0.18 (the coordination mechanism has modest overhead but is not extractive).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
