% ============================================================================
% CONSTRAINT STORY: somatic_focusing_awareness
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_somatic_focusing_awareness, []).

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
 *   constraint_id: somatic_focusing_awareness
 *   human_readable: Somatic Focusing Awareness: The Constraint of Felt Presence
 *   domain: social/psychological/embodied_practice
 *
 * SUMMARY:
 *   Somatic Focusing Awareness names a perennial constraint that binds all
 *   embodied beings: the impossibility of directly forcing a change in felt
 *   sensation without creating secondary dysregulation. The practice of
 *   'staying with feelings through interested curiosity without trying to
 *   change them or force communication' describes how humans can work with
 *   this constraint productively. Rather than fighting the constraint (which
 *   amplifies dysregulation), the practice coordinates with it — using the
 *   body's own signaling system to process information. This constraint
 *   exhibits the structure of a Mountain (natural law of embodied
 *   consciousness) with a Rope layer (coordinating with the constraint
 *   through learned attention). The theater ratio is exceptionally low (0.15)
 *   because the practice is fundamentally anti-performative: genuine somatic
 *   focusing requires the absence of audience, measurement pressure, or
 *   outcome demands. The constraint is immediate in its temporal scope
 *   (present-moment sensation) and local/internal in its spatial scope
 *   (individual nervous system), yet the principles are universal across all
 *   embodied beings.
 *
 * KEY AGENTS:
 *   - The Individual Body: Primary beneficiary (powerless/mobile) — direct access to embodied wisdom through non-forcing awareness
 *   - Authentic Emotional Processing: Primary beneficiary (abstract coordination function) — the body's own natural healing and integration capacity
 *   - Somatic Therapists and Facilitators: Secondary beneficiary (moderate/mobile) — ability to guide others toward their own embodied knowing
 *   - Medicalized Psychology: Institutional actor (institutional/constrained) — attempts to systematize and measure the practice, introducing theater through diagnostic frameworks
 *   - Western Embodiment Communities: Regional collective (moderate/mobile) — shared epistemic framework for understanding somatic awareness
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes the constraint as a law of embodied consciousness invariant across contexts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(somatic_focusing_awareness, 0.12).
domain_priors:suppression_score(somatic_focusing_awareness, 0.08).
domain_priors:theater_ratio(somatic_focusing_awareness, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(somatic_focusing_awareness, extractiveness, 0.12).
narrative_ontology:constraint_metric(somatic_focusing_awareness, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(somatic_focusing_awareness, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(somatic_focusing_awareness, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(somatic_focusing_awareness, resistance, 0.09).

% --- Constraint claim ---
narrative_ontology:constraint_claim(somatic_focusing_awareness, rope).
narrative_ontology:human_readable(somatic_focusing_awareness, "Somatic Focusing Awareness: The Constraint of Felt Presence").
narrative_ontology:topic_domain(somatic_focusing_awareness, "social/psychological/embodied_practice").

domain_priors:emerges_naturally(somatic_focusing_awareness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(somatic_focusing_awareness, practitioner_embodied_wisdom).
narrative_ontology:constraint_beneficiary(somatic_focusing_awareness, authentic_emotional_processing).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the capacity to sustain curious, non-forcing attention to felt sensation is a fundamental property of human nervous system function. The constraint that 'forcing attention away from sensations creates physiological dysregulation' is not a social convention but an intrinsic limit of embodied consciousness. ε=0.12, suppression=0.08, accessibility_collapse=0.88, resistance=0.09 all satisfy mountain thresholds. No beneficiary/victim structure — this is the ground truth of how bodies process information.
constraint_indexing:constraint_classification(somatic_focusing_awareness, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: INDIVIDUAL PRACTITIONER (ROPE) — The person learning to focus feels the constraint as coordination: their own body's signals must be honored, not forced. Trying to 'fix' sensations or manufacture communication creates additional dysregulation. The constraint solves the private action problem of 'how do I stay present to my own experience?' d≈0.35, f(d)≈0.30, σ=0.8 → χ≈0.03. Low extractiveness; the practitioner directly benefits from learning the practice.
constraint_indexing:constraint_classification(somatic_focusing_awareness, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: THERAPEUTIC COMMUNITY (ROPE) — Therapists and somatic facilitators recognize this constraint as pure coordination: the practice of 'interested curiosity without forcing' solves the collective action problem of how to help people access their own embodied wisdom. No asymmetric extraction — the coordination benefit flows to the person in the body. d≈0.30, f(d)≈0.20, σ=0.9 → χ≈0.03. Beneficiary + mobile → low chi.
constraint_indexing:constraint_classification(somatic_focusing_awareness, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: MEDICALIZED PSYCHOLOGY INSTITUTION (PITON) — When somatic focusing is institutionalized as 'Somatic Experiencing Protocol' or integrated into clinical psychology curricula, it becomes subject to diagnostic theater and measurement pressure. The practice itself (interested curiosity, non-forcing) persists, but the institutional frame adds performative elements: trauma narratives, diagnostic codes, compliance documentation. theater_ratio=0.15 seems low for a piton, but the piton emerges in the gap between the practice (low theater) and its institutional embedding (higher theater) — the institution maintains the practice label while degrading its functionality through measurement and manualization. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.08. The institution sees some coordination value but also experiences the constraint as slightly constraining (cannot fully algorithmize embodied presence).
constraint_indexing:constraint_classification(somatic_focusing_awareness, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(somatic_focusing_awareness_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(somatic_focusing_awareness, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(somatic_focusing_awareness, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(somatic_focusing_awareness, TR),
    TR >= 0.70.

:- end_tests(somatic_focusing_awareness_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The constraint itself creates no asymmetric extraction — it is a symmetrical property of embodied consciousness. All beings benefit equally from learning to work with this constraint. The low value reflects that the practice solves a coordination problem (how to be present to sensation) with zero rent or asymmetric advantage. Suppression (0.08): Minimal. The constraint suppresses only forcing and coercion — the very behaviors that create secondary dysregulation. Learning the practice actually reduces suppression by expanding what the body can do. Theater ratio (0.15): Exceptionally low. Somatic focusing is anti-performative by design. Genuine practice requires the absence of external measurement, audience expectation, or outcome pressure. The small increase over the interval (0.10→0.15) reflects institutional embedding — when the practice enters therapeutic contexts, some documentation and framing is necessary, but the core practice remains low-theater. Accessibility collapse (0.88): High. The constraint is nearly impossible to escape — all embodied beings experience it constantly. You cannot 'leave' your somatic sensations. The accessibility failure is total except through dissociation (which creates different problems). Resistance (0.09): Minimal. The constraint requires no enforcement apparatus. It self-enforces through the feedback loop of dysregulation — trying to fight it produces immediate feedback that fighting doesn't work.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is minimal because the constraint is nearly uniform across observers. The analytical observer might try to naturalize it as a mountain (which it is), but the individual practitioner doesn't experience this as a constraint at all — they experience it as freedom (the rope that liberates them from forcing). The therapeutic community sees coordination function (rope). The medicalized psychology institution is the one perspective where extraction appears (piton) — through the theater of diagnostic codes and measurement protocols layered onto the practice. But even this is not true extraction; it's institutional inertia and measurement pressure rather than asymmetric benefit-taking. The convergence of perspectives (all mostly rope or mountain) indicates a structurally pure constraint with minimal room for competing interpretations.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual practitioner: Beneficiary + mobile → d≈0.35, f(d)≈0.30. The body itself benefits; the person has agency. Therapeutic community: Beneficiary + mobile → d≈0.30, f(d)≈0.20. Facilitators benefit from the coordination function but are not extracting from those they serve. Medicalized psychology: Mixed. As institution, it benefits from systematizing the practice (institutional/arbitrage → d≈0.05), but the practice itself constrains institutional expansion (institutions cannot force embodied presence). The piton classification comes not from extraction but from the gap between the practice's inherent low-theater nature and the institution's higher-theater embedding. Analytical observer: Sees the constraint as a natural law (mountain), not as extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    forcing_boundary_definition,
    'What distinguishes ''interested curiosity'' (acceptable) from subtle forcing (unacceptable) when attending to somatic sensation?',
    'Phenomenological analysis of practitioner reports; physiological markers (breath pattern, muscle tension) during sessions coded for forcing vs curiosity; comparison of outcomes between high-curiosity vs high-effortful-focus conditions',
    'If boundary is clear and teachable: rope classification confirmed — pure coordination problem. If boundary is tacit and individual: constraint becomes more about the practitioner''s existing embodied wisdom (mountain) than a learned coordination mechanism (rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(forcing_boundary_definition, empirical, 'Distinction between interested curiosity and subtle forcing in somatic awareness').

omega_variable(
    mechanism_of_dysregulation_avoidance,
    'Does ''non-forcing curious attention'' prevent dysregulation through physiological pathway (vagal tone stabilization) or through relational pathway (the sense of being ''met'' by witnessing attention)?',
    'Biofeedback studies separating solo practice vs guided practice; measurement of nervous system markers (heart rate variability, skin conductance) in solo curious attention vs forced effort; comparison with placebo conditions',
    'If physiological: the constraint is a property of individual bodies (mountain). If relational: the constraint depends on real or imagined witnessing (rope requiring interpersonal coordination). This determines whether the practice can be learned alone or requires external guidance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mechanism_of_dysregulation_avoidance, empirical, 'Whether non-forcing awareness prevents dysregulation through physiology or relational witnessing').

omega_variable(
    cultural_variance_in_embodied_knowing,
    'Does ''interested curiosity to somatic sensation'' represent a universal human capacity or a culturally contingent epistemic practice emerging from Western psychological frameworks?',
    'Cross-cultural phenomenological study: interviews with practitioners in somatic traditions (yoga, tai chi, qigong, capoeira, dance, martial arts); analysis of attention metaphors and agency models in non-Western embodied practices; historical tracing of ''somatic awareness'' terminology',
    'If universal: strong evidence for mountain classification — all humans can access this mode. If culturally contingent: the constraint may be better understood as a rope within specific communities (Western psychological, embodiment-focused) rather than a natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_variance_in_embodied_knowing, conceptual, 'Whether somatic curiosity is universal or culturally contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(somatic_focusing_awareness, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soma_tr_t0, somatic_focusing_awareness, theater_ratio, 0, 0.1).
narrative_ontology:measurement(soma_tr_t1, somatic_focusing_awareness, theater_ratio, 1, 0.12).
narrative_ontology:measurement(soma_tr_t2, somatic_focusing_awareness, theater_ratio, 2, 0.15).

% Extraction over time
narrative_ontology:measurement(soma_be_t0, somatic_focusing_awareness, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(soma_be_t1, somatic_focusing_awareness, base_extractiveness, 1, 0.1).
narrative_ontology:measurement(soma_be_t2, somatic_focusing_awareness, base_extractiveness, 2, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(somatic_focusing_awareness, information_standard).
narrative_ontology:affects_constraint(somatic_focusing_awareness, nervous_system_dysregulation).
narrative_ontology:affects_constraint(somatic_focusing_awareness, trauma_integration_bottleneck).
narrative_ontology:affects_constraint(somatic_focusing_awareness, authentic_emotional_expression).

% DUAL FORMULATION NOTE:
% Somatic Focusing Awareness is downstream of the fundamental constraint that embodied consciousness cannot force sensations without creating dysregulation. This story focuses on the practice coordination layer (rope); the upstream constraint (the natural law of embodied feedback) is implicit in the mountain perspective. Related constraints in emotional processing and trauma integration depend on this awareness as a prerequisite.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
