% ============================================================================
% CONSTRAINT STORY: structural_opacity_of_self
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_opacity_of_self, []).

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
 *   constraint_id: structural_opacity_of_self
 *   human_readable: Structural Opacity of Self-Knowledge
 *   domain: cognitive_science/evolutionary_psychology/philosophy_of_mind
 *
 * SUMMARY:
 *   The structural opacity of self-knowledge creates a binding mechanism
 *   where cognitive errors and psychological dysfunctions arrive
 *   pre-naturalized — experienced as intrinsic properties of the self rather
 *   than as contingent patterns that could be examined and modified. This
 *   constraint operates through identity fusion: the agent's self-concept is
 *   constituted through the very cognitive patterns that would need to be
 *   examined for correction to occur. Introspection systematically fails
 *   because it queries the same system that generates the errors, returning
 *   confabulated explanations that feel authoritative but miss the actual
 *   causal mechanisms. The constraint exhibits high suppression (0.72)
 *   because exit requires not just acquiring new information but dissolving
 *   the identity frame through which the information would be interpreted.
 *   The theater ratio (0.68) reflects that introspective self-examination
 *   feels like genuine investigation but produces systematically unreliable
 *   outputs — the ritual of 'looking inward' is maintained through
 *   phenomenological vividness despite poor epistemic track record. The
 *   constraint's extractiveness has increased over the biographical interval
 *   as initial childhood patterns become more deeply entrenched and
 *   elaborated into comprehensive self-narratives.
 *
 * KEY AGENTS:
 *   - Identity-Locked Self: Primary victim (powerless/identity_locked) — agent whose self-concept is fused with dysfunction; cannot exit without identity dissolution; bears full cost of cognitive errors while experiencing them as natural
 *   - Therapeutic Client: Secondary victim (moderate/constrained) — agent attempting self-examination with external scaffolding; faces high psychological costs to exit but has partial access to corrective frameworks
 *   - Ego Defense System: Primary beneficiary (institutional/arbitrage) — psychological homeostasis mechanisms that maintain opacity to protect against overwhelming affect; experiences constraint as beneficial regulation
 *   - Metacognitive Training Coalition: Organized agents (organized/mobile) — mindfulness interventions, CBT protocols, psychedelic therapy, contemplative neuroscience building alternative pathways to self-knowledge that bypass introspective opacity
 *   - Introspective Method: Institutional actor (institutional/constrained) — classical introspection as folk-psychological method; maintained through cultural inertia despite poor epistemic reliability (piton perspective)
 *   - Evolutionary Psychologist: Analytical observer (analytical/analytical) — sees opacity as adaptive feature written into cognitive architecture by selection; risks naturalizing contingent design features as immutable laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_opacity_of_self, 0.58).
domain_priors:suppression_score(structural_opacity_of_self, 0.72).
domain_priors:theater_ratio(structural_opacity_of_self, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_opacity_of_self, extractiveness, 0.58).
narrative_ontology:constraint_metric(structural_opacity_of_self, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(structural_opacity_of_self, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_opacity_of_self, snare).
narrative_ontology:human_readable(structural_opacity_of_self, "Structural Opacity of Self-Knowledge").
narrative_ontology:topic_domain(structural_opacity_of_self, "cognitive_science/evolutionary_psychology/philosophy_of_mind").

domain_priors:requires_active_enforcement(structural_opacity_of_self).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_opacity_of_self, psychological_homeostasis).
narrative_ontology:constraint_beneficiary(structural_opacity_of_self, ego_defense_mechanisms).
narrative_ontology:constraint_victim(structural_opacity_of_self, self_corrective_capacity).
narrative_ontology:constraint_victim(structural_opacity_of_self, therapeutic_intervention_targets).
narrative_ontology:constraint_victim(structural_opacity_of_self, metacognitive_accuracy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IDENTITY-LOCKED SELF (SNARE) — The agent whose cognitive errors have fused with their self-concept cannot exit because their identity is constituted through the dysfunction. Introspection returns the error wearing the clothes of truth. Maximum extraction: the agent pays the cost of the dysfunction while experiencing it as natural or necessary. Identity-locked rather than trapped because the barriers are internal — the agent could theoretically access corrective information but cannot integrate it without identity dissolution.
constraint_indexing:constraint_classification(structural_opacity_of_self, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: THERAPEUTIC CLIENT (TANGLED ROPE) — Agent in therapy or self-examination who has partial access to corrective frameworks but faces high costs to exit: confronting trauma, revising self-narrative, tolerating cognitive dissonance. Experiences both coordination (therapeutic alliance provides scaffolding for self-examination) and extraction (the opacity itself resists examination, creating cycles of insight and relapse). Constrained exit: can leave but at significant psychological cost.
constraint_indexing:constraint_classification(structural_opacity_of_self, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EGO DEFENSE SYSTEM (ROPE) — The psychological homeostasis mechanisms (rationalization, denial, projection) that maintain opacity are the primary beneficiary. From their perspective, opacity is pure coordination: protecting the self from overwhelming affect, maintaining functional coherence, enabling daily operation despite unresolved conflicts. Arbitrage exit: can modulate defenses flexibly in response to threat level. Experiences the constraint as beneficial regulation, not extraction.
constraint_indexing:constraint_classification(structural_opacity_of_self, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: METACOGNITIVE TRAINING COALITION (SCAFFOLD) — Organized agents (mindfulness-based interventions, cognitive behavioral therapy protocols, psychedelic-assisted therapy, contemplative neuroscience) building alternative pathways to self-knowledge that bypass introspective opacity. See the constraint as temporary: third-person behavioral data, implicit association tests, and altered states provide external mirrors that reveal what introspection cannot. Sunset mechanism: as metacognitive training becomes culturally widespread and neuroimaging provides objective correlates of subjective states, the opacity loses its extraction power.
constraint_indexing:constraint_classification(structural_opacity_of_self, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INTROSPECTIVE METHOD (PITON) — Classical introspection as a method for self-knowledge is largely theatrical in the post-Nisbett-Wilson era. The method persists in folk psychology and popular self-help despite decades of evidence that introspective reports systematically misattribute causation, confabulate reasons, and miss implicit processes. Maintained through cultural inertia and the phenomenological vividness of introspective experience, not through epistemic reliability. High theater ratio: the ritual of 'looking inward' feels authoritative but produces systematically unreliable outputs.
constraint_indexing:constraint_classification(structural_opacity_of_self, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: EVOLUTIONARY PSYCHOLOGIST / NATURAL LAW VIEW (MOUNTAIN) — From an evolutionary perspective, introspective opacity is an adaptive feature, not a bug: accurate self-knowledge would interfere with strategic self-presentation, coalition maintenance, and motivated reasoning that serves reproductive fitness. The constraint appears immutable because it is written into the architecture of human cognition by selection pressures. However, this perspective risks naturalizing what may be a contingent design feature that can be modified through cultural evolution, neuroplasticity, or technological augmentation. The engine's false summit detector will test whether the 'adaptive opacity' framing is a structural law or a naturalized institutional arrangement.
constraint_indexing:constraint_classification(structural_opacity_of_self, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_opacity_of_self_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(structural_opacity_of_self, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(structural_opacity_of_self, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_opacity_of_self, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(structural_opacity_of_self, TR),
    TR >= 0.70.

:- end_tests(structural_opacity_of_self_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The identity-locked agent pays significant costs — relationship dysfunction, career limitations, emotional dysregulation, missed opportunities for growth — while experiencing the dysfunction as natural or necessary. The extraction is substantial but not maximal because some agents do achieve partial exit through therapy or life disruption, and the homeostatic benefits (affect regulation, coherence maintenance) are genuine even if they come at high cost. Suppression (0.72): High. Exit barriers are primarily internal but structurally robust: confronting naturalized dysfunction requires tolerating intense affect, revising core self-narratives, maintaining motivation through cycles of insight and relapse, and accessing specialized therapeutic frameworks. The suppression is not total — therapeutic interventions do work for some agents — but it is severe enough that most identity-locked agents remain trapped throughout their biographical timespan. Theater ratio (0.68): Moderate-high. Introspective self-examination feels authoritative and is culturally validated as the primary method for self-knowledge, but it systematically produces confabulated explanations, misattributes causation, and misses implicit processes. The theater has increased over the interval as the agent elaborates increasingly sophisticated rationalizations that feel like insight but maintain the underlying opacity.
 *
 * PERSPECTIVAL GAP:
 *   The identity-locked self experiences pure extraction (Snare) — trapped by internal barriers that feel insurmountable from within the identity frame. The therapeutic client experiences mixed coordination and extraction (Tangled Rope) — the therapeutic alliance provides genuine scaffolding, but the opacity itself resists examination. The ego defense system experiences pure coordination (Rope) — opacity is a beneficial regulation mechanism protecting against overwhelming affect. The metacognitive training coalition sees a temporary problem with a sunset (Scaffold) — third-person data and altered states are building alternative pathways that bypass introspective opacity. The introspective method sees its own degraded ritual (Piton) — maintained through cultural inertia and phenomenological vividness despite poor epistemic reliability. The analytical observer risks seeing an immutable natural law (Mountain) — opacity is adaptive architecture — but the structural data may reveal this as naturalization of a contingent design feature that can be modified through cultural evolution or technological augmentation.
 *
 * DIRECTIONALITY LOGIC:
 *   The identity-locked self is a victim with identity_locked exit options, producing high directionality (d ≈ 0.89) and high experienced extraction. The therapeutic client is a victim with constrained exit, producing moderate-high directionality (d ≈ 0.85). The ego defense system is a beneficiary with arbitrage exit, producing low directionality (d ≈ 0.05) and experiencing the constraint as beneficial coordination. The metacognitive training coalition is organized with mobile exit, producing moderate directionality (d ≈ 0.55) — they see both the problem and the solution path. The introspective method is institutional with constrained exit, producing moderate directionality (d ≈ 0.65) — it persists despite poor performance because alternatives haven't fully replaced it. The analytical observer uses the analytical context, producing moderate-high directionality (d ≈ 0.72) by default, but risks naturalizing the constraint as immutable when it may be contingent.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that the same opacity mechanism serves genuine homeostatic functions (coordination) while simultaneously extracting costs from the agent's self-corrective capacity (extraction). The ego defense system's perspective (Rope) is not false — affect regulation and coherence maintenance are real benefits. The identity-locked self's perspective (Snare) is also not false — the costs of missed self-correction are real and severe. The constraint is genuinely hybrid: it coordinates psychological stability while extracting from epistemic accuracy and growth capacity. The mandatrophy resolution shows that 'dysfunction naturalized before awareness can examine it' is both a protective mechanism and an extractive trap, depending on the observer's structural position and time horizon. The Tangled Rope classification at the moderate/constrained perspective captures this hybridity most accurately, while the Snare classification at the powerless/identity_locked perspective captures the experienced severity for agents who cannot exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    introspective_access_floor,
    'Is there an irreducible floor to introspective opacity determined by neural architecture, or is all opacity contingent on developmental and cultural factors?',
    'Cross-cultural studies of metacognitive accuracy; longitudinal studies of contemplative practitioners; neuroimaging of self-referential processing in expert meditators vs controls; developmental trajectories of metacognitive accuracy',
    'If architectural floor exists: some opacity is mountain (immutable). If contingent: all opacity is snare or tangled_rope (modifiable through intervention).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(introspective_access_floor, empirical, 'Whether introspective opacity has an architectural floor or is fully contingent').

omega_variable(
    therapeutic_access_threshold,
    'What proportion of identity-locked agents can achieve exit through therapeutic intervention vs remaining structurally trapped?',
    'Meta-analysis of therapy outcome studies stratified by trauma severity and identity fusion measures; longitudinal tracking of clients who terminate therapy prematurely vs those who complete treatment; comparison of insight-oriented vs exposure-based vs psychedelic-assisted modalities',
    'If threshold is low (<30%): most identity-locked agents are functionally trapped, and the snare classification understates severity. If high (>70%): the constraint is more tangled_rope than snare for most agents.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(therapeutic_access_threshold, empirical, 'Proportion of identity-locked agents who can exit via therapy').

omega_variable(
    implicit_explicit_divergence_mechanism,
    'Does implicit-explicit belief divergence reflect parallel processing systems (dual-process theory) or sequential developmental layering where implicit beliefs are childhood residue?',
    'Developmental studies tracking implicit attitude formation; intervention studies testing whether implicit attitudes can be directly modified or only overridden; neuroimaging of implicit vs explicit belief activation',
    'If parallel systems: divergence is architectural (mountain component). If developmental layering: divergence is historical accident (snare component) that can be resolved through re-consolidation or memory reconsolidation therapy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_explicit_divergence_mechanism, conceptual, 'Whether implicit-explicit divergence is architectural or developmental').

omega_variable(
    naturalization_reversibility,
    'Once a dysfunction is naturalized (experienced as ''just how I am''), what interventions can denaturalize it without triggering defensive re-naturalization?',
    'Clinical trials of schema therapy, internal family systems, and psychedelic-assisted therapy measuring naturalization reversal rates; identification of intervention components that predict sustained denaturalization vs relapse to naturalized framing',
    'If reversibility is high with appropriate intervention: scaffold perspective is validated (sunset is real). If low: identity-locked agents remain trapped, and the snare classification is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naturalization_reversibility, empirical, 'Reversibility of dysfunction naturalization').

omega_variable(
    metacognitive_training_generalization,
    'Do metacognitive skills trained in one domain (e.g., mindfulness meditation, CBT for anxiety) generalize to other domains of self-opacity, or is each dysfunction domain-specific?',
    'Transfer studies measuring whether mindfulness training improves implicit bias awareness, whether CBT for one disorder improves metacognitive accuracy for unrelated cognitive distortions, whether psychedelic-assisted therapy produces domain-general increases in psychological flexibility',
    'If generalization is strong: metacognitive training is a genuine scaffold with broad sunset. If domain-specific: each opacity requires separate intervention, and the scaffold perspective is overly optimistic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metacognitive_training_generalization, empirical, 'Domain-generality of metacognitive training effects').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_opacity_of_self, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_childhood, structural_opacity_of_self, theater_ratio, 0, 0.5).
narrative_ontology:measurement(theater_adolescence, structural_opacity_of_self, theater_ratio, 3, 0.58).
narrative_ontology:measurement(theater_early_adult, structural_opacity_of_self, theater_ratio, 6, 0.65).
narrative_ontology:measurement(theater_mature_adult, structural_opacity_of_self, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(extract_childhood, structural_opacity_of_self, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(extract_adolescence, structural_opacity_of_self, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(extract_early_adult, structural_opacity_of_self, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(extract_mature_adult, structural_opacity_of_self, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_opacity_of_self, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of temporal_perception_mismatch (mountain — the present-moment bias that makes historical patterns invisible) and pattern_recognition_as_error_lock (tangled_rope — the tendency to recognize patterns that confirm existing schemas while missing disconfirming evidence). The structural opacity inherits immutability from temporal_perception_mismatch (childhood patterns are invisible because they occurred in a cognitively inaccessible past) and extraction from pattern_recognition_as_error_lock (the agent's pattern recognition system actively maintains the opacity by filtering disconfirming evidence). The three constraints form a family where architectural limits (mountain) enable cognitive traps (tangled_rope) that crystallize into identity fusion (snare).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
