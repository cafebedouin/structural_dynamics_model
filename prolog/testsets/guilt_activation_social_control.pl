% ============================================================================
% CONSTRAINT STORY: guilt_activation_social_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_guilt_activation_social_control, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: guilt_activation_social_control
 *   human_readable: Guilt Activation Social Control Mechanism
 *   domain: interpersonal_psychology/social_control
 *
 * SUMMARY:
 *   Guilt activation operates as a social control mechanism in dyadic and
 *   small-group relationships (families, friendships, romantic partnerships,
 *   mentorships). The mechanism functions by holding the target responsible
 *   for the activator's emotional state, creating a binding obligation to
 *   prevent or relieve the activator's suffering. This obligation is enforced
 *   through moral narrative inversion: the target's resistance to demands is
 *   reframed as selfishness, cruelty, or betrayal. The target becomes trapped
 *   in a double bind — compliance maintains the relationship but requires
 *   abandoning autonomy; resistance preserves autonomy but triggers
 *   abandonment threat or moral condemnation. Guilt activation exhibits all
 *   six DR classifications depending on perspective, with the strongest case
 *   for Snare (pure extraction) from the target's powerless/trapped position.
 *   The constraint's extractiveness has increased over the interval (0.35 →
 *   0.68) as guilt activation becomes more refined and normalized within the
 *   relationship. Theater ratio has also risen (0.40 → 0.58), reflecting
 *   increasing deployment of therapeutic language ('guilt-based parenting is
 *   really about connection') and cultural normalization that masks
 *   extraction as care.
 *
 * KEY AGENTS:
 *   - Guilt Target: Primary victim (powerless/trapped at biographical horizon, or moderate/identity_locked at biographical horizon with identity-locked variant) — bears extraction through emotional labor, compliance with unreasonable demands, and autonomy loss
 *   - Guilt Activator: Primary beneficiary (institutional/arbitrage) — captures compliance, emotional regulation benefits, and relational dominance through guilt mechanism
 *   - Relational Culture Carriers: Secondary actor (organized/constrained) — transmit guilt-based control norms across generations; extract compliance from members while bearing costs of maintaining normative legitimacy
 *   - Therapeutic Industry: Tertiary actor (institutional/arbitrage) — maintains diagnostic categories around guilt while offering institutional solutions; benefits from guilt normalization without dismantling the mechanism
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing guilt activation as inherent to human bonding rather than contingent institutional practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(guilt_activation_social_control, 0.68).
domain_priors:suppression_score(guilt_activation_social_control, 0.72).
domain_priors:theater_ratio(guilt_activation_social_control, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(guilt_activation_social_control, extractiveness, 0.68).
narrative_ontology:constraint_metric(guilt_activation_social_control, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(guilt_activation_social_control, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(guilt_activation_social_control, snare).
narrative_ontology:human_readable(guilt_activation_social_control, "Guilt Activation Social Control Mechanism").
narrative_ontology:topic_domain(guilt_activation_social_control, "interpersonal_psychology/social_control").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(guilt_activation_social_control, guilt_activator).
narrative_ontology:constraint_victim(guilt_activation_social_control, guilt_target).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GUILT TARGET (SNARE) — Trapped in a compliance mechanism driven by internalized shame and obligation. No viable exit without severe relational cost. Bears full extraction through emotional labor, compliance with unreasonable demands, and loss of autonomy. Suppression operates through moral narrative inversion: target's resistance is reframed as selfishness or cruelty.
constraint_indexing:constraint_classification(guilt_activation_social_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: GUILT TARGET — IDENTITY-LOCKED VARIANT (TANGLED ROPE) — Same agent at biographical horizon but with identity-locked exit option. The target's self-concept is constituted through the relational role ('good daughter', 'loyal friend', 'responsible partner'). Structural mobility exists (could walk away) but identity frame makes exit unthinkable. Perceives the constraint as changeable in principle but cannot exercise the change from within their identity. The guilt mechanism coordinates relational expectations alongside asymmetric extraction.
constraint_indexing:constraint_classification(guilt_activation_social_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 3: GUILT ACTIVATOR (ROPE) — Experiences the constraint as pure coordination: expressing needs through guilt activation is a successful communication strategy within the relationship. Net beneficiary with exit option (can shift to direct communication or abandon relationship at acceptable cost). Perceives guilt activation as a functional solution to coordination problems, not extraction.
constraint_indexing:constraint_classification(guilt_activation_social_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: RELATIONAL CULTURE CARRIERS (SNARE) — Organized groups (family lineages, cultural communities, religious traditions) that transmit guilt-based social control across generations. Constrained by cultural identity and communal enforcement. Extract compliance through internalized guilt norms, yet also bear the cost of maintaining these norms and face pressure from alternative cultural frames that delegitimize guilt activation.
constraint_indexing:constraint_classification(guilt_activation_social_control, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: THERAPEUTIC INDUSTRY (PITON) — Psychology, counseling, and self-help frameworks maintain guilt-activation concepts ('codependency', 'people-pleasing', 'enmeshment') as primary diagnostic categories while offering institutional solutions (therapy, workshops, self-help books). The industry benefits from guilt normalization while appearing to critique it. Theater ratio high because the therapeutic language is often performative — naming the pattern does not break it. The industry's primary function (profit capture) is masked by its secondary stated function (liberation).
constraint_indexing:constraint_classification(guilt_activation_social_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, guilt activation might appear as an irreducible feature of human social bonding: all social species require mechanisms to enforce pro-social behavior, guilt is a universal human emotion, and guilt-based coordination enables cooperation at scale. This view risks naturalizing what is actually a specific institutional arrangement of guilt (activated deliberately, instrumentalized for control) as an inherent property of human nature. False summit detection applies here.
constraint_indexing:constraint_classification(guilt_activation_social_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(guilt_activation_social_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(guilt_activation_social_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(guilt_activation_social_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(guilt_activation_social_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(guilt_activation_social_control, TR),
    TR >= 0.70.

:- end_tests(guilt_activation_social_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, approaching snare threshold (0.66+). The guilt activation mechanism generates consistent extraction of compliance, emotional labor, and autonomy from the target. The mechanism is refined: guilt is activated precisely to produce compliance, creating a contingency trap (resistance → guilt → compliance → temporary relief → new activation). The extractiveness has accumulated over the interval, suggesting refinement and deepening of the mechanism. Suppression (0.72): Very high. The constraint operates through profound suppression: (1) Material — target faces relational abandonment if resistance persists; (2) Cognitive — guilt is internalized as moral failure ('I am a bad person if I resist'); (3) Epistemic — the target cannot reality-test their experience within the relationship, as the activator controls the narrative frame; (4) Cultural — guilt-based control is normalized in family and relational culture, making it appear natural. Theater ratio (0.58): Moderate-high and rising. The initial theater ratio reflects the gap between guilt activation's stated function (coordination, accountability) and its actual function (extraction). As the mechanism matures, theater increases as therapeutic language ('codependency', 'family enmeshment') is deployed to normalize guilt without changing the underlying extraction flow.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal in this constraint. The guilt activator (Rope) experiences the mechanism as coordination — a functional way to communicate needs and maintain relational stability. The guilt target (Snare, or Tangled Rope if identity-locked) experiences it as pure extraction — bearing costs with no compensation or escape. The relational culture carriers (Snare) are trapped between transmitting normative guilt practices and suffering under them. The therapeutic industry (Piton) maintains the constraint while appearing to critique it. The analytical observer (Mountain, false summit) risks naturalizing the mechanism as inherent to human nature rather than as a specific institutional practice. The identity-locked variant reveals the key diagnostic gap: the target is structurally mobile (could leave) but identity-trapped (cannot leave from within their self-concept). This gap is precisely where guilt activation operates most effectively — it binds the target not through material barriers but through identity fusion.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim declarations and exit options. The guilt activator has d ≈ 0.10 (beneficiary + arbitrage exit → low d → negative f(d) → experiences constraint as coordinating). The guilt target with trapped exit has d ≈ 0.95 (victim + trapped → high d → f(d) ≈ 1.42 → maximum experienced extraction). The guilt target with identity_locked exit has d ≈ 0.88 (victim + identity_locked → high d → f(d) ≈ 1.28 → very high extraction, slightly less than trapped due to structural mobility). The directionality pipeline correctly assigns higher extraction to agents with trapped or identity-locked exits, which matches the structural reality of guilt activation's mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION ROBUST: The guilt activation mechanism meets snare thresholds from the target's perspective (ε=0.68 ≥ 0.46, suppression=0.72 ≥ 0.60, χ ≥ 0.66). The mechanism is pure extraction: minimal coordination function exists (guilt activation is inefficient for actual communication), high coercion (imminent relational abandonment), minimal beneficiary alternatives (the activator could use direct communication but guilt activation is more effective at generating compliance). TANGLED ROPE RESOLUTION: The identity-locked variant reveals why some targets persist in guilt-based relationships despite high extraction — the constraint coordinates relational expectations (maintaining family/friend identity, fulfilling cultural role obligations) alongside extraction. From the target's identity-locked perspective, the constraint is both genuine coordination (I am this person; this is my role) and asymmetric extraction (my autonomy is sacrificed for this role). This dual character is the mandatrophy resolution: both snare and tangled rope are correct, depending on whether we emphasize the extraction mechanism (snare) or the identity-coordination function that binds the target (tangled rope). The false summit (mountain perspective) is neutralized by structural analysis: guilt activation is not inevitable to human bonding, only to this institutional arrangement of guilt.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    guilt_internalization_boundary,
    'At what point does guilt-based social coordination transition from functional accountability to extractive control?',
    'Longitudinal analysis of target''s autonomy and wellbeing trajectories under guilt-based vs direct-communication relationships. Measurement of target''s capacity to pursue own values without activation spikes.',
    'If boundary is permeable: much guilt-based parenting and friendship is functional (Rope), not extractive (Snare). If boundary is sharp: guilt activation at any intensity is a suppression mechanism. Classification hinges on this threshold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guilt_internalization_boundary, empirical, 'Boundary between functional guilt-based accountability and extractive control').

omega_variable(
    identity_lock_vs_constrained_exit,
    'Is the target''s immobility due to internalized identity fusion (identity_locked) or high-cost external barriers (constrained)?',
    'Hypothetical scenario: if target''s identity frame suddenly shifted (ideological conversion, new social context), would exit become possible? If yes, mechanism is identity_locked. If barriers remain, mechanism is constrained. Post-exit trajectory analysis: does suppression persist after relational exit?',
    'If identity_locked: the constraint''s effective suppression is higher than structural measure suggests — target carries internalized prohibition after exit. If constrained: suppression is primarily external and diminishes after exit. Classification impacts mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_exit, empirical, 'Whether target''s immobility is identity-based or barrier-based').

omega_variable(
    guilt_activation_intentionality,
    'Is guilt activation deliberate (the activator strategically induces guilt) or emergent (both parties participate in guilt scripts without explicit intent)?',
    'Interview/testimony analysis: Does the activator acknowledge deliberately inducing guilt? Does the target recognize guilt as a mechanism or experience it as spontaneous emotion? Behavioral analysis: Does guilt activation pattern match contingency (reward for compliance, punishment for resistance) or does it appear random?',
    'If deliberate: snare classification is robust. If emergent: both parties are trapped in a coordination equilibrium neither consciously maintains; classification might shift toward tangled rope at the group level, even as snare at the target level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guilt_activation_intentionality, empirical, 'Whether guilt activation is deliberate strategy or emergent equilibrium').

omega_variable(
    alternative_coordination_sufficiency,
    'Do direct communication, contract-based boundaries, or explicit negotiation reduce extraction while maintaining relational coordination?',
    'Intervention studies: relational pairs transitioning from guilt-based to direct-communication protocols. Measurement of relational stability, target autonomy, and activator''s ability to influence behavior.',
    'If alternatives are sufficient: guilt activation is not inherent to coordination; classification shifts toward snare (choice-based extraction rather than necessity). If alternatives fail: guilt activation might be necessary for relational stability; classification softens toward tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_coordination_sufficiency, empirical, 'Whether direct communication can replace guilt activation while maintaining coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(guilt_activation_social_control, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(guilt_tr_t0, guilt_activation_social_control, theater_ratio, 0, 0.4).
narrative_ontology:measurement(guilt_tr_t3, guilt_activation_social_control, theater_ratio, 3, 0.48).
narrative_ontology:measurement(guilt_tr_t6, guilt_activation_social_control, theater_ratio, 6, 0.55).
narrative_ontology:measurement(guilt_tr_t9, guilt_activation_social_control, theater_ratio, 9, 0.58).

% Extraction over time
narrative_ontology:measurement(guilt_be_t0, guilt_activation_social_control, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(guilt_be_t3, guilt_activation_social_control, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(guilt_be_t6, guilt_activation_social_control, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(guilt_be_t9, guilt_activation_social_control, base_extractiveness, 9, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(guilt_activation_social_control, attachment_coordination).
narrative_ontology:boltzmann_floor_override(guilt_activation_social_control, 0.12).
narrative_ontology:affects_constraint(guilt_activation_social_control, relational_trauma_bonding).
narrative_ontology:affects_constraint(guilt_activation_social_control, family_enmeshment_norm).
narrative_ontology:affects_constraint(guilt_activation_social_control, moral_injury_from_broken_obligation).

% DUAL FORMULATION NOTE:
% Guilt activation social control is upstream of trauma bonding (guilt mechanism can generate attachment to abusive relationships) and family enmeshment norms (guilt operates as the primary enforcement mechanism for enmeshed boundaries). The mechanism is also linked to moral injury trajectories when targets eventually reject guilt frames and experience identity discontinuity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(guilt_activation_social_control, moderate, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
