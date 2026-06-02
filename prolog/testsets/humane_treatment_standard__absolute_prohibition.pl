% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__absolute_prohibition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__absolute_prohibition, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: humane_treatment_standard__absolute_prohibition
 *   human_readable: Common Article 3 Absolute Prohibition on Torture and Degrading Treatment
 *   domain: international_humanitarian_law/human_rights/state_security
 *
 * SUMMARY:
 *   Common Article 3 of the Geneva Conventions establishes a non-derogable
 *   absolute prohibition on torture, cruel treatment, and degrading
 *   punishment of persons hors de combat (detainees, wounded, sick,
 *   shipwrecked). This constraint is instantiated as the absolute-prohibition
 *   reading of the humane_treatment_standard kernel — the reading that
 *   declares the prohibition is not negotiable under any circumstance,
 *   including existential security threats. This reading forecloses the
 *   sibling contextual-necessity reading (which permits exceptions when
 *   security is at stake) and coexists with the proportionality-balancing
 *   reading only by rejecting its premise that states may calibrate the
 *   prohibition based on threat level. The absolute-prohibition reading
 *   grounds itself in deontological human dignity: some acts (torture) are
 *   intrinsically wrongful regardless of utility or emergency. The constraint
 *   appears as a mountain from all four perspectives — an unchangeable floor
 *   of both formal law and human rights principle. However, the false-summit
 *   detector will flag this constraint as a candidate for reclassification,
 *   because identifiable beneficiaries (the IHL framework itself, states that
 *   benefit from predictable mutual restraint) exist. The engine must
 *   determine whether the apparent mountain is genuinely natural law or a
 *   constructed institutional norm maintained by powerful states' enforcement
 *   and interpretive control. The absolute-prohibition reading embeds this
 *   ambiguity as an irreducible omega: is the universality of the standard
 *   evidence of natural law, or evidence that powerful states have
 *   successfully imposed their humanitarian preferences globally?
 *
 * KEY AGENTS:
 *   - Detained persons in armed conflict: Primary victims (powerless/trapped) — experience the prohibition as an absolute floor; bear maximum extraction if the prohibition is violated or interpreted away
 *   - States under security pressure: Institutional actors (institutional/constrained) — experience the constraint as mixed coordination-extraction: coordination through mutual assurance, extraction through interrogation foreclosure
 *   - International humanitarian law framework: Institutional authority (institutional/analytical) — maintains the prohibition as a formal invariant; enforces through treaty compliance and judicial review
 *   - Non-state armed groups: Institutional actors (institutional/analytical) — formally bound by Common Article 3 but lack enforcement mechanisms and treaty signatory status; effectively constrained by threat of prosecution rather than by internal commitment
 *   - International courts and monitoring bodies: Institutional interpreters (institutional/arbitrage) — define the boundary between degrading treatment and harsh-but-permissible interrogation; shape the constraint's operational scope through jurisprudence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__absolute_prohibition, 0.08).
domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, 0.02).
domain_priors:theater_ratio(humane_treatment_standard__absolute_prohibition, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, extractiveness, 0.08).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__absolute_prohibition, mountain).
narrative_ontology:human_readable(humane_treatment_standard__absolute_prohibition, "Common Article 3 Absolute Prohibition on Torture and Degrading Treatment").
narrative_ontology:topic_domain(humane_treatment_standard__absolute_prohibition, "international_humanitarian_law/human_rights/state_security").

domain_priors:emerges_naturally(humane_treatment_standard__absolute_prohibition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__absolute_prohibition, '0effc970-2028-49a5-a8a4-148c5ef2a056').
narrative_ontology:cs_kernel_codification('0effc970-2028-49a5-a8a4-148c5ef2a056', formalized).
narrative_ontology:cs_authority_grounding('0effc970-2028-49a5-a8a4-148c5ef2a056', lineage).
narrative_ontology:cs_interpretation_layer_present('0effc970-2028-49a5-a8a4-148c5ef2a056').
narrative_ontology:cs_reading_relation('0effc970-2028-49a5-a8a4-148c5ef2a056', humane_treatment_standard__contextual_necessity, forecloses).
narrative_ontology:cs_reading_relation('0effc970-2028-49a5-a8a4-148c5ef2a056', humane_treatment_standard__proportionality_balancing, coexists_with).
narrative_ontology:cs_axiom('0effc970-2028-49a5-a8a4-148c5ef2a056', foundational, torture_categorically_impermissible).
narrative_ontology:cs_axiom_status(torture_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('0effc970-2028-49a5-a8a4-148c5ef2a056', torture_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('0effc970-2028-49a5-a8a4-148c5ef2a056', foundational, detainee_dignity_inalienable).
narrative_ontology:cs_axiom_status(detainee_dignity_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('0effc970-2028-49a5-a8a4-148c5ef2a056', detainee_dignity_inalienable, deontological).
narrative_ontology:cs_reference_frame('0effc970-2028-49a5-a8a4-148c5ef2a056', post_world_war_two_humanitarian_covenant).
narrative_ontology:cs_drift_state('0effc970-2028-49a5-a8a4-148c5ef2a056', contemporary_security_state_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0effc970-2028-49a5-a8a4-148c5ef2a056', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(humane_treatment_standard__absolute_prohibition, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, detainees_under_armed_conflict).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, international_humanitarian_law_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DETAINED PERSON (MOUNTAIN) — From the standpoint of a person deprived of liberty in armed conflict, the prohibition on torture and degrading treatment is experienced as an invariant boundary. No negotiation, no exception, no circumstance overrides this floor. The constraint is non-derogable by definition — cannot be suspended even in emergency. Accessibility collapse is near-total: the standard applies regardless of the detainer's strategic pressure, resource constraints, or security arguments. Resistance to the standard (attempts to evade or reinterpret) is minimal in the formal legal architecture, though practice may diverge. This is the lived instantiation of absolute prohibition.
constraint_indexing:constraint_classification(humane_treatment_standard__absolute_prohibition, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: INTERNATIONAL HUMANITARIAN LAW FRAMEWORK (MOUNTAIN) — From the systemic standpoint of IHL as a formal architecture, Common Article 3's absolute prohibition is a structural invariant. It is non-derogable in all circumstances (Article 27, ICCPR confirms). The accessibility collapse is the definition of the constraint itself: no state, security condition, or emergency provision can override this floor. Resistance to the standard exists as practice drift (torture occurring despite prohibition) but not as formal legal wiggle room. The standard is fixed by the Geneva Conventions and cannot be reinterpreted to permit torture. From this institutional perspective, the constraint is a mountain — an unchangeable floor of the legal order itself.
constraint_indexing:constraint_classification(humane_treatment_standard__absolute_prohibition, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: SECURITY STATE (TANGLED ROPE) — From the standpoint of a state engaged in armed conflict and facing genuine security threats, the absolute prohibition constraint functions as a mixed coordination-extraction hybrid. Coordination element: the state benefits from a stable, predictable rule that applies to all parties equally — the prohibition creates mutual assurance that detainees will not be tortured by adversary forces, enabling trust in exchanges and reducing incentive escalation. Extraction element: the state's interrogation options are asymmetrically constrained — the prohibition forecloses coercive methods that might yield actionable intelligence. The state experiences this as an asymmetric burden: it sacrifices potential extraction of information while competitors (non-state armed groups, adversary states not bound by IHL) may not honor the same restraint. Effective extraction (chi) from this perspective is moderate because the coordination benefit (mutual assurance, rule of law stability, avoided escalation) partially offsets the interrogation constraint. Active enforcement is required: IHL compliance requires institutional mechanisms to monitor interrogation practices, train personnel, and sanction violations. This is tangled rope, not mountain — the state perceives the constraint as changeable in principle (security emergency exceptions are proposed repeatedly) but cannot formally change it without breaching the Geneva Conventions framework.
constraint_indexing:constraint_classification(humane_treatment_standard__absolute_prohibition, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER — DEONTOLOGICAL READING (MOUNTAIN) — From the standpoint of analytical observation across the full history of humanitarian law and human rights doctrine, the absolute prohibition on torture and degrading treatment appears as a mountain — an unchangeable floor grounded in deontological principles rather than empirical consequences. The standard is non-derogable not because torture is empirically ineffective (a contingent claim) but because torture violates categorical human dignity rights that cannot be overridden by security calculus. The prohibition emerges from the axiom that some acts are intrinsically wrongful regardless of utility. Accessibility collapse is near-total: the standard applies equally to powerful and powerless, wealthy and poor, Western and non-Western states. Resistance is minimal in the formal architecture, though practice diverges sharply. The analytical observer sees the constraint as a mountain because it instantiates a non-negotiable principle — human dignity is not a bargaining chip.
constraint_indexing:constraint_classification(humane_treatment_standard__absolute_prohibition, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__absolute_prohibition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(humane_treatment_standard__absolute_prohibition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(humane_treatment_standard__absolute_prohibition, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(humane_treatment_standard__absolute_prohibition, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(humane_treatment_standard__absolute_prohibition, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, ExtMetricName, E),
    domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(humane_treatment_standard__absolute_prohibition),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(humane_treatment_standard__absolute_prohibition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The absolute prohibition extracts minimal net coercive capacity from detainees beyond the legitimate confinement itself. The prohibition explicitly forbids torture and degrading treatment — mechanisms that would increase extraction. The state retains the power to detain, interrogate (within bounds), and prosecute; the prohibition constrains interrogation methods but does not foreclose interrogation itself. Extractiveness is low because the constraint's primary function is to establish a floor below which treatment cannot fall, not to optimize extraction. Suppression (0.02): Near-zero. The prohibition is stated in clear, non-ambiguous terms ('no torture,' 'no degrading treatment'). Accessibility collapse is near-total: the standard is equally accessible to powerful and powerless states, wealthy and poor nations, Western and non-Western legal systems. The prohibition applies universally with no carve-outs. Resistance is minimal in formal law — no state claims torture is permissible under the Geneva Conventions, though state practice often diverges. Theater ratio (0.15): Very low. The prohibition involves minimal performative content. The formal standard is stark and unambiguous; compliance is verifiable (interrogation practices are observable; reports of torture are documentable). Theater consists mainly of debates about definitional boundaries (what counts as degrading?) and state claims that interrogation methods are harsh-but-not-degrading (claims that deflect rather than engage the substance). The low theater reflects the high accessibility and clarity of the underlying principle.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives classify the constraint as mountain, but the structural experiences differ radically. The detained person experiences mountain as invariant protection — absolute floor that cannot be negotiated away by security pressure. The IHL framework experiences mountain as formal architecture — the constraint is encoded in non-derogable treaty language and cannot be suspended even in emergency. The security state experiences mountain as institutional boundary they cannot formally cross, but they perceive structural pressure to reinterpret the boundary (definitional wiggle room); the security state's perspective is borderline tangled-rope because they experience both coordination benefit (mutual assurance) and extraction constraint (interrogation foreclosure). The analytical observer experiences mountain as deontological principle — unquestionable human dignity right, not a utilitarian calculation. The perspectival gap appears in the security state's constrained exit options: institutional actors facing genuine security threats repeatedly seek interpretive exceptions (enhanced interrogation, stress positions justified as non-degrading). This gap signals that the mountain classification may conceal a false-summit — the absolute prohibition is mountain in formal law but tangled-rope or snare in state security practice.
 *
 * DIRECTIONALITY LOGIC:
 *   The absolute-prohibition reading declares that detainees enter the full rights-holder set the moment they are detained — they are not reducible to security threats or intelligence sources. Directionality is asymmetric: from the detainee's perspective (powerless/trapped), d ≈ 1.0 (full target of the constraint — they are the ones whose dignity is protected and whose torturability is prohibited). From the state's perspective (institutional/constrained), d ≈ 0.55 (both beneficiary and target — benefits from mutual restraint assurance, constrained by interrogation foreclosure). The derived d values produce the classification gap: detainees experience mountain (invulnerable floor), states experience tangled-rope (mixed coordination and extraction). The beneficiary declaration (detainees_under_armed_conflict, IHL_framework) feeds the directionality derivation: beneficiaries of the prohibition are detainees; the IHL framework benefits from having a stable, universally-applicable standard. The false-summit detector will examine whether the constraint's universality and formal power are evidence of genuine natural law or evidence of institutional construction. If the prohibition is truly natural (discovered by all societies independently), the mountain holds. If it is constructed (created post-WWII by victorious powers), it may be tangled-rope or snare enforced by institutional power.
 *
 * MANDATROPHY ANALYSIS:
 *   The absolute-prohibition reading avoids mandatrophy by committing unambiguously to a non-negotiable floor: torture is forbidden, period. The reading makes no claim to balance competing goods or optimize interrogation — it forecloses the optimization frame entirely. The mandatrophy question ('Is this coordination that serves extraction, or extraction that masquerades as coordination?') does not arise because the reading explicitly rejects the frame that interrogation can be optimized. However, the false-summit pressure emerges: if the constraint is truly mountain (natural law), practice divergence should provoke systemic correction; if the constraint is actually tangled-rope or snare enforced by institutional power, practice divergence should persist. The omega variables document this irreducible ambiguity: the absolute-prohibition reading claims mountain status on deontological grounds, but structural data (state pressure to reinterpret, definitional boundary shifting, practice drift) suggest the constraint is maintained by institutional enforcement, not natural law inevitability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_institutional,
    'Is the absolute prohibition on torture a natural law of human dignity that states discover and apply, or a constructed institutional norm that states invented and could theoretically unmake?',
    'Historical genealogy: trace the prohibition''s origin. If it emerges from unanimous recognition of a principle all societies arrive at independently, it has natural-law credibility. If it emerges from political negotiation among victorious powers (post-WWII), it is institutional construction. Check whether the prohibition precedes formal codification (torture taboos in pre-modern societies). Evaluate whether any state has successfully renounced the Geneva Conventions on the ground that torture is permissible. Examine whether the prohibition is universally held (zero exception states) or whether some states maintain explicit torture approval in their frameworks.',
    'If natural law: the constraint''s mountain classification is robust; attempts to override it reveal deep commitment violations. If constructed institutional norm: the constraint is actually a tangled rope or snare maintained by powerful states'' enforcement (U.S., Western powers enforce on others but reserve interpretation flexibility); false summit likely. If universally held: supports mountain. If some exception-states exist: constraint is contested at the definitional level, not truly non-derogable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_institutional, empirical, 'Natural law vs institutional construction origin').

omega_variable(
    definition_boundary_ambiguity,
    'Where exactly is the boundary between degrading treatment (prohibited absolutely) and harsh-but-permissible interrogation (constrained but possible)?',
    'Case law analysis: collect all judgments in international courts (ICJ, ICTY, ECHR) that distinguish degrading from harsh-but-permissible. Identify the threshold markers (sleep deprivation duration, temperature exposure, isolation period, noise level, stress position duration). If thresholds are consistent across cases, boundary is operationalized. If thresholds vary case-by-case, boundary is ambiguous. Query whether states cite ambiguity to justify enhanced interrogation (sleep deprivation for 72 hours classified as harsh not degrading). If systematic boundary-shifting occurs to accommodate state security claims, the absolute prohibition is actually constrained by practice drift.',
    'If boundary is sharp and consistent: absolute prohibition is operationally real (mountain). If boundary is ambiguous or state-dependent: constraint is actually tangled rope or snare using definition manipulation to maintain extraction space. The false-summit detector may fire if boundary ambiguity permits state interrogation programs despite formal prohibition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_boundary_ambiguity, empirical, 'Definitional boundary between degrading and harsh treatment').

omega_variable(
    security_exception_pressure,
    'What structural pressure arises for states to reinterpret or evade the absolute prohibition when facing genuine existential security threats (active terrorism campaigns, invasion)?',
    'Historical case study of state behavior under acute security pressure: examine U.S. Enhanced Interrogation Program post-9/11 (claimed legal authorization reinterpretation), Israeli Shin Bet practices during Palestinian uprisings (claimed necessity exception), French practices in Algeria (torture justified by security emergency), British practices in Northern Ireland. For each case, determine: did the state formally declare an exception to the prohibition, or did it reinterpret the boundary of prohibited conduct? Did it maintain formal compliance while creating operational divergence? Did international pressure force reversal or did the practice persist? If security-pressured states repeatedly seek ways to override the absolute prohibition, the constraint is experientially tangled rope or snare, not mountain — the absoluteness is formal, not structural.',
    'If states reliably reverse course under international pressure: constraint is mountain despite practice drift. If states sustain exception-seeking even under international pressure: constraint is tangled rope or snare, and the absolute prohibition is aspirational rather than operational. The false-summit detector may flag if practice divergence is systematic and unpenalized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(security_exception_pressure, empirical, 'State pressure to reinterpret absolute prohibition under security threats').

omega_variable(
    reading_scope_ambiguity,
    'Does ''absolute prohibition'' apply only to states and state-aligned forces, or does it bind all armed parties including non-state actors and private military companies?',
    'Text analysis of Common Article 3: determine whether the prohibition explicitly extends to non-state armed groups. Check IHL jurisprudence (ICRC interpretation, national court rulings) on whether non-state actors are bound. Examine state practice: do states enforce the prohibition against non-state armed groups in their custody? Do PMCs claim IHL exemptions or acknowledge the prohibition? If the prohibition is formal for states but unenforceable for non-state actors, the constraint is actually asymmetric (tangled rope for states, snare for detainees of non-state actors), not universally mountain.',
    'If absolute prohibition extends to all parties: constraint is universally mountain. If it applies only to states: constraint is mountain for states but snare for detainees of non-state actors — a false summit of universality masking structural asymmetry. This affects the reading''s claim to absolute applicability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_scope_ambiguity, empirical, 'Scope of absolute prohibition: state vs non-state actors').

omega_variable(
    sibling_reading_coexistence,
    'Can the absolute-prohibition reading coexist with the contextual-necessity reading (security exceptions permitted) within the same legal framework, or does one foreclose the other?',
    'Jurisprudential analysis: examine whether any court or authoritative legal body holds both readings as legitimate. If a court permits emergency derogation from the prohibition (contextual-necessity), does it explicitly reject absolute-prohibition reading, or does it claim consistency? Examine constitutional law in states with emergency provisions: do they claim the prohibition is absolute yet permit necessity exceptions? If coexistence is claimed, the readings are tangled in a single framework (confusing foundation). If one explicitly forecloses the other (as this reading claims), the framework is logically stratified.',
    'If readings coexist: the constraint is actually less absolute than this reading claims; it is tangled rope with sibling readings embedding exception spaces. If readings foreclose each other: this reading truly instantiates absolute prohibition, but the framework is fractured between commitments. This affects the reading''s epistemic status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, conceptual, 'Logical coexistence of absolute-prohibition and contextual-necessity readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__absolute_prohibition, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hts_abs_theater_t0, humane_treatment_standard__absolute_prohibition, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hts_abs_theater_t25, humane_treatment_standard__absolute_prohibition, theater_ratio, 25, 0.14).
narrative_ontology:measurement(hts_abs_theater_t50, humane_treatment_standard__absolute_prohibition, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(hts_abs_extract_t0, humane_treatment_standard__absolute_prohibition, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(hts_abs_extract_t25, humane_treatment_standard__absolute_prohibition, base_extractiveness, 25, 0.07).
narrative_ontology:measurement(hts_abs_extract_t50, humane_treatment_standard__absolute_prohibition, base_extractiveness, 50, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__absolute_prohibition, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__contextual_necessity).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__proportionality_balancing).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, interrogation_practice_constraint).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, detention_standards_framework).

% DUAL FORMULATION NOTE:
% The humane_treatment_standard kernel decomposes into three constraint stories instantiating three readings: absolute-prohibition (this file), contextual-necessity (sibling), and proportionality-balancing (sibling). Each reading has its own epsilon value reflecting empirical status. Absolute-prohibition has ε=0.08 (low extraction, mountain classification). The sibling readings will have higher epsilon values reflecting the structural ambiguity and state practice divergence they accommodate. All three stories are linked via network.affects_constraints and document the kernel's internal logical structure through reading_relations and axioms in cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
