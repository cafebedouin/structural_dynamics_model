% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__narrow_armed_attack_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__narrow_armed_attack_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article_51_self_defense__narrow_armed_attack_reading
 *   human_readable: Article 51 Narrow Armed Attack Reading: State-Attributable Self-Defense Constraint
 *   domain: international law / security studies / constitutional interpretation
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the Article 51 self-defense
 *   kernel: self-defense is constrained to responses to actual or imminent
 *   armed attacks by states or state-attributable actors under international
 *   law. The narrow reading is the ICJ's canonical interpretation (Nicaragua
 *   case, Armed Activities case) and the formal position of the UN General
 *   Assembly and most states outside the United States, United Kingdom, and
 *   Israel — the very states most constrained by the reading. The constraint
 *   exhibits classic tangled-rope structure: it genuinely solves the
 *   collective-action problem of distinguishing legitimate self-defense from
 *   opportunistic aggression (the coordination function), AND it
 *   asymmetrically constrains powerful states while protecting weaker ones
 *   and multilateral institutions (the extraction function). The measurement
 *   series spans 1945–2026, capturing the post-WWII establishment (low
 *   extractiveness, high respect for the constraint), the Cold War and
 *   post-Cold War erosion (rising theater as preventive doctrine emerged),
 *   the 9/11 pivot (sharp jump in suppression as powerful states actively
 *   tested the constraint), and the post-2015 stabilization (theater remains
 *   elevated but extractiveness plateaus as the constraint shows structural
 *   resilience despite persistent violations).
 *
 * KEY AGENTS:
 *   - weaker_states: Medium power, constrained exit, beneficiary under the narrow reading — protected from unilateral attack for hosting non-state actors unless attribution shown
 *   - multilateral_institutions (UN, ICJ, international legal order): Institutional power, analytical exit, beneficiary — authority preserved, gatekeeping role enabled
 *   - powerful_states (US, UK, Russia, China in different measure): Institutional power, constrained exit, payer — strategic autonomy narrowed, preventive authority foreclosed
 *   - security_establishments: Institutional power, constrained exit, payer — operational doctrine constrained to wait for actual/imminent attack
 *   - non-state threat hosts: Moderate power, trapped exit, excluded — ambiguous position not addressed by the narrow reading's core
 *   - ICJ: Institutional power, analytical exit, agenda_setter — interprets the constraint, sets boundaries, enforces through adjudication
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, 0.31).
domain_priors:suppression_score(article_51_self_defense__narrow_armed_attack_reading, 0.22).
domain_priors:theater_ratio(article_51_self_defense__narrow_armed_attack_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__narrow_armed_attack_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__narrow_armed_attack_reading, "Article 51 Narrow Armed Attack Reading: State-Attributable Self-Defense Constraint").
narrative_ontology:topic_domain(article_51_self_defense__narrow_armed_attack_reading, "international law / security studies / constitutional interpretation").

domain_priors:requires_active_enforcement(article_51_self_defense__narrow_armed_attack_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__narrow_armed_attack_reading, '4fc6c9bb-6f95-48bc-bdfd-1e4901a7f921').
narrative_ontology:cs_kernel_codification('4fc6c9bb-6f95-48bc-bdfd-1e4901a7f921', fixed_text).
narrative_ontology:cs_authority_grounding('4fc6c9bb-6f95-48bc-bdfd-1e4901a7f921', lineage).
narrative_ontology:cs_interpretation_layer_present('4fc6c9bb-6f95-48bc-bdfd-1e4901a7f921').
narrative_ontology:cs_reading_relation('4fc6c9bb-6f95-48bc-bdfd-1e4901a7f921', article_51_self_defense__expansive_preventive_reading, forecloses).
narrative_ontology:cs_reading_relation('4fc6c9bb-6f95-48bc-bdfd-1e4901a7f921', article_51_self_defense__unable_unwilling_doctrine_reading, coexists_with).
narrative_ontology:cs_axiom('4fc6c9bb-6f95-48bc-bdfd-1e4901a7f921', foundational, state_attribution_requirement).
narrative_ontology:cs_axiom_status(state_attribution_requirement, holdable).
narrative_ontology:cs_axiom_grounding('4fc6c9bb-6f95-48bc-bdfd-1e4901a7f921', state_attribution_requirement, deontological).
narrative_ontology:cs_axiom('4fc6c9bb-6f95-48bc-bdfd-1e4901a7f921', foundational, temporal_constraint_imminent_only).
narrative_ontology:cs_axiom_status(temporal_constraint_imminent_only, holdable).
narrative_ontology:cs_axiom_grounding('4fc6c9bb-6f95-48bc-bdfd-1e4901a7f921', temporal_constraint_imminent_only, empirically_contingent).
narrative_ontology:cs_reference_frame('4fc6c9bb-6f95-48bc-bdfd-1e4901a7f921', collective_security_system_with_imminent_attack_gate).
narrative_ontology:cs_drift_state('4fc6c9bb-6f95-48bc-bdfd-1e4901a7f921', contemporary_non_state_threat_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4fc6c9bb-6f95-48bc-bdfd-1e4901a7f921', '2026-06-11T14:23:45Z').
narrative_ontology:cs_kernel_id(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, weaker_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, multilateral_institutions).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, international_legal_order).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, powerful_states).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, security_establishments).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, collective_security_system).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, principle_of_non_intervention).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, rule_of_law_in_armed_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The narrow reading protects their sovereignty: they cannot be unilaterally attacked by powerful states claiming preemptive self-defense against non-state actors on their territory, or against emerging threats the powerful state defines as imminent. The rule channels all legitimate force through the Security Council, where weaker states have veto or blocking power. Their material interest is in restraining powerful states' unilateral military action.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, weaker_states, beneficiary,
    moderate, generational, constrained, global).

% The UN Security Council, International Court of Justice, and international legal regime preserve their authority to adjudicate legitimate use of force. The narrow reading requires states to go through these bodies for any use of force beyond response to actual/imminent armed attack. Institutions gain role and discretion; their legitimacy depends on states respecting the constraint.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, multilateral_institutions, beneficiary,
    institutional, generational, analytical, global).

% Bear the strategic constraint: they cannot unilaterally declare preventive war, preemptive action against emerging threats, or respond to non-state actor attacks originating from permissive host states without going through the Security Council (where they may be blocked). Their material interest is in broader self-defense authority; the reading constrains their options to actual/imminent attacks by states or state-attributable actors. Exit is constrained by reputational cost of disregarding international law and by Security Council blocking power.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, powerful_states, payer,
    institutional, biographical, constrained, global).

% Military and intelligence establishments in powerful states are constrained in their operational doctrines and strategic planning. They cannot plan preemptive strikes against emerging threats, non-state actors, or countries hosting non-state threats without triggering international legal violation. Their operational doctrine must wait for actual/imminent attack or go through multilateral authorization. They bear the cost of operational constraint; exit requires the powerful state to accept international legal censure.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, security_establishments, payer,
    institutional, biographical, constrained, global).

% Host states with non-state actors on their territory occupy a structurally ambiguous position excluded from the five-questions conversation. Under the narrow reading, they are protected from unilateral attack UNLESS the powerful state can demonstrate the attack is attributable to a state actor (impossible if the threat is genuinely non-state) OR the host state is 'unwilling or unable' to suppress the threat (the contested alternative reading). Their silence in the framework is the constraint's blind spot.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, non_state_threat_hosts, excluded,
    moderate, biographical, trapped, global).

% Would argue for expansive preventive self-defense but are silenced by the reading's core premise: actual/imminent attack only. They could challenge the constraint through doctrine, precedent, or state practice, but doing so invites international legal censure and Security Council action. Their exclusion is ideological/jurisdictional, not structural.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, states_claiming_preventive_authority, excluded,
    institutional, biographical, constrained, global).

% Adjudicates the reading's application: does a particular armed attack constitute an 'actual or imminent' armed attack? Is it 'attributable' to a state? Has the narrow reading been violated? The ICJ sets the interpretive boundaries and enforces the constraint through advisory opinions and contentious cases. It serves both as a neutral arbiter and as an institutional seat whose authority depends on states respecting the narrow reading.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, international_court_of_justice, agenda_setter,
    institutional, generational, analytical, global).

% Interprets and contests the reading through scholarship, commentary on state practice, and influence on judicial decisions. They serve as an observer and analytical seat; their role is to make the reading's structural implications visible and to document where practice diverges from the constraint.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, academic_international_law_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__narrow_armed_attack_reading, powerful_states).
narrative_ontology:fixing_cost_class(article_51_self_defense__narrow_armed_attack_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared framework for determining when force is legitimate: only when responding to actual or imminent armed attack by a state or state-attributable actors. This solves the collective-action problem of distinguishing legitimate self-defense from opportunistic aggression, allowing states to plan security posture against known attack thresholds rather than existential preemption spirals.
% TRANSFER_FUNCTION: Transfers strategic autonomy from powerful states (which lose unilateral preventive authority) to weaker states and multilateral institutions (which gain protection and role). Powerful states lose the ability to unilaterally declare necessity; multilateral bodies gain gatekeeping authority. Weaker states gain predictability: they will not be attacked for hosting non-state actors unless the powerful state can prove state attribution or get Security Council approval.
% ABSENT_VOICES: States arguing for preventive self-defense doctrine are silenced: their advocacy for broad self-defense against emerging threats is excluded from the framework. Host states of non-state actors have no voice in defining whether they are 'unwilling or unable' to suppress threats — the powerful state makes that determination unilaterally (the 'unable/unwilling' doctrine is a contested sibling reading). Intelligence communities and security establishments cannot openly argue for their preferred doctrinal breadth without appearing to advocate international law violation.
% DISAPPEARANCE_RATIONALE: If the narrow reading disappeared and self-defense reverted to expansive preventive authority, powerful states would resume unilateral force against non-state threats, host states, and emerging threat countries without Security Council gatekeeping. Weaker states would lose the institutional shield; the multilateral system would fragment; the security environment would shift to unilateral deterrence and counter-deterrence. The constraint's removal would reorganize the global security order fundamentally.
% FOUNDING_PROBLEM: Post-World War II collective security system required a shared definition of legitimate force to prevent great-power aggression disguised as self-defense. The narrow reading was established to close the loophole that permitted one great power to unilaterally declare another's existence a threat and attack preemptively. The founding problem: how to distinguish self-defense from aggression when self-defense claims are costless and expanding.
% FOUNDING_PROBLEM_CORROBORATION: The UN General Assembly and International Court of Justice affirm the narrow reading as the canonical constraint (Nicaragua case, General Assembly resolutions on Use of Force). Powerful states attesting preventive doctrine argue the founding problem is obsolete in the terrorism era: non-state threats render the imminent attack requirement inoperable. Academic experts outside the security establishment, Global South governments, and international law commissions testify the founding problem remains live: powerful states' preventive claims would destabilize the system without constraint. No single external voice corroborates — the dispute is between institutional authorities (ICJ narrow reading) and state practice (powerful states' preventive doctrine in action).
narrative_ontology:disappearance_verdict(article_51_self_defense__narrow_armed_attack_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__narrow_armed_attack_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__narrow_armed_attack_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_51_self_defense__narrow_armed_attack_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__narrow_armed_attack_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__narrow_armed_attack_reading_tests).
:- end_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.31: the constraint does extract strategic autonomy from powerful states (preventing preventive war, requiring Security Council authorization or actual attack threshold), but this extraction is not pure coercion — it serves a genuine coordination function (the imminent-attack standard is more administrable than preventive necessity). Suppression is low (0.22) because the constraint is nominally accepted by all states; no party openly rejects it. What varies is compliance: powerful states operate doctrines (unable/unwilling, preventive action) that stretch the narrow reading to breaking points, but they do so through reinterpretation rather than open violation. Theater_ratio is 0.18: the constraint's enforcement apparatus (ICJ adjudication, General Assembly debate, state legal arguments) has grown theatrically over the interval, but the underlying functional constraint (actual/imminent attack requirement) remains partially operative. Accessibility_collapse is 0.72: once the narrow reading is understood, alternatives (preventive authority, non-state actor trigger, willing-but-ineffective host standard) are technically available but carry high political cost (international legal violation, Security Council action, loss of legitimacy). Resistance is 0.68: the constraint meets constant, high-level resistance from powerful states' security establishments and from state practice (preventive operations that violate the reading), yet the constraint persists because weaker states and institutions defend it actively. The measurements show extractiveness rising 1945–2001 (the 9/11 pivot moment when suppression spiked as powerful states tested the constraint intensely) then plateauing: the constraint's extractive force stabilized once the unable/unwilling doctrine crystallized as a parallel reading that partially absorbed the pressure. Theater rose 1945–2001, plateaued, and remained elevated 2001–2026: the constraint's enforcement became increasingly performative (more adjudication, more legal argument, less restraint on powerful state action) even as extractiveness stabilized. This profile is tangled_rope: genuine coordination (imminent attack standard is administrable) PLUS asymmetric extraction (weaker states benefit, powerful states pay) PLUS active enforcement (Security Council gatekeeping, ICJ jurisdiction, legal pressure).
 *
 * PERSPECTIVAL GAP:
 *   The powerful-state seat and the weaker-state seat compute fundamentally differently. From the powerful-state security-establishment seat, the constraint is a strategic loss: legitimate preventive action is foreclosed, emerging threats cannot be preempted, and the Security Council veto allows adversaries to shield themselves. The reading appears as an extraction mechanism protecting rivals. From the weaker-state seat, the same constraint is protection: powerful states cannot attack them unilaterally under preventive doctrine, and the Security Council veto gives them blocking power. The reading appears as coordination — a shared standard that prevents aggression disguised as self-defense. The ICJ and multilateral institutions compute the constraint as legitimate authority-preservation: their role is necessary to adjudicate self-defense and enforce the narrow reading. All three seats experience the same structural constraint, but its experienced directionality (how it extracts or benefits them) diverges sharply. The engine computes this divergence from power-level + exit-options + beneficiary/victim declarations; the claim (tangled_rope) is structurally true because the constraint solves coordination AND asymmetrically distributes the cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Weaker states: d ≈ 0.2 (beneficiary). They benefit from the constraint (protected from preventive attack), their exit options are constrained (Security Council veto is all they have), and their power is low (institutional). Beneficiary derivation applies; they compute low directionality. Multilateral institutions: d ≈ 0.15 (beneficiary). Their role and authority depend on states respecting the constraint; they are not targets but frameworks. Powerful states: d ≈ 0.75 (target). They pay the strategic cost (preventive authority foreclosed), their exit options are constrained (open violation invites legal action and Security Council response; informal violation risks precedent), and their power is high (institutional). Target derivation applies; they compute high directionality. Security establishments: d ≈ 0.80 (target). They pay the operational cost directly (doctrine constrained, planning horizons narrowed, emerging threats cannot be addressed preemptively), their exit is constrained by the powerful state's political commitment to the constraint, and they have institutional but subordinate power. The directionality overrides section is not needed here: the structural derivation captures the asymmetry correctly. The constraint's extractiveness is amplified for the powerful-state targets (high d, institutional power, global scope) and inverted into subsidy for weaker-state beneficiaries (low d, constrained power, global scope).
 *
 * MANDATROPHY ANALYSIS:
 *   The narrow reading shows early signs of mandatrophy: the founding problem (distinguishing self-defense from aggression in a world of great-power conflict) was live in 1945 and remains theoretically live in 2026, but the constraint's actual suppressive force has decayed. The measurement series shows theater_ratio rising sharply 2001–2015 (the War on Terror era, when preventive doctrine was tested intensely) then stabilizing at 0.18: the constraint produces legal arguments and ICJ adjudications (theater) but fails to suppress powerful state preventive action (suppression stayed at 0.22). The unable_unwilling doctrine emerged as a shadow reading that preserves the narrow reading's form while gutting its function: powerful states can now claim non-state attacks are attributable to 'unwilling' host states, collapsing the state-attribution requirement and shifting from narrow to expansive authority without formally renouncing the narrow reading. This is classic Piton behavior — the original constraint (actual/imminent state attack) is still cited but its protective function is eroded by a compatible reinterpretation. The constraint is not fully Piton yet (suppression and extractiveness are not negligible; weaker states still benefit from the formal constraint), but the trajectory is toward atrophy. The mandatrophy is NOT resolved; it is in process. The founding problem remains live (preventing aggression disguised as self-defense), but the constraint's capacity to solve it has degraded. The engine's computation will detect this as a divergence between claimed type (tangled_rope) and computed type (possibly piton or snare depending on per-seat calculations): the weaker-state seat will compute tangled_rope (genuine coordination, asymmetric protection), while the powerful-state seat will compute something closer to snare or piton (the constraint is nominal, enforcement is minimal, extraction continues under alternative framings).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_narrow_vs_expansive,
    'Is self-defense legitimately constrained to actual/imminent attacks by states, or does the international legal kernel permit expansive preventive authority against non-state and emerging threats?',
    'State practice over the next decade: do powerful states continue preventive operations (falsifying the narrow reading''s authority) or defer to the constraint? Do major states ratify amendments affirming narrow reading or draft preventive doctrines claiming legal justification?',
    'If practice systematically violates the narrow reading, the constraint''s enforceability collapses and the reading becomes performative (high theater_ratio, low suppression as powerful states ignore it). If practice defers, the narrow reading maintains authority. The sibling readings (expansive_preventive, unable_unwilling) will be instantiated as separate constraints depending on which practice dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_narrow_vs_expansive, empirical, 'Whether the narrow reading''s core premise (actual/imminent state-attributed attack only) is sustained by state practice or eroded by preventive doctrine.').

omega_variable(
    state_attribution_determination_ambiguity,
    'Who determines whether a non-state actor attack is attributable to a host state? Is attribution a legal test with discoverable answers, or a political judgment masquerading as law?',
    'Examine ICJ determinations and state arguments in practice: do parties disagree on facts (an empirical disagreement the law resolves) or on standards of proof and causal chains (a doctrinal disagreement the law leaves open)? Compare Nicaragua, Wall advisory opinion, and subsequent cases.',
    'If attribution is determinate, the constraint is legally administrable and the narrow reading is robust. If attribution is indeterminate, powerful states can claim non-state attacks are state-attributable without evidence, inflating effective self-defense authority and making the constraint performative (narrow reading claims to apply but applies to almost everything).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_attribution_determination_ambiguity, empirical, 'Whether state attribution is a discoverable legal fact or a political determination with legal language.').

omega_variable(
    imminence_threshold_drift,
    'What constitutes ''imminent'' armed attack? Does imminence require days/weeks (literal immediacy) or does it drift to include months or years of observed threat preparation?',
    'Track state practice and ICJ case law on imminent attack determinations: Caroline doctrine (immediate threat), versus post-9/11 expansive imminence (capability + intent over horizons of years). Examine how major powers justify preventive actions and whether courts accept the justifications.',
    'If imminence drifts, the constraint effectively expands: powerful states can claim imminent attacks years in advance. The narrow reading''s constraining power erodes through semantic drift rather than explicit challenge. Theater_ratio would rise (more justificatory apparatus, less actual constraint). The constraint becomes a Piton — structurally present but functionally atrophied.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(imminence_threshold_drift, empirical, 'Whether imminence remains a tight temporal constraint or drifts to encompass long-horizon threat perception.').

omega_variable(
    sibling_reading_institutional_pressure,
    'As non-state threats (terrorism, cyber, pandemics) rise in salience, does institutional and state practice pressure the narrow reading toward the unable_unwilling doctrine (the middle-ground sibling) or toward expansive_preventive authority?',
    'Monitor ICJ advisory opinions, state arguments in use-of-force debates, General Assembly resolutions on counterterrorism, and emerging customary law claims. Track which sibling reading gains state endorsement and institutional support over the 2026-2035 interval.',
    'Institutional pressure toward unable_unwilling would create a hybrid constraint (separate story) that preserves narrow reading''s formal authority while opening exceptions for non-state threats. This maintains theater (the narrow reading is still cited) while expanding effective authority. Pressure toward expansive_preventive would openly contest the narrow reading (forecloses relation confirmed). The narrow reading''s survival depends on institutional resistance to both siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_institutional_pressure, empirical, 'Whether the narrow reading survives institutional pressure or is displaced by sibling readings in practice.').

omega_variable(
    powerful_state_compliance_asymmetry,
    'Do powerful states comply with the narrow reading when constrained by it, or do they systematically violate it and rely on Security Council veto and soft enforcement to avoid consequences?',
    'Analyze Security Council voting records on force resolutions, state practice in preventive actions, and political consequences (sanctions, isolation, etc.) following Article 51 violations. Measure compliance rate for powerful states vs. other states.',
    'High compliance asymmetry would indicate the constraint is enforced unequally: weaker states conform; powerful states are protected by institutional veto. This transforms the constraint from a coordination rule into an extraction mechanism (powerful states extract the benefit of constraint on others while exempting themselves). Suppression remains low because the constraint is nominally respected; extractiveness remains moderate because powerful states selectively violate. This reading would compute as tangled_rope on powerful state seats and as snare on weaker state seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(powerful_state_compliance_asymmetry, empirical, 'Whether the narrow reading constraint is equally enforced across power levels or asymmetrically applied.').

omega_variable(
    reading_specification_vs_kernel_ambiguity,
    'Does the UN Charter''s Article 51 language (nothing in the charter impairs the right to self-defense) actually specify the narrow reading, or is the narrow reading a particular interpretation of ambiguous kernel language?',
    'Textual analysis of Article 51 and preparatory works (travaux préparatoires): does the text exclude preventive action or merely fail to address it? Examine whether the narrow reading is derived from the kernel or imported from post-hoc jurisprudence.',
    'If the kernel is genuinely ambiguous, the narrow reading is one legitimate interpretation competing with others; sibling readings are not aberrations but alternative valid readings. If the kernel specifies the narrow reading, sibling readings are doctrinal drift or violation. This affects whether the kernel is viewed as fixed or contestable — a fundamental question about the framework''s legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_specification_vs_kernel_ambiguity, conceptual, 'Whether the narrow reading is specified by the kernel or imposed on ambiguous kernel language.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__narrow_armed_attack_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement_basis(arti_tr_t1945, observed).
narrative_ontology:measurement(arti_tr_t1970, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement_basis(arti_tr_t1970, observed).
narrative_ontology:measurement(arti_tr_t1990, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement_basis(arti_tr_t1990, observed).
narrative_ontology:measurement(arti_tr_t2001, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2001, 0.22).
narrative_ontology:measurement_basis(arti_tr_t2001, observed).
narrative_ontology:measurement(arti_tr_t2015, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement_basis(arti_tr_t2015, observed).
narrative_ontology:measurement(arti_tr_t2026, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2026, 0.18).
narrative_ontology:measurement_basis(arti_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1945, 0.18).
narrative_ontology:measurement_basis(arti_be_t1945, observed).
narrative_ontology:measurement(arti_be_t1970, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1970, 0.22).
narrative_ontology:measurement_basis(arti_be_t1970, observed).
narrative_ontology:measurement(arti_be_t1990, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement_basis(arti_be_t1990, observed).
narrative_ontology:measurement(arti_be_t2001, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2001, 0.35).
narrative_ontology:measurement_basis(arti_be_t2001, observed).
narrative_ontology:measurement(arti_be_t2015, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2015, 0.31).
narrative_ontology:measurement_basis(arti_be_t2015, observed).
narrative_ontology:measurement(arti_be_t2026, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2026, 0.31).
narrative_ontology:measurement_basis(arti_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1945, 0.15).
narrative_ontology:measurement_basis(arti_su_t1945, observed).
narrative_ontology:measurement(arti_su_t1970, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1970, 0.18).
narrative_ontology:measurement_basis(arti_su_t1970, observed).
narrative_ontology:measurement(arti_su_t1990, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement_basis(arti_su_t1990, observed).
narrative_ontology:measurement(arti_su_t2001, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2001, 0.28).
narrative_ontology:measurement_basis(arti_su_t2001, observed).
narrative_ontology:measurement(arti_su_t2015, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2015, 0.22).
narrative_ontology:measurement_basis(arti_su_t2015, observed).
narrative_ontology:measurement(arti_su_t2026, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2026, 0.22).
narrative_ontology:measurement_basis(arti_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__narrow_armed_attack_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_51_self_defense__narrow_armed_attack_reading, 0.12).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense__expansive_preventive_reading).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense__unable_unwilling_doctrine_reading).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, use_of_force_authorization_security_council).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, non_state_actor_attribution_standard).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, imminence_threshold_definition).

% DUAL FORMULATION NOTE:
% The Article 51 self-defense kernel decomposes into three structurally distinct constraint stories, distinguished by their readings of the core premise (who counts as an attacker and when does self-defense apply). The narrow_armed_attack_reading instantiated here (state-attribution + imminent-temporal requirement) is the foundational constraint; the sibling readings (expansive_preventive and unable_unwilling) are downstream constraints that reinterpret or extend the kernel. The narrow reading forecloses the expansive reading (both cannot be held in a single consistent legal framework); coexists_with the unable_unwilling reading (different state coalitions hold each, and unable_unwilling preserves narrow-reading language while functionally opening exceptions). All three stories link via network.affects_constraints to show the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_51_self_defense__narrow_armed_attack_reading, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
