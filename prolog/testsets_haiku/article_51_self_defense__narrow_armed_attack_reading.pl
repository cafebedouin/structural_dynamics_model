% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__narrow_armed_attack_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: article_51_self_defense__narrow_armed_attack_reading
 *   human_readable: Article 51 Self-Defense (Narrow Armed Attack Reading)
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   Article 51 of the UN Charter establishes the right of individual or
 *   collective self-defense if an armed attack occurs. This constraint story
 *   instantiates the NARROW ARMED ATTACK READING: self-defense is triggered
 *   only by actual or imminent armed attacks carried out by or attributable
 *   to a state under international law. Non-state actor threats do not
 *   trigger Article 51 unless causally linked to the host state via effective
 *   control or public direction (the ICJ's attribution test). This reading
 *   constrains the strategic freedom of powerful states while preserving the
 *   authority of weaker states and multilateral institutions. The narrow
 *   reading is institutionally entrenched in the UN system and International
 *   Court of Justice doctrine, but it has been contested since at least 2001
 *   when powerful states faced non-state actor threats they deemed
 *   existential. The sibling readings (expansive_preventive_reading and
 *   unable_unwilling_doctrine_reading) offer broader triggers; this story
 *   models only the narrow reading.
 *
 * KEY AGENTS:
 *   - Weaker state sovereigns (benefit from constraint; protected from preemptive strikes)
 *   - Powerful state militaries (pay through constrained strategic freedom)
 *   - Multilateral institutional authority / UN system (benefit from preserved gate-keeping role)
 *   - Non-state actor host states (constrained by attribution liability, excluded from Article 51 themselves)
 *   - International law interpreters (courts, ICJ, scholars; observe and adjudicate)
 *   - Non-state armed groups (excluded from the reading; their attacks do not themselves trigger Article 51)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, 0.38).
domain_priors:suppression_score(article_51_self_defense__narrow_armed_attack_reading, 0.22).
domain_priors:theater_ratio(article_51_self_defense__narrow_armed_attack_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__narrow_armed_attack_reading, mountain).
narrative_ontology:human_readable(article_51_self_defense__narrow_armed_attack_reading, "Article 51 Self-Defense (Narrow Armed Attack Reading)").
narrative_ontology:topic_domain(article_51_self_defense__narrow_armed_attack_reading, "international_law/security_studies").

domain_priors:emerges_naturally(article_51_self_defense__narrow_armed_attack_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__narrow_armed_attack_reading, 'f37b15d9-50f5-4aa0-9df0-ff137384a4a1').
narrative_ontology:cs_kernel_codification('f37b15d9-50f5-4aa0-9df0-ff137384a4a1', fixed_text).
narrative_ontology:cs_authority_grounding('f37b15d9-50f5-4aa0-9df0-ff137384a4a1', lineage).
narrative_ontology:cs_interpretation_layer_present('f37b15d9-50f5-4aa0-9df0-ff137384a4a1').
narrative_ontology:cs_reading_relation('f37b15d9-50f5-4aa0-9df0-ff137384a4a1', article_51_self_defense__expansive_preventive_reading, forecloses).
narrative_ontology:cs_reading_relation('f37b15d9-50f5-4aa0-9df0-ff137384a4a1', article_51_self_defense__unable_unwilling_doctrine_reading, coexists_with).
narrative_ontology:cs_axiom('f37b15d9-50f5-4aa0-9df0-ff137384a4a1', foundational, armed_attack_requires_state_attribution).
narrative_ontology:cs_axiom_status(armed_attack_requires_state_attribution, holdable).
narrative_ontology:cs_axiom_grounding('f37b15d9-50f5-4aa0-9df0-ff137384a4a1', armed_attack_requires_state_attribution, deontological).
narrative_ontology:cs_axiom('f37b15d9-50f5-4aa0-9df0-ff137384a4a1', foundational, self_defense_responds_not_prevents).
narrative_ontology:cs_axiom_status(self_defense_responds_not_prevents, holdable).
narrative_ontology:cs_axiom_grounding('f37b15d9-50f5-4aa0-9df0-ff137384a4a1', self_defense_responds_not_prevents, conventional).
narrative_ontology:cs_reference_frame('f37b15d9-50f5-4aa0-9df0-ff137384a4a1', treaty_self_defense_responding_to_attack).
narrative_ontology:cs_drift_state('f37b15d9-50f5-4aa0-9df0-ff137384a4a1', post_2001_non_state_actor_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f37b15d9-50f5-4aa0-9df0-ff137384a4a1', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, weaker_state_sovereigns).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, multilateral_institutional_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, attacked_state_civilians).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, powerful_state_militaries).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, non_state_actor_host_states).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, attacked_state_civilians).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, proportionality_principle).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, legal_necessity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Smaller, less militarily capable states benefit from the constraint's narrow reading because it prevents larger powers from invoking self-defense as a cover for expansionist action. The constraint protects their borders from preemptive strikes or punitive interventions justified as responses to speculative threats. Their security depends on the legal rule limiting what counts as a trigger for legitimate force.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, weaker_state_sovereigns, beneficiary,
    moderate, generational, analytical, global).

% Their strategic flexibility is constrained by the requirement that self-defense respond only to actual or imminent armed attacks attributable to a state under international law. They cannot unilaterally invoke self-defense against non-state actor threats originating from a host state they deem unwilling or unable to respond, nor can they launch preemptive strikes against emerging threats. They must either seek UN Security Council authorization, suffer the attack, or act outside the legal framework and face attribution/censure.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, powerful_state_militaries, payer,
    powerful, biographical, constrained, global).

% The UN system and international law institutions preserve their gatekeeping authority when self-defense is narrowly defined. Powerful states that face non-state actor threats must petition the Security Council rather than act unilaterally, which maintains the collective security model and the institutional hierarchy. Broader self-defense readings would erode this authority by permitting unilateral justifications.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, multilateral_institutional_authority, beneficiary,
    institutional, generational, analytical, universal).

% States that harbor non-state armed groups face a narrow version of liability under this reading: they are only responsible for attacks attributable to them under international law's stringent tests (effective control or public direction). They are constrained from tolerating non-state actors that launch attacks, but they are not held liable for mere presence of a group unless causation and attribution are proven. They also cannot invoke self-defense against the host state's counter-operations unless the counter-operation itself is an actual or imminent armed attack by the host state.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, non_state_actor_host_states, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__narrow_armed_attack_reading, non_state_actor_host_states, excluded).

% Non-state actors that launch attacks do not themselves trigger Article 51 self-defense; their attacks may trigger forcible response from the target state, but only if the response is attributable to a state under international law or is otherwise justified. They are outside the constraint's frame — they have no seat at the negotiation over what counts as triggering self-defense, and the constraint shapes whether states can respond to them unilaterally.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, non_state_armed_groups, excluded,
    powerless, immediate, trapped, local).

% They benefit from the constraint when it prevents larger powers from using speculative threats as a pretext for intervention that would destabilize their region. They also bear costs if the narrow constraint prevents rapid response to actual non-state actor attacks and they are trapped between the threat and legal delay. The constraint shapes whether their government can respond immediately or must wait for multilateral authorization.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, attacked_state_civilians, beneficiary,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__narrow_armed_attack_reading, attacked_state_civilians, payer).

% Courts, treaty bodies, and legal scholars assess whether specific attacks and responses satisfy the constraint's terms. They interpret what constitutes an 'armed attack,' what 'imminent' means, and what attribution standard applies. Their interpretations shape whether states' actions are deemed lawful or violations, and they carry authority from the legal tradition itself.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, international_law_interpreters, observer,
    institutional, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared legal standard for when military force in self-defense is permissible under international law, preventing states from unilaterally claiming self-defense as a pretext for expansion and preserving the collective security model. The constraint coordinates state behavior by defining a common boundary between legitimate self-help and illegal aggression.
% TRANSFER_FUNCTION: Transfers strategic freedom (the ability to unilaterally decide when force is justified) from powerful states with unilateral capacity to a multilateral system in which smaller states and institutional gatekeepers have veto or delay authority. Powerful states must seek Security Council authorization or act outside the legal framework and face consequences; weaker states and institutions gain de facto power to slow or prevent unilateral action.
% ABSENT_VOICES: Non-state actors whose attacks would not trigger Article 51 are excluded from the reading of what constitutes a valid trigger — their violence does not legally authorize response under this framework without host-state attribution. Powerful states that would prefer broader self-defense rights are structurally constrained and would argue for an expansive reading but are held in place by treaty text and institutional consensus.
% DISAPPEARANCE_RATIONALE: If this constraint — the narrow reading of self-defense — were abandoned, powerful states would unilaterally claim self-defense against non-state actors in host states, preemptive strikes would proliferate, the UN Security Council's gate-keeping role would erode, and the international legal order would shift from a multilateral model to a unilateral power-based system. Regional powers and weaker states would lose the legal protection the constraint provides. The constraint is what holds the collective security model together.
% FOUNDING_PROBLEM: After World War II, states needed a shared rule defining when military force is legitimate self-defense versus illegal aggression, to prevent the cycles of escalation and preemption that had driven conflict. The constraint was designed to preserve state sovereignty while establishing a bright line: self-defense responds to actual or imminent armed attacks, not speculative threats, and the response must be proportional and reported to the Security Council.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is still cited by advocates of the narrow reading and by international law scholars who emphasize collective security and legal certainty. However, powerful states that have faced non-state actor threats (particularly after 2001) dispute whether the problem is adequately addressed by the narrow reading — they argue that non-state actors cannot be deterred by the prospect of Security Council vetoes and that the constraint leaves them vulnerable. Legal scholars outside the NATO/powerful-state alignment (from the Global South, from the UN Secretariat, from institutions like the International Court of Justice) attest that the founding problem remains live: the constraint prevents the unilateral force escalation that destabilizes weaker regions.
narrative_ontology:disappearance_verdict(article_51_self_defense__narrow_armed_attack_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__narrow_armed_attack_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__narrow_armed_attack_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(article_51_self_defense__narrow_armed_attack_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, ExtMetricName, E),
    domain_priors:suppression_score(article_51_self_defense__narrow_armed_attack_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(article_51_self_defense__narrow_armed_attack_reading),
    narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The narrow reading produces moderate extractiveness (0.38) because powerful states genuinely bear a constraint on their strategic freedom, but the constraint is not total — they retain the ability to seek Security Council authorization, to argue for imminent threat, or to act outside the framework. Suppression is low (0.22) because the constraint is anchored in treaty text and institutional doctrine that all states recognize as binding; resistance is high (0.67) because powerful states continuously push against the constraint through practice (drone strikes, 'unable/unwilling' doctrine, proxy forces) and legal reinterpretation. Theater is low (0.12) — the constraint's enforcement is mostly about legal interpretation and attribution disputes, not performative compliance. Accessibility collapse is very high (0.91): once a state grasps the constraint, the alternatives (unilateral preemption, expansion, preventive war) are legally foreclosed unless the state exits the treaty framework entirely or reinterprets the reading. The constraint emerges as natural law because it is grounded in treaty text and has accrued 80 years of institutional consensus, yet the measurement profile (rising extractiveness post-2001, persistent high resistance) signals that the naturalness is contested when powerful states face non-state threats.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of weaker states and the UN institutional view, the constraint is genuinely natural law — it is the foundation of the legal order that protects them. From the perspective of powerful state militaries facing non-state actor threats (post-2001), the constraint is increasingly experienced as an extractive limitation on legitimate self-help. The engine should compute different types at each seat: the institutional and weaker-state seats should see a mountain (natural law that protects them); the powerful-state seats should compute closer to tangled_rope or snare (a constraint they bear and whose persistence they dispute). The narrow reading itself is the claim; the measured extraction reflects the power asymmetry in how different seats experience it.
 *
 * DIRECTIONALITY LOGIC:
 *   Weaker states and multilateral institutions are beneficiaries (d near 0.0): the constraint protects them and they have no reason to exit it. Powerful states are targets (d near 1.0): they bear the cost of constrained force options and continuously resist, but they are also bound by the legal framework because exit would delegitimize them globally. Non-state actor host states are ambiguously positioned: they are constrained by liability but also protected by the attribution requirement. The directionality derivation is straightforward: beneficiary group = {weaker_state_sovereigns, multilateral_institutional_authority}, victim group = {powerful_state_militaries}. No overrides are needed; the structural data maps cleanly to directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint scores as mountain because it meets the natural-law criteria: it is grounded in a treaty kernel (the UN Charter Article 51 text) and has achieved near-universal acceptance as a legal principle. The founding problem (preventing cycles of unilateral escalation) is live in the sense that the constraint does prevent this cycle, though the effectiveness is contested. The constraint's persistence depends partly on institutional consensus and partly on the fact that exit is costly (a state that renounces it faces diplomatic isolation). The measurement data show an uptick in resistance post-2001 (when non-state actor threats became salient) and a corresponding rise in theater_ratio and extractiveness, but the trajectory plateaus — this is NOT mandatrophy because the constraint's core function (preventing unilateral major-power expansion) remains intact. The constraint is not dead; it is under pressure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_vs_constructed_constraint,
    'Is the narrow armed attack reading a natural law of international relations (the necessary limit on self-defense that emerges from state coordination), or is it a constructed constraint that benefits weaker states and multilateral institutions at the expense of powerful states'' security interests?',
    'Examine whether powerful states would converge on the narrow reading absent institutional pressure and without the benefit from other states'' compliance. If they would abandon it to embrace a broader doctrine when facing non-state threats, the constraint is constructed, not natural.',
    'If natural, the constraint belongs in the mountain category; if constructed, it should be reclassified to tangled_rope or snare (coordination benefit for some, extraction cost for others, requiring active institutional enforcement).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturalness_vs_constructed_constraint, conceptual, 'Whether the narrow reading is grounded in inherent logic or in beneficiary-backed institution building.').

omega_variable(
    attribution_test_ambiguity,
    'What standard of attribution — effective control, overall control, or public direction — correctly operationalizes the narrow reading''s requirement that an armed attack be ''by a state''? The ICJ adopted effective control (Nicaragua case), but state practice and ICRC doctrine vary.',
    'Track state practice and treaty interpretation: do states consistently apply one test, or do powerful states apply different standards to attacks favorable/unfavorable to them?',
    'If attribution standards diverge by actor or interest, the constraint becomes an extraction mechanism for those who can set the standard rather than a neutral rule. The constraint''s type would shift from mountain to tangled_rope or snare at the institutional seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(attribution_test_ambiguity, empirical, 'Whether attribution is a stable, neutral standard or a site of power-driven reinterpretation.').

omega_variable(
    imminent_threat_definition_drift,
    'How much operational latitude does ''imminent'' threat provide for powerful states? If imminent can mean ''within 6 months'' or ''when intelligence suggests capability and intent are converging,'' the narrow reading becomes practically broader. If it means ''hours or days,'' it is genuinely narrow.',
    'Analyze state invocations of imminence and international law scholarship: where do interpreters place the boundary, and has it shifted over time (especially post-2001)?',
    'If imminence is operationally broader than traditionally understood, the extractiveness of the constraint on powerful states is lower than authored; if it remains narrow, the extractiveness is higher. The measurement post-2001 may reflect either actual constraint tightening or powerful-state resistance to interpretations they view as increasingly restrictive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imminent_threat_definition_drift, empirical, 'Whether ''imminent'' is a stable, narrow operational standard or a site of expansive interpretation.').

omega_variable(
    unable_unwilling_doctrine_coexistence,
    'The unable/unwilling doctrine is held as an alternative reading by some powerful states; does it logically foreclose the narrow reading, coexist with it as a live faction, or influence it toward practical broadening?',
    'Assess whether states holding unable/unwilling are claiming it as a supplementary ground (coexists) or as a replacement (forecloses) for the narrow reading.',
    'If coexistence, the narrow reading''s institutional entrenchment is weaker than it appears — the reading survives only because some powerful states have not yet formally defected. If influences, the narrow reading is shifting toward practical broadening via interpretation rather than via explicit reading-change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unable_unwilling_doctrine_coexistence, conceptual, 'Whether unable/unwilling doctrine is coexisting alternative, foreclosing competitor, or incremental influence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__narrow_armed_attack_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement_basis(arti_tr_t1945, observed).
narrative_ontology:measurement(arti_tr_t1975, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1975, 0.08).
narrative_ontology:measurement_basis(arti_tr_t1975, observed).
narrative_ontology:measurement(arti_tr_t1990, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1990, 0.09).
narrative_ontology:measurement_basis(arti_tr_t1990, observed).
narrative_ontology:measurement(arti_tr_t2001, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2001, 0.14).
narrative_ontology:measurement_basis(arti_tr_t2001, observed).
narrative_ontology:measurement(arti_tr_t2015, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2015, 0.12).
narrative_ontology:measurement_basis(arti_tr_t2015, observed).
narrative_ontology:measurement(arti_tr_t2024, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2024, 0.12).
narrative_ontology:measurement_basis(arti_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1945, 0.22).
narrative_ontology:measurement_basis(arti_be_t1945, observed).
narrative_ontology:measurement(arti_be_t1975, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1975, 0.28).
narrative_ontology:measurement_basis(arti_be_t1975, observed).
narrative_ontology:measurement(arti_be_t1990, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1990, 0.32).
narrative_ontology:measurement_basis(arti_be_t1990, observed).
narrative_ontology:measurement(arti_be_t2001, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2001, 0.42).
narrative_ontology:measurement_basis(arti_be_t2001, observed).
narrative_ontology:measurement(arti_be_t2015, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2015, 0.38).
narrative_ontology:measurement_basis(arti_be_t2015, observed).
narrative_ontology:measurement(arti_be_t2024, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2024, 0.38).
narrative_ontology:measurement_basis(arti_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1945, 0.15).
narrative_ontology:measurement_basis(arti_su_t1945, observed).
narrative_ontology:measurement(arti_su_t1975, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1975, 0.18).
narrative_ontology:measurement_basis(arti_su_t1975, observed).
narrative_ontology:measurement(arti_su_t1990, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1990, 0.19).
narrative_ontology:measurement_basis(arti_su_t1990, observed).
narrative_ontology:measurement(arti_su_t2001, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2001, 0.28).
narrative_ontology:measurement_basis(arti_su_t2001, observed).
narrative_ontology:measurement(arti_su_t2015, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2015, 0.22).
narrative_ontology:measurement_basis(arti_su_t2015, observed).
narrative_ontology:measurement(arti_su_t2024, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2024, 0.22).
narrative_ontology:measurement_basis(arti_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__narrow_armed_attack_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_51_self_defense__narrow_armed_attack_reading, 0.12).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense__expansive_preventive_reading).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense__unable_unwilling_doctrine_reading).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, international_humanitarian_law_proportionality).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, un_security_council_gatekeeping_authority).

% DUAL FORMULATION NOTE:
% The Article 51 self-defense kernel admits three structurally distinct readings, each with different ε values and beneficiary/victim structures. This file models the NARROW ARMED ATTACK READING — high constraint on powerful states, protection for weaker states and multilateral authority. Sibling constraints model the expansive and unable/unwilling readings. All three are linked via network.affects_constraints because each reading creates structural pressure on the others' legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_51_self_defense__narrow_armed_attack_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
