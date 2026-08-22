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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Article 51 Self-Defense: Narrow Armed Attack Reading
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested Article 51
 *   kernel—the narrow armed attack reading. Article 51 of the UN Charter
 *   states: 'Nothing in the present Charter shall impair the inherent right
 *   of individual or collective self-defence if an armed attack occurs
 *   against a Member of the United Nations.' The narrow reading interprets
 *   'armed attack' strictly: as a threshold of kinetic force by a state actor
 *   or by non-state actors attributable to a state under international law
 *   (or by a non-state actor operating from a territory where the host state
 *   is unwilling or unable to suppress them). This reading constrains
 *   powerful states' ability to invoke self-defense unilaterally and
 *   preserves the Security Council's gatekeeping role. The measurement series
 *   track how this constraint's extractiveness and suppression have shifted
 *   as the post-9/11 era introduced competing interpretations (the
 *   unable/unwilling doctrine, preventive self-defense theories) that
 *   challenge the narrow reading's dominance. The theater ratio rises sharply
 *   post-2001 (when rhetorical invocations of self-defense expand) but falls
 *   again by 2026 as international courts reassert the narrow interpretation
 *   and powerful states face reputational costs for overreach.
 *
 * KEY AGENTS:
 *   - Weaker states: benefit from the constraint because it prevents unilateral interventions justified by diffuse threats
 *   - Powerful states with strategic reach: bear the constraint; must demonstrate attribution or state responsibility to invoke self-defense
 *   - Multilateral institutions (UN Security Council, ICJ): benefit because the constraint preserves their authority
 *   - Non-state armed groups: excluded from the framework entirely; can only trigger self-defense claims if attributed to a state
 *   - Permanent Security Council members: possess veto power over interpretations but are also bound by the prevailing legal standard
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, 0.62).
domain_priors:suppression_score(article_51_self_defense__narrow_armed_attack_reading, 0.71).
domain_priors:theater_ratio(article_51_self_defense__narrow_armed_attack_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__narrow_armed_attack_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__narrow_armed_attack_reading, "Article 51 Self-Defense: Narrow Armed Attack Reading").
narrative_ontology:topic_domain(article_51_self_defense__narrow_armed_attack_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__narrow_armed_attack_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__narrow_armed_attack_reading, '763a43ac-e813-47ff-ae43-c040fc5e6de6').
narrative_ontology:cs_kernel_codification('763a43ac-e813-47ff-ae43-c040fc5e6de6', fixed_text).
narrative_ontology:cs_authority_grounding('763a43ac-e813-47ff-ae43-c040fc5e6de6', lineage).
narrative_ontology:cs_interpretation_layer_present('763a43ac-e813-47ff-ae43-c040fc5e6de6').
narrative_ontology:cs_reading_relation('763a43ac-e813-47ff-ae43-c040fc5e6de6', article_51_self_defense__expansive_preventive_reading, forecloses).
narrative_ontology:cs_reading_relation('763a43ac-e813-47ff-ae43-c040fc5e6de6', article_51_self_defense__unable_unwilling_doctrine_reading, coexists_with).
narrative_ontology:cs_axiom('763a43ac-e813-47ff-ae43-c040fc5e6de6', foundational, armed_attack_actual_or_imminent_requirement).
narrative_ontology:cs_axiom_status(armed_attack_actual_or_imminent_requirement, holdable).
narrative_ontology:cs_axiom_grounding('763a43ac-e813-47ff-ae43-c040fc5e6de6', armed_attack_actual_or_imminent_requirement, empirically_contingent).
narrative_ontology:cs_axiom('763a43ac-e813-47ff-ae43-c040fc5e6de6', foundational, state_attribution_or_state_responsibility_prerequisite).
narrative_ontology:cs_axiom_status(state_attribution_or_state_responsibility_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('763a43ac-e813-47ff-ae43-c040fc5e6de6', state_attribution_or_state_responsibility_prerequisite, deontological).
narrative_ontology:cs_reference_frame('763a43ac-e813-47ff-ae43-c040fc5e6de6', collective_security_framework_with_unilateral_exception).
narrative_ontology:cs_drift_state('763a43ac-e813-47ff-ae43-c040fc5e6de6', contemporary_post_9_11_counterterrorism_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('763a43ac-e813-47ff-ae43-c040fc5e6de6', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, weaker_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, multilateral_institutions).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, powerful_states_with_strategic_reach).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, host_states_harbouring_groups).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, host_states_harbouring_groups).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, sovereignty_principle).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, collective_security_authority).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, state_attribution_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a narrow self-defense reading because it prevents more powerful states from using diffuse non-state threats as justification for unilateral intervention. The constraint preserves the principle that only attacks attributable to another state (or the state's failure to control non-state actors within its borders) trigger Article 51. This protects weaker states from being treated as responsible for transnational groups they cannot fully control.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, weaker_states, beneficiary,
    moderate, generational, constrained, global).

% Bear the strategic constraint that self-defense claims require attribution to a state actor or state negligence/unwillingness to suppress. They cannot unilaterally declare war on non-state groups operating across borders without showing state responsibility or obtaining multilateral authorization. This limits their operational freedom and requires them to either demonstrate attribution (a high evidentiary bar) or seek Security Council approval.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, powerful_states_with_strategic_reach, payer,
    institutional, generational, constrained, global).

% The UN Security Council and international law institutions benefit from a narrow reading because it preserves their gatekeeping role. When powerful states must invoke Article 51, they are anchored to an interpretation that requires them to justify their claims against an objective standard (actual or imminent attack, state attribution) rather than subjective threat assessment. This preserves the collective security framework and the Council's authority to authorize or constrain responses.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, multilateral_institutions, beneficiary,
    institutional, generational, analytical, global).

% Are structurally excluded from direct legal standing under Article 51. They cannot invoke self-defense (the text is limited to states), and their actions trigger the constraint only insofar as they are attributed to a state sponsor or a permissive state. They would benefit from an expansive reading that recognized their own defensive claims, but the constraint structure locks them out of this framing entirely.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, non_state_armed_groups, excluded,
    moderate, biographical, trapped, global).

% Face a dual position: they bear a cost (attributability—if non-state groups operate from their territory, they can be held responsible for those groups under international law), but they also benefit from the narrow reading's requirement that an attack must be imminent or actual before triggering justified response. The constraint prevents pre-emptive strikes against potential threats, giving them time to suppress the groups themselves and avoid attribution.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, host_states_harbouring_groups, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__narrow_armed_attack_reading, host_states_harbouring_groups, beneficiary).

% Possess veto power over Article 51 interpretations through the Security Council's authority to determine what constitutes a threat to international peace and security. They can approve broader readings of self-defense or constrain them. They also have the material power to invoke self-defense claims themselves and thus have incentives to preserve some interpretation flexibility, but the narrow reading requires them to argue their case through the Council rather than act unilaterally on expansive doctrines.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, permanent_security_council_members, agenda_setter,
    institutional, generational, arbitrage, global).

% Interpret and apply Article 51 case by case. The narrow reading is the dominant interpretation in existing case law (Nicaragua case, Oil Platforms case), though these bodies acknowledge the interpretive contestation. They observe the constraint's enforcement and generate interpretive guidance, but lack direct enforcement machinery.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, international_courts_and_tribunals, observer,
    powerful, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__narrow_armed_attack_reading, multilateral_institutions).
narrative_ontology:fixing_cost_class(article_51_self_defense__narrow_armed_attack_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Constrains unilateral force claims to a shared, verifiable standard (actual or imminent attack; state attribution or state responsibility for non-state actors). This prevents each state from unilaterally defining 'self-defense' as any threat it perceives and reduces the risk of cascading claims to use force that destabilize the international order.
% TRANSFER_FUNCTION: Transfers the right to authorize force from individual states to the collective Security Council in all cases where Article 51 does not apply. Powerful states with strategic reach are constrained; weaker states and multilateral institutions gain gatekeeping power and protection from unilateral intervention.
% ABSENT_VOICES: Non-state armed groups have no standing in this framework at all—they cannot invoke Article 51. They would argue for recognition of their own defensive claims and would contest the attribution requirement that locks them into dependence on state framing. Actors advocating for broader preventive doctrines (some permanent Council members in specific geopolitical moments) are also structurally marginalized by this reading.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared—if the narrow reading were abandoned and states could invoke self-defense against any perceived threat without attribution or imminence requirements—the international order would reorganize around unilateral force claims. Powerful states would expand military operations against non-state groups and preventive targets; weaker states would lose protection; the Security Council would be circumvented. The multilateral framework would degrade.
% FOUNDING_PROBLEM: The UN Charter was drafted to prevent great powers from using vague security justifications to wage unilateral wars (the League of Nations failure). Article 51 needed an interpretation that constrained self-defense claims to clear, verifiable cases and preserved the Council's authority to authorize force collectively.
% FOUNDING_PROBLEM_CORROBORATION: The International Court of Justice, in the Nicaragua case (1986) and Oil Platforms case (2003), affirmed that Article 51 applies only to armed attacks and that the definition of 'armed attack' requires a threshold of force and attributability. Legal scholars outside any state's interest (the Institut de Droit International, UN Office of Legal Affairs) recognize the narrow reading as the prevailing interpretation. However, powerful states including the United States have in practice invoked broader doctrines (unable/unwilling, preventive self-defense), so the founding problem—how to constrain force claims—remains contested in state practice.
narrative_ontology:disappearance_verdict(article_51_self_defense__narrow_armed_attack_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__narrow_armed_attack_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__narrow_armed_attack_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_51_self_defense__narrow_armed_attack_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__narrow_armed_attack_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.62) because the constraint extracts power from individual states and concentrates it in the multilateral framework. The beneficiaries (weaker states, multilateral institutions) do not run the constraint—it runs on legal authority and state acceptance. Powerful states are the targets and must work harder to justify force claims. Suppression is high (0.71) because the constraint requires active enforcement: the Security Council must adjudicate attribution claims, international courts must rule on the definition of 'armed attack,' and powerful states must maintain rhetorical compliance even when they disagree. Theater ratio starts low (0.08 in 1945, when the constraint was new and states still saw it as binding) and peaks at 0.35 in 2001 (when the War on Terror era produced maximum rhetorical self-defense claims decoupled from actual application of the narrow standard), then falls back toward 0.28 as courts reassert the traditional reading. The constraint's extractiveness actually declines slightly post-2001 (from 0.65 to 0.62) because powerful states' theatrical invocations fail more often under legal scrutiny—the constraint adapts by rejecting claims that don't meet the standard. Accessibility collapse is high (0.78): once states understand that Article 51 requires proof of armed attack and attribution, alternatives (declaring war, seeking Council authorization, operating as non-state proxies) do not offer meaningful escape. Resistance is high (0.72) because major powers continuously push back against the narrow reading through practice, legal argumentation, and selective invocations of broader doctrines.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of weaker states and multilateral institutions, this constraint is genuine coordination: it creates a shared rule that prevents the largest powers from dominating through force alone. From the perspective of powerful states, especially those claiming security threats from non-state actors, the constraint is extractive: it forces them to justify claims they believe are self-evident and delays responses to urgent threats. The narrow reading privileges legality over strategic urgency. The engine computes these divergent directionalities from the stakeholder data: weaker states and the UN sit near the beneficiary end (d ~ 0.2), powerful states sit near the target end (d ~ 0.8). The same constraint produces opposite classifications in different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Weaker states have low directionality (d near 0.0–0.2): the constraint subsidizes them by protecting them from intervention. Multilateral institutions have low directionality (d near 0.1–0.2): the constraint reserves power for them. Powerful states have high directionality (d near 0.7–0.9): the constraint extracts their strategic freedom and forces them to work through institutional channels. Host states face a mixed position (d near 0.4–0.6): they bear the cost of attribution (if groups operate from their territory) but gain time to suppress them before being held responsible for an attack. Non-state groups are excluded (no d value). The overrides are minimal here—the structural derivation is coherent without amendment.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT exhibit mandatrophy. The founding problem—constraining unilateral force claims—remains live and the narrow reading remains the functional solution to it, despite state practice divergence. The constraint is not a zombie. However, there is a 'zombie risk' in the measurement data: the post-2001 spike in theater ratio (0.35 in 2001) indicates that powerful states invoked self-defense claims theatrically while failing to meet the narrow standard. This is not mandatrophy (the constraint did not atrophy; instead, it filtered out false claims) but it is a stress indicator. The constraint worked by rejection: claims that did not meet the standard were challenged in courts and public opinion, and the narrow reading reasserted itself. By 2026, theater has settled back to 0.28, indicating a return to functional (non-theatrical) invocation. The constraint is extractive, not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attribution_standard_ambiguity,
    'What threshold of state control or tolerance for non-state actors suffices to establish attribution for self-defense purposes? Does harboring, financing, or merely failing to suppress constitute attribution, or is active sponsorship required?',
    'International court rulings on specific cases (e.g., attributability of Hamas to Iran, ISIS to external sponsors); development of customary international law through state practice and ICJ opinions.',
    'A permissive attribution standard would expand the effective scope of Article 51 and approach the unable/unwilling doctrine. A restrictive standard (requiring active sponsorship) would narrow self-defense claims further. The constraint''s extractiveness depends on where this line is drawn.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_standard_ambiguity, conceptual, 'Where the attribution threshold lies determines the effective breadth of the narrow reading.').

omega_variable(
    imminence_definition_ambiguity,
    'What time horizon qualifies as ''imminent'' threat under Article 51? Days? Weeks? Months? Does the accumulation of minor provocations constitute an imminent armed attack, or must the attack be proximate in time and identifiable in target?',
    'ICJ case law on imminence (e.g., Caroline Incident principles); state practice in invoking preemptive self-defense; development of agreed definitions in Security Council practice.',
    'A broad definition of imminence (e.g., ''within strategic planning horizon'') would allow earlier interventions and approach preventive doctrine. A narrow definition (e.g., ''within hours'') would preserve the distinction between self-defense and preventive war. This is the operational cutting edge where the narrow reading is contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(imminence_definition_ambiguity, empirical, 'The imminence standard is where the narrow reading''s boundary is tested in practice.').

omega_variable(
    reading_vs_doctrine_boundary,
    'Is the narrow reading a rule of international law (binding and enforceable) or a doctrine (a persuasive interpretation that powerful states can override)? Does the fact that major powers routinely invoke broader doctrines undermine the narrow reading''s legal status?',
    'Examination of state compliance patterns over time; ICJ doctrine development; evolution of Security Council practice; whether override states face meaningful consequences.',
    'If the narrow reading is merely doctrinal, it lacks enforcement teeth and extractiveness should be reclassified downward (it becomes more like a piton—performatively maintained but not functionally constraining). If it is binding law, the high extractiveness and suppression values hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_doctrine_boundary, conceptual, 'Whether the constraint is law or persuasive doctrine shapes its actual force.').

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of a contested kernel. The sibling readings (expansive preventive, unable/unwilling) are held by different state coalitions and legal traditions. Is the contest a resolvable disagreement about the kernel''s true meaning, or an irreducible divergence of interests encoded in incompatible readings?',
    'Long-term observation of whether states converge on one reading through international court authority, Security Council consensus, or continued coexistence of incompatible doctrines. If convergence fails and powerful states routinely override the narrow reading, the question resolves as ''irreducible divergence.''',
    'If resolvable, the narrow reading may ultimately be adopted or superseded by clear legal development. If irreducible, the constraint functions as a negotiated compromise held together by institutional inertia, not by shared acceptance of the reading''s truth—it becomes an increasingly theatrical structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the kernel''s reading contest is an epistemic or political divergence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__narrow_armed_attack_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1945, 0.08).
narrative_ontology:measurement_basis(arti_tr_t1945, projected).
narrative_ontology:measurement(arti_tr_t1962, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1962, 0.12).
narrative_ontology:measurement_basis(arti_tr_t1962, observed).
narrative_ontology:measurement(arti_tr_t1986, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1986, 0.18).
narrative_ontology:measurement_basis(arti_tr_t1986, observed).
narrative_ontology:measurement(arti_tr_t2001, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2001, 0.35).
narrative_ontology:measurement_basis(arti_tr_t2001, observed).
narrative_ontology:measurement(arti_tr_t2015, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2015, 0.32).
narrative_ontology:measurement_basis(arti_tr_t2015, observed).
narrative_ontology:measurement(arti_tr_t2026, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2026, 0.28).
narrative_ontology:measurement_basis(arti_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1945, 0.48).
narrative_ontology:measurement_basis(arti_be_t1945, projected).
narrative_ontology:measurement(arti_be_t1962, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1962, 0.52).
narrative_ontology:measurement_basis(arti_be_t1962, observed).
narrative_ontology:measurement(arti_be_t1986, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1986, 0.58).
narrative_ontology:measurement_basis(arti_be_t1986, observed).
narrative_ontology:measurement(arti_be_t2001, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2001, 0.65).
narrative_ontology:measurement_basis(arti_be_t2001, observed).
narrative_ontology:measurement(arti_be_t2015, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement_basis(arti_be_t2015, observed).
narrative_ontology:measurement(arti_be_t2026, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2026, 0.62).
narrative_ontology:measurement_basis(arti_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1945, 0.45).
narrative_ontology:measurement_basis(arti_su_t1945, projected).
narrative_ontology:measurement(arti_su_t1962, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1962, 0.52).
narrative_ontology:measurement_basis(arti_su_t1962, observed).
narrative_ontology:measurement(arti_su_t1986, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1986, 0.62).
narrative_ontology:measurement_basis(arti_su_t1986, observed).
narrative_ontology:measurement(arti_su_t2001, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2001, 0.78).
narrative_ontology:measurement_basis(arti_su_t2001, observed).
narrative_ontology:measurement(arti_su_t2015, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2015, 0.74).
narrative_ontology:measurement_basis(arti_su_t2015, observed).
narrative_ontology:measurement(arti_su_t2026, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(arti_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__narrow_armed_attack_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_51_self_defense__narrow_armed_attack_reading, 0.12).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense__expansive_preventive_reading).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense__unable_unwilling_doctrine_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Article 51 self-defense kernel. The narrow armed attack reading constrains unilateral force; the expansive preventive reading expands it to potential threats; the unable/unwilling doctrine reading bridges the two by allowing response to non-state attacks from permissive states. Each reading has its own constraint story with distinct ε values, beneficiary/victim structures, and measurement series. They are linked as a family via the network.affects_constraints edges. The narrow reading influences both siblings by raising the evidentiary bar and preserving the attribution requirement; it forecloses the preventive reading's core premise (actual vs. potential attack) but coexists with the unable/unwilling doctrine as a refinement of attribution rules.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_51_self_defense__narrow_armed_attack_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
