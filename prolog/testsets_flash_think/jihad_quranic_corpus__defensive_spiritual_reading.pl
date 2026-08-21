% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__defensive_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__defensive_spiritual_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: jihad_quranic_corpus__defensive_spiritual_reading
 *   human_readable: Jihad as Internal Spiritual Struggle and Defensive War
 *   domain: islamic_jurisprudence/political_theology
 *
 * SUMMARY:
 *   This constraint instantiates the 'defensive_spiritual_reading' of the
 *   'jihad_quranic_corpus' kernel. It emphasizes jihad as primarily an
 *   internal spiritual struggle (jihad al-nafs) for self-purification and
 *   moral improvement, alongside a strictly defensive armed response to
 *   aggression (jihad al-qital) that is constrained by proportionality,
 *   non-combatant immunity, and legitimate state authority. This reading
 *   actively refutes interpretations that advocate for offensive warfare,
 *   individual declaration of war, or indiscriminate violence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__defensive_spiritual_reading, 0.25).
domain_priors:suppression_score(jihad_quranic_corpus__defensive_spiritual_reading, 0.4).
domain_priors:theater_ratio(jihad_quranic_corpus__defensive_spiritual_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__defensive_spiritual_reading, rope).
narrative_ontology:human_readable(jihad_quranic_corpus__defensive_spiritual_reading, "Jihad as Internal Spiritual Struggle and Defensive War").
narrative_ontology:topic_domain(jihad_quranic_corpus__defensive_spiritual_reading, "islamic_jurisprudence/political_theology").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__defensive_spiritual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__defensive_spiritual_reading, '68d39359-6c1e-488c-8780-b92d46160766').
narrative_ontology:cs_kernel_codification('68d39359-6c1e-488c-8780-b92d46160766', fixed_text).
narrative_ontology:cs_authority_grounding('68d39359-6c1e-488c-8780-b92d46160766', lineage).
narrative_ontology:cs_interpretation_layer_present('68d39359-6c1e-488c-8780-b92d46160766').
narrative_ontology:cs_reading_relation('68d39359-6c1e-488c-8780-b92d46160766', jihad_quranic_corpus__expansionist_legalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('68d39359-6c1e-488c-8780-b92d46160766', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('68d39359-6c1e-488c-8780-b92d46160766', foundational, jihad_primarily_spiritual_self_purification).
narrative_ontology:cs_axiom_status(jihad_primarily_spiritual_self_purification, holdable).
narrative_ontology:cs_axiom_grounding('68d39359-6c1e-488c-8780-b92d46160766', jihad_primarily_spiritual_self_purification, deontological).
narrative_ontology:cs_axiom('68d39359-6c1e-488c-8780-b92d46160766', foundational, armed_jihad_requires_state_authority_proportionality).
narrative_ontology:cs_axiom_status(armed_jihad_requires_state_authority_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('68d39359-6c1e-488c-8780-b92d46160766', armed_jihad_requires_state_authority_proportionality, conventional).
narrative_ontology:cs_reference_frame('68d39359-6c1e-488c-8780-b92d46160766', early_islamic_community_ethics).
narrative_ontology:cs_drift_state('68d39359-6c1e-488c-8780-b92d46160766', contemporary_islamic_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('68d39359-6c1e-488c-8780-b92d46160766', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, muslim_community).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, non_combatants).
narrative_ontology:constraint_victim(jihad_quranic_corpus__defensive_spiritual_reading, aggressor_forces).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, islamic_ethics_of_war).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, spiritual_self_purification).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, coexistence_with_non_muslims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the internal discipline of jihad al-nafs, leading to spiritual growth and moral rectitude. Also benefits from collective defense against aggression, ensuring security and preservation of faith. Bears the burden of self-discipline and, when necessary, defensive struggle.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, muslim_community, beneficiary,
    organized, generational, identity_locked, global).

% Interpret and transmit the Quranic corpus, emphasizing the spiritual and defensive aspects of jihad. They define the conditions for legitimate armed response, including proportionality and non-combatant immunity. Their authority is crucial for maintaining this reading.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, islamic_scholars_jurists, agenda_setter,
    institutional, generational, constrained, global).

% Are the sole legitimate authority for declaring and conducting armed defensive jihad, ensuring it adheres to jurisprudential constraints. They are responsible for protecting the Muslim community and upholding the ethical framework of war.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, legitimate_state_authorities, agenda_setter,
    institutional, biographical, constrained, national).

% Bear the costs of being resisted when they initiate aggression against Muslim lands or communities. They are the target of the defensive armed response, which aims to repel their aggression, not to conquer or convert them.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, aggressor_forces, payer,
    powerful, immediate, mobile, regional).

% Are explicitly protected by the rules of non-combatant immunity, regardless of their faith. They benefit from the ethical constraints on warfare, which aim to minimize harm to civilians, infrastructure, and religious sites.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, non_combatants, beneficiary,
    powerless, immediate, trapped, local).

% Are structurally excluded from the legitimate discourse of this reading. Their interpretations, which often advocate for offensive jihad, individual declaration of war, or targeting of non-combatants, are actively refuted by scholars upholding the defensive/spiritual reading.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, radical_groups_misinterpreting_jihad, excluded,
    organized, biographical, identity_locked, global).

% Analyze and compare this reading of jihad with other religious and secular ethics of war. They observe its internal coherence, its historical application, and its contemporary influence, without being subject to its direct enforcement.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, comparative_theologians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__defensive_spiritual_reading, diffuse).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__defensive_spiritual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates individual spiritual discipline (jihad al-nafs) for moral development and provides a framework for collective defensive action against external aggression, ensuring ethical conduct in warfare and protecting non-combatants.
% TRANSFER_FUNCTION: Transfers individual effort and self-restraint from the believer to their spiritual development; transfers defensive force from the Muslim community to aggressor forces, aiming to repel aggression and restore peace.
% ABSENT_VOICES: Radical groups advocating for offensive, non-state-sanctioned, or indiscriminate forms of jihad are excluded. They would argue for a broader, more aggressive interpretation, but their views are actively rejected by proponents of this reading.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the Muslim community would lose a core ethical framework for both personal conduct and collective defense. It would likely lead to either passive vulnerability or unconstrained, unethical warfare, fundamentally altering the moral landscape of Islamic societies.
% FOUNDING_PROBLEM: The early Muslim community faced both internal moral challenges and external aggression, requiring a framework for ethical self-conduct and legitimate defense that distinguished between righteous struggle and unjust violence.
% FOUNDING_PROBLEM_CORROBORATION: Islamic scholars and ethicists widely corroborate that the problems of internal moral struggle and external aggression remain live, requiring continuous application of this ethical framework. International legal scholars also recognize the historical development of Islamic just war theory, supporting the defensive and ethical dimensions.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__defensive_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__defensive_spiritual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__defensive_spiritual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jihad_quranic_corpus__defensive_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__defensive_spiritual_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).
:- end_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the primary goal is spiritual growth and legitimate defense, not conquest or material gain. Any 'extraction' is from aggressors, not from the Muslim community itself. Suppression is moderate (0.4) as it requires active enforcement of ethical rules and suppression of alternative, more aggressive interpretations. Theater ratio is low (0.15) because both the internal struggle and the defensive armed response are understood as genuine, functional obligations, not mere performance. Accessibility collapse is moderate (0.5) as the 'true' meaning requires significant scholarly effort to grasp and apply, and misinterpretations are common. Resistance is moderate (0.4) from those who prefer more expansive or revolutionary interpretations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Muslim community, this reading provides a coherent and ethical framework for living and defending themselves. From the perspective of radical groups, this reading is seen as a betrayal or weakening of Islamic obligations. The engine's classification will highlight how the same textual kernel can yield vastly different structural outcomes depending on the interpretive lens.
 *
 * DIRECTIONALITY LOGIC:
 *   The Muslim community and non-combatants are beneficiaries, gaining spiritual peace and protection. Islamic scholars and state authorities act as agenda-setters, defining and enforcing the ethical boundaries. Aggressor forces are the payers, bearing the cost of being resisted. Radical groups are excluded, as their interpretations are deemed illegitimate by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling genuine spiritual and defensive coordination as pure extraction. By emphasizing the internal and constrained defensive aspects, it highlights the coordination function of maintaining ethical boundaries and collective security, rather than portraying it as an inherently extractive or aggressive doctrine. The low extractiveness and moderate suppression reflect this balance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretation_vs_practice_gap,
    'To what extent does the actual practice of Muslim communities and states align with this defensive/spiritual reading, versus being influenced by more aggressive interpretations?',
    'Empirical study of historical and contemporary conflicts, analysis of state policies, and surveys of public opinion within Muslim communities regarding the justification and conduct of warfare.',
    'If practice frequently deviates towards more aggressive interpretations, the effective extractiveness and suppression of ''jihad'' as a concept would be higher than this reading suggests, indicating a gap between normative ideal and lived reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretation_vs_practice_gap, empirical, 'Gap between the normative ideal of this reading and its real-world application.').

omega_variable(
    threshold_of_aggression_ambiguity,
    'What constitutes ''aggression'' sufficient to trigger a legitimate defensive armed response, and who authoritatively determines this threshold?',
    'Comparative legal analysis of Islamic jurisprudence across different schools of thought and historical contexts, focusing on the criteria for ''just cause'' in defensive war. Clarification by a universally recognized Islamic legal body.',
    'A low or ambiguous threshold for ''aggression'' could allow for interpretations that justify pre-emptive or retaliatory actions beyond strict defense, increasing the potential for extraction and suppression. A high, clearly defined threshold reinforces the defensive nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_of_aggression_ambiguity, conceptual, 'Ambiguity in defining the trigger for defensive armed jihad.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine, distinct reading of the Quranic corpus, or a modern re-interpretation driven by contemporary political needs?',
    'Historical-critical analysis of classical Islamic texts and jurisprudential debates to trace the lineage and evolution of this specific interpretation of jihad, distinguishing it from later apologetic or politically motivated framings.',
    'If primarily a modern re-interpretation, its authority grounding might shift from ''lineage'' to ''practice'' or ''expertise'' (modern scholars), potentially altering its stability and influence within the broader commitment system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Nature of this reading''s historical and theological grounding.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__defensive_spiritual_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jiha_tr_t350, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 350, 0.12).
narrative_ontology:measurement(jiha_tr_t700, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 700, 0.15).
narrative_ontology:measurement(jiha_tr_t1050, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 1050, 0.13).
narrative_ontology:measurement(jiha_tr_t1400, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 1400, 0.15).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(jiha_be_t350, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 350, 0.22).
narrative_ontology:measurement(jiha_be_t700, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 700, 0.25).
narrative_ontology:measurement(jiha_be_t1050, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 1050, 0.23).
narrative_ontology:measurement(jiha_be_t1400, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 1400, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(jiha_su_t350, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 350, 0.38).
narrative_ontology:measurement(jiha_su_t700, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 700, 0.4).
narrative_ontology:measurement(jiha_su_t1050, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 1050, 0.37).
narrative_ontology:measurement(jiha_su_t1400, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 1400, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__defensive_spiritual_reading, identity_coordination).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, islamic_ethics_of_war).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, islamic_law_of_nations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
