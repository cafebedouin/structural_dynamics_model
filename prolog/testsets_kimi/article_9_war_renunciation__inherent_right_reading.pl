% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__inherent_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__inherent_right_reading, []).

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
 *   constraint_id: article_9_war_renunciation__inherent_right_reading
 *   human_readable: Article 9 Inherent Right of Self-Defense Reading (Minimum Necessary Defense)
 *   domain: constitutional law/security policy/institutional legitimacy
 *
 * SUMMARY:
 *   Article 9 of the Japanese Constitution renounces war and the maintenance
 *   of armed forces. The inherent right reading interprets this as
 *   prohibiting aggressive war while preserving the sovereign right to
 *   minimum necessary self-defense, legitimizing the Self-Defense Forces
 *   without formal constitutional amendment. This constraint story models
 *   that reading as a constitutional-interpretive arrangement that
 *   coordinates security policy while asymmetrically overriding both pacifist
 *   textualism and hawkish normalization ambitions. It is one reading of the
 *   Article 9 kernel; sibling readings include strict pacifism and collective
 *   self-defense extension.
 *
 * KEY AGENTS:
 *   - Cabinet and Cabinet Legislation Bureau (agenda_setter/beneficiary) â interprets and enforces the reading
 *   - Self-Defense Forces personnel (beneficiary) â gain legitimacy under scope limits
 *   - Pacifist civic groups (payer) â bear cost of overridden constitutional vision
 *   - Constitutional revisionists (payer) â constrained by minimum necessary threshold
 *   - Constitutional scholars (observer) â provide analytical contestation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__inherent_right_reading, 0.62).
domain_priors:suppression_score(article_9_war_renunciation__inherent_right_reading, 0.73).
domain_priors:theater_ratio(article_9_war_renunciation__inherent_right_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, suppression_requirement, 0.73).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__inherent_right_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__inherent_right_reading, "Article 9 Inherent Right of Self-Defense Reading (Minimum Necessary Defense)").
narrative_ontology:topic_domain(article_9_war_renunciation__inherent_right_reading, "constitutional law/security policy/institutional legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__inherent_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__inherent_right_reading, 'e8896931-41f7-45a1-9c09-28e9ffd13e21').
narrative_ontology:cs_kernel_codification('e8896931-41f7-45a1-9c09-28e9ffd13e21', fixed_text).
narrative_ontology:cs_authority_grounding('e8896931-41f7-45a1-9c09-28e9ffd13e21', lineage).
narrative_ontology:cs_interpretation_layer_present('e8896931-41f7-45a1-9c09-28e9ffd13e21').
narrative_ontology:cs_reading_relation('e8896931-41f7-45a1-9c09-28e9ffd13e21', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('e8896931-41f7-45a1-9c09-28e9ffd13e21', article_9_war_renunciation__collective_self_defense_reading, influences).
narrative_ontology:cs_axiom('e8896931-41f7-45a1-9c09-28e9ffd13e21', foundational, inherent_right_self_defense_survives_renunciation).
narrative_ontology:cs_axiom_status(inherent_right_self_defense_survives_renunciation, holdable).
narrative_ontology:cs_axiom_grounding('e8896931-41f7-45a1-9c09-28e9ffd13e21', inherent_right_self_defense_survives_renunciation, conventional).
narrative_ontology:cs_axiom('e8896931-41f7-45a1-9c09-28e9ffd13e21', foundational, minimum_necessary_proportionality_limits_force).
narrative_ontology:cs_axiom_status(minimum_necessary_proportionality_limits_force, holdable).
narrative_ontology:cs_axiom_grounding('e8896931-41f7-45a1-9c09-28e9ffd13e21', minimum_necessary_proportionality_limits_force, conventional).
narrative_ontology:cs_reference_frame('e8896931-41f7-45a1-9c09-28e9ffd13e21', minimum_necessary_defense_framework).
narrative_ontology:cs_drift_state('e8896931-41f7-45a1-9c09-28e9ffd13e21', contemporary_security_policy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e8896931-41f7-45a1-9c09-28e9ffd13e21', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, cabinet_government).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, sdf_personnel).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, pacifist_civic_groups).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, constitutional_revisionists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 9 through the Cabinet Legislation Bureau to permit the Self-Defense Forces while maintaining the constitutional text unchanged. Gains policy flexibility and avoids the political cost of formal amendment while retaining control over security doctrine. Exit would require constitutional revision or abandonment of the SDF, both politically costly.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, cabinet_government, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__inherent_right_reading, cabinet_government, beneficiary).

% Gain organizational legitimacy and budgetary existence under a constitutional framework that permits minimum necessary defense. Professional identity and career paths depend on the reading's stability. Scope limitations constrain permissible missions and equipment doctrine.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, sdf_personnel, beneficiary,
    organized, biographical, constrained, national).

% Bear the cost of a constitutional text they view as categorically prohibiting military forces being interpreted to permit them. Political and legal efforts to enforce strict textual pacifism are overridden by decades of executive and judicial interpretive practice. Exit from this constraint would require constitutional amendment or a fundamental shift in government doctrine.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, pacifist_civic_groups, payer,
    moderate, generational, constrained, national).

% Seek full normalization of Japan's military status, including explicit constitutional revision and removal of scope limitations on collective defense and force projection. The minimum necessary threshold blocks their policy preferences. They are constrained by the political difficulty of overturning a decades-old interpretive framework.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, constitutional_revisionists, payer,
    moderate, biographical, constrained, national).

% Analyze and debate the textual legitimacy of the inherent right reading. Some support it as legally necessary under international law; others view it as interpretive overreach that revises the constitution without formal amendment. They produce the analytical frameworks that inform political contestation but do not directly set the agenda.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, constitutional_scholars, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__inherent_right_reading, cabinet_government).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__inherent_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permits a self-defense capacity under a constitutional text that renounces war, avoiding formal amendment while stabilizing Japan's security institutions across decades of Cold War and post-Cold War transitions.
% TRANSFER_FUNCTION: Transfers interpretive authority over constitutional meaning from textual literalists to the executive branch and Cabinet Legislation Bureau; transfers legitimacy to the Self-Defense Forces while imposing proportionality limits on their scope.
% ABSENT_VOICES: Strict pacifist constitutional scholars and civic groups who read the text as categorically prohibiting armed forces; allied defense planners seeking automatic collective defense commitments beyond territorial defense.
% DISAPPEARANCE_RATIONALE: If the reading vanished, the SDF would lack constitutional footing, requiring either immediate disbandment or urgent constitutional amendment; the government's interpretive edifice would collapse, and both pacifist and hawkish camps would demand radically different constitutional settlements.
% FOUNDING_PROBLEM: How to maintain national security and self-defense capacity after World War II under a constitutional provision that appeared to prohibit all war potential and armed forces.
% FOUNDING_PROBLEM_CORROBORATION: Government and defense officials attest the problem was live and required the inherent right reading to preserve state survival. Pacifist scholars and opposition parties attest the problem was exaggerated to permit remilitarization; external historians note the original drafting context under SCAP occupation remains disputed regarding whether the text was intended to permit defensive forces.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__inherent_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__inherent_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__inherent_right_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_9_war_renunciation__inherent_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__inherent_right_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__inherent_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__inherent_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_9_war_renunciation__inherent_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) because the reading co-opts a pacifist constitutional text to permit substantial military capacity, extracting interpretive legitimacy from a text that structurally resists it. Suppression is high (0.73) because the arrangement requires continuous active enforcement: Cabinet Legislation Bureau opinions, Diet legislation, and judicial avoidance to prevent the strict textual reading from prevailing. Theater ratio is elevated (0.60) because the 'minimum necessary' threshold has become largely performative as the SDF operates as a normalized military with advanced capabilities. Accessibility collapse (0.65) reflects the political impossibility of constitutional amendment and the marginalization of strict pacifism as a legal position. Resistance (0.55) captures ongoing pacifist opposition and scholarly contestation. The measurement series show accumulation of extraction and theater from 1954 to 2024.
 *
 * PERSPECTIVAL GAP:
 *   From the government's seat, the reading is necessary coordination preserving peace constitution symbolism while ensuring security. From pacifist seats, it is extractive override of a sacred textual prohibition. From hawkish seats, it is an artificial constraint blocking full normalization. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The cabinet and SDF are structural beneficiaries (low d) â the constraint subsidizes their policy continuity and organizational existence. Pacifist civic groups and constitutional revisionists are targets (high d) â the constraint extracts by overriding their respective constitutional visions. Scholars sit near symmetric depending on their doctrinal position. The exit options are constrained for all non-analytical agents because constitutional politics in Japan is institutionally sticky.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â maintaining defense under a war-renouncing text â may be partially dead in the sense that the SDF is now normalized and the security environment has changed. However, the reading persists because it solves a live coordination problem: avoiding the political fracture of constitutional amendment. The R5 mismatch (contested founding problem status plus world_rearranges disappearance verdict) signals that the arrangement has taken on independent institutional weight beyond its original security rationale, consistent with tangled_rope rather than scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the minimum necessary defense framework a faithful interpretation of Article 9''s textual kernel, or a pragmatic revision disguised as interpretation?',
    'Comparative constitutional history analysis of drafting intent (SCAP records, Diet debates 1946-47) versus subsequent interpretive drift.',
    'If the reading is a pragmatic revision, the constraint''s legitimacy rests on practice and political necessity rather than textual fidelity, increasing its tangled_rope character. If faithful, it is closer to a rope (genuine coordination around discovered textual meaning).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the inherent right reading is interpretation or disguised revision').

omega_variable(
    minimum_necessary_threshold,
    'What institutional mechanism determines when defensive capacity exceeds ''minimum necessary'' under this reading?',
    'Judicial review of defense budgets and force posture, or legislative standards defining proportionality.',
    'If no enforceable threshold exists, the constraint is theater â a proportionality limit without a limiting function â and the reading collapses toward a snare (extractive cover for unlimited military normalization).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimum_necessary_threshold, empirical, 'Whether the minimum necessary threshold is operationally enforceable').

omega_variable(
    collective_defense_pressure,
    'Does the inherent right reading structurally compel expansion toward collective self-defense, or can it permanently hold at territorial defense?',
    'Longitudinal analysis of Cabinet Legislation Bureau opinions and defense white papers for scope creep.',
    'If structural expansion is inevitable, the reading is a scaffold toward full military normalization rather than a stable tangled rope. If it can hold, it is a durable coordination-extraction hybrid.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_defense_pressure, empirical, 'Whether inherent right reading inevitably expands to collective defense').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__inherent_right_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_9_war_renunciation__inherent_right_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(arti_tr_t14, article_9_war_renunciation__inherent_right_reading, theater_ratio, 14, 0.28).
narrative_ontology:measurement(arti_tr_t28, article_9_war_renunciation__inherent_right_reading, theater_ratio, 28, 0.38).
narrative_ontology:measurement(arti_tr_t42, article_9_war_renunciation__inherent_right_reading, theater_ratio, 42, 0.48).
narrative_ontology:measurement(arti_tr_t56, article_9_war_renunciation__inherent_right_reading, theater_ratio, 56, 0.55).
narrative_ontology:measurement(arti_tr_t70, article_9_war_renunciation__inherent_right_reading, theater_ratio, 70, 0.6).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(arti_be_t14, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 14, 0.38).
narrative_ontology:measurement(arti_be_t28, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 28, 0.45).
narrative_ontology:measurement(arti_be_t42, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 42, 0.52).
narrative_ontology:measurement(arti_be_t56, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 56, 0.58).
narrative_ontology:measurement(arti_be_t70, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 70, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(arti_su_t14, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 14, 0.55).
narrative_ontology:measurement(arti_su_t28, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 28, 0.6).
narrative_ontology:measurement(arti_su_t42, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 42, 0.68).
narrative_ontology:measurement(arti_su_t56, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 56, 0.75).
narrative_ontology:measurement(arti_su_t70, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 70, 0.73).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__inherent_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, collective_self_defense_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'Article 9' conflates three structurally distinct readings: strict pacifist (categorical prohibition), inherent right (minimum necessary territorial defense), and collective self-defense (extension to allied defense). Each reading instantiates a different constraint with different epsilon, stakeholders, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
