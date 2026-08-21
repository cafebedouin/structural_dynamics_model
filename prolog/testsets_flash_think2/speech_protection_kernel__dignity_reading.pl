% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__dignity_reading, []).

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
 *   constraint_id: speech_protection_kernel__dignity_reading
 *   human_readable: Speech Protection Conditional on Dignity (Dignity Reading)
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint represents the 'dignity reading' of the speech protection
 *   kernel, which posits that speech protection is conditional on it not
 *   functioning as structural subordination of target groups. This reading
 *   attempts to rebalance free speech jurisprudence by recognizing the
 *   material effects of speech on marginalized communities. However, in
 *   practice, the enforcement of this condition is often weak and contested,
 *   leading to a tension between the normative ideal and the actual outcomes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, 0.7).
domain_priors:suppression_score(speech_protection_kernel__dignity_reading, 0.8).
domain_priors:theater_ratio(speech_protection_kernel__dignity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__dignity_reading, "Speech Protection Conditional on Dignity (Dignity Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__dignity_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__dignity_reading, '2d9cd7f7-b43d-4f5f-81fd-ef7000e0385e').
narrative_ontology:cs_kernel_codification('2d9cd7f7-b43d-4f5f-81fd-ef7000e0385e', fixed_text).
narrative_ontology:cs_authority_grounding('2d9cd7f7-b43d-4f5f-81fd-ef7000e0385e', lineage).
narrative_ontology:cs_interpretation_layer_present('2d9cd7f7-b43d-4f5f-81fd-ef7000e0385e').
narrative_ontology:cs_reading_relation('2d9cd7f7-b43d-4f5f-81fd-ef7000e0385e', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('2d9cd7f7-b43d-4f5f-81fd-ef7000e0385e', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_reading_relation('2d9cd7f7-b43d-4f5f-81fd-ef7000e0385e', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('2d9cd7f7-b43d-4f5f-81fd-ef7000e0385e', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('2d9cd7f7-b43d-4f5f-81fd-ef7000e0385e', foundational, speech_is_action_with_material_effects).
narrative_ontology:cs_axiom_status(speech_is_action_with_material_effects, holdable).
narrative_ontology:cs_axiom_grounding('2d9cd7f7-b43d-4f5f-81fd-ef7000e0385e', speech_is_action_with_material_effects, empirically_contingent).
narrative_ontology:cs_axiom('2d9cd7f7-b43d-4f5f-81fd-ef7000e0385e', foundational, equal_dignity_is_precondition_for_free_speech).
narrative_ontology:cs_axiom_status(equal_dignity_is_precondition_for_free_speech, holdable).
narrative_ontology:cs_axiom_grounding('2d9cd7f7-b43d-4f5f-81fd-ef7000e0385e', equal_dignity_is_precondition_for_free_speech, deontological).
narrative_ontology:cs_reference_frame('2d9cd7f7-b43d-4f5f-81fd-ef7000e0385e', equal_dignity_public_sphere).
narrative_ontology:cs_drift_state('2d9cd7f7-b43d-4f5f-81fd-ef7000e0385e', contemporary_legal_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2d9cd7f7-b43d-4f5f-81fd-ef7000e0385e', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__dignity_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, speakers_of_non_subordinating_speech).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, free_speech_advocates).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, target_groups_of_subordinating_speech).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, speakers_of_subordinating_speech).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the brunt of speech that contributes to their structural subordination, often without effective legal recourse, despite the stated condition. Their ability to participate equally in public life is diminished.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, target_groups_of_subordinating_speech, payer,
    powerless, biographical, constrained, national).

% Often have their speech protected due to weak enforcement of the dignity condition, allowing them to contribute to structural subordination without significant legal consequence. They benefit from a broad interpretation of speech rights.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, speakers_of_subordinating_speech, beneficiary,
    powerful, immediate, mobile, national).

% Benefit from general speech protection without engaging in subordinating speech, aligning with the broader goals of free expression and a robust public sphere.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, speakers_of_non_subordinating_speech, beneficiary,
    moderate, biographical, mobile, national).

% Interpret and apply speech protection laws, often struggling to balance broad free speech principles with the dignity condition, leading to inconsistent enforcement and a contested legal landscape.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, courts_and_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% Advocate for broad interpretations of speech protection, sometimes resisting restrictions even when they aim to prevent structural subordination, viewing any restriction as a threat to fundamental rights.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, free_speech_advocates, beneficiary,
    organized, generational, constrained, national).

% Monitor and critique the effectiveness of speech protection in upholding human dignity and preventing structural subordination, often advocating for stronger enforcement of the condition and a rebalancing of rights.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__dignity_reading, speakers_of_subordinating_speech).
narrative_ontology:fixing_cost_class(speech_protection_kernel__dignity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate public discourse by establishing boundaries that prevent speech from actively undermining the equal dignity and social participation of marginalized groups, while generally protecting other forms of expression to foster a robust public sphere.
% TRANSFER_FUNCTION: In theory, transfers the burden of tolerating structurally subordinating speech from target groups to the speakers of such speech. In practice, due to enforcement challenges and prevailing legal interpretations, it often transfers the burden of enduring subordination from speakers to target groups.
% ABSENT_VOICES: Historically, the voices of marginalized communities experiencing systemic speech-related harm were largely absent from the foundational debates shaping free speech jurisprudence, leading to doctrines that often failed to account for power imbalances and the unique harms of subordinating speech.
% DISAPPEARANCE_RATIONALE: If the condition of not functioning as structural subordination vanished, speech that actively contributes to the marginalization and disempowerment of target groups would likely proliferate without legal consequence. This would severely impact social cohesion, equality, and the ability of these groups to participate fully in public life, leading to a more hostile and fragmented public sphere.
% FOUNDING_PROBLEM: The historical failure of traditional free speech doctrines to adequately address the systemic harms of speech that contributes to the structural subordination of marginalized groups, often treating all speech as equally impactful regardless of power dynamics and historical context.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars in critical race theory, feminist legal theory, and human rights law, as well as numerous civil society organizations representing marginalized communities, provide extensive corroboration through empirical studies, legal analyses, and lived experience narratives, documenting the ongoing harms and the inadequacy of existing legal frameworks.
narrative_ontology:disappearance_verdict(speech_protection_kernel__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__dignity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__dignity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(speech_protection_kernel__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__dignity_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__dignity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.7) reflects the ongoing burden placed on target groups due to the frequent failure to effectively enforce the dignity condition, allowing subordinating speech to persist. Suppression (0.8) is high because it requires constant legal and social struggle to even attempt to restrict such speech against powerful counter-arguments for broad protection. The moderate theater ratio (0.45) indicates that while there are genuine efforts to address speech-related harms, much of the legal and public discourse around this condition can be performative, failing to translate into consistent, effective protection for target groups. Resistance (0.75) is high from those who claim their speech is being unjustly restricted, often invoking traditional free speech principles.
 *
 * PERSPECTIVAL GAP:
 *   There is a significant perspectival gap between the normative ideal of the dignity reading and its practical implementation. From the perspective of target groups and human rights advocates, the constraint often fails to deliver its promised protection, functioning as a source of ongoing harm. From the perspective of many speakers and free speech advocates, the constraint is seen as an overreach that threatens fundamental liberties, even when its intent is to prevent subordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers of non-subordinating speech and free speech advocates are beneficiaries, as their broader interest in free expression is generally upheld. However, speakers of subordinating speech also often benefit from the practical inability to consistently enforce the dignity condition. Target groups of subordinating speech are victims, as they continue to experience the harms of speech that contributes to their structural subordination, despite the normative intent of this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_subordination_definition_ambiguity,
    'How is ''structural subordination'' precisely defined and empirically measured in a legal context, and to what extent do different interpretations of this concept lead to varying enforcement outcomes?',
    'Development of clear, judicially recognized criteria and empirical indicators for structural subordination, coupled with consistent application across jurisdictions.',
    'A clear and consistent definition would reduce the ambiguity that currently allows subordinating speech to be protected, potentially increasing the constraint''s effective suppression and reducing extraction from target groups. Conversely, an overly broad or vague definition could lead to over-restriction of speech.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_subordination_definition_ambiguity, conceptual, 'Ambiguity in defining and measuring structural subordination in legal practice.').

omega_variable(
    enforcement_efficacy_gap,
    'To what extent is the ''dignity reading'' genuinely enforceable within existing legal frameworks, or does its implementation primarily serve as a rhetorical ideal rather than a consistently applied legal standard?',
    'Comparative legal analysis of jurisdictions attempting to implement similar conditions, assessing the frequency of successful prosecutions or restrictions of subordinating speech versus the prevalence of such speech in public discourse.',
    'If found to be largely rhetorical, the constraint''s effective extractiveness from target groups would be higher than currently measured, and its classification might shift closer to a Snare for them. If found to be genuinely enforceable, its extractiveness from speakers of subordinating speech would be higher, and its coordination function for a dignified public sphere would be more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_efficacy_gap, empirical, 'Gap between the normative ideal of the dignity reading and its practical enforceability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__dignity_reading, 1960, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1960, speech_protection_kernel__dignity_reading, theater_ratio, 1960, 0.3).
narrative_ontology:measurement(spee_tr_t1975, speech_protection_kernel__dignity_reading, theater_ratio, 1975, 0.35).
narrative_ontology:measurement(spee_tr_t1990, speech_protection_kernel__dignity_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(spee_tr_t2005, speech_protection_kernel__dignity_reading, theater_ratio, 2005, 0.43).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_kernel__dignity_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(spee_be_t1960, speech_protection_kernel__dignity_reading, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement(spee_be_t1975, speech_protection_kernel__dignity_reading, base_extractiveness, 1975, 0.6).
narrative_ontology:measurement(spee_be_t1990, speech_protection_kernel__dignity_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(spee_be_t2005, speech_protection_kernel__dignity_reading, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement(spee_be_t2024, speech_protection_kernel__dignity_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1960, speech_protection_kernel__dignity_reading, suppression_requirement, 1960, 0.6).
narrative_ontology:measurement(spee_su_t1975, speech_protection_kernel__dignity_reading, suppression_requirement, 1975, 0.68).
narrative_ontology:measurement(spee_su_t1990, speech_protection_kernel__dignity_reading, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(spee_su_t2005, speech_protection_kernel__dignity_reading, suppression_requirement, 2005, 0.78).
narrative_ontology:measurement(spee_su_t2024, speech_protection_kernel__dignity_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__dignity_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
