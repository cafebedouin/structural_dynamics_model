% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_preparation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_preparation, []).

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
 *   constraint_id: kodashim_obligation__study_as_preparation
 *   human_readable: Kodashim Obligation: Study as Messianic Preparation
 *   domain: religious_studies/jewish_law/textual_preservation
 *
 * SUMMARY:
 *   This constraint describes the religious obligation within Jewish law to
 *   study the Kodashim (sacrificial) order of the Mishnah and Talmud, despite
 *   the destruction of the Temple rendering actual performance impossible.
 *   The study is understood not as a mere academic exercise, but as a binding
 *   religious duty that actively preserves the technical knowledge and
 *   spiritual continuity necessary for the eventual messianic restoration of
 *   the Temple service. This reading emphasizes the instrumental role of
 *   study as preparation for a future state.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_preparation, 0.15).
domain_priors:suppression_score(kodashim_obligation__study_as_preparation, 0.2).
domain_priors:theater_ratio(kodashim_obligation__study_as_preparation, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, extractiveness, 0.15).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_preparation, rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_preparation, "Kodashim Obligation: Study as Messianic Preparation").
narrative_ontology:topic_domain(kodashim_obligation__study_as_preparation, "religious_studies/jewish_law/textual_preservation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_preparation, 'ffc361ab-46c7-4114-a357-2c3c9e75e866').
narrative_ontology:cs_kernel_codification('ffc361ab-46c7-4114-a357-2c3c9e75e866', fixed_text).
narrative_ontology:cs_authority_grounding('ffc361ab-46c7-4114-a357-2c3c9e75e866', lineage).
narrative_ontology:cs_interpretation_layer_present('ffc361ab-46c7-4114-a357-2c3c9e75e866').
narrative_ontology:cs_reading_relation('ffc361ab-46c7-4114-a357-2c3c9e75e866', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('ffc361ab-46c7-4114-a357-2c3c9e75e866', kodashim_obligation__study_as_archive, coexists_with).
narrative_ontology:cs_axiom('ffc361ab-46c7-4114-a357-2c3c9e75e866', foundational, sacrificial_law_binding_in_exile).
narrative_ontology:cs_axiom_status(sacrificial_law_binding_in_exile, holdable).
narrative_ontology:cs_axiom_grounding('ffc361ab-46c7-4114-a357-2c3c9e75e866', sacrificial_law_binding_in_exile, deontological).
narrative_ontology:cs_axiom('ffc361ab-46c7-4114-a357-2c3c9e75e866', foundational, knowledge_preservation_for_restoration).
narrative_ontology:cs_axiom_status(knowledge_preservation_for_restoration, holdable).
narrative_ontology:cs_axiom_grounding('ffc361ab-46c7-4114-a357-2c3c9e75e866', knowledge_preservation_for_restoration, instrumental).
narrative_ontology:cs_reference_frame('ffc361ab-46c7-4114-a357-2c3c9e75e866', post_temple_destruction_halakha).
narrative_ontology:cs_drift_state('ffc361ab-46c7-4114-a357-2c3c9e75e866', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ffc361ab-46c7-4114-a357-2c3c9e75e866', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_preparation, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, messianic_future_generation).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, rabbinic_scholars).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, current_generation_of_jews).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_preparation, current_generation_of_jews).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, divine_covenant_continuity).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, oral_torah_authority).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, messianic_redemption_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% They interpret and transmit the sacrificial laws, guiding the community in their study. They gain spiritual merit, communal respect, and ensure the continuity of tradition. Their role is to maintain the technical knowledge for future restoration.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, rabbinic_scholars, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_preparation, rabbinic_scholars, beneficiary).

% They bear the burden of diligent study and the deferred cosmic repair (the inability to perform sacrifices now). They benefit from maintaining a spiritual connection to the divine covenant and a shared communal identity, and contribute to the future restoration.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, current_generation_of_jews, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_preparation, current_generation_of_jews, beneficiary).

% This future generation is the ultimate beneficiary, receiving the preserved technical knowledge and the spiritual continuity necessary to resume sacrificial service upon the Temple's restoration. They are not yet present to act.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, messianic_future_generation, beneficiary,
    powerless, civilizational, analytical, universal).

% The ultimate recipient of the sacrificial service and the source of the divine command. Observes the adherence to the obligation and the efforts of preservation.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, divine_presence, observer,
    analytical, civilizational, analytical, universal).

% They study Kodashim as historical texts and cultural artifacts, but typically do not acknowledge its binding legal or cosmic efficacy. Their perspective is excluded from the internal religious discourse on obligation and preparation.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, secular_historians, excluded,
    moderate, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_obligation__study_as_preparation, diffuse).
narrative_ontology:fixing_cost_class(kodashim_obligation__study_as_preparation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To collectively preserve the intricate technical knowledge of sacrificial laws across generations, ensuring its availability and understanding for a future messianic restoration when performance can resume.
% TRANSFER_FUNCTION: Transfers spiritual merit, communal identity, and detailed ritual knowledge from the current generation (through study and transmission) to the messianic future generation, maintaining the continuity of the divine covenant.
% ABSENT_VOICES: Secular historians or purely academic scholars, who would frame the study of Kodashim as historical or cultural preservation rather than a binding religious obligation with cosmic implications. They are excluded by the theological framing of the constraint.
% DISAPPEARANCE_RATIONALE: If the obligation to study Kodashim vanished, the technical knowledge for sacrificial service would likely be lost or fragmented over generations. This would render the messianic restoration of the Temple service impossible, fundamentally altering a core tenet of Jewish eschatology and communal identity.
% FOUNDING_PROBLEM: How to maintain the binding nature, technical details, and spiritual relevance of sacrificial law after the destruction of the Second Temple, when physical performance became impossible, without allowing the tradition to atrophy or be forgotten.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing existence of the destroyed Temple and the continued anticipation of messianic redemption, attested by centuries of rabbinic literature, communal prayer, and continuous study, corroborate that the founding problem remains live. This is affirmed by religious authorities and widely accepted within the observant Jewish community.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_preparation, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_preparation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_preparation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(kodashim_obligation__study_as_preparation, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_preparation, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_preparation_tests).
:- end_tests(kodashim_obligation__study_as_preparation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the 'cost' to the current generation (effort of study, deferred cosmic repair) is understood as a necessary contribution to a collective, future good, rather than a coercive extraction. Suppression is low (0.20) as adherence is primarily voluntary, driven by religious conviction and communal norms, not active enforcement. Theater ratio is low (0.10) because the function of knowledge preservation is genuinely active and purposeful, not merely performative. Accessibility collapse is moderate (0.40) as alternative forms of engagement with Jewish tradition exist, but none fully substitute for this specific obligation. Resistance is low (0.10) as the obligation is widely accepted within observant communities.
 *
 * PERSPECTIVAL GAP:
 *   While rabbinic scholars and the current generation largely share the understanding of study as preparation, the 'messianic_future_generation' (as an abstract beneficiary) experiences only the benefit, not the cost. The 'secular_historians' would view the entire enterprise differently, focusing on historical context rather than religious obligation, leading to a fundamental divergence in perceived function and value.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'messianic_future_generation' is the primary beneficiary, receiving the preserved knowledge. 'Rabbinic_scholars' are beneficiaries through their role in guiding and transmitting this knowledge, gaining spiritual merit and communal authority. The 'current_generation_of_jews' acts as both a beneficiary (maintaining spiritual connection, communal identity) and a 'payer' or 'victim' (bearing the effort of study and the burden of deferred cosmic repair). The 'divine_presence' is an observer, and 'secular_historians' are excluded from the internal logic of the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_of_knowledge_preservation,
    'How effectively does textual study alone preserve the practical, technical knowledge required for actual sacrificial performance, given the centuries of non-practice?',
    'Empirical analysis of historical gaps in ritual transmission, or a counterfactual scenario of immediate Temple restoration: would the preserved knowledge be sufficient for immediate, correct performance?',
    'If preservation is found to be highly effective, it strengthens the ''preparation'' framing. If found to be insufficient, it weakens the instrumental justification, potentially shifting the constraint towards ''study_as_archive'' or ''study_as_performance'' (where practical efficacy is less central).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_knowledge_preservation, empirical, 'Assesses the practical utility of study for future ritual performance.').

omega_variable(
    preparation_vs_performance_ambiguity,
    'Is the primary function of study truly ''preparation'' for a future event, or does it also carry an implicit ''performance'' aspect, where the act of study itself is seen as a substitute for sacrifice?',
    'Theological and halakhic analysis of rabbinic texts and commentaries that discuss the spiritual efficacy of study in the absence of the Temple. If texts strongly emphasize study''s intrinsic merit as a substitute, it suggests a ''study_as_performance'' component.',
    'If a significant ''performance'' aspect is identified, the constraint''s extractiveness might be re-evaluated (as the ''cost'' of deferred cosmic repair is partially offset by present spiritual benefit), and its classification might shift towards a hybrid with ''study_as_performance''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preparation_vs_performance_ambiguity, conceptual, 'Distinguishes instrumental preparation from intrinsic spiritual performance in study.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_preparation, 70, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t70, kodashim_obligation__study_as_preparation, theater_ratio, 70, 0.05).
narrative_ontology:measurement(koda_tr_t500, kodashim_obligation__study_as_preparation, theater_ratio, 500, 0.07).
narrative_ontology:measurement(koda_tr_t1000, kodashim_obligation__study_as_preparation, theater_ratio, 1000, 0.08).
narrative_ontology:measurement(koda_tr_t1500, kodashim_obligation__study_as_preparation, theater_ratio, 1500, 0.09).
narrative_ontology:measurement(koda_tr_t2024, kodashim_obligation__study_as_preparation, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(koda_be_t70, kodashim_obligation__study_as_preparation, base_extractiveness, 70, 0.1).
narrative_ontology:measurement(koda_be_t500, kodashim_obligation__study_as_preparation, base_extractiveness, 500, 0.12).
narrative_ontology:measurement(koda_be_t1000, kodashim_obligation__study_as_preparation, base_extractiveness, 1000, 0.13).
narrative_ontology:measurement(koda_be_t1500, kodashim_obligation__study_as_preparation, base_extractiveness, 1500, 0.14).
narrative_ontology:measurement(koda_be_t2024, kodashim_obligation__study_as_preparation, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t70, kodashim_obligation__study_as_preparation, suppression_requirement, 70, 0.15).
narrative_ontology:measurement(koda_su_t500, kodashim_obligation__study_as_preparation, suppression_requirement, 500, 0.17).
narrative_ontology:measurement(koda_su_t1000, kodashim_obligation__study_as_preparation, suppression_requirement, 1000, 0.18).
narrative_ontology:measurement(koda_su_t1500, kodashim_obligation__study_as_preparation, suppression_requirement, 1500, 0.19).
narrative_ontology:measurement(koda_su_t2024, kodashim_obligation__study_as_preparation, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_preparation, information_standard).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation__study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation__study_as_archive).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'kodashim_obligation' kernel. Each reading presents a different structural understanding of the same underlying religious texts and communal practice, leading to different ε values and classifications. This reading emphasizes study as instrumental preparation for a future messianic restoration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
