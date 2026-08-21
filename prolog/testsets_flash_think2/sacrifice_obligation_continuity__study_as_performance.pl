% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__study_as_performance, []).

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
 *   constraint_id: sacrifice_obligation_continuity__study_as_performance
 *   human_readable: Sacrifice Obligation Continuity: Study as Performance
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This constraint describes the religious legal interpretation that the
 *   study of sacrifice law is itself a fulfillment of the commandment to
 *   offer sacrifices. This reading emerged after the destruction of the
 *   Second Temple, providing a mechanism for the continuity of a central
 *   religious obligation. It is a 'rope' because it coordinates the community
 *   around an accessible and beneficial practice, solving a genuine
 *   collective-action problem (how to fulfill a commandment when its physical
 *   performance is impossible) with minimal extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__study_as_performance, 0.15).
domain_priors:suppression_score(sacrifice_obligation_continuity__study_as_performance, 0.05).
domain_priors:theater_ratio(sacrifice_obligation_continuity__study_as_performance, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, extractiveness, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__study_as_performance, "Sacrifice Obligation Continuity: Study as Performance").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__study_as_performance, "religious_law/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__study_as_performance, '625efcb4-019c-4dd8-a7b5-50c871e7b433').
narrative_ontology:cs_kernel_codification('625efcb4-019c-4dd8-a7b5-50c871e7b433', fixed_text).
narrative_ontology:cs_authority_grounding('625efcb4-019c-4dd8-a7b5-50c871e7b433', lineage).
narrative_ontology:cs_interpretation_layer_present('625efcb4-019c-4dd8-a7b5-50c871e7b433').
narrative_ontology:cs_reading_relation('625efcb4-019c-4dd8-a7b5-50c871e7b433', sacrifice_obligation_continuity__archival_preservation, influences).
narrative_ontology:cs_reading_relation('625efcb4-019c-4dd8-a7b5-50c871e7b433', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('625efcb4-019c-4dd8-a7b5-50c871e7b433', sacrifice_obligation_continuity__performance_only, forecloses).
narrative_ontology:cs_axiom('625efcb4-019c-4dd8-a7b5-50c871e7b433', foundational, study_is_equivalent_to_action).
narrative_ontology:cs_axiom_status(study_is_equivalent_to_action, holdable).
narrative_ontology:cs_axiom_grounding('625efcb4-019c-4dd8-a7b5-50c871e7b433', study_is_equivalent_to_action, deontological).
narrative_ontology:cs_axiom('625efcb4-019c-4dd8-a7b5-50c871e7b433', foundational, divine_will_is_expressed_in_text_and_interpretation).
narrative_ontology:cs_axiom_status(divine_will_is_expressed_in_text_and_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('625efcb4-019c-4dd8-a7b5-50c871e7b433', divine_will_is_expressed_in_text_and_interpretation, theological).
narrative_ontology:cs_reference_frame('625efcb4-019c-4dd8-a7b5-50c871e7b433', post_temple_rabbinic_consensus).
narrative_ontology:cs_drift_state('625efcb4-019c-4dd8-a7b5-50c871e7b433', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('625efcb4-019c-4dd8-a7b5-50c871e7b433', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, religious_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, adherents_of_tradition).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__study_as_performance, halakhic_continuity_principle).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__study_as_performance, torah_study_as_ultimate_mitzvah).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret, transmit, and embody the tradition that study of sacrifice law is itself fulfillment. Their authority and social role are reinforced by this interpretation, as they are the primary practitioners and teachers of this study.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, religious_scholars, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__study_as_performance, religious_scholars, beneficiary).

% Are enabled to fulfill a central religious commandment through accessible textual engagement, maintaining their connection to the tradition and its divine mandates, even in the absence of physical ritual performance.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, adherents_of_tradition, beneficiary,
    moderate, biographical, mobile, global).

% Uphold and teach this interpretation as normative, ensuring the continuity of the religious legal system and the spiritual well-being of the community. Their legitimacy is tied to providing a viable path for commandment fulfillment.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, traditional_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Adhere to a different reading of the sacrifice obligation, believing it is suspended pending messianic restoration and physical rebuilding of the Temple. They are excluded from the dominant discourse of current fulfillment through study, though they may still engage in study for readiness.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, messianic_restorationists, excluded,
    organized, generational, identity_locked, global).

% Analyze the historical development and sociological function of this interpretation within the broader religious tradition, without necessarily adhering to its normative claims. They observe its role in maintaining cultural and religious continuity.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, secular_historians, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__study_as_performance, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__study_as_performance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent, accessible, and continuous path for adherents to fulfill a central religious commandment in the absence of its physical performance, thereby coordinating the community's religious practice and identity.
% TRANSFER_FUNCTION: Transfers the locus of ritual performance from physical sacrifice (now impossible) to intellectual engagement (study), from the individual's direct action to the community's collective and individual textual engagement.
% ABSENT_VOICES: Adherents of the 'performance_only' reading would object, arguing that study is preparation for future restoration, not fulfillment itself, and that the obligation remains unfulfilled without physical action. They are marginalized in the dominant discourse of current fulfillment.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished overnight, a central religious commandment would become unfulfillable for nearly two millennia, leading to widespread theological crisis, profound reinterpretation of core tenets, or a significant fracturing/abandonment of the religious tradition's continuity.
% FOUNDING_PROBLEM: The destruction of the Second Temple (70 CE) rendered the central commandment of physical animal sacrifice impossible to fulfill, threatening the continuity, coherence, and spiritual vitality of the religious tradition.
% FOUNDING_PROBLEM_CORROBORATION: The historical record of rabbinic discourse immediately following the Temple's destruction, the subsequent codification in the Talmud, and the ongoing practice of textual study as a central religious act across diverse Jewish communities for nearly two millennia, all corroborate the problem's historical reality and its continued theological relevance.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__study_as_performance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sacrifice_obligation_continuity__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__study_as_performance, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__study_as_performance_tests).
:- end_tests(sacrifice_obligation_continuity__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low because study is widely accessible and provides genuine spiritual fulfillment without significant material cost. Suppression is minimal, as study is encouraged rather than coerced. Theater ratio is low because the act of study is considered a sincere and effective fulfillment, not a mere substitute performance. Accessibility collapse is high for the physical act of sacrifice, but the 'study as performance' reading provides a robust and widely adopted alternative.
 *
 * PERSPECTIVAL GAP:
 *   While this reading is widely accepted, those who adhere to a 'performance_only' view would experience the absence of physical sacrifice as an unfulfilled obligation, whereas adherents of 'study_as_performance' experience it as a fulfilled one. The engine's classification reflects the structural reality of this reading, where the obligation is indeed fulfillable.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious scholars and traditional authorities are beneficiaries and agenda-setters, as their roles are central to the interpretation and transmission of this practice. Adherents are beneficiaries, gaining a path to fulfillment. There are no direct victims, as the obligation is fulfilled, not extracted from. Messianic restorationists are excluded, as their interpretation defers fulfillment rather than enabling it through study.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_ambiguity_study_vs_action,
    'Is the study of sacrifice law truly equivalent to physical performance in the eyes of divine law, or is it a pragmatic rabbinic innovation to cope with historical circumstances?',
    'Continued theological and exegetical debate within the tradition, potentially informed by new textual discoveries or shifts in theological consensus.',
    'If definitively established as a pragmatic innovation rather than a true equivalence, the constraint might be reclassified as a ''scaffold'' (a temporary solution) or a ''tangled_rope'' (coordination with an underlying theological compromise/extraction from the ''true'' obligation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_ambiguity_study_vs_action, conceptual, 'Ambiguity regarding the theological status of study as fulfillment.').

omega_variable(
    framing_underdetermination_authority_grounding,
    'Is the authority grounding for this reading truly ''lineage'' (transmission from past authorities), or has it become ''practice'' based on its widespread adoption and ongoing communal enactment?',
    'Sociological and anthropological study of contemporary religious authority structures and halakhic decision-making processes within the tradition.',
    'If the grounding is primarily ''practice'', the constraint''s resilience to external challenges or internal dissent might be different, potentially altering its ''cs_pattern'' and its vulnerability to shifts in communal norms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_underdetermination_authority_grounding, conceptual, 'Ambiguity in the primary source of authority grounding for this interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__study_as_performance, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sacr_tr_t300, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 300, 0.06).
narrative_ontology:measurement(sacr_tr_t800, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 800, 0.07).
narrative_ontology:measurement(sacr_tr_t1200, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 1200, 0.08).
narrative_ontology:measurement(sacr_tr_t1600, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 1600, 0.09).
narrative_ontology:measurement(sacr_tr_t1950, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 1950, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(sacr_be_t300, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 300, 0.11).
narrative_ontology:measurement(sacr_be_t800, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 800, 0.12).
narrative_ontology:measurement(sacr_be_t1200, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 1200, 0.13).
narrative_ontology:measurement(sacr_be_t1600, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 1600, 0.14).
narrative_ontology:measurement(sacr_be_t1950, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 1950, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(sacr_su_t300, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 300, 0.03).
narrative_ontology:measurement(sacr_su_t800, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 800, 0.03).
narrative_ontology:measurement(sacr_su_t1200, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 1200, 0.04).
narrative_ontology:measurement(sacr_su_t1600, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 1600, 0.04).
narrative_ontology:measurement(sacr_su_t1950, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 1950, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, halakhic_continuity_principle).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, torah_study_as_ultimate_mitzvah).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
