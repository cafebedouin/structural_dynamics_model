% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__messianic_suspension_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__messianic_suspension_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sacrifice_obligation_kernel__messianic_suspension_reading
 *   human_readable: Sacrifice Obligation: Messianic Suspension Reading
 *   domain: religious_law/halakhic_authority
 *
 * SUMMARY:
 *   This constraint represents the 'messianic suspension' reading of the
 *   sacrifice obligation kernel within Jewish religious law. Following the
 *   destruction of the Temple, the obligation to offer sacrifices is
 *   understood as divinely suspended, not abrogated or transformed into a
 *   different mitzvah. The study of sacrifice laws (Kodashim) is seen as
 *   maintaining operational readiness for the messianic era, when the Temple
 *   will be rebuilt and sacrifices restored. This reading emphasizes the
 *   temporary abeyance of the physical act while preserving the knowledge and
 *   intent. It is claimed as a Mountain due to its grounding in divine decree
 *   and its persistence across millennia, with minimal extraction as the
 *   obligation is not actively enforced in its suspended state.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__messianic_suspension_reading, 0.05).
domain_priors:suppression_score(sacrifice_obligation_kernel__messianic_suspension_reading, 0.02).
domain_priors:theater_ratio(sacrifice_obligation_kernel__messianic_suspension_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__messianic_suspension_reading, mountain).
narrative_ontology:human_readable(sacrifice_obligation_kernel__messianic_suspension_reading, "Sacrifice Obligation: Messianic Suspension Reading").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__messianic_suspension_reading, "religious_law/halakhic_authority").

domain_priors:emerges_naturally(sacrifice_obligation_kernel__messianic_suspension_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__messianic_suspension_reading, '19c94c3f-db91-4d46-96e6-58b03cf5c4e9').
narrative_ontology:cs_kernel_codification('19c94c3f-db91-4d46-96e6-58b03cf5c4e9', fixed_text).
narrative_ontology:cs_authority_grounding('19c94c3f-db91-4d46-96e6-58b03cf5c4e9', lineage).
narrative_ontology:cs_interpretation_layer_present('19c94c3f-db91-4d46-96e6-58b03cf5c4e9').
narrative_ontology:cs_reading_relation('19c94c3f-db91-4d46-96e6-58b03cf5c4e9', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_reading_relation('19c94c3f-db91-4d46-96e6-58b03cf5c4e9', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('19c94c3f-db91-4d46-96e6-58b03cf5c4e9', sacrifice_obligation_kernel__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('19c94c3f-db91-4d46-96e6-58b03cf5c4e9', foundational, divine_suspension_of_physical_mitzvah).
narrative_ontology:cs_axiom_status(divine_suspension_of_physical_mitzvah, holdable).
narrative_ontology:cs_axiom_grounding('19c94c3f-db91-4d46-96e6-58b03cf5c4e9', divine_suspension_of_physical_mitzvah, theological).
narrative_ontology:cs_axiom('19c94c3f-db91-4d46-96e6-58b03cf5c4e9', foundational, study_as_operational_readiness).
narrative_ontology:cs_axiom_status(study_as_operational_readiness, holdable).
narrative_ontology:cs_axiom_grounding('19c94c3f-db91-4d46-96e6-58b03cf5c4e9', study_as_operational_readiness, conventional).
narrative_ontology:cs_reference_frame('19c94c3f-db91-4d46-96e6-58b03cf5c4e9', divinely_suspended_obligation).
narrative_ontology:cs_drift_state('19c94c3f-db91-4d46-96e6-58b03cf5c4e9', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('19c94c3f-db91-4d46-96e6-58b03cf5c4e9', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations_of_israel).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, kohanim_priestly_lineage).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__messianic_suspension_reading, contemporary_jewish_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the preservation of operational knowledge for the eventual restoration of the Temple service, ensuring continuity of religious practice. Their identity is deeply tied to this continuity.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations_of_israel, beneficiary,
    powerless, generational, identity_locked, global).

% Maintains their unique role and readiness for the Temple service through the study of sacrifice laws, even in its suspended state. Their identity and communal standing are defined by this inherited obligation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, kohanim_priestly_lineage, beneficiary,
    organized, generational, identity_locked, global).

% Bears the diffuse cost of maintaining the intellectual and spiritual infrastructure for study, without direct performance of sacrifices. The 'cost' is primarily intellectual effort and communal resource allocation for learning.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, contemporary_jewish_community, payer,
    moderate, biographical, constrained, global).

% Interpret and transmit the divine command regarding the suspension and future restoration of sacrifices. They set the agenda for study and maintain the legal framework for this understanding. Their authority is grounded in lineage and textual interpretation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, halakhic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% The ultimate source of the constraint, posited as a fixed and unchangeable decree that dictates the suspension and future restoration of the sacrifice obligation. It is the referent for the 'natural' emergence of the constraint.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, divine_command, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_kernel__messianic_suspension_reading, divine_command).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective understanding and practice of the Jewish people regarding the Temple service during its absence, ensuring continuity and readiness for its eventual restoration.
% TRANSFER_FUNCTION: Transfers the obligation from immediate physical performance to intellectual engagement and readiness, preserving the spiritual and legal framework across generations.
% ABSENT_VOICES: Those who might argue for immediate, symbolic, or substitutive performance of sacrifices are implicitly excluded by the divine decree of suspension. Their voices are present in historical debates but not currently operative within this halakhic framework.
% DISAPPEARANCE_RATIONALE: If this understanding of suspended obligation vanished, it would fundamentally alter Jewish religious practice, identity, and the role of study. The entire halakhic system for the Temple service would need reinterpretation, leading to profound communal and theological reorganization.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered physical performance of sacrifices impossible, creating a crisis of religious obligation and continuity.
% FOUNDING_PROBLEM_CORROBORATION: The problem is universally acknowledged as live within the Jewish tradition, attested by centuries of rabbinic literature, communal prayer, and historical memory. This corroboration comes from the entire tradition, not just benefiting parties.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__messianic_suspension_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__messianic_suspension_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_obligation_kernel__messianic_suspension_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, ExtMetricName, E),
    domain_priors:suppression_score(sacrifice_obligation_kernel__messianic_suspension_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sacrifice_obligation_kernel__messianic_suspension_reading),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because the obligation is suspended, not actively extracting from practitioners. The 'cost' is primarily the intellectual effort of study, which is seen as a spiritual benefit rather than a burden. Suppression is minimal (0.02) as adherence is voluntary and driven by religious commitment, not coercion. Theater ratio is low (0.1) because the study is genuinely aimed at preserving knowledge for future performance, not merely a performative substitute. Accessibility collapse is high (0.9) because, within this framework, there are no legitimate alternatives to the divinely ordained suspension and future restoration. Resistance is negligible (0.01) as this reading is widely accepted within Orthodox Judaism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the contemporary Jewish community, the constraint is a foundational aspect of their religious life, ensuring continuity. From an external, secular perspective, it might appear as an elaborate system for maintaining an 'obsolete' practice, but within the internal logic, it is a divinely mandated suspension. The engine's classification as a Mountain from all seats reflects this internal coherence and low extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations and the Kohanim priestly lineage are beneficiaries, as the constraint ensures the continuity of their religious heritage and future roles. The contemporary Jewish community is a diffuse payer, bearing the 'cost' of study and communal maintenance. Halakhic authorities are agenda-setters, interpreting and transmitting the divine command. The divine command itself is an analytical observer, the ultimate source of the constraint's naturalness.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_suspension_naturalness,
    'Is the ''divine suspension'' a genuine natural law (Mountain) or a halakhic interpretation (constructed constraint) that benefits identifiable agents (False Summit)?',
    'Theological consensus shifts, or a new authoritative interpretation emerges that redefines the nature of the suspension.',
    'If reclassified as a constructed constraint, its Mountain status would be challenged, potentially shifting to a Rope or Tangled Rope if beneficiaries are seen as actively maintaining the interpretation for their own gain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_suspension_naturalness, conceptual, 'Ambiguity between divine decree and interpretive construction.').

omega_variable(
    study_as_readiness_vs_substitute,
    'Is the study of sacrifice laws truly for ''operational readiness'' or does it function as a de facto substitute for the actual mitzvah, masking a deeper transformation of the obligation?',
    'Analysis of rabbinic responsa and communal practice over time: if the emphasis shifts from future performance to the intrinsic value of study, it suggests a functional substitution.',
    'If study is a de facto substitute, the constraint''s ''suspension'' claim is weakened, and its extractiveness might be re-evaluated if the ''cost'' of study is seen as fulfilling an obligation that could otherwise be fulfilled differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_readiness_vs_substitute, empirical, 'Whether study maintains readiness or acts as a functional substitute.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__messianic_suspension_reading, 70, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t70, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 70, 0.1).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 500, 0.1).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(sacr_tr_t2024, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t70, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 70, 0.05).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 500, 0.05).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1000, 0.05).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1500, 0.05).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 2000, 0.05).
narrative_ontology:measurement(sacr_be_t2024, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t70, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 70, 0.02).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 500, 0.02).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1000, 0.02).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1500, 0.02).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 2000, 0.02).
narrative_ontology:measurement(sacr_su_t2024, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 2024, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__messianic_suspension_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
