% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__study_as_performance, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: sacrifice_commandment__study_as_performance
 *   human_readable: Study of Sacrifice Law as Fulfillment of Commandment
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   In halakhic theory, the study of sacrifice law is regarded as the actual
 *   exercise of the commandment itself — intellectual engagement fulfills the
 *   divine obligation even in the absence of the Temple. This reading
 *   (study_as_performance) is one of three contested readings of the
 *   sacrifice_commandment kernel. It asserts zero extractiveness: the
 *   scholar-worshipper is the sole beneficiary, and there is no victim set.
 *   The constraint operates as a voluntary coordination mechanism for divine
 *   service.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__study_as_performance, 0.05).
domain_priors:suppression_score(sacrifice_commandment__study_as_performance, 0.1).
domain_priors:theater_ratio(sacrifice_commandment__study_as_performance, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, extractiveness, 0.05).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_commandment__study_as_performance, "Study of Sacrifice Law as Fulfillment of Commandment").
narrative_ontology:topic_domain(sacrifice_commandment__study_as_performance, "religious/halakhic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__study_as_performance, '99eb3554-0226-444e-976a-6e36a0a431c7').
narrative_ontology:cs_kernel_codification('99eb3554-0226-444e-976a-6e36a0a431c7', fixed_text).
narrative_ontology:cs_authority_grounding('99eb3554-0226-444e-976a-6e36a0a431c7', lineage).
narrative_ontology:cs_interpretation_layer_present('99eb3554-0226-444e-976a-6e36a0a431c7').
narrative_ontology:cs_reading_relation('99eb3554-0226-444e-976a-6e36a0a431c7', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_reading_relation('99eb3554-0226-444e-976a-6e36a0a431c7', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_axiom('99eb3554-0226-444e-976a-6e36a0a431c7', foundational, study_equates_performance).
narrative_ontology:cs_axiom_status(study_equates_performance, holdable).
narrative_ontology:cs_axiom_grounding('99eb3554-0226-444e-976a-6e36a0a431c7', study_equates_performance, deontological).
narrative_ontology:cs_axiom('99eb3554-0226-444e-976a-6e36a0a431c7', foundational, intellectual_engagement_fulfills_obligation).
narrative_ontology:cs_axiom_status(intellectual_engagement_fulfills_obligation, holdable).
narrative_ontology:cs_axiom_grounding('99eb3554-0226-444e-976a-6e36a0a431c7', intellectual_engagement_fulfills_obligation, deontological).
narrative_ontology:cs_reference_frame('99eb3554-0226-444e-976a-6e36a0a431c7', talmudic_derivation).
narrative_ontology:cs_drift_state('99eb3554-0226-444e-976a-6e36a0a431c7', contemporary_halakhic_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('99eb3554-0226-444e-976a-6e36a0a431c7', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__study_as_performance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, scholar_worshipper).
narrative_ontology:constraint_vindicates(sacrifice_commandment__study_as_performance, study_as_worship_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_commandment__study_as_performance, divine_obligation_fulfilled_by_intellect).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engages in study of sacrifice laws as fulfillment of the divine commandment; derives spiritual benefit and communal recognition; participation is voluntary and self-directed.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, scholar_worshipper, beneficiary,
    organized, biographical, mobile, global).

% Posits and teaches that study of sacrifices fulfills the commandment; maintains the interpretive tradition through responsa, codes, and curricula; authority derives from chain of transmission.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, halakhic_authority, agenda_setter,
    institutional, generational, analytical, global).

% Hold the archive_maintenance reading: study preserves technical knowledge for future Temple restoration, not present worship; their voice is marginalized in this reading's framework.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, temple_restoration_advocates, excluded,
    organized, generational, constrained, global).

% Hold the performance_only reading: the commandment requires physical execution and is suspended without the Temple; they are not part of this reading's community of practice.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, performance_only_adherents, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the community's fulfillment of the sacrifice commandment through intellectual engagement when physical performance is impossible.
% TRANSFER_FUNCTION: Moves spiritual merit and divine favor from the act of study to the scholar-worshipper; no material transfer.
% ABSENT_VOICES: Temple restoration advocates (archive_maintenance reading) and performance-only adherents who maintain the commandment is suspended; they are excluded from this reading's framework.
% DISAPPEARANCE_RATIONALE: Without the principle that study fulfills the commandment, the sacrifice commandment would be entirely suspended for the diaspora community, removing a major avenue of divine service.
% FOUNDING_PROBLEM: After the destruction of the Second Temple, the Jewish people could no longer perform the sacrificial commandments, creating a crisis of divine obligation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by the Talmudic sages (Menachot 110a) who derived this principle from scriptural interpretation, and by centuries of halakhic consensus across diverse communities.
narrative_ontology:disappearance_verdict(sacrifice_commandment__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_commandment__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__study_as_performance, 0.05, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__study_as_performance_tests).
:- end_tests(sacrifice_commandment__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near zero (0.05) because the practice is intrinsically motivated and yields no material rents; suppression is minimal (0.1) as participation is voluntary and alternatives (other commandments, other readings) exist; theater_ratio is low (0.1) because the study is genuine worship, not performative. Accessibility_collapse (0.3) reflects that other forms of worship remain available. Resistance (0.2) comes from competing readings but does not threaten the practice.
 *
 * PERSPECTIVAL GAP:
 *   From the scholar_worshipper seat, the constraint is a genuine rope: a voluntary coordination that solves the problem of suspended Temple service. From the excluded seats, the same principle appears as a contested interpretive move that forecloses their preferred readings. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The scholar_worshipper is the beneficiary (d near 0.0) — they receive spiritual benefit. The halakhic_authority is the agenda_setter (d near 0.5) — they maintain the tradition but do not extract. The excluded seats (temple_restoration_advocates, performance_only_adherents) are not coordinated by this constraint; their exclusion is a feature of the kernel contest, not of this constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Temple destruction) remains live, so the constraint has not suffered mandatrophy. The arrangement continues to serve its original coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    social_coercion_ambiguity,
    'Does communal expectation create de facto coercion to study, making the practice extractive for those who would prefer other forms of worship?',
    'Sociological study of diaspora communities measuring participation rates vs. stated preference; comparison with communities where the reading is not taught.',
    'If significant coercion exists, extractiveness would rise and the constraint might reclassify as tangled_rope or snare for the scholar_worshipper seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_coercion_ambiguity, empirical, 'Whether voluntary practice masks social extraction.').

omega_variable(
    kernel_reading_boundary,
    'Is the study_as_performance reading logically compatible with the archive_maintenance reading within a single halakhic framework, or do they foreclose each other?',
    'Analysis of major poskim (decisors) who hold both that study fulfills the commandment now AND that it preserves knowledge for the future.',
    'If they coexist, the relation is coexists_with; if the study-as-performance claim logically negates the preparatory function, the relation becomes forecloses.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural relationship between sibling readings of the sacrifice_commandment kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__study_as_performance, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__study_as_performance, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t25, sacrifice_commandment__study_as_performance, theater_ratio, 25, 0.1).
narrative_ontology:measurement(sacr_tr_t50, sacrifice_commandment__study_as_performance, theater_ratio, 50, 0.1).
narrative_ontology:measurement(sacr_tr_t75, sacrifice_commandment__study_as_performance, theater_ratio, 75, 0.1).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_commandment__study_as_performance, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__study_as_performance, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(sacr_be_t25, sacrifice_commandment__study_as_performance, base_extractiveness, 25, 0.05).
narrative_ontology:measurement(sacr_be_t50, sacrifice_commandment__study_as_performance, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(sacr_be_t75, sacrifice_commandment__study_as_performance, base_extractiveness, 75, 0.05).
narrative_ontology:measurement(sacr_be_t100, sacrifice_commandment__study_as_performance, base_extractiveness, 100, 0.05).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_commandment__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__study_as_performance, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_commandment__study_as_performance, 0.08).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__archive_maintenance).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__performance_only).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the sacrifice_commandment kernel. The three readings form a constraint family linked by the shared kernel. The study_as_performance reading claims zero extractiveness and present fulfillment; the archive_maintenance reading claims instrumental value for future restoration; the performance_only reading claims suspension. Each has distinct ε and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
