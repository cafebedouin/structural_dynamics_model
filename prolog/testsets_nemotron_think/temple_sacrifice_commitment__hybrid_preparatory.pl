% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__hybrid_preparatory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__hybrid_preparatory, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: temple_sacrifice_commitment__hybrid_preparatory
 *   human_readable: Hybrid Preparatory Study of Temple Sacrifice Law
 *   domain: religious/legal
 *
 * SUMMARY:
 *   The hybrid_preparatory reading of the temple sacrifice commitment holds
 *   that study of sacrificial law maintains the divine obligation in a
 *   suspended state — neither full occupation (since the Temple is destroyed)
 *   nor mere archiving. It is a preparatory exercise for messianic
 *   restoration, extracting cognitive and material resources from students
 *   and funders for an uncertain future benefit. This reading coexists with
 *   other readings (study_as_exercise, performance_only,
 *   symbolic_transformation) within the halakhic tradition, each offering a
 *   different account of what study accomplishes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__hybrid_preparatory, 0.4).
domain_priors:suppression_score(temple_sacrifice_commitment__hybrid_preparatory, 0.2).
domain_priors:theater_ratio(temple_sacrifice_commitment__hybrid_preparatory, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, extractiveness, 0.4).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__hybrid_preparatory, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__hybrid_preparatory, "Hybrid Preparatory Study of Temple Sacrifice Law").
narrative_ontology:topic_domain(temple_sacrifice_commitment__hybrid_preparatory, "religious/legal").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__hybrid_preparatory).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__hybrid_preparatory, '737c59e8-338e-4a39-a3b1-fe102d2952fd').
narrative_ontology:cs_kernel_codification('737c59e8-338e-4a39-a3b1-fe102d2952fd', fixed_text).
narrative_ontology:cs_authority_grounding('737c59e8-338e-4a39-a3b1-fe102d2952fd', lineage).
narrative_ontology:cs_interpretation_layer_present('737c59e8-338e-4a39-a3b1-fe102d2952fd').
narrative_ontology:cs_reading_relation('737c59e8-338e-4a39-a3b1-fe102d2952fd', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('737c59e8-338e-4a39-a3b1-fe102d2952fd', temple_sacrifice_commitment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('737c59e8-338e-4a39-a3b1-fe102d2952fd', temple_sacrifice_commitment__symbolic_transformation, coexists_with).
narrative_ontology:cs_axiom('737c59e8-338e-4a39-a3b1-fe102d2952fd', foundational, study_as_preparatory_for_messianic_restoration).
narrative_ontology:cs_axiom_status(study_as_preparatory_for_messianic_restoration, holdable).
narrative_ontology:cs_axiom_grounding('737c59e8-338e-4a39-a3b1-fe102d2952fd', study_as_preparatory_for_messianic_restoration, theological).
narrative_ontology:cs_axiom('737c59e8-338e-4a39-a3b1-fe102d2952fd', foundational, suspended_commitment_preserves_obligation).
narrative_ontology:cs_axiom_status(suspended_commitment_preserves_obligation, holdable).
narrative_ontology:cs_axiom_grounding('737c59e8-338e-4a39-a3b1-fe102d2952fd', suspended_commitment_preserves_obligation, deontological).
narrative_ontology:cs_reference_frame('737c59e8-338e-4a39-a3b1-fe102d2952fd', suspended_commitment_awaiting_restoration).
narrative_ontology:cs_drift_state('737c59e8-338e-4a39-a3b1-fe102d2952fd', post_temple_destruction_exile, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('737c59e8-338e-4a39-a3b1-fe102d2952fd', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, halakhic_authorities).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, devout_community).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, study_funders).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, students).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__hybrid_preparatory, divine_command_to_study).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__hybrid_preparatory, messianic_restoration_certainty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% They define the curriculum and obligation for studying sacrifice law, maintaining the commitment in suspended state. Their authority rests on rabbinic lineage and they do not bear the direct resource costs of study.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, halakhic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__hybrid_preparatory, halakhic_authorities, beneficiary).

% The community benefits from the continuity of tradition and the hope of restoration. They support study institutions financially and culturally, but their exit is constrained by religious identity.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, devout_community, beneficiary,
    organized, generational, identity_locked, global).

% Individuals and institutions that fund yeshivas and kollels where sacrifice law is studied. They allocate resources to a non-performable practice with uncertain future benefit. Exit is constrained by communal expectations and tax structures.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, study_funders, payer,
    moderate, biographical, constrained, global).

% Students dedicate years to studying intricate laws of sacrifices that cannot be performed. They invest cognitive resources and forego alternative livelihoods. Exit is constrained by identity and communal pressure.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, students, payer,
    moderate, biographical, constrained, global).

% Those who do not accept the religious obligation but are affected by resource allocation in Israeli society (e.g., state funding for yeshivas). They are excluded from the halakhic discourse.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, secular_jews, excluded,
    organized, biographical, mobile, global).

% Academic scholar analyzing the halakhic system from outside, providing historical and structural analysis.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, halakhic_scholar, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the halakhic commitment to Temple sacrifice in a suspended state during exile, preserving the legal framework for future messianic restoration.
% TRANSFER_FUNCTION: Moves cognitive resources (study time, intellectual labor) and material resources (funding for study institutions) from funders and students to the maintenance of a non-performable legal corpus.
% ABSENT_VOICES: Secular Jews and non-Orthodox denominations who object to resource allocation for non-performable study but are excluded from halakhic decision-making.
% DISAPPEARANCE_RATIONALE: The study obligation structures a significant portion of Orthodox communal economy and identity; its removal would cause reorganization of institutions, funding, and individual life plans.
% FOUNDING_PROBLEM: After the Temple's destruction, how to maintain the divine command of sacrifices when material performance is impossible?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by classical rabbinic sources (e.g., Talmud, Maimonides) and acknowledged by contemporary halakhic authorities across the spectrum. No external corroboration needed as it is internal to the tradition.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__hybrid_preparatory, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__hybrid_preparatory, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__hybrid_preparatory, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__hybrid_preparatory, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__hybrid_preparatory, 0.4, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).
:- end_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.4) because study demands significant intellectual labor and funding without immediate performable return. Suppression is low (0.2) because participation is largely voluntary within the committed community, though social pressure exists. Theater ratio is low (0.1) because the study is genuine intellectual engagement, not performative. Accessibility collapse is moderate (0.5) because alternative interpretations exist but are marginalized within the Orthodox framework. Resistance is low (0.2) because the reading is widely accepted in its community.
 *
 * PERSPECTIVAL GAP:
 *   From the authority seat, the constraint is a rope (coordination of tradition). From the payer seats, it is a tangled rope (coordination plus extraction). The engine will compute per-seat types from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic authorities are agenda-setters and beneficiaries (they shape the obligation and gain authority). Devout community members are beneficiaries (they gain spiritual continuity). Study funders and students are payers (they bear resource costs). Secular Jews are excluded from the discourse but affected by public funding. The analytical observer sees the full structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining sacrifice obligation without Temple) remains live because exile continues. The arrangement has not atrophied; it actively structures communal life. No mandatrophy resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Does the hybrid_preparatory reading constitute a structurally distinct constraint from study_as_exercise, or are they variants of the same coordination function?',
    'Compare resource flows: if study_as_exercise directs resources to immediate spiritual fulfillment while hybrid_preparatory directs them to future restoration, the extraction profiles differ.',
    'If distinct, each reading gets its own ε and classification; if variants, they merge into one constraint with internal ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the preparatory framing creates a separate constraint story.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative interpretations structural (communal enforcement) or internalized (identity fusion with the commitment)?',
    'Post-exit observation: if former participants continue to feel obligated to study after leaving the community, suppression is partially internalized.',
    'If internalized, effective suppression is higher than structural measures suggest; the constraint travels with the agent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in interpersonal-religious constraints.').

omega_variable(
    messianic_restoration_uncertainty,
    'Does the messianic restoration have a non-zero probability in the reading''s own epistemic framework, or is it a theological placeholder?',
    'Analyze halakhic sources: if restoration is treated as certain future fact, extraction is investment; if treated as symbolic, extraction is pure cost.',
    'If certainty is doctrinal, extractiveness is justified as deferred coordination; if symbolic, extractiveness is less defensible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(messianic_restoration_uncertainty, preference, 'Epistemic status of the messianic horizon in the reading''s own terms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__hybrid_preparatory, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temple_sacrifice_commitment__hybrid_preparatory_tr_t0, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0, 0.05).
narrative_ontology:measurement(temple_sacrifice_commitment__hybrid_preparatory_tr_t400, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 400, 0.07).
narrative_ontology:measurement(temple_sacrifice_commitment__hybrid_preparatory_tr_t800, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 800, 0.08).
narrative_ontology:measurement(temple_sacrifice_commitment__hybrid_preparatory_tr_t1200, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 1200, 0.09).
narrative_ontology:measurement(temple_sacrifice_commitment__hybrid_preparatory_tr_t1600, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(temple_sacrifice_commitment__hybrid_preparatory_tr_t2000, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 2000, 0.1).

% Extraction over time
narrative_ontology:measurement(temple_sacrifice_commitment__hybrid_preparatory_be_t0, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(temple_sacrifice_commitment__hybrid_preparatory_be_t400, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 400, 0.32).
narrative_ontology:measurement(temple_sacrifice_commitment__hybrid_preparatory_be_t800, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 800, 0.35).
narrative_ontology:measurement(temple_sacrifice_commitment__hybrid_preparatory_be_t1200, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 1200, 0.38).
narrative_ontology:measurement(temple_sacrifice_commitment__hybrid_preparatory_be_t1600, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 1600, 0.39).
narrative_ontology:measurement(temple_sacrifice_commitment__hybrid_preparatory_be_t2000, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 2000, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(temple_sacrifice_commitment__hybrid_preparatory_su_t0, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(temple_sacrifice_commitment__hybrid_preparatory_su_t400, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 400, 0.18).
narrative_ontology:measurement(temple_sacrifice_commitment__hybrid_preparatory_su_t800, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 800, 0.2).
narrative_ontology:measurement(temple_sacrifice_commitment__hybrid_preparatory_su_t1200, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 1200, 0.2).
narrative_ontology:measurement(temple_sacrifice_commitment__hybrid_preparatory_su_t1600, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 1600, 0.2).
narrative_ontology:measurement(temple_sacrifice_commitment__hybrid_preparatory_su_t2000, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 2000, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__hybrid_preparatory, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__hybrid_preparatory, 0.08).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the temple_sacrifice_commitment kernel. The hybrid_preparatory reading differs from study_as_exercise by denying that study is performance; from performance_only by affirming study as suspended occupation; from symbolic_transformation by awaiting material restoration rather than accepting transformation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
