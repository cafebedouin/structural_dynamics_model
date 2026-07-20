% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__performance_only, []).

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
 *   constraint_id: temple_sacrifice_commitment__performance_only
 *   human_readable: Temple Sacrifice Commitment â Performance-Only Reading
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the performance_only reading of the
 *   temple_sacrifice_commitment kernel. In this reading, the halakhic
 *   commandment of temple sacrifice remains structurally unfulfilled in the
 *   absence of material conditions; study of sacrificial law is classified as
 *   archival preservation rather than occupation of the commitment. The
 *   commitment persists as a dormant legal husk, maintained by rabbinic
 *   interpretive inertia and generational study practices that coordinate the
 *   community around a potential future restoration but do not themselves
 *   satisfy the commandment. This reading is structurally distinct from
 *   sibling readings (study_as_exercise, hybrid_preparatory,
 *   symbolic_transformation) that assign present-tense religious value to
 *   study itself. The constraint currently exhibits low extractiveness and no
 *   current victim set, but carries latent extraction potential should
 *   restoration be attempted without ethical evolution.
 *
 * KEY AGENTS:
 *   - halakhic_authorities: Agenda-setter (institutional/analytical) â adjudicates the performance-only boundary
 *   - observant_community: Payer (organized/identity_locked) â bears the theological burden of unfulfilled commandment
 *   - temple_restoration_movement: Excluded (moderate/constrained) â advocates immediate performance, marginalized by mainstream dormancy framing
 *   - rabbinic_students: Beneficiary/payer dual (moderate/constrained) â study the corpus, inherit the tradition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__performance_only, 0.2).
domain_priors:suppression_score(temple_sacrifice_commitment__performance_only, 0.15).
domain_priors:theater_ratio(temple_sacrifice_commitment__performance_only, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, extractiveness, 0.2).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, theater_ratio, 0.8).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__performance_only, piton).
narrative_ontology:human_readable(temple_sacrifice_commitment__performance_only, "Temple Sacrifice Commitment â Performance-Only Reading").
narrative_ontology:topic_domain(temple_sacrifice_commitment__performance_only, "religious_law/halakhic_tradition/commitment_system_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__performance_only, '4bb13fdc-f3d8-4214-ac3d-fe6ca8bc81a3').
narrative_ontology:cs_kernel_codification('4bb13fdc-f3d8-4214-ac3d-fe6ca8bc81a3', fixed_text).
narrative_ontology:cs_authority_grounding('4bb13fdc-f3d8-4214-ac3d-fe6ca8bc81a3', lineage).
narrative_ontology:cs_interpretation_layer_present('4bb13fdc-f3d8-4214-ac3d-fe6ca8bc81a3').
narrative_ontology:cs_reading_relation('4bb13fdc-f3d8-4214-ac3d-fe6ca8bc81a3', temple_sacrifice_commitment__study_as_exercise, forecloses).
narrative_ontology:cs_reading_relation('4bb13fdc-f3d8-4214-ac3d-fe6ca8bc81a3', temple_sacrifice_commitment__hybrid_preparatory, forecloses).
narrative_ontology:cs_reading_relation('4bb13fdc-f3d8-4214-ac3d-fe6ca8bc81a3', temple_sacrifice_commitment__symbolic_transformation, forecloses).
narrative_ontology:cs_axiom('4bb13fdc-f3d8-4214-ac3d-fe6ca8bc81a3', foundational, material_instantiation_requirement).
narrative_ontology:cs_axiom_status(material_instantiation_requirement, holdable).
narrative_ontology:cs_axiom_grounding('4bb13fdc-f3d8-4214-ac3d-fe6ca8bc81a3', material_instantiation_requirement, deontological).
narrative_ontology:cs_axiom('4bb13fdc-f3d8-4214-ac3d-fe6ca8bc81a3', foundational, study_as_non_occupation).
narrative_ontology:cs_axiom_status(study_as_non_occupation, holdable).
narrative_ontology:cs_axiom_grounding('4bb13fdc-f3d8-4214-ac3d-fe6ca8bc81a3', study_as_non_occupation, conventional).
narrative_ontology:cs_reference_frame('4bb13fdc-f3d8-4214-ac3d-fe6ca8bc81a3', material_instantiation_norm).
narrative_ontology:cs_drift_state('4bb13fdc-f3d8-4214-ac3d-fe6ca8bc81a3', contemporary_diaspora, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('4bb13fdc-f3d8-4214-ac3d-fe6ca8bc81a3', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, rabbinic_students).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__performance_only, observant_community).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__performance_only, rabbinic_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the interpretive tradition that sacrifice law requires material instantiation in a Temple; adjudicate that study of sacrificial tractates without performance constitutes archival preservation rather than commandment-occupation; preserve the legal architecture and pedagogical transmission of a defunct practice across generations.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, halakhic_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Bound by the halakhic commitment to regard the sacrificial commandment as dormant and unfulfilled; bears the theological burden of an awaiting messianic restoration that would enable performance; study of sacrifice law is obligatory but explicitly does not satisfy the underlying commitment; exit would require leaving the covenantal framework entirely.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, observant_community, payer,
    organized, generational, identity_locked, global).

% Advocates for immediate restoration of sacrificial practice, often tied to political messianism and Temple Mount activism; structurally marginalized in mainstream halakhic discourse because the performance_only reading treats current conditions as definitively pre-restoration; their voices are absent from normative adjudication.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, temple_restoration_movement, excluded,
    moderate, biographical, constrained, national).

% Study sacrificial law as part of standard rabbinic curriculum; benefit from continuity of tradition, textual mastery, and communal identity; pay in the labor of mastering elaborate legal details that have no material outlet; their study is framed as preservation, not fulfillment.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, rabbinic_students, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__performance_only, rabbinic_students, payer).

narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Study of sacrifice law coordinates the observant community around a shared historical commitment and potential future restoration, preserving legal continuity across generations where material performance is impossible.
% TRANSFER_FUNCTION: The arrangement moves halakhic attention and scholarly labor toward the maintenance of a dormant legal corpus; the community transfers time and cognitive energy to the study of defunct practice, receiving in return continuity of identity and messianic optionality.
% ABSENT_VOICES: The temple restoration movement and voices arguing for ethical evolution before restoration are marginal or excluded; they would demand immediate performance or permanent transformation but are not seated in mainstream halakhic adjudication.
% DISAPPEARANCE_RATIONALE: If the performance-only reading vanished, the theological status of the community's study would shift from archival preservation to potential occupation of the commandment; alternative readings (study-as-exercise, symbolic-transformation) would gain legitimacy, rearranging rabbinic pedagogy, liturgical framing, and the political theology of restoration movements.
% FOUNDING_PROBLEM: The destruction of the Second Temple removed the material conditions for sacrifice, creating a crisis of halakhic continuity: how does Israel remain constituted by a commandment it cannot perform?
% FOUNDING_PROBLEM_CORROBORATION: Historical historians of rabbinic Judaism and academic scholars of religion attest the destruction crisis from outside the halakhic beneficiary circle; contemporary ethicists and religious historians attest that the founding problem is dead and the arrangement persists as inertia, while traditional authorities attest it remains live awaiting messianic resolution.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__performance_only, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__performance_only, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__performance_only, 0.2, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__performance_only_tests).
:- end_tests(temple_sacrifice_commitment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint scores low on extractiveness (0.20) and suppression (0.15) because the absence of the Temple makes active enforcement impossible; there is nothing to enforce. Theater_ratio is high (0.80) because the primary activity maintaining the constraint is the elaborate study and rehearsal of legal procedures that cannot be performed â the constraint is sustained by performative maintenance of a defunct practice. Accessibility_collapse is moderate (0.60) because within the halakhic framework, the alternatives (symbolic transformation, study-as-exercise) are theologically available but structurally subordinated. Resistance is low (0.25) because the dormant state is widely accepted; active resistance is limited to marginal restoration movements.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (halakhic authorities) experiences the constraint as a faithful preservation of divine law requiring no adjustment. The payer seat (observant community) experiences it as a generational burden of unfulfillment. The excluded seat (restoration movement) experiences it as an illegitimate deferral. The engine should compute divergent classifications: near-piton or rope for the authority seat, piton for the community, and near-snare for the excluded seat if they were subject to suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   The halakhic_authorities sit near the beneficiary end (d low) because they derive institutional continuity and authority from maintaining the interpretive framework. The observant_community sits near symmetric-to-target (d moderate-high) because they bear the obligation without fulfillment. However, because exit is identity_locked rather than trapped, the effective extraction is damped â the constraint is internalized as theological identity rather than externally imposed. No directionality override is needed because beneficiary/victim declarations plus exit options capture the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by distinguishing the coordination function (study preserves legal continuity, a low-epsilon rope) from the commitment status (the commitment itself is a dormant piton). Without this distinction, one might classify the entire sacrificial corpus as either active coordination (rope) or abandoned (mountain-of-history). The piton classification captures that the constraint is maintained performatively without current function, while the rope classification of study-as-practice is a separable coordination mechanism nested within the same institutional shell.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_only_kernel_reading,
    'Is the performance-only reading a historically faithful continuation of classical halakhic material instantiation, or a post-traumatic reconstruction that retroactively hardened a soft commitment?',
    'Historical-philological analysis of Talmudic and geonic sources on the status of sacrificial study in the absence of the Temple; comparison with Second Temple and Second Commonwealth textual evidence.',
    'If the material-instantiation requirement was historically softer or more contested, the performance_only reading would shift from mountain-adjacent norm to historically contingent interpretation, lowering its authority and potentially reclassifying its constraint type from piton to scaffold or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_only_kernel_reading, empirical, 'Historical fidelity of the material-instantiation requirement').

omega_variable(
    restoration_ethics_ambiguity,
    'If material restoration of sacrifice were attempted without ethical evolution, would the constraint generate a victim set (e.g., animal welfare concerns, ethical violence, coercive theocratic imposition)?',
    'Ethical modeling of restoration scenarios under current moral frameworks; analysis of how the performance_only reading would interface with modernity if material conditions returned.',
    'If restoration without ethical evolution would create victims, the dormant constraint is a latent snare rather than a benign piton; the current low-extraction dormancy is a temporary state, not a stable equilibrium.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_ethics_ambiguity, preference, 'Latent victimhood under restoration scenarios').

omega_variable(
    sibling_reading_structural_delta,
    'Does the performance_only reading foreclose all sibling readings within a single halakhic framework, or can it coexist with hybrid or symbolic framings as legitimate minority positions?',
    'Jurisprudential analysis of whether mainstream halakhic adjudication treats these readings as mutually exclusive legal positions or as permissible theological variations within a single legal system.',
    'If the readings coexist as legitimate options, the structural relationship should be coexists_with rather than forecloses, and the constraint''s classification should reflect distributed authority rather than lineage-based foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Logical exclusivity versus pluralism within halakhic framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__performance_only, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__performance_only, theater_ratio, 0, 0.2).
narrative_ontology:measurement(temp_tr_t400, temple_sacrifice_commitment__performance_only, theater_ratio, 400, 0.45).
narrative_ontology:measurement(temp_tr_t800, temple_sacrifice_commitment__performance_only, theater_ratio, 800, 0.65).
narrative_ontology:measurement(temp_tr_t1200, temple_sacrifice_commitment__performance_only, theater_ratio, 1200, 0.75).
narrative_ontology:measurement(temp_tr_t1600, temple_sacrifice_commitment__performance_only, theater_ratio, 1600, 0.8).
narrative_ontology:measurement(temp_tr_t2000, temple_sacrifice_commitment__performance_only, theater_ratio, 2000, 0.8).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__performance_only, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(temp_be_t400, temple_sacrifice_commitment__performance_only, base_extractiveness, 400, 0.35).
narrative_ontology:measurement(temp_be_t800, temple_sacrifice_commitment__performance_only, base_extractiveness, 800, 0.25).
narrative_ontology:measurement(temp_be_t1200, temple_sacrifice_commitment__performance_only, base_extractiveness, 1200, 0.2).
narrative_ontology:measurement(temp_be_t1600, temple_sacrifice_commitment__performance_only, base_extractiveness, 1600, 0.18).
narrative_ontology:measurement(temp_be_t2000, temple_sacrifice_commitment__performance_only, base_extractiveness, 2000, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__performance_only, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(temp_su_t400, temple_sacrifice_commitment__performance_only, suppression_requirement, 400, 0.25).
narrative_ontology:measurement(temp_su_t800, temple_sacrifice_commitment__performance_only, suppression_requirement, 800, 0.2).
narrative_ontology:measurement(temp_su_t1200, temple_sacrifice_commitment__performance_only, suppression_requirement, 1200, 0.15).
narrative_ontology:measurement(temp_su_t1600, temple_sacrifice_commitment__performance_only, suppression_requirement, 1600, 0.15).
narrative_ontology:measurement(temp_su_t2000, temple_sacrifice_commitment__performance_only, suppression_requirement, 2000, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__performance_only, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the temple_sacrifice_commitment kernel, decomposed per the epsilon-invariance principle from sibling readings (study_as_exercise, hybrid_preparatory, symbolic_transformation) because each reading instantiates a structurally distinct constraint with different beneficiary/victim structures and epsilon profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
