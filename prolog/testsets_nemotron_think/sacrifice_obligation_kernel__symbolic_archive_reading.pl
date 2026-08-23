% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__symbolic_archive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__symbolic_archive_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: sacrifice_obligation_kernel__symbolic_archive_reading
 *   human_readable: Sacrifice Law as Cultural-Historical Archive (Symbolic Archive Reading)
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   This constraint story models the symbolic_archive_reading of the
 *   sacrifice_obligation_kernel — the position that sacrifice law (korbanot)
 *   functions today solely as a cultural-historical archive. Study of Mishnah
 *   Kodashim, Talmud Zevachim, and Maimonides' Hilkhot Avodah is voluntary
 *   cultural practice preserving Jewish collective memory and identity. No
 *   halakhic obligation to study exists; no violation occurs from non-study;
 *   no extraction or coercion is present. The constraint coordinates identity
 *   preservation across a dispersed, voluntary participant base — a rope by
 *   structural metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__symbolic_archive_reading, 0.0).
domain_priors:suppression_score(sacrifice_obligation_kernel__symbolic_archive_reading, 0.0).
domain_priors:theater_ratio(sacrifice_obligation_kernel__symbolic_archive_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, resistance, 0.0).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__symbolic_archive_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__symbolic_archive_reading, "Sacrifice Law as Cultural-Historical Archive (Symbolic Archive Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__symbolic_archive_reading, "religious/halakhic/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__symbolic_archive_reading, '82f74f81-3e43-4685-acb3-ceb337b622c5').
narrative_ontology:cs_kernel_codification('82f74f81-3e43-4685-acb3-ceb337b622c5', distributed).
narrative_ontology:cs_authority_grounding('82f74f81-3e43-4685-acb3-ceb337b622c5', practice).
narrative_ontology:cs_reading_relation('82f74f81-3e43-4685-acb3-ceb337b622c5', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('82f74f81-3e43-4685-acb3-ceb337b622c5', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('82f74f81-3e43-4685-acb3-ceb337b622c5', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_axiom('82f74f81-3e43-4685-acb3-ceb337b622c5', foundational, sacrifice_law_is_cultural_archive_only).
narrative_ontology:cs_axiom_status(sacrifice_law_is_cultural_archive_only, holdable).
narrative_ontology:cs_axiom_grounding('82f74f81-3e43-4685-acb3-ceb337b622c5', sacrifice_law_is_cultural_archive_only, conventional).
narrative_ontology:cs_axiom('82f74f81-3e43-4685-acb3-ceb337b622c5', foundational, no_halakhic_obligation_to_study_sacrifice_law).
narrative_ontology:cs_axiom_status(no_halakhic_obligation_to_study_sacrifice_law, holdable).
narrative_ontology:cs_axiom_grounding('82f74f81-3e43-4685-acb3-ceb337b622c5', no_halakhic_obligation_to_study_sacrifice_law, conventional).
narrative_ontology:cs_reference_frame('82f74f81-3e43-4685-acb3-ceb337b622c5', post_temple_rabbinic_continuity).
narrative_ontology:cs_drift_state('82f74f81-3e43-4685-acb3-ceb337b622c5', contemporary_voluntary_engagement, gap(stable, minor, true)).
narrative_ontology:cs_created_at('82f74f81-3e43-4685-acb3-ceb337b622c5', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, talmudic_scholars).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__symbolic_archive_reading, cultural_continuity_through_voluntary_study).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__symbolic_archive_reading, identity_preservation_without_halakhic_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Voluntarily engage with sacrifice law texts (Mishnah Kodashim, Talmud Zevachim, Maimonides Hilkhot Avodah) as cultural-historical study. No halakhic obligation to study; participation is driven by identity continuity, intellectual curiosity, or communal tradition. Can cease engagement at any costless moment. Gains: connection to historical heritage, communal cohesion, intellectual framework.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_participants, beneficiary,
    moderate, generational, mobile, global).

% Transmit, interpret, and teach sacrifice law as academic and cultural curriculum in yeshivas and universities. Their authority is scholarly, not coercive — no power to compel study or penalize non-participation. Gains: professional standing, intellectual vocation, role in cultural transmission.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, talmudic_scholars, agenda_setter,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__symbolic_archive_reading, talmudic_scholars, beneficiary).

% Hold the messianic_suspension_reading: believe sacrifice obligation is divinely suspended until Temple restoration, and study maintains operational readiness. They would object to the symbolic archive reading's denial of halakhic force, but are not participants in the voluntary study framework — their framework is a different reading of the same kernel.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, messianic_expectation_holders, excluded,
    organized, civilizational, identity_locked, global).

% Hold the performance_only_reading: believe sacrifice obligation requires physical performance on the Temple Mount, and study is merely preparatory. They would object to the symbolic archive reading's claim that study has no halakhic dimension. Their position is maintained through institutional structures (Temple Institute, certain yeshivas) that make exit socially costly.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, performance_only_adherents, excluded,
    organized, generational, constrained, global).

% Hold the study_as_exercise_reading: believe intellectual engagement with sacrifice law constitutes genuine fulfillment of the mitzvah. They would object to the symbolic archive reading's denial that study occupies the obligation. Their position is embedded in rabbinic curricula where this reading is normative; exit risks professional and communal marginalization.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, study_as_exercise_proponents, excluded,
    organized, biographical, constrained, global).

% Analyze sacrifice law as historical phenomenon and cultural archive from outside the halakhic framework. Provide corroboration for the symbolic archive reading's descriptive claims without endorsing any halakhic stance.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, secular_jewish_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves Jewish collective memory and identity continuity through voluntary, non-coercive engagement with sacrifice law texts as a shared cultural-historical archive.
% TRANSFER_FUNCTION: Transmits cultural knowledge, historical consciousness, and identity markers from the post-Temple rabbinic tradition to contemporary participants — no material resources, status, or halakhic standing are transferred.
% ABSENT_VOICES: Holders of the three sibling readings (messianic_suspension, performance_only, study_as_exercise) who maintain that sacrifice law carries binding halakhic force — whether suspended, awaiting performance, or fulfilled through study. They are excluded from this reading's voluntary framework because their frameworks treat the obligation as live, not archival.
% DISAPPEARANCE_RATIONALE: If the symbolic archive reading vanished overnight, the texts would remain available, other readings would continue unchanged, and voluntary participants could simply stop studying or switch to other identity practices. No material arrangements depend on this reading's existence — it is a non-coercive interpretive layer.
% FOUNDING_PROBLEM: After the Second Temple's destruction (70 CE), how to preserve Jewish identity and continuity when the central cultic obligation (sacrifice) became physically impossible to perform.
% FOUNDING_PROBLEM_CORROBORATION: Historians of religion (e.g., Jacob Neusner, Shaye Cohen), sociologists of Jewish identity (e.g., Steven Cohen, Ari Kelman), and scholars of rabbinic literature outside the halakhic framework corroborate that the symbolic archive function emerged as a response to the Temple's destruction and persists as a live identity-preservation mechanism. The reading's own beneficiaries (traditional scholars) also attest the problem is live but from within the framework.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__symbolic_archive_reading, world_unchanged).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__symbolic_archive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__symbolic_archive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, 0.0, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).
:- end_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.0 because no binding obligation exists to be violated — participants cannot be extracted from. Suppression is 0.0 because alternatives (other identity practices, secular engagement, other readings) remain fully accessible and unpenalized. Theater ratio is low (0.1) — some performative ritual study exists but it is not the dominant mode. Accessibility collapse is minimal (0.15) — the archive is open, exits are costless. Resistance is 0.0 — no one resists a voluntary practice. The claimed_type is rope: genuine coordination of collective memory without extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the participant seat, this is a benign cultural practice (rope). From the sibling-reading seats, this reading's existence may feel like erosion of halakhic seriousness — but that is a cross-reading tension, not internal extraction. The engine computes per-seat types from the structural data; the symbolic archive reading itself has no internal payer seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish collective participants are beneficiaries (d ≈ 0.0) — they voluntarily engage and gain identity continuity. Talmudic scholars are agenda_setters with beneficiary overlap — they curate the archive and gain professional standing. The three excluded sibling-reading groups are not participants in this constraint; they operate under different readings of the same kernel. Their exclusion is not suppression by this reading — it is the kernel's contest structure. Secular historians are analytical observers providing external corroboration.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (identity preservation post-Temple) remains live. The arrangement has not outlived its function — it continues to serve voluntary identity coordination. No mandatrophy is present because the constraint never claimed halakhic authority to begin with; it is explicitly non-halakhic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the symbolic_archive_reading a genuinely distinct constraint from its siblings, or merely a descriptive label for non-participation in the other readings?',
    'Test whether participants in this reading exhibit coordinated behavior (shared curricula, communal study cycles, identity markers) that would not exist if they simply ''did not hold'' the other readings. Ethnographic study of voluntary study communities.',
    'If no coordinated behavior exists, the constraint dissolves into ''absence of other readings'' — not a rope but a null reading. The engine would then classify the kernel''s other readings without this as a separate constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, empirical, 'Whether the symbolic archive reading has independent coordination structure or is merely negative definition.').

omega_variable(
    cultural_vs_halakhic_boundary,
    'Where does the cultural-historical archive function shade into implicit halakhic normativity (e.g., when voluntary study becomes expected for communal membership)?',
    'Survey communal expectations: is non-study of sacrifice law noticed, questioned, or treated as identity-deficit in communities that nominally hold the symbolic archive reading?',
    'If implicit normativity exists, extractiveness > 0 and suppression > 0 emerge — the reading would reclassify toward tangled_rope. The zero-extraction claim holds only where the cultural/halakhic boundary is strictly maintained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cultural_vs_halakhic_boundary, conceptual, 'Whether the symbolic archive reading''s zero-extraction claim survives contact with communal enforcement of identity norms.').

omega_variable(
    reading_relations_stability,
    'Do the declared reading_relations (all coexists_with) hold under framework unification pressure — e.g., if a single authority attempted to impose one reading?',
    'Historical analysis: have any rabbinic authorities attempted to foreclose the symbolic archive reading in favor of study_as_exercise or messianic_suspension? What was the structural outcome?',
    'If foreclosure attempts exist and succeed structurally, some relations should be ''forecloses'' or ''influences'' rather than ''coexists_with''. The current declaration assumes stable pluralism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relations_stability, empirical, 'Whether the coexistence of readings is structurally stable or contingent on absence of unifying authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__symbolic_archive_reading, 70, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t70, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 70, 0.05).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 500, 0.08).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(sacr_tr_t1800, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(sacr_tr_t1950, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(sacr_tr_t2024, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t70, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 70, 0.0).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 500, 0.0).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 1000, 0.0).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 1500, 0.0).
narrative_ontology:measurement(sacr_be_t1800, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 1800, 0.0).
narrative_ontology:measurement(sacr_be_t1950, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 1950, 0.0).
narrative_ontology:measurement(sacr_be_t2024, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 2024, 0.0).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__symbolic_archive_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__symbolic_archive_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_kernel__symbolic_archive_reading, 0.08).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__study_as_exercise_reading).

% DUAL FORMULATION NOTE:
% This constraint and its three siblings form the sacrifice_obligation_kernel family. Each reading instantiates a different constraint with different ε, beneficiary/victim structure, and type. The kernel is the contested halakhic status of sacrifice obligation post-Temple. This reading (symbolic_archive) has ε=0, no victims, beneficiaries=voluntary participants. The sibling readings have ε>0 (binding obligation creates extraction from non-compliers), victims (those who fail the obligation), and different coordination functions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
