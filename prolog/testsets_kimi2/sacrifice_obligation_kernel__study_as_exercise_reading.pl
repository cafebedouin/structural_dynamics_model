% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__study_as_exercise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__study_as_exercise_reading, []).

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
 *   constraint_id: sacrifice_obligation_kernel__study_as_exercise_reading
 *   human_readable: Study-as-Exercise Reading of Sacrifice Obligation
 *   domain: religious_law/halakhic_authority
 *
 * SUMMARY:
 *   This constraint instantiates the study_as_exercise_reading of the
 *   sacrifice_obligation_kernel: the claim that intellectual engagement with
 *   sacrificial law is the legitimate halakhic fulfillment of the biblical
 *   commandment under post-Temple conditions. The kernel is contested among
 *   four live readings (performance-only, messianic suspension,
 *   study-as-exercise, and symbolic archive). This reading asserts near-zero
 *   extraction: the substitution is an authorized transformation, not a
 *   suspension or a cover for extraction. Rabbinic authority is the primary
 *   beneficiary of the interpretive monopoly, but all participants in the
 *   study system are net beneficiaries of the coordination. Structurally,
 *   this is a commitment-system rope: a coordination mechanism rooted in a
 *   fixed textual kernel, mediated by a lineage-based interpretive authority
 *   with a functioning interpretation layer (Talmudic discourse).
 *
 * KEY AGENTS:
 *   - rabbinic_authority (institutional/constrained): Agenda-setter and beneficiary; administers the halakhic substitution and collects interpretive legitimacy.
 *   - yeshiva_students (moderate/identity_locked): Primary beneficiary; their religious identity is fused with study-as-service.
 *   - observant_community (organized/constrained): Diffuse beneficiary; relies on the ruling to avoid a normative crisis.
 *   - temple_mount_activists (organized/constrained): Excluded voice; rejects substitution and demands literal performance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__study_as_exercise_reading, 0.08).
domain_priors:suppression_score(sacrifice_obligation_kernel__study_as_exercise_reading, 0.15).
domain_priors:theater_ratio(sacrifice_obligation_kernel__study_as_exercise_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__study_as_exercise_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__study_as_exercise_reading, "Study-as-Exercise Reading of Sacrifice Obligation").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__study_as_exercise_reading, "religious_law/halakhic_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__study_as_exercise_reading, '62c12dbb-50df-4ddf-bbb2-20e0e6aeabed').
narrative_ontology:cs_kernel_codification('62c12dbb-50df-4ddf-bbb2-20e0e6aeabed', fixed_text).
narrative_ontology:cs_authority_grounding('62c12dbb-50df-4ddf-bbb2-20e0e6aeabed', lineage).
narrative_ontology:cs_interpretation_layer_present('62c12dbb-50df-4ddf-bbb2-20e0e6aeabed').
narrative_ontology:cs_reading_relation('62c12dbb-50df-4ddf-bbb2-20e0e6aeabed', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('62c12dbb-50df-4ddf-bbb2-20e0e6aeabed', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('62c12dbb-50df-4ddf-bbb2-20e0e6aeabed', sacrifice_obligation_kernel__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('62c12dbb-50df-4ddf-bbb2-20e0e6aeabed', foundational, study_fulfills_sacrifice_mitzvah).
narrative_ontology:cs_axiom_status(study_fulfills_sacrifice_mitzvah, holdable).
narrative_ontology:cs_axiom_grounding('62c12dbb-50df-4ddf-bbb2-20e0e6aeabed', study_fulfills_sacrifice_mitzvah, theological).
narrative_ontology:cs_axiom('62c12dbb-50df-4ddf-bbb2-20e0e6aeabed', foundational, rabbinic_substitution_authority).
narrative_ontology:cs_axiom_status(rabbinic_substitution_authority, holdable).
narrative_ontology:cs_axiom_grounding('62c12dbb-50df-4ddf-bbb2-20e0e6aeabed', rabbinic_substitution_authority, deontological).
narrative_ontology:cs_reference_frame('62c12dbb-50df-4ddf-bbb2-20e0e6aeabed', divine_commandment_perpetually_binding).
narrative_ontology:cs_drift_state('62c12dbb-50df-4ddf-bbb2-20e0e6aeabed', post_temple_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('62c12dbb-50df-4ddf-bbb2-20e0e6aeabed', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, yeshiva_students).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, observant_community).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__study_as_exercise_reading, torah_study_as_divine_service).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_oral_law_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the halakhic framework that authorizes study of sacrificial law as fulfillment of the biblical commandment. Derives institutional legitimacy and continuity from this interpretive role. Can exit the formal rabbinate but carries the interpretive tradition with them.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority, beneficiary).

% Engage in daily study of Temple ritual and sacrifice texts as an act of divine service believed to fulfill the mitzvah. Their religious identity is fused with this practice; exit would mean leaving the communal-religious framework entirely.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, yeshiva_students, beneficiary,
    moderate, biographical, identity_locked, global).

% Relies on the rabbinic ruling to maintain a relationship with the sacrifice commandment without a Temple. They benefit from the continuity and the avoidance of the crisis of non-fulfillment. Exit to alternative readings is possible but socially costly.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, observant_community, beneficiary,
    organized, generational, constrained, global).

% Advocate for literal performance of sacrifices on the Temple Mount and reject rabbinic substitutions. They are structurally excluded from the halakhic discourse that validates study-as-fulfillment; their objections are treated as outside the normative framework rather than as internal dissent.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, temple_mount_activists, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves collective continuity with the biblical sacrifice commandment after the loss of the Temple by substituting a widely accessible practice â textual study â for a geographically and institutionally centralized ritual, allowing dispersed communities to participate.
% TRANSFER_FUNCTION: Moves the religious obligation from the physical altar to the study hall, and transfers the authority to validate that transfer to the rabbinic interpretive class, which regulates the content and context of the study.
% ABSENT_VOICES: Temple Mount activist groups and sectarian movements that demand literal performance are excluded from the normative halakhic conversation; secular scholars who treat the sacrificial code as cultural memory rather than binding law are also outside the framework. Their absence means the consensus around study-as-fulfillment is not tested by the parties who would most directly challenge it.
% DISAPPEARANCE_RATIONALE: Without the study-as-exercise framework, the observant community would face a halakhic vacuum regarding the central biblical commandment of sacrifice; the social and liturgical arrangements built around Talmudic study of Temple law would lose their normative anchor, forcing a shift to competing readings (messianic waiting, activist performance, or symbolic identity).
% FOUNDING_PROBLEM: The destruction of the Second Temple removed the physical and institutional site for biblical sacrificial worship, threatening the continuity of a core commandment and the cohesion of the community around it.
% FOUNDING_PROBLEM_CORROBORATION: Talmudic literature (Menahot 110a, Taanit 27b) and Maimonides' codification attest the rabbinic response from within the tradition. External corroboration comes from historians of religion (e.g., Jacob Neusner, Shaye J.D. Cohen) who document the rabbinic substitution strategy as a socio-historical response to the destruction, though they do not affirm its theological validity.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__study_as_exercise_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__study_as_exercise_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__study_as_exercise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, 0.08, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).
:- end_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.08 (near-zero) because the constraint's primary operation is coordination: it solves the collective problem of maintaining a relationship to sacrifice without a Temple. The rabbinic interpretive monopoly carries a small status premium, but there is no identifiable victim set, no active suppression of alternatives within the halakhic frame, and the theater ratio is low (0.15). The measurement series shows mild institutional drift over two millennia as rabbinic institutions solidify, but the trajectory remains well below rope-to-tangled-rope thresholds. Accessibility collapse is moderate (0.25): once inside the rabbinic epistemic framework, alternatives appear intellectually distant, but they are visible from outside. Resistance is low (0.2) because the reading is hegemonic within Orthodox discourse and faces only marginal activist challenge.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic agenda-setter seat, the arrangement is the legitimate unfolding of oral law authority, with study as the natural continuation of sacrifice. From the excluded activist seat, the same arrangement is an illegitimate suppression of the literal commandment. The engine will compute near-beneficiary directionality for rabbinic authority, students, and the observant community (all subsidized by the coordination), while the excluded seat sits outside the constraint's directionality derivation because it is not structurally governed by it. The gap is not about metric disagreement but about membership in the framework itself.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared beneficiaries (rabbinic authority, yeshiva students, observant community) receive low directionality values because the constraint subsidizes their religious continuity and identity maintenance rather than extracting from them. No victim declarations are authored, so no high-directionality target seat is structurally derived. The excluded temple activists would compute as high-directionality if they were inside the constraint, but their exclusion places them outside the derivation chain. The effective extraction (Ï) for all internal seats is therefore negligible.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy (the descent of coordination into inertial performance) because the founding problem â maintaining continuity with sacrifice after the destruction â remains addressed by live study practice. The constraint is not a piton because there is no atrophied function being theatrically maintained; the study practice is functionally central to the community's religious life. It is not a snare because there is no concentrated extraction and no trapped victim class. The low theater ratio and the absence of a sunset clause are consistent with a rope that has successfully coordinated a long-term adaptation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the sacrifice_obligation_kernel. How would its classification change if the performance_only_reading or messianic_suspension_reading became the dominant communal framework?',
    'Historical comparison with communities where performance-only or messianic-waiting frameworks dominate (e.g., Samaritan practice, certain Temple Mount groups).',
    'If performance-only dominated, the rabbinic interpretive monopoly would collapse and the constraint would likely reclassify as a scaffold or broken rope; if messianic suspension dominated, extraction would fall to near zero but so would coordination function, producing a piton-like waiting structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural dependence on this reading remaining dominant within the kernel').

omega_variable(
    interpretive_monopoly_extraction_floor,
    'Does an interpretive monopoly that concentrates legitimacy in rabbinic authority inherently carry a non-zero extractive floor even when no victim is identifiable?',
    'Cross-domain comparison with professional licensing and expertise monopolies to determine whether status-concentration constitutes extraction in the DR framework.',
    'If yes, the rope classification requires that this floor remain below the Boltzmann threshold; if the floor is substantial, the constraint may compute as tangled_rope despite the absence of a declared victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_monopoly_extraction_floor, conceptual, 'Whether monopoly-status without identifiable victims is extractive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__study_as_exercise_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sacr_tr_t25, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 25, 0.07).
narrative_ontology:measurement(sacr_tr_t50, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 50, 0.09).
narrative_ontology:measurement(sacr_tr_t75, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 75, 0.12).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(sacr_be_t25, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 25, 0.03).
narrative_ontology:measurement(sacr_be_t50, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 50, 0.04).
narrative_ontology:measurement(sacr_be_t75, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 75, 0.06).
narrative_ontology:measurement(sacr_be_t100, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 100, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__study_as_exercise_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__study_as_exercise_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the sacrifice_obligation_kernel family. It shares the referent (the biblical commandment regarding sacrifice) with performance_only_reading, messianic_suspension_reading, and symbolic_archive_reading, but each story carries a distinct epsilon and stakeholder structure corresponding to its reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
