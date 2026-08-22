% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__performance_only_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-22
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__performance_only_reading, []).

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
 *   constraint_id: sacrifice_obligation_kernel__performance_only_reading
 *   human_readable: Sacrifice Obligation — Performance-Only Reading
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   This constraint story instantiates the performance-only reading of the
 *   sacrifice obligation kernel: the biblical command to bring korbanot
 *   remains binding in full, requires physical performance in the Temple with
 *   a qualified priesthood and altar, and study of the laws is preparatory
 *   (hechsher mitzvah) but does not fulfill the mitzvah. The obligation has
 *   been unperformable for 1,900 years since the Temple's destruction. The
 *   reading treats this as structural impossibility, not divine suspension or
 *   transformation. The victim set is the entire Jewish people commanded but
 *   unable to perform; there is no human beneficiary extracting rents — the
 *   extraction is the persistent gap between command and capacity. The
 *   reading is maintained by halakhic authorities who adjudicate the boundary
 *   against sibling readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, 0.82).
domain_priors:suppression_score(sacrifice_obligation_kernel__performance_only_reading, 0.78).
domain_priors:theater_ratio(sacrifice_obligation_kernel__performance_only_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__performance_only_reading, snare).
narrative_ontology:human_readable(sacrifice_obligation_kernel__performance_only_reading, "Sacrifice Obligation — Performance-Only Reading").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__performance_only_reading, "religious/halakhic/commitment_system").

domain_priors:requires_active_enforcement(sacrifice_obligation_kernel__performance_only_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__performance_only_reading, '0d625ab2-78f2-41c0-bfc6-efedef5c0f3c').
narrative_ontology:cs_kernel_codification('0d625ab2-78f2-41c0-bfc6-efedef5c0f3c', fixed_text).
narrative_ontology:cs_authority_grounding('0d625ab2-78f2-41c0-bfc6-efedef5c0f3c', lineage).
narrative_ontology:cs_interpretation_layer_present('0d625ab2-78f2-41c0-bfc6-efedef5c0f3c').
narrative_ontology:cs_reading_relation('0d625ab2-78f2-41c0-bfc6-efedef5c0f3c', sacrifice_obligation_kernel__messianic_suspension_reading, forecloses).
narrative_ontology:cs_reading_relation('0d625ab2-78f2-41c0-bfc6-efedef5c0f3c', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_reading_relation('0d625ab2-78f2-41c0-bfc6-efedef5c0f3c', sacrifice_obligation_kernel__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('0d625ab2-78f2-41c0-bfc6-efedef5c0f3c', foundational, korbanot_require_physical_beis_hamikdash).
narrative_ontology:cs_axiom_status(korbanot_require_physical_beis_hamikdash, holdable).
narrative_ontology:cs_axiom_grounding('0d625ab2-78f2-41c0-bfc6-efedef5c0f3c', korbanot_require_physical_beis_hamikdash, deontological).
narrative_ontology:cs_axiom('0d625ab2-78f2-41c0-bfc6-efedef5c0f3c', foundational, study_is_hechsher_not_kiyum).
narrative_ontology:cs_axiom_status(study_is_hechsher_not_kiyum, holdable).
narrative_ontology:cs_axiom_grounding('0d625ab2-78f2-41c0-bfc6-efedef5c0f3c', study_is_hechsher_not_kiyum, conventional).
narrative_ontology:cs_reference_frame('0d625ab2-78f2-41c0-bfc6-efedef5c0f3c', biblical_command_intact).
narrative_ontology:cs_drift_state('0d625ab2-78f2-41c0-bfc6-efedef5c0f3c', contemporary_post_temple_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0d625ab2-78f2-41c0-bfc6-efedef5c0f3c', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, jewish_people_obligated).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__performance_only_reading, torah_scholars_kodashim).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__performance_only_reading, korbanot_require_physical_beis_hamikdash).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__performance_only_reading, study_is_preparatory_not_fulfillment).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__performance_only_reading, messianic_restoration_is_precondition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Biblically commanded to bring sacrifices in the Temple; unable to perform for 1,900 years due to destruction of the Temple and absence of priesthood/qualified altar. The obligation remains binding in this reading's framework. Exit would require abandoning halakhic commitment or accepting an alternative reading; identity is fused to the obligation's unredeemed state.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, jewish_people_obligated, payer,
    powerless, generational, identity_locked, global).

% Adjudicate the boundary of the obligation, define what counts as fulfillment, transmit the performance-only framework. They maintain the authoritative interpretation that study is preparatory only. Their institutional authority is partially grounded in holding this boundary against the study-as-exercise and messianic-suspension readings.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, halakhic_authorities, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__performance_only_reading, halakhic_authorities, observer).

% Devote careers to study of sacrificial law (Kodashim tractates, Rambam Hilchot Korbanot). In this reading, their study is honored as preparatory and meritorious but does not discharge the obligation. They benefit from institutional recognition, funding, and status as guardians of the unperformable knowledge, while the obligation's impossibility persists.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, torah_scholars_kodashim, beneficiary,
    organized, biographical, constrained, global).

% Hold that the obligation is divinely suspended until messianic restoration, not merely impossible. Their reading would relieve the generational burden of unfulfillable command. They are excluded from the halakhic conversation because their frame treats the obligation as suspended rather than binding-but-impossible — a structural difference this reading forecloses.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, messianic_expectants, excluded,
    moderate, generational, identity_locked, global).

% Analyze sacrificial law as historical priestly literature, cultural archive, and identity-forming narrative. They make no halakhic claim. Their perspective is excluded from the internal halakhic debate but provides the external reference frame for the symbolic-archive reading.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, historical_critical_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the textual, conceptual, and procedural integrity of sacrificial law across 1,900 years of impossibility so that the system can be reactivated if/when Temple conditions are restored. Coordinates collective memory and legal readiness around a fixed kernel (the biblical command) without which the practice would have been lost.
% TRANSFER_FUNCTION: Transfers the burden of an unperformable command from generation to generation: each cohort inherits the obligation, devotes resources to its study, and transmits it undischarged. No agent collects the transfer — the extraction is structural (the gap between command and capacity), not agential.
% ABSENT_VOICES: Jews who would adopt the messianic-suspension or study-as-exercise readings if they were presented as live halakhic options — especially those for whom the generational burden of impossible obligation produces alienation rather than devotion. Also absent: voices from periods when Temple stood (they could perform, not merely study).
% DISAPPEARANCE_RATIONALE: If the performance-only reading vanished overnight, the obligation would either be treated as suspended (messianic reading), fulfilled through study (study-as-exercise reading), or archived as cultural heritage (symbolic-archive reading). The halakhic system would lose its fixed boundary that the obligation remains binding in full despite impossibility — the kernel's claim on the present would be re-negotiated.
% FOUNDING_PROBLEM: After the Temple's destruction (70 CE), how to preserve the sacrificial system's legal and conceptual integrity when its physical preconditions are gone, without declaring the command null or fulfilled by substitute?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Talmudic sages themselves (Yoma 5b, Menachot 110a) who explicitly frame study as 'preparatory' (hechsher mitzvah) not fulfillment. Maimonides (Hilchot Beit HaBechirah 1:1, Hilchot Korbanot throughout) codifies the obligation as binding and unperformable. Modern historians (e.g., Jacob Neusner, Lawrence Schiffman) corroborate from outside the tradition that the rabbinic project was precisely to preserve the system's reactivatability.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__performance_only_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__performance_only_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__performance_only_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(sacrifice_obligation_kernel__performance_only_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__performance_only_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at interval end) because the constraint imposes a binding obligation that cannot be fulfilled, creating a structural deficit that accumulates across generations. The victim (jewish_people_obligated) bears the full weight of an unredeemed command with no exit that preserves halakhic integrity. Suppression (0.78) reflects the active maintenance of the boundary: the halakhic system must continuously exclude alternative readings (messianic suspension, study-as-fulfillment, symbolic archive) that would relieve the burden. Theater ratio is relatively low (0.25) because the study of sacrificial law is genuine intellectual labor with real halakhic function (preserving reactivatability), not mere performance — but the gap between study and fulfillment grows over time, introducing a performative element. Accessibility collapse (0.65) is substantial: once the performance-only frame is accepted, the alternative readings appear as evasions of the command. Resistance (0.45) is moderate: the sibling readings persist as live positions, and alienation from the impossible obligation is a recurring phenomenon.
 *
 * PERSPECTIVAL GAP:
 *   From the halakhic authority seat, the constraint is a necessary preservation of divine command integrity — the system would collapse into adaptationism without this boundary. From the jewish_people_obligated seat, it is a generational burden with no resolution in sight — an obligation that cannot be kept but cannot be released. The engine computes this divergence from the identity_locked exit of the obligated versus the analytical exit of the authorities.
 *
 * DIRECTIONALITY LOGIC:
 *   The jewish_people_obligated are full targets (d → 1.0): identity-locked into the obligation, no exit that preserves halakhic standing, bearing the full structural extraction. Halakhic authorities are near-symmetric but lean beneficiary (d ~ 0.3): they administer the boundary, their institutional authority depends on maintaining it, but they also bear the burden of transmission. Torah scholars of Kodashim are mild beneficiaries (d ~ 0.4): they gain status and resources from guarding the unperformable knowledge, but their work is genuine preservation labor. Messianic expectants are excluded (d undefined by derivation): their reading would relieve the burden, so they are structurally outside the constraint's operation. Historical-critical scholars are analytical observers (d = 0.5): they analyze from outside the commitment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving reactivatability) remains live in this reading's framework — the Temple could be rebuilt, and the preserved system must be ready. But the 1,900-year duration raises mandatrophy pressure: at what point does preservation become its own justification, detached from the prospect of actual performance? The rising theater ratio and extractiveness suggest the constraint is accumulating extractive weight beyond its coordination function. The mandated-happy path (performance upon restoration) remains the declared telos, but the lived reality is study-without-end. This reading does NOT declare mandatrophy_resolved — the tension is structural to the kernel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_without_beneficiary,
    'Can a constraint be extractive (high ε) when no identifiable agent collects the extraction — when the extraction is the structural gap between command and capacity itself?',
    'Compare classification outcomes for this reading versus sibling readings where extraction is agential (e.g., study_as_exercise_reading where scholars benefit from redefining fulfillment). If the engine classifies both as snare/tangled_rope despite different beneficiary structures, the framework validates structural extraction without agential capture.',
    'If structural extraction without beneficiary is not recognized as snare, this reading may misclassify as mountain (no beneficiary/victim in agential sense) or piton (high theater, no capturer). The omega forces the framework to confront whether extraction requires a capturer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_without_beneficiary, conceptual, 'Whether extractiveness requires an agential beneficiary or can be purely structural.').

omega_variable(
    identity_lock_mechanism,
    'Is the jewish_people_obligated''s identity_locked exit driven by theological commitment (the command is divine and cannot be released), communal identity (being Jewish means carrying this unredeemed obligation), or institutional enforcement (halakhic authorities exclude exiters)?',
    'Survey data on why observant Jews remain committed to impossible obligations; historical analysis of apostasy patterns when Temple stood vs. after destruction; comparative study of identity_lock in other impossible-command systems (e.g., Christian eschatological commands).',
    'If theological, the lock is internal and the constraint is self-imposed (mountain-like from inside). If communal/institutional, the lock is external and the constraint is enforced (snare-like). Changes directionality derivation for the obligated seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Mechanism of identity_lock for the obligated population.').

omega_variable(
    committer_structure_ambiguity,
    'This reading (performance_only) forecloses messianic_suspension_reading within a single halakhic framework (the obligation cannot be both binding-and-impossible AND divinely suspended). But it coexists_with study_as_exercise_reading and symbolic_archive_reading as live positions held by different parties. Is the forecloses relation correctly assigned, or does the performance-only reading merely influence (create pressure on) the messianic reading without logically eliminating it?',
    'Analyze whether any halakhic authority has ever held both readings simultaneously in a single coherent framework. If yes, forecloses is wrong — the relation is coexists_with or influences. If no authority has held both, forecloses is correct.',
    'Forecloses implies the kernel cannot resolve the dispute without one reading displacing the other. Coexists_with implies stable pluralism. Influences implies asymmetric pressure (performance-only reading''s dominance makes messianic reading marginal). Changes cs_structure.reading_relations and downstream CS drift analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_ambiguity, conceptual, 'Structural relation between performance-only and messianic-suspension readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__performance_only_reading, 70, 1970).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t70, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 70, 0.05).
narrative_ontology:measurement(sacr_tr_t370, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 370, 0.08).
narrative_ontology:measurement(sacr_tr_t770, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 770, 0.12).
narrative_ontology:measurement(sacr_tr_t1170, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1170, 0.18).
narrative_ontology:measurement(sacr_tr_t1570, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1570, 0.22).
narrative_ontology:measurement(sacr_tr_t1970, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1970, 0.25).

% Extraction over time
narrative_ontology:measurement(sacr_be_t70, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 70, 0.35).
narrative_ontology:measurement(sacr_be_t370, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 370, 0.45).
narrative_ontology:measurement(sacr_be_t770, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 770, 0.55).
narrative_ontology:measurement(sacr_be_t1170, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1170, 0.65).
narrative_ontology:measurement(sacr_be_t1570, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1570, 0.73).
narrative_ontology:measurement(sacr_be_t1970, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1970, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t70, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 70, 0.4).
narrative_ontology:measurement(sacr_su_t370, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 370, 0.5).
narrative_ontology:measurement(sacr_su_t770, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 770, 0.58).
narrative_ontology:measurement(sacr_su_t1170, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1170, 0.65).
narrative_ontology:measurement(sacr_su_t1570, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1570, 0.72).
narrative_ontology:measurement(sacr_su_t1970, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1970, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__performance_only_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% Kernel family: sacrifice_obligation_kernel with four readings. This reading (performance_only) has highest ε (structural impossibility), no human beneficiary, identity_locked victims. messianic_suspension has lower ε (suspension relieves burden), same victim set but different mechanism. study_as_exercise has beneficiary (scholars who redefine fulfillment), lower extraction. symbolic_archive has near-zero ε (no halakhic claim), observer-only stakeholders.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
