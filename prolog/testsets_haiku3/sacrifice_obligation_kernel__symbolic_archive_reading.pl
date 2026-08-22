% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__symbolic_archive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: sacrifice_obligation_kernel__symbolic_archive_reading
 *   human_readable: Sacrifice Law as Symbolic-Archive Reading
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   Under the symbolic-archive reading, sacrifice law is understood as a
 *   preserved corpus of ritual knowledge and cultural memory, not as a
 *   binding obligation to be fulfilled, suspended, or exercised. Study serves
 *   the function of cultural continuity and identity preservation. The
 *   reading makes no halakhic claim about whether sacrifice obligations are
 *   currently binding, divinely suspended, performable, or fulfilable through
 *   study—it steps outside that frame entirely and positions the law as
 *   archival. This reading has very low extractiveness (ε ≈ 0.0) because no
 *   coercive obligation is asserted; participation in study is entirely
 *   voluntary, and no party extracts rents or benefits from enforcement. The
 *   constraint operates as pure cultural coordination.
 *
 * KEY AGENTS:
 *   - Jewish collective memory — the accumulated tradition and identity beneficiary
 *   - Study practitioners — voluntary engaged participants in preservation and learning
 *   - Halakhic authority structure — the rabbinic institutions maintaining textual and interpretive integrity
 *   - Other sacrificial readings — sibling halakhic positions assigning binding force to sacrifice law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__symbolic_archive_reading, 0.0).
domain_priors:suppression_score(sacrifice_obligation_kernel__symbolic_archive_reading, 0.0).
domain_priors:theater_ratio(sacrifice_obligation_kernel__symbolic_archive_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__symbolic_archive_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__symbolic_archive_reading, "Sacrifice Law as Symbolic-Archive Reading").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__symbolic_archive_reading, "religious/halakhic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__symbolic_archive_reading, 'ec760019-5d99-4ba8-a171-c4e07e9cb26f').
narrative_ontology:cs_kernel_codification('ec760019-5d99-4ba8-a171-c4e07e9cb26f', fixed_text).
narrative_ontology:cs_authority_grounding('ec760019-5d99-4ba8-a171-c4e07e9cb26f', lineage).
narrative_ontology:cs_interpretation_layer_present('ec760019-5d99-4ba8-a171-c4e07e9cb26f').
narrative_ontology:cs_reading_relation('ec760019-5d99-4ba8-a171-c4e07e9cb26f', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec760019-5d99-4ba8-a171-c4e07e9cb26f', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec760019-5d99-4ba8-a171-c4e07e9cb26f', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_axiom('ec760019-5d99-4ba8-a171-c4e07e9cb26f', foundational, study_preserves_without_obligating).
narrative_ontology:cs_axiom_status(study_preserves_without_obligating, holdable).
narrative_ontology:cs_axiom_grounding('ec760019-5d99-4ba8-a171-c4e07e9cb26f', study_preserves_without_obligating, conventional).
narrative_ontology:cs_axiom('ec760019-5d99-4ba8-a171-c4e07e9cb26f', secondary, cultural_memory_is_self_justifying).
narrative_ontology:cs_axiom_status(cultural_memory_is_self_justifying, holdable).
narrative_ontology:cs_axiom_grounding('ec760019-5d99-4ba8-a171-c4e07e9cb26f', cultural_memory_is_self_justifying, deontological).
narrative_ontology:cs_reference_frame('ec760019-5d99-4ba8-a171-c4e07e9cb26f', post_temple_cultural_preservation_mandate).
narrative_ontology:cs_drift_state('ec760019-5d99-4ba8-a171-c4e07e9cb26f', contemporary_pluralist_jewish_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ec760019-5d99-4ba8-a171-c4e07e9cb26f', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, study_practitioners).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__symbolic_archive_reading, cultural_continuity_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__symbolic_archive_reading, identity_preservation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The accumulated practice of studying sacrifice law sustains collective Jewish memory and ritual identity across generations. This is not an actor collecting rents, but the emergent property of continuous engagement with foundational texts and practices. Under this reading, the study preserves cultural continuity without imposing any binding obligation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory).

% Engage in voluntary study of sacrifice texts and principles. They choose this practice as cultural preservation and intellectual engagement, not under coercive obligation. The study connects them to ancestral tradition and deepens their understanding of Jewish law and ritual.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, study_practitioners, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__symbolic_archive_reading, study_practitioners, observer).

% The rabbinic institutions and jurisprudential tradition that interpret and transmit sacrifice law. Under this reading, they preserve the texts and maintain their interpretive integrity as cultural inheritance, not as a binding legal requirement awaiting fulfilment or suspension.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, halakhic_authority_structure, agenda_setter,
    institutional, civilizational, analytical, global).

% The competing halakhic readings (messianic suspension, performance-only, study-as-exercise) assert that sacrifice law does carry binding force in some form. Under the symbolic-archive reading, these alternative framings are not present in the room when the archive rationale is defended; their existence as live positions elsewhere is a source of interpretive contention.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, other_sacrificial_readings, excluded,
    institutional, civilizational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the structural knowledge of sacrifice law and its operational logic within Jewish cultural memory. The study and transmission of sacrifice texts ensures that the ritual system remains intelligible and intact as cultural inheritance, even absent performance.
% TRANSFER_FUNCTION: Moves time, attention, and intellectual engagement from practitioners into the study and preservation of sacrifice texts and hermeneutics. No material wealth transfers; the transfer is toward cultural continuity and identity maintenance.
% ABSENT_VOICES: The competing halakhic readings are structurally excluded from this particular framing. Practitioners who hold that sacrifice law carries binding force (either as suspended obligation, performance requirement, or fulfilled through study) would object that the symbolic-archive reading strips the law of its normative character. These objecting readings are live elsewhere in the halakhic tradition but are not present when the archive rationale alone is being articulated.
% DISAPPEARANCE_RATIONALE: If the symbolic-archive reading disappeared (i.e., if the practice of preserving sacrifice law ceased), the arrangement of Jewish religious law would not fundamentally rearrange—other readings would persist, and the obligation would still be articulated through messianic suspension, performance requirement, or exercise-through-study frames. The archive reading is a supplementary cultural practice, not a structural necessity.
% FOUNDING_PROBLEM: After the destruction of the Second Temple and the cessation of sacrifice performance, Jewish tradition needed a framework for transmitting and preserving the complex operational knowledge of the sacrifice system so that the practice would not be forgotten. The symbolic-archive reading solves this by positioning study as the preservation mechanism itself.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic texts (Mishnah, Talmud) explicitly frame the study of sacrificial law as preserving the memory of the system. Post-Temple halakhic authorities including Maimonides and subsequent decisors attest that study serves this preservational function. Historical-critical scholarship on Jewish jurisprudence (outside the tradition's own benefiting institutions) confirms that cultural memory preservation became a central rationale for sacrifice-law study after Temple destruction.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__symbolic_archive_reading, world_unchanged).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__symbolic_archive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__symbolic_archive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is zero because the symbolic-archive reading imposes no binding obligation and generates no victim class. No party is coerced into study; participation is voluntary cultural engagement. Suppression is zero: there are no alternatives being suppressed or made inaccessible. Theater ratio is zero because the function (cultural preservation) and the activity (study) are transparently aligned—there is no performative maintenance of a degraded function. Accessibility collapse is high (0.95) because once the reading is adopted, the recognition that 'this is cultural memory, not binding obligation' makes alternative interpretations (performance-requirement, divine obligation) incompatible within the same framework at the same moment—but this is not coercive collapse; it is the coherence boundary of the reading itself. Resistance is near-zero (0.05) because there is no extractive mechanism to resist; some participants may prefer other readings, but they are not being forced into the symbolic-archive frame.
 *
 * PERSPECTIVAL GAP:
 *   The perspective of a study practitioner and the perspective of the halakhic authority structure both see this arrangement as voluntary, beneficial cultural practice—no seat gap, no divergence in experienced constraint type. The excluded parties (holders of other sacrifice readings) experience a different relationship to the same texts: for them, the texts carry normative force that the archive reading denies. But under this reading alone, there is no internal divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary here is 'jewish_collective_memory,' which is not an agent in the ordinary sense but a non-agent entity representing the cumulative cultural practice. Study practitioners are beneficiaries because they voluntarily engage in an activity they value (learning, cultural connection, identity maintenance). The halakhic authority structure is the agenda_setter because it preserves, transmits, and maintains the interpretive integrity of the texts—but it does not extract from other parties or coerce participation. All directionalities point toward pure coordination: d ≈ 0.0 for beneficiaries (they benefit from voluntary participation), d ≈ 0.5 for the authority structure (it serves and is served by the tradition equally), and no d values above 0.5 because no party bears extractive costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The symbolic-archive reading avoids mandatrophy by design: it makes no binding claim about sacrifice obligation, so there is no mandate to outlive its function. The function (cultural preservation) remains live and perpetually renewable—whenever Jewish practitioners engage in study, the function is simultaneously performed. There is no decoupling between what the reading claims to do (preserve memory) and what it does (sustain study of the texts). Other readings (messianic suspension, performance-only, study-as-exercise) face mandatrophy risk because they assert binding obligations whose original function (performing sacrifice at the Temple) became impossible; the archive reading sidesteps this by reframing the entire problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    archive_vs_obligation_boundary,
    'Is the symbolic-archive reading structurally compatible with the claim that sacrifice law carries binding halakhic force (under any of the sibling readings), or does adoption of the archive frame require the abandonment of halakhic obligation altogether?',
    'Examine halakhic sources and contemporary practice: can a single practitioner hold both the archive reading (for cultural-preservation purposes) and one of the obligation readings (for normative purposes) simultaneously, or are they mutually exclusive mental frames?',
    'If compatible, the archive reading is a supplementary cultural layer that coexists with halakhic obligation. If mutually exclusive, the reading represents a fundamental reframing away from halakhic normativity, and the kernel itself is contested in a deeper sense than surface-level disagreement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archive_vs_obligation_boundary, conceptual, 'Whether the archive reading is compatible with halakhic obligation or requires its negation.').

omega_variable(
    voluntariness_under_cultural_expectation,
    'Is study truly voluntary when cultural expectation (family, community, identity affiliation) strongly incentivizes participation, or does the symbolic-archive reading understate a subtle normative pressure embedded in cultural identity itself?',
    'Ethnographic or sociological study of practitioners'' experience: do they feel free to opt out of sacrifice-law study without social cost, or does opting out carry identity-boundary consequences?',
    'If voluntariness is genuine despite cultural expectation, the constraint remains extractiveness-zero and coordination-pure. If cultural pressure creates a suppression component, extractiveness and suppression would rise slightly, potentially reclassifying the constraint toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(voluntariness_under_cultural_expectation, empirical, 'Whether the declared voluntariness is robust against social-identity pressures.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the symbolic-archive reading logically foreclose the study-as-exercise reading (which asserts that study fulfills the mitzvah), or do the two readings coexist as valid interpretations of the same texts held by different authorities?',
    'Examination of halakhic sources: do authorities endorsing the archive reading explicitly reject the exercise reading, or do both readings appear in the tradition without direct logical contradiction?',
    'If foreclosed, the reading_relation from archive to exercise should be ''forecloses''. If coexistent, the relation should be ''coexists_with''. This determines the network structure of the kernel and the depth of the interpretive contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether the symbolic-archive and study-as-exercise readings logically foreclose each other or coexist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__symbolic_archive_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 500, 0.0).
narrative_ontology:measurement_basis(sacr_tr_t500, observed).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 1000, 0.0).
narrative_ontology:measurement_basis(sacr_tr_t1000, observed).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 1500, 0.0).
narrative_ontology:measurement_basis(sacr_tr_t1500, observed).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 2000, 0.0).
narrative_ontology:measurement_basis(sacr_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 500, 0.0).
narrative_ontology:measurement_basis(sacr_be_t500, observed).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 1000, 0.0).
narrative_ontology:measurement_basis(sacr_be_t1000, observed).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 1500, 0.0).
narrative_ontology:measurement_basis(sacr_be_t1500, observed).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 2000, 0.0).
narrative_ontology:measurement_basis(sacr_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 0, 0.0).
narrative_ontology:measurement_basis(sacr_su_t0, observed).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 500, 0.0).
narrative_ontology:measurement_basis(sacr_su_t500, observed).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 1000, 0.0).
narrative_ontology:measurement_basis(sacr_su_t1000, observed).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 1500, 0.0).
narrative_ontology:measurement_basis(sacr_su_t1500, observed).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 2000, 0.0).
narrative_ontology:measurement_basis(sacr_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__symbolic_archive_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_kernel__symbolic_archive_reading, 0.02).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__performance_only_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the sacrifice_obligation_kernel. The kernel represents the standing commitment to study and understand sacrifice law; four halakhic readings interpret this commitment differently. The symbolic-archive reading treats the obligation as cultural preservation without normative binding force (ε ≈ 0.0, rope). The study-as-exercise reading treats study as fulfilling the mitzvah (ε ≈ 0.3, rope or tangled_rope). The performance-only reading treats study as preparatory but not constitutive (ε ≈ 0.4, tangled_rope). The messianic-suspension reading treats the obligation as divinely suspended but operationally maintained (ε ≈ 0.5, tangled_rope). Each reading has a distinct ε and beneficiary/victim structure; together they model the halakhic contest around sacrifice law. The four constraint stories are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
