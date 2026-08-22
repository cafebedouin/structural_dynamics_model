% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__symbolic_archive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Sacrifice Law as Cultural-Historical Archive Reading
 *   domain: religious/commitment_system
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the
 *   sacrifice_obligation_kernel contested across Jewish law traditions. Under
 *   the symbolic_archive_reading, the obligation to study and preserve
 *   knowledge of Temple sacrifice law functions as voluntary cultural
 *   practice that maintains Jewish collective memory and historical
 *   continuity — not as a binding halakhic command. No victim set exists
 *   because no obligation binds and no extraction occurs: participation is
 *   self-selecting, motivated by identity and cultural preservation rather
 *   than coercive obligation. Study preserves the tradition's intellectual
 *   and identity heritage without claiming to occupy or fulfill the original
 *   mitzvah. This reading coexists with three sibling readings
 *   (messianic_suspension, performance_only, study_as_exercise) that each
 *   assert different structural relationships to the sacrifice obligation
 *   itself.
 *
 * KEY AGENTS:
 *   - jewish_collective_memory: the beneficiary — preserved and sustained through voluntary study and transmission of sacrifice law knowledge
 *   - jewish_community_participants: voluntary transmitters and students of sacrifice law — motivated by identity and tradition-bearing rather than obligation compliance
 *   - halakhic_authorities: diverse seats (Orthodox, Conservative, Reconstructionist, etc.) that interpret the obligation's status — some frame it as suspended, some as archive, some as unfulfilled, some as exercise
 *   - analytical_observer: the external reader assessing whether the constraint's operation matches this reading's structure
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
narrative_ontology:human_readable(sacrifice_obligation_kernel__symbolic_archive_reading, "Sacrifice Law as Cultural-Historical Archive Reading").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__symbolic_archive_reading, "religious/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__symbolic_archive_reading, 'a2eec6bc-f7b5-4406-b2e8-d6ec11e17921').
narrative_ontology:cs_kernel_codification('a2eec6bc-f7b5-4406-b2e8-d6ec11e17921', fixed_text).
narrative_ontology:cs_authority_grounding('a2eec6bc-f7b5-4406-b2e8-d6ec11e17921', lineage).
narrative_ontology:cs_interpretation_layer_present('a2eec6bc-f7b5-4406-b2e8-d6ec11e17921').
narrative_ontology:cs_reading_relation('a2eec6bc-f7b5-4406-b2e8-d6ec11e17921', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('a2eec6bc-f7b5-4406-b2e8-d6ec11e17921', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('a2eec6bc-f7b5-4406-b2e8-d6ec11e17921', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_axiom('a2eec6bc-f7b5-4406-b2e8-d6ec11e17921', foundational, obligation_transformed_not_suspended).
narrative_ontology:cs_axiom_status(obligation_transformed_not_suspended, holdable).
narrative_ontology:cs_axiom_grounding('a2eec6bc-f7b5-4406-b2e8-d6ec11e17921', obligation_transformed_not_suspended, conventional).
narrative_ontology:cs_axiom('a2eec6bc-f7b5-4406-b2e8-d6ec11e17921', foundational, study_preserves_not_fulfills).
narrative_ontology:cs_axiom_status(study_preserves_not_fulfills, holdable).
narrative_ontology:cs_axiom_grounding('a2eec6bc-f7b5-4406-b2e8-d6ec11e17921', study_preserves_not_fulfills, instrumental).
narrative_ontology:cs_reference_frame('a2eec6bc-f7b5-4406-b2e8-d6ec11e17921', post_temple_destruction_knowledge_archive).
narrative_ontology:cs_drift_state('a2eec6bc-f7b5-4406-b2e8-d6ec11e17921', contemporary_jewish_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a2eec6bc-f7b5-4406-b2e8-d6ec11e17921', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_community_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The abstract good of preserved collective knowledge and cultural continuity. Sustained through voluntary transmission and study of sacrifice law. Non-agent entity; included for completeness in documenting the archive reading's beneficiary structure.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory).

% Individuals who engage in voluntary study and transmission of sacrifice law as cultural-identity practice. They benefit from participation in tradition-bearing without bearing costs of mandatory obligation. They can cease participation without consequences; continuance is motivated by identity affiliation and cultural valuation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_community_participants, beneficiary,
    moderate, biographical, mobile, global).

% Rabbinic and scholarly authorities across different Jewish traditions that interpret the halakhic status of the sacrifice obligation. Under the archive reading, they frame study as voluntary cultural preservation rather than binding obligation, and validate the practice's legitimacy without coercive enforcement. They do not extract from or punish non-participants because the reading posits no obligation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, halakhic_authorities, observer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__symbolic_archive_reading, halakhic_authorities, agenda_setter).

% Academic analysis of the constraint's structure and operation. Evaluates whether the archive reading's zero-extraction claim holds empirically and whether voluntary participation in identity preservation genuinely functions as claimed.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, external_observer, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves and transmits Jewish cultural-historical knowledge of Temple sacrifice law and practice, maintaining continuity of legal and liturgical tradition across generations. Solves the collective-action problem of knowledge preservation without institutional enforcement: individual study and teaching sustain the archive that would otherwise degrade.
% TRANSFER_FUNCTION: Transfers knowledge, cultural identity, and spiritual continuity from older to younger generations through voluntary study, teaching, and commemorative practice. No material extraction occurs; the 'transfer' is immaterial — tradition-bearing itself.
% ABSENT_VOICES: Adherents of the sibling readings (messianic_suspension, performance_only, study_as_exercise) would object that this reading misframes the obligation's status and evacuates the mitzvah of its binding force. They would argue that study under the archive reading fails to occupy or prepare for the obligation as it actually stands. These dissenting halakhic voices are not systematically excluded; they coexist as live positions within Jewish tradition. The archive reading does not silence them — it competes with them in ongoing halakhic discourse.
% DISAPPEARANCE_RATIONALE: If the practice of studying and preserving sacrifice law disappeared, Jewish collective knowledge of an entire dimension of biblical law and pre-Temple practice would degrade; liturgical continuity with historical tradition would weaken; and a principal site of Jewish identity-bearing practice would vanish. The Jewish community would need to reconstruct knowledge and tradition from scratch or adapt without that continuity. The constraint's disappearance would reorganize Jewish cultural memory and transmission.
% FOUNDING_PROBLEM: Preservation of Jewish legal and liturgical knowledge of Temple sacrifice after the Second Temple's destruction made physical performance of sacrifice impossible. The founding problem was maintaining intellectual and cultural continuity with a central mitzvah that could no longer be enacted.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary Jewish communities across multiple traditions (Orthodox, Conservative, Reconstructionist, Reform) continue to study and teach sacrifice law; Jewish scholarship and educational curricula maintain the archive; and halakhic authorities across streams validate the practice as maintaining cultural continuity. Academic historians and Jewish-studies scholars outside the halakhic community confirm that the knowledge-preservation function is empirically active and valued. The founding problem remains live: the challenge of maintaining continuity with an obligation impossible to physically perform persists.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__symbolic_archive_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__symbolic_archive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   Extractiveness is 0.0 because the symbolic_archive reading asserts no binding obligation exists to be violated and no party coerces participation. Study is framed as voluntary cultural practice, not mandatory mitzvah performance. Suppression is 0.0 because no enforcement machinery maintains the constraint — it persists through voluntary transmission of cultural value. Theater_ratio is 0.0 because the practice is performatively minimal: study occurs as genuine intellectual engagement, not theatrical maintenance of an empty form. Accessibility_collapse is 0.95 because once understood through this reading's frame, the alternative interpretations (suspension, unfulfilled, exercise) become fully transparent as competing halakhic claims — no participant is unaware they are choosing among readings. Resistance is 0.05 (near-zero) because the constraint's operation depends on no coercive opposition; what little resistance exists comes from sibling-reading adherents who dispute the archive framing itself. The flat measurement series (all zeros across extractiveness, theater, suppression) reflects this reading's core claim: zero extraction, zero enforcement, zero performative cover.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap exists BETWEEN this reading and the sibling readings, not within this reading's own structure. From the messianic_suspension perspective, an obligation remains binding (though unfulfillable) and study has a quasi-legal status. From the performance_only perspective, study is preparatory work toward an obligation it does not occupy. From the study_as_exercise perspective, study IS the fulfillment of the obligation. From this symbolic_archive perspective, no obligation exists at all and study is purely cultural preservation. The engine should compute a single type for each reading independently; type divergence BETWEEN seatings of different readings is the measurement the corpus takes. Within the archive reading itself, there is no divergence: participants and authorities aligned with this reading experience the constraint identically — as voluntary, non-binding, identity-bearing, zero-extraction preservation work.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading has no payer seats — no agent bears extraction costs. The beneficiary is jewish_collective_memory itself (a non-agent entity preserving continuity), and participants are voluntary. Because extractiveness is zero, directionality does not operate in the standard sense: there are no target agents (d→1.0) and no victim seats. Participants sit near d=0.0 (full beneficiary) because they gain identity and cultural coherence from the practice with no cost imposed. If this reading is correct, the constraint should compute as pure rope (coordination without extraction) across all seated participants.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits zero mandatrophy risk under the archive reading because the founding problem (preserving knowledge of sacrifice law and Jewish continuity) remains genuinely live and the practice directly addresses it through voluntary transmission. The constraint does not persist through inertia or theatrical maintenance — it persists through active cultural valuation and transmission. If the constraint disappeared, Jewish collective knowledge of sacrifice law would degrade and cultural continuity would be disrupted (world_rearranges verdict). No gap exists between the founding function and the operating function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    archive_vs_obligation_kernel_contest,
    'Is the sacrifice obligation genuinely suspended (messianic_suspension_reading) until restoration, or has the meaning-bearing function of the mitzvah transformed entirely into historical-cultural preservation (symbolic_archive_reading)?',
    'Textual analysis of contemporary halakhic authority positions on the status of sacrifice study post-Temple; comparison of how different streams of Judaism (Orthodox, Conservative, Reform) frame the halakhic status of the obligation itself (suspended vs. transformed vs. superseded).',
    'If the obligation is genuinely suspended, study maintains operative readiness and some dimension of ''occupying'' an unfulfillable mitzvah persists; if transformed into archive, no obligation exists to violate and the constraint is pure voluntary coordination without extraction. Classification shifts from contested tension to settled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(archive_vs_obligation_kernel_contest, conceptual, 'Whether sacrifice obligation remains binding (suspended) or has been reframed as cultural-historical practice.').

omega_variable(
    voluntary_vs_coercive_preservation,
    'Is Jewish collective identity genuinely preserved through voluntary study and cultural transmission, or does communal social pressure to maintain continuity function as a coercive mechanism that masks extraction from those carrying the identity work?',
    'Ethnographic study of Jewish communities: do participants experience sacrifice study and teaching as voluntary identity work, or as obligatory continuity burden? Comparative analysis with other traditions'' historical-archive functions (e.g., classical Latin study in Western education) to assess whether voluntary participation in identity preservation is distinguishable from identity-based coercion.',
    'If truly voluntary, extractiveness remains 0.0 and the constraint is pure rope—coordination without coercion. If communal pressure operates as internalized suppression, extractiveness rises and the identity group becomes partially victim. This omega addresses the ''identity-locked internalized suppression'' risk flagged in interpersonal constraint guidance (2026-06-11).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_coercive_preservation, empirical, 'Whether identity preservation operates as coercive cultural pressure or genuine voluntary coordination.').

omega_variable(
    reading_dependent_constraint_identity,
    'Does the ε-invariance principle require decomposition here: is ''sacrifice law as archive'' a different constraint from ''sacrifice law as suspended obligation''?',
    'Apply the ε-invariance test: does measuring the constraint through the archive reading (ε≈0) versus the obligation reading (ε>0, contested) amount to measuring two different constraints via different observables? The prompt indicates they have structurally distinct ε values (archive=zero extractiveness; other readings=non-zero). By DP-001, if ε changes when the observable changes, the constraints are different.',
    'If the readings measure different constraints (which the prompt structure suggests), this story is correct as a single-reading decomposition and should link to siblings via network.affects_constraints. If the readings are the same constraint viewed from different angles, the ε-invariance principle is violated and the JSON should decompose further or merge readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_dependent_constraint_identity, conceptual, 'Whether the symbolic-archive reading and the obligation readings instantiate the same constraint (ε-variant measurement) or different constraints (ε-invariant decomposition).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__symbolic_archive_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(sacr_tr_t5, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 5, 0.0).
narrative_ontology:measurement(sacr_tr_t10, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 10, 0.0).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(sacr_be_t5, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 5, 0.0).
narrative_ontology:measurement(sacr_be_t10, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 10, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 0, 0.0).
narrative_ontology:measurement(sacr_su_t5, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 5, 0.0).
narrative_ontology:measurement(sacr_su_t10, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 10, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__symbolic_archive_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_kernel__symbolic_archive_reading, 0.05).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__study_as_exercise_reading).

% DUAL FORMULATION NOTE:
% sacrifice_obligation_kernel decomposes into four structurally distinct constraint stories, one per reading. The symbolic_archive_reading (this story) frames sacrifice law as voluntary cultural-historical preservation with zero extractiveness and zero binding obligation. The sibling readings (messianic_suspension, performance_only, study_as_exercise) each assert different structural positions for the obligation itself and generate non-zero ε values. These are not the same constraint viewed from different angles; by DP-001 ε-invariance, they are distinct constraints linked through the contested kernel. Each reading instantiates the same kernel (sacrifice obligation in Jewish law) but produces different constraints by declaring different functions (suspension, preparatory work, genuine exercise, archive preservation) and thus different victim/beneficiary structures. All four stories share the kernel_id 'sacrifice_obligation_kernel' and list each other in network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
