% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__study_as_exercise, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: temple_sacrifice_commitment__study_as_exercise
 *   human_readable: Temple Sacrifice Commitment: Study as Exercise Reading
 *   domain: religious_law/commitment_systems
 *
 * SUMMARY:
 *   This constraint models one reading of the contested kernel of the Temple
 *   sacrifice commitment. The reading asserts that intellectual engagement
 *   with sacrifice law—study of the Talmudic and halakhic texts that detail
 *   the Temple system—is itself the performance and occupation of the divine
 *   command, not mere preservation or proxy for a defunct practice. In the
 *   absence of material temple conditions (destroyed 70 CE), this reading
 *   holds that the covenant commitment is fully occupied through textual
 *   mastery and interpretive practice. The beneficiary is the studying
 *   community itself, bound together by their shared commitment to this
 *   understanding. There is no extraction: study produces no transfer from
 *   payers to beneficiaries; no victim class exists. The constraint emerges
 *   as a stable, trans-historical interpretive commitment grounded in
 *   authoritative textual tradition and accepted as natural law by the
 *   community that holds it.
 *
 * KEY AGENTS:
 *   - studying_community: maintains covenant fidelity through textual engagement; identity-locked (to exit the reading would require abandoning core identity as covenant-keeper)
 *   - textual_authority_keepers: institutional holders of the interpretive lineage; analytical position relative to the commitment itself
 *   - alternative_reading_holders: excluded from consensus inside this reading but hold competing framings (performance_only, hybrid_preparatory, symbolic_transformation) that would contest the claim that study alone occupies the commitment
 *   - analytical_observer: position from which to examine the structural properties of the reading and its relationship to the kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__study_as_exercise, 0.0).
domain_priors:suppression_score(temple_sacrifice_commitment__study_as_exercise, 0.0).
domain_priors:theater_ratio(temple_sacrifice_commitment__study_as_exercise, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, extractiveness, 0.0).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__study_as_exercise, mountain).
narrative_ontology:human_readable(temple_sacrifice_commitment__study_as_exercise, "Temple Sacrifice Commitment: Study as Exercise Reading").
narrative_ontology:topic_domain(temple_sacrifice_commitment__study_as_exercise, "religious_law/commitment_systems").

domain_priors:emerges_naturally(temple_sacrifice_commitment__study_as_exercise).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__study_as_exercise, '640853af-4ec2-49f3-8ab3-746ac0a16a86').
narrative_ontology:cs_kernel_codification('640853af-4ec2-49f3-8ab3-746ac0a16a86', fixed_text).
narrative_ontology:cs_authority_grounding('640853af-4ec2-49f3-8ab3-746ac0a16a86', lineage).
narrative_ontology:cs_interpretation_layer_present('640853af-4ec2-49f3-8ab3-746ac0a16a86').
narrative_ontology:cs_reading_relation('640853af-4ec2-49f3-8ab3-746ac0a16a86', temple_sacrifice_commitment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('640853af-4ec2-49f3-8ab3-746ac0a16a86', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_reading_relation('640853af-4ec2-49f3-8ab3-746ac0a16a86', temple_sacrifice_commitment__symbolic_transformation, coexists_with).
narrative_ontology:cs_axiom('640853af-4ec2-49f3-8ab3-746ac0a16a86', foundational, study_occupies_commitment).
narrative_ontology:cs_axiom_status(study_occupies_commitment, holdable).
narrative_ontology:cs_axiom_grounding('640853af-4ec2-49f3-8ab3-746ac0a16a86', study_occupies_commitment, deontological).
narrative_ontology:cs_axiom('640853af-4ec2-49f3-8ab3-746ac0a16a86', foundational, intellectual_engagement_fulfills_obligation).
narrative_ontology:cs_axiom_status(intellectual_engagement_fulfills_obligation, holdable).
narrative_ontology:cs_axiom_grounding('640853af-4ec2-49f3-8ab3-746ac0a16a86', intellectual_engagement_fulfills_obligation, deontological).
narrative_ontology:cs_reference_frame('640853af-4ec2-49f3-8ab3-746ac0a16a86', amoraic_study_occupation_principle).
narrative_ontology:cs_drift_state('640853af-4ec2-49f3-8ab3-746ac0a16a86', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('640853af-4ec2-49f3-8ab3-746ac0a16a86', '2026-08-15T14:22:33Z').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, studying_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the covenant commitment through intellectual engagement with sacrifice law. Study of the halakhic details is understood as itself occupying the divine command in the absence of material temple conditions. The studying community views textual mastery and interpretive practice as the substantive exercise of the commitment, not as substitute or placeholder for the 'real' obligation. Their commitment to this reading is fused with identity as covenant-keepers and bearers of tradition.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, studying_community, beneficiary,
    organized, civilizational, identity_locked, continental).

% Preserve, transmit, and adjudicate the reading of sacrifice law as occupying commitment through study. They maintain the interpretive frameworks (Mishna, Talmud, halakhic codes) that embody this understanding. Their authority grounds itself in lineage from the Amoraic tradition and ongoing interpretive legitimacy of recognized scholars.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, textual_authority_keepers, agenda_setter,
    institutional, civilizational, analytical, continental).

% Hold competing readings of the kernel—performance_only, hybrid_preparatory, symbolic_transformation—that understand sacrifice commitment differently. They would argue that study without material performance leaves the commitment suspended or transformed, not occupied. Their positions are not represented in the studying community's interpretive consensus.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, alternative_reading_holders, excluded,
    organized, civilizational, constrained, continental).

% Examines the structural features of the commitment reading: whether intellectual engagement alone constitutes occupation, how the reading relates to material conditions and textual authority, and what the alternatives would assert differently.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the covenant commitment across the centuries-long hiatus of material sacrifice. The commitment to study sacrifice law as itself the occupying practice solves the problem of how to remain bound to a divine command whose material conditions (the Temple) no longer exist. Study aggregates the community into a coherent keeper-of-tradition role and preserves the textual corpus that grounds all future interpretations.
% TRANSFER_FUNCTION: No transfer occurs under this reading. Study is not extracted from one party and given to another. The studying community receives the intrinsic value of participating in the commitment itself; no asymmetric benefit accrues to an authority holder beyond their role as transmitters and adjudicators of the tradition. Textual authority keepers gain status and interpretive recognition, but this is incidental to the reading, not its structural purpose.
% ABSENT_VOICES: Holders of the performance_only reading—those who hold that sacrifice commitment requires material instantiation and that study is archival preservation, not occupation—are not represented in this reading's committal. Similarly, symbolic_transformation adherents (who hold that the commitment has undergone authorized change into prayer and study) and hybrid_preparatory holders (who view study as maintaining the commitment in suspended, preparatory state) are excluded. Their objections to treating study-alone as full occupation would reshape the entire reading if heard inside the same framework.
% DISAPPEARANCE_RATIONALE: If this reading (that study occupies the commitment) disappeared—if the studying community abandoned the conviction that intellectual engagement IS the exercise of the divine command—the Temple would still be absent and the physical conditions of sacrifice still precluded. Some other reading would have to fill the void (performance_only would require either literal rebuilding or acceptance of permanent breach; hybrid_preparatory would settle into indefinite suspension; symbolic_transformation would embrace the prayer-based alternative). The world doesn't rearrange; the community's understanding of which reading is valid is what shifts. The commitment itself—the kernel—would remain.
% FOUNDING_PROBLEM: After the Second Temple's destruction (70 CE), the community faced an irreducible contradiction: a covenant bound by divine command to the sacrifice system, with no possibility of material performance. The studying community's reading solves this by redefining occupation of the commitment: intellectual mastery of the halakhic system IS the keeping of the covenant. Study preserves both the letter of the obligation and the community's status as bound to it, without requiring the material conditions that are permanently unavailable.
% FOUNDING_PROBLEM_CORROBORATION: The Talmudic (Amoraic era, 3rd-5th centuries) establishment of the principle that study of sacrifice law equals its performance attests the founding problem remained live at the time. Maimonides (12th century) and subsequent halakhic authorities outside the studying community confirm they maintained this framework as a solution to the absence of Temple. Historians of Judaism and comparative analysts of religious commitment systems (scholars in the rabbinic studies and religious ethics fields outside the beneficiary community) attest that the reading emerged as response to the concrete problem of maintaining covenant fidelity across a permanent material rupture.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__study_as_exercise, world_unchanged).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__study_as_exercise, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__study_as_exercise, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__study_as_exercise_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, ExtMetricName, E),
    domain_priors:suppression_score(temple_sacrifice_commitment__study_as_exercise, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(temple_sacrifice_commitment__study_as_exercise),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(temple_sacrifice_commitment__study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score of 0.0 reflects the reading's structural claim: study produces no transfer, no asymmetric benefit capture, no victim class. The studying community is both the agent of practice (studying) and the beneficiary (covenant fidelity), with no separation. Suppression and theater_ratio both score 0.0 because the reading posits that intellectual engagement IS the real commitment, not performance masking another purpose or coercion maintaining the system. Accessibility_collapse at 0.95 reflects that once the reading is accepted—that study IS occupation—the alternatives become nearly inaccessible within the framework. A studying community member cannot simultaneously hold that study equals performance AND that study is merely preserving a defunct practice; the alternatives collapse from that person's perspective. Resistance at 0.15 reflects that some historical and contemporary voices contest the reading (holders of performance_only and other framings), but resistance is not substantial enough to crack the interpretive consensus inside the studying community. The measurement series holds flat across 1,956 years because the reading asserts a timeless principle: study has always occupied the commitment in the absence of material conditions, with no drift in extraction or performativity. The constraint IS what it always was—a reading that study IS the commitment—and the metrics remain stable.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival divergence within this reading, because acceptance of the reading entails a unified understanding: study IS occupation. However, the deep perspectival gap lies between this reading and the sibling readings (performance_only, hybrid_preparatory, symbolic_transformation), which are not represented here but would compute differently from outside this framework. A performance_only reading would see the same historical studying practice as archival preservation (extractive in a sense—the archive collects intellectual effort that does not satisfy the real obligation). A hybrid_preparatory reading would see study as a suspended-state exercise, waiting. Symbolic_transformation would see study as a redesigned obligation. From within THIS reading, these alternatives are foreclosed or coexist in separate communities; the engine computes the per-seat difference in classification across the kernels when all readings are generated.
 *
 * DIRECTIONALITY LOGIC:
 *   The studying community occupies a dual structural position: they are both the agent who performs the commitment (through study) and the beneficiary of that performance (through covenant fidelity). The directionality d is undefined (the constraint has no payer seat; no extraction flow) and resolves to a symmetric position—the community both acts and benefits, with no separation. The identity_locked exit option reflects that exiting this reading (ceasing to understand study as occupation) is equivalent to abandoning one's identity as a covenant-keeper, which is existentially constitutive for members of the community. The textual authority keepers occupy an agenda-setter role—they interpret, preserve, and transmit the framework—but they do not extract from the community; they serve the commitment's transmission. Alternative reading holders are excluded because their frameworks would redefine what it means for the commitment to be occupied, which would displace the study-as-exercise reading from the consensus. There is no seat-divergence on the extracted value (there is none) or on the victim class (there is none); all seats who accept the reading agree on its structure.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy condition arises in this reading. The founding problem (covenant obligation without material conditions) remains live, and the disappearance verdict (world_unchanged) aligns with the founding_problem_status (live). The reading asserts that study occupies the commitment; the commitment's mandate has not outlived its function. If the reading collapsed and members abandoned the conviction that study equals occupation, they would face a genuine mandatrophy question: how to be bound by a commitment whose material conditions are permanently absent and whose intellectual substitute is no longer accepted? But within this reading's own framework, the mandate remains fully operational and the practice (study) remains the legitimate occupation of it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_versus_performance_ontology,
    'Is intellectual engagement with sacrifice law truly the same thing as performance of sacrifice, or is it a substitute or proxy for a commitment that remains structurally unoccupied?',
    'Hermeneutical analysis of the Talmudic texts that establish the equivalence (Menachot 110a and related passages): do they assert ontological identity or functional equivalence? Cross-reading examination of how the studying community itself describes the status (as occupation vs. proxy vs. suspension). Comparative analysis with other religious traditions'' handling of materially impossible commands.',
    'If true identity: the reading is correct and extractiveness remains 0.0. If functional proxy/substitute: the reading becomes a tangled_rope (coordination function of maintaining commitment identity + extraction in the form of intellectual labor without material outcome). If suspension: the constraint becomes hybrid_preparatory (awaiting messianic restoration) rather than study_as_exercise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_versus_performance_ontology, conceptual, 'Whether study and sacrifice are ontologically identical or functionally equivalent under this reading').

omega_variable(
    natural_law_versus_constructed_reading,
    'Is the claim that study occupies the commitment a natural or necessary consequence of Jewish law and theology, or is it an interpretive choice made by the studying community to solve a problem?',
    'Historical analysis of the emergence of this reading in the Amoraic era: was it argued as discovered doctrine or constructed solution? Analysis of whether the same kernel (Temple sacrifice obligation) could sustain the performance_only or hybrid_preparatory readings with equal logical coherence. Examination of minority dissenting voices who rejected the study-as-occupation principle and what they offered instead.',
    'If natural law (emerges necessarily from the kernel): the mountain classification and zero extractiveness are correct. If constructed reading (one choice among several coherent alternatives): an FSM candidate—the reading benefits identifiable agents (the studying community, textual authority keepers) and makes claims about natural fidelity that might not survive scrutiny. The beneficiaries array would then trigger FSM evaluation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_versus_constructed_reading, conceptual, 'Whether the study-as-occupation equivalence is discovered or constructed').

omega_variable(
    identity_lock_internalization,
    'Is the identity-locked exit status of the studying community a structural feature of the commitment system, or a result of internalized identity fusion that would dissolve if the reading''s truth claim were questioned?',
    'Post-questioning trajectory analysis: in contexts where the reading is openly contested (modern movements that challenge whether study alone maintains covenant fidelity), do members who encounter performance_only or symbolic_transformation arguments experience exit as internally conflicted (identity-lock holds) or cognitively liberating (identity-lock dissolves)? Sociological study of communities that have shifted readings.',
    'If structural: the identity_locked exit option correctly describes the commitment system''s binding mechanism. If internalized and dissolved-by-questioning: the suppression metric might rise above 0.0 (the reading maintains itself partly through making exit feel identity-impossible), and the constraint''s type might shift toward tangled_rope or snare depending on whether the reading is actively defended.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_internalization, empirical, 'Whether identity-lock in the studying community is structural or internalized').

omega_variable(
    sibling_reading_foreclosure,
    'Does the study-as-occupation reading logically foreclose the performance_only reading, or do they coexist as competing interpretations of the same kernel?',
    'Logical analysis of the core premises: study_as_exercise asserts that intellectual engagement IS occupation; performance_only asserts that occupation requires material instantiation. Can both be true in the same framework? Is there a higher-order framework (e.g., ''there are multiple valid readings depending on community choice'') that holds both? Or do they genuinely contradict?',
    'If foreclosure: the reading relations in cs_structure should be ''forecloses'' rather than ''coexists_with''. If coexistence: the current ''coexists_with'' assignment is correct and reflects that different communities hold different readings simultaneously. If they coexist under a higher-order framework, that higher-order commitment system would itself be a separate constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether study_as_exercise and performance_only readings logically foreclose each other or coexist').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__study_as_exercise, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t70, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 70, 0.0).
narrative_ontology:measurement_basis(temp_tr_t70, observed).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 500, 0.0).
narrative_ontology:measurement_basis(temp_tr_t500, observed).
narrative_ontology:measurement(temp_tr_t1200, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1200, 0.0).
narrative_ontology:measurement_basis(temp_tr_t1200, observed).
narrative_ontology:measurement(temp_tr_t1800, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1800, 0.0).
narrative_ontology:measurement_basis(temp_tr_t1800, observed).
narrative_ontology:measurement(temp_tr_t2000, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 2000, 0.0).
narrative_ontology:measurement_basis(temp_tr_t2000, observed).
narrative_ontology:measurement(temp_tr_t2026, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 2026, 0.0).
narrative_ontology:measurement_basis(temp_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(temp_be_t70, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 70, 0.0).
narrative_ontology:measurement_basis(temp_be_t70, observed).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 500, 0.0).
narrative_ontology:measurement_basis(temp_be_t500, observed).
narrative_ontology:measurement(temp_be_t1200, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1200, 0.0).
narrative_ontology:measurement_basis(temp_be_t1200, observed).
narrative_ontology:measurement(temp_be_t1800, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1800, 0.0).
narrative_ontology:measurement_basis(temp_be_t1800, observed).
narrative_ontology:measurement(temp_be_t2000, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 2000, 0.0).
narrative_ontology:measurement_basis(temp_be_t2000, observed).
narrative_ontology:measurement(temp_be_t2026, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 2026, 0.0).
narrative_ontology:measurement_basis(temp_be_t2026, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_commitment__study_as_exercise, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__study_as_exercise, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__study_as_exercise, 0.0).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% The temple_sacrifice_commitment kernel is decomposed into four structurally distinct constraint stories, one per reading. The kernel is the obligation to perform Temple sacrifice; the readings diverge on how that obligation is occupied after 70 CE when material performance became impossible. Each reading instantiates a different constraint with different ε values, beneficiary/victim structures, and types. The study_as_exercise reading (this story) asserts zero extractiveness—study IS the occupation of the commitment. The performance_only reading asserts the commitment remains unoccupied or breached (study is archival, not occupation). The hybrid_preparatory reading treats study as maintaining a suspended commitment. The symbolic_transformation reading holds the commitment has undergone authorized structural change. The four stories are linked via network.affects_constraints because they share a kernel and any shift in which reading is accepted in a community would reshape that community's relationship to the others. Each story carries its own ε, its own axioms, and its own cs_structure data for the sibling relations it bears.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
