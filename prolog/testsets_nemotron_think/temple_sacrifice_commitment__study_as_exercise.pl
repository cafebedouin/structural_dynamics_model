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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Study of Sacrifice Law as Performance of the Divine Command
 *   domain: religious_law/halakhic_tradition/commitment_system
 *
 * SUMMARY:
 *   This constraint story captures one reading of the contested kernel
 *   'temple_sacrifice_commitment': the position that intellectual engagement
 *   with sacrificial law (talmud torah of kodashim) IS the performance of the
 *   divine command when the Temple is destroyed. The reading originates in
 *   Talmudic sources (Menachot 110a: 'whoever engages in the study of the
 *   laws of sacrifice is as if he offered a sacrifice') and is codified by
 *   Maimonides and subsequent halakhic tradition. It claims zero
 *   extractiveness — the studying community experiences the practice as
 *   intrinsically valuable fulfillment, not as a burden imposed by authority.
 *   The constraint emerges naturally from the internal logic of the covenant:
 *   a perpetual commandment cannot be nullified by historical circumstance,
 *   so its form must adapt while its binding force remains.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__study_as_exercise, 0.02).
domain_priors:suppression_score(temple_sacrifice_commitment__study_as_exercise, 0.03).
domain_priors:theater_ratio(temple_sacrifice_commitment__study_as_exercise, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, extractiveness, 0.02).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__study_as_exercise, mountain).
narrative_ontology:human_readable(temple_sacrifice_commitment__study_as_exercise, "Study of Sacrifice Law as Performance of the Divine Command").
narrative_ontology:topic_domain(temple_sacrifice_commitment__study_as_exercise, "religious_law/halakhic_tradition/commitment_system").

domain_priors:emerges_naturally(temple_sacrifice_commitment__study_as_exercise).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__study_as_exercise, '68f276a8-a9ce-4e73-a04d-35da5aca19cf').
narrative_ontology:cs_kernel_codification('68f276a8-a9ce-4e73-a04d-35da5aca19cf', fixed_text).
narrative_ontology:cs_authority_grounding('68f276a8-a9ce-4e73-a04d-35da5aca19cf', lineage).
narrative_ontology:cs_interpretation_layer_present('68f276a8-a9ce-4e73-a04d-35da5aca19cf').
narrative_ontology:cs_reading_relation('68f276a8-a9ce-4e73-a04d-35da5aca19cf', temple_sacrifice_commitment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('68f276a8-a9ce-4e73-a04d-35da5aca19cf', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_reading_relation('68f276a8-a9ce-4e73-a04d-35da5aca19cf', temple_sacrifice_commitment__symbolic_transformation, influences).
narrative_ontology:cs_axiom('68f276a8-a9ce-4e73-a04d-35da5aca19cf', foundational, study_occupies_commitment_structural_place).
narrative_ontology:cs_axiom_status(study_occupies_commitment_structural_place, holdable).
narrative_ontology:cs_axiom_grounding('68f276a8-a9ce-4e73-a04d-35da5aca19cf', study_occupies_commitment_structural_place, deontological).
narrative_ontology:cs_axiom('68f276a8-a9ce-4e73-a04d-35da5aca19cf', foundational, perpetual_commandment_cannot_be_nullified_by_history).
narrative_ontology:cs_axiom_status(perpetual_commandment_cannot_be_nullified_by_history, holdable).
narrative_ontology:cs_axiom_grounding('68f276a8-a9ce-4e73-a04d-35da5aca19cf', perpetual_commandment_cannot_be_nullified_by_history, deontological).
narrative_ontology:cs_reference_frame('68f276a8-a9ce-4e73-a04d-35da5aca19cf', sinai_covenantal_perpetuity).
narrative_ontology:cs_drift_state('68f276a8-a9ce-4e73-a04d-35da5aca19cf', post_70ce_exilic_continuity, gap(stable, minor, true)).
narrative_ontology:cs_created_at('68f276a8-a9ce-4e73-a04d-35da5aca19cf', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, studying_community).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__study_as_exercise, talmudic_principle_study_as_sacrifice).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__study_as_exercise, covenant_fidelity_through_intellectual_engagement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities that maintain covenant fidelity through sustained intellectual engagement with sacrificial law. Their identity is constituted by this practice — exit would mean dissolution of the communal self-understanding as bearers of the priestly covenant. They experience the study not as burden but as the very form of their covenantal life.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, studying_community, beneficiary,
    organized, generational, identity_locked, global).

% Rabbinic authorities who transmit, interpret, and authorize the reading that study constitutes performance. They maintain the interpretive tradition (Talmud Menachot 110a, Maimonides Hilkhot Temidin u'Musafin) that grounds this equivalence. Their authority derives from the lineage of transmission; they cannot unilaterally revoke the reading without breaking the chain.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, halakhic_authorities, agenda_setter,
    institutional, civilizational, constrained, global).

% Activists who advocate for immediate material restoration of sacrificial worship on the Temple Mount. They reject the study-as-performance reading as quietism that delays the actual obligation. Their voice is excluded from the halakhic consensus that treats study as fulfillment.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, temple_mount_activists, excluded,
    moderate, biographical, mobile, regional).

% Academic observers who analyze the constraint as a historical-legal phenomenon. They see the reading as a brilliant survival mechanism that preserved priestly identity through exile, but do not participate in the commitment it sustains.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, secular_scholars_of_halakha, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the priestly covenant and communal identity across two millennia without a Temple, by transmuting the material obligation of sacrifice into an intellectual practice that occupies the same structural place in the commitment system.
% TRANSFER_FUNCTION: No material transfer occurs. The arrangement moves cognitive attention and communal reverence from the absent altar to the text of the law; the 'payment' is study itself, which the studying community experiences as intrinsically valuable, not as a cost extracted from them.
% ABSENT_VOICES: Temple Mount activists and restorationist groups who would object that study without performance is evasion, not fulfillment. They are excluded because the halakhic consensus treats the Temple's destruction as a divine suspension of material performance, making the intellectual mode the only currently authorized path.
% DISAPPEARANCE_RATIONALE: If the equivalence between study and sacrifice were rejected overnight, the priestly covenant would lose its operative form. Kohanim would lose their distinctive ongoing obligation; the textual corpus of sacrificial law would become purely archival; the communal identity structured around 'avodah she'balev' (service of the heart) would dissolve or require a new foundation.
% FOUNDING_PROBLEM: How to maintain the binding force of the sacrificial commandments — which the Torah presents as perpetual — when the Temple, their necessary material locus, has been destroyed by Roman power and not yet restored by divine will.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Talmud itself (Menachot 110a, Ta'anit 27b) which records the sages' explicit anxiety about this exact question. Maimonides (Guide III:32, Hilkhot Temidin u'Musafin) treats it as a live structural problem. Contemporary halakhic authorities across Orthodox, Conservative, and Reform movements all engage the question, though they diverge on the answer. No beneficiary group claims the problem is solved; the Temple remains unrebuilt.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__study_as_exercise, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__study_as_exercise, 0.02, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is near-zero (0.02) because no resources are extracted from participants — study is voluntarily undertaken and experienced as the very substance of covenantal life. Suppression is minimal (0.03) because no coercion enforces participation; communities that abandon the practice simply cease to be the studying community. Theater ratio is low (0.05) because the intellectual engagement is functionally real — it actually transmits the law, forms identity, and sustains the priestly lineage's self-understanding. Accessibility collapse is very high (0.92) because once one accepts the covenantal framework, the alternative (the commandment is simply void) is structurally unavailable — the logic of 'perpetual statute' demands a mode of fulfillment. Resistance is low (0.08) because the reading resolves an existential problem for the community rather than imposing one.
 *
 * PERSPECTIVAL GAP:
 *   From the studying community's seat, this is a Mountain — the only coherent way to maintain a perpetual commandment under historical constraint. From the Temple Mount activist seat, it would compute as a Snare or Piton — a quietist substitution that prevents actual restoration. From the secular scholar seat, it computes as a brilliant cultural adaptation (Rope or Scaffold). The engine computes these divergences from the structural data; the authored claim (Mountain) reflects the internal logic of the reading itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The studying community is the beneficiary (d ≈ 0.1): they receive covenantal continuity and identity formation through the practice. Halakhic authorities are agenda_setters with constrained exit (d ≈ 0.3): they maintain the reading but cannot easily revise it without breaking transmission lineage. Temple Mount activists are excluded — their rejection of the reading places them outside the consensus. Secular scholars are observers (d = 0.5 analytical). The directionality derivation from beneficiary declarations + identity_locked exit for the studying community yields the expected near-zero effective extraction for participants.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to fulfill a perpetual commandment without its material locus) remains live — the Temple is not rebuilt. The arrangement has not atrophied; it continues to structure the daily practice, identity, and textual transmission of the studying community. Mandatrophy is not resolved because the mandate (covenantal fidelity) persists and the arrangement remains its active fulfillment. The reading would only become mandatrophic if the Temple were rebuilt and the community continued study instead of restoring material sacrifice — at which point the symbolic_transformation reading would become the live question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_structure,
    'This constraint is one reading (study_as_exercise) of the contested kernel ''temple_sacrifice_commitment''. What structural elements do the sibling readings (performance_only, hybrid_preparatory, symbolic_transformation) change relative to this reading?',
    'Comparative constraint story generation for each sibling reading, followed by cross-reading structural diff analysis. Each sibling reading should instantiate its own constraint story with its own ε, beneficiary/victim structure, and type.',
    'If sibling readings produce substantially different ε values (e.g., performance_only shows high extractiveness from guilt imposition, symbolic_transformation shows scaffold dynamics), the kernel decomposition is validated. If all readings produce near-zero ε, the contest may be semantic rather than structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_structure, conceptual, 'Committee frame: this story is one reading of a contested kernel; sibling readings are other constraints, not perspectives within this one.').

omega_variable(
    study_as_performance_boundary,
    'Where exactly does the equivalence between study and sacrifice hold, and where does it break? Does it cover all sacrificial categories (korbanot) equally, or only certain types?',
    'Halakhic source analysis: Talmudic sugyot (Menachot 110a, Ta''anit 27b, Megillah 31b), Maimonides'' codification, later acharonim. Identify whether the equivalence is total or partial, and what happens to categories that resist intellectualization (e.g., communal offerings requiring physical altar).',
    'If the equivalence is partial, the constraint may have internal extraction gradients — some community members (those focused on non-intellectualizable korbanot) may experience it differently. This would create sub-seats within the studying_community stakeholder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_performance_boundary, empirical, 'Boundary of the study-as-performance equivalence across sacrificial categories.').

omega_variable(
    identity_locked_exit_mechanism,
    'The studying_community is coded as identity_locked exit. What is the precise identity-fusion mechanism? Is it professional (kohanic lineage), communal (yeshiva world), or covenantal (all Israel as ''kingdom of priests'')?',
    'Sociological and theological analysis of how different sub-communities experience the practice. Survey of kohanim vs. non-kohanim, yeshiva vs. non-yeshiva communities, regarding whether they could conceive of Jewish identity without this practice.',
    'If identity lock operates differently across sub-groups, the single stakeholder ''studying_community'' may need decomposition. Kohanim may experience stronger identity lock (lineage-specific obligation); lay communities may experience it as communal rather than personal identity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_exit_mechanism, empirical, 'Mechanism and scope of identity-locked exit for the studying community.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__study_as_exercise, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temple_sacrifice_study_exercise_tr_t0, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0, 0.05).
narrative_ontology:measurement(temple_sacrifice_study_exercise_tr_t500, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 500, 0.05).
narrative_ontology:measurement(temple_sacrifice_study_exercise_tr_t1000, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1000, 0.05).
narrative_ontology:measurement(temple_sacrifice_study_exercise_tr_t1500, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1500, 0.05).
narrative_ontology:measurement(temple_sacrifice_study_exercise_tr_t2000, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 2000, 0.05).

% Extraction over time
narrative_ontology:measurement(temple_sacrifice_study_exercise_be_t0, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(temple_sacrifice_study_exercise_be_t500, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 500, 0.02).
narrative_ontology:measurement(temple_sacrifice_study_exercise_be_t1000, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1000, 0.02).
narrative_ontology:measurement(temple_sacrifice_study_exercise_be_t1500, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1500, 0.02).
narrative_ontology:measurement(temple_sacrifice_study_exercise_be_t2000, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 2000, 0.02).

% Suppression requirement over time
narrative_ontology:measurement(temple_sacrifice_study_exercise_su_t0, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 0, 0.03).
narrative_ontology:measurement(temple_sacrifice_study_exercise_su_t500, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 500, 0.03).
narrative_ontology:measurement(temple_sacrifice_study_exercise_su_t1000, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 1000, 0.03).
narrative_ontology:measurement(temple_sacrifice_study_exercise_su_t1500, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 1500, 0.03).
narrative_ontology:measurement(temple_sacrifice_study_exercise_su_t2000, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 2000, 0.03).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__study_as_exercise, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__study_as_exercise, 0.05).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one member of the temple_sacrifice_commitment constraint family. All four readings decompose the single natural-language concept 'what happens to sacrificial obligation without the Temple' into structurally distinct constraints with different ε values, beneficiary/victim structures, and types. This reading (study_as_exercise) claims Mountain (ε≈0). performance_only likely claims Snare/Tangled Rope (guilt extraction from non-performance). hybrid_preparatory likely claims Scaffold (transitional with messianic sunset). symbolic_transformation likely claims Rope (authorized transformation with coordination function).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
