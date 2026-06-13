% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__catastrophe_validation_axis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__catastrophe_validation_axis, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tsunami_stone_commitment__catastrophe_validation_axis
 *   human_readable: Tsunami Stone Commitment: Catastrophe Validation Axis
 *   domain: disaster_anthropology/commitment_systems
 *
 * SUMMARY:
 *   Japanese tsunami stone markers (tsudamijo—'monuments that transmit')
 *   encode survival knowledge across centuries: 'Build homes above this
 *   point'; 'When the earth shakes, evacuate to high ground.' The commitment
 *   system is ancient and decentralized—oral stories, carved warnings,
 *   commemorative practices. The 2011 Tōhoku earthquake and tsunami provided
 *   a decisive empirical test of whether the commitment was still
 *   functionally alive. Communities with maintained stone markers and strong
 *   oral traditions evacuated quickly and experienced near-zero mortality
 *   among evacuees. Communities that had abandoned or ignored the tradition
 *   suffered catastrophic losses. The catastrophe_validation_axis reading
 *   frames this binary outcome—saved or lost lives—as the decisive test
 *   mechanism. The physics (subduction zone earthquakes, tsunami generation)
 *   is indifferent to human commitment; the constraint validates itself when
 *   the next large tsunami arrives. This reading is one of three sibling
 *   readings of the same kernel: behavioral_competence_reading (the tradition
 *   still actively governs behavior through norm enforcement) and
 *   commemorative_husk_reading (the stone markers are now symbolic artifacts,
 *   compliance is coincidental rather than committed). All three coexist in
 *   post-2011 scholarship, but the catastrophe_validation_axis reading
 *   carries the empirical weight because the 2011 outcomes stratify so
 *   sharply by prior commitment maintenance.
 *
 * KEY AGENTS:
 *   - japanese_coastal_communities_intergenerational: inheritors and transmitters of tsunami survival knowledge; their behavior in 2011 was the outcome that tested the constraint
 *   - tsunami_wave_physics: the mechanism that tests commitment adherence; subduction zone earthquakes and tsunami generation are indifferent to human commitment but provide the binary verdict—lives saved or lost
 *   - stone_inscriptions_and_oral_tradition: the specification of the commitment system; their maintenance or abandonment determined evacuation behavior in 2011
 *   - 2011_tsunami_outcome: the empirical verdict; mortality rates stratified sharply by commitment maintenance
 *   - researchers_and_observers: post-2011 documentation and analysis that traced the outcome back to the prior commitment system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__catastrophe_validation_axis, 0.0).
domain_priors:suppression_score(tsunami_stone_commitment__catastrophe_validation_axis, 0.0).
domain_priors:theater_ratio(tsunami_stone_commitment__catastrophe_validation_axis, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, extractiveness, 0.0).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__catastrophe_validation_axis, mountain).
narrative_ontology:human_readable(tsunami_stone_commitment__catastrophe_validation_axis, "Tsunami Stone Commitment: Catastrophe Validation Axis").
narrative_ontology:topic_domain(tsunami_stone_commitment__catastrophe_validation_axis, "disaster_anthropology/commitment_systems").

domain_priors:emerges_naturally(tsunami_stone_commitment__catastrophe_validation_axis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__catastrophe_validation_axis, 'a35b0bff-3d28-4e95-8a28-b88d01e88c90').
narrative_ontology:cs_kernel_codification('a35b0bff-3d28-4e95-8a28-b88d01e88c90', fixed_text).
narrative_ontology:cs_authority_grounding('a35b0bff-3d28-4e95-8a28-b88d01e88c90', practice).
narrative_ontology:cs_interpretation_layer_present('a35b0bff-3d28-4e95-8a28-b88d01e88c90').
narrative_ontology:cs_reading_relation('a35b0bff-3d28-4e95-8a28-b88d01e88c90', tsunami_stone_commitment__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('a35b0bff-3d28-4e95-8a28-b88d01e88c90', tsunami_stone_commitment__commemorative_husk_reading, influences).
narrative_ontology:cs_axiom('a35b0bff-3d28-4e95-8a28-b88d01e88c90', foundational, physics_provides_binary_verdict).
narrative_ontology:cs_axiom_status(physics_provides_binary_verdict, holdable).
narrative_ontology:cs_axiom_grounding('a35b0bff-3d28-4e95-8a28-b88d01e88c90', physics_provides_binary_verdict, empirically_contingent).
narrative_ontology:cs_axiom('a35b0bff-3d28-4e95-8a28-b88d01e88c90', foundational, tsunami_recurrence_tests_commitment_maintenance).
narrative_ontology:cs_axiom_status(tsunami_recurrence_tests_commitment_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('a35b0bff-3d28-4e95-8a28-b88d01e88c90', tsunami_recurrence_tests_commitment_maintenance, empirically_contingent).
narrative_ontology:cs_reference_frame('a35b0bff-3d28-4e95-8a28-b88d01e88c90', intergenerational_tsunami_survival_covenant).
narrative_ontology:cs_drift_state('a35b0bff-3d28-4e95-8a28-b88d01e88c90', contemporary_pre_2011, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a35b0bff-3d28-4e95-8a28-b88d01e88c90', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__catastrophe_validation_axis, japanese_coastal_communities_intergenerational).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__catastrophe_validation_axis, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(tsunami_stone_commitment__catastrophe_validation_axis, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__catastrophe_validation_axis_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, ExtMetricName, E),
    domain_priors:suppression_score(tsunami_stone_commitment__catastrophe_validation_axis, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(tsunami_stone_commitment__catastrophe_validation_axis),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(tsunami_stone_commitment__catastrophe_validation_axis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Mountain because: (1) it emerges naturally from unchangeable physical law—subduction zone earthquakes and tsunami generation cannot be negotiated away; (2) accessibility to alternatives is nearly complete closure—communities on the Sanriku coast cannot escape the hazard by leaving; (3) resistance is vanishingly low—the constraint is not enforced by coercion but by physics itself. The extractiveness and suppression scores are zero because there is no extraction mechanism and no need for suppressive coercion—the constraint operates through the physics of oceanic waves and coastal geography. The theater ratio is zero because there is no performative maintenance required—the constraint is indifferent to whether communities believe in it. The accessibility_collapse score is very high (0.92) because once the hazard is understood, alternatives collapse to two: evacuate or die. The resistance score is very low (0.03) because the constraint is imposed by physics, not by an agent with power to resist. The measurement series shows zero extractiveness across all historical periods because the constraint never extracts; it merely tests whether prior knowledge was retained and obeyed. The beneficiaries array includes the intergenerational community because this reading (unlike a pure natural law) recognizes that the tradition benefited those who maintained it—a potential false-summit candidate. The omega variables document the ambiguity: is the constraint the physics itself (natural law, no beneficiaries) or the commitment system that encodes recognition of the physics (constructed, with beneficiaries)? The 2011 outcome provides strong evidence for the constructed reading: the tradition's maintenance/abandonment, not the physics, determined outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The catastrophe_validation_axis reading emphasizes the physics as the decisive test apparatus. From the perspective of coastal communities, the constraint is experienced as both natural (the tsunami will come regardless) and constructed (we must choose to maintain and obey the knowledge system). From the perspective of researchers studying the 2011 outcome, the reading stratifies sharply: those who maintained the tradition survived; those who abandoned it did not. The behavioral_competence reading emphasizes the active norm enforcement and intergenerational transmission—the commitment system as a living social fact. The commemorative_husk reading emphasizes the decoupling of the tradition from actual behavior in modernized communities—the markers became symbols, not imperatives. The engine computes seat-level types from structural data; this reading supplies the empirical evidence (the 2011 outcome) that validates the catastrophe_validation_axis framing: the physics itself provided the binary test.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary group (intergenerational coastal communities) experiences full directionality toward subsidy—the constraint saves their lives if they maintain it and obey the knowledge system. Directionality d approaches 0.0 (full beneficiary) because the constraint imposes no cost except the effort to maintain oral tradition and ritual. The only cost bearer would be communities that must migrate away from the coast to escape the hazard, but they are outside the constraint's scope. The paradox—a Mountain constraint with declared beneficiaries—is resolved by the false-summit omega: the beneficiary declaration is what FSM uses to detect whether a claimed 'natural law' is actually a constructed commitment system that benefits specific agents. The 2011 data support the constructed reading: the tradition's maintenance/abandonment, not physics alone, determined survival outcomes.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of the tsunami-stone commitment is: encode and transmit survival knowledge across generations so that descendants evacuate correctly when the next tsunami arrives. The founding problem is live—tsunami recurrence intervals are measured in centuries, demographic turnover ensures generational knowledge gaps, and intergenerational transmission is the only mechanism to bridge them. The 2011 outcome validates that the mandate is still necessary and that the commitment system, where maintained, still accomplishes its mandate. The constraint does not exhibit mandatrophy because the founding problem has not been superseded by an alternative (engineered breakwaters failed in 2011; early warning systems complement but do not replace the stone markers); the commitment system remains the primary intergenerational knowledge transmission mechanism for tsunami survival.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_commitment,
    'Is the tsunami-stone constraint a natural law (tsunami physics is indifferent; the stone inscriptions are merely human recognition of unchangeable reality) or a constructed commitment system (the inscriptions encode a collective covenant to treat certain knowledge as binding)?',
    'Examine pre-inscription tsunami fatality patterns in communities without the tradition versus post-inscription patterns in communities with it. If mortality reduction follows from the tradition''s adoption and maintenance (rather than from independent physical changes), the constraint is more constructed than natural. If mortality reduction tracks tsunami physics alone, it is a natural law. The 2011 data strongly supports the constructed reading: communities that abandoned the tradition suffered catastrophic losses despite unchanged physics.',
    'If natural law: the constraint has no beneficiaries (beneficiaries are incoherent for natural laws); the force of the tradition derives from recognition of unchangeable physics. If constructed commitment: the beneficiary group (intergenerational community) forms around maintaining the knowledge system, and the tradition''s persistence depends on active transmission, not passive discovery. This omega explains why a Mountain constraint carries beneficiaries—a false-summit candidate detection mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_commitment, conceptual, 'Whether the constraint is physical law or socially constructed knowledge system.').

omega_variable(
    kernel_reading_contest_frame,
    'Which reading of the tsunami-stone commitment kernel is validated by the 2011 empirical record: catastrophe_validation_axis (physics as decisive test), behavioral_competence_reading (live norm enforcement), or commemorative_husk_reading (symbolic artifact, compliance coincidental)?',
    'Stratified mortality analysis post-2011: Did evacuation speed and safety correlate with proximity to stone markers and strength of oral tradition maintenance? The empirical answer is yes—unambiguously. Communities with strong tradition adherence evacuated in 5–10 minutes and had near-zero mortality among evacuees. Communities that abandoned the tradition or ignored the markers suffered 20–30% mortality. This outcome strongly validates the catastrophe_validation_axis reading: the physics itself (tsunami wave propagation, timing, inundation depth) provided a decisive, binary test of whether the commitment system was still functionally alive.',
    'The catastrophe_validation_axis reading is supported as the empirically dominant reading; this constrains but does not foreclose the other readings. The behavioral_competence reading remains live (the tradition was behaviorally competent in 2011); the commemorative_husk reading is weakened by the empirical correlation but not falsified (one could argue the compliance was coincidental, though the evidence runs strongly against it). The three readings coexist in post-2011 scholarly discourse, but the catastrophe_validation_axis reading now carries the empirical weight.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_frame, empirical, 'Post-2011 empirical validation of the reading contest.').

omega_variable(
    intergenerational_knowledge_decay,
    'How fast does the tsunami-stone commitment degrade when the oral tradition is not actively maintained? Between major tsunami events (return period ~70–100+ years), does intergenerational transmission alone sustain the commitment, or does commemorative practice, school education, or social narrative reinforce it?',
    'Compare evacuation behavior and stone-marker knowledge retention across regions with strong vs. weak post-1960 ritual maintenance. The Sanriku region (strong tradition) vs. Sendai region (rapid urbanization, weaker tradition) show marked differences. Post-2011, communities began formal education integration and monument restoration to rebuild the tradition after decades of degradation. Long-term tracking of knowledge retention across generational turnover intervals would resolve this.',
    'If oral tradition degrades rapidly without active ritual and social reinforcement: the constraint is more fragile than the ''mountain'' classification suggests; it depends on sustained human effort, not physics alone. If it persists through generations without reinforcement: the classification as a natural law-like constraint is stronger. The empirical picture is mixed: strong decay observed in modernizing communities, but rapid recovery observed post-2011 when the commitment was re-engaged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_knowledge_decay, empirical, 'Rate of knowledge decay and regeneration between tsunami events.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__catastrophe_validation_axis, 1600, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t1600, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1600, 0.0).
narrative_ontology:measurement(tsun_tr_t1700, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1700, 0.0).
narrative_ontology:measurement(tsun_tr_t1868, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1868, 0.0).
narrative_ontology:measurement(tsun_tr_t1952, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1952, 0.0).
narrative_ontology:measurement(tsun_tr_t1960, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1960, 0.0).
narrative_ontology:measurement(tsun_tr_t2011, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 2011, 0.0).

% Extraction over time
narrative_ontology:measurement(tsun_be_t1600, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1600, 0.0).
narrative_ontology:measurement(tsun_be_t1700, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1700, 0.0).
narrative_ontology:measurement(tsun_be_t1868, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1868, 0.0).
narrative_ontology:measurement(tsun_be_t1952, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1952, 0.0).
narrative_ontology:measurement(tsun_be_t1960, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1960, 0.0).
narrative_ontology:measurement(tsun_be_t2011, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 2011, 0.0).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(tsunami_stone_commitment__catastrophe_validation_axis, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__catastrophe_validation_axis, attachment_coordination).
narrative_ontology:affects_constraint(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment__behavioral_competence_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The tsunami_stone_commitment kernel decomposes into three readings with distinct ε values and classification vectors. catastrophe_validation_axis frames the 2011 tsunami as the decisive empirical test of whether the intergenerational commitment system remained functionally alive. The reading is a Mountain because the physics is indifferent and unchangeable, but it carries beneficiaries (the intergenerational community) because the tradition's maintenance/abandonment determined 2011 outcomes—a false-summit candidate that FSM evaluates. The sibling readings (behavioral_competence_reading, commemorative_husk_reading) address the social embedding and narrative status of the same kernel; catastrophe_validation_axis is the empirical adjudication layer. Each reading has its own stakeholder configuration, metrics, and claim/metric divergence; they are linked by the kernel and the 2011 outcome that tests all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
