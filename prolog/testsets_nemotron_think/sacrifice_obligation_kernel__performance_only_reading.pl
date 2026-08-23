% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__performance_only_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: sacrifice_obligation_kernel__performance_only_reading
 *   human_readable: Sacrifice Obligation Requires Physical Performance (Study Does Not Fulfill)
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   The performance_only_reading of the sacrifice obligation kernel holds
 *   that the biblical command to bring korbanot (sacrifices) in the Jerusalem
 *   Temple remains binding halakhah, but its physical performance has been
 *   impossible since the Temple's destruction in 70 CE. Study of sacrifice
 *   laws (kodshim) is meritorious preparation but does not fulfill the
 *   mitzvah. The obligation therefore persists as an unfillable demand on
 *   every Jew for 1,950 years. The constraint is framed as divine law
 *   (mountain), but its operational metrics show high extractiveness (ε=0.82)
 *   — the commanded population expends massive cognitive, emotional, and
 *   communal resources on an obligation that cannot be discharged. No human
 *   agent collects this extraction; the 'beneficiary' is the vindicated
 *   proposition of divine command perpetuity. The halakhic authority
 *   (agenda_setter) maintains the obligation's bindingness without being able
 *   to enable its fulfillment.
 *
 * KEY AGENTS:
 *   - halakhic_authority: agenda_setter (institutional/generational/analytical/universal) — maintains the obligation's binding status through interpretive tradition
 *   - jewish_people: payer (organized/generational/identity_locked/universal) — commanded but unable to perform; bears the extractiveness
 *   - messianic_expectants: excluded (moderate/biographical/constrained/universal) — hold sibling reading (messianic_suspension) but are not in the performance_only conversation
 *   - secular_jews: excluded (powerless/biographical/mobile/national) — would reject bindingness but are structurally outside halakhic discourse
 *   - academic_observers: observer (analytical/civilizational/analytical/universal) — analyze the constraint from outside the commitment system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, 0.82).
domain_priors:suppression_score(sacrifice_obligation_kernel__performance_only_reading, 0.88).
domain_priors:theater_ratio(sacrifice_obligation_kernel__performance_only_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, accessibility_collapse, 0.94).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__performance_only_reading, mountain).
narrative_ontology:human_readable(sacrifice_obligation_kernel__performance_only_reading, "Sacrifice Obligation Requires Physical Performance (Study Does Not Fulfill)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__performance_only_reading, "religious/halakhic/commitment_system").

domain_priors:emerges_naturally(sacrifice_obligation_kernel__performance_only_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__performance_only_reading, 'af7c6c7f-a3e1-46e1-8400-d0e2adc9f8a4').
narrative_ontology:cs_kernel_codification('af7c6c7f-a3e1-46e1-8400-d0e2adc9f8a4', formalized).
narrative_ontology:cs_authority_grounding('af7c6c7f-a3e1-46e1-8400-d0e2adc9f8a4', lineage).
narrative_ontology:cs_interpretation_layer_present('af7c6c7f-a3e1-46e1-8400-d0e2adc9f8a4').
narrative_ontology:cs_reading_relation('af7c6c7f-a3e1-46e1-8400-d0e2adc9f8a4', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('af7c6c7f-a3e1-46e1-8400-d0e2adc9f8a4', sacrifice_obligation_kernel__study_as_exercise_reading, forecloses).
narrative_ontology:cs_reading_relation('af7c6c7f-a3e1-46e1-8400-d0e2adc9f8a4', sacrifice_obligation_kernel__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('af7c6c7f-a3e1-46e1-8400-d0e2adc9f8a4', foundational, physical_performance_necessary_for_korban).
narrative_ontology:cs_axiom_status(physical_performance_necessary_for_korban, holdable).
narrative_ontology:cs_axiom_grounding('af7c6c7f-a3e1-46e1-8400-d0e2adc9f8a4', physical_performance_necessary_for_korban, deontological).
narrative_ontology:cs_axiom('af7c6c7f-a3e1-46e1-8400-d0e2adc9f8a4', foundational, study_is_preparatory_only_not_fulfillment).
narrative_ontology:cs_axiom_status(study_is_preparatory_only_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('af7c6c7f-a3e1-46e1-8400-d0e2adc9f8a4', study_is_preparatory_only_not_fulfillment, conventional).
narrative_ontology:cs_reference_frame('af7c6c7f-a3e1-46e1-8400-d0e2adc9f8a4', sinaitic_korban_obligation).
narrative_ontology:cs_drift_state('af7c6c7f-a3e1-46e1-8400-d0e2adc9f8a4', post_churban_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('af7c6c7f-a3e1-46e1-8400-d0e2adc9f8a4', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, jewish_people).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__performance_only_reading, divine_command_korbanot).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__performance_only_reading, halakhic_bindingness_perpetual).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__performance_only_reading, physical_performance_necessary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The rabbinic leadership (poskim, roshei yeshiva, halakhic decisors) maintains the bindingness of the sacrifice obligation through interpretive tradition. They authoritatively rule that study does not fulfill, prayer for restoration is required, and the obligation remains intact. Their legitimacy derives from being the chain of transmission (mesorah) from Sinai. They do not personally 'collect' the extraction but their authority is sustained by the system's continuity. Exit for them is analytical — they can reinterpret, but doing so would undermine their authority's grounding in lineage.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, halakhic_authority, agenda_setter,
    institutional, generational, analytical, universal).

% The entire Jewish people (within the halakhic system) are commanded to bring sacrifices but cannot. They bear the extractiveness: daily prayers for Temple restoration (3x daily), study of kodshim tractates, architectural/ritual preparation (e.g., Temple Institute), messianic yearning, and the psychological burden of perpetual unfulfillment. Exit requires leaving the covenantal identity — conversion out, secularization, or joining movements that reject halakhic bindingness. Even those who exit often carry residual suppression (internalized obligation). The obligation is universal in scope (all Jews, all times since 70 CE).
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, jewish_people, payer,
    organized, generational, identity_locked, universal).

% A subgroup within the halakhic world who hold the messianic_suspension_reading: they believe the obligation is divinely suspended, not binding in current form, and study maintains operational readiness for restoration. They are excluded from the performance_only_reading's conversation because that reading treats the obligation as actively binding now. Their exit to the performance_only frame is constrained — adopting it would require abandoning their reading of divine suspension. They are not victims of this reading's extraction (they don't experience the obligation as unfulfilled demand) but they are structurally excluded from shaping it.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, messianic_expectants, excluded,
    moderate, biographical, constrained, universal).

% Jews who do not accept halakhic authority. They would reject the sacrifice obligation's bindingness entirely. They are excluded from the halakhic conversation (not part of the commitment system). Their exit from the constraint is mobile — they simply don't participate. However, they may experience indirect suppression via communal/familial pressure or Israeli state rabbinate policies influenced by the halakhic system. They are the 'absent voices' who would object to the obligation's persistence if they were in the room.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, secular_jews, excluded,
    powerless, biographical, mobile, national).

% Scholars of halakha, religious studies, anthropology, history who analyze the sacrifice obligation from outside the commitment system. They do not bear the obligation's extraction nor benefit from its maintenance. They observe the structural dynamics: the 1,950-year persistence of an unfulfillable command, the identity_locked exit, the interpretive work maintaining bindingness. Their seat is analytical — they see the full structure including the sibling readings the insiders cannot simultaneously hold.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, academic_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally coordinated centralized divine service through the Jerusalem Temple: a single locus for atonement, thanksgiving, and communal gathering, solving the coordination problem of distributed local altars (bamot) and unifying the people around one sanctuary.
% TRANSFER_FUNCTION: Moves life-orientation, study labor (thousands of hours of kodshim learning), messianic yearning, prayer focus, and communal resources (Temple Institute, sacrificial animal preparation) from the jewish_people (payer) to... no human recipient. The transfer terminates in the unfillable obligation itself — the 'divine command' as vindicated proposition. The halakhic_authority (agenda_setter) receives authority/legitimacy maintenance, not the extraction itself.
% ABSENT_VOICES: Secular Jews, Reform/Conservative/Reconstructionist movements, Jewish atheists, and Jews-by-choice who reject halakhic bindingness — they would argue the obligation is historically conditioned, not perpetually binding, and that study or ethical action fulfills the spiritual intent. They are excluded because the halakhic conversation defines participation as acceptance of the system's axioms. Also absent: women's voices in traditional halakhic process (though women bear the obligation's extractiveness equally in this reading).
% DISAPPEARANCE_RATIONALE: If the performance_only_reading's obligation vanished overnight (e.g., halakhic consensus shifted to study_as_exercise or messianic_suspension), the jewish_people's daily prayer liturgy would change (removal of Temple restoration prayers), kodshim study would shift from 'preparation' to 'fulfillment' or 'historical study', messianic yearning would lose its central ritual focus, the Temple Institute would dissolve or repurpose, and the halakhic_authority's lineage-grounded legitimacy would face a crisis. Jewish practice and identity would reorganize around a fulfillable or suspended obligation.
% FOUNDING_PROBLEM: How to serve God through centralized sacrificial worship in the Jerusalem Temple, replacing distributed local altars (bamot) and unifying the people around one divinely chosen sanctuary.
% FOUNDING_PROBLEM_CORROBORATION: Historical and archaeological consensus: the Temple was destroyed in 70 CE and has not been rebuilt. The centralized sacrificial system ceased. No corroborating source outside the halakhic system's own beneficiaries (the halakhic_authority) attests that the founding problem persists. The halakhic_authority itself claims the problem is live (divine command persists regardless of Temple), but this is self-assertion from the benefiting seat. External corroboration (history, archaeology, non-Orthodox Jewish movements, academic scholarship) uniformly attests the founding problem is dead.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__performance_only_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__performance_only_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__performance_only_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__performance_only_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__performance_only_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, ExtMetricName, E),
    domain_priors:suppression_score(sacrifice_obligation_kernel__performance_only_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sacrifice_obligation_kernel__performance_only_reading),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the obligation demands total life-orientation toward an impossible performance — daily prayers for Temple restoration, study of kodshim, architectural/ritual preparation, messianic yearning — with zero fulfillment probability. Suppression is high (0.88) because exit from the obligation requires exit from Jewish identity/halakhic system (identity_locked), not merely non-compliance. Theater_ratio is low (0.15) because the performance_only_reading genuinely believes the obligation is real and binding; the study/preparation is not theatrical substitute but sincere preparation. Accessibility_collapse is near-total (0.94) — no alternative fulfillment exists within this reading (study explicitly does not fulfill). Resistance is near-zero (0.12) — within the commitment system, resisting the obligation is incoherent (denying divine command). The claimed_type is mountain (divine law, emerges_naturally) but metrics describe a constraint that extracts heavily from its subjects without human beneficiary — a structural anomaly the engine will detect.
 *
 * PERSPECTIVAL GAP:
 *   From the halakhic_authority seat (analytical, generational), the constraint is a mountain: divine law, unchangeable, zero extraction (God commands, we obey). From the jewish_people seat (identity_locked, generational), the constraint is experienced as snare-level extraction: a demand that cannot be met, extracting lifelong orientation with no discharge. The engine computes this divergence from the structural data — the agenda_setter has analytical exit and beneficiary-directionality (d≈0.0), the payer has identity_locked exit and victim-directionality (d≈1.0). The vindicated_propositions (divine_command) receive no rents but are the structural 'beneficiary' in the reading's own logic.
 *
 * DIRECTIONALITY LOGIC:
 *   halakhic_authority: beneficiary-directionality (d≈0.1) — the authority's legitimacy and coherence derive from maintaining the obligation; it is subsidized by the system's continuity. jewish_people: full target directionality (d≈0.95) — commanded, unable to perform, identity_locked exit (exit = leaving the people/covenant), universal scope. messianic_expectants: not in this reading's stakeholder set (excluded from performance_only conversation). secular_jews: mobile exit (d≈0.3) — can exit the halakhic system with moderate social cost. academic_observers: analytical (d=0.5) — symmetric observer. The engine derives d from beneficiary/victim declarations + exit_options + power. No beneficiaries declared (vindicated_propositions are not agents), so only victims drive high d for the payer seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (centralized sacrificial worship in Temple) is dead (Temple destroyed 70 CE, no restoration). The arrangement persists without its founding problem — classic mandatrophy. But the reading claims the problem is NOT dead (divine command persists regardless of Temple). The mismatch (founding_problem_status=dead per historical fact, but reading claims live) is the mandatrophy signal. The halakhic_authority could 'fix' it by adopting study_as_exercise or messianic_suspension readings (cheap fix — interpretive shift), but does not because the performance_only reading is structurally necessary to the authority's self-understanding as guardian of unchanged divine law. Fixing_cost=prohibitive not because of material cost but because the fix would dissolve the authority's grounding. Gain_flow=diffuse — no seat captures the extraction; it dissipates as unfulfilled yearning and study labor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'This constraint is one reading (performance_only_reading) of the contested kernel sacrifice_obligation_kernel. Sibling readings: messianic_suspension_reading, study_as_exercise_reading, symbolic_archive_reading. What structural elements do the readings disagree on?',
    'Map each reading''s beneficiary/victim structure, extractiveness referent, and authority grounding. The disagreement is located on: (1) whether the obligation persists as binding command (performance_only, messianic_suspension) vs. transformed/fulfilled by study (study_as_exercise) vs. dissolved into cultural archive (symbolic_archive); (2) whether extractiveness is experienced by the commanded (performance_only) or borne by a substitute practice (study_as_exercise); (3) whether the halakhic authority''s maintenance of the obligation constitutes extraction or preservation.',
    'If performance_only_reading is the only reading with high extractiveness and no human beneficiary, it occupies a unique structural position: a mountain-claimed constraint with snare-level extraction metrics but no extractor. This would trigger false_summit_mountain logic only if beneficiaries were declared — here the ''beneficiary'' is the vindicated proposition (divine command), not an agent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Kernel reading decomposition: structural disagreement locus across four readings of sacrifice_obligation_kernel').

omega_variable(
    extraction_without_extractor,
    'Can a constraint exhibit high extractiveness (ε ≈ 0.82) without a human beneficiary collecting the extraction? The performance_only_reading describes 1,900 years of unfulfillable obligation extracting life-orientation, study labor, and messianic yearning from the entire Jewish people, yet names no agent who benefits.',
    'Compare with piton constraints where extraction persists without concentrated beneficiary. If the halakhic system itself (as institution) captures the extraction (authority, coherence, continuity), then halakhic_authority is a beneficiary. If the extraction is genuinely diffuse — the obligation''s persistence serves no one''s interest but persists due to identity_locked exit — then the constraint is a mountain with anomalous extractiveness, or a piton where the ''agenda_setter'' is also trapped.',
    'If halakhic_authority is beneficiary, reclassify toward snare/tangled_rope with agenda_setter as beneficiary. If genuinely no beneficiary, the high ε on a mountain claim is a false summit of a different kind: natural-law framing (divine command) masking structural extraction experienced by the commanded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_without_extractor, conceptual, 'Whether high extractiveness without human beneficiary implies hidden institutional beneficiary or genuine structural anomaly').

omega_variable(
    identity_lock_mechanism,
    'The jewish_people stakeholder has exit_options: identity_locked. What specific identity-fusion mechanism binds them? Is it covenantal identity (the obligation constitutes the people), communal enforcement (social cost of exit), internalized theological frame (disobedience = spiritual defect), or halakhic process identity (the system has ''become'' its function)?',
    'Interview data from exiters (secular, converts out, Reform affiliates): does the obligation persist as felt demand after formal exit? Measure suppression persistence post-exit. If suppression persists, internalized component is high.',
    'If internalized suppression is high, effective suppression exceeds structural measure — the constraint travels with the agent. This would increase χ for the payer seat even without active enforcement. Affects piton vs. snare classification for the payer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Mechanism of identity_locked exit for jewish_people under unfulfillable sacrifice obligation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__performance_only_reading, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacrifice_obligation_perf_only_tr_t0, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sacrifice_obligation_perf_only_tr_t500, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 500, 0.08).
narrative_ontology:measurement(sacrifice_obligation_perf_only_tr_t1000, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(sacrifice_obligation_perf_only_tr_t1500, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1500, 0.12).
narrative_ontology:measurement(sacrifice_obligation_perf_only_tr_t1950, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1950, 0.15).

% Extraction over time
narrative_ontology:measurement(sacrifice_obligation_perf_only_be_t0, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(sacrifice_obligation_perf_only_be_t500, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 500, 0.8).
narrative_ontology:measurement(sacrifice_obligation_perf_only_be_t1000, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1000, 0.81).
narrative_ontology:measurement(sacrifice_obligation_perf_only_be_t1500, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1500, 0.82).
narrative_ontology:measurement(sacrifice_obligation_perf_only_be_t1950, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1950, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(sacrifice_obligation_perf_only_su_t0, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(sacrifice_obligation_perf_only_su_t500, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 500, 0.87).
narrative_ontology:measurement(sacrifice_obligation_perf_only_su_t1000, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1000, 0.88).
narrative_ontology:measurement(sacrifice_obligation_perf_only_su_t1500, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1500, 0.88).
narrative_ontology:measurement(sacrifice_obligation_perf_only_su_t1950, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1950, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__performance_only_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_kernel__performance_only_reading, 0.12).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% Four-reading constraint family for sacrifice_obligation_kernel. performance_only_reading (this) claims obligation persists unchanged → high extractiveness, no fulfillment. messianic_suspension_reading claims divine suspension → lower extractiveness, study as readiness. study_as_exercise_reading claims study fulfills → near-zero extractiveness (coordination fulfilled). symbolic_archive_reading claims no halakhic claim → mountain (cultural fact). ε values differ radically across readings: this reading ε≈0.82, study_as_exercise ε≈0.05, messianic_suspension ε≈0.35, symbolic_archive ε≈0.02. Linked via affects_constraints for contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_obligation_kernel__performance_only_reading, institutional, 0.1).
constraint_indexing:directionality_override(sacrifice_obligation_kernel__performance_only_reading, organized, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
