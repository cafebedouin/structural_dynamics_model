% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__archive_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__archive_maintenance, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: sacrifice_commandment__archive_maintenance
 *   human_readable: Sacrifice Law Study as Archive Maintenance for Future Temple Restoration
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   In Jewish law, after the Temple's destruction in 70 CE, sacrifice became
 *   impossible yet remained the subject of a commandment in Torah. The
 *   halakhic response was to reinterpret the commandment: study of sacrifice
 *   law fulfills the obligation even without physical execution. This
 *   constraint story instantiates the archive_maintenance reading: study
 *   preserves technical knowledge for future Temple restoration and
 *   represents preparation for messianic realization. It is one reading of a
 *   contested kernel (the sacrifice commandment itself); sibling readings
 *   argue either that study does NOT fulfill the commandment
 *   (performance_only) or that study itself IS performance
 *   (study_as_performance). The archive_maintenance reading is distinctive
 *   because it justifies present burden by reference to a future beneficiary
 *   (the generation at Temple restoration) who does not exist and did not
 *   consent. This creates moderate extractiveness: present practitioners bear
 *   the cost of studying law they cannot execute; the benefit is theoretical
 *   and conditional on events the present generation cannot control. The
 *   constraint is classified as Scaffold because its justification is
 *   explicitly transitional—study is maintenance until the Temple is restored
 *   and sacrifice becomes commandable again. Without the sunset condition
 *   (Temple restoration), the constraint would be a rope (genuine
 *   coordination of a multi-generational transmission problem) or a piton
 *   (performance maintained by institutional inertia). With the sunset, it is
 *   preparation.
 *
 * KEY AGENTS:
 *   - halakhic_scholars: agenda-setters maintaining the interpretive tradition that study fulfills the commandment; identity_locked to the doctrine; organized power
 *   - present_community_practitioners: payers bearing the study burden; constrained exit (rejecting scholars' framing is costly within community); moderate power
 *   - future_generation_at_temple_restoration: beneficiaries (hypothetical); powerless; cannot consent; benefit is conditional on messianic event
 *   - alternative_interpretation_communities: excluded; they hold performance_only reading; constrained exit from majority institutions
 *   - messianic_traditionalists: beneficiaries (present); powerful; some are institutional agenda-setters; benefit from narrative affirming eschatological hope
 *   - theological_historians: observers; analytical seat; document institutional and textual mechanisms producing the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__archive_maintenance, 0.48).
domain_priors:suppression_score(sacrifice_commandment__archive_maintenance, 0.22).
domain_priors:theater_ratio(sacrifice_commandment__archive_maintenance, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, extractiveness, 0.48).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__archive_maintenance, scaffold).
narrative_ontology:human_readable(sacrifice_commandment__archive_maintenance, "Sacrifice Law Study as Archive Maintenance for Future Temple Restoration").
narrative_ontology:topic_domain(sacrifice_commandment__archive_maintenance, "religious/halakhic").

domain_priors:requires_active_enforcement(sacrifice_commandment__archive_maintenance).
narrative_ontology:has_sunset_clause(sacrifice_commandment__archive_maintenance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__archive_maintenance, '16fe384e-a03f-4a5c-bcb6-408d41398ab1').
narrative_ontology:cs_kernel_codification('16fe384e-a03f-4a5c-bcb6-408d41398ab1', fixed_text).
narrative_ontology:cs_authority_grounding('16fe384e-a03f-4a5c-bcb6-408d41398ab1', lineage).
narrative_ontology:cs_interpretation_layer_present('16fe384e-a03f-4a5c-bcb6-408d41398ab1').
narrative_ontology:cs_reading_relation('16fe384e-a03f-4a5c-bcb6-408d41398ab1', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('16fe384e-a03f-4a5c-bcb6-408d41398ab1', sacrifice_commandment__study_as_performance, influences).
narrative_ontology:cs_axiom('16fe384e-a03f-4a5c-bcb6-408d41398ab1', foundational, study_fulfills_via_preparation).
narrative_ontology:cs_axiom_status(study_fulfills_via_preparation, holdable).
narrative_ontology:cs_axiom_grounding('16fe384e-a03f-4a5c-bcb6-408d41398ab1', study_fulfills_via_preparation, theological).
narrative_ontology:cs_axiom('16fe384e-a03f-4a5c-bcb6-408d41398ab1', foundational, temple_restoration_eschatologically_live).
narrative_ontology:cs_axiom_status(temple_restoration_eschatologically_live, holdable).
narrative_ontology:cs_axiom_grounding('16fe384e-a03f-4a5c-bcb6-408d41398ab1', temple_restoration_eschatologically_live, theological).
narrative_ontology:cs_reference_frame('16fe384e-a03f-4a5c-bcb6-408d41398ab1', study_as_commandment_fulfillment).
narrative_ontology:cs_drift_state('16fe384e-a03f-4a5c-bcb6-408d41398ab1', contemporary_post_2000_years, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('16fe384e-a03f-4a5c-bcb6-408d41398ab1', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__archive_maintenance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, future_generation_at_temple_restoration).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, halakhic_scholars).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, present_community_practitioners).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__archive_maintenance, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(sacrifice_commandment__archive_maintenance, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__archive_maintenance_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_commandment__archive_maintenance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_commandment__archive_maintenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) rather than low because the present generation bears cost (intense study of inaccessible law) with uncertain future return. It is not high because a genuine coordination problem exists (multi-generational transmission of knowledge) and some practitioners report authentic motivation to preserve the tradition. Suppression is low (0.22) because the constraint is sustained more by institutional authority and identity commitment than by coercive barriers—practitioners could exit by changing communities, though this is identity-costly. Theater_ratio rises from 0.42 to 0.58 over 2000 years, indicating increasing performative component relative to instrumental function: the closer the present gets to 2000 years post-destruction, the harder it is to maintain that study is serious preparation (restoration has not occurred) rather than identity maintenance and institutional ritual. The measurement series tracks this gradual shift. Accessibility_collapse is low (0.35) because alternative interpretations remain available (performance_only, study_as_performance) and even outside the Orthodox frame, non-sacrificial Judaism flourishes globally. Resistance is moderate (0.41) because alternative reading communities, reform movements, and secular practitioners all resist the mandate to study sacrifice law.
 *
 * PERSPECTIVAL GAP:
 *   This is a high-salience perspectival gap. The halakhic scholar seat experiences archive_maintenance as genuine coordination of a multi-generational knowledge problem; the institutional authority is earned and legitimate. The present practitioner seat experiences it as obligation to study law for uncertain future benefit, justified by institutional authority they did not choose. The future generation seat (analytical, not present) experiences it as a burden imposed without consent, though the stated purpose is their benefit. The theological historian seat sees the constraint as the institutional solution to the theological problem created by Temple destruction; it is neither natural law nor voluntary coordination but a constructed doctrine sustained by authority and textual interpretation. These four perspectives will produce different DR type conclusions. The schema requires authoring all structural data (beneficiary/victim, power, exit_options, enforcement, etc.) without preconforming to a predicted type; the engine computes per-seat classification from the data.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic scholars occupy the beneficiary/agenda-setter role (they define what counts as fulfillment, control the curriculum, gain professional prestige from the tradition's continuity). Their directionality is near the beneficiary end (low d): they benefit from the mandate's existence and authority. Present practitioners are payers: they invest effort in law they cannot execute. Their directionality is moderate-to-high (0.55–0.65): they bear cost and have constrained exit (identity_locked to the community). The future generation is powerless and analytical—they have no present directional relationship to the constraint, though they are its nominal beneficiary. Messianic traditionalists are also beneficiaries (the narrative affirms their theology) but some have institutional power-setter roles, so they occupy a dual seat: beneficiary ideology + agenda-setter authority. The engine's per-seat computation will likely produce different type classifications: from the scholars' seat, the constraint may compute as rope (genuine coordination, they benefit). From the practitioners' seat, it may compute as tangled_rope (coordination function, but asymmetric extraction—they study while scholars teach, they bear burden for scholars' benefit, even if the stated beneficiary is future generations). From the observer seat (theological historians), it may compute as piton (institutional performance maintained by authority and identity, not by active function).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Temple destruction created an obligation that cannot be executed) is addressed by the archive_maintenance reading via reinterpretation: study fulfills the obligation, and the mandate's real purpose is preservation for future restoration. The founding_problem_status is contested: traditionalists argue the problem is live (Temple destruction is still a fact, restoration is still awaited); historians and reformers argue the problem is dead (the obligation was solved in the 2nd century CE by reinterpretation, and the constraint now persists for institutional reasons, not because the foundational theological problem remains urgent). The disappearance_verdict is contested: traditionalists say that if the study mandate vanished, knowledge would be lost and restoration would be impossible; reformers and historians say that without the mandate, alternative community practices flourish and the loss is acceptable or even desirable. The measurement series showing rising theater_ratio suggests that the constraint is evolving toward mandatrophy: the study is increasingly performative (theater) relative to archival instrumental function. This pattern is consistent with a constraint whose founding problem has receded or been reframed, but whose institutional machinery persists due to inertia and identity investment. The archive_maintenance reading is the one most vulnerable to this transition: if the messianic premise erodes further, the reading loses its justificatory power and the constraint becomes either (1) performance_only (study does not fulfill commandment), (2) study_as_performance (study is its own fulfillment, not preparation), or (3) piton (institutional theater maintained for identity and authority, not for future utility).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    temple_restoration_probability,
    'What is the epistemic status of Temple restoration—is it a live eschatological expectation with non-negligible probability, or a distant theoretical possibility maintained for theological consistency?',
    'The future event itself cannot be resolved until it occurs or becomes logically impossible (e.g., if the Temple site is permanently altered). Present resolution depends on documented eschatological theology: how seriously do halakhic authorities treat restoration as a near-term vs. far-future vs. conditional possibility?',
    'If restoration is regarded as distant but real (non-zero probability in a 1000+ year horizon), the sacrifice_commandment archive_maintenance reading is justified as contingency planning. If restoration is maintained purely for theological completeness with vanishingly low probability, the present-generation burden (study of inaccessible law) appears extractive with unclear beneficiary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(temple_restoration_probability, empirical, 'Whether Temple restoration is a live eschatological expectation or a theoretical theological commitment.').

omega_variable(
    knowledge_preservation_efficacy,
    'Does intensive rabbinic study of sacrifice law actually preserve the knowledge needed for future Temple restoration, or is the knowledge sufficiently recorded in texts that continuous study is redundant?',
    'Comparative analysis: (a) document what specific details of sacrifice procedure are preserved only in oral tradition and commentary vs. what is fixed in written sources; (b) test whether a future generation could reconstruct the law from primary texts without intermediate study tradition.',
    'If study preserves unique knowledge not recoverable from texts, it serves genuine archival function. If the knowledge is fully recorded and study is pedagogical-only, the mandate to study becomes harder to justify as archive maintenance and more easily read as identity maintenance or institutional self-perpetuation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_preservation_efficacy, empirical, 'Whether continuous study is structurally necessary for knowledge preservation vs. replaceable by textual transmission.').

omega_variable(
    substitution_vs_suspension_vs_preparation,
    'Is the archive_maintenance reading''s normative claim—that study fulfills the sacrifice commandment—based on the premise that study IS a substitute performance, or on the weaker claim that study IS preparation (not fulfillment but justified postponement)?',
    'Textual and doctrinal analysis of halakhic reasoning: does the halakhic literature present study as equivalent execution (substitution) or as alternative valid engagement pending restoration (preparation)? The distinction matters: substitution implies present fulfillment; preparation implies present obligation with justified deferral.',
    'If the reading rests on substitution, it forecloses the performance_only reading (you cannot hold both ''study substitutes for sacrifice'' and ''only performance executes the commandment''). If the reading is preparation-based, it coexists_with performance_only (both agree study is not performance; they differ on whether the commandment is suspended). This distinction determines whether the archive_maintenance reading is logically exclusive or merely competitive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substitution_vs_suspension_vs_preparation, conceptual, 'Whether study fulfills the sacrifice commandment (substitution) or merely preserves knowledge for future fulfillment (preparation).').

omega_variable(
    messianic_intentionality_in_present_practice,
    'To what degree is the present-generation burden (intensive study of inaccessible law) borne as preparation for a messianic future, vs. as an identity-maintenance practice whose messianic framing has become vestigial?',
    'Ethnographic and institutional study: track whether practitioners report motivation as (a) active preparation for near-term restoration, (b) identity affirmation within a tradition, or (c) obedience to rabbinical mandate independent of future utility. Survey changes in eschatological expectation over time and correlation with changes in study intensity.',
    'If study is primarily motivated by active messianic expectation, the archive_maintenance frame is operative. If motivation is identity maintenance with messianic narrative as justification, the constraint functions more like an identity_coordination rope or a piton (performance without primary utility). This affects classification: a constraint that extracts effort for future benefit (archive_maintenance scaffold) differs from one extracting effort for present-community identity benefit (rope/piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_intentionality_in_present_practice, empirical, 'Whether present study burden is motivated by active messianic preparation or by present-generation institutional and identity needs.').

omega_variable(
    alternative_reading_forecast,
    'Given the measurement series showing rising theater_ratio (study appearing increasingly performative relative to instrumental) and moderate extractiveness, is the constraint evolving toward the performance_only reading (study is not fulfillment, commandment is suspended) or being stabilized as piton (study is theater maintained by institutional inertia)?',
    'Projection forward: if theater_ratio continues rising above 0.6 while Temple restoration remains indefinitely deferred, the constraint becomes harder to justify as archive_maintenance and easier to read as either suspended obligation (performance_only) or institutional performance (piton). The inflection point is whether the rising theater tracks genuine shift in what practitioners believe (normative shift toward one sibling reading) or institutional maintenance of a doctrine no longer believed (piton dynamics).',
    'This omega documents the foreclosure risk: archive_maintenance is the reading that depends most heavily on the messianic premise being live. If that premise erodes, the reading''s justification collapses and the constraint either transitions to a different reading (performance_only) or degrades to piton. Early detection of this trajectory would appear in theater_ratio drift and in institutional resistance to acknowledging the shift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_reading_forecast, empirical, 'Risk that archive_maintenance reading transitions to performance_only or degrades to piton as theater_ratio rises and messianic expectation erodes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__archive_maintenance, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__archive_maintenance, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sacr_tr_t250, sacrifice_commandment__archive_maintenance, theater_ratio, 250, 0.46).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_commandment__archive_maintenance, theater_ratio, 500, 0.5).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_commandment__archive_maintenance, theater_ratio, 1000, 0.56).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_commandment__archive_maintenance, theater_ratio, 1500, 0.59).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_commandment__archive_maintenance, theater_ratio, 2000, 0.58).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__archive_maintenance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sacr_be_t250, sacrifice_commandment__archive_maintenance, base_extractiveness, 250, 0.38).
narrative_ontology:measurement(sacr_be_t500, sacrifice_commandment__archive_maintenance, base_extractiveness, 500, 0.41).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_commandment__archive_maintenance, base_extractiveness, 1000, 0.46).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_commandment__archive_maintenance, base_extractiveness, 1500, 0.49).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_commandment__archive_maintenance, base_extractiveness, 2000, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__archive_maintenance, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(sacr_su_t250, sacrifice_commandment__archive_maintenance, suppression_requirement, 250, 0.17).
narrative_ontology:measurement(sacr_su_t500, sacrifice_commandment__archive_maintenance, suppression_requirement, 500, 0.19).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_commandment__archive_maintenance, suppression_requirement, 1000, 0.21).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_commandment__archive_maintenance, suppression_requirement, 1500, 0.23).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_commandment__archive_maintenance, suppression_requirement, 2000, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__archive_maintenance, attachment_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_commandment__archive_maintenance, 0.12).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__performance_only).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__study_as_performance).

% DUAL FORMULATION NOTE:
% The sacrifice_commandment kernel decomposes into three structurally distinct constraints: archive_maintenance (this story, moderate extraction justified by future benefit, scaffold), performance_only (low extraction, suspension framing, mountain-adjacent), and study_as_performance (low extraction, substitution framing, rope). Each reading has a different ε and occupies a different place in the DR type space. All three are live positions within contemporary Jewish legal discourse and theology. The kernel itself (the obligation to sacrifice) is fixed_text in the Torah; the contest is entirely over how to interpret its applicability after Temple destruction. The network linking these stories preserves the genealogy: they are not independent constraints but three readings of one contested commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_commandment__archive_maintenance, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
