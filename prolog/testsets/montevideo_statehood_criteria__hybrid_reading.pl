% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__hybrid_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__hybrid_reading
 *   human_readable: Montevideo Statehood Criteria (Hybrid: Objective + Normative Legitimacy Reading)
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The Montevideo Convention (1933) established four objective criteria for
 *   statehood: defined territory, permanent population, government, and
 *   capacity to conduct international relations. The hybrid reading adds a
 *   fifth gate: normative legitimacy grounded in democratic governance, human
 *   rights protection, and non-aggression. This reading is institutionalized
 *   in UN practice, human rights covenants, and the doctrines of liberal
 *   democratic states. It differs from the declaratory reading (objective
 *   criteria alone suffice) and the constitutive reading (statehood requires
 *   active recognition by existing states). The hybrid reading splits the
 *   difference: objective criteria are necessary but insufficient; normative
 *   alignment is also required. This creates a gatekeeping mechanism that
 *   benefits liberal democratic states and humanitarian intervention
 *   advocates while imposing costs on non-liberal secessionists and
 *   post-colonial regimes. The constraint's persistence depends on active
 *   enforcement (recognition denial, sanctions, conditional aid) and on
 *   suppressing the alternative declaratory reading (which remains live in
 *   legal scholarship and among Global South states).
 *
 * KEY AGENTS:
 *   - liberal_democratic_states: institutional power, agenda-setters, beneficiaries — control recognition decisions and the interpretation of normative criteria
 *   - non_liberal_secessionists: moderate power, payers — denied recognition because they fail the normative tests; trapped between accepting liberal criteria and remaining unrecognized
 *   - post_colonial_regimes_failing_liberal_criteria: moderate power, payers with identity-locked exit — inherited or chose non-liberal governance; now subject to permanent conditional statehood review
 *   - humanitarian_intervention_advocates: institutional power, beneficiaries — gain justification for intervention when statehood-normative criteria fail
 *   - declaratory_reading_advocates: excluded from the hybrid frame — their legal position is structurally incompatible
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, 0.67).
domain_priors:suppression_score(montevideo_statehood_criteria__hybrid_reading, 0.71).
domain_priors:theater_ratio(montevideo_statehood_criteria__hybrid_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, extractiveness, 0.67).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__hybrid_reading, "Montevideo Statehood Criteria (Hybrid: Objective + Normative Legitimacy Reading)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__hybrid_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__hybrid_reading, '17cdb9cd-93b2-461d-81fc-90c4d0f92f45').
narrative_ontology:cs_kernel_codification('17cdb9cd-93b2-461d-81fc-90c4d0f92f45', fixed_text).
narrative_ontology:cs_authority_grounding('17cdb9cd-93b2-461d-81fc-90c4d0f92f45', extraction).
narrative_ontology:cs_interpretation_layer_present('17cdb9cd-93b2-461d-81fc-90c4d0f92f45').
narrative_ontology:cs_reading_relation('17cdb9cd-93b2-461d-81fc-90c4d0f92f45', montevideo_statehood_criteria__declaratory_reading, forecloses).
narrative_ontology:cs_reading_relation('17cdb9cd-93b2-461d-81fc-90c4d0f92f45', montevideo_statehood_criteria__constitutive_reading, coexists_with).
narrative_ontology:cs_axiom('17cdb9cd-93b2-461d-81fc-90c4d0f92f45', foundational, normative_legitimacy_as_statehood_prerequisite).
narrative_ontology:cs_axiom_status(normative_legitimacy_as_statehood_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('17cdb9cd-93b2-461d-81fc-90c4d0f92f45', normative_legitimacy_as_statehood_prerequisite, deontological).
narrative_ontology:cs_axiom('17cdb9cd-93b2-461d-81fc-90c4d0f92f45', foundational, objective_criteria_insufficiency).
narrative_ontology:cs_axiom_status(objective_criteria_insufficiency, holdable).
narrative_ontology:cs_axiom_grounding('17cdb9cd-93b2-461d-81fc-90c4d0f92f45', objective_criteria_insufficiency, deontological).
narrative_ontology:cs_reference_frame('17cdb9cd-93b2-461d-81fc-90c4d0f92f45', montevideo_objective_criteria_plus_liberal_normative_gate).
narrative_ontology:cs_drift_state('17cdb9cd-93b2-461d-81fc-90c4d0f92f45', contemporary_post_human_rights_codification, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('17cdb9cd-93b2-461d-81fc-90c4d0f92f45', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, humanitarian_intervention_advocates).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionists).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, post_colonial_regimes_failing_liberal_criteria).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, authoritarian_successor_states).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__hybrid_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__hybrid_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.67) because the constraint transfers authority over statehood from an objective, verifiable test to a discretionary normative gate administered by a subset of powerful states. Suppression is high (0.71) because the enforcement of this constraint requires actively denying recognition to entities that meet the objective criteria, and this denial must be justified by reference to normative failures — which means suppressing the alternative declaratory reading and maintaining consensus among liberal states about what counts as legitimate governance. Theater is moderate (0.42) because the constraint involves genuine international legal discourse (UN recognition proceedings, human rights reviews), but an increasing share of the enforcement activity is devoted to maintaining the normative gate rather than verifying the objective criteria. The measurement series shows extractiveness and suppression rising sharply from 0-24 (the period of greatest contestation and norm-articulation in UN practice, roughly 1945-1993) and then plateauing (stabilization of the hybrid norm from 1993-2045), while theater rises gradually as enforcement becomes more routinized. The temporal pattern reflects the constraint hardening into institutional practice.
 *
 * PERSPECTIVAL GAP:
 *   From the liberal democratic state perspective, the hybrid reading is coherent statehood law: objective criteria prevent sovereign claims by non-territorial entities, and normative criteria prevent international legal status from laundering authoritarian regimes. From the non-liberal secession or post-colonial perspective, the same constraint is neo-colonial gatekeeping that weaponizes human rights discourse to deny self-determination to entities that do not adopt Western governance. From the declaratory reading advocates' perspective, it is a corruption of objective law by ideological preference. The engine should compute these divergences from the structural data: the beneficiary seats (liberal democracies, intervention advocates) will see rope or even pure coordination; the payer seats (non-liberal secessionists, post-colonial regimes) should compute as higher extraction. The analytical observer seat (existing state system) will compute as symmetric, observing a coordination function that has been layered with asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Liberal democratic states are structurally beneficiaries: they control the interpretation of normative criteria and can deny recognition to competitors or ideological rivals under the guise of human rights enforcement. Their directionality (d) is low, indicating they collect from the constraint without bearing its costs. Non-liberal secessionists and post-colonial regimes are structurally victimized: they are denied a legal status they claim to meet objectively, unless they restructure their governance. Their directionality is high (near 1.0). Humanitarian intervention advocates benefit from the constraint because it supplies legal cover for interventions justified by statehood-normative failure. The declaratory reading advocates are excluded, not because they are powerless, but because their legal position is logically incompatible with the hybrid frame — they cannot coexist in the same institutional structure. Indigenous peoples are powerless and trapped: if statehood criteria included normative tests, a would-be indigenous state might be required to adopt liberal democracy or forfeit recognition, foreclosing customary governance models.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing authoritarian regimes from claiming sovereignty to avoid intervention) was live in 1945-1990 and remains contested today. The hybrid reading does NOT resolve mandatrophy: the constraint persists because it benefits liberal democracies and intervention advocates, not because the founding problem remains acute. The gatekeeping mechanism outlasts the original security rationale. The theater ratio rising from 0.25 to 0.42 indicates increasing performative defense of the normative criteria — more UN discourse about human rights and legitimate governance, less about the objective criteria themselves. This is classic mandatrophy drift: the constraint was built to solve a security coordination problem; it persists as a mechanism for distributing international status and legitimacy according to liberal ideology. Mandatrophy is NOT resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_criteria_as_gatekeeping,
    'Are the normative legitimacy criteria (democracy, human rights, non-aggression) genuinely universal standards, or are they liberal-democratic preferences weaponized as international law?',
    'Comparative analysis of state recognition patterns: do liberal democracies deny recognition consistently based on normative failures, or do they selectively apply the criteria based on geopolitical interest? Do non-liberal states that meet the objective criteria gain recognition despite normative failures?',
    'If the criteria are genuinely universal, the constraint is rope (genuine coordination with asymmetric enforcement). If they are selectively applied, the constraint reclassifies as snare (pure extraction with ideological cover). If inconsistently applied, it is a piton (normative maintenance of an atrophied gate).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(normative_criteria_as_gatekeeping, empirical, 'Whether normative criteria function as objective gatekeeping or selective enforcement.').

omega_variable(
    neo_colonial_imposition,
    'Does the requirement for liberal democratic governance constitute a structural imposition of Western governance models on post-colonial and non-Western societies, effectively making self-determination conditional on adopting liberal norms?',
    'Historical analysis of sovereignty claims denied based on non-liberal governance; testimony from post-colonial and non-aligned states about the constraint''s perceived legitimacy; comparison with pre-1945 statehood law that had no normative criteria.',
    'If this is accurately characterized as neo-colonial imposition, the constraint''s extraction from post-colonial and non-liberal entities is substantially higher (χ approaches 1.0 for victim seats), and the constraint should be reclassified as snare rather than tangled_rope. The legitimacy structure itself becomes contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(neo_colonial_imposition, conceptual, 'Whether normative statehood criteria constitute neo-colonial governance imposition.').

omega_variable(
    humanitarian_intervention_justification,
    'Does the hybrid reading supply genuinely novel legal justification for humanitarian intervention, or does it merely legitimize interventions that would occur regardless of statehood doctrine?',
    'Counterfactual analysis: comparing the timing, frequency, and success of humanitarian interventions under the declaratory reading vs. hybrid reading; examining whether intervention advocates cite statehood-normative failure as the primary legal justification or as a secondary cover story.',
    'If the hybrid reading is the primary justification, it is creating a new extraction mechanism (humanitarian intervention as a tool of liberal state power). If it is a cover story, the constraint is pure snare rationalization. Either way, the extracted value is higher than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_intervention_justification, empirical, 'Whether the hybrid reading supplies novel or post-hoc justification for intervention.').

omega_variable(
    indigenous_peoples_and_non_liberal_governance,
    'How does the hybrid reading''s requirement for liberal democracy affect the statehood claims of indigenous peoples and nations with customary or communal governance models that do not fit Western liberal categories?',
    'Analysis of indigenous statehood claims and how the normative criteria are applied to them; assessment of whether indigenous governance models are recognized as legitimate alternatives to liberal democracy or treated as deficient.',
    'If indigenous governance is treated as deficient, the victim set expands and the constraint''s extraction from indigenous and marginalized populations is severe (χ near 1.0). The constraint becomes a mechanism for foreclosing non-liberal self-determination entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_peoples_and_non_liberal_governance, conceptual, 'Whether the constraint forecloses non-liberal governance models for indigenous and marginalized populations.').

omega_variable(
    reading_contest_and_implementation,
    'What is the relationship between the three readings (declaratory, constitutive, hybrid) in actual UN and international legal practice? Are they genuinely competing frameworks, or does the hybrid reading dominate in practice while the other two remain abstract positions?',
    'Institutional analysis of UN General Assembly recognition votes, UN Security Council positions, and state practice in recognizing new entities over the past 75 years; mapping which reading each state adopts as its position.',
    'If the hybrid reading dominates practice, the constraint is locked in. If the readings genuinely coexist with competing states adopting different readings, the constraint is contested and unstable. The distribution of readings across state positions would indicate whether the constraint is enforced consensus or maintained by a coalition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_and_implementation, empirical, 'The actual institutional distribution of the three readings across state positions and practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t0, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mont_tr_t8, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(mont_tr_t16, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(mont_tr_t24, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(mont_tr_t32, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(mont_tr_t40, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(mont_tr_t50, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(mont_be_t0, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(mont_be_t8, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(mont_be_t16, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(mont_be_t24, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(mont_be_t32, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(mont_be_t40, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(mont_be_t50, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 50, 0.67).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t0, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(mont_su_t8, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(mont_su_t16, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(mont_su_t24, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(mont_su_t32, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(mont_su_t40, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement(mont_su_t50, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(montevideo_statehood_criteria__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria__declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria__constitutive_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, humanitarian_intervention_doctrine).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, recognition_as_legal_personhood).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, self_determination_vs_sovereignty).

% DUAL FORMULATION NOTE:
% The montevideo_statehood_criteria kernel has three structurally distinct constraint readings: declaratory (objective criteria alone), constitutive (recognition-based), and hybrid (objective plus normative). This story instantiates the hybrid reading. The three readings have different ε values, beneficiary/victim structures, and types because they instantiate different institutional mechanisms for determining statehood. The declaratory reading treats statehood as a legal fact (ε low, mountain candidate). The constitutive reading treats it as a social construction (ε moderate, rope or snare depending on coalition). The hybrid reading treats it as gatekept coordination (ε high, tangled rope or snare depending on whether normative criteria are genuinely universal or selectively applied). Each reading must be authored as a separate constraint with its own stakeholders and metrics; the readings are linked via network.affects_constraints because each reading's adoption would change the institutional conditions for the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(montevideo_statehood_criteria__hybrid_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
