% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__national_liberation_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: zionist_legitimacy_basis__national_liberation_reading
 *   human_readable: Zionist Legitimacy: National Liberation Reading
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   This constraint instantiates the national liberation reading of the
 *   Zionist legitimacy kernel: the claim that Zionism is an indigenous
 *   national liberation movement of a persecuted people returning to its
 *   ancestral homeland. It functions as both a genuine coordination mechanism
 *   for Jewish diaspora solidarity and state-building, and as an asymmetric
 *   extraction mechanism that justifies Palestinian displacement and
 *   occupation. The claim/metric independence is maintained: the reading is
 *   claimed as a coordination narrative, while the metrics describe
 *   substantial extraction, active suppression, and moderate theater.
 *
 * KEY AGENTS:
 *   - Israeli State: Primary agenda-setter (institutional/constrained) â administers the territorial and military enforcement of the constraint.
 *   - Zionist Jewish Community: Primary beneficiary (organized/identity_locked) â receives identity coordination and homeland attachment; exit is socially and relationally costly.
 *   - Palestinian Arabs: Primary target (powerless/trapped) â bears the costs of displacement, occupation, and denial of self-determination.
 *   - Anti-Zionist Jews: Excluded voice (moderate/identity_locked) â structurally marginalized within the Jewish community for rejecting the national liberation frame.
 *   - International Human Rights Bodies: Analytical observer (institutional/analytical) â documents violations but lacks enforcement leverage.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, 0.75).
domain_priors:suppression_score(zionist_legitimacy_basis__national_liberation_reading, 0.8).
domain_priors:theater_ratio(zionist_legitimacy_basis__national_liberation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__national_liberation_reading, "Zionist Legitimacy: National Liberation Reading").
narrative_ontology:topic_domain(zionist_legitimacy_basis__national_liberation_reading, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__national_liberation_reading, '9e831816-3521-4ef6-a540-1fbab2bf1a74').
narrative_ontology:cs_kernel_codification('9e831816-3521-4ef6-a540-1fbab2bf1a74', formalized).
narrative_ontology:cs_authority_grounding('9e831816-3521-4ef6-a540-1fbab2bf1a74', lineage).
narrative_ontology:cs_interpretation_layer_present('9e831816-3521-4ef6-a540-1fbab2bf1a74').
narrative_ontology:cs_reading_relation('9e831816-3521-4ef6-a540-1fbab2bf1a74', zionist_legitimacy_basis__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('9e831816-3521-4ef6-a540-1fbab2bf1a74', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('9e831816-3521-4ef6-a540-1fbab2bf1a74', foundational, persecuted_indigenous_return_right).
narrative_ontology:cs_axiom_status(persecuted_indigenous_return_right, holdable).
narrative_ontology:cs_axiom_grounding('9e831816-3521-4ef6-a540-1fbab2bf1a74', persecuted_indigenous_return_right, deontological).
narrative_ontology:cs_axiom('9e831816-3521-4ef6-a540-1fbab2bf1a74', foundational, anti_zionism_as_rights_denial).
narrative_ontology:cs_axiom_status(anti_zionism_as_rights_denial, holdable).
narrative_ontology:cs_axiom_grounding('9e831816-3521-4ef6-a540-1fbab2bf1a74', anti_zionism_as_rights_denial, conventional).
narrative_ontology:cs_reference_frame('9e831816-3521-4ef6-a540-1fbab2bf1a74', indigenous_liberation_framework).
narrative_ontology:cs_drift_state('9e831816-3521-4ef6-a540-1fbab2bf1a74', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9e831816-3521-4ef6-a540-1fbab2bf1a74', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, zionist_jewish_community).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinian_arabs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the territorial state, military occupation, and diplomatic defense of the national liberation narrative. Controls the institutional machinery that enforces the constraint's boundaries and maintains the exclusivity of the Jewish national character within the polity.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, israeli_state, agenda_setter,
    institutional, generational, constrained, national).

% Receives identity coordination, diaspora homeland attachment, and perceived security guarantee from the existence of a Jewish state. Exit from the Zionist identity framework is socially costly within the organized community, making the constraint self-reinforcing through relational identity.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, zionist_jewish_community, beneficiary,
    organized, generational, identity_locked, global).

% Bear the costs of displacement, military occupation, settlement expansion, and denial of collective self-determination. Exit options are limited to exile, fragmented autonomy under military rule, or acceptance of permanently subordinate civic status.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_arabs, payer,
    powerless, generational, trapped, national).

% Excluded from the national liberation narrative; their Jewish identity is used to delegitimize their opposition. They face social exclusion from the organized Jewish community and are rendered invisible in mainstream diaspora institutions.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, anti_zionist_jews, excluded,
    moderate, biographical, identity_locked, global).

% Monitor and report on violations of international humanitarian law and Palestinian rights. Their findings are contested by the agenda-setter and often disregarded by beneficiaries, creating a persistent legitimacy gap between legal interpretation and political practice.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective survival, military defense, and state-building of a dispersed persecuted people by unifying diaspora communities around a territorial project and institutional homeland, solving the collective-action problem of nation-building without a territorial base.
% TRANSFER_FUNCTION: Moves land, sovereignty, and demographic control from the indigenous Palestinian population to the Jewish Zionist community and the Israeli state, underwritten by the moral and diplomatic capital of the national liberation claim.
% ABSENT_VOICES: Palestinian refugees and their descendants, anti-Zionist Jews, and advocates of binational or one-state equality are structurally excluded from the national liberation framework; their claims are delegitimized as denial of Jewish rights or as security threats rather than heard as legitimate political opposition.
% DISAPPEARANCE_RATIONALE: The constraint underwrites the Israeli state's territorial claims, its military and diplomatic posture, and the global Jewish diaspora's relationship to Israel. Without it, the territorial claims become contested settler claims without the protective framing of liberation, diaspora solidarity loses its moral anchor, and Palestinian claims would reconfigure regional politics fundamentally.
% FOUNDING_PROBLEM: The historical persecution, statelessness, and vulnerability of Jewish populations in Europe and the Middle East, culminating in the Holocaust, created an existential need for a sovereign territorial refuge and self-determination.
% FOUNDING_PROBLEM_CORROBORATION: Jewish historians and Holocaust chroniclers attest to the historical persecution from outside the immediate beneficiary seat; Palestinian historians and post-colonial scholars contest that the founding problem justifies the current extraction, arguing the arrangement has outlived its defensive rationale and now perpetuates a new victimization. No consensus from outside all benefiting parties exists.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__national_liberation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__national_liberation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__national_liberation_reading, 0.75, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__national_liberation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the constraint transfers land, sovereignty, and demographic control to one group while extracting them from another. Suppression is higher (0.80) because the arrangement requires active military, legal, and diplomatic enforcement to maintain the exclusivity of the national liberation framework against Palestinian resistance and international law challenges. Theater ratio is moderate (0.40): state-building and military defense are genuine, but a significant share of institutional activity is performative maintenance of the liberation narrative in the face of occupation and settlement practices that contradict it. Accessibility collapse (0.65) reflects that binational or one-state alternatives are heavily suppressed in mainstream discourse though they persist globally. Resistance (0.70) is high due to persistent Palestinian opposition and growing international dissent.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience the constraint as necessary coordination for collective survival, while the payer and excluded seats experience it as enforced extraction and delegitimation. The engine computes this divergence from the structural data: same constraint, opposite directionalities, different computed seat types.
 *
 * DIRECTIONALITY LOGIC:
 *   The Zionist Jewish community and the Israeli state sit near the beneficiary end of the directionality axis: the constraint subsidizes their collective identity and territorial control. Palestinian Arabs sit near the full-target end: the constraint extracts land, political rights, and self-determination from them. Anti-Zionist Jews, though Jewish, are positioned as excluded rather than beneficiaries because the constraint's identity coordination function is conditional on acceptance of the national liberation frame; their rejection locks them into a high-cost outsider status within the community.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of Jewish persecution and statelessness has been substantially addressed by state formation, yet the constraint persists and has expanded into occupation and settlement. This prevents mislabeling the current structure as pure coordination (rope) by documenting the accumulated extraction. It also prevents mislabeling it as pure snare by acknowledging the genuine coordination function for Jewish collective survival that would not be replicated by a simple extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the national liberation reading of Zionist legitimacy exhaust the kernel, or do the settler-colonial and religious restoration readings capture structural elements this reading obscures?',
    'Comparative analysis of the three constraint stories and their metric profiles; empirical assessment of whether displacement was necessitated by persecution or driven by settler-colonial logic.',
    'If the settler-colonial reading captures more of the constraint''s operational structure, this reading''s classification as tangled_rope may be too generous â the coordination function may be cover rather than genuine dual function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Structural ambiguity between liberation and settler-colonial framings of the same kernel.').

omega_variable(
    indigenous_status_empirical_basis,
    'Is the claim of Jewish indigenous status to the Levant historically and anthropologically sufficient to ground a national liberation framework, or does it function as a constructed political category?',
    'Independent historical and anthropological review of indigenous continuity claims, comparing diaspora Jewish communities to other recognized indigenous peoples.',
    'If the indigenous claim is weak empirically, the national liberation reading loses its foundational warrant and collapses toward either religious restoration (theological grounding) or settler-colonial (constructed migration).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_status_empirical_basis, empirical, 'Empirical basis of the indigenous status claim underlying the national liberation reading.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is Palestinian compliance with the constraint achieved primarily through structural military coercion or through internalized narrative acceptance of Jewish entitlement?',
    'Measurement of resistance rates, survey of political attitudes under occupation, and analysis of compliance in areas of limited direct military presence.',
    'If internalized, effective extraction is higher than structural measures suggest; if purely structural, removal of military enforcement might rapidly collapse the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism for Palestinian subordination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__national_liberation_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t0, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(zion_tr_t15, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(zion_tr_t30, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(zion_tr_t45, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 45, 0.4).
narrative_ontology:measurement(zion_tr_t60, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(zion_tr_t75, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 75, 0.4).

% Extraction over time
narrative_ontology:measurement(zion_be_t0, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(zion_be_t15, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(zion_be_t30, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(zion_be_t45, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 45, 0.72).
narrative_ontology:measurement(zion_be_t60, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 60, 0.73).
narrative_ontology:measurement(zion_be_t75, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 75, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t0, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(zion_su_t15, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(zion_su_t30, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(zion_su_t45, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 45, 0.72).
narrative_ontology:measurement(zion_su_t60, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 60, 0.74).
narrative_ontology:measurement(zion_su_t75, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 75, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__national_liberation_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, settler_colonial_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, religious_restoration_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the zionist_legitimacy_basis kernel. The national liberation reading and its siblings (settler_colonial_reading, religious_restoration_reading) instantiate structurally distinct constraints from the same ideological kernel, with different epsilon values, beneficiary/victim structures, and types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
