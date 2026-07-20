% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__liberal_nationalist_reading, []).

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
 *   constraint_id: jewish_self_determination__liberal_nationalist_reading
 *   human_readable: Liberal-Nationalist Reading of Jewish Self-Determination
 *   domain: political/nationalism/postcolonial
 *
 * SUMMARY:
 *   This constraint instantiates the liberal-nationalist reading of the
 *   contested jewish_self_determination kernel. It posits that Jewish people
 *   constitute a nation with equal claim to self-determination as other
 *   peoples, coordinating international recognition and (ideally) territorial
 *   partition as the just resolution of competing claims. It is one of five
 *   sibling readings; the others (diasporist, indigenous return, religious
 *   covenant, settler-colonial) are structurally distinct constraints linked
 *   via network edges. This reading assumes that secular, universal national
 *   equality is the appropriate framework and that partition can resolve the
 *   territorial competition without creating victims in principle.
 *
 * KEY AGENTS:
 *   - Jewish diaspora refugee seekers: beneficiary (moderate/constrained) â gain sovereignty and refuge through the framework
 *   - UN partition architects: agenda setter (institutional/analytical) â administer the coordination mechanism
 *   - Palestinian national movement: excluded (organized/constrained) â competing claimants largely external to the original coordination design
 *   - Diasporist Jewish critics: excluded (moderate/mobile) â reject the territorial premise
 *   - Postcolonial observers: observer (analytical/analytical) â analyze the gap between symmetry premise and implementation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__liberal_nationalist_reading, 0.35).
domain_priors:suppression_score(jewish_self_determination__liberal_nationalist_reading, 0.25).
domain_priors:theater_ratio(jewish_self_determination__liberal_nationalist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__liberal_nationalist_reading, rope).
narrative_ontology:human_readable(jewish_self_determination__liberal_nationalist_reading, "Liberal-Nationalist Reading of Jewish Self-Determination").
narrative_ontology:topic_domain(jewish_self_determination__liberal_nationalist_reading, "political/nationalism/postcolonial").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__liberal_nationalist_reading, 'e01dc37a-4939-4b80-8cfe-3b8762cfc62b').
narrative_ontology:cs_kernel_codification('e01dc37a-4939-4b80-8cfe-3b8762cfc62b', formalized).
narrative_ontology:cs_authority_grounding('e01dc37a-4939-4b80-8cfe-3b8762cfc62b', lineage).
narrative_ontology:cs_interpretation_layer_present('e01dc37a-4939-4b80-8cfe-3b8762cfc62b').
narrative_ontology:cs_reading_relation('e01dc37a-4939-4b80-8cfe-3b8762cfc62b', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e01dc37a-4939-4b80-8cfe-3b8762cfc62b', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('e01dc37a-4939-4b80-8cfe-3b8762cfc62b', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('e01dc37a-4939-4b80-8cfe-3b8762cfc62b', jewish_self_determination__settler_colonial_reading, influences).
narrative_ontology:cs_axiom('e01dc37a-4939-4b80-8cfe-3b8762cfc62b', foundational, jewish_peoplehood_constitutes_nation).
narrative_ontology:cs_axiom_status(jewish_peoplehood_constitutes_nation, holdable).
narrative_ontology:cs_axiom_grounding('e01dc37a-4939-4b80-8cfe-3b8762cfc62b', jewish_peoplehood_constitutes_nation, empirically_contingent).
narrative_ontology:cs_axiom('e01dc37a-4939-4b80-8cfe-3b8762cfc62b', foundational, equal_claim_to_self_determination).
narrative_ontology:cs_axiom_status(equal_claim_to_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('e01dc37a-4939-4b80-8cfe-3b8762cfc62b', equal_claim_to_self_determination, deontological).
narrative_ontology:cs_reference_frame('e01dc37a-4939-4b80-8cfe-3b8762cfc62b', westphalian_national_equality).
narrative_ontology:cs_drift_state('e01dc37a-4939-4b80-8cfe-3b8762cfc62b', post_1967_occupation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e01dc37a-4939-4b80-8cfe-3b8762cfc62b', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_refuge_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Stateless or minority Jewish communities in Europe and elsewhere who sought refuge and sovereign self-protection through international recognition of Jewish nationhood. Their exit from the constraint is limited by the absence of alternative state-bearing frameworks that guarantee collective security.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_refuge_seekers, beneficiary,
    moderate, biographical, constrained, global).

% International legal and diplomatic bodies (League of Nations, UN, Western foreign ministries) that formalized the partition and recognition framework. They administer the principle of national self-determination as a coordinating rule for post-imperial territorial disputes and bear no direct cost of implementation.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, un_partition_architects, agenda_setter,
    institutional, generational, analytical, global).

% Represents the competing national claim to the same territory. Rejected the 1947 partition recommendation and contests the framework's assumption that partition justly resolves competing claims. Structurally excluded from full parity in the original coordination design despite the reading's theoretical symmetry.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, palestinian_national_movement, excluded,
    organized, generational, constrained, national).

% Jewish intellectual and communal currents that reject territorial sovereignty as the appropriate form of Jewish collective life, arguing instead for diaspora minority rights, cultural autonomy, and anti-assimilationism without statehood.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, diasporist_jewish_critics, excluded,
    moderate, generational, mobile, global).

% Advance a competing reading grounding Jewish territorial title in divine covenant rather than secular national equality. They participate in the same political project but reject the liberal nationalist epistemology and authority structure.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, religious_zionist_theocrats, excluded,
    organized, civilizational, identity_locked, national).

% Analyze the liberal nationalist reading as a universalist legitimization narrative that may mask settler-colonial asymmetries under the formal equality of nations. They observe the divergence between the reading's symmetry premise and its territorial implementation.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, postcolonial_academic_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To resolve competing national claims to Palestine by applying the universal principle of national self-determination to the Jewish people, coordinating international recognition and territorial partition as a mechanism for refugee resettlement and sovereign equality among nations.
% TRANSFER_FUNCTION: Moves international diplomatic recognition, refugee resettlement capacity, and territorial sovereignty from the pre-war imperial order to a Jewish national state, while nominally reserving equivalent self-determination rights to other national communities in the same territory.
% ABSENT_VOICES: Palestinian Arab nationalists who reject partition as unjust imposition; Jewish diasporists who deny that statehood is the appropriate form of Jewish collective life; religious Zionists who reject the secular-nationalist grounding of the claim.
% DISAPPEARANCE_RATIONALE: If the principle vanished overnight, the legal and moral architecture underwriting Jewish statehood would revert to contested theological or colonial framings, and the post-1945 international refugee settlement order would lose a key coordinating premise for Jewish collective security.
% FOUNDING_PROBLEM: European Jewish statelessness, minority persecution, and the refugee crisis of the early twentieth century; the pre-state Yishuv's lack of internationally recognized political autonomy in Palestine.
% FOUNDING_PROBLEM_CORROBORATION: British Peel Commission archives and UN Special Committee on Palestine (UNSCOP) records document the statelessness and persecution problem from outside the Zionist beneficiary set, though both were commissioned by imperial powers with their own strategic interests. Postcolonial historians contest whether the solution addressed the founding problem or created a new dispossession.
narrative_ontology:disappearance_verdict(jewish_self_determination__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__liberal_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__liberal_nationalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__liberal_nationalist_reading, 0.35, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__liberal_nationalist_reading_tests).
:- end_tests(jewish_self_determination__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.35 (low-to-moderate) because the claim channels recognition and territorial sovereignty specifically to Jewish statehood, but frames the transfer as symmetrical and universal. Suppression is 0.25 because the reading's own logic does not require active suppression of alternatives (diasporist, settler-colonial critiques are marginalized by discursive dominance rather than structural coercion). Theater_ratio is 0.30 and rising: the coordination function (mutual recognition, partition) has atrophied into ritual advocacy as the two-state solution recedes. Accessibility_collapse is 0.45 because once one accepts the national-equalization framework, binational or diasporist alternatives become less cognitively accessible. Resistance is 0.55 because the reading faces sustained opposition from Palestinian, postcolonial, and diasporist seats.
 *
 * PERSPECTIVAL GAP:
 *   The Jewish diaspora beneficiary seat experiences the constraint as genuine coordination (refuge, sovereignty, international legitimacy). The Palestinian excluded seat experiences the same framework as legitimizing a territorial outcome that truncates their claim. The diasporist excluded seat experiences it as a dangerous deviation from Jewish historical destiny. The engine should compute significant seat divergence: low effective extraction for the beneficiary and agenda-setter seats, substantially higher for the excluded Palestinian seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish diaspora refugees are declared beneficiaries, deriving low directionality and damped effective extraction. UN partition architects are agenda-setters with analytical exit, also near the beneficiary end. Palestinian nationalists are excluded from the coordination benefit and have constrained exit, placing them nearer the target end despite not being declared victims (the reading denies victimhood, but the structural position extracts territorial possibility). Diasporist critics have mobile exit and are excluded, placing them at moderate directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â Jewish statelessness and persecution â was live in the early twentieth century. The reading's status is contested: beneficiaries argue the problem persists (antisemitism, security threats), while observers note the arrangement has outlived its original refugee-crisis context and now persists as an institutionalized claim. The T17 abductive trigger is relevant if base_extractiveness accumulates further, but the authored metrics show a plateau rather than continuous accumulation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the liberal nationalist reading a structurally distinct constraint, or does its classification collapse into the settler-colonial or indigenous readings once empirical premises about Jewish historical continuity or colonial violence are settled?',
    'Comparative historical sociology of Jewish collective identity formation and archival analysis of Zionist institutional dependency on European imperial powers.',
    'If Jewish nationhood is shown to be a modern political construct without historical continuity, the empirically_contingent axiom weakens and the reading may drift toward either a purely instrumental scaffold or a false-summit mountain; if indigenous continuity is established, the reading may merge with the indigenous return reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether this kernel reading maintains independent structural identity or collapses into sibling readings upon empirical settlement.').

omega_variable(
    partition_mutual_recognition_viability,
    'Does the coordination function of this reading depend on a territorial partition and mutual recognition that is no longer politically feasible, converting a rope into a piton or tangled rope?',
    'Longitudinal analysis of two-state solution polling, settlement expansion data, and Palestinian sovereignty indicators.',
    'If partition is permanently blocked, the coordination function atrophies and the reading becomes performative (theater_ratio rises), either degrading to piton or, if enforcement against Palestinian claims intensifies, to tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_mutual_recognition_viability, empirical, 'Whether the reading''s core coordination mechanism remains structurally viable or has become theatrical.').

omega_variable(
    nationhood_empirical_status,
    'Is Jewish peoplehood under this reading an empirically continuous nation or a modern political construct?',
    'Interdisciplinary consensus or dispute among historians, sociologists, and anthropologists regarding the pre-modern existence of Jewish national consciousness versus modern Zionist nation-building.',
    'If the empirical premise of continuous nationhood is refuted, the foundational empirically_contingent axiom is overridden, weakening the reading''s authority and potentially triggering axiom_overriding drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nationhood_empirical_status, empirical, 'Empirical status of the Jewish nationhood claim underwriting the reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__liberal_nationalist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jewi_tr_t20, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(jewi_tr_t40, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(jewi_tr_t60, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(jewi_tr_t80, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 80, 0.28).
narrative_ontology:measurement(jewi_tr_t100, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(jewi_be_t20, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(jewi_be_t40, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(jewi_be_t60, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(jewi_be_t80, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 80, 0.38).
narrative_ontology:measurement(jewi_be_t100, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 100, 0.35).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(jewish_self_determination__liberal_nationalist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__liberal_nationalist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, diasporist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, settler_colonial_reading).

% DUAL FORMULATION NOTE:
% The jewish_self_determination kernel decomposes into five structurally distinct constraints (readings) because the natural-language label conflates claims with different epsilon values, beneficiary structures, and authority groundings. Each reading has its own constraint_id and is linked to its siblings via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
