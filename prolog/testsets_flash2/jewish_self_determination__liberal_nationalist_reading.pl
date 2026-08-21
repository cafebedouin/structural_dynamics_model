% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   human_readable: Jewish Self-Determination (Liberal Nationalist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint represents the liberal nationalist reading of Jewish
 *   self-determination, asserting that Jewish people constitute a nation with
 *   an equal claim to self-determination as other peoples. This reading
 *   frames the establishment of a Jewish state as a legitimate exercise of
 *   universal national rights, often advocating for a two-state solution or
 *   territorial partition to accommodate competing claims. It is one reading
 *   of the broader 'Jewish self-determination' kernel, distinct from
 *   religious, indigenous, diasporist, or settler-colonial interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__liberal_nationalist_reading, 0.35).
domain_priors:suppression_score(jewish_self_determination__liberal_nationalist_reading, 0.45).
domain_priors:theater_ratio(jewish_self_determination__liberal_nationalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__liberal_nationalist_reading, rope).
narrative_ontology:human_readable(jewish_self_determination__liberal_nationalist_reading, "Jewish Self-Determination (Liberal Nationalist Reading)").
narrative_ontology:topic_domain(jewish_self_determination__liberal_nationalist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__liberal_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__liberal_nationalist_reading, 'a3a2a15b-8a7f-4439-8a39-fb69780b622a').
narrative_ontology:cs_kernel_codification('a3a2a15b-8a7f-4439-8a39-fb69780b622a', formalized).
narrative_ontology:cs_authority_grounding('a3a2a15b-8a7f-4439-8a39-fb69780b622a', lineage).
narrative_ontology:cs_interpretation_layer_present('a3a2a15b-8a7f-4439-8a39-fb69780b622a').
narrative_ontology:cs_reading_relation('a3a2a15b-8a7f-4439-8a39-fb69780b622a', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('a3a2a15b-8a7f-4439-8a39-fb69780b622a', jewish_self_determination__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('a3a2a15b-8a7f-4439-8a39-fb69780b622a', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('a3a2a15b-8a7f-4439-8a39-fb69780b622a', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_axiom('a3a2a15b-8a7f-4439-8a39-fb69780b622a', foundational, universal_right_to_national_self_determination).
narrative_ontology:cs_axiom_status(universal_right_to_national_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('a3a2a15b-8a7f-4439-8a39-fb69780b622a', universal_right_to_national_self_determination, deontological).
narrative_ontology:cs_axiom('a3a2a15b-8a7f-4439-8a39-fb69780b622a', secondary, territorial_partition_as_equitable_solution).
narrative_ontology:cs_axiom_status(territorial_partition_as_equitable_solution, holdable).
narrative_ontology:cs_axiom_grounding('a3a2a15b-8a7f-4439-8a39-fb69780b622a', territorial_partition_as_equitable_solution, instrumental).
narrative_ontology:cs_reference_frame('a3a2a15b-8a7f-4439-8a39-fb69780b622a', post_wwii_liberal_international_order).
narrative_ontology:cs_drift_state('a3a2a15b-8a7f-4439-8a39-fb69780b622a', contemporary_postcolonial_critique, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('a3a2a15b-8a7f-4439-8a39-fb69780b622a', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_seeking_sovereignty).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_self_determination__liberal_nationalist_reading, palestinian_people_seeking_self_determination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seeks a secure homeland and self-determination for Jewish people, viewing it as a universal right applicable to all nations. Benefits from the recognition of this claim in international forums and by other nations.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_seeking_sovereignty, beneficiary,
    organized, generational, constrained, global).

% Bears the costs of competing claims to the same territory, often experiencing displacement, occupation, and denial of their own national rights. Their situation is directly impacted by the implementation of Jewish self-determination in a shared land.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, palestinian_people_seeking_self_determination, payer,
    powerless, generational, trapped, regional).

% Acts as a mediator and arbiter of national claims, attempting to coordinate solutions like territorial partition. Its recognition and enforcement of self-determination principles shape the political landscape for both parties.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, international_community, agenda_setter,
    institutional, generational, analytical, global).

% Advocates for the universal right to national self-determination, applying it to the Jewish people as to any other. Seeks to reconcile competing national claims through principles of justice, equality, and mutual recognition, often through partition.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, liberal_nationalist_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate the recognition of Jewish national rights within a framework of universal national self-determination, seeking to resolve competing claims to territory through equitable partition and mutual recognition.
% TRANSFER_FUNCTION: Transfers legitimacy and political agency to the Jewish people to establish and maintain a sovereign state, implicitly requiring a transfer of land and resources from existing inhabitants or a division of shared territory.
% ABSENT_VOICES: Radical anti-Zionist groups who reject any form of Jewish national self-determination, and maximalist Palestinian groups who reject any partition, are often excluded from the liberal nationalist discourse, as their positions are seen as foreclosing a negotiated solution.
% DISAPPEARANCE_RATIONALE: If the liberal nationalist reading of Jewish self-determination vanished, the international legal and political framework for resolving the Israeli-Palestinian conflict would lose a key pillar. The discourse would shift dramatically, potentially leading to more radical or less compromising positions from all sides, and the existing state structures would face a profound legitimacy crisis.
% FOUNDING_PROBLEM: The historical persecution and statelessness of Jewish people, culminating in the Holocaust, demonstrated the need for a secure national home where Jewish people could exercise self-determination and ensure their collective survival.
% FOUNDING_PROBLEM_CORROBORATION: Historians and international legal scholars widely corroborate the historical context of Jewish persecution and statelessness as a foundational problem. While the specific solution (a nation-state) is contested, the underlying problem of Jewish security and self-determination is generally acknowledged by a broad range of non-benefiting parties.
narrative_ontology:disappearance_verdict(jewish_self_determination__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__liberal_nationalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__liberal_nationalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_self_determination__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__liberal_nationalist_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Rope because its core function is to coordinate the recognition of Jewish national rights within a universalist framework, aiming for a solution that respects the self-determination of all peoples. Extractiveness is low-to-moderate (0.35) as it seeks a just partition, but still involves a transfer of land and power that impacts other groups. Suppression (0.45) is present due to the need to enforce borders and manage competing claims, but it's not primarily coercive in its ideal form. Theater ratio is low (0.1) as the claim is genuinely about national rights, not a cover for other agendas within this specific reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Jewish people seeking sovereignty, this is a just and necessary coordination mechanism for their survival and flourishing. From the perspective of Palestinian people, it is a constraint that imposes significant costs and requires active suppression of their own counter-claims, even if framed as 'liberal' or 'universalist'. The engine's classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish diaspora seeking sovereignty are the primary beneficiaries, as the constraint legitimizes their national aspirations. Palestinian people seeking self-determination are the primary payers, as the implementation of this claim in a shared territory directly impacts their land and rights. The international community acts as an agenda-setter, attempting to coordinate a solution based on these principles. Liberal nationalist theorists are observers, analyzing and advocating for this framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a distinct reading of the ''Jewish self-determination'' kernel, or is it conflated with other interpretations?',
    'Analysis of primary texts and political discourse to identify unique axiomatic foundations and policy implications that distinguish this liberal nationalist reading from religious, indigenous, diasporist, or settler-colonial interpretations.',
    'If conflated, the metrics (especially extractiveness and suppression) may be inaccurate, reflecting a composite of different structural claims. If distinct, the classification accurately reflects the specific dynamics of this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ensures the constraint accurately represents one specific reading of the kernel.').

omega_variable(
    partition_feasibility_and_justice,
    'Is equitable territorial partition, as implied by this liberal nationalist reading, genuinely feasible and just for all parties, or does it inherently lead to asymmetric extraction?',
    'Empirical analysis of historical and contemporary partition outcomes, focusing on long-term stability, economic equity, and human rights for all populations involved. Counterfactual modeling of alternative arrangements.',
    'If partition is inherently extractive or unstable, the ''Rope'' classification would be challenged, potentially reclassifying towards ''Tangled Rope'' or ''Snare'' due to higher effective extraction and suppression. If feasible and just, the ''Rope'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_feasibility_and_justice, empirical, 'Assesses the real-world outcome of the liberal nationalist solution.').

omega_variable(
    universalism_vs_particularism,
    'To what extent does the ''universal'' claim of national self-determination, as applied to Jewish people, genuinely extend to other groups (e.g., Palestinians) in practice, or does it become a particularist claim in universalist disguise?',
    'Comparative analysis of policy implementation, legal frameworks, and international advocacy by proponents of this reading, examining consistency in applying self-determination principles to all affected groups. Discourse analysis of ''universalist'' rhetoric versus actual outcomes.',
    'If the claim becomes particularist in practice, the constraint''s suppression and extractiveness would be higher than currently estimated, as it would actively suppress the self-determination of others while claiming universal principles. This would push classification towards ''Tangled Rope'' or ''Snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universalism_vs_particularism, conceptual, 'Examines the practical application of universalist principles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__liberal_nationalist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1948, 0.05).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(jewi_tr_t1993, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1948, 0.3).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1967, 0.4).
narrative_ontology:measurement(jewi_be_t1993, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1993, 0.35).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1948, 0.4).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1967, 0.5).
narrative_ontology:measurement(jewi_su_t1993, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1993, 0.45).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__liberal_nationalist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__diasporist_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
