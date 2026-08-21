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
 *   This constraint story instantiates the 'liberal_nationalist_reading' of
 *   the 'jewish_self_determination' kernel. From this perspective, the Jewish
 *   people constitute a nation with an equal claim to self-determination as
 *   other peoples, grounded in universal principles of national rights. The
 *   constraint is framed as a Rope, aiming to coordinate competing national
 *   claims through mutual recognition and, ideally, territorial partition,
 *   leading to low-to-moderate extraction. The metrics reflect the inherent
 *   friction and costs of implementing such a claim in a contested
 *   geopolitical context, but from this reading's own lights, it is
 *   fundamentally a coordination mechanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__liberal_nationalist_reading, 0.45).
domain_priors:suppression_score(jewish_self_determination__liberal_nationalist_reading, 0.3).
domain_priors:theater_ratio(jewish_self_determination__liberal_nationalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__liberal_nationalist_reading, rope).
narrative_ontology:human_readable(jewish_self_determination__liberal_nationalist_reading, "Jewish Self-Determination (Liberal Nationalist Reading)").
narrative_ontology:topic_domain(jewish_self_determination__liberal_nationalist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__liberal_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__liberal_nationalist_reading, 'b8ed326a-2cfb-4d42-93b4-d33d498cd5ce').
narrative_ontology:cs_kernel_codification('b8ed326a-2cfb-4d42-93b4-d33d498cd5ce', formalized).
narrative_ontology:cs_authority_grounding('b8ed326a-2cfb-4d42-93b4-d33d498cd5ce', lineage).
narrative_ontology:cs_interpretation_layer_present('b8ed326a-2cfb-4d42-93b4-d33d498cd5ce').
narrative_ontology:cs_reading_relation('b8ed326a-2cfb-4d42-93b4-d33d498cd5ce', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8ed326a-2cfb-4d42-93b4-d33d498cd5ce', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8ed326a-2cfb-4d42-93b4-d33d498cd5ce', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8ed326a-2cfb-4d42-93b4-d33d498cd5ce', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_axiom('b8ed326a-2cfb-4d42-93b4-d33d498cd5ce', foundational, national_self_determination_is_universal_right).
narrative_ontology:cs_axiom_status(national_self_determination_is_universal_right, holdable).
narrative_ontology:cs_axiom_grounding('b8ed326a-2cfb-4d42-93b4-d33d498cd5ce', national_self_determination_is_universal_right, deontological).
narrative_ontology:cs_axiom('b8ed326a-2cfb-4d42-93b4-d33d498cd5ce', foundational, territorial_sovereignty_is_legitimate_expression_of_nationhood).
narrative_ontology:cs_axiom_status(territorial_sovereignty_is_legitimate_expression_of_nationhood, holdable).
narrative_ontology:cs_axiom_grounding('b8ed326a-2cfb-4d42-93b4-d33d498cd5ce', territorial_sovereignty_is_legitimate_expression_of_nationhood, conventional).
narrative_ontology:cs_reference_frame('b8ed326a-2cfb-4d42-93b4-d33d498cd5ce', universal_national_self_determination).
narrative_ontology:cs_drift_state('b8ed326a-2cfb-4d42-93b4-d33d498cd5ce', contemporary_postcolonial_critique, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b8ed326a-2cfb-4d42-93b4-d33d498cd5ce', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_seeking_sovereignty).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_self_determination__liberal_nationalist_reading, other_national_groups_in_contested_territory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seeks to establish or maintain a sovereign state for the Jewish people, grounded in the universal right to national self-determination. Benefits from international recognition and the security of a national home.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_seeking_sovereignty, beneficiary,
    organized, generational, constrained, global).

% May bear the costs of territorial partition or the recognition of a competing national claim, even when framed within liberal principles. Their self-determination claims may conflict with the implementation of this constraint.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, other_national_groups_in_contested_territory, payer,
    organized, generational, constrained, regional).

% Acts as a mediator and arbiter of national claims, providing frameworks for recognition, statehood, and conflict resolution. Its legitimacy derives from upholding universal principles of self-determination.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, international_community, agenda_setter,
    institutional, civilizational, analytical, global).

% Develop and advocate for the theoretical framework that grounds Jewish self-determination in universal liberal principles, emphasizing mutual recognition and peaceful coexistence. They analyze the consistency and implications of the claim.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, liberal_nationalist_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the recognition of the Jewish people's right to self-determination within a framework of universal national rights, aiming for peaceful coexistence and mutual recognition among national groups.
% TRANSFER_FUNCTION: Transfers recognition, legitimacy, and potentially territory/resources to the Jewish people for the establishment or maintenance of a sovereign state, ideally through negotiated partition or mutual agreement.
% ABSENT_VOICES: Those who reject the concept of national self-determination for the Jewish people, or who assert exclusive claims to the same territory, are often excluded from the framing of this particular reading, as it assumes a framework of universal, mutually recognized national rights.
% DISAPPEARANCE_RATIONALE: If the claim to Jewish national self-determination, as understood through a liberal nationalist lens, vanished overnight, the geopolitical landscape of the Middle East and the global discourse on national rights would fundamentally shift, leading to a rearrangement of alliances, claims, and conflicts. The international legal framework for national self-determination would also be challenged.
% FOUNDING_PROBLEM: The historical persecution and statelessness of Jewish people, culminating in the Holocaust, demonstrated the need for a secure national home and the right to self-determination to ensure collective safety and cultural flourishing, consistent with universal principles of national rights.
% FOUNDING_PROBLEM_CORROBORATION: Historians, international legal scholars, and human rights organizations (outside of direct beneficiaries) corroborate the historical context of Jewish statelessness and persecution as a foundational problem for the emergence of Zionist thought and the push for self-determination, framing it within the broader context of national liberation movements.
narrative_ontology:disappearance_verdict(jewish_self_determination__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__liberal_nationalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__liberal_nationalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jewish_self_determination__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__liberal_nationalist_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.45) and suppression (0.30) are low-to-moderate, reflecting the liberal nationalist ideal of a non-coercive, mutually recognized self-determination, while acknowledging the practical difficulties and inherent friction in achieving it. The 'none in principle' victim declaration for this reading assumes that territorial partition and mutual recognition can resolve competing claims without creating victims. Theater ratio is low (0.10) as the claim is understood as a genuine political and moral imperative, not a performance. The slight increase in extractiveness and suppression over time reflects the ongoing challenges and conflicts in implementing this ideal.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Jewish diaspora seeking sovereignty, this constraint is a Rope, coordinating their national aspirations with international norms. However, from the perspective of other national groups in contested territories, the implementation of this claim, even under liberal nationalist principles, can lead to perceived extraction or dispossession, making it feel more like a Tangled Rope or Snare due to the inherent zero-sum nature of territorial claims. This divergence is a key point of contestation around the kernel.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish diaspora seeking sovereignty is the primary beneficiary, gaining recognition and a national home. Other national groups in contested territories are positioned as payers, as they bear the costs of partition or the recognition of a competing claim. The international community acts as an agenda-setter, mediating and legitimizing claims within a universal framework. Liberal nationalist theorists are observers, analyzing the consistency and implications of the claim.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''liberal_nationalist_reading'' of the ''jewish_self_determination'' kernel. What are the implications of this specific framing?',
    'Comparative analysis with sibling readings to identify how different framings alter perceived beneficiaries, victims, and overall constraint type.',
    'Understanding the specific framing clarifies the normative assumptions and expected outcomes of this particular claim to self-determination, distinguishing it from other, potentially more extractive or less inclusive, interpretations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identifies this constraint as a specific reading of a contested kernel.').

omega_variable(
    territorial_partition_feasibility,
    'The ''none in principle'' victim declaration for this reading relies on the feasibility of territorial partition and mutual recognition. Is this assumption empirically viable in contested territories?',
    'Empirical studies of historical and contemporary partition efforts, analysis of demographic realities, and assessment of political will for mutual recognition among all parties.',
    'If partition is not feasible or mutual recognition fails, the ''none in principle'' victim status would be invalidated, and the constraint would likely reclassify towards a Tangled Rope or Snare due to inherent dispossession or extraction from other groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_partition_feasibility, empirical, 'Assesses the empirical viability of the liberal nationalist assumption of victim-free self-determination through partition.').

omega_variable(
    universal_application_consistency,
    'Is the principle of self-determination, as applied to the Jewish people in this reading, consistently applied to all other national groups, particularly those in contested territories?',
    'Comparative legal and political analysis of how self-determination claims are recognized and implemented for various national groups globally, especially in situations of competing claims.',
    'Inconsistent application would undermine the universalist grounding of this reading, potentially reclassifying it as a Tangled Rope if the inconsistency benefits the primary beneficiary at the expense of others, or a Snare if it actively suppresses other claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_application_consistency, conceptual, 'Examines the consistency of applying universal self-determination principles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__liberal_nationalist_reading, 1948, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(jewi_tr_t1963, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1963, 0.1).
narrative_ontology:measurement(jewi_tr_t1978, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1978, 0.1).
narrative_ontology:measurement(jewi_tr_t1993, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(jewi_tr_t2008, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 2008, 0.1).
narrative_ontology:measurement(jewi_tr_t2023, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1948, 0.35).
narrative_ontology:measurement(jewi_be_t1963, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1963, 0.38).
narrative_ontology:measurement(jewi_be_t1978, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1978, 0.4).
narrative_ontology:measurement(jewi_be_t1993, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1993, 0.42).
narrative_ontology:measurement(jewi_be_t2008, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 2008, 0.44).
narrative_ontology:measurement(jewi_be_t2023, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 2023, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1948, 0.2).
narrative_ontology:measurement(jewi_su_t1963, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1963, 0.22).
narrative_ontology:measurement(jewi_su_t1978, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1978, 0.25).
narrative_ontology:measurement(jewi_su_t1993, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1993, 0.27).
narrative_ontology:measurement(jewi_su_t2008, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 2008, 0.29).
narrative_ontology:measurement(jewi_su_t2023, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 2023, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__liberal_nationalist_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
