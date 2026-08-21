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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   This constraint story analyzes the claim that Jewish people constitute a
 *   nation with an equal claim to self-determination as other peoples,
 *   specifically from a liberal nationalist perspective. This reading frames
 *   the issue within universal principles of national rights, often implying
 *   that a just resolution (e.g., territorial partition) can accommodate
 *   competing claims without creating victims. It is one reading of the
 *   broader 'jewish_self_determination' kernel, which is highly contested.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__liberal_nationalist_reading, 0.35).
domain_priors:suppression_score(jewish_self_determination__liberal_nationalist_reading, 0.2).
domain_priors:theater_ratio(jewish_self_determination__liberal_nationalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__liberal_nationalist_reading, rope).
narrative_ontology:human_readable(jewish_self_determination__liberal_nationalist_reading, "Jewish Self-Determination (Liberal Nationalist Reading)").
narrative_ontology:topic_domain(jewish_self_determination__liberal_nationalist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__liberal_nationalist_reading, '8fa21836-a77e-46e8-a47d-d696841c2a5b').
narrative_ontology:cs_kernel_codification('8fa21836-a77e-46e8-a47d-d696841c2a5b', formalized).
narrative_ontology:cs_authority_grounding('8fa21836-a77e-46e8-a47d-d696841c2a5b', lineage).
narrative_ontology:cs_interpretation_layer_present('8fa21836-a77e-46e8-a47d-d696841c2a5b').
narrative_ontology:cs_reading_relation('8fa21836-a77e-46e8-a47d-d696841c2a5b', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8fa21836-a77e-46e8-a47d-d696841c2a5b', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('8fa21836-a77e-46e8-a47d-d696841c2a5b', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('8fa21836-a77e-46e8-a47d-d696841c2a5b', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_axiom('8fa21836-a77e-46e8-a47d-d696841c2a5b', foundational, jewish_people_constitute_a_nation).
narrative_ontology:cs_axiom_status(jewish_people_constitute_a_nation, holdable).
narrative_ontology:cs_axiom_grounding('8fa21836-a77e-46e8-a47d-d696841c2a5b', jewish_people_constitute_a_nation, deontological).
narrative_ontology:cs_axiom('8fa21836-a77e-46e8-a47d-d696841c2a5b', foundational, universal_right_to_self_determination).
narrative_ontology:cs_axiom_status(universal_right_to_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('8fa21836-a77e-46e8-a47d-d696841c2a5b', universal_right_to_self_determination, deontological).
narrative_ontology:cs_reference_frame('8fa21836-a77e-46e8-a47d-d696841c2a5b', post_holocaust_international_order).
narrative_ontology:cs_drift_state('8fa21836-a77e-46e8-a47d-d696841c2a5b', contemporary_postcolonial_critique, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('8fa21836-a77e-46e8-a47d-d696841c2a5b', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_seeking_sovereignty).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_self_determination__liberal_nationalist_reading, palestinian_nationalists).
narrative_ontology:constraint_vindicates(jewish_self_determination__liberal_nationalist_reading, universal_self_determination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seeks recognition of Jewish people as a nation with the right to self-determination, often advocating for a sovereign state as a refuge and cultural center. Benefits from the international legitimacy this claim provides.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_seeking_sovereignty, beneficiary,
    organized, generational, constrained, global).

% Proponents of this reading who articulate and promote the claim within international forums and academic discourse. They frame Jewish self-determination within a universalist framework of national rights.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, liberal_nationalist_advocates, agenda_setter,
    organized, biographical, mobile, global).

% While this reading assumes territorial partition resolves competing claims, in practice, Palestinian nationalists perceive the assertion of Jewish self-determination as a claim that impacts their own right to self-determination and territorial integrity, requiring them to 'pay' in terms of recognition or land.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, palestinian_nationalists, payer,
    organized, generational, trapped, regional).

% Institutions (e.g., UN, ICJ) that adjudicate and interpret principles of national self-determination. They are the primary forum for the claim's recognition and its coordination with other national claims.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, international_law_bodies, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__liberal_nationalist_reading, international_law_bodies, observer).

% Jewish groups who reject the premise of Jewish nationhood or its expression through a sovereign state, often advocating for diasporic flourishing or universal human rights instead. Their perspective is often marginalized in mainstream discussions of Jewish self-determination.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, anti_zionist_jewish_groups, excluded,
    moderate, biographical, mobile, global).

% Academics and activists who frame Zionism as a settler-colonial project, fundamentally challenging the liberal nationalist premise of equal claims to self-determination in a context of dispossession. Their arguments are often excluded from the liberal nationalist framework's internal logic.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, settler_colonial_critics, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__liberal_nationalist_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_self_determination__liberal_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the recognition of Jewish national rights within a universal framework of national self-determination, aiming for a resolution (e.g., territorial partition) that respects all peoples' claims.
% TRANSFER_FUNCTION: Transfers international legitimacy and political recognition to the Jewish national claim, enabling the pursuit of sovereignty and collective rights, ideally without dispossessing other peoples.
% ABSENT_VOICES: Settler-colonial critics and anti-Zionist Jewish groups are often excluded from the framing of this claim, as their arguments fundamentally challenge its premises or proposed solutions. They would object to the framing of Jewish people as a nation or the territorial expression of self-determination.
% DISAPPEARANCE_RATIONALE: If the claim for Jewish self-determination, framed as a liberal nationalist right, vanished, the international discourse on national rights would be incomplete, and the specific conflict over self-determination in the region would lose a key interpretive framework, leading to a fundamental reorganization of political and legal arguments.
% FOUNDING_PROBLEM: The historical persecution, statelessness, and vulnerability of Jewish people, culminating in the Holocaust, necessitated a secure national home and collective political agency.
% FOUNDING_PROBLEM_CORROBORATION: International human rights declarations, historical records of antisemitism, and the ongoing need for refuge and cultural preservation for Jewish communities facing persecution or assimilation. This is corroborated by historical scholarship and international legal instruments, not solely by benefiting parties.
narrative_ontology:disappearance_verdict(jewish_self_determination__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__liberal_nationalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__liberal_nationalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The base extractiveness is low-to-moderate (0.35) because, by its own lights, this reading aims for a coordination solution (like a two-state solution) where all parties' rights are respected, thus minimizing inherent extraction. However, the practical implementation and contestation of this claim mean it's not zero. Suppression is low (0.2) as the claim itself is an assertion of rights, not an enforcement mechanism, though its political realization may involve coercive state power. Theater ratio is low (0.1) as it's a genuine political and philosophical claim. Resistance is moderate (0.5) due to significant counter-claims and critiques.
 *
 * PERSPECTIVAL GAP:
 *   The liberal nationalist reading aims for a win-win coordination, but from the perspective of Palestinian nationalists and settler-colonial critics, the claim itself, especially when tied to territorial outcomes, can be deeply extractive and suppressive. The engine's computation of per-seat classification will highlight this divergence, showing a 'rope' for beneficiaries and a 'tangled_rope' or 'snare' for those who bear the costs of its implementation.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish diaspora seeking sovereignty is the primary beneficiary, gaining legitimacy and a framework for collective rights. Liberal nationalist advocates are agenda-setters, shaping the discourse. Palestinian nationalists are positioned as payers, as the claim, in its practical application, often involves negotiations over land and recognition that they must concede. International law bodies act as both agenda-setters and observers, mediating the claim within global frameworks. Anti-Zionist Jewish groups and settler-colonial critics are excluded, as their fundamental disagreements fall outside the liberal nationalist framing.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_feasibility_ambiguity,
    'Is a territorial partition that genuinely resolves competing national claims and respects all parties'' rights practically feasible and mutually acceptable?',
    'Empirical observation of successful, mutually recognized, and stable territorial partitions in similar contexts, or the successful implementation of such a partition in this specific case.',
    'If feasible, the ''rope'' classification holds, as the claim leads to genuine coordination. If infeasible, the claim''s implementation becomes inherently extractive, pushing it towards a ''tangled_rope'' or ''snare'' for those whose claims are not accommodated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_feasibility_ambiguity, empirical, 'The practical viability of the liberal nationalist solution.').

omega_variable(
    universal_application_ambiguity,
    'Is the principle of national self-determination applied universally and consistently to all peoples, or is its application selective and contingent on geopolitical power dynamics?',
    'Comparative analysis of international legal and political practice regarding self-determination claims across diverse contexts, assessing for consistent application regardless of power asymmetries.',
    'If applied universally, the liberal nationalist framing gains strength. If selectively applied, the claim''s legitimacy is undermined, and its implementation may be perceived as a ''snare'' for those excluded from the principle''s benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_application_ambiguity, conceptual, 'The consistency of the universal self-determination principle.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''jewish_self_determination'' kernel. What specific structural elements would change if a sibling reading (e.g., ''settler_colonial_reading'') were adopted?',
    'Analysis of the core premises and implications of each reading, identifying points of logical contradiction or fundamental divergence in beneficiary/victim structures.',
    'Adopting the ''settler_colonial_reading'' would fundamentally shift the constraint''s classification from a ''rope'' (coordination) to a ''snare'' (extraction), with ''palestinian_nationalists'' becoming clear victims and ''jewish_diaspora_seeking_sovereignty'' becoming beneficiaries of an extractive structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Documenting the structural delta between kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__liberal_nationalist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1948, 0.25).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1967, 0.3).
narrative_ontology:measurement(jewi_be_t1987, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1987, 0.32).
narrative_ontology:measurement(jewi_be_t2000, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 2000, 0.34).
narrative_ontology:measurement(jewi_be_t2010, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 2010, 0.35).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1948, 0.15).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1967, 0.17).
narrative_ontology:measurement(jewi_su_t1987, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1987, 0.18).
narrative_ontology:measurement(jewi_su_t2000, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 2000, 0.19).
narrative_ontology:measurement(jewi_su_t2010, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 2010, 0.2).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__liberal_nationalist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, israeli_palestinian_conflict_resolution).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, universal_human_rights_framework).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the 'jewish_self_determination' kernel, each representing a distinct structural claim. This 'liberal_nationalist_reading' focuses on universal principles of national self-determination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
