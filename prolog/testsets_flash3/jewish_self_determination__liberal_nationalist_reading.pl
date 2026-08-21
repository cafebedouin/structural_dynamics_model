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
 *   This constraint represents the liberal-nationalist reading of Jewish
 *   self-determination, asserting that Jewish people constitute a nation with
 *   an equal claim to self-determination as other peoples. This reading
 *   emphasizes universal principles of national rights and typically
 *   advocates for a two-state solution based on territorial partition and
 *   mutual recognition. It frames the establishment of a Jewish state as a
 *   response to historical persecution and a fulfillment of national
 *   aspirations, aiming to resolve a coordination problem among competing
 *   national claims. The constraint is classified as a Rope due to its core
 *   function of coordinating national claims, though its implementation has
 *   led to significant extraction from other groups.
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
narrative_ontology:cs_story_uid(jewish_self_determination__liberal_nationalist_reading, 'b20414c4-cdd5-4cbd-b892-5849dbf26796').
narrative_ontology:cs_kernel_codification('b20414c4-cdd5-4cbd-b892-5849dbf26796', formalized).
narrative_ontology:cs_authority_grounding('b20414c4-cdd5-4cbd-b892-5849dbf26796', lineage).
narrative_ontology:cs_interpretation_layer_present('b20414c4-cdd5-4cbd-b892-5849dbf26796').
narrative_ontology:cs_reading_relation('b20414c4-cdd5-4cbd-b892-5849dbf26796', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('b20414c4-cdd5-4cbd-b892-5849dbf26796', jewish_self_determination__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('b20414c4-cdd5-4cbd-b892-5849dbf26796', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('b20414c4-cdd5-4cbd-b892-5849dbf26796', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_axiom('b20414c4-cdd5-4cbd-b892-5849dbf26796', foundational, universal_national_self_determination).
narrative_ontology:cs_axiom_status(universal_national_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('b20414c4-cdd5-4cbd-b892-5849dbf26796', universal_national_self_determination, deontological).
narrative_ontology:cs_axiom('b20414c4-cdd5-4cbd-b892-5849dbf26796', foundational, territorial_partition_as_equitable_solution).
narrative_ontology:cs_axiom_status(territorial_partition_as_equitable_solution, holdable).
narrative_ontology:cs_axiom_grounding('b20414c4-cdd5-4cbd-b892-5849dbf26796', territorial_partition_as_equitable_solution, instrumental).
narrative_ontology:cs_reference_frame('b20414c4-cdd5-4cbd-b892-5849dbf26796', post_wwii_liberal_international_order).
narrative_ontology:cs_drift_state('b20414c4-cdd5-4cbd-b892-5849dbf26796', contemporary_postcolonial_critique, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b20414c4-cdd5-4cbd-b892-5849dbf26796', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_seeking_sovereignty).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_self_determination__liberal_nationalist_reading, palestinian_people).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seeks a secure homeland and self-determination, viewing it as a universal right for all nations. Benefits from the recognition of Jewish nationhood and the establishment of a sovereign state, providing refuge and cultural continuity. Exit options are constrained by historical persecution and the perceived need for a safe haven.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_seeking_sovereignty, beneficiary,
    organized, generational, constrained, global).

% Bears the costs of territorial partition and displacement, experiencing loss of land, sovereignty, and self-determination. Their situation is defined by the competing claims to the same territory, with limited exit options due to identity-lock to their ancestral land.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, palestinian_people, payer,
    powerless, generational, trapped, regional).

% Advocates for universal principles of national self-determination and seeks to mediate conflicts through territorial partition and mutual recognition. Benefits from the perceived resolution of national conflicts through established international norms. Their role is to legitimize and enforce a two-state solution.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, liberal_international_community, agenda_setter,
    institutional, generational, analytical, global).

% Would object to a purely secular, liberal-nationalist framing, asserting a divine right to the land that supersedes political agreements. Their claims are rooted in religious texts and traditions, making compromise on territorial division difficult.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, religious_zionists, excluded,
    organized, civilizational, identity_locked, local).

% Would object to any form of Jewish territorial nationalism, arguing that Jewish safety and flourishing are best achieved through integration into diverse societies and universal human rights, not through a nation-state. They see nationalism as inherently problematic.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, anti_zionist_diasporists, excluded,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the competing claims of Jewish people and other national groups (specifically Palestinians) to self-determination and territory, aiming for a just and equitable partition that allows both to flourish.
% TRANSFER_FUNCTION: Transfers recognition of national rights and territorial sovereignty to Jewish people, with the implicit transfer of land and resources from existing inhabitants (Palestinians) in exchange for their own recognized self-determination.
% ABSENT_VOICES: Religious Zionists and anti-Zionist diasporists are often marginalized in the liberal-nationalist discourse, as their claims (divine right, universal human rights over nationalism) challenge the foundational premises of territorial partition and secular national self-determination. They would argue for alternative frameworks for Jewish collective life and land claims.
% DISAPPEARANCE_RATIONALE: If the liberal-nationalist framework for Jewish self-determination vanished, the international legal and political scaffolding for a two-state solution would collapse. This would lead to intensified conflict over land and rights, as the primary conceptual tool for resolving competing claims would be gone, forcing a re-evaluation of sovereignty and belonging.
% FOUNDING_PROBLEM: The historical persecution and statelessness of Jewish people, culminating in the Holocaust, created an urgent need for a secure homeland and the recognition of their right to self-determination as a nation.
% FOUNDING_PROBLEM_CORROBORATION: The liberal international community, human rights organizations, and many Jewish communities globally corroborate the historical problem of statelessness and the ongoing need for security and self-determination. While the specific solution (territorial partition) is contested, the underlying problem of Jewish collective vulnerability is widely acknowledged outside of direct beneficiaries.
narrative_ontology:disappearance_verdict(jewish_self_determination__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__liberal_nationalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__liberal_nationalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.35) is moderate, reflecting the costs imposed on Palestinians through territorial partition, but not pure extraction, as the framework aims for mutual recognition. Suppression (0.45) is also moderate, as the liberal-nationalist framework actively suppresses alternative claims (e.g., religious or diasporist) that challenge the nation-state paradigm or the legitimacy of partition. Theater ratio (0.1) is low, as the coordination function is genuine, though its implementation is often criticized for failing to achieve its stated goals of equitable resolution. The temporal measurements reflect fluctuations in extractiveness and suppression based on periods of conflict and peace processes, with a general trend of increasing extractiveness as the conflict persists without a resolution.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Jewish people seeking sovereignty, this is a legitimate and necessary act of national self-determination. From the perspective of Palestinians, it is a source of dispossession and ongoing conflict. The liberal international community attempts to bridge this gap by framing it as a coordination problem requiring a two-state solution, but the inherent asymmetry of power and historical context means the costs are not evenly distributed. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish diaspora seeking sovereignty are the primary beneficiaries, gaining recognition and a homeland. Palestinian people are the primary payers, bearing the costs of displacement and loss of self-determination. The liberal international community acts as an agenda-setter, promoting and legitimizing this framework. Religious Zionists and anti-Zionist diasporists are excluded, as their foundational claims fall outside the liberal-nationalist paradigm.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to secure Jewish self-determination and resolve competing national claims) is still live, but its effectiveness in achieving a truly equitable coordination is contested. The persistence of the conflict and the ongoing extraction from Palestinians suggest that while the coordination problem is real, the current liberal-nationalist framing may be insufficient or flawed in its implementation, leading to a 'tangled' outcome rather than a pure 'rope' from all seats. The classification as Rope reflects the *claimed* function, while the metrics capture the *actual* operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''liberal_nationalist_reading'' of Jewish self-determination, or does it conflate with other readings?',
    'Detailed textual analysis of policy documents, UN resolutions, and academic discourse to isolate specific arguments and their underlying premises. Comparison with the core tenets of other readings (e.g., indigenous return, settler colonial, religious covenant, diasporist).',
    'If conflated, the extractiveness and suppression metrics may be inaccurate, reflecting elements of other readings. A clearer identification would refine the classification and highlight specific points of contestation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ensuring the constraint accurately represents the specified reading of the kernel.').

omega_variable(
    partition_feasibility_and_equity,
    'Is a truly equitable territorial partition, as envisioned by liberal nationalism, empirically feasible and capable of resolving the core conflict without ongoing extraction?',
    'Empirical study of historical and contemporary partition outcomes, analysis of demographic and geographic realities, and assessment of political will for mutual recognition and equitable resource sharing.',
    'If not feasible or inherently inequitable, the ''Rope'' classification would be challenged, potentially reclassifying towards ''Tangled Rope'' or ''Snare'' due to persistent, unavoidable extraction. If feasible, it would strengthen the ''Rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_feasibility_and_equity, empirical, 'Assessing the practical viability and fairness of the liberal-nationalist solution.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative claims structural (e.g., international legal frameworks, diplomatic pressure) or internalized (e.g., self-censorship, ideological conformity within liberal institutions)?',
    'Analysis of policy enforcement mechanisms versus discourse analysis of academic and political narratives. If suppression persists even when structural barriers are lowered, it suggests internalized mechanisms.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as dissenting voices carry the suppression with them. This would amplify the perceived extractiveness of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative national narratives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__liberal_nationalist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1948, 0.05).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1967, 0.08).
narrative_ontology:measurement(jewi_tr_t1993, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1993, 0.07).
narrative_ontology:measurement(jewi_tr_t2000, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(jewi_tr_t2014, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 2014, 0.12).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1948, 0.2).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1967, 0.3).
narrative_ontology:measurement(jewi_be_t1993, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1993, 0.25).
narrative_ontology:measurement(jewi_be_t2000, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(jewi_be_t2014, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 2014, 0.4).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1948, 0.3).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1967, 0.4).
narrative_ontology:measurement(jewi_su_t1993, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1993, 0.35).
narrative_ontology:measurement(jewi_su_t2000, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(jewi_su_t2014, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 2014, 0.5).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__liberal_nationalist_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jewish_self_determination' kernel. Other readings (indigenous_return_reading, settler_colonial_reading, religious_covenant_reading, diasporist_reading) represent distinct constraints with different structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
