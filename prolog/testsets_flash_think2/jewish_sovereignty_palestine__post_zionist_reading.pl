% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__post_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__post_zionist_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__post_zionist_reading
 *   human_readable: Jewish Sovereignty in Palestine: Post-Zionist Reading
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint represents the 'post-Zionist' reading of Jewish
 *   sovereignty in Palestine, focusing on how the state's founding narrative
 *   and ethnic-national framework, while achieving statehood, now actively
 *   obstruct civic equality for all citizens and hinder regional integration.
 *   This reading identifies Israeli Palestinians and occupied populations as
 *   victims of an ongoing ethnic privilege structure, with Jewish citizens as
 *   beneficiaries via laws like the Law of Return and land access
 *   asymmetries. The metrics reflect a substantially extractive and
 *   suppressive constraint, actively enforced by state institutions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, 0.75).
domain_priors:suppression_score(jewish_sovereignty_palestine__post_zionist_reading, 0.8).
domain_priors:theater_ratio(jewish_sovereignty_palestine__post_zionist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__post_zionist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__post_zionist_reading, "Jewish Sovereignty in Palestine: Post-Zionist Reading").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__post_zionist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__post_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__post_zionist_reading, '00e6672c-a463-4f9e-9d79-096b55be38a8').
narrative_ontology:cs_kernel_codification('00e6672c-a463-4f9e-9d79-096b55be38a8', formalized).
narrative_ontology:cs_authority_grounding('00e6672c-a463-4f9e-9d79-096b55be38a8', lineage).
narrative_ontology:cs_interpretation_layer_present('00e6672c-a463-4f9e-9d79-096b55be38a8').
narrative_ontology:cs_reading_relation('00e6672c-a463-4f9e-9d79-096b55be38a8', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('00e6672c-a463-4f9e-9d79-096b55be38a8', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('00e6672c-a463-4f9e-9d79-096b55be38a8', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('00e6672c-a463-4f9e-9d79-096b55be38a8', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('00e6672c-a463-4f9e-9d79-096b55be38a8', foundational, ethnic_privilege_obstructs_equality).
narrative_ontology:cs_axiom_status(ethnic_privilege_obstructs_equality, holdable).
narrative_ontology:cs_axiom_grounding('00e6672c-a463-4f9e-9d79-096b55be38a8', ethnic_privilege_obstructs_equality, empirically_contingent).
narrative_ontology:cs_axiom('00e6672c-a463-4f9e-9d79-096b55be38a8', foundational, statehood_does_not_justify_occupation).
narrative_ontology:cs_axiom_status(statehood_does_not_justify_occupation, holdable).
narrative_ontology:cs_axiom_grounding('00e6672c-a463-4f9e-9d79-096b55be38a8', statehood_does_not_justify_occupation, deontological).
narrative_ontology:cs_reference_frame('00e6672c-a463-4f9e-9d79-096b55be38a8', civic_equality_and_regional_integration).
narrative_ontology:cs_drift_state('00e6672c-a463-4f9e-9d79-096b55be38a8', contemporary_state_practice, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('00e6672c-a463-4f9e-9d79-096b55be38a8', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, israeli_palestinians).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, occupied_palestinians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The institutional apparatus that defines and enforces the ethnic-national framework, including laws like the Nation-State Law and the Law of Return, and policies regarding land, citizenship, and security. It benefits from the stability and continuity of this framework.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, state_of_israel, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the ethnic-national framework through preferential immigration rights (Law of Return), land allocation policies, and the state's self-definition as a Jewish state, which grants them a privileged status relative to non-Jewish citizens.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens, beneficiary,
    powerful, generational, mobile, national).

% Citizens of Israel who face systemic discrimination, civic inequality, and restrictions on land and resources due to the state's ethnic-national character. Their identity is locked into a state that defines itself against their ethnic identity.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, israeli_palestinians, payer,
    organized, generational, identity_locked, national).

% Populations in the West Bank, Gaza, and East Jerusalem living under military occupation or blockade, denied self-determination and subject to a separate legal and administrative system that prioritizes the security and expansion of Jewish settlements.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, occupied_palestinians, payer,
    powerless, generational, trapped, regional).

% Individuals and groups who advocate for a non-ethnic, civic state or for broader regional integration and cooperation, but whose vision is obstructed by the existing ethnic-national framework and its associated conflicts. They are largely excluded from mainstream political discourse.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, regional_integration_advocates, excluded,
    moderate, generational, constrained, regional).

% Monitor and document human rights violations and discriminatory practices stemming from the ethnic-national framework, providing critical analysis and advocating for international legal intervention or policy changes.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, international_human_rights_organizations, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides state governance, security, and a framework for national identity and self-determination for Jewish citizens, consolidating a Jewish majority and cultural continuity in the region.
% TRANSFER_FUNCTION: Transfers civic and land rights, political power, and national belonging from non-Jewish populations (particularly Palestinians) to Jewish citizens, maintaining an ethnic-national hierarchy.
% ABSENT_VOICES: Advocates for a secular, democratic state for all its citizens, or for a binational state, are largely absent from the dominant political discourse, as are those who prioritize regional peace and integration over ethnic exclusivity. Their perspectives are actively marginalized by the state's self-definition.
% DISAPPEARANCE_RATIONALE: If the ethnic-national framework vanished overnight, the state's foundational laws, citizenship criteria, land policies, and its relationship with its non-Jewish citizens and neighboring populations would fundamentally change. This would lead to a complete reordering of political, social, and economic life in the region, likely with significant shifts in power and resource allocation.
% FOUNDING_PROBLEM: To establish a secure homeland and self-determination for the Jewish people in their ancestral land, following centuries of persecution, antisemitism, and statelessness.
% FOUNDING_PROBLEM_CORROBORATION: While the historical context of Jewish persecution and the desire for self-determination is widely acknowledged, the claim that the current ethnic-national framework is *still* necessary for Jewish security and self-determination is contested. Post-Zionist scholars, Palestinian civil society, and international human rights organizations argue that the framework now primarily serves to maintain an ethnic privilege structure, rather than solely addressing existential threats. Legislative hearings and independent academic analyses from outside the benefiting parties support this shifted-function reading.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__post_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__post_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__post_zionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jewish_sovereignty_palestine__post_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__post_zionist_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the ethnic-national framework systematically disadvantages non-Jewish populations in favor of Jewish citizens, leading to significant transfers of rights, resources, and opportunities. Suppression is also high (0.80) as the state actively enforces laws and policies that maintain this hierarchy and prevent the emergence of alternative, more inclusive political structures. The theater ratio is low (0.20) because the mechanisms of obstruction and privilege are functional and effective, not merely performative. Resistance is high (0.70) due to ongoing Palestinian activism and international scrutiny. Accessibility collapse is moderate-high (0.65) as structural barriers make alternatives to the current framework difficult to achieve.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Jewish citizens and the State of Israel (as framed by dominant narratives), the constraint is a necessary mechanism for national self-determination and security. From the perspective of Israeli Palestinians and occupied populations, it is a structure of ongoing oppression and extraction. The engine's classification will highlight this divergence, showing a 'tangled_rope' for the system as a whole, but with vastly different effective extraction for beneficiaries and targets.
 *
 * DIRECTIONALITY LOGIC:
 *   The State of Israel, as the agenda-setter, benefits from the continuity of the framework. Jewish citizens are direct beneficiaries of the privileges it confers. Israeli Palestinians and occupied Palestinians are clear targets, bearing the costs of civic inequality and lack of self-determination. Regional integration advocates are excluded, as their vision directly challenges the constraint's core tenets.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ethnic_framework_necessity,
    'Is the ethnic-national framework of the State of Israel still a necessary condition for the security and self-determination of the Jewish people, or does it primarily function as an obstructive privilege structure?',
    'Empirical analysis of security outcomes under alternative, more inclusive governance models (e.g., binational or civic-national proposals), or a shift in regional geopolitical dynamics that reduces perceived existential threats.',
    'If deemed primarily an obstructive privilege structure, the constraint''s extractiveness and suppression would be unequivocally classified as illegitimate. If deemed necessary for security, a portion of the extraction might be re-interpreted as a coordination cost for collective survival.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ethnic_framework_necessity, conceptual, 'Ambiguity between security imperative and privilege structure.').

omega_variable(
    founding_problem_status_ambiguity,
    'Is the founding problem of Jewish statelessness and insecurity still ''live'' in a way that justifies the current ethnic-national framework, or has it been ''dead'' for decades, with the framework persisting for other reasons?',
    'Consensus among independent historical and political analyses, and a shift in the narratives accepted by a broad range of stakeholders, including those outside the benefiting parties.',
    'If the founding problem is widely acknowledged as ''dead'', the constraint would be reclassified closer to a ''piton'' or ''snare'', indicating its persistence is due to inertia or pure extraction rather than a live coordination need. If ''live'', its ''tangled_rope'' classification would be more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_status_ambiguity, empirical, 'Contestability of the founding problem''s ongoing relevance.').

omega_variable(
    reading_identity_and_scope,
    'To what extent does this ''post-Zionist'' reading overlap with or diverge from the ''settler-colonial'' reading, particularly regarding the historical origins and ongoing nature of the conflict?',
    'Detailed comparative analysis of the core tenets, historical interpretations, and proposed solutions of both readings, identifying points of convergence and irreconcilable difference.',
    'If significant overlap is found, it might suggest a deeper structural connection or even a partial subsumption, potentially leading to a re-evaluation of the network relationships or the need for a more granular decomposition of the kernel. If divergence is strong, it reinforces the distinct analytical utility of each reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_and_scope, conceptual, 'Relationship between post-Zionist and settler-colonial readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__post_zionist_reading, 1993, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1993, 0.25).
narrative_ontology:measurement(jewi_tr_t1998, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1998, 0.23).
narrative_ontology:measurement(jewi_tr_t2003, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2003, 0.22).
narrative_ontology:measurement(jewi_tr_t2008, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2008, 0.21).
narrative_ontology:measurement(jewi_tr_t2013, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2013, 0.2).
narrative_ontology:measurement(jewi_tr_t2018, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2018, 0.2).
narrative_ontology:measurement(jewi_tr_t2023, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2023, 0.2).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1993, 0.6).
narrative_ontology:measurement(jewi_be_t1998, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1998, 0.65).
narrative_ontology:measurement(jewi_be_t2003, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2003, 0.68).
narrative_ontology:measurement(jewi_be_t2008, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2008, 0.7).
narrative_ontology:measurement(jewi_be_t2013, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2013, 0.72).
narrative_ontology:measurement(jewi_be_t2018, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2018, 0.74).
narrative_ontology:measurement(jewi_be_t2023, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2023, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1993, 0.65).
narrative_ontology:measurement(jewi_su_t1998, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1998, 0.7).
narrative_ontology:measurement(jewi_su_t2003, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2003, 0.74).
narrative_ontology:measurement(jewi_su_t2008, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2008, 0.77).
narrative_ontology:measurement(jewi_su_t2013, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2013, 0.78).
narrative_ontology:measurement(jewi_su_t2018, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2018, 0.79).
narrative_ontology:measurement(jewi_su_t2023, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2023, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__post_zionist_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'jewish_sovereignty_palestine' kernel, each representing a distinct structural interpretation of the same underlying commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
