% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__political_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__political_zionism_reading, []).

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
 *   constraint_id: jewish_territorial_claim__political_zionism_reading
 *   human_readable: Political Zionist Claim for Jewish Territorial Sovereignty
 *   domain: Political History/Settler Colonialism/Nationalism Studies
 *
 * SUMMARY:
 *   This constraint represents the 'political Zionism' reading of the Jewish
 *   territorial claim, which posits that Jewish statehood, requiring
 *   territorial sovereignty and a Jewish demographic majority in Palestine,
 *   is the necessary solution to antisemitism and the 'Jewish Question'. This
 *   reading prioritized state-building over cultural content and viewed the
 *   existing Arab population as an obstacle to be managed, often through
 *   population transfer. The constraint's operation involved active
 *   settlement, land acquisition, and the establishment of institutions that
 *   progressively displaced the indigenous Palestinian Arab population.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, 0.78).
domain_priors:suppression_score(jewish_territorial_claim__political_zionism_reading, 0.85).
domain_priors:theater_ratio(jewish_territorial_claim__political_zionism_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__political_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__political_zionism_reading, "Political Zionist Claim for Jewish Territorial Sovereignty").
narrative_ontology:topic_domain(jewish_territorial_claim__political_zionism_reading, "Political History/Settler Colonialism/Nationalism Studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__political_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__political_zionism_reading, '5745993e-f40f-4f9c-bcb0-17e8d8ff175e').
narrative_ontology:cs_kernel_codification('5745993e-f40f-4f9c-bcb0-17e8d8ff175e', formalized).
narrative_ontology:cs_authority_grounding('5745993e-f40f-4f9c-bcb0-17e8d8ff175e', lineage).
narrative_ontology:cs_interpretation_layer_present('5745993e-f40f-4f9c-bcb0-17e8d8ff175e').
narrative_ontology:cs_reading_relation('5745993e-f40f-4f9c-bcb0-17e8d8ff175e', jewish_territorial_claim__cultural_zionism_reading, influences).
narrative_ontology:cs_reading_relation('5745993e-f40f-4f9c-bcb0-17e8d8ff175e', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('5745993e-f40f-4f9c-bcb0-17e8d8ff175e', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('5745993e-f40f-4f9c-bcb0-17e8d8ff175e', foundational, jewish_self_determination_requires_sovereignty).
narrative_ontology:cs_axiom_status(jewish_self_determination_requires_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('5745993e-f40f-4f9c-bcb0-17e8d8ff175e', jewish_self_determination_requires_sovereignty, deontological).
narrative_ontology:cs_axiom('5745993e-f40f-4f9c-bcb0-17e8d8ff175e', foundational, demographic_majority_is_essential_for_statehood).
narrative_ontology:cs_axiom_status(demographic_majority_is_essential_for_statehood, holdable).
narrative_ontology:cs_axiom_grounding('5745993e-f40f-4f9c-bcb0-17e8d8ff175e', demographic_majority_is_essential_for_statehood, empirically_contingent).
narrative_ontology:cs_reference_frame('5745993e-f40f-4f9c-bcb0-17e8d8ff175e', basel_program_1897).
narrative_ontology:cs_drift_state('5745993e-f40f-4f9c-bcb0-17e8d8ff175e', contemporary_post_1967_occupation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5745993e-f40f-4f9c-bcb0-17e8d8ff175e', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, zionist_political_leadership).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, jewish_zionist_settlers).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_arab_population).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__political_zionism_reading, jewish_self_determination).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__political_zionism_reading, national_self_defense).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Articulated the vision for a Jewish state, mobilized resources, negotiated with international powers, and directed settlement efforts. Benefited from the establishment of state institutions and the realization of their political goals.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, zionist_political_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% Migrated to Palestine, established communities, and eventually formed the demographic and political base of the Jewish state. Benefited from land acquisition, security, and the promise of self-determination, often at the expense of the existing population.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, jewish_zionist_settlers, beneficiary,
    powerful, biographical, constrained, regional).

% The indigenous population of Palestine, whose land, resources, and political self-determination were progressively displaced or suppressed by the Zionist project. Faced dispossession, violence, and eventual statelessness.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, palestinian_arab_population, payer,
    powerless, generational, trapped, regional).

% Administered Palestine under a League of Nations mandate, tasked with facilitating the establishment of a Jewish national home while safeguarding the rights of existing non-Jewish communities. Their policies often enabled Zionist expansion while failing to protect Palestinian rights, leading to escalating conflict.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, british_mandate_authorities, agenda_setter,
    institutional, biographical, mobile, national).

% Observed, debated, and eventually sanctioned the establishment of a Jewish state, often balancing humanitarian concerns for Jewish refugees with the rights of the indigenous population. Their actions (e.g., UN Partition Plan) were crucial to the constraint's enforcement.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, international_community, observer,
    institutional, civilizational, analytical, global).

% Advocated for a Jewish spiritual and cultural center in Palestine, often without prioritizing political sovereignty or a demographic majority. Their vision was largely sidelined by the dominant political Zionist agenda, which focused on state-building and demographic control.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, cultural_zionists, excluded,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a secure territorial homeland for the Jewish people, solving the 'Jewish Question' of antisemitism and statelessness through political sovereignty and a demographic majority.
% TRANSFER_FUNCTION: Transfers land, resources, and political control from the indigenous Palestinian Arab population to Jewish settlers and the nascent Jewish state, facilitated by international recognition and active enforcement.
% ABSENT_VOICES: The indigenous Palestinian Arab population, whose claims to self-determination and land were largely excluded from the international and Zionist discourse regarding the establishment of a Jewish state. Their resistance was met with suppression rather than inclusion in the political process.
% DISAPPEARANCE_RATIONALE: If this claim and its historical enforcement vanished overnight, the entire political and demographic structure of the region, including the existence of the State of Israel, would be fundamentally altered. The mobile software economy would reorganize around open payment routing.
% FOUNDING_PROBLEM: The existential threat of antisemitism and the statelessness of the Jewish people, culminating in the Holocaust, which underscored the perceived necessity of a sovereign Jewish state.
% FOUNDING_PROBLEM_CORROBORATION: The historical experience of Jewish persecution and statelessness is widely corroborated by historical records and international recognition of the need for Jewish self-determination. However, the specific solution (territorial state with a Jewish majority requiring displacement) is contested by Palestinian narratives, some anti-Zionist Jewish groups, and international legal scholars, who argue it created new injustices.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__political_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__political_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__political_zionism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jewish_territorial_claim__political_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__political_zionism_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__political_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__political_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.78) and suppression (0.85) are high because the core mechanism of achieving a Jewish majority and sovereignty inherently involved dispossessing and suppressing the political and territorial claims of the existing Palestinian Arab population. The 'coordination' for Jewish self-determination was directly coupled with 'extraction' from another group. The theater ratio is low (0.1) because the project was highly functional and effective in achieving its stated goals of state-building and demographic change, with little performative maintenance. Resistance was consistently high from the Palestinian population, leading to escalating enforcement requirements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of political Zionists, the constraint was a necessary and just act of national liberation and self-defense, solving an existential problem. From the perspective of the Palestinian Arab population, it was a settler-colonial project leading to dispossession and ongoing conflict. The engine's classification as a Tangled Rope reflects this structural asymmetry: coordination for one group, extraction for another, through the same enforced structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist political leadership and Jewish settlers were the primary beneficiaries, gaining a homeland and political power (low d). The Palestinian Arab population was the primary target, bearing the costs of displacement and loss of sovereignty (high d). British Mandate authorities acted as initial agenda-setters, facilitating the process, while the international community observed and eventually sanctioned the outcome. Cultural Zionists, with a different vision, were largely excluded from the dominant political process.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    solution_or_new_conflict,
    'Did the establishment of a Jewish state, as envisioned by political Zionism, solve the ''Jewish Question'' of antisemitism and statelessness, or did it primarily create a new, intractable conflict with the indigenous Palestinian population?',
    'Long-term historical analysis of both Jewish security and regional stability, including the perspectives of all affected parties, over multiple generations.',
    'If primarily a new conflict, the constraint''s ''coordination function'' for Jewish people is overshadowed by its ''extraction function'' from Palestinians, potentially reclassifying it closer to a Snare. If it genuinely solved the Jewish Question without creating new injustices, its Tangled Rope classification would lean more towards a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(solution_or_new_conflict, conceptual, 'Whether the constraint''s primary outcome was problem-solving or conflict-generating.').

omega_variable(
    demographic_majority_necessity,
    'Was a Jewish demographic majority, achieved through population transfer and land acquisition, truly essential for Jewish self-determination and security, or were alternative models of shared sovereignty or cultural autonomy viable?',
    'Counterfactual historical analysis exploring alternative political arrangements and their outcomes, or comparative studies of other national liberation movements.',
    'If alternatives were viable, the high extractiveness and suppression associated with achieving a majority would be seen as unnecessary and purely extractive, strengthening the Snare-like aspects of the Tangled Rope. If essential, these costs might be viewed as unavoidable for the coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(demographic_majority_necessity, empirical, 'The necessity of demographic majority for the political Zionist project.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__political_zionism_reading, 1897, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1897, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1897, 0.05).
narrative_ontology:measurement(jewi_tr_t1907, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1907, 0.06).
narrative_ontology:measurement(jewi_tr_t1917, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1917, 0.07).
narrative_ontology:measurement(jewi_tr_t1927, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1927, 0.08).
narrative_ontology:measurement(jewi_tr_t1937, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1937, 0.09).
narrative_ontology:measurement(jewi_tr_t1947, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1947, 0.1).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1948, 0.1).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1897, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1897, 0.45).
narrative_ontology:measurement(jewi_be_t1907, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1907, 0.52).
narrative_ontology:measurement(jewi_be_t1917, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1917, 0.6).
narrative_ontology:measurement(jewi_be_t1927, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1927, 0.68).
narrative_ontology:measurement(jewi_be_t1937, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1937, 0.75).
narrative_ontology:measurement(jewi_be_t1947, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1947, 0.77).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1948, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1897, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1897, 0.4).
narrative_ontology:measurement(jewi_su_t1907, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1907, 0.5).
narrative_ontology:measurement(jewi_su_t1917, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1917, 0.65).
narrative_ontology:measurement(jewi_su_t1927, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1927, 0.75).
narrative_ontology:measurement(jewi_su_t1937, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1937, 0.82).
narrative_ontology:measurement(jewi_su_t1947, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1947, 0.84).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1948, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__political_zionism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jewish_territorial_claim' kernel. Its structural properties, metrics, and classification differ from sibling readings due to its specific emphasis on political sovereignty and demographic majority, which inherently involved displacement and active enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
