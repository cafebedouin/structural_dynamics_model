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
 *   constraint_id: jewish_territorial_claim__political_zionism_reading
 *   human_readable: Political Zionist Reading of Jewish Territorial Claim
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   This constraint represents the 'political Zionism' reading of the Jewish
 *   territorial claim, which posits that Jewish statehood, requiring
 *   territorial sovereignty and a Jewish demographic majority in Palestine,
 *   is the necessary solution to antisemitism and the 'Jewish Question'. This
 *   reading prioritizes state-building and treats the existing Arab
 *   population as an obstacle to be managed or transferred. The constraint is
 *   classified as a Snare due to its high extraction from and suppression of
 *   the indigenous population, with the coordination story of solving the
 *   Jewish Question serving as a cover for these extractive dynamics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, 0.85).
domain_priors:suppression_score(jewish_territorial_claim__political_zionism_reading, 0.9).
domain_priors:theater_ratio(jewish_territorial_claim__political_zionism_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, resistance, 0.92).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__political_zionism_reading, snare).
narrative_ontology:human_readable(jewish_territorial_claim__political_zionism_reading, "Political Zionist Reading of Jewish Territorial Claim").
narrative_ontology:topic_domain(jewish_territorial_claim__political_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__political_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__political_zionism_reading, '11e62e11-51ab-45e8-97ea-86a2cee1cb11').
narrative_ontology:cs_kernel_codification('11e62e11-51ab-45e8-97ea-86a2cee1cb11', formalized).
narrative_ontology:cs_authority_grounding('11e62e11-51ab-45e8-97ea-86a2cee1cb11', extraction).
narrative_ontology:cs_interpretation_layer_present('11e62e11-51ab-45e8-97ea-86a2cee1cb11').
narrative_ontology:cs_reading_relation('11e62e11-51ab-45e8-97ea-86a2cee1cb11', jewish_territorial_claim__cultural_zionism_reading, influences).
narrative_ontology:cs_reading_relation('11e62e11-51ab-45e8-97ea-86a2cee1cb11', jewish_territorial_claim__labor_zionism_reading, influences).
narrative_ontology:cs_reading_relation('11e62e11-51ab-45e8-97ea-86a2cee1cb11', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('11e62e11-51ab-45e8-97ea-86a2cee1cb11', foundational, jewish_self_determination_requires_sovereign_state).
narrative_ontology:cs_axiom_status(jewish_self_determination_requires_sovereign_state, holdable).
narrative_ontology:cs_axiom_grounding('11e62e11-51ab-45e8-97ea-86a2cee1cb11', jewish_self_determination_requires_sovereign_state, conventional).
narrative_ontology:cs_axiom('11e62e11-51ab-45e8-97ea-86a2cee1cb11', foundational, demographic_majority_is_prerequisite_for_statehood).
narrative_ontology:cs_axiom_status(demographic_majority_is_prerequisite_for_statehood, holdable).
narrative_ontology:cs_axiom_grounding('11e62e11-51ab-45e8-97ea-86a2cee1cb11', demographic_majority_is_prerequisite_for_statehood, empirically_contingent).
narrative_ontology:cs_reference_frame('11e62e11-51ab-45e8-97ea-86a2cee1cb11', basel_program_1897).
narrative_ontology:cs_drift_state('11e62e11-51ab-45e8-97ea-86a2cee1cb11', post_1948_establishment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('11e62e11-51ab-45e8-97ea-86a2cee1cb11', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, zionist_movement_leadership).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, jewish_settlers).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_arabs).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, non_jewish_minorities_in_state).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, antisemitic_regimes_and_movements).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__political_zionism_reading, zionist_ideology).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__political_zionism_reading, national_self_determination_for_jews).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Orchestrates the political and diplomatic efforts to establish and secure a Jewish state, defining its territorial and demographic parameters. Benefits from the realization of the statehood project and the power it confers.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, zionist_movement_leadership, agenda_setter,
    institutional, generational, mobile, global).

% Directly benefits from land acquisition, state protection, and the establishment of a Jewish majority, often at the expense of existing Palestinian communities. Their presence is a key mechanism for achieving the demographic goal.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, jewish_settlers, beneficiary,
    organized, biographical, constrained, regional).

% Bear the primary costs of displacement, loss of land, political disenfranchisement, and the suppression of their national aspirations. Their existence as a majority or significant population is seen as an obstacle to the constraint's core goal.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, palestinian_arabs, payer,
    powerless, generational, trapped, local).

% Experience legal and social discrimination, and their collective rights are subordinated to the state's Jewish character, as the state prioritizes the Jewish majority and identity.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, non_jewish_minorities_in_state, payer,
    powerless, biographical, constrained, local).

% Engages in diplomatic efforts, passes resolutions, and provides aid, often navigating conflicting claims and interests regarding the establishment and actions of the Jewish state.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, international_community, observer,
    institutional, generational, analytical, global).

% Indirectly benefit by the 'solution' removing Jewish populations from their territories, reinforcing their own nationalist or racist ideologies by validating the idea that Jews do not belong in their societies.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, antisemitic_regimes_and_movements, beneficiary,
    institutional, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__political_zionism_reading, zionist_movement_leadership).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__political_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the political, diplomatic, and settlement efforts of the Zionist movement to establish a sovereign Jewish state with a Jewish demographic majority in Palestine, providing a centralized solution to the 'Jewish Question'.
% TRANSFER_FUNCTION: Transfers territorial sovereignty, political control, and demographic majority from the existing Palestinian Arab population to the Jewish people and the nascent Jewish state, along with land and resources.
% ABSENT_VOICES: Palestinian political leadership and indigenous rights advocates are structurally excluded from the decision-making process regarding the establishment of the Jewish state; they would object to the displacement and dispossession inherent in the project.
% DISAPPEARANCE_RATIONALE: If the political Zionist claim and its enforcement vanished overnight, the existing state structure would lose its foundational legitimacy, leading to a fundamental reorganization of political power, land ownership, and demographic composition in the territory, likely resulting in a binational or Palestinian-majority state.
% FOUNDING_PROBLEM: The 'Jewish Question': the persistent persecution, antisemitism, and lack of national self-determination faced by Jewish people across the diaspora, culminating in pogroms and state-sponsored discrimination.
% FOUNDING_PROBLEM_CORROBORATION: The historical reality of antisemitism and the 'Jewish Question' is widely corroborated by Jewish historical accounts and international studies. However, the claim that a territorial state with a Jewish majority is the *only* or *necessary* solution, and that the problem is 'live' in a way that justifies displacement, is contested by Palestinian historians, human rights organizations, and anti-Zionist Jewish intellectuals.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__political_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__political_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__political_zionism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jewish_territorial_claim__political_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__political_zionism_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very high (0.85) because the constraint fundamentally involves the transfer of land, political rights, and self-determination from the indigenous Palestinian population to Jewish settlers and the nascent state. Suppression is also very high (0.90) as the establishment and maintenance of a Jewish majority and territorial control against an existing population required and continues to require significant coercive force, legal disenfranchisement, and the suppression of Palestinian resistance. The theater ratio is low (0.20) because the actions taken (settlement, land acquisition, military control) are direct and functional towards the stated goal, with rhetorical justifications serving to legitimize rather than obscure a performative core.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Zionist movement leadership and Jewish settlers, this constraint is a necessary and just act of national self-determination, a 'rope' or 'scaffold' to build a secure future. From the perspective of Palestinian Arabs and non-Jewish minorities, it is a 'snare' of dispossession and oppression, actively enforced to maintain an extractive structure. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist movement leadership and Jewish settlers are clear beneficiaries, gaining land, sovereignty, and security (low directionality). Palestinian Arabs and non-Jewish minorities are the primary targets/victims, losing land, rights, and self-determination (high directionality). The international community acts as an observer, while antisemitic regimes indirectly benefit by the removal of Jewish populations from their own territories.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    solution_vs_displacement_ambiguity,
    'Is the establishment of a Jewish state primarily a solution to antisemitism and the Jewish Question, or primarily a project of settler-colonial displacement of the indigenous Palestinian population?',
    'Analysis of historical outcomes for both Jewish and Palestinian populations, and the degree to which the ''solution'' for one group created new problems for the other.',
    'If primarily displacement, the extractiveness and suppression metrics are more accurately understood as inherent to the project, rather than unfortunate side effects. If primarily a solution, the costs borne by Palestinians might be framed as tragic but unavoidable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(solution_vs_displacement_ambiguity, conceptual, 'Ambiguity regarding the primary nature of the constraint: solution or displacement.').

omega_variable(
    natural_right_vs_colonial_project,
    'Is the claim to territorial sovereignty a natural right of national self-determination for the Jewish people, or a colonial project enabled by imperial powers?',
    'Comparative historical analysis with other national liberation movements and settler-colonial enterprises, examining patterns of land acquisition, demographic engineering, and international backing.',
    'If a natural right, the constraint''s legitimacy is grounded in universal principles. If a colonial project, its persistence depends on power imbalances and external support, making its extraction more clearly illegitimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_right_vs_colonial_project, conceptual, 'Ambiguity regarding the grounding of the territorial claim: natural right or colonial project.').

omega_variable(
    transfer_mechanism_necessity,
    'Was the ''transfer'' (expulsion or forced migration) of the Arab population a necessary mechanism for achieving a Jewish majority, or an avoidable outcome of political choices?',
    'Examination of alternative proposals for a binational state or federated structures, and analysis of the historical record of decisions made during periods of conflict and demographic change.',
    'If necessary, the high suppression is an unavoidable cost of the constraint''s core goal. If avoidable, the suppression is a direct consequence of specific, contestable policy choices, amplifying its extractive nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_mechanism_necessity, empirical, 'Whether demographic transfer was a necessary or chosen mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__political_zionism_reading, 1897, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1897, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1897, 0.25).
narrative_ontology:measurement(jewi_tr_t1907, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1907, 0.23).
narrative_ontology:measurement(jewi_tr_t1917, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1917, 0.22).
narrative_ontology:measurement(jewi_tr_t1927, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1927, 0.21).
narrative_ontology:measurement(jewi_tr_t1937, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1937, 0.2).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1948, 0.2).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1897, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1897, 0.6).
narrative_ontology:measurement(jewi_be_t1907, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1907, 0.68).
narrative_ontology:measurement(jewi_be_t1917, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1917, 0.75).
narrative_ontology:measurement(jewi_be_t1927, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1927, 0.8).
narrative_ontology:measurement(jewi_be_t1937, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1937, 0.83).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1948, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1897, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1897, 0.65).
narrative_ontology:measurement(jewi_su_t1907, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1907, 0.72).
narrative_ontology:measurement(jewi_su_t1917, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1917, 0.78).
narrative_ontology:measurement(jewi_su_t1927, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1927, 0.83).
narrative_ontology:measurement(jewi_su_t1937, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1937, 0.87).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1948, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__political_zionism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'jewish_territorial_claim' kernel, each representing a distinct structural claim about Jewish statehood and its implications. This reading (political Zionism) emphasizes statehood and demographic majority, differing in its core tenets and consequences from other Zionist currents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
