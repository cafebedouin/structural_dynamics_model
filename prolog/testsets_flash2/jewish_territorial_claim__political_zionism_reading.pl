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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Political Zionist Claim to Jewish Territorial Sovereignty
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   This constraint story analyzes the 'political Zionism' reading of the
 *   Jewish territorial claim, which posits that Jewish statehood in
 *   Palestine, requiring territorial sovereignty and a Jewish demographic
 *   majority, is the necessary solution to antisemitism and the 'Jewish
 *   Question'. This reading prioritizes state-building over cultural content
 *   and views the existing Arab population as an obstacle to be managed or
 *   transferred. The period covered is from the First Zionist Congress (1897)
 *   to the establishment of Israel (1948). The constraint is classified as a
 *   Snare due to its high extractiveness and suppression of the indigenous
 *   Palestinian population.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, 0.85).
domain_priors:suppression_score(jewish_territorial_claim__political_zionism_reading, 0.9).
domain_priors:theater_ratio(jewish_territorial_claim__political_zionism_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__political_zionism_reading, snare).
narrative_ontology:human_readable(jewish_territorial_claim__political_zionism_reading, "Political Zionist Claim to Jewish Territorial Sovereignty").
narrative_ontology:topic_domain(jewish_territorial_claim__political_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__political_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__political_zionism_reading, '9616e530-6595-468f-9a08-706d327c38e5').
narrative_ontology:cs_kernel_codification('9616e530-6595-468f-9a08-706d327c38e5', formalized).
narrative_ontology:cs_authority_grounding('9616e530-6595-468f-9a08-706d327c38e5', lineage).
narrative_ontology:cs_interpretation_layer_present('9616e530-6595-468f-9a08-706d327c38e5').
narrative_ontology:cs_reading_relation('9616e530-6595-468f-9a08-706d327c38e5', jewish_territorial_claim__cultural_zionism_reading, influences).
narrative_ontology:cs_reading_relation('9616e530-6595-468f-9a08-706d327c38e5', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('9616e530-6595-468f-9a08-706d327c38e5', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('9616e530-6595-468f-9a08-706d327c38e5', foundational, jewish_statehood_is_sole_solution_to_antisemitism).
narrative_ontology:cs_axiom_status(jewish_statehood_is_sole_solution_to_antisemitism, holdable).
narrative_ontology:cs_axiom_grounding('9616e530-6595-468f-9a08-706d327c38e5', jewish_statehood_is_sole_solution_to_antisemitism, deontological).
narrative_ontology:cs_axiom('9616e530-6595-468f-9a08-706d327c38e5', foundational, jewish_majority_is_prerequisite_for_sovereignty).
narrative_ontology:cs_axiom_status(jewish_majority_is_prerequisite_for_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('9616e530-6595-468f-9a08-706d327c38e5', jewish_majority_is_prerequisite_for_sovereignty, empirically_contingent).
narrative_ontology:cs_reference_frame('9616e530-6595-468f-9a08-706d327c38e5', basel_program_1897).
narrative_ontology:cs_drift_state('9616e530-6595-468f-9a08-706d327c38e5', post_1948_establishment, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9616e530-6595-468f-9a08-706d327c38e5', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, political_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, jewish_immigrants_to_palestine).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_arab_population).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, anti_zionist_jews).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for and actively works to establish a sovereign Jewish state with a Jewish majority in Palestine, viewing it as the only permanent solution to antisemitism and the Jewish Question. Prioritizes state-building and demographic control.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, political_zionist_movement, agenda_setter,
    institutional, generational, constrained, global).

% Are offered a national home and refuge from persecution, with the promise of self-determination and security within a Jewish-majority state. They benefit from the political and material support of the Zionist movement.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, jewish_immigrants_to_palestine, beneficiary,
    moderate, biographical, mobile, regional).

% Are dispossessed of land, displaced from homes, and subjected to policies aimed at maintaining a Jewish demographic majority. Their national aspirations are denied, and their presence is viewed as an obstacle to the political Zionist project.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, palestinian_arab_population, payer,
    powerless, generational, trapped, local).

% Are marginalized and often condemned by the political Zionist movement for rejecting the necessity or legitimacy of a Jewish state in Palestine. Their alternative visions for Jewish continuity or coexistence are suppressed within the dominant discourse.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, anti_zionist_jews, excluded,
    moderate, biographical, identity_locked, global).

% Administered Palestine and, at various times, facilitated or constrained Zionist settlement and political aims, often balancing competing Arab and Jewish demands. Their policies directly impacted the feasibility of a Jewish majority and statehood.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, british_mandate_authorities, agenda_setter,
    institutional, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__political_zionism_reading, british_mandate_authorities, observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate Jewish national aspirations and provide a secure refuge from antisemitism by establishing a sovereign state with a Jewish majority, thereby solving the 'Jewish Question' through self-determination.
% TRANSFER_FUNCTION: Transfers land, political control, and demographic majority from the indigenous Palestinian Arab population to Jewish immigrants and the nascent Jewish state.
% ABSENT_VOICES: The Palestinian Arab population, whose national rights and claims to self-determination are systematically excluded from the political Zionist framework, would object to the premise of a Jewish state requiring their displacement or subjugation. Anti-Zionist Jewish voices, advocating for alternative solutions to antisemitism or for binational coexistence, are also excluded.
% DISAPPEARANCE_RATIONALE: If the political Zionist claim to a Jewish state with a Jewish majority vanished overnight, the entire political and demographic structure of Israel/Palestine would be fundamentally challenged. The legal and institutional framework prioritizing Jewish national rights would collapse, leading to a radical reordering of land ownership, citizenship, and political power, likely resulting in a binational or secular democratic state.
% FOUNDING_PROBLEM: The historical problem of antisemitism and the 'Jewish Question' – the precarious status of Jews as a minority in diaspora, facing persecution and lacking self-determination.
% FOUNDING_PROBLEM_CORROBORATION: The political Zionist movement and its supporters continue to assert that antisemitism remains a live threat, justifying the ongoing need for a sovereign Jewish state. Critics, including Palestinian and some Jewish voices, acknowledge historical antisemitism but argue that the 'solution' has created new problems of dispossession and conflict, making the original problem's 'solution' contested in its current form.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__political_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__political_zionism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__political_zionism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness (0.85) is high because the core mechanism involves the dispossession and displacement of the indigenous Palestinian Arab population to achieve a Jewish majority. Suppression (0.90) is also very high, as the project required active enforcement, often with British Mandate support, to overcome Palestinian resistance and suppress alternative political formations. The theater ratio (0.10) is low, indicating that the movement was highly functional in its state-building goals, with little performative activity masking a lack of genuine function. Accessibility collapse (0.75) reflects the systematic closure of alternatives for Palestinians, while resistance (0.80) highlights the active and sustained opposition from the Palestinian Arab population.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the political Zionist movement, this was a necessary and just act of national liberation and self-determination. From the perspective of the Palestinian Arab population, it was a process of colonization and dispossession. The engine's classification as a Snare reflects the structural asymmetry of extraction and suppression, independent of the self-justifying narratives.
 *
 * DIRECTIONALITY LOGIC:
 *   The political Zionist movement and Jewish immigrants are beneficiaries, gaining a national home and political power. The Palestinian Arab population are clear victims, bearing the costs of displacement and loss of sovereignty. Anti-Zionist Jews are excluded, as their alternative visions are suppressed. British Mandate authorities acted as both agenda-setters and observers, facilitating the process while also attempting to manage its consequences.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (solving antisemitism via statehood) remains 'live' for its beneficiaries, but its methods (requiring a Jewish majority via displacement) are highly contested. The Snare classification prevents mislabeling this as a coordination problem, highlighting the coercive and extractive nature of achieving the demographic and territorial goals at the expense of the indigenous population. The high and increasing extractiveness and suppression over time indicate an enforcement ratchet, where the costs imposed on the victims grew as the state-building project advanced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_transfer,
    'Is the ''transfer'' (expulsion) of the Arab population a necessary and legitimate mechanism for achieving a Jewish majority and statehood, or an act of ethnic cleansing?',
    'Historical and legal analysis of international law regarding population transfer and self-determination, combined with ethical evaluation of the means-ends justification.',
    'If deemed necessary and legitimate, the extractiveness might be reframed as a tragic but unavoidable cost of national self-determination. If deemed ethnic cleansing, it reinforces the Snare classification and highlights severe violations of human rights.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_transfer, conceptual, 'Evaluates the moral and legal status of population transfer as a means to achieve demographic goals.').

omega_variable(
    alternative_solutions_to_jewish_question,
    'Were there viable alternative solutions to antisemitism and the ''Jewish Question'' that did not require territorial sovereignty and a Jewish majority in Palestine?',
    'Historical counterfactual analysis and examination of alternative Jewish political and cultural movements (e.g., autonomism, diaspora nationalism, binationalism).',
    'If viable alternatives existed, it would weaken the ''necessity'' claim of political Zionism, further exposing the extractive nature of its chosen path. If no viable alternatives are found, it might lend some support to the ''founding problem'' rationale, though not necessarily its methods.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_solutions_to_jewish_question, empirical, 'Assesses the historical availability and feasibility of non-territorial solutions to the Jewish Question.').

omega_variable(
    palestinian_national_identity_status,
    'To what extent was a distinct Palestinian national identity formed and asserted prior to and during the early Zionist project, challenging the ''land without a people'' narrative?',
    'Historical research into Palestinian social, cultural, and political organization in the late Ottoman and Mandate periods, including primary sources from Palestinian voices.',
    'Strong evidence of a pre-existing and robust Palestinian national identity would directly contradict a core premise of political Zionism, reinforcing the ''victim'' status and the high suppression metric. Weak evidence might lend more credence to the ''empty land'' narrative, though this is highly contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(palestinian_national_identity_status, empirical, 'Examines the historical reality of Palestinian national identity formation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__political_zionism_reading, 1897, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1897, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1897, 0.05).
narrative_ontology:measurement(jewi_tr_t1917, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1917, 0.08).
narrative_ontology:measurement(jewi_tr_t1929, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1929, 0.1).
narrative_ontology:measurement(jewi_tr_t1936, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1936, 0.1).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1948, 0.1).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1897, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1897, 0.6).
narrative_ontology:measurement(jewi_be_t1917, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1917, 0.7).
narrative_ontology:measurement(jewi_be_t1929, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1929, 0.75).
narrative_ontology:measurement(jewi_be_t1936, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1936, 0.8).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1948, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1897, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1897, 0.5).
narrative_ontology:measurement(jewi_su_t1917, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1917, 0.65).
narrative_ontology:measurement(jewi_su_t1929, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1929, 0.75).
narrative_ontology:measurement(jewi_su_t1936, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1936, 0.85).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1948, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__political_zionism_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
