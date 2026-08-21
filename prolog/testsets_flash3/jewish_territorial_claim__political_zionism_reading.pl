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
 *   human_readable: Political Zionist Claim for Jewish Territorial Statehood
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   This constraint represents the 'political Zionism' reading of the Jewish
 *   territorial claim, which prioritizes the establishment of a sovereign
 *   Jewish state with a demographic majority in Palestine as the solution to
 *   the 'Jewish Question' and antisemitism. This reading views the indigenous
 *   Arab population as an obstacle to this goal, making population transfer a
 *   necessary consideration. The constraint is classified as a Snare due to
 *   its high extractiveness and suppression of the indigenous population,
 *   whose land and self-determination are directly targeted for the benefit
 *   of the Jewish state-building project. This is one reading of the
 *   'jewish_territorial_claim' kernel.
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
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__political_zionism_reading, snare).
narrative_ontology:human_readable(jewish_territorial_claim__political_zionism_reading, "Political Zionist Claim for Jewish Territorial Statehood").
narrative_ontology:topic_domain(jewish_territorial_claim__political_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__political_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__political_zionism_reading, '2a56dc2a-5f80-4ede-b585-aa27447e45af').
narrative_ontology:cs_kernel_codification('2a56dc2a-5f80-4ede-b585-aa27447e45af', formalized).
narrative_ontology:cs_authority_grounding('2a56dc2a-5f80-4ede-b585-aa27447e45af', lineage).
narrative_ontology:cs_interpretation_layer_present('2a56dc2a-5f80-4ede-b585-aa27447e45af').
narrative_ontology:cs_reading_relation('2a56dc2a-5f80-4ede-b585-aa27447e45af', jewish_territorial_claim__cultural_zionism_reading, influences).
narrative_ontology:cs_reading_relation('2a56dc2a-5f80-4ede-b585-aa27447e45af', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('2a56dc2a-5f80-4ede-b585-aa27447e45af', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('2a56dc2a-5f80-4ede-b585-aa27447e45af', foundational, jewish_sovereignty_is_existential_necessity).
narrative_ontology:cs_axiom_status(jewish_sovereignty_is_existential_necessity, holdable).
narrative_ontology:cs_axiom_grounding('2a56dc2a-5f80-4ede-b585-aa27447e45af', jewish_sovereignty_is_existential_necessity, deontological).
narrative_ontology:cs_axiom('2a56dc2a-5f80-4ede-b585-aa27447e45af', foundational, demographic_majority_is_prerequisite_for_statehood).
narrative_ontology:cs_axiom_status(demographic_majority_is_prerequisite_for_statehood, holdable).
narrative_ontology:cs_axiom_grounding('2a56dc2a-5f80-4ede-b585-aa27447e45af', demographic_majority_is_prerequisite_for_statehood, empirically_contingent).
narrative_ontology:cs_reference_frame('2a56dc2a-5f80-4ede-b585-aa27447e45af', basel_program_political_statehood).
narrative_ontology:cs_drift_state('2a56dc2a-5f80-4ede-b585-aa27447e45af', post_mandate_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2a56dc2a-5f80-4ede-b585-aa27447e45af', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, political_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, jewish_immigrants_to_palestine).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, indigenous_palestinian_arabs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for and actively works towards the establishment of a sovereign Jewish state in Palestine with a Jewish demographic majority, viewing it as the only solution to antisemitism and the 'Jewish Question'. It mobilizes international support and resources for settlement and state-building.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, political_zionist_movement, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the political Zionist project by gaining access to land, resources, and a national identity within the developing state structure. They are often fleeing persecution and see the project as their only secure refuge.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, jewish_immigrants_to_palestine, beneficiary,
    moderate, biographical, mobile, regional).

% Bear the primary costs of this constraint through displacement, loss of land and property, and the imposition of a new political order that denies their national self-determination. Their demographic majority is seen as an obstacle, leading to policies of exclusion and potential transfer.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, indigenous_palestinian_arabs, payer,
    powerless, generational, trapped, local).

% Administer Palestine under a League of Nations mandate, balancing competing claims. While not fully aligned with political Zionism, their policies (e.g., facilitating Jewish immigration) often enable the constraint's progression, even as they attempt to manage Arab resistance.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, british_mandate_authorities, agenda_setter,
    institutional, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__political_zionism_reading, british_mandate_authorities, observer).

% Observes and, at times, intervenes in the conflict. Its actions (e.g., UN partition plan) can legitimize or challenge the political Zionist claim, but it often struggles to enforce resolutions against entrenched interests.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the efforts of Jewish diaspora communities and international political actors towards the establishment of a sovereign Jewish state, providing a unified political goal and framework for action.
% TRANSFER_FUNCTION: Transfers territorial control, demographic majority, and political sovereignty from indigenous Palestinian Arabs to the Jewish people, specifically to the nascent Jewish state.
% ABSENT_VOICES: Palestinian political leadership and civil society, whose claims to self-determination and territorial integrity are directly contradicted by the constraint. Their voices are suppressed through military, legal, and demographic means.
% DISAPPEARANCE_RATIONALE: If the political Zionist claim for a Jewish majority state vanished, the entire political and demographic structure of the region would be fundamentally altered. The justification for many existing laws, institutions, and settlement patterns would dissolve, leading to a radical re-negotiation of power and land claims.
% FOUNDING_PROBLEM: The 'Jewish Question' and persistent antisemitism across Europe, which rendered Jewish communities vulnerable and stateless, necessitating a territorial solution for national self-determination and security.
% FOUNDING_PROBLEM_CORROBORATION: The political Zionist movement and many Jewish communities globally attest that antisemitism remains a live threat, validating the need for a sovereign Jewish state. Critics, including some anti-Zionist Jews and Palestinian advocates, argue that while antisemitism is real, the specific territorial solution has created new problems and injustices, making the 'solution' itself contested.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__political_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__political_zionism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__political_zionism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because the core mechanism involves dispossessing an indigenous population of land and sovereignty to establish a new national entity. Suppression is also very high (0.90) as the project requires active, often violent, enforcement to overcome indigenous resistance and maintain a demographic majority. Theater ratio is low (0.20) because the state-building project is highly functional and goal-oriented, with little performative maintenance; the 'security' justifications are often genuine from the perspective of the beneficiaries, even if they mask extraction from the victims. The metrics reflect the period leading up to the establishment of the State of Israel, where the political Zionist agenda was actively implemented.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the political Zionist movement, this is a necessary and just act of national liberation and self-determination, a 'Rope' or even 'Mountain' of historical necessity. From the perspective of indigenous Palestinians, it is a clear 'Snare' of dispossession and colonial imposition. The engine's classification from the authored metrics reflects the latter, while the 'claimed_type' of 'snare' aligns with the analytical observer's view of the structural reality.
 *
 * DIRECTIONALITY LOGIC:
 *   The political Zionist movement and Jewish immigrants are clear beneficiaries (d near 0.0) as they gain a state, land, and security. Indigenous Palestinian Arabs are the primary victims (d near 1.0), experiencing displacement, loss of sovereignty, and suppression of their national aspirations. British Mandate authorities occupy a complex position, often enabling the constraint's progression while attempting to manage the conflict, making their directionality more moderate but still leaning towards enabling the extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_right_vs_colonial_imposition,
    'Is the claim to Jewish territorial sovereignty a natural right of self-determination for a persecuted people, or a form of settler-colonial imposition on an indigenous population?',
    'Historical and legal analysis of international law regarding indigenous rights and national self-determination, combined with a critical examination of the historical context of Zionist settlement.',
    'If primarily a natural right, the extractiveness from Palestinians might be reframed as an unavoidable consequence of competing nationalisms. If primarily settler-colonial, the extractiveness is a core feature of an unjust system, reinforcing the ''Snare'' classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_right_vs_colonial_imposition, conceptual, 'Framing of the core claim as either inherent right or colonial act.').

omega_variable(
    necessity_of_demographic_majority,
    'Was a Jewish demographic majority and the associated policies (e.g., population transfer) truly necessary for the security and viability of a Jewish state, or was it a maximalist political goal?',
    'Counterfactual historical analysis exploring alternative models of statehood or binationalism proposed at the time, and their potential for long-term viability and security.',
    'If truly necessary, the high suppression and extractiveness might be seen as tragic but unavoidable. If a maximalist goal, it highlights the discretionary nature of the extraction and suppression, reinforcing the ''Snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_of_demographic_majority, empirical, 'Whether demographic majority was a necessity or a political choice.').

omega_variable(
    antisemitism_as_justification,
    'To what extent did the genuine threat of antisemitism justify the specific means and consequences of political Zionism, particularly the displacement of indigenous Palestinians?',
    'Ethical and historical analysis weighing the moral imperative of protecting a persecuted group against the rights of an indigenous population, considering the availability of alternative solutions.',
    'If the justification is seen as absolute, it might mitigate the perceived moral weight of the extraction. If the justification is conditional or limited, it underscores the ethical costs of the constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(antisemitism_as_justification, preference, 'Ethical justification of means by the threat of antisemitism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__political_zionism_reading, 1897, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1897, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1897, 0.1).
narrative_ontology:measurement(jewi_tr_t1917, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1917, 0.15).
narrative_ontology:measurement(jewi_tr_t1929, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1929, 0.18).
narrative_ontology:measurement(jewi_tr_t1936, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1936, 0.2).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1948, 0.2).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1897, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1897, 0.6).
narrative_ontology:measurement(jewi_be_t1917, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1917, 0.7).
narrative_ontology:measurement(jewi_be_t1929, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1929, 0.78).
narrative_ontology:measurement(jewi_be_t1936, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1936, 0.82).
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


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
