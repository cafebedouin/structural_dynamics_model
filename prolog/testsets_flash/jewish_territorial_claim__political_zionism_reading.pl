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
 *   This constraint represents the 'political_zionism_reading' of the broader
 *   'jewish_territorial_claim' kernel. It posits that Jewish statehood,
 *   requiring territorial sovereignty and a Jewish demographic majority in
 *   Palestine, is the necessary solution to antisemitism and the 'Jewish
 *   Question'. This reading prioritizes state-building and security over
 *   cultural or socialist content, viewing the existing Arab population as an
 *   obstacle to the desired demographic balance, often leading to policies of
 *   land acquisition and population transfer. The constraint's persistence
 *   relies heavily on active enforcement and suppression of resistance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, 0.85).
domain_priors:suppression_score(jewish_territorial_claim__political_zionism_reading, 0.92).
domain_priors:theater_ratio(jewish_territorial_claim__political_zionism_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__political_zionism_reading, snare).
narrative_ontology:human_readable(jewish_territorial_claim__political_zionism_reading, "Political Zionist Reading of Jewish Territorial Claim").
narrative_ontology:topic_domain(jewish_territorial_claim__political_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__political_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__political_zionism_reading, '7f70abc0-e08c-41d9-86b9-07a198edce58').
narrative_ontology:cs_kernel_codification('7f70abc0-e08c-41d9-86b9-07a198edce58', formalized).
narrative_ontology:cs_authority_grounding('7f70abc0-e08c-41d9-86b9-07a198edce58', lineage).
narrative_ontology:cs_interpretation_layer_present('7f70abc0-e08c-41d9-86b9-07a198edce58').
narrative_ontology:cs_reading_relation('7f70abc0-e08c-41d9-86b9-07a198edce58', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('7f70abc0-e08c-41d9-86b9-07a198edce58', jewish_territorial_claim__cultural_zionism_reading, influences).
narrative_ontology:cs_reading_relation('7f70abc0-e08c-41d9-86b9-07a198edce58', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('7f70abc0-e08c-41d9-86b9-07a198edce58', foundational, jewish_sovereignty_is_existential).
narrative_ontology:cs_axiom_status(jewish_sovereignty_is_existential, holdable).
narrative_ontology:cs_axiom_grounding('7f70abc0-e08c-41d9-86b9-07a198edce58', jewish_sovereignty_is_existential, deontological).
narrative_ontology:cs_axiom('7f70abc0-e08c-41d9-86b9-07a198edce58', foundational, demographic_majority_is_security).
narrative_ontology:cs_axiom_status(demographic_majority_is_security, holdable).
narrative_ontology:cs_axiom_grounding('7f70abc0-e08c-41d9-86b9-07a198edce58', demographic_majority_is_security, empirically_contingent).
narrative_ontology:cs_reference_frame('7f70abc0-e08c-41d9-86b9-07a198edce58', basel_program_sovereignty).
narrative_ontology:cs_drift_state('7f70abc0-e08c-41d9-86b9-07a198edce58', contemporary_demographic_challenges, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7f70abc0-e08c-41d9-86b9-07a198edce58', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, jewish_settlers).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, zionist_political_leadership).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_arabs).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, non_jewish_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Articulates the necessity of a Jewish state with a Jewish majority, formulates policies for land acquisition, settlement, and security, and directs the state's enforcement apparatus. Their legitimacy and power are tied to the successful implementation of this vision.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, zionist_political_leadership, agenda_setter,
    institutional, generational, constrained, national).

% Directly benefit from land allocation, state protection, and the establishment of Jewish-majority communities. They are active participants in the territorial expansion and demographic engineering, often acting as a vanguard for state policy.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, jewish_settlers, beneficiary,
    organized, biographical, mobile, local).

% Bear the primary costs of the constraint through land expropriation, displacement, loss of property, and denial of self-determination. Their political and economic agency is systematically suppressed to maintain the Jewish majority. Exit options are limited to internal displacement, emigration, or resistance.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, palestinian_arabs, payer,
    powerless, generational, trapped, local).

% Experience legal and social discrimination, limiting their access to resources and political participation, even if not directly displaced. Their status as a minority is structurally enforced to preserve the Jewish character of the state.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, non_jewish_minorities, payer,
    moderate, biographical, constrained, national).

% Monitor and report on human rights violations, advocate for Palestinian rights, and challenge the legality of settlements and discriminatory policies under international law. They exert moral and political pressure but lack direct enforcement power.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, international_human_rights_organizations, observer,
    organized, generational, analytical, global).

% Engages in negotiations, mediation, and diplomatic pressure to resolve the conflict. Their actions are often constrained by geopolitical interests and the power dynamics of the region, leading to limited effectiveness in altering the core constraint.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, international_diplomatic_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__political_zionism_reading, zionist_political_leadership).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__political_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the establishment and maintenance of a sovereign state for the Jewish people, providing a collective security framework and a national home in response to historical antisemitism.
% TRANSFER_FUNCTION: Transfers land, resources, and political control from the indigenous Palestinian Arab population to the Jewish state and its citizens, in exchange for the promise of Jewish national self-determination and security.
% ABSENT_VOICES: Palestinian refugees and their descendants, displaced from their homes, are structurally excluded from the political process that determines their fate. Their voices would demand the right of return and full self-determination, directly challenging the demographic and territorial foundations of the constraint.
% DISAPPEARANCE_RATIONALE: If the political Zionist reading of Jewish territorial claim vanished overnight, the state's foundational justification for its current borders and demographic policies would collapse. This would necessitate a fundamental re-evaluation of land ownership, citizenship rights, and the status of Palestinian refugees, leading to a radical rearrangement of the political and social landscape.
% FOUNDING_PROBLEM: The historical problem of antisemitism, pogroms, and the lack of a secure national home for the Jewish people, culminating in the Holocaust, which necessitated a sovereign Jewish state as a refuge and guarantor of safety.
% FOUNDING_PROBLEM_CORROBORATION: Zionist political leadership and many Jewish communities attest that the founding problem of Jewish insecurity and antisemitism remains live, justifying the state's existence and policies. Palestinian Arabs, international human rights organizations, and some Jewish anti-Zionist groups attest that while antisemitism is a real problem, the specific 'solution' of a Jewish-majority state achieved through dispossession has created new injustices and is no longer a legitimate response to the original problem; they cite ongoing conflict and human rights abuses as evidence that the founding problem's 'solution' has become a source of new problems.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__political_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__political_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__political_zionism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_territorial_claim__political_zionism_reading, 'none', 1).

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
 *   The high extractiveness (0.85) and suppression (0.92) reflect the structural costs imposed on the indigenous Palestinian Arab population through land expropriation, displacement, and denial of self-determination, all enforced to achieve and maintain a Jewish majority. The 'solution' for one group becomes a severe problem for another. Theater ratio is low (0.15) as the state-building project is highly functional in its own terms, with little performative maintenance; enforcement is direct and material. Accessibility collapse is high (0.78) for Palestinians, as alternatives to displacement or subjugation are systematically removed. Resistance is also high (0.88) due to the direct and severe impact on the victim population.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Zionist political leadership and Jewish settlers, this constraint is a necessary, even existential, act of self-determination and security (a 'Rope' or 'Scaffold' for survival). From the perspective of Palestinian Arabs, it is a pure 'Snare' of dispossession and oppression. The engine's classification will reflect the structural reality of extraction and suppression, independent of the claimed justification.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish settlers and Zionist political leadership are primary beneficiaries (d near 0.0) as they gain territorial control, security, and self-determination. Palestinian Arabs and non-Jewish minorities are primary victims (d near 1.0) as they face displacement, loss of land, and political marginalization. International bodies and human rights organizations act as observers, often challenging the legitimacy and methods of the constraint, but with limited direct power to alter its operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (solving antisemitism and the Jewish Question) is presented as perpetually live by its beneficiaries. However, the methods employed (territorial acquisition, demographic engineering) have created a new, ongoing conflict, suggesting a potential Mandatrophy where the 'solution' has become a self-perpetuating problem. The high and increasing suppression indicates that the constraint's persistence is driven by active coercion rather than a solved problem or mutual benefit. The 'contested' status of the founding problem further supports this, as the original problem's resolution is debated while the extractive mechanisms continue.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_zionism_vs_cultural_zionism,
    'Is the establishment of a sovereign Jewish state with a demographic majority the only viable solution to antisemitism and the Jewish Question, or could a cultural/spiritual center suffice?',
    'Historical counterfactual analysis of alternative Zionist movements'' outcomes; empirical study of Jewish diaspora security in non-sovereign contexts.',
    'If a cultural center were sufficient, the political Zionist reading''s high extractiveness and suppression would be reclassified as unnecessary, shifting it towards a Snare or even a Piton if the original problem is solved by other means.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_zionism_vs_cultural_zionism, conceptual, 'Distinguishes political Zionism''s state-centric approach from cultural Zionism''s non-sovereign focus.').

omega_variable(
    demographic_majority_necessity,
    'Is a Jewish demographic majority a necessary condition for Jewish statehood and security, or can a secure Jewish state exist with a significant non-Jewish minority?',
    'Comparative political science analysis of multi-ethnic states'' stability and security; legal and political reforms within the state to ensure equal rights and security for all citizens.',
    'If a demographic majority is not strictly necessary, the policies of land expropriation and population transfer (which drive extractiveness and suppression) would be reclassified as purely extractive, not coordinative, strengthening the Snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_majority_necessity, empirical, 'Examines the necessity of a Jewish demographic majority for the state''s existence.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''political_zionism_reading'' of the ''jewish_territorial_claim'' kernel. What would change if a sibling reading were adopted?',
    'Analysis of historical outcomes and policy implications if ''labor_zionism_reading'' (socialist transformation), ''cultural_zionism_reading'' (spiritual center), or ''revisionist_zionism_reading'' (maximalist territory) had been dominant.',
    'Each sibling reading would produce a structurally different constraint with distinct extractiveness, suppression, and beneficiary/victim sets. For example, ''cultural_zionism_reading'' would likely have lower extractiveness and suppression, potentially classifying as a Rope or even Mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Documents this constraint as one specific reading of the ''jewish_territorial_claim'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__political_zionism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_territorial_claim__political_zionism_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(jewi_tr_t10, jewish_territorial_claim__political_zionism_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(jewi_tr_t20, jewish_territorial_claim__political_zionism_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(jewi_tr_t30, jewish_territorial_claim__political_zionism_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(jewi_be_t10, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(jewi_be_t20, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 20, 0.82).
narrative_ontology:measurement(jewi_be_t30, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(jewi_su_t10, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(jewi_su_t20, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 20, 0.88).
narrative_ontology:measurement(jewi_su_t30, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 30, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__political_zionism_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jewish_territorial_claim' kernel, focusing on political statehood and demographic majority. Other readings (labor, cultural, revisionist Zionism) represent distinct constraints with different structural properties and impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
