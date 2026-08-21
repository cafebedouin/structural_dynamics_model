% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__settler_colonial_reading, []).

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
 *   constraint_id: jewish_self_determination__settler_colonial_reading
 *   human_readable: Zionism as Settler-Colonial Project
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'settler_colonial_reading' of the
 *   'jewish_self_determination' kernel. It frames Zionism as a European
 *   settler-colonial project that systematically dispossessed indigenous
 *   Palestinians through violence and legal exclusion. The constraint's
 *   operation is characterized by high extraction and suppression, with the
 *   Israeli state and European Jewish settlers as beneficiaries, and
 *   Palestinian Arabs as victims. The claimed type is 'snare', reflecting a
 *   structure designed for extraction and elimination of an indigenous
 *   population.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, 0.9).
domain_priors:suppression_score(jewish_self_determination__settler_colonial_reading, 0.95).
domain_priors:theater_ratio(jewish_self_determination__settler_colonial_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_self_determination__settler_colonial_reading, "Zionism as Settler-Colonial Project").
narrative_ontology:topic_domain(jewish_self_determination__settler_colonial_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__settler_colonial_reading, 'e7f0d530-e1a2-4595-ad99-ff7f6cfce1a8').
narrative_ontology:cs_kernel_codification('e7f0d530-e1a2-4595-ad99-ff7f6cfce1a8', formalized).
narrative_ontology:cs_authority_grounding('e7f0d530-e1a2-4595-ad99-ff7f6cfce1a8', extraction).
narrative_ontology:cs_interpretation_layer_present('e7f0d530-e1a2-4595-ad99-ff7f6cfce1a8').
narrative_ontology:cs_reading_relation('e7f0d530-e1a2-4595-ad99-ff7f6cfce1a8', jewish_self_determination__liberal_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('e7f0d530-e1a2-4595-ad99-ff7f6cfce1a8', jewish_self_determination__indigenous_return_reading, forecloses).
narrative_ontology:cs_reading_relation('e7f0d530-e1a2-4595-ad99-ff7f6cfce1a8', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('e7f0d530-e1a2-4595-ad99-ff7f6cfce1a8', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_axiom('e7f0d530-e1a2-4595-ad99-ff7f6cfce1a8', foundational, zionism_as_european_colonial_project).
narrative_ontology:cs_axiom_status(zionism_as_european_colonial_project, holdable).
narrative_ontology:cs_axiom_grounding('e7f0d530-e1a2-4595-ad99-ff7f6cfce1a8', zionism_as_european_colonial_project, empirically_contingent).
narrative_ontology:cs_axiom('e7f0d530-e1a2-4595-ad99-ff7f6cfce1a8', foundational, palestinian_dispossession_is_structural).
narrative_ontology:cs_axiom_status(palestinian_dispossession_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('e7f0d530-e1a2-4595-ad99-ff7f6cfce1a8', palestinian_dispossession_is_structural, empirically_contingent).
narrative_ontology:cs_reference_frame('e7f0d530-e1a2-4595-ad99-ff7f6cfce1a8', european_colonial_expansion_framework).
narrative_ontology:cs_drift_state('e7f0d530-e1a2-4595-ad99-ff7f6cfce1a8', contemporary_postcolonial_critique, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e7f0d530-e1a2-4595-ad99-ff7f6cfce1a8', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__settler_colonial_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, israeli_state).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, european_jewish_settlers).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_arabs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary institutional actor that establishes, maintains, and expands the settler-colonial project through legislation, military force, and resource allocation. It benefits directly from land acquisition, resource control, and the demographic engineering of a Jewish majority.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, israeli_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Individuals and communities who benefit from the dispossession of Palestinians, gaining access to land, housing, and resources often at subsidized rates or through state-backed initiatives. Their presence and expansion are central to the settler-colonial dynamic.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, european_jewish_settlers, beneficiary,
    powerful, biographical, constrained, regional).

% The indigenous population that has been systematically dispossessed of land, property, and self-determination through violence, legal exclusion, and ongoing occupation. They bear the direct costs of the settler-colonial project, including displacement, loss of livelihood, and denial of rights.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_arabs, payer,
    powerless, generational, trapped, regional).

% Monitor and document human rights abuses, violations of international law, and the impact of the settler-colonial project on Palestinians. They advocate for accountability and changes in policy but lack direct enforcement power.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, international_human_rights_organizations, observer,
    organized, biographical, analytical, global).

% Analyze and theorize the historical and ongoing dynamics of Zionism through a settler-colonial lens, contributing to the intellectual framework that defines this reading. They influence discourse but have no direct power over the constraint's operation.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, postcolonial_scholars, observer,
    analytical, generational, analytical, global).

% Jewish individuals and groups in the diaspora who critically oppose Zionism as a settler-colonial project, often facing exclusion or marginalization from mainstream Jewish institutions and discourse. They advocate for Palestinian rights and a different vision of Jewish identity.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, diaspora_jewish_critics, excluded,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__settler_colonial_reading, israeli_state).
narrative_ontology:fixing_cost_class(jewish_self_determination__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the establishment and maintenance of a Jewish majority state in historic Palestine by facilitating Jewish immigration, land acquisition, and the creation of exclusive institutions and legal frameworks.
% TRANSFER_FUNCTION: Transfers land, resources, political power, and demographic control from indigenous Palestinian Arabs to European Jewish settlers and the Israeli state, through systematic displacement and legal exclusion.
% ABSENT_VOICES: The voices of dispossessed Palestinians, particularly refugees and those living under occupation, are systematically marginalized or silenced within the dominant narratives that justify the settler-colonial project. Anti-Zionist Jewish voices are also often excluded from mainstream discourse.
% DISAPPEARANCE_RATIONALE: If the settler-colonial framework and its enforcement mechanisms vanished, the entire political, legal, and demographic structure of Israel/Palestine would collapse. Palestinians would assert their right of return and self-determination, leading to a fundamental reorganization of land ownership, citizenship, and governance.
% FOUNDING_PROBLEM: The founding problem, from the perspective of Zionist proponents, was the historical persecution of Jewish people in Europe (antisemitism) and the desire for Jewish self-determination and refuge in their ancestral homeland.
% FOUNDING_PROBLEM_CORROBORATION: Palestinian historians, postcolonial scholars, and critical international legal analyses corroborate that the founding problem, while real for Jewish people, was instrumentalized to justify a colonial project, and its 'solution' created a new problem of indigenous dispossession. This is supported by historical records of land expropriation and displacement.
narrative_ontology:disappearance_verdict(jewish_self_determination__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__settler_colonial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jewish_self_determination__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__settler_colonial_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.9) due to the ongoing seizure of Palestinian land, resources, and the denial of their right to return, which constitutes a continuous transfer of wealth and sovereignty. Suppression is extremely high (0.95) because the project relies on systematic violence, military occupation, legal discrimination (e.g., Law of Return asymmetry), and the active suppression of Palestinian resistance and political agency. Theater ratio is low (0.1) as the extractive and suppressive functions are overt and directly enforced, with justifications (security, historical right) serving as cover rather than primary function. Accessibility collapse is high (0.9) for Palestinians, as alternatives to dispossession or resistance are systematically removed. Resistance is high (0.8) reflecting the continuous struggle against the project.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Israeli state and many Jewish settlers, the project is one of national liberation and self-defense, justifying its actions as necessary for survival. From the perspective of Palestinian Arabs and this reading, the same actions constitute ongoing colonization and ethnic cleansing. The engine's computation of per-seat classification will highlight this divergence, with beneficiaries experiencing it as a 'rope' or 'scaffold' (coordination/support) and victims experiencing it as a 'snare' (pure extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli state and European Jewish settlers are the primary beneficiaries, gaining land, security, and political power (low directionality). Palestinian Arabs are the clear targets, bearing the costs of displacement, occupation, and legal exclusion (high directionality). International human rights organizations and postcolonial scholars act as analytical observers, while diaspora Jewish critics are excluded from mainstream Zionist discourse but align with the critique.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigeneity_claim_ambiguity,
    'To what extent does the claim of Jewish indigeneity to the land function as a genuine historical connection versus a rhetorical tool to legitimize settler-colonial practices?',
    'Comparative historical and archaeological analysis of continuous presence and land stewardship, alongside critical examination of the political functions of indigeneity claims in colonial contexts.',
    'If primarily rhetorical, it strengthens the settler-colonial framing and the classification as a snare; if a genuine, unbroken connection with land stewardship, it complicates the ''settler'' aspect, though not necessarily the ''colonial'' outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigeneity_claim_ambiguity, conceptual, 'Ambiguity regarding the function of Jewish indigeneity claims.').

omega_variable(
    historical_trauma_justification,
    'How does the historical trauma of antisemitism and the Holocaust influence the perception and justification of the settler-colonial project, and does it structurally alter its extractive nature?',
    'Sociological and psychological studies on collective trauma and its impact on national identity and policy, alongside ethical analysis of whether historical victimhood justifies contemporary dispossession.',
    'If historical trauma is seen as a primary driver that mitigates intent, it might shift the perception of extractiveness from pure malice to a desperate, albeit harmful, act of self-preservation, potentially influencing the ''theater_ratio'' or ''suppression'' interpretation. However, it does not change the structural fact of dispossession.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_trauma_justification, conceptual, 'Role of historical trauma in justifying the project.').

omega_variable(
    alternative_historical_paths,
    'Could Zionism have developed as a non-colonial project, or was its settler-colonial character inherent from its inception given the geopolitical context and demographic goals?',
    'Counterfactual historical analysis exploring alternative Zionist movements (e.g., cultural Zionism, binationalism) and their suppression or failure, examining the structural conditions that favored the settler-colonial path.',
    'If alternative non-colonial paths were genuinely viable and suppressed, it highlights the contingent nature of the current snare. If the settler-colonial path was structurally inevitable, it reinforces the inherent extractiveness of the project.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_historical_paths, empirical, 'Inherent vs. contingent settler-colonial character of Zionism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__settler_colonial_reading, 1948, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__settler_colonial_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__settler_colonial_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement(jewi_tr_t1987, jewish_self_determination__settler_colonial_reading, theater_ratio, 1987, 0.1).
narrative_ontology:measurement(jewi_tr_t2000, jewish_self_determination__settler_colonial_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(jewi_tr_t2010, jewish_self_determination__settler_colonial_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(jewi_tr_t2023, jewish_self_determination__settler_colonial_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1948, 0.75).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1967, 0.82).
narrative_ontology:measurement(jewi_be_t1987, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1987, 0.85).
narrative_ontology:measurement(jewi_be_t2000, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2000, 0.88).
narrative_ontology:measurement(jewi_be_t2010, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2010, 0.89).
narrative_ontology:measurement(jewi_be_t2023, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2023, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1948, 0.8).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1967, 0.88).
narrative_ontology:measurement(jewi_su_t1987, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1987, 0.9).
narrative_ontology:measurement(jewi_su_t2000, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2000, 0.92).
narrative_ontology:measurement(jewi_su_t2010, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2010, 0.94).
narrative_ontology:measurement(jewi_su_t2023, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2023, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__settler_colonial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, law_of_return_asymmetry).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, israeli_occupation_of_palestinian_territories).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, gaza_blockade).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jewish_self_determination' kernel, focusing on its settler-colonial aspects. Other readings (liberal nationalist, indigenous return, religious covenant, diasporist) offer alternative interpretations of the same underlying historical and political phenomena, leading to different structural classifications and ethical implications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
