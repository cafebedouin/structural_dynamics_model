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
 *   human_readable: Zionism as a Settler-Colonial Project
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint models Zionism as a European settler-colonial project,
 *   focusing on the systematic dispossession of indigenous Palestinians
 *   through violence and legal exclusion. It is one reading of the 'Jewish
 *   self-determination' kernel. From this perspective, the constraint's
 *   structure is designed for extraction and the elimination of an indigenous
 *   population, making it a snare. The beneficiaries are European Jewish
 *   settlers and the Israeli state, while Palestinian Arabs are the victims,
 *   experiencing displacement, occupation, and legal discrimination.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, 0.92).
domain_priors:suppression_score(jewish_self_determination__settler_colonial_reading, 0.88).
domain_priors:theater_ratio(jewish_self_determination__settler_colonial_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_self_determination__settler_colonial_reading, "Zionism as a Settler-Colonial Project").
narrative_ontology:topic_domain(jewish_self_determination__settler_colonial_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__settler_colonial_reading, 'd2ee0f0b-ab3d-42c7-bc37-2e875172dff9').
narrative_ontology:cs_kernel_codification('d2ee0f0b-ab3d-42c7-bc37-2e875172dff9', formalized).
narrative_ontology:cs_authority_grounding('d2ee0f0b-ab3d-42c7-bc37-2e875172dff9', extraction).
narrative_ontology:cs_interpretation_layer_present('d2ee0f0b-ab3d-42c7-bc37-2e875172dff9').
narrative_ontology:cs_reading_relation('d2ee0f0b-ab3d-42c7-bc37-2e875172dff9', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2ee0f0b-ab3d-42c7-bc37-2e875172dff9', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2ee0f0b-ab3d-42c7-bc37-2e875172dff9', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2ee0f0b-ab3d-42c7-bc37-2e875172dff9', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_axiom('d2ee0f0b-ab3d-42c7-bc37-2e875172dff9', foundational, zionism_as_european_colonialism).
narrative_ontology:cs_axiom_status(zionism_as_european_colonialism, holdable).
narrative_ontology:cs_axiom_grounding('d2ee0f0b-ab3d-42c7-bc37-2e875172dff9', zionism_as_european_colonialism, conventional).
narrative_ontology:cs_axiom('d2ee0f0b-ab3d-42c7-bc37-2e875172dff9', foundational, palestinian_dispossession_as_systematic).
narrative_ontology:cs_axiom_status(palestinian_dispossession_as_systematic, holdable).
narrative_ontology:cs_axiom_grounding('d2ee0f0b-ab3d-42c7-bc37-2e875172dff9', palestinian_dispossession_as_systematic, empirically_contingent).
narrative_ontology:cs_reference_frame('d2ee0f0b-ab3d-42c7-bc37-2e875172dff9', colonial_settler_state_formation).
narrative_ontology:cs_drift_state('d2ee0f0b-ab3d-42c7-bc37-2e875172dff9', contemporary_postcolonial_discourse, gap(stable, minor, false)).
narrative_ontology:cs_created_at('d2ee0f0b-ab3d-42c7-bc37-2e875172dff9', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__settler_colonial_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, european_jewish_settlers).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, israeli_state).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_arabs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from land acquisition, resource control, and legal protections that prioritize their presence and expansion. Their identity and material well-being are deeply intertwined with the settler-colonial project.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, european_jewish_settlers, beneficiary,
    institutional, generational, arbitrage, regional).

% The primary institutional actor that designs, implements, and enforces policies of dispossession, settlement expansion, and legal discrimination. Its legitimacy and existence are tied to the ongoing project.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, israeli_state, agenda_setter,
    institutional, generational, constrained, national).

% The indigenous population subjected to systematic displacement, land confiscation, resource deprivation, and legal exclusion. They bear the full cost of the settler-colonial project, with limited options for resistance or exit.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_arabs, payer,
    powerless, generational, trapped, local).

% Document and condemn human rights abuses, land confiscation, and discriminatory laws. They provide critical analysis but lack direct enforcement power over the Israeli state.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, international_human_rights_organizations, observer,
    organized, biographical, analytical, global).

% Analyze Zionism through the lens of settler-colonial theory, identifying patterns of dispossession, racialization, and state-building that mirror other colonial contexts. Their work informs critical discourse but does not directly alter policy.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, postcolonial_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the acquisition and control of land and resources for the benefit of European Jewish settlers, establishing a new society on existing indigenous territory.
% TRANSFER_FUNCTION: Transfers land, water, and other resources from indigenous Palestinian Arabs to European Jewish settlers and the Israeli state, along with political power and legal rights.
% ABSENT_VOICES: The voices of dispossessed Palestinians, particularly those in diaspora or under occupation, are systematically marginalized in dominant international discourse. Their narratives of dispossession and resistance are often suppressed or reframed.
% DISAPPEARANCE_RATIONALE: If the settler-colonial framework vanished, the entire legal, political, and demographic structure of the region would be fundamentally altered. Land ownership, citizenship rights, and resource allocation would have to be renegotiated, leading to a complete reorganization of society.
% FOUNDING_PROBLEM: The problem of establishing a Jewish national home in Palestine, perceived as a solution to European antisemitism and the lack of Jewish sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: The Israeli state and its supporters continue to assert the necessity of maintaining a Jewish majority and control over the land for security and national self-determination. Palestinian and international human rights organizations corroborate the ongoing nature of the dispossession, but frame it as a problem of colonial expansion, not national liberation.
narrative_ontology:disappearance_verdict(jewish_self_determination__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__settler_colonial_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_self_determination__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__settler_colonial_reading, 0.92, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.92) is high due to ongoing land confiscation, settlement expansion, and differential legal status that systematically disadvantages Palestinians. Suppression (0.88) is also very high, reflecting the active military occupation, legal frameworks (like the Law of Return asymmetry), and physical barriers that prevent Palestinian return and self-determination. The theater ratio (0.15) is low, as the project's primary function is direct control and expansion, with minimal performative cover. Resistance is high (0.85) due to continuous Palestinian struggle against the occupation and dispossession.
 *
 * PERSPECTIVAL GAP:
 *   The settler-colonial reading fundamentally diverges from other readings of Jewish self-determination. While other readings might emphasize national liberation or religious fulfillment, this reading highlights the inherent extractive and suppressive nature of the project from the perspective of the indigenous population. The engine's classification will reflect this high extraction and suppression, contrasting with claims of self-determination or return.
 *
 * DIRECTIONALITY LOGIC:
 *   European Jewish settlers and the Israeli state are clear beneficiaries, with their power and exit options reflecting their structural advantage. Palestinian Arabs are the primary targets, trapped by the constraint's mechanisms of control and dispossession. International human rights organizations and postcolonial scholars act as observers, documenting and analyzing the constraint's operation without direct participation in its benefits or costs.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_causality_of_dispossession,
    'To what extent was the dispossession of Palestinians a direct, intentional outcome of Zionist ideology and planning, versus an unintended consequence of conflict and demographic shifts?',
    'Declassified archival documents, historical scholarship on Zionist planning, and oral histories from Palestinian refugees.',
    'If intentional, it strengthens the snare classification and the claim of systematic settler-colonialism. If largely unintended, it might shift the classification towards a tangled rope, acknowledging a coordination problem with severe, but not fully premeditated, extractive outcomes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_causality_of_dispossession, empirical, 'Ambiguity regarding the intentionality of Palestinian dispossession.').

omega_variable(
    indigeneity_of_jewish_people,
    'Is the Jewish claim to indigeneity in the land of Israel/Palestine compatible with the settler-colonial framework, or does it fundamentally alter the classification?',
    'Conceptual analysis of indigeneity in postcolonial theory, historical and archaeological evidence of continuous Jewish presence, and comparative studies of indigenous rights movements.',
    'If Jewish indigeneity is recognized as a primary claim, it challenges the ''European settler'' aspect of the reading, potentially shifting the constraint''s origin story and the nature of its beneficiaries. However, it does not negate the dispossession of Palestinians, but rather reframes the conflict as a clash of indigenous claims, potentially leading to a more complex tangled_rope or even a contested mountain classification from a different reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigeneity_of_jewish_people, conceptual, 'The conceptual tension between Jewish indigeneity and settler-colonial analysis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__settler_colonial_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__settler_colonial_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__settler_colonial_reading, theater_ratio, 1967, 0.18).
narrative_ontology:measurement(jewi_tr_t1987, jewish_self_determination__settler_colonial_reading, theater_ratio, 1987, 0.16).
narrative_ontology:measurement(jewi_tr_t2000, jewish_self_determination__settler_colonial_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(jewi_tr_t2014, jewish_self_determination__settler_colonial_reading, theater_ratio, 2014, 0.15).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__settler_colonial_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1948, 0.85).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1967, 0.88).
narrative_ontology:measurement(jewi_be_t1987, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1987, 0.9).
narrative_ontology:measurement(jewi_be_t2000, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2000, 0.91).
narrative_ontology:measurement(jewi_be_t2014, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2014, 0.92).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1948, 0.8).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1967, 0.85).
narrative_ontology:measurement(jewi_su_t1987, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1987, 0.87).
narrative_ontology:measurement(jewi_su_t2000, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2000, 0.88).
narrative_ontology:measurement(jewi_su_t2014, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2014, 0.88).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__settler_colonial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, israeli_citizenship_law).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, law_of_return_asymmetry).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, settlement_expansion_policy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
