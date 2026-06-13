% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__jewish_national_home_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__jewish_national_home_primacy, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: balfour_mandate_instruments__jewish_national_home_primacy
 *   human_readable: Balfour Mandate Instruments: Jewish National Home Primacy Reading
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This constraint represents the 'Jewish National Home Primacy' reading of
 *   the Balfour Mandate instruments, which interpreted the 'national home'
 *   clause as requiring active demographic and territorial transformation to
 *   establish Jewish sovereignty in Palestine. This reading directed the
 *   British Mandatory power to facilitate Jewish immigration, land
 *   acquisition, and the development of Jewish quasi-governmental
 *   institutions, while systematically downgrading Palestinian Arab political
 *   representation and land rights. The constraint is structurally a Tangled
 *   Rope, as it purports to coordinate the establishment of a national home
 *   while simultaneously extracting land and political agency from the
 *   indigenous population through active enforcement.
 *
 * KEY AGENTS:
 *   - british_mandatory_power: Agenda setter (institutional/civilizational) — enforces the Mandate's terms, adjudicates land and immigration policies.
 *   - zionist_institutions: Beneficiary (organized/generational) — gains quasi-governmental status, facilitates immigration and land acquisition.
 *   - jewish_migrants: Beneficiary (powerless/biographical) — benefits from facilitated immigration and settlement.
 *   - palestinian_arab_landholders: Payer (powerless/biographical) — loses land through facilitated sales and expropriation.
 *   - palestinian_arab_political_leadership: Payer (moderate/biographical) — structurally excluded from effective political power, resists Mandate policies.
 *   - palestinian_arab_population: Victim (powerless/generational) — experiences demographic transformation, loss of self-determination.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, 0.85).
domain_priors:suppression_score(balfour_mandate_instruments__jewish_national_home_primacy, 0.9).
domain_priors:theater_ratio(balfour_mandate_instruments__jewish_national_home_primacy, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, extractiveness, 0.85).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__jewish_national_home_primacy, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__jewish_national_home_primacy, "Balfour Mandate Instruments: Jewish National Home Primacy Reading").
narrative_ontology:topic_domain(balfour_mandate_instruments__jewish_national_home_primacy, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__jewish_national_home_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__jewish_national_home_primacy, '3ab6fdee-3a10-4f61-a9b6-bf9415371d75').
narrative_ontology:cs_kernel_codification('3ab6fdee-3a10-4f61-a9b6-bf9415371d75', fixed_text).
narrative_ontology:cs_authority_grounding('3ab6fdee-3a10-4f61-a9b6-bf9415371d75', lineage).
narrative_ontology:cs_interpretation_layer_present('3ab6fdee-3a10-4f61-a9b6-bf9415371d75').
narrative_ontology:cs_reading_relation('3ab6fdee-3a10-4f61-a9b6-bf9415371d75', balfour_mandate_instruments__dual_obligation_indigenous_rights, forecloses).
narrative_ontology:cs_reading_relation('3ab6fdee-3a10-4f61-a9b6-bf9415371d75', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('3ab6fdee-3a10-4f61-a9b6-bf9415371d75', foundational, jewish_national_home_as_proto_state).
narrative_ontology:cs_axiom_status(jewish_national_home_as_proto_state, holdable).
narrative_ontology:cs_axiom_grounding('3ab6fdee-3a10-4f61-a9b6-bf9415371d75', jewish_national_home_as_proto_state, conventional).
narrative_ontology:cs_axiom('3ab6fdee-3a10-4f61-a9b6-bf9415371d75', foundational, demographic_transformation_as_mandate_goal).
narrative_ontology:cs_axiom_status(demographic_transformation_as_mandate_goal, holdable).
narrative_ontology:cs_axiom_grounding('3ab6fdee-3a10-4f61-a9b6-bf9415371d75', demographic_transformation_as_mandate_goal, instrumental).
narrative_ontology:cs_reference_frame('3ab6fdee-3a10-4f61-a9b6-bf9415371d75', balfour_declaration_intent).
narrative_ontology:cs_drift_state('3ab6fdee-3a10-4f61-a9b6-bf9415371d75', post_unscop_report, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3ab6fdee-3a10-4f61-a9b6-bf9415371d75', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, jewish_migrants).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a 'national home for the Jewish people' in Palestine, as articulated in the Balfour Declaration and enshrined in the Mandate, by facilitating Jewish immigration and settlement.
% TRANSFER_FUNCTION: Transfers land, political agency, and demographic control from the Palestinian Arab population to Zionist institutions and Jewish migrants, under the administrative authority of the British Mandatory power.
% ABSENT_VOICES: Palestinian Arab representatives were largely excluded from the drafting of the Mandate and consistently denied effective political participation during its implementation. Their voices, advocating for self-determination and protection of existing rights, were systematically suppressed.
% DISAPPEARANCE_RATIONALE: If this reading of the Mandate (and its enforcement) had disappeared overnight, the demographic and territorial transformation of Palestine would not have occurred in the same manner. Jewish immigration and land acquisition would have faced different legal and political constraints, and the trajectory towards a Jewish state would have been fundamentally altered, leading to a vastly different political landscape.
% FOUNDING_PROBLEM: The problem this reading was built to solve was the establishment of a 'national home for the Jewish people' in Palestine, addressing the Zionist movement's political aspirations and the historical persecution of Jews.
% FOUNDING_PROBLEM_CORROBORATION: Zionist institutions and their supporters attest that the founding problem (the need for a Jewish national home) remains live. Palestinian Arab leadership and international legal scholars, however, contest the legitimacy of the 'founding problem' as framed by this reading, arguing it was predicated on colonial assumptions and disregarded indigenous rights. Corroboration for the 'live' status comes primarily from within the benefiting parties; external corroboration is contested by those who view the 'national home' as a colonial imposition.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__jewish_national_home_primacy, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__jewish_national_home_primacy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__jewish_national_home_primacy, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(balfour_mandate_instruments__jewish_national_home_primacy, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__jewish_national_home_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__jewish_national_home_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the systematic transfer of land and political power from the Palestinian Arab population to Zionist institutions and Jewish migrants. Suppression (0.9) is high due to the active enforcement by the British Mandatory power to overcome Arab resistance to these policies, including restrictions on political organization and land sales. The theater ratio is low (0.1) because the Mandate's stated coordination function (establishing a national home) was actively pursued, but its implementation was heavily skewed towards one group, making the 'coordination' aspect a cover for asymmetric extraction. The accessibility collapse (0.75) indicates that alternatives for Palestinian Arabs (e.g., self-determination, independent land tenure) were significantly curtailed by the Mandate's legal and administrative framework. Resistance (0.8) was high, reflecting continuous Palestinian Arab opposition to the Mandate's policies.
 *
 * PERSPECTIVAL GAP:
 *   The British Mandatory power and Zionist institutions would experience this as a complex but necessary coordination effort to fulfill an international obligation. Palestinian Arab landholders and political leadership, however, would experience it as a highly extractive and suppressive imposition, designed to dispossess them and deny their political rights. The engine's per-seat classification would reflect this divergence, with beneficiaries seeing a Rope-like function and victims experiencing a Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   The British Mandatory Power, as the agenda setter, benefits from maintaining its imperial authority and fulfilling its international obligations (d near 0.2). Zionist institutions and Jewish migrants are clear beneficiaries, gaining land, political influence, and a national home (d near 0.0-0.1). Palestinian Arab landholders, political leadership, and the general population are the primary targets, bearing the costs of land loss, political marginalization, and demographic transformation (d near 0.9-1.0).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading of the Mandate is a Tangled Rope because it genuinely attempts to coordinate the establishment of a 'national home' (a complex, multi-party endeavor) but does so through asymmetric extraction and active suppression of the indigenous population. It prevents mislabeling as a pure Snare by acknowledging the stated coordination goal, but prevents mislabeling as a Rope by highlighting the coercive and extractive mechanisms used to achieve that goal for one party at the expense of another. The Mandate's mandate was to establish a national home, but this reading transformed it into an instrument of demographic and territorial transformation, making the original coordination function a cover for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine interpretation of the Mandate''s intent, or a politically motivated reading that prioritizes one group''s claims?',
    'Historical analysis of diplomatic correspondence and contemporary international legal interpretations of self-determination and colonial mandates.',
    'If a politically motivated reading, the constraint''s legitimacy is undermined, reclassifying it closer to a Snare. If a genuine interpretation, its Tangled Rope classification holds, reflecting a flawed but intended coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'This constraint is one reading (''jewish_national_home_primacy'') of the ''balfour_mandate_instruments'' kernel. Sibling readings (''dual_obligation_indigenous_rights'', ''mandatory_interpretive_discretion'') would shift the beneficiary/victim structure and the overall classification.').

omega_variable(
    land_transfer_legitimacy,
    'Were land transfers from Arab to Jewish owners genuinely voluntary and fair, or were they systematically facilitated by coercive mechanisms inherent in the Mandate''s implementation?',
    'Detailed archival research into land transaction records, pricing, and the socio-economic conditions of Arab sellers under Mandate administration.',
    'Evidence of systemic coercion would increase the ''suppression'' and ''extractiveness'' metrics, pushing the classification closer to a Snare by highlighting the non-consensual nature of the ''coordination''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_transfer_legitimacy, empirical, 'Ambiguity regarding the voluntariness of land sales under the Mandate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__jewish_national_home_primacy, 1922, 1947).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t0, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 0, 0.15).
narrative_ontology:measurement(balf_tr_t5, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 5, 0.12).
narrative_ontology:measurement(balf_tr_t10, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 10, 0.1).
narrative_ontology:measurement(balf_tr_t15, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 15, 0.1).

% Extraction over time
narrative_ontology:measurement(balf_be_t0, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(balf_be_t5, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(balf_be_t10, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(balf_be_t15, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 15, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t0, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(balf_su_t5, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 5, 0.8).
narrative_ontology:measurement(balf_su_t10, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(balf_su_t15, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 15, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__jewish_national_home_primacy, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'balfour_mandate_instruments' kernel. This reading emphasizes the primacy of establishing a Jewish national home, leading to high extraction and suppression for the Palestinian Arab population. Other readings would yield different classifications and stakeholder dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
