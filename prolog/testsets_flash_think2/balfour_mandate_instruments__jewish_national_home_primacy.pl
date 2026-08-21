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
 *   constraint_id: balfour_mandate_instruments__jewish_national_home_primacy
 *   human_readable: Balfour Mandate: Jewish National Home Primacy Reading
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This constraint story instantiates the 'jewish_national_home_primacy'
 *   reading of the Balfour Mandate instruments. In this reading, the Mandate
 *   is interpreted as directing demographic and territorial transformation to
 *   establish Jewish sovereignty, with the 'national home' understood as a
 *   proto-state requiring facilitated land access, immigration, and Jewish
 *   institutional supremacy. This interpretation led to high extraction from
 *   the Palestinian Arab population and required active enforcement by the
 *   British Mandatory Power to suppress resistance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, 0.92).
domain_priors:suppression_score(balfour_mandate_instruments__jewish_national_home_primacy, 0.95).
domain_priors:theater_ratio(balfour_mandate_instruments__jewish_national_home_primacy, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, extractiveness, 0.92).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__jewish_national_home_primacy, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__jewish_national_home_primacy, "Balfour Mandate: Jewish National Home Primacy Reading").
narrative_ontology:topic_domain(balfour_mandate_instruments__jewish_national_home_primacy, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__jewish_national_home_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__jewish_national_home_primacy, '5c5ac468-dfea-443d-a430-858ed30e4db5').
narrative_ontology:cs_kernel_codification('5c5ac468-dfea-443d-a430-858ed30e4db5', fixed_text).
narrative_ontology:cs_authority_grounding('5c5ac468-dfea-443d-a430-858ed30e4db5', lineage).
narrative_ontology:cs_interpretation_layer_present('5c5ac468-dfea-443d-a430-858ed30e4db5').
narrative_ontology:cs_reading_relation('5c5ac468-dfea-443d-a430-858ed30e4db5', balfour_mandate_instruments__dual_obligation_indigenous_rights, forecloses).
narrative_ontology:cs_reading_relation('5c5ac468-dfea-443d-a430-858ed30e4db5', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('5c5ac468-dfea-443d-a430-858ed30e4db5', foundational, jewish_national_home_paramountcy).
narrative_ontology:cs_axiom_status(jewish_national_home_paramountcy, holdable).
narrative_ontology:cs_axiom_grounding('5c5ac468-dfea-443d-a430-858ed30e4db5', jewish_national_home_paramountcy, conventional).
narrative_ontology:cs_axiom('5c5ac468-dfea-443d-a430-858ed30e4db5', foundational, demographic_territorial_transformation_legitimacy).
narrative_ontology:cs_axiom_status(demographic_territorial_transformation_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('5c5ac468-dfea-443d-a430-858ed30e4db5', demographic_territorial_transformation_legitimacy, conventional).
narrative_ontology:cs_reference_frame('5c5ac468-dfea-443d-a430-858ed30e4db5', jewish_national_home_establishment).
narrative_ontology:cs_drift_state('5c5ac468-dfea-443d-a430-858ed30e4db5', end_of_mandate_1948, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5c5ac468-dfea-443d-a430-858ed30e4db5', '').
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

% Actively interpreted the Mandate to facilitate Jewish land acquisition, immigration, and institutional development, effectively gaining quasi-governmental status. Benefited directly from the Mandate's implementation.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefited from facilitated immigration, land access, and the development of a supportive institutional framework under the Mandate, enabling settlement and community building.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, jewish_migrants, beneficiary,
    moderate, biographical, mobile, global).

% Experienced systematic land sales and transfers, often under duress or through legal mechanisms that favored Jewish acquisition, leading to displacement and economic marginalization.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders, payer,
    powerless, generational, trapped, local).

% Their political representation and self-determination aspirations were structurally downgraded and subordinated to the Mandate's primary objective of establishing a Jewish national home, leading to persistent political disempowerment.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership, payer,
    powerless, biographical, constrained, regional).

% As the indigenous population, they bore the brunt of demographic and territorial transformation, facing dispossession, political marginalization, and the erosion of their collective identity and future prospects in their homeland.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_population, payer,
    powerless, generational, identity_locked, local).

% Administered the Mandate, enforcing its terms and balancing (or failing to balance) competing obligations. From this reading, their actions primarily facilitated the establishment of the Jewish national home, despite later policy shifts.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, british_mandatory_power, agenda_setter,
    institutional, generational, arbitrage, global).

% The international body that granted the Mandate, theoretically overseeing its implementation. However, its enforcement power was limited, and it largely deferred to the Mandatory Power's interpretation and actions.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, league_of_nations, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__jewish_national_home_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish an internationally recognized framework for the development of a Jewish national home in Palestine, facilitating Jewish immigration and settlement under British administration.
% TRANSFER_FUNCTION: Transfers land, political power, and demographic control from the existing Palestinian Arab population to Zionist institutions and Jewish migrants, under the administration of the British Mandatory Power, in service of establishing a Jewish proto-state.
% ABSENT_VOICES: Palestinian Arab representatives were largely excluded from the drafting of the Mandate and its initial implementation, and their political aspirations for self-determination were systematically subordinated. They would have objected to the primacy given to the Jewish national home over their existing rights.
% DISAPPEARANCE_RATIONALE: The entire political, demographic, and territorial structure of Mandatory Palestine, and subsequently the State of Israel, is fundamentally shaped by these instruments and their interpretation. Their disappearance would necessitate a complete re-evaluation and reorganization of the region's history and current geopolitical reality.
% FOUNDING_PROBLEM: The Zionist movement's aspiration for a Jewish national home, coupled with the perceived need for international recognition and facilitation of this goal following World War I and the collapse of the Ottoman Empire.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historical narratives and institutional records consistently corroborate this framing, emphasizing the historical necessity and international legitimacy of the Jewish national home. However, international legal scholars and Palestinian historians widely contest this, arguing it was a colonial imposition that disregarded indigenous rights.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__jewish_national_home_primacy, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__jewish_national_home_primacy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__jewish_national_home_primacy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(balfour_mandate_instruments__jewish_national_home_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__jewish_national_home_primacy, 0.92, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high and increasing because the Mandate's implementation, under this reading, systematically transferred resources and power from one group to another. Suppression is also very high, as the transformation was met with significant Palestinian resistance, requiring continuous and often violent enforcement by the British. Theater ratio is low, as the enforcement was direct and functional for achieving the stated (from this reading's perspective) goals, rather than merely performative. Accessibility collapse is high for Palestinians, as their alternatives for self-determination and land retention were severely limited by the Mandate's structure.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Zionist institutions, the Mandate was a legitimate international instrument for national self-determination. From the Palestinian perspective, it was a colonial imposition that systematically dispossessed them. The engine's classification will highlight this divergence by computing different effective extraction values for each seat based on their structural relationship to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist institutions and Jewish migrants are clear beneficiaries, gaining land, political influence, and facilitated immigration. Palestinian Arab landholders, political leadership, and the general population are victims, experiencing dispossession, political marginalization, and demographic pressure. The British Mandatory Power acts as an agenda-setter, enforcing the terms of the Mandate, which, under this reading, primarily served the 'national home' objective.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_intent_vs_outcome,
    'Was the original intent of the Mandate primarily a coordination mechanism for a ''national home'' with incidental extraction, or was asymmetric extraction baked into its core design from the outset?',
    'Analysis of primary diplomatic records and internal British policy documents from the drafting period, alongside contemporary international legal interpretations of self-determination and colonial mandates.',
    'If extraction was inherent, the constraint''s base extractiveness is structurally higher and less amenable to ''fixing'' without fundamental re-evaluation. If intent was coordination, the high extraction is a drift from original purpose.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_intent_vs_outcome, conceptual, 'Ambiguity regarding the Mandate''s foundational intent versus its operational outcome.').

omega_variable(
    suppression_legitimacy_ambiguity,
    'To what extent was the British Mandatory Power''s suppression of Palestinian resistance a legitimate enforcement of international law, versus a coercive act to facilitate a colonial project?',
    'Historical and legal analysis comparing British actions to contemporary international norms regarding self-determination, indigenous rights, and the use of force in mandated territories, as well as the League of Nations'' own oversight mechanisms.',
    'If suppression is deemed illegitimate, it amplifies the constraint''s effective extraction and reclassifies the enforcement as purely coercive. If deemed legitimate, it dampens the perceived extractiveness of the enforcement component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_legitimacy_ambiguity, conceptual, 'Legitimacy of British suppression of Palestinian resistance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__jewish_national_home_primacy, 1922, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1922, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1922, 0.1).
narrative_ontology:measurement(balf_tr_t1927, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1927, 0.1).
narrative_ontology:measurement(balf_tr_t1932, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1932, 0.12).
narrative_ontology:measurement(balf_tr_t1937, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1937, 0.15).
narrative_ontology:measurement(balf_tr_t1942, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1942, 0.18).
narrative_ontology:measurement(balf_tr_t1948, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1948, 0.2).

% Extraction over time
narrative_ontology:measurement(balf_be_t1922, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1922, 0.75).
narrative_ontology:measurement(balf_be_t1927, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1927, 0.8).
narrative_ontology:measurement(balf_be_t1932, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1932, 0.85).
narrative_ontology:measurement(balf_be_t1937, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1937, 0.88).
narrative_ontology:measurement(balf_be_t1942, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1942, 0.9).
narrative_ontology:measurement(balf_be_t1948, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1948, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1922, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1922, 0.8).
narrative_ontology:measurement(balf_su_t1927, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1927, 0.85).
narrative_ontology:measurement(balf_su_t1932, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1932, 0.88).
narrative_ontology:measurement(balf_su_t1937, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1937, 0.9).
narrative_ontology:measurement(balf_su_t1942, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1942, 0.92).
narrative_ontology:measurement(balf_su_t1948, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1948, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__jewish_national_home_primacy, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, state_of_israel_founding_principles).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'balfour_mandate_instruments' kernel, focusing on the primacy of establishing a Jewish national home. Other readings emphasize dual obligations or British interpretive discretion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
