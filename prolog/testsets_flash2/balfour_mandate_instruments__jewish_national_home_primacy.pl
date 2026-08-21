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
 *   human_readable: Balfour Mandate Instruments: Jewish National Home Primacy Reading
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This constraint represents the 'Jewish National Home Primacy' reading of
 *   the Balfour Mandate instruments, where the 'national home' was
 *   interpreted as a proto-state requiring demographic and territorial
 *   transformation. This reading prioritized land access, immigration
 *   facilitation for Jewish migrants, and the establishment of Jewish
 *   institutional supremacy, often at the expense of existing Palestinian
 *   Arab rights and political representation. The high extractiveness and
 *   suppression reflect the coercive nature of this interpretation's
 *   implementation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, 0.85).
domain_priors:suppression_score(balfour_mandate_instruments__jewish_national_home_primacy, 0.9).
domain_priors:theater_ratio(balfour_mandate_instruments__jewish_national_home_primacy, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, extractiveness, 0.85).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__jewish_national_home_primacy, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__jewish_national_home_primacy, "Balfour Mandate Instruments: Jewish National Home Primacy Reading").
narrative_ontology:topic_domain(balfour_mandate_instruments__jewish_national_home_primacy, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__jewish_national_home_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__jewish_national_home_primacy, '0d61eff8-4d73-43a2-a462-2c999a54f6f0').
narrative_ontology:cs_kernel_codification('0d61eff8-4d73-43a2-a462-2c999a54f6f0', fixed_text).
narrative_ontology:cs_authority_grounding('0d61eff8-4d73-43a2-a462-2c999a54f6f0', lineage).
narrative_ontology:cs_interpretation_layer_present('0d61eff8-4d73-43a2-a462-2c999a54f6f0').
narrative_ontology:cs_reading_relation('0d61eff8-4d73-43a2-a462-2c999a54f6f0', balfour_mandate_instruments__dual_obligation_indigenous_rights, forecloses).
narrative_ontology:cs_reading_relation('0d61eff8-4d73-43a2-a462-2c999a54f6f0', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('0d61eff8-4d73-43a2-a462-2c999a54f6f0', foundational, jewish_national_home_as_proto_state).
narrative_ontology:cs_axiom_status(jewish_national_home_as_proto_state, holdable).
narrative_ontology:cs_axiom_grounding('0d61eff8-4d73-43a2-a462-2c999a54f6f0', jewish_national_home_as_proto_state, conventional).
narrative_ontology:cs_axiom('0d61eff8-4d73-43a2-a462-2c999a54f6f0', foundational, demographic_transformation_as_mandate_goal).
narrative_ontology:cs_axiom_status(demographic_transformation_as_mandate_goal, holdable).
narrative_ontology:cs_axiom_grounding('0d61eff8-4d73-43a2-a462-2c999a54f6f0', demographic_transformation_as_mandate_goal, instrumental).
narrative_ontology:cs_reference_frame('0d61eff8-4d73-43a2-a462-2c999a54f6f0', balfour_declaration_as_foundational_charter).
narrative_ontology:cs_drift_state('0d61eff8-4d73-43a2-a462-2c999a54f6f0', end_of_mandate_1948, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0d61eff8-4d73-43a2-a462-2c999a54f6f0', '').
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

% Gained quasi-governmental status under Mandate Article 4, actively facilitating land acquisition, immigration, and institutional development for the Jewish national home. Directly benefited from the Mandate's interpretation as a proto-state project.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions, agenda_setter,
    institutional, generational, arbitrage, regional).

% Benefited from facilitated immigration, land access, and the establishment of a supportive institutional framework, enabling settlement and community building in Palestine.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, jewish_migrants, beneficiary,
    moderate, biographical, mobile, regional).

% Experienced systematic pressure and legal mechanisms facilitating land sales to Jewish entities, often leading to displacement and loss of ancestral lands, with limited legal recourse.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders, payer,
    powerless, generational, trapped, local).

% Structurally downgraded in political representation and influence, facing a Mandate administration that prioritized the Jewish national home project, leading to a systematic erosion of their authority and ability to represent their population's interests.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership, payer,
    powerless, generational, constrained, regional).

% Subjected to policies that favored Jewish immigration and settlement, leading to demographic shifts, economic marginalization, and a diminishing prospect of self-determination within their homeland.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_population, payer,
    powerless, generational, trapped, regional).

% Administered the Mandate, actively implementing policies that facilitated the Jewish national home project, including land transfers and immigration quotas, while suppressing Arab resistance. Its interpretive discretion was key to this reading's operationalization.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, british_mandatory_power, agenda_setter,
    institutional, generational, constrained, global).

% The international body that granted the Mandate, theoretically overseeing its implementation. Its oversight was largely ineffective in challenging the British interpretation or protecting Palestinian Arab rights under this reading.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, league_of_nations, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinated the establishment of a 'Jewish national home' in Palestine by directing British administrative efforts, facilitating Zionist institutional development, and managing immigration and land policies to achieve demographic and territorial transformation.
% TRANSFER_FUNCTION: Transferred land, political influence, and demographic advantage from the indigenous Palestinian Arab population to Zionist institutions and Jewish migrants, under the authority of the British Mandatory Power.
% ABSENT_VOICES: Palestinian Arab representatives and advocates for indigenous rights were systematically marginalized or excluded from decision-making processes, particularly regarding land and immigration policies. They would have argued for self-determination and protection of existing rights.
% DISAPPEARANCE_RATIONALE: If this interpretation of the Mandate had vanished, the trajectory of state formation in Palestine would have been fundamentally different. Land transfers would have been halted or reversed, immigration policies would have been non-discriminatory, and Palestinian Arab political aspirations would have been prioritized, leading to a vastly different geopolitical outcome.
% FOUNDING_PROBLEM: The problem of establishing a 'Jewish national home' as promised in the Balfour Declaration, requiring a framework for its realization through international recognition and administrative support.
% FOUNDING_PROBLEM_CORROBORATION: Zionist institutions and their supporters attest that the founding problem of establishing and securing a Jewish national home remains live. Palestinian Arab historians and international legal scholars, from outside the benefiting parties, corroborate the historical existence of this problem but contest its legitimacy and the means of its resolution.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__jewish_national_home_primacy, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__jewish_national_home_primacy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__jewish_national_home_primacy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(balfour_mandate_instruments__jewish_national_home_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__jewish_national_home_primacy, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) due to the systematic transfer of resources and political power from the Palestinian Arab population to Zionist institutions and Jewish migrants. Suppression is also very high (0.90) as this reading required active enforcement by the British Mandatory Power to overcome significant Palestinian Arab resistance and to maintain policies that were inherently discriminatory. The theater ratio is low (0.20) because the Mandate's stated 'dual obligation' was largely performative under this reading; the primary function was indeed the establishment of the Jewish national home, not a balanced development for both populations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Zionist institutions, this reading represented a legitimate and necessary coordination mechanism for state-building. From the perspective of the Palestinian Arab population, it was a clear snare, an extractive and suppressive colonial project. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist institutions and Jewish migrants were the primary beneficiaries, gaining land, political influence, and a secure institutional framework. The Palestinian Arab population, landholders, and political leadership were the primary victims, experiencing displacement, political marginalization, and suppression of their national aspirations. The British Mandatory Power acted as the agenda-setter, actively implementing and enforcing this interpretation, benefiting from maintaining imperial control and fulfilling its geopolitical commitments.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_intent_ambiguity,
    'Was the original intent of the Balfour Declaration and Mandate instruments primarily to establish a Jewish proto-state, or to create a ''national home'' within a framework that respected existing indigenous rights?',
    'Historical analysis of primary diplomatic and legal documents, including internal British government communications and League of Nations debates, to ascertain the dominant interpretive framework at the time of drafting.',
    'If the primary intent was a proto-state, this reading''s high extractiveness is consistent with the Mandate''s structural design. If the intent was a balanced approach, this reading represents a severe deviation and an extractive misinterpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_intent_ambiguity, conceptual, 'Ambiguity regarding the foundational intent of the Mandate instruments.').

omega_variable(
    land_transfer_coercion_level,
    'To what extent were land transfers from Palestinian Arabs to Jewish entities genuinely voluntary market transactions versus outcomes of structural coercion (e.g., debt, legal pressure, administrative policies)?',
    'Detailed socio-economic studies of land transactions during the Mandate period, analyzing prices, seller motivations, and the impact of British land ordinances and taxation policies.',
    'Higher levels of structural coercion would increase the effective extractiveness and suppression attributed to this reading, reinforcing its Snare-like qualities. Genuine voluntariness would slightly reduce the extractiveness, though the overall demographic impact would remain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_transfer_coercion_level, empirical, 'Degree of coercion in land transfers under the Mandate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__jewish_national_home_primacy, 1922, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1922, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1922, 0.1).
narrative_ontology:measurement(balf_tr_t1928, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1928, 0.15).
narrative_ontology:measurement(balf_tr_t1934, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1934, 0.2).
narrative_ontology:measurement(balf_tr_t1940, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1940, 0.25).
narrative_ontology:measurement(balf_tr_t1948, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1948, 0.2).

% Extraction over time
narrative_ontology:measurement(balf_be_t1922, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1922, 0.75).
narrative_ontology:measurement(balf_be_t1928, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1928, 0.8).
narrative_ontology:measurement(balf_be_t1934, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1934, 0.85).
narrative_ontology:measurement(balf_be_t1940, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1940, 0.88).
narrative_ontology:measurement(balf_be_t1948, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1948, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1922, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1922, 0.7).
narrative_ontology:measurement(balf_su_t1928, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1928, 0.78).
narrative_ontology:measurement(balf_su_t1934, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1934, 0.85).
narrative_ontology:measurement(balf_su_t1940, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1940, 0.9).
narrative_ontology:measurement(balf_su_t1948, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1948, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__jewish_national_home_primacy, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments__dual_obligation_indigenous_rights).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments__mandatory_interpretive_discretion).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, un_partition_plan_1947).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'balfour_mandate_instruments' kernel, each representing a distinct structural interpretation of the Mandate's intent and operationalization. This reading (Jewish National Home Primacy) directly influenced the UN Partition Plan and stands in tension with the Dual Obligation and Mandatory Interpretive Discretion readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
