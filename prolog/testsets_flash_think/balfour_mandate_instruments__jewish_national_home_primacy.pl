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
 *   This constraint story instantiates the 'Jewish National Home Primacy'
 *   reading of the Balfour Declaration and the League of Nations Mandate for
 *   Palestine. In this reading, the Mandate instruments are interpreted as
 *   directing demographic and territorial transformation to establish Jewish
 *   sovereignty, with the 'national home' understood as a proto-state. This
 *   required systematic facilitation of land access, immigration, and the
 *   supremacy of Jewish institutions, often at the expense of the existing
 *   Palestinian Arab population. The constraint is classified as a Tangled
 *   Rope due to its genuine coordination function (establishing a framework
 *   for Jewish settlement and institutional development) coupled with high,
 *   actively enforced extraction from the indigenous population.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, 0.85).
domain_priors:suppression_score(balfour_mandate_instruments__jewish_national_home_primacy, 0.9).
domain_priors:theater_ratio(balfour_mandate_instruments__jewish_national_home_primacy, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, extractiveness, 0.85).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__jewish_national_home_primacy, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__jewish_national_home_primacy, "Balfour Mandate: Jewish National Home Primacy Reading").
narrative_ontology:topic_domain(balfour_mandate_instruments__jewish_national_home_primacy, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__jewish_national_home_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__jewish_national_home_primacy, 'f59014a0-ce84-4997-8914-2f556e714329').
narrative_ontology:cs_kernel_codification('f59014a0-ce84-4997-8914-2f556e714329', fixed_text).
narrative_ontology:cs_authority_grounding('f59014a0-ce84-4997-8914-2f556e714329', lineage).
narrative_ontology:cs_interpretation_layer_present('f59014a0-ce84-4997-8914-2f556e714329').
narrative_ontology:cs_reading_relation('f59014a0-ce84-4997-8914-2f556e714329', balfour_mandate_instruments__dual_obligation_indigenous_rights, forecloses).
narrative_ontology:cs_reading_relation('f59014a0-ce84-4997-8914-2f556e714329', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('f59014a0-ce84-4997-8914-2f556e714329', foundational, jewish_national_home_implies_sovereignty).
narrative_ontology:cs_axiom_status(jewish_national_home_implies_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('f59014a0-ce84-4997-8914-2f556e714329', jewish_national_home_implies_sovereignty, conventional).
narrative_ontology:cs_axiom('f59014a0-ce84-4997-8914-2f556e714329', foundational, demographic_majority_is_prerequisite).
narrative_ontology:cs_axiom_status(demographic_majority_is_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('f59014a0-ce84-4997-8914-2f556e714329', demographic_majority_is_prerequisite, instrumental).
narrative_ontology:cs_reference_frame('f59014a0-ce84-4997-8914-2f556e714329', balfour_declaration_1917_intent).
narrative_ontology:cs_drift_state('f59014a0-ce84-4997-8914-2f556e714329', post_un_partition_plan, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('f59014a0-ce84-4997-8914-2f556e714329', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, jewish_migrants).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gained quasi-governmental status (Mandate Article 4), systematically facilitated land acquisition from Arab owners, and directed immigration to favor demographic transformation. Actively interpreted the 'national home' as a proto-state requiring Jewish institutional supremacy.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefited directly from facilitated immigration, land access, and the development of Jewish institutions under the Mandate's protective framework, contributing to the demographic transformation.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, jewish_migrants, beneficiary,
    moderate, biographical, mobile, global).

% Faced systematic land sales, often under duress or through legal mechanisms that favored Jewish acquisition, leading to displacement and loss of traditional livelihoods. Their land tenure was not adequately protected against the Mandate's primary objective.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders, payer,
    powerless, generational, constrained, local).

% Their political representation was structurally downgraded, and their claims for self-determination and protection of existing rights were largely unheeded, making them unable to effectively counter the Mandate's direction.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership, payer,
    powerless, biographical, trapped, regional).

% Administered the Mandate, balancing (or failing to balance) competing obligations. In this reading, their actions consistently prioritized the establishment of the Jewish national home, enforcing policies that facilitated demographic and territorial transformation.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, british_mandatory_power, agenda_setter,
    institutional, generational, constrained, global).

% Monitored the Mandate's implementation, often divided on interpretation. This seat represents the various international bodies and states observing the process, whose analyses and resolutions would later reflect the consequences of this primacy reading.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, international_community_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__jewish_national_home_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a framework for the administration of Palestine under British authority, aiming to facilitate the establishment of a Jewish national home by coordinating immigration, land policy, and institutional development.
% TRANSFER_FUNCTION: Systematically transferred land, political power, and demographic advantage from the existing Palestinian Arab population to Zionist institutions and Jewish migrants, as a means to achieve the 'national home' objective.
% ABSENT_VOICES: The majority Palestinian Arab population, whose self-determination claims and existing rights were largely unheeded in the framing and initial implementation of the Mandate. Their political structures were deliberately kept subordinate.
% DISAPPEARANCE_RATIONALE: If this interpretation and its enforcement had vanished, the entire state-building project in Palestine and the subsequent conflict would have unfolded fundamentally differently. Land ownership patterns, demographic composition, and political power structures would not have been transformed in the same way, leading to a vastly different historical trajectory.
% FOUNDING_PROBLEM: The perceived need for a secure national home for the Jewish people, following centuries of persecution and statelessness, combined with the geopolitical reordering of the Middle East after the collapse of the Ottoman Empire post-WWI.
% FOUNDING_PROBLEM_CORROBORATION: Zionist organizations and their international supporters attested to the problem's urgency and the Mandate's necessity. Palestinian Arab leaders and sympathetic international observers contested the premise that this required dispossessing the existing population. Subsequent UN resolutions and historical analyses provide external corroboration of the contestation regarding the founding problem's scope and proposed solution.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__jewish_national_home_primacy, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__jewish_national_home_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__jewish_national_home_primacy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is high (0.85) because the policies systematically dispossessed one population for the benefit of another, transferring significant resources and power. Suppression is also very high (0.90) as the British Mandatory Power actively enforced policies that curtailed Arab political development and land rights, often using force to quell resistance. The theater ratio is low (0.15) because the Mandate's stated goals, as interpreted by this reading, were actively pursued and enforced, with little performative maintenance masking an atrophied function. Accessibility collapse is high (0.75) for Palestinian Arabs, as alternatives to the Mandate's framework were systematically closed off. Resistance was high (0.80) from the Palestinian Arab population throughout the period.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Zionist institutions and Jewish migrants, the Mandate was a legitimate and necessary coordination mechanism for national self-determination. From the perspective of Palestinian Arabs, it was a coercive, extractive mechanism for colonial dispossession. The engine's classification will reflect this divergence based on the declared structural relationships and metrics, showing a Tangled Rope for the victims and a more Rope-like experience for the beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist institutions and Jewish migrants are the primary beneficiaries, gaining land, political influence, and a secure framework for settlement. Palestinian Arab landholders and political leadership are the primary victims, experiencing dispossession, political marginalization, and suppression of their national aspirations. The British Mandatory Power acts as the agenda-setter, enforcing the policies that enable this extraction, while also bearing some administrative costs and international scrutiny.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    national_home_definition_ambiguity,
    'Was ''national home'' in the Balfour Declaration and Mandate intended to mean a proto-state with sovereign aspirations, or a cultural/religious center within a broader political entity?',
    'Analysis of primary diplomatic correspondence and legal interpretations from the time, particularly those not directly affiliated with Zionist or Arab national movements.',
    'If ''proto-state'' is not the original or dominant intent, the extractiveness and suppression metrics for this reading would be re-evaluated as higher, as the policies would then represent an overreach beyond the original mandate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_home_definition_ambiguity, conceptual, 'Ambiguity in the foundational term ''national home''.').

omega_variable(
    dual_obligation_balance,
    'To what extent did the British Mandatory Power genuinely attempt to balance the ''dual obligation'' to facilitate a Jewish national home and protect the civil and religious rights of existing non-Jewish communities?',
    'Historical analysis of British administrative records, policy debates, and resource allocation decisions, particularly in response to Arab protests and commissions of inquiry.',
    'If the balance was consistently skewed towards the Jewish national home, it reinforces the high extractiveness and suppression of this reading. If genuine attempts at balance were made but failed, it might suggest a more complex, though still extractive, dynamic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dual_obligation_balance, empirical, 'British intent and execution of the ''dual obligation''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__jewish_national_home_primacy, 1920, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1920, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1920, 0.1).
narrative_ontology:measurement(balf_tr_t1925, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1925, 0.12).
narrative_ontology:measurement(balf_tr_t1930, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1930, 0.14).
narrative_ontology:measurement(balf_tr_t1935, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1935, 0.16).
narrative_ontology:measurement(balf_tr_t1940, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1940, 0.15).
narrative_ontology:measurement(balf_tr_t1948, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1948, 0.15).

% Extraction over time
narrative_ontology:measurement(balf_be_t1920, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1920, 0.7).
narrative_ontology:measurement(balf_be_t1925, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1925, 0.75).
narrative_ontology:measurement(balf_be_t1930, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1930, 0.8).
narrative_ontology:measurement(balf_be_t1935, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1935, 0.83).
narrative_ontology:measurement(balf_be_t1940, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1940, 0.86).
narrative_ontology:measurement(balf_be_t1948, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1948, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1920, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1920, 0.75).
narrative_ontology:measurement(balf_su_t1925, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1925, 0.8).
narrative_ontology:measurement(balf_su_t1930, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1930, 0.85).
narrative_ontology:measurement(balf_su_t1935, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1935, 0.88).
narrative_ontology:measurement(balf_su_t1940, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1940, 0.9).
narrative_ontology:measurement(balf_su_t1948, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1948, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__jewish_national_home_primacy, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
