% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__cultural_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__cultural_zionism_reading, []).

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
 *   constraint_id: jewish_territorial_claim__cultural_zionism_reading
 *   human_readable: Jewish Cultural Center in Palestine (Cultural Zionism Reading)
 *   domain: political_history/nationalism_studies/settler_colonialism
 *
 * SUMMARY:
 *   This constraint story instantiates the 'cultural Zionism' reading of the
 *   broader 'Jewish territorial claim' kernel. It describes the vision of
 *   establishing a Jewish spiritual and cultural center in Palestine,
 *   prioritizing cultural and national regeneration over explicit political
 *   sovereignty or demographic majority. While its stated intent was less
 *   overtly political or extractive than other Zionist currents, its
 *   implementation still involved settlement in a contested land, leading to
 *   an inherent, albeit lower, degree of extraction and suppression from the
 *   indigenous Palestinian population. The metrics reflect this gap between
 *   ideal and reality, showing a 'rope' claimed type with moderate
 *   extractiveness and suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__cultural_zionism_reading, 0.35).
domain_priors:suppression_score(jewish_territorial_claim__cultural_zionism_reading, 0.4).
domain_priors:theater_ratio(jewish_territorial_claim__cultural_zionism_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__cultural_zionism_reading, rope).
narrative_ontology:human_readable(jewish_territorial_claim__cultural_zionism_reading, "Jewish Cultural Center in Palestine (Cultural Zionism Reading)").
narrative_ontology:topic_domain(jewish_territorial_claim__cultural_zionism_reading, "political_history/nationalism_studies/settler_colonialism").

domain_priors:requires_active_enforcement(jewish_territorial_claim__cultural_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__cultural_zionism_reading, '60baeff8-31eb-4c71-b51b-c114e7344627').
narrative_ontology:cs_kernel_codification('60baeff8-31eb-4c71-b51b-c114e7344627', implicit).
narrative_ontology:cs_authority_grounding('60baeff8-31eb-4c71-b51b-c114e7344627', lineage).
narrative_ontology:cs_interpretation_layer_present('60baeff8-31eb-4c71-b51b-c114e7344627').
narrative_ontology:cs_reading_relation('60baeff8-31eb-4c71-b51b-c114e7344627', jewish_territorial_claim__political_zionism_reading, influences).
narrative_ontology:cs_reading_relation('60baeff8-31eb-4c71-b51b-c114e7344627', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('60baeff8-31eb-4c71-b51b-c114e7344627', jewish_territorial_claim__revisionist_zionism_reading, forecloses).
narrative_ontology:cs_axiom('60baeff8-31eb-4c71-b51b-c114e7344627', foundational, jewish_spiritual_cultural_autonomy_in_palestine).
narrative_ontology:cs_axiom_status(jewish_spiritual_cultural_autonomy_in_palestine, holdable).
narrative_ontology:cs_axiom_grounding('60baeff8-31eb-4c71-b51b-c114e7344627', jewish_spiritual_cultural_autonomy_in_palestine, deontological).
narrative_ontology:cs_axiom('60baeff8-31eb-4c71-b51b-c114e7344627', secondary, binational_coexistence_potential).
narrative_ontology:cs_axiom_status(binational_coexistence_potential, holdable).
narrative_ontology:cs_axiom_grounding('60baeff8-31eb-4c71-b51b-c114e7344627', binational_coexistence_potential, conventional).
narrative_ontology:cs_reference_frame('60baeff8-31eb-4c71-b51b-c114e7344627', spiritual_cultural_renaissance_in_palestine).
narrative_ontology:cs_drift_state('60baeff8-31eb-4c71-b51b-c114e7344627', pre_1948_political_realities, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('60baeff8-31eb-4c71-b51b-c114e7344627', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, cultural_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, jewish_cultural_institutions).
narrative_ontology:constraint_victim(jewish_territorial_claim__cultural_zionism_reading, palestinian_indigenous_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for and organizes the establishment of Jewish cultural and spiritual centers in Palestine, emphasizing quality of life and cultural output over political sovereignty or demographic majority. They guide settlement patterns and institutional development.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, cultural_zionist_movement, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the establishment of a dedicated space for Jewish cultural and spiritual expression, receiving resources and legitimacy from the movement. They are the direct recipients of the cultural 'center' being built.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, jewish_cultural_institutions, beneficiary,
    organized, biographical, mobile, national).

% Bears the costs of land acquisition, resource use, and the imposition of a new cultural presence in their ancestral homeland, even if not overtly political. Their claims to self-determination and land are implicitly challenged by any form of Jewish settlement.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, palestinian_indigenous_population, payer,
    powerless, generational, trapped, regional).

% Historically, these authorities controlled Palestine and, through policies like land sales and immigration permits, facilitated or constrained Jewish settlement, thereby shaping the physical manifestation of cultural Zionism. Their actions had a direct impact on the viability of the cultural center.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, ottoman_british_authorities, agenda_setter,
    institutional, biographical, constrained, regional).

% Observe the development of cultural Zionism and often advocate for a binational state where both Jewish and Palestinian national aspirations could be realized without one dominating the other. They analyze the potential for cultural Zionism to align with or diverge from this vision.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, binational_advocates, observer,
    moderate, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish and sustain a vibrant Jewish spiritual and cultural life in Palestine, fostering a sense of national identity and continuity without necessarily relying on political statehood or demographic majority.
% TRANSFER_FUNCTION: Transfers land, resources, and cultural space from the indigenous Palestinian population to Jewish settlers and institutions, in exchange for the promise of a non-dominating, culturally rich presence.
% ABSENT_VOICES: Palestinian political leadership and nationalist movements are largely absent from the internal discourse of cultural Zionism; they would argue that any form of Jewish territorial claim, even cultural, inherently undermines Palestinian self-determination and rights.
% DISAPPEARANCE_RATIONALE: If the cultural Zionist claim vanished, the political and demographic landscape of Palestine would be fundamentally altered, potentially opening pathways for alternative political arrangements and land claims, as the foundational justification for Jewish settlement would be removed.
% FOUNDING_PROBLEM: The perceived spiritual and cultural decay of Jewish life in the diaspora, coupled with persistent antisemitism, leading to a desire for national regeneration and a return to the ancestral homeland as a cultural and spiritual center.
% FOUNDING_PROBLEM_CORROBORATION: Primarily attested by cultural Zionist thinkers, historians, and adherents. External corroboration for the 'problem' as a justification for territorial claim is limited, as the problem is largely internal to Jewish identity and experience, though antisemitism is externally verifiable.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__cultural_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__cultural_zionism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__cultural_zionism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jewish_territorial_claim__cultural_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__cultural_zionism_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__cultural_zionism_reading_tests).
:- end_tests(jewish_territorial_claim__cultural_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) and suppression (0.40) are moderate, reflecting that while cultural Zionism did not explicitly seek political domination, its settlement activities still entailed land acquisition and resource use that impacted the indigenous population. The 'rope' classification is based on its primary function of coordinating Jewish cultural and spiritual life. Theater ratio is low (0.15) as the movement was genuinely focused on cultural building, not mere performance. The temporal measurements show a gradual increase in extractiveness and suppression as the political realities of Palestine intensified, pushing the cultural vision into a more contested space.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of cultural Zionists, the constraint is a legitimate and necessary coordination mechanism for Jewish national and spiritual revival. From the Palestinian perspective, even a 'cultural' claim to land is an act of dispossession and an imposition, regardless of its stated non-political intent. The engine's classification will highlight this divergence by computing different effective extraction for each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The cultural Zionist movement and its institutions are beneficiaries, as they achieve their goals of cultural regeneration. The Palestinian indigenous population are victims, bearing the costs of displacement and the challenge to their own national aspirations. Ottoman/British authorities acted as agenda-setters, controlling the conditions under which settlement occurred. Binational advocates serve as observers, analyzing the constraint's implications.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_vs_political_impact,
    'Can a ''cultural center'' be established in a contested territory without inherently having political and demographic impacts that are extractive for the indigenous population?',
    'Comparative analysis of other settler-colonial contexts where cultural movements preceded or accompanied political ones, assessing the unavoidable political consequences of cultural settlement.',
    'If cultural settlement is found to be inherently political in its effects, the measured extractiveness and suppression for this reading would be re-evaluated upwards, potentially shifting its classification towards a ''tangled_rope'' or ''snare'' from the victim''s seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_vs_political_impact, conceptual, 'Ambiguity of ''cultural'' claims in a contested political landscape.').

omega_variable(
    binational_potential_realization,
    'Was the potential for a binational framework, inherent in cultural Zionism''s less maximalist vision, genuinely pursued or was it always secondary to an implicit drive for Jewish majority?',
    'Historical analysis of policy proposals, diplomatic efforts, and internal debates within the cultural Zionist movement regarding power-sharing and minority rights with Palestinians.',
    'If genuine pursuit of binationalism is confirmed, the constraint''s suppression and extractiveness might be seen as lower, reflecting a more inclusive intent. If it was always secondary, the values would remain as authored, reflecting the de facto imposition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(binational_potential_realization, empirical, 'The sincerity and feasibility of binational coexistence within cultural Zionism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__cultural_zionism_reading, 1880, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1880, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1880, 0.1).
narrative_ontology:measurement(jewi_tr_t1900, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(jewi_tr_t1920, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1920, 0.14).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1948, 0.15).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1880, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1880, 0.25).
narrative_ontology:measurement(jewi_be_t1900, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1900, 0.28).
narrative_ontology:measurement(jewi_be_t1920, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1920, 0.32).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1948, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1880, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1880, 0.3).
narrative_ontology:measurement(jewi_su_t1900, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1900, 0.34).
narrative_ontology:measurement(jewi_su_t1920, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1920, 0.38).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1948, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__cultural_zionism_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'Jewish territorial claim' kernel, focusing on cultural Zionism. It is linked to other Zionist readings which represent different structural claims and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
