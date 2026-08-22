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
 *   human_readable: Balfour Mandate: Jewish National Home Primacy Reading
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This constraint story analyzes the Balfour Mandate instruments through
 *   the 'Jewish National Home Primacy' reading, which interpreted the Mandate
 *   as directing demographic and territorial transformation to establish
 *   Jewish sovereignty. This reading prioritized land access, immigration
 *   facilitation, and Jewish institutional supremacy, effectively treating
 *   the 'national home' as a proto-state. The Mandate, under this
 *   interpretation, functioned as a highly extractive and suppressive
 *   mechanism for the Palestinian Arab population, while providing
 *   significant benefits and coordination for Zionist institutions and Jewish
 *   migrants.
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
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__jewish_national_home_primacy, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__jewish_national_home_primacy, "Balfour Mandate: Jewish National Home Primacy Reading").
narrative_ontology:topic_domain(balfour_mandate_instruments__jewish_national_home_primacy, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__jewish_national_home_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__jewish_national_home_primacy, 'e937561a-5e79-4e14-b5cb-84fe1dfeb332').
narrative_ontology:cs_kernel_codification('e937561a-5e79-4e14-b5cb-84fe1dfeb332', formalized).
narrative_ontology:cs_authority_grounding('e937561a-5e79-4e14-b5cb-84fe1dfeb332', lineage).
narrative_ontology:cs_interpretation_layer_present('e937561a-5e79-4e14-b5cb-84fe1dfeb332').
narrative_ontology:cs_reading_relation('e937561a-5e79-4e14-b5cb-84fe1dfeb332', balfour_mandate_instruments__dual_obligation_indigenous_rights, forecloses).
narrative_ontology:cs_reading_relation('e937561a-5e79-4e14-b5cb-84fe1dfeb332', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('e937561a-5e79-4e14-b5cb-84fe1dfeb332', foundational, jewish_national_home_as_proto_state).
narrative_ontology:cs_axiom_status(jewish_national_home_as_proto_state, holdable).
narrative_ontology:cs_axiom_grounding('e937561a-5e79-4e14-b5cb-84fe1dfeb332', jewish_national_home_as_proto_state, conventional).
narrative_ontology:cs_axiom('e937561a-5e79-4e14-b5cb-84fe1dfeb332', foundational, demographic_transformation_as_mandate_goal).
narrative_ontology:cs_axiom_status(demographic_transformation_as_mandate_goal, holdable).
narrative_ontology:cs_axiom_grounding('e937561a-5e79-4e14-b5cb-84fe1dfeb332', demographic_transformation_as_mandate_goal, instrumental).
narrative_ontology:cs_reference_frame('e937561a-5e79-4e14-b5cb-84fe1dfeb332', balfour_declaration_as_foundational_charter).
narrative_ontology:cs_drift_state('e937561a-5e79-4e14-b5cb-84fe1dfeb332', contemporary_international_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e937561a-5e79-4e14-b5cb-84fe1dfeb332', '').
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

% Acted as a quasi-governmental body (Jewish Agency) under the Mandate, facilitating land acquisition, immigration, and institutional development for the Jewish population. Benefited directly from the Mandate's interpretation prioritizing the 'national home'.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions, agenda_setter,
    institutional, generational, mobile, regional).

% Benefited from facilitated immigration, land access, and the development of a supportive institutional infrastructure, all directed towards establishing a Jewish majority and eventual state.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, jewish_migrants, beneficiary,
    moderate, biographical, constrained, regional).

% Experienced systematic pressure and legal mechanisms facilitating land transfer to Jewish entities, often leading to displacement and loss of ancestral lands. Their traditional land tenure was undermined by the Mandate's interpretation.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders, payer,
    powerless, generational, trapped, local).

% Their political aspirations for self-determination were systematically subordinated to the 'national home' project. Their institutions were denied equivalent status to Zionist ones, and their resistance was often suppressed by the Mandatory power.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership, payer,
    organized, biographical, constrained, regional).

% As a whole, faced demographic transformation, loss of political control, and the erosion of their national identity and rights under an interpretation of the Mandate that prioritized Jewish state-building over their existing presence and claims.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_population, payer,
    powerless, generational, identity_locked, regional).

% Administered the Mandate, actively enforcing policies that facilitated Jewish immigration and land acquisition, and suppressing Arab resistance. While claiming neutrality, its actions consistently favored the 'national home' interpretation.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, british_mandatory_power, agenda_setter,
    institutional, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a framework for the administration of Palestine that would facilitate the creation of a 'Jewish national home' while maintaining civil and religious rights for non-Jewish communities.
% TRANSFER_FUNCTION: Transferred land, political authority, and demographic advantage from the existing Palestinian Arab population to Zionist institutions and Jewish migrants, under the guise of international administration.
% ABSENT_VOICES: The vast majority of the Palestinian Arab population, whose consent was never sought for the Mandate's terms or its interpretation, and whose political representation was systematically marginalized. Their perspective on self-determination was actively suppressed.
% DISAPPEARANCE_RATIONALE: If this interpretation of the Mandate had vanished, the demographic and territorial transformation would not have occurred, the state of Israel would not have been established in its historical form, and the subsequent conflict would have taken a fundamentally different trajectory. The entire political and social structure of the region would be unrecognizable.
% FOUNDING_PROBLEM: The problem of establishing a 'Jewish national home' in Palestine, as articulated in the Balfour Declaration, and managing the competing claims and populations in the territory after the collapse of the Ottoman Empire.
% FOUNDING_PROBLEM_CORROBORATION: Zionist institutions and their supporters continue to assert the founding problem is live, citing ongoing security needs and the historical imperative of a Jewish state. Palestinian voices and international legal scholars, from outside the benefiting parties, corroborate the historical existence of the problem but contest its current status as a justification for ongoing extraction.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__jewish_national_home_primacy, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__jewish_national_home_primacy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__jewish_national_home_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   The high extractiveness (0.85) reflects the systematic transfer of land and political power. Suppression (0.90) was severe, enforced by the British Mandatory power against Arab resistance, and by the structural downgrading of Arab political representation. The theater ratio (0.20) is relatively low, as the Mandate's stated purpose of facilitating the 'national home' was actively pursued, with less performative cover for other functions. The claimed type is 'tangled_rope' because it presented a coordination function (establishing a national home) but operated with severe asymmetric extraction and required active enforcement to hold.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Zionist institutions, the Mandate was a legitimate international instrument for national self-determination, a 'rope' coordinating the establishment of a homeland. From the perspective of the Palestinian Arab population, it was a 'snare' or 'tangled rope' that dispossessed them and suppressed their rights under colonial administration. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist institutions and Jewish migrants were clear beneficiaries, receiving facilitated immigration and institutional support. Palestinian Arab landholders and political leadership were primary victims, experiencing land loss, political marginalization, and suppression of self-determination. The British Mandatory Power acted as an agenda-setter, enforcing the Mandate's terms in a way that consistently favored the 'national home' primacy reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_original_intent,
    'Was the original intent of the Balfour Declaration and Mandate instruments to establish a Jewish proto-state, or a ''national home'' within a broader framework of indigenous rights?',
    'Historical analysis of diplomatic correspondence, legal interpretations by international bodies contemporaneous with the Mandate''s drafting, and the records of the League of Nations Permanent Mandates Commission.',
    'If the original intent was primarily proto-state formation, this reading''s high extractiveness is consistent with the Mandate''s core purpose. If the intent was more balanced, the high extractiveness indicates a deviation from the original coordination function, strengthening the ''snare'' aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_original_intent, conceptual, 'Ambiguity regarding the Mandate''s foundational purpose.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the measured suppression primarily structural (legal/administrative barriers) or internalized (cognitive patterns of resignation/despair among the Palestinian Arab population)?',
    'Post-Mandate analysis of Palestinian political mobilization and resistance movements: if resistance persisted and intensified, structural suppression was dominant; if it waned despite continued grievances, internalized suppression played a larger role.',
    'If internalized, the constraint''s effective suppression was higher than the structural measure suggests, as the target population carried the suppression with them. If purely structural, removing the Mandate''s enforcement would have immediately altered the power dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__jewish_national_home_primacy, 1922, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1922, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1922, 0.25).
narrative_ontology:measurement(balf_tr_t1928, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1928, 0.22).
narrative_ontology:measurement(balf_tr_t1934, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1934, 0.2).
narrative_ontology:measurement(balf_tr_t1940, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1940, 0.18).
narrative_ontology:measurement(balf_tr_t1948, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1948, 0.2).

% Extraction over time
narrative_ontology:measurement(balf_be_t1922, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1922, 0.75).
narrative_ontology:measurement(balf_be_t1928, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1928, 0.78).
narrative_ontology:measurement(balf_be_t1934, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1934, 0.82).
narrative_ontology:measurement(balf_be_t1940, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1940, 0.85).
narrative_ontology:measurement(balf_be_t1948, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1948, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1922, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1922, 0.75).
narrative_ontology:measurement(balf_su_t1928, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1928, 0.8).
narrative_ontology:measurement(balf_su_t1934, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1934, 0.85).
narrative_ontology:measurement(balf_su_t1940, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1940, 0.9).
narrative_ontology:measurement(balf_su_t1948, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1948, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__jewish_national_home_primacy, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments__dual_obligation_indigenous_rights).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments__mandatory_interpretive_discretion).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'balfour_mandate_instruments' kernel. This specific reading, 'jewish_national_home_primacy', interprets the Mandate as a directive for Jewish state-building, leading to high extraction and suppression. Sibling readings offer alternative interpretations with different structural outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
