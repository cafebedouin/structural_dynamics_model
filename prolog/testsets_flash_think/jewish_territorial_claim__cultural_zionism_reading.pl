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
 *   human_readable: Cultural Zionist Vision for a Jewish Center in Palestine
 *   domain: political_history/nationalism_studies/cultural_studies
 *
 * SUMMARY:
 *   This constraint represents the Cultural Zionist reading of the Jewish
 *   territorial claim in Palestine, emphasizing the establishment of a Jewish
 *   spiritual and cultural center without necessarily requiring political
 *   sovereignty or a demographic majority. It prioritizes cultural and
 *   intellectual development, Hebrew language revival, and the creation of a
 *   vibrant Jewish national culture in the ancestral homeland. While
 *   ideologically distinct from political Zionism, its practical
 *   implementation through settlement and institutional building in a
 *   contested land inevitably had political and demographic consequences.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__cultural_zionism_reading, 0.55).
domain_priors:suppression_score(jewish_territorial_claim__cultural_zionism_reading, 0.55).
domain_priors:theater_ratio(jewish_territorial_claim__cultural_zionism_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__cultural_zionism_reading, rope).
narrative_ontology:human_readable(jewish_territorial_claim__cultural_zionism_reading, "Cultural Zionist Vision for a Jewish Center in Palestine").
narrative_ontology:topic_domain(jewish_territorial_claim__cultural_zionism_reading, "political_history/nationalism_studies/cultural_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__cultural_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__cultural_zionism_reading, 'f37f24b3-fa6d-4cba-a067-efb20c9b9e7d').
narrative_ontology:cs_kernel_codification('f37f24b3-fa6d-4cba-a067-efb20c9b9e7d', implicit).
narrative_ontology:cs_authority_grounding('f37f24b3-fa6d-4cba-a067-efb20c9b9e7d', practice).
narrative_ontology:cs_interpretation_layer_present('f37f24b3-fa6d-4cba-a067-efb20c9b9e7d').
narrative_ontology:cs_reading_relation('f37f24b3-fa6d-4cba-a067-efb20c9b9e7d', jewish_territorial_claim__political_zionism_reading, influences).
narrative_ontology:cs_reading_relation('f37f24b3-fa6d-4cba-a067-efb20c9b9e7d', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('f37f24b3-fa6d-4cba-a067-efb20c9b9e7d', jewish_territorial_claim__revisionist_zionism_reading, forecloses).
narrative_ontology:cs_axiom('f37f24b3-fa6d-4cba-a067-efb20c9b9e7d', foundational, jewish_spiritual_cultural_autonomy_in_palestine).
narrative_ontology:cs_axiom_status(jewish_spiritual_cultural_autonomy_in_palestine, holdable).
narrative_ontology:cs_axiom_grounding('f37f24b3-fa6d-4cba-a067-efb20c9b9e7d', jewish_spiritual_cultural_autonomy_in_palestine, deontological).
narrative_ontology:cs_axiom('f37f24b3-fa6d-4cba-a067-efb20c9b9e7d', foundational, political_sovereignty_not_prerequisite_for_national_home).
narrative_ontology:cs_axiom_status(political_sovereignty_not_prerequisite_for_national_home, holdable).
narrative_ontology:cs_axiom_grounding('f37f24b3-fa6d-4cba-a067-efb20c9b9e7d', political_sovereignty_not_prerequisite_for_national_home, deontological).
narrative_ontology:cs_reference_frame('f37f24b3-fa6d-4cba-a067-efb20c9b9e7d', spiritual_cultural_renaissance).
narrative_ontology:cs_drift_state('f37f24b3-fa6d-4cba-a067-efb20c9b9e7d', pre_state_israel_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f37f24b3-fa6d-4cba-a067-efb20c9b9e7d', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, jewish_cultural_institutions).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, jewish_intellectuals).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, jewish_immigrants_to_palestine).
narrative_ontology:constraint_victim(jewish_territorial_claim__cultural_zionism_reading, palestinian_arabs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively establish and maintain cultural, educational, and spiritual centers in Palestine, promoting Hebrew language, arts, and scholarship. They define the scope and nature of the cultural presence.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, jewish_cultural_institutions, agenda_setter,
    institutional, generational, constrained, regional).

% Find a spiritual and intellectual home for Jewish thought and creativity, contributing to and benefiting from the cultural renaissance envisioned by this reading. They are key proponents and shapers of the cultural narrative.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, jewish_intellectuals, beneficiary,
    powerful, biographical, mobile, global).

% Seek to live in Palestine as part of a vibrant Jewish cultural and spiritual community, without necessarily seeking political dominance. They benefit from the established cultural infrastructure but face the realities of living in a contested land.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, jewish_immigrants_to_palestine, beneficiary,
    moderate, biographical, constrained, local).

% Experience the establishment of Jewish cultural centers as a form of encroachment on their land and cultural space, even if not overtly political. Their claims to land and self-determination are implicitly challenged by a growing Jewish presence, leading to marginalization and resource competition.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, palestinian_arabs, payer,
    powerless, generational, trapped, local).

% Historically governed Palestine and influenced the conditions under which Jewish immigration and cultural development occurred. Their policies, though not directly aligned with Cultural Zionism, shaped its practical implementation and interaction with the local population.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, ottoman_british_authorities, agenda_setter,
    institutional, biographical, arbitrage, regional).

% Observe and sometimes support cultural initiatives in Palestine, often without fully engaging with the political implications of such activities in a contested territory. They may provide funding or recognition for cultural projects.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, international_cultural_organizations, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the establishment and flourishing of Jewish cultural and spiritual life in Palestine, fostering a shared identity, intellectual hub, and a 'center' for the Jewish people, without requiring political sovereignty or demographic majority.
% TRANSFER_FUNCTION: Transfers cultural influence, intellectual capital, and, implicitly, land and resources towards Jewish institutions and individuals in Palestine, away from existing Palestinian inhabitants, through settlement and institutional building.
% ABSENT_VOICES: Palestinian political leaders and local communities are structurally excluded from the framing of this cultural vision; they would object to any form of Zionist claim on the land, regardless of its stated cultural intent, viewing it as an inherent challenge to their own national and cultural rights.
% DISAPPEARANCE_RATIONALE: If the cultural Zionist claim and its associated institutions vanished overnight, the historical trajectory of Jewish settlement in Palestine would be fundamentally altered. The impetus for a non-political Jewish presence would disappear, leading to different demographic, cultural, and political realities in the region.
% FOUNDING_PROBLEM: The perceived spiritual and cultural decline of Jewish life in the diaspora, and the desire for a revitalized Jewish identity rooted in the ancestral homeland, fostering a 'spiritual center' for the Jewish people.
% FOUNDING_PROBLEM_CORROBORATION: Jewish cultural and religious leaders, and historians of Zionism attest to the founding problem. However, the claim that this vision is non-extractive or non-displacing in practice is not corroborated by Palestinian sources or independent observers, who view any Zionist presence as inherently impacting their claims.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__cultural_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__cultural_zionism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__cultural_zionism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jewish_territorial_claim__cultural_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__cultural_zionism_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__cultural_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__cultural_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__cultural_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.55) and suppression (0.55) are moderate, reflecting the inherent tension of establishing a 'cultural center' in a land already inhabited and claimed by another people. While the intent was not overtly extractive or coercive, the act of settlement and resource acquisition, even for cultural purposes, implicitly displaced and marginalized existing Palestinian Arab communities. Active enforcement (true) was required to protect and maintain these cultural institutions and settlements against local resistance, even if not for political sovereignty. The theater ratio is low (0.15) because the movement was genuinely focused on cultural building, not mere performance. The claimed type is 'rope' as it aims for coordination (of Jewish cultural life) but the metrics indicate a significant extractive component due to the contested context.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Cultural Zionists, the constraint was a pure 'rope' for coordinating Jewish cultural revival. From the perspective of Palestinian Arabs, the same constraint operated as a 'snare' or 'tangled_rope,' as it facilitated a growing Jewish presence that implicitly undermined their own national aspirations and led to tangible losses, despite the lack of explicit political demands.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish cultural institutions, intellectuals, and immigrants are the primary beneficiaries, gaining a spiritual and cultural home and a platform for national revival. Palestinian Arabs are the payers/victims, experiencing implicit displacement, marginalization, and competition for resources and land, even in the absence of explicit political subjugation. Ottoman/British authorities acted as agenda-setters, shaping the conditions of settlement. International cultural organizations are observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_claim_vs_political_impact,
    'Is the cultural claim of Cultural Zionism genuinely separable from its political and demographic impacts in a contested territory like Palestine?',
    'Historical analysis of the actual consequences of cultural settlement on local Palestinian populations and subsequent political developments, comparing stated intent with observed outcomes.',
    'If inseparable, the ''rope'' classification for this reading is misleading, and its effective extraction and suppression are higher than its ideal form suggests, pushing it towards ''tangled_rope'' or ''snare'' from the Palestinian perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cultural_claim_vs_political_impact, empirical, 'Whether cultural presence inevitably leads to political claims and displacement in a contested land.').

omega_variable(
    binational_potential_vs_demographic_reality,
    'Could a genuine binational framework, as sometimes envisioned by Cultural Zionists, have emerged, or did any Jewish presence inevitably push towards a Jewish majority and political sovereignty?',
    'Counterfactual historical analysis exploring alternative policy choices and their likely outcomes, or comparative studies of similar ethno-nationalist movements in contested territories.',
    'If binationalism was structurally foreclosed by the dynamics of settlement, the reading''s non-majoritarian axiom is overridden by historical practice, increasing its effective extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binational_potential_vs_demographic_reality, conceptual, 'The feasibility of a non-majoritarian Jewish national home in Palestine.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (e.g., British Mandate policies favoring Jewish settlement) or internalized (e.g., Palestinian communities feeling powerless against a growing, internationally supported movement)?',
    'Sociological and historical studies examining the agency and resistance strategies of Palestinian communities, and the specific mechanisms of control employed by authorities and Zionist institutions.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them, making exit or effective resistance more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for Palestinian Arabs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__cultural_zionism_reading, 1880, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jewi_tr_t10, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(jewi_tr_t20, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(jewi_tr_t30, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(jewi_tr_t40, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(jewi_tr_t50, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement(jewi_tr_t60, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 60, 0.15).
narrative_ontology:measurement(jewi_tr_t68, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 68, 0.15).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(jewi_be_t10, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(jewi_be_t20, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(jewi_be_t30, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(jewi_be_t40, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(jewi_be_t50, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement(jewi_be_t60, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(jewi_be_t68, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 68, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(jewi_su_t10, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(jewi_su_t20, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(jewi_su_t30, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(jewi_su_t40, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(jewi_su_t50, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement(jewi_su_t60, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(jewi_su_t68, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 68, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__cultural_zionism_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'Jewish territorial claim' kernel, focusing on cultural and spiritual development. It is distinct from political, labor, and revisionist Zionist readings, which emphasize statehood, socialist transformation, and maximalist territorial claims, respectively.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
