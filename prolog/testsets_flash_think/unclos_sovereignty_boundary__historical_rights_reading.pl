% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__historical_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__historical_rights_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: unclos_sovereignty_boundary__historical_rights_reading
 *   human_readable: Historical Usage and Occupation Override UNCLOS EEZ
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint represents the claim that historical usage and occupation
 *   create sovereign rights that predate and override the Exclusive Economic
 *   Zone (EEZ) provisions of the United Nations Convention on the Law of the
 *   Sea (UNCLOS). This reading is primarily advanced by states seeking to
 *   expand their maritime control beyond UNCLOS limits, often in
 *   resource-rich or strategically vital areas. The claim is presented as a
 *   matter of historical justice and continuity, but its operational effect
 *   is to extract control and resources from other coastal states and to
 *   suppress international navigation rights.
 *
 * KEY AGENTS:
 *   - historical_claimant_states: Agenda setter, beneficiary (institutional/arbitrage)
 *   - eez_coastal_states: Payer, victim (institutional/constrained)
 *   - international_shipping_companies: Payer, victim (organized/constrained)
 *   - fishing_fleets: Payer, victim (organized/constrained)
 *   - unclos_signatories: Observer (institutional/analytical)
 *   - international_arbitration_bodies: Observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, 0.85).
domain_priors:suppression_score(unclos_sovereignty_boundary__historical_rights_reading, 0.9).
domain_priors:theater_ratio(unclos_sovereignty_boundary__historical_rights_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__historical_rights_reading, snare).
narrative_ontology:human_readable(unclos_sovereignty_boundary__historical_rights_reading, "Historical Usage and Occupation Override UNCLOS EEZ").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__historical_rights_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__historical_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__historical_rights_reading, '0d6d27b0-f153-4c22-86df-5f5b737e9f80').
narrative_ontology:cs_kernel_codification('0d6d27b0-f153-4c22-86df-5f5b737e9f80', formalized).
narrative_ontology:cs_authority_grounding('0d6d27b0-f153-4c22-86df-5f5b737e9f80', lineage).
narrative_ontology:cs_interpretation_layer_present('0d6d27b0-f153-4c22-86df-5f5b737e9f80').
narrative_ontology:cs_reading_relation('0d6d27b0-f153-4c22-86df-5f5b737e9f80', unclos_sovereignty_boundary__strict_eez_reading, forecloses).
narrative_ontology:cs_reading_relation('0d6d27b0-f153-4c22-86df-5f5b737e9f80', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coexists_with).
narrative_ontology:cs_axiom('0d6d27b0-f153-4c22-86df-5f5b737e9f80', foundational, historical_possession_creates_sovereignty).
narrative_ontology:cs_axiom_status(historical_possession_creates_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('0d6d27b0-f153-4c22-86df-5f5b737e9f80', historical_possession_creates_sovereignty, conventional).
narrative_ontology:cs_axiom('0d6d27b0-f153-4c22-86df-5f5b737e9f80', foundational, unclos_subordinate_to_prior_rights).
narrative_ontology:cs_axiom_status(unclos_subordinate_to_prior_rights, holdable).
narrative_ontology:cs_axiom_grounding('0d6d27b0-f153-4c22-86df-5f5b737e9f80', unclos_subordinate_to_prior_rights, conventional).
narrative_ontology:cs_reference_frame('0d6d27b0-f153-4c22-86df-5f5b737e9f80', pre_unclos_maritime_order).
narrative_ontology:cs_drift_state('0d6d27b0-f153-4c22-86df-5f5b737e9f80', contemporary_unclos_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('0d6d27b0-f153-4c22-86df-5f5b737e9f80', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, historical_claimant_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, eez_coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, international_shipping_companies).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, fishing_fleets).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__historical_rights_reading, historical_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__historical_rights_reading, uti_possidetis_juris_maritime).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States that assert historical rights and claims over maritime areas, often overlapping with other states' UNCLOS-defined Exclusive Economic Zones (EEZs). They actively enforce these claims through naval presence, administrative acts, and diplomatic pressure, gaining control over resources and strategic advantage.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, historical_claimant_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Coastal states whose UNCLOS-derived EEZ rights are challenged or encroached upon by historical claims. They face loss of exclusive resource access, increased security risks, and diplomatic pressure. Their options are to protest, litigate, or seek international arbitration, often at significant cost.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, eez_coastal_states, payer,
    institutional, generational, constrained, regional).

% Commercial entities operating vessels through contested maritime areas. They face increased risks of harassment, detention, or rerouting, leading to higher operational costs and insurance premiums. Their options are limited by the need to use established shipping lanes.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, international_shipping_companies, payer,
    organized, immediate, constrained, global).

% Commercial fishing operations that traditionally operate in areas now claimed by historical rights. They face exclusion from fishing grounds, harassment, and confiscation of vessels or catch. Their livelihoods are directly threatened, with few alternative fishing areas.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, fishing_fleets, payer,
    moderate, biographical, constrained, regional).

% The vast majority of states that have ratified UNCLOS and uphold its provisions as the primary framework for maritime governance. They observe the erosion of the UNCLOS regime by historical claims and often issue diplomatic protests or support legal challenges.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, unclos_signatories, observer,
    institutional, civilizational, analytical, global).

% Legal and judicial institutions (e.g., ITLOS, PCA) tasked with adjudicating maritime disputes. They analyze claims based on international law, including UNCLOS and customary law, but their jurisdiction and enforcement power can be challenged by states asserting historical rights.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, international_arbitration_bodies, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to provide a stable, historically-grounded framework for maritime boundaries, reducing ambiguity by appealing to long-standing practice and traditional rights in contested areas.
% TRANSFER_FUNCTION: Transfers control over maritime zones, their resources (e.g., fisheries, hydrocarbons), and strategic advantage from states adhering strictly to UNCLOS EEZ limits to states asserting historical rights.
% ABSENT_VOICES: International maritime law scholars would argue for the primacy of UNCLOS and the limited scope of historical claims. Environmental protection advocates would argue for unified, science-based governance of shared seas, unhindered by contested sovereignty claims.
% DISAPPEARANCE_RATIONALE: If historical claims vanished overnight, UNCLOS EEZ provisions would become the undisputed framework for maritime boundaries. This would lead to a significant rearrangement of maritime control, resource access, and geopolitical influence, particularly in regions like the South China Sea, with claimant states losing vast areas of asserted control.
% FOUNDING_PROBLEM: To resolve overlapping or ambiguous maritime claims based on historical presence and usage, particularly in areas where modern delimitations (like those under UNCLOS) are contested or seen as insufficient by certain states.
% FOUNDING_PROBLEM_CORROBORATION: The historical claimant states themselves attest that the problem of reconciling historical rights with modern legal frameworks is still live, citing historical maps, ancient texts, and traditional fishing practices. However, international legal scholars and the majority of UNCLOS signatories largely dispute the validity of these claims overriding UNCLOS, viewing the 'problem' as a pretext for expansionism.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__historical_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__historical_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__historical_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(unclos_sovereignty_boundary__historical_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__historical_rights_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very high (0.85) because this claim allows claimant states to assert control over vast maritime areas and their resources, which would otherwise fall under the EEZ of other states or be part of the high seas. Suppression is also very high (0.90) as these claims are actively enforced through naval patrols, harassment, and diplomatic pressure, directly suppressing the UNCLOS-derived rights of other states and the freedom of navigation. The theater ratio is moderate (0.40); while there's a genuine historical narrative invoked, a significant portion of the activity is performative assertion of sovereignty to justify current expansionism. Resistance is high (0.88) due to strong opposition from other coastal states, international bodies, and freedom of navigation operations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of historical claimant states, this constraint is a legitimate assertion of long-standing rights, providing stability. From the perspective of EEZ coastal states and international maritime actors, it is an extractive and suppressive mechanism that destabilizes the international legal order established by UNCLOS. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Historical claimant states are clear beneficiaries and agenda-setters, gaining control and resources. EEZ coastal states, international shipping companies, and fishing fleets are direct targets and payers, losing access, facing restrictions, and bearing increased costs. UNCLOS signatories and international arbitration bodies act as observers, attempting to uphold the international legal framework against these claims.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_evidence_validity,
    'Is the historical evidence presented by claimant states genuinely compelling under international law, or is it selectively interpreted to justify contemporary expansionism?',
    'Independent, multi-disciplinary historical and legal review by an internationally recognized body, assessing the continuity, effectiveness, and acquiescence elements of customary international law.',
    'If the evidence is found to be weak or selectively interpreted, the legitimacy of the historical claims would collapse, strengthening the UNCLOS framework and reclassifying the constraint as a pure Snare. If found compelling, it would lend some legitimacy to the coordination aspect, potentially shifting it towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_evidence_validity, empirical, 'Assesses the factual and legal basis of historical claims.').

omega_variable(
    customary_law_vs_treaty_law_hierarchy,
    'To what extent do these historical claims, even if recognized as customary international law, genuinely override or coexist with the specific treaty provisions of UNCLOS?',
    'Adjudication by the International Court of Justice or a similar body, specifically ruling on the hierarchy and interplay between established customary international law and subsequent multilateral treaties like UNCLOS.',
    'If UNCLOS is deemed to supersede prior customary claims in this context, the historical claims would be legally nullified, exposing the constraint as pure extraction. If customary law is found to hold precedence, it would legitimize the claims, reducing the perceived extractiveness from the claimant''s perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(customary_law_vs_treaty_law_hierarchy, conceptual, 'Clarifies the legal hierarchy between historical customary claims and UNCLOS.').

omega_variable(
    coordination_extraction_boundary,
    'Is the assertion of historical rights a genuine attempt to coordinate stable maritime boundaries based on tradition, or is it primarily a cover for resource extraction and geopolitical control?',
    'Analysis of claimant state behavior: if claims are consistently accompanied by resource exploitation and military buildup without genuine negotiation or dispute resolution, it points to extraction. If claims are primarily defensive and lead to stable, negotiated outcomes, it points to coordination.',
    'If primarily extraction, the Snare classification is reinforced. If a genuine, albeit contested, coordination function is identified, the constraint might shift towards a Tangled Rope, acknowledging a dual function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Distinguishes genuine coordination from extractive cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__historical_rights_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 1982, 0.2).
narrative_ontology:measurement(uncl_tr_t1992, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 1992, 0.28).
narrative_ontology:measurement(uncl_tr_t2002, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2002, 0.33).
narrative_ontology:measurement(uncl_tr_t2012, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2012, 0.37).
narrative_ontology:measurement(uncl_tr_t2024, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 1982, 0.6).
narrative_ontology:measurement(uncl_be_t1992, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 1992, 0.7).
narrative_ontology:measurement(uncl_be_t2002, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2002, 0.78).
narrative_ontology:measurement(uncl_be_t2012, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2012, 0.82).
narrative_ontology:measurement(uncl_be_t2024, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 1982, 0.65).
narrative_ontology:measurement(uncl_su_t1992, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 1992, 0.75).
narrative_ontology:measurement(uncl_su_t2002, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2002, 0.82).
narrative_ontology:measurement(uncl_su_t2012, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2012, 0.87).
narrative_ontology:measurement(uncl_su_t2024, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__historical_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, freedom_of_navigation_operations).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'unclos_sovereignty_boundary' kernel. This 'historical_rights_reading' asserts that historical usage and occupation create sovereign rights that predate and override UNCLOS EEZ provisions, contrasting with the 'strict_eez_reading' and 'non_ratifier_enforcement_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
