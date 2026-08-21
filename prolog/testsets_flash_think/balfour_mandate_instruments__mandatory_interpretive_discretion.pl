% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__mandatory_interpretive_discretion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__mandatory_interpretive_discretion, []).

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
 *   constraint_id: balfour_mandate_instruments__mandatory_interpretive_discretion
 *   human_readable: British Mandatory Interpretive Discretion
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This constraint describes the operational system of British mandatory
 *   interpretive discretion over the Balfour Declaration and the League of
 *   Nations Mandate for Palestine. It is a reading of the
 *   'balfour_mandate_instruments' kernel, focusing on how British power to
 *   adjudicate between competing interpretations without external review
 *   constituted a constraint. This discretion created strategic uncertainty
 *   and path-dependent lock-in for both Arab and Zionist communities, as
 *   British policy oscillated (e.g., land regimes 1920 vs 1940; White Papers
 *   1922/1930/1939). The constraint is claimed as a Rope by the British
 *   (coordination of complex claims) but operates as a Snare, extracting
 *   political agency and strategic advantage for the Mandatory power.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.65).
domain_priors:suppression_score(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.75).
domain_priors:theater_ratio(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, extractiveness, 0.65).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__mandatory_interpretive_discretion, snare).
narrative_ontology:human_readable(balfour_mandate_instruments__mandatory_interpretive_discretion, "British Mandatory Interpretive Discretion").
narrative_ontology:topic_domain(balfour_mandate_instruments__mandatory_interpretive_discretion, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__mandatory_interpretive_discretion).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__mandatory_interpretive_discretion, 'dc2e6ca2-a57d-4fc7-9128-3846726b3692').
narrative_ontology:cs_kernel_codification('dc2e6ca2-a57d-4fc7-9128-3846726b3692', fixed_text).
narrative_ontology:cs_authority_grounding('dc2e6ca2-a57d-4fc7-9128-3846726b3692', extraction).
narrative_ontology:cs_interpretation_layer_present('dc2e6ca2-a57d-4fc7-9128-3846726b3692').
narrative_ontology:cs_reading_relation('dc2e6ca2-a57d-4fc7-9128-3846726b3692', balfour_mandate_instruments__jewish_national_home_primacy, coexists_with).
narrative_ontology:cs_reading_relation('dc2e6ca2-a57d-4fc7-9128-3846726b3692', balfour_mandate_instruments__dual_obligation_indigenous_rights, coexists_with).
narrative_ontology:cs_axiom('dc2e6ca2-a57d-4fc7-9128-3846726b3692', foundational, unilateral_interpretive_sovereignty).
narrative_ontology:cs_axiom_status(unilateral_interpretive_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('dc2e6ca2-a57d-4fc7-9128-3846726b3692', unilateral_interpretive_sovereignty, conventional).
narrative_ontology:cs_reference_frame('dc2e6ca2-a57d-4fc7-9128-3846726b3692', unfettered_mandatory_prerogative).
narrative_ontology:cs_drift_state('dc2e6ca2-a57d-4fc7-9128-3846726b3692', post_world_war_ii_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dc2e6ca2-a57d-4fc7-9128-3846726b3692', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_community).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The British Mandatory power, which held the authority to interpret the Mandate instruments without external review. They benefited from policy flexibility, allowing them to manage competing claims and maintain strategic control through a 'divide-and-rule' approach. Their discretion allowed for oscillations in policy (e.g., land regimes, White Papers) that served British interests.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators, agenda_setter,
    institutional, generational, arbitrage, global).

% The indigenous Arab population of Palestine, whose civil and political rights were theoretically protected by the Mandate but were subject to British interpretive discretion. They faced strategic uncertainty and path-dependent lock-in, unable to appeal to fixed textual meaning or external arbitration, often bearing the costs of policies favoring Zionist immigration and land acquisition.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_community, payer,
    powerless, generational, trapped, regional).

% The Jewish community in Palestine and the broader Zionist movement, which sought to establish a Jewish national home. While often benefiting from British policies that facilitated immigration and land purchases, they also faced strategic uncertainty due to British interpretive oscillations and were unable to secure a definitive commitment to their maximalist claims, making them a target of the discretion's ambiguity.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_community, payer,
    moderate, generational, constrained, regional).

% The League of Nations and later the nascent United Nations, which established the mandate system but lacked effective mechanisms to review or constrain British interpretive discretion. They observed the unfolding situation and eventually sought to resolve it through partition, but could not directly challenge the British authority during the mandate period.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, international_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__mandatory_interpretive_discretion, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The mandate system, and British discretion within it, was intended to coordinate the transition of Palestine towards self-governance, balancing the Balfour Declaration's promise of a Jewish national home with the rights of existing non-Jewish communities, under international oversight.
% TRANSFER_FUNCTION: Transfers political agency, self-determination, and control over land and immigration policies from the local communities to the British Mandatory power, which then allocates these resources based on its unilateral interpretations.
% ABSENT_VOICES: Independent international arbitration bodies or a truly empowered League of Nations oversight mechanism were absent. Had they been present, they would have challenged the unilateral nature of British interpretive authority and pushed for more consistent application of international law and self-determination principles.
% DISAPPEARANCE_RATIONALE: The disappearance of British mandatory interpretive discretion led directly to the end of the Mandate in 1948, the immediate outbreak of the Arab-Israeli War, and the subsequent establishment of the State of Israel. The underlying conflicts, previously managed (or exacerbated) by British discretion, erupted, fundamentally reorganizing the political landscape of the region.
% FOUNDING_PROBLEM: To manage the post-Ottoman disposition of Palestine, balancing the promise of a Jewish national home with the civil and religious rights of existing non-Jewish communities, under the League of Nations mandate system, in a region of significant geopolitical importance.
% FOUNDING_PROBLEM_CORROBORATION: Historians and international legal scholars widely attest that the founding problem of balancing competing claims under a temporary mandate was superseded by British strategic interests and the eventual collapse of the mandate system. The problem was ultimately 'solved' by partition and war, not by the successful exercise of British discretion. Legislative hearings and independent analyses from outside the benefiting parties support this shifted-function reading.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__mandatory_interpretive_discretion, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__mandatory_interpretive_discretion, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(balfour_mandate_instruments__mandatory_interpretive_discretion, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.65) because British discretion consistently prioritized imperial strategic interests over a balanced implementation of the Mandate's dual obligations, leading to asymmetric outcomes. Suppression is high (0.75) as both communities were structurally unable to appeal British interpretations or exit the mandate framework, with British military and administrative power actively enforcing these interpretations. The theater ratio is low (0.20) because the interpretive discretion was actively and functionally used to shape policy, rather than being a mere performance. The increasing extractiveness and suppression over the interval reflect the growing divergence between the stated goals of the Mandate and its operational reality.
 *
 * PERSPECTIVAL GAP:
 *   From the British perspective, their interpretive discretion was a necessary 'Rope' to navigate complex, competing claims and maintain order in a volatile region. From the perspective of both Arab and Zionist communities, this same discretion functioned as a 'Snare,' trapping them in a system where their fundamental rights and aspirations were subject to an unaccountable external authority, leading to substantial extraction of political agency and resources.
 *
 * DIRECTIONALITY LOGIC:
 *   The British colonial administrators are the primary beneficiaries (low directionality), gaining policy flexibility and strategic control. Both the Arab and Zionist communities are targets (high directionality), as their aspirations and rights were subject to the Mandatory power's unilateral interpretation, leading to uncertainty and imposed outcomes. The international community acts as an observer, with limited direct influence on the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_reading,
    'Is this constraint a genuine structural feature of British mandatory power, or is it merely one reading of the ''balfour_mandate_instruments'' kernel?',
    'Analysis of alternative readings of the Mandate instruments (e.g., ''jewish_national_home_primacy'', ''dual_obligation_indigenous_rights'') and their structural implications, to determine if British discretion is an emergent property or a chosen interpretation.',
    'If it is merely a chosen interpretation, the constraint''s extractiveness is amplified by the suppression of alternative, less extractive readings. If it is an emergent structural feature, its extractiveness is inherent to the mandate system itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_reading, conceptual, 'This constraint is one reading of the ''balfour_mandate_instruments'' kernel, focusing on British interpretive discretion.').

omega_variable(
    sibling_impact_jewish_national_home_primacy,
    'How would the constraint''s classification change if the ''jewish_national_home_primacy'' reading of the Mandate instruments were structurally dominant?',
    'Counterfactual analysis: if British discretion were consistently constrained to facilitate Jewish state-building, its ''arbitrage'' aspect would be reduced, potentially shifting its classification for the Zionist community towards a beneficiary, while intensifying extraction for the Arab community.',
    'The constraint''s effective extractiveness for the Zionist community would decrease, while for the Arab community it would likely increase, potentially reclassifying the constraint as a more direct Snare for the latter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_impact_jewish_national_home_primacy, conceptual, 'Impact of the ''jewish_national_home_primacy'' sibling reading on this constraint.').

omega_variable(
    sibling_impact_dual_obligation_indigenous_rights,
    'How would the constraint''s classification change if the ''dual_obligation_indigenous_rights'' reading were structurally dominant?',
    'Counterfactual analysis: if British discretion were consistently constrained to protect Arab civil and political rights, its ''divide-and-rule'' aspect would be reduced, potentially shifting its classification for the Arab community towards a beneficiary, while intensifying extraction for the Zionist community.',
    'The constraint''s effective extractiveness for the Arab community would decrease, while for the Zionist community it would likely increase, potentially reclassifying the constraint as a more direct Snare for the latter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_impact_dual_obligation_indigenous_rights, conceptual, 'Impact of the ''dual_obligation_indigenous_rights'' sibling reading on this constraint.').

omega_variable(
    disagreement_location_interpretive_power,
    'Where is the fundamental disagreement located regarding British interpretive power?',
    'Analysis of historical legal arguments and diplomatic exchanges concerning the scope and limits of British authority under the Mandate, focusing on whether the authority was considered absolute or subject to external review.',
    'The disagreement is located in the ''authority_grounding'' and ''axioms'' concerning the unilateral nature of British interpretive sovereignty. Resolving this would clarify whether the constraint''s extractiveness was a deviation from its mandate or inherent to its structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location_interpretive_power, conceptual, 'The disagreement is located in the scope and limits of British interpretive power, specifically its unilateral nature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__mandatory_interpretive_discretion, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t0, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 0, 0.18).
narrative_ontology:measurement(balf_tr_t5, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 5, 0.19).
narrative_ontology:measurement(balf_tr_t10, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 10, 0.2).
narrative_ontology:measurement(balf_tr_t15, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 15, 0.2).
narrative_ontology:measurement(balf_tr_t20, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 20, 0.2).
narrative_ontology:measurement(balf_tr_t28, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 28, 0.2).

% Extraction over time
narrative_ontology:measurement(balf_be_t0, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(balf_be_t5, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(balf_be_t10, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(balf_be_t15, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(balf_be_t20, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(balf_be_t28, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 28, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t0, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(balf_su_t5, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(balf_su_t10, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(balf_su_t15, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(balf_su_t20, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(balf_su_t28, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 28, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__mandatory_interpretive_discretion, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
