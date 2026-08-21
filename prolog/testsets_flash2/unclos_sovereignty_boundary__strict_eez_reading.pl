% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__strict_eez_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__strict_eez_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__strict_eez_reading
 *   human_readable: UNCLOS Strict EEZ Boundary (200-Nautical-Mile Exclusive Economic Zone)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint represents the 'strict EEZ' reading of UNCLOS, where
 *   coastal states have exclusive and enforceable rights within 200 nautical
 *   miles, and no overlaying claims (e.g., historical rights, freedom of
 *   navigation for non-ratifiers) are considered valid. It is a tangled rope
 *   because it provides a coordination function (clear jurisdiction) but also
 *   involves significant extraction from those excluded from these zones,
 *   requiring active enforcement. The metrics reflect the increasing
 *   assertiveness of coastal states in enforcing these boundaries and the
 *   corresponding resistance from those whose access is curtailed.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, 0.65).
domain_priors:suppression_score(unclos_sovereignty_boundary__strict_eez_reading, 0.75).
domain_priors:theater_ratio(unclos_sovereignty_boundary__strict_eez_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__strict_eez_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__strict_eez_reading, "UNCLOS Strict EEZ Boundary (200-Nautical-Mile Exclusive Economic Zone)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__strict_eez_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__strict_eez_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__strict_eez_reading, '7781f827-8c38-4875-96e5-49cdf33a585a').
narrative_ontology:cs_kernel_codification('7781f827-8c38-4875-96e5-49cdf33a585a', fixed_text).
narrative_ontology:cs_authority_grounding('7781f827-8c38-4875-96e5-49cdf33a585a', lineage).
narrative_ontology:cs_interpretation_layer_present('7781f827-8c38-4875-96e5-49cdf33a585a').
narrative_ontology:cs_reading_relation('7781f827-8c38-4875-96e5-49cdf33a585a', unclos_sovereignty_boundary__historical_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('7781f827-8c38-4875-96e5-49cdf33a585a', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coexists_with).
narrative_ontology:cs_axiom('7781f827-8c38-4875-96e5-49cdf33a585a', foundational, unclos_article_57_primacy).
narrative_ontology:cs_axiom_status(unclos_article_57_primacy, holdable).
narrative_ontology:cs_axiom_grounding('7781f827-8c38-4875-96e5-49cdf33a585a', unclos_article_57_primacy, conventional).
narrative_ontology:cs_axiom('7781f827-8c38-4875-96e5-49cdf33a585a', foundational, exclusive_resource_sovereignty).
narrative_ontology:cs_axiom_status(exclusive_resource_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('7781f827-8c38-4875-96e5-49cdf33a585a', exclusive_resource_sovereignty, conventional).
narrative_ontology:cs_reference_frame('7781f827-8c38-4875-96e5-49cdf33a585a', unclos_ratification_era).
narrative_ontology:cs_drift_state('7781f827-8c38-4875-96e5-49cdf33a585a', contemporary_geopolitical_contestation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7781f827-8c38-4875-96e5-49cdf33a585a', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, overlapping_claimants).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_fleets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claim and enforce exclusive rights over resources within their 200-nautical-mile EEZ, as defined by UNCLOS. They benefit from resource control and jurisdiction, but are constrained by the need for international recognition and the costs of enforcement.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, coastal_states, agenda_setter,
    institutional, generational, constrained, national).

% States whose historical claims or geographic proximity lead to overlapping EEZ claims with other coastal states. They bear the cost of lost access or contested jurisdiction, often resorting to diplomatic or military means to assert their interpretations.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, overlapping_claimants, payer,
    powerful, generational, constrained, regional).

% Commercial fishing operations that historically operated in areas now designated as EEZs. They face exclusion or require costly licensing from coastal states, impacting their economic viability and access to traditional fishing grounds.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_fleets, payer,
    organized, biographical, constrained, global).

% Adjudicate disputes related to UNCLOS, including EEZ boundaries. Their rulings provide legal clarity but depend on the willingness of states to submit to and abide by their jurisdiction.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, international_maritime_tribunals, observer,
    institutional, generational, analytical, global).

% States that have not ratified UNCLOS but operate under customary international law. They are excluded from the formal UNCLOS dispute resolution mechanisms and their claims regarding EEZs are often challenged by ratifying states.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, non_ratifier_states, excluded,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, internationally recognized framework for maritime jurisdiction and resource management, reducing potential conflicts over ocean resources by assigning exclusive rights to coastal states.
% TRANSFER_FUNCTION: Transfers exclusive control over marine resources (fish, oil, gas) and scientific research rights from a global commons regime to individual coastal states, along with the responsibility for management and enforcement.
% ABSENT_VOICES: States with historical claims to fishing or navigation rights in areas now designated as EEZs, and non-ratifier states asserting alternative interpretations of maritime law, are often marginalized in UNCLOS-centric discussions. They would argue for greater recognition of pre-existing or customary rights.
% DISAPPEARANCE_RATIONALE: If the strict EEZ framework vanished, there would be immediate and widespread disputes over maritime resources, increased naval confrontations, and a collapse of coordinated ocean governance, leading to a scramble for resources and potential overexploitation.
% FOUNDING_PROBLEM: Unregulated exploitation of ocean resources, overlapping claims, and a lack of clear jurisdiction beyond territorial waters led to international disputes and unsustainable practices.
% FOUNDING_PROBLEM_CORROBORATION: Coastal states and international organizations attest that the problem of resource management and jurisdictional clarity remains live, citing ongoing challenges with illegal fishing and maritime boundary disputes. Independent legal scholars and environmental groups corroborate the need for a clear framework to prevent chaos and ensure sustainability.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__strict_eez_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__strict_eez_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__strict_eez_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unclos_sovereignty_boundary__strict_eez_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__strict_eez_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because coastal states gain exclusive control over vast resources, often at the expense of traditional users or other claimants. Suppression is also high, as enforcement (naval patrols, legal challenges) is actively used to deter competing claims and activities. Theater ratio is low, as the enforcement is generally functional, not merely performative. Accessibility collapse is moderate-high, as alternative frameworks for maritime claims are largely suppressed by the UNCLOS regime. Resistance is moderate, coming from states with overlapping claims or those asserting historical rights.
 *
 * PERSPECTIVAL GAP:
 *   Coastal states perceive this as a legitimate and necessary coordination mechanism for resource management. Overlapping claimants and distant-water fishing fleets view it as an extractive mechanism that dispossesses them of traditional access or historical rights. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal states are clear beneficiaries, gaining exclusive resource control. Overlapping claimants and distant-water fishing fleets are victims, losing access and bearing costs. International tribunals act as observers, while non-ratifier states are excluded from the formal UNCLOS framework, facing challenges to their alternative interpretations.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_rights_validity,
    'To what extent do historical usage and occupation create sovereign rights that genuinely predate and override UNCLOS EEZ provisions?',
    'International legal precedent from non-UNCLOS-specific tribunals, or a shift in customary international law recognized by a majority of states.',
    'If historical rights are deemed to override UNCLOS, the extractiveness from overlapping claimants would decrease, and the suppression of their claims would be reclassified as illegitimate, potentially shifting this constraint towards a snare for coastal states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_rights_validity, conceptual, 'Ambiguity regarding the precedence of historical claims versus UNCLOS provisions.').

omega_variable(
    non_ratifier_freedom_of_navigation,
    'Are freedom of navigation principles customary international law, enforceable by naval presence, independent of UNCLOS ratification, and do they permit activities within EEZs that UNCLOS would otherwise restrict?',
    'A definitive ruling by the International Court of Justice on the scope of customary international law for non-ratifiers within EEZs, or a widely accepted state practice that clarifies these rights.',
    'If non-ratifier freedom of navigation is affirmed to permit activities restricted by UNCLOS, the suppression metric for non-ratifier states would decrease, and the EEZ''s exclusivity would be weakened, potentially reclassifying it as a less extractive tangled rope or even a rope for some activities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_ratifier_freedom_of_navigation, empirical, 'Ambiguity regarding the rights of non-UNCLOS ratifiers within EEZs.').

omega_variable(
    eez_enforcement_sustainability,
    'Is the current level of active enforcement by coastal states sustainable in the long term, given the costs and potential for conflict?',
    'Longitudinal study of enforcement budgets, naval deployments, and incident rates, coupled with geopolitical analysis of regional stability.',
    'If enforcement proves unsustainable, the suppression metric would decline, potentially leading to increased resistance and a reclassification towards a piton if the benefits of enforcement no longer outweigh the costs for coastal states.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(eez_enforcement_sustainability, empirical, 'Sustainability of active enforcement of EEZ boundaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__strict_eez_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(uncl_tr_t10, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(uncl_tr_t20, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(uncl_tr_t30, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(uncl_tr_t40, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 40, 0.1).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(uncl_be_t10, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(uncl_be_t20, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(uncl_be_t30, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(uncl_be_t40, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(uncl_su_t10, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(uncl_su_t20, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(uncl_su_t30, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(uncl_su_t40, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__strict_eez_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'unclos_sovereignty_boundary' kernel, focusing on the strict interpretation of EEZ limits. It is linked to other readings of the same kernel, which represent alternative interpretations of maritime sovereignty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
