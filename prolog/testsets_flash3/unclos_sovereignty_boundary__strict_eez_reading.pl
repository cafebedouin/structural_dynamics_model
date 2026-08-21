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
 *   constraint_id: unclos_sovereignty_boundary__strict_eez_reading
 *   human_readable: UNCLOS Strict EEZ Boundary (200-Nautical-Mile Exclusive Economic Zone)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint represents the 'strict EEZ' reading of UNCLOS, where
 *   coastal states have exclusive and enforceable rights within 200 nautical
 *   miles, and no overlaying claims (e.g., historical rights, freedom of
 *   navigation for non-ratifiers) are considered valid. It is claimed as a
 *   tangled rope because it provides a coordination function for resource
 *   management but involves significant extraction from those excluded from
 *   EEZs, requiring active enforcement. The metrics reflect the increasing
 *   assertiveness of coastal states in enforcing these boundaries and the
 *   corresponding suppression of alternative claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, 0.65).
domain_priors:suppression_score(unclos_sovereignty_boundary__strict_eez_reading, 0.78).
domain_priors:theater_ratio(unclos_sovereignty_boundary__strict_eez_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__strict_eez_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__strict_eez_reading, "UNCLOS Strict EEZ Boundary (200-Nautical-Mile Exclusive Economic Zone)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__strict_eez_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__strict_eez_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__strict_eez_reading, '002c3a56-9449-4778-b59c-c035f685d633').
narrative_ontology:cs_kernel_codification('002c3a56-9449-4778-b59c-c035f685d633', fixed_text).
narrative_ontology:cs_authority_grounding('002c3a56-9449-4778-b59c-c035f685d633', lineage).
narrative_ontology:cs_interpretation_layer_present('002c3a56-9449-4778-b59c-c035f685d633').
narrative_ontology:cs_reading_relation('002c3a56-9449-4778-b59c-c035f685d633', unclos_sovereignty_boundary__historical_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('002c3a56-9449-4778-b59c-c035f685d633', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coexists_with).
narrative_ontology:cs_axiom('002c3a56-9449-4778-b59c-c035f685d633', foundational, unclos_article_57_primacy).
narrative_ontology:cs_axiom_status(unclos_article_57_primacy, holdable).
narrative_ontology:cs_axiom_grounding('002c3a56-9449-4778-b59c-c035f685d633', unclos_article_57_primacy, conventional).
narrative_ontology:cs_axiom('002c3a56-9449-4778-b59c-c035f685d633', foundational, exclusive_resource_sovereignty).
narrative_ontology:cs_axiom_status(exclusive_resource_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('002c3a56-9449-4778-b59c-c035f685d633', exclusive_resource_sovereignty, conventional).
narrative_ontology:cs_reference_frame('002c3a56-9449-4778-b59c-c035f685d633', unclos_ratification_era_consensus).
narrative_ontology:cs_drift_state('002c3a56-9449-4778-b59c-c035f685d633', contemporary_geopolitical_contestation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('002c3a56-9449-4778-b59c-c035f685d633', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_fleets).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, overlapping_claimants).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, non_ratifier_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claim and enforce exclusive rights to resources within their 200-nautical-mile EEZ, deriving legitimacy from UNCLOS. They benefit from resource control and reduced competition, but are constrained by the need for international recognition and enforcement capacity.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, coastal_states, agenda_setter,
    institutional, generational, constrained, national).

% Historically fished in areas now designated as EEZs. They face exclusion or require costly licensing from coastal states, impacting their economic viability. Their options are to comply, seek new fishing grounds, or risk confrontation.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_fleets, payer,
    organized, immediate, constrained, global).

% States with historical or geographical claims that overlap with another state's UNCLOS-derived EEZ. They lose access to resources and strategic space, often leading to diplomatic disputes or military standoffs. Exit is not an option without ceding sovereignty.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, overlapping_claimants, payer,
    powerful, generational, trapped, regional).

% States that have not ratified UNCLOS but are affected by its provisions. They may assert freedom of navigation or historical rights, but face international pressure and potential enforcement actions from ratifying coastal states. Their position is legally ambiguous and often contested.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, non_ratifier_states, payer,
    institutional, generational, constrained, global).

% Adjudicate disputes related to UNCLOS, including EEZ boundaries and rights. They interpret the convention and issue rulings, influencing the enforcement and legitimacy of claims. Their role is to apply the strict EEZ reading in cases brought before them.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, international_maritime_tribunals, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, internationally recognized framework for maritime resource management and jurisdiction, reducing potential conflicts over ocean resources by assigning exclusive rights to coastal states.
% TRANSFER_FUNCTION: Transfers exclusive control over marine living and non-living resources (e.g., fish stocks, oil, gas) within 200 nautical miles from a global commons regime to individual coastal states.
% ABSENT_VOICES: Indigenous communities with traditional fishing rights extending beyond current territorial waters, and future generations whose access to shared ocean resources is diminished by current exclusive claims, are largely absent from the UNCLOS negotiation and enforcement framework.
% DISAPPEARANCE_RATIONALE: If the strict EEZ framework vanished, there would be immediate and widespread disputes over maritime resources, leading to increased naval confrontations, unregulated fishing, and a collapse of current ocean governance structures. The global maritime order would fundamentally reorganize.
% FOUNDING_PROBLEM: Unregulated exploitation of ocean resources, overlapping claims, and a lack of clear jurisdiction over vast ocean areas led to resource depletion and international disputes in the mid-20th century.
% FOUNDING_PROBLEM_CORROBORATION: Coastal states and international organizations corroborate that the problem of resource management and jurisdictional clarity remains live, citing ongoing challenges with illegal fishing and maritime boundary disputes, which UNCLOS aims to mitigate. Independent environmental groups also attest to the continued need for clear resource management to prevent overexploitation.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__strict_eez_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__strict_eez_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__strict_eez_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high because coastal states gain exclusive control over vast resources, often at the expense of traditional users or other claimants. Suppression is also high, as the strict EEZ reading requires active naval patrols, diplomatic pressure, and legal challenges to deter infringements and suppress alternative interpretations of maritime law. Theater ratio is low, as the enforcement is genuinely aimed at maintaining exclusive control, not merely performance. Accessibility collapse is moderate-high, as alternative access to these zones is severely restricted. Resistance is moderate, coming from distant-water fishing nations and states with overlapping claims.
 *
 * PERSPECTIVAL GAP:
 *   Coastal states perceive the EEZ as a legitimate and necessary framework for national resource security and environmental protection. Excluded parties, however, view it as an extractive mechanism that privatizes global commons and suppresses historical or customary rights. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal states are clear beneficiaries, gaining exclusive resource access and jurisdiction. Distant-water fishing fleets, overlapping claimants, and non-ratifier states are victims, losing access or facing enforcement. International maritime tribunals act as observers, interpreting and applying the strict EEZ reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (orderly resource management) is still live, but its implementation under the strict EEZ reading has shifted towards maximizing national extraction, rather than purely coordinating global commons. The high extractiveness and suppression indicate it's not a pure rope, but the coordination function prevents it from being a pure snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_rights_legitimacy,
    'To what extent do historical usage and traditional fishing rights constitute a legitimate basis for claims that predate or override UNCLOS EEZ provisions?',
    'International legal precedent from cases explicitly weighing UNCLOS against historical claims, or a new international convention that formalizes the recognition of such rights.',
    'If historical rights are recognized as overriding, the strict EEZ reading''s legitimacy would be undermined, potentially reclassifying it as a snare for those with historical claims. If not, the strict EEZ reading''s authority is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_rights_legitimacy, conceptual, 'Ambiguity regarding the legal weight of historical rights versus UNCLOS provisions.').

omega_variable(
    non_ratifier_freedom_of_navigation,
    'Is freedom of navigation for non-ratifier states a customary international law that applies within EEZs, independent of UNCLOS ratification, and can it be enforced by naval presence?',
    'A definitive ruling by the International Court of Justice on the customary status of freedom of navigation within EEZs for non-ratifiers, or a shift in state practice towards universal acceptance or rejection of such claims.',
    'If freedom of navigation is upheld for non-ratifiers, the strict EEZ reading''s suppression of these claims would be challenged, potentially reducing its effective extractiveness and suppression for non-ratifier states. If rejected, the strict EEZ reading''s enforcement is further legitimized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_ratifier_freedom_of_navigation, empirical, 'Ambiguity regarding the customary international law status of freedom of navigation for non-ratifiers within EEZs.').

omega_variable(
    resource_depletion_vs_national_interest,
    'Is the primary driver for strict EEZ enforcement genuine resource conservation and sustainable management, or is it primarily national economic interest and geopolitical control?',
    'Independent audits of coastal states'' resource management practices, comparing declared conservation efforts with actual fishing quotas, environmental protection measures, and economic benefits derived.',
    'If national interest is the dominant driver, the coordination function of the strict EEZ reading is diminished, pushing it closer to a snare. If conservation is primary, its rope-like qualities are reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resource_depletion_vs_national_interest, preference, 'Whether EEZ enforcement prioritizes conservation or national economic gain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__strict_eez_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 1982, 0.05).
narrative_ontology:measurement(uncl_tr_t1995, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 1995, 0.08).
narrative_ontology:measurement(uncl_tr_t2005, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2005, 0.09).
narrative_ontology:measurement(uncl_tr_t2015, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(uncl_tr_t2024, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 1982, 0.5).
narrative_ontology:measurement(uncl_be_t1995, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(uncl_be_t2005, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement(uncl_be_t2015, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2015, 0.64).
narrative_ontology:measurement(uncl_be_t2024, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 1982, 0.6).
narrative_ontology:measurement(uncl_su_t1995, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement(uncl_su_t2005, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2005, 0.73).
narrative_ontology:measurement(uncl_su_t2015, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2015, 0.76).
narrative_ontology:measurement(uncl_su_t2024, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__strict_eez_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, maritime_boundary_disputes).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, deep_sea_mining_regulations).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, freedom_of_navigation_operations).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'unclos_sovereignty_boundary' kernel. It focuses on the strict interpretation of EEZ limits as exclusive and enforceable, contrasting with historical rights and non-ratifier claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
