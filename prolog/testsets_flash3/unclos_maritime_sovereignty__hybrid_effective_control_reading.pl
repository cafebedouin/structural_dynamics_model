% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__hybrid_effective_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__hybrid_effective_control_reading, []).

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
 *   constraint_id: unclos_maritime_sovereignty__hybrid_effective_control_reading
 *   human_readable: UNCLOS Maritime Sovereignty: Hybrid Effective Control Reading
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint represents a 'hybrid effective control' reading of UNCLOS
 *   maritime sovereignty, where natural features grant full maritime zones,
 *   but artificial features, through prolonged and unchallenged effective
 *   control, can mature beyond mere safety zones into de facto territorial
 *   claims. This reading acknowledges the practical realities of state
 *   behavior and technological advancements, creating a graduated system of
 *   sovereignty. It is a Tangled Rope because it offers a coordination
 *   function (some clarity on claims) but also enables asymmetric extraction
 *   by states with the capacity to construct and maintain control.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.65).
domain_priors:suppression_score(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.7).
domain_priors:theater_ratio(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__hybrid_effective_control_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__hybrid_effective_control_reading, "UNCLOS Maritime Sovereignty: Hybrid Effective Control Reading").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__hybrid_effective_control_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__hybrid_effective_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'e9f820c5-2dc6-4ba3-bb41-a1fcde058f91').
narrative_ontology:cs_kernel_codification('e9f820c5-2dc6-4ba3-bb41-a1fcde058f91', formalized).
narrative_ontology:cs_authority_grounding('e9f820c5-2dc6-4ba3-bb41-a1fcde058f91', practice).
narrative_ontology:cs_interpretation_layer_present('e9f820c5-2dc6-4ba3-bb41-a1fcde058f91').
narrative_ontology:cs_reading_relation('e9f820c5-2dc6-4ba3-bb41-a1fcde058f91', unclos_maritime_sovereignty__strict_geographic_reading, coexists_with).
narrative_ontology:cs_reading_relation('e9f820c5-2dc6-4ba3-bb41-a1fcde058f91', unclos_maritime_sovereignty__expansive_construction_reading, coexists_with).
narrative_ontology:cs_axiom('e9f820c5-2dc6-4ba3-bb41-a1fcde058f91', foundational, effective_control_as_legitimacy_factor).
narrative_ontology:cs_axiom_status(effective_control_as_legitimacy_factor, holdable).
narrative_ontology:cs_axiom_grounding('e9f820c5-2dc6-4ba3-bb41-a1fcde058f91', effective_control_as_legitimacy_factor, conventional).
narrative_ontology:cs_axiom('e9f820c5-2dc6-4ba3-bb41-a1fcde058f91', foundational, graduated_sovereignty_for_artificial_features).
narrative_ontology:cs_axiom_status(graduated_sovereignty_for_artificial_features, holdable).
narrative_ontology:cs_axiom_grounding('e9f820c5-2dc6-4ba3-bb41-a1fcde058f91', graduated_sovereignty_for_artificial_features, conventional).
narrative_ontology:cs_reference_frame('e9f820c5-2dc6-4ba3-bb41-a1fcde058f91', post_unclos_technological_advancement).
narrative_ontology:cs_drift_state('e9f820c5-2dc6-4ba3-bb41-a1fcde058f91', contemporary_geopolitical_contest, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e9f820c5-2dc6-4ba3-bb41-a1fcde058f91', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_capacity).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, regional_powers).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimants).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_without_construction_capacity).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, international_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states benefit from the ambiguity, using their technological and financial capacity to construct artificial features and gradually assert control, leveraging the 'effective control' aspect of this reading to expand their maritime claims beyond strict geographic interpretations.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_capacity, beneficiary,
    powerful, generational, mobile, regional).

% These powers actively shape the interpretation and enforcement of maritime law in their regions, pushing for readings that allow for the expansion of their influence through constructed features and prolonged presence. They administer the de facto rules of engagement.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, regional_powers, agenda_setter,
    institutional, generational, arbitrage, regional).

% These states bear the cost of expanded claims by powerful neighbors, losing access to fishing grounds, resources, and strategic sea lanes. They lack the military or diplomatic power to effectively challenge prolonged effective control.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimants, payer,
    powerless, biographical, trapped, regional).

% These states are disadvantaged by a reading that allows artificial features to mature into territorial claims, as they cannot compete in the construction race. Their claims are limited to natural features, while others expand through artificial means.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_without_construction_capacity, payer,
    moderate, biographical, constrained, global).

% The broader international community, represented by UNCLOS signatories and international legal bodies, struggles to maintain a consistent interpretation of maritime law. This reading allows for creeping expansion that undermines the spirit of the convention, but direct challenge is often politically costly.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, international_community, excluded,
    organized, civilizational, constrained, global).

% These bodies interpret and apply UNCLOS, but their rulings are often non-binding or subject to political pressure. They observe the practical drift in state behavior and attempt to reconcile it with legal principles, often facing challenges in enforcing strict interpretations against powerful actors.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, international_maritime_tribunals, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_capacity).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__hybrid_effective_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for states to delineate maritime boundaries and manage resources, aiming to prevent conflict over sea areas by offering a graduated system of claims based on geographic features and, implicitly, the reality of sustained presence.
% TRANSFER_FUNCTION: Transfers potential sovereign rights and resource access from the international commons or weaker claimants to states capable of establishing and maintaining effective control over artificial features, gradually converting de facto control into de jure claims.
% ABSENT_VOICES: Small island developing states and landlocked states, who lack the capacity to project power or construct artificial features, would advocate for a strict interpretation of natural features to protect their limited maritime entitlements and prevent encroachment by larger powers.
% DISAPPEARANCE_RATIONALE: If this hybrid reading vanished, the legal landscape for maritime claims would become either strictly geographic (favoring natural features) or entirely open to artificial expansion (favoring construction capacity). This would lead to a significant rearrangement of geopolitical strategies, resource allocation, and potential for conflict, as states would either lose or gain substantial maritime territory.
% FOUNDING_PROBLEM: The original UNCLOS framework struggled to anticipate the technological capacity for large-scale artificial island construction and the geopolitical implications of such features in contested areas, leading to ambiguity regarding their legal status.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars and environmental organizations corroborate that the problem of defining the legal status of artificial features and the role of effective control remains a live and contentious issue, with ongoing disputes in various maritime regions. This is attested by numerous academic papers, UN reports, and environmental impact assessments from outside the benefiting parties.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__hybrid_effective_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__hybrid_effective_control_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__hybrid_effective_control_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__hybrid_effective_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because this reading allows powerful states to expand their maritime claims at the expense of weaker ones or the international commons. Suppression (0.70) is also high, as the 'unchallenged' aspect of effective control implies a need for military or diplomatic power to deter counter-claims. Theater ratio (0.20) is moderate; while there's genuine legal debate, the 'maturation' of claims often involves performative assertions of sovereignty. The metrics reflect the practical, power-driven application of this interpretation over time.
 *
 * PERSPECTIVAL GAP:
 *   Powerful states view this reading as a pragmatic adaptation of international law to modern realities, allowing for stability through established control. Weaker states see it as a legitimization of 'might makes right,' enabling resource grabs and territorial encroachment. The engine's classification will highlight this divergence, showing a Tangled Rope for the system as a whole, but potentially a Rope or even a Mountain from the perspective of the powerful beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   States with construction capacity and regional powers are beneficiaries (d near 0.0) as they gain territory and resources. Militarily weaker claimants and states without construction capacity are victims (d near 1.0) as they lose out. The international community is excluded, bearing the diffuse cost of a less stable maritime order. International maritime tribunals are observers, attempting to apply law to a shifting reality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_effective_control,
    'What constitutes ''prolonged effective control absent challenge'' in a legally binding sense, and at what point does a safety zone mature into a territorial claim?',
    'A definitive ruling by the International Court of Justice or a new UNCLOS protocol specifically addressing artificial features and the criteria for their territorial maturation.',
    'Clearer definitions would reduce ambiguity, potentially lowering extractiveness and suppression by limiting opportunistic claims, or conversely, solidifying existing de facto claims into de jure rights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_effective_control, conceptual, 'Ambiguity in the criteria for converting artificial features into territorial claims.').

omega_variable(
    challenge_capacity_asymmetry,
    'To what extent does the ''absent challenge'' clause inherently favor militarily and economically powerful states, effectively suppressing legitimate counter-claims from weaker nations?',
    'Empirical analysis of maritime disputes involving artificial features, correlating outcomes with the relative power of claimant states and the frequency/effectiveness of challenges from weaker parties.',
    'If challenge capacity is highly asymmetric, the constraint''s suppression and extractiveness are higher than measured, as the ''absence of challenge'' is a coerced outcome, not a consensual one. This would push the classification closer to a Snare for weaker claimants.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(challenge_capacity_asymmetry, empirical, 'The ''absent challenge'' clause disproportionately benefits powerful states.').

omega_variable(
    environmental_impact_vs_sovereignty,
    'How should the environmental impact of artificial feature construction and the destruction of marine ecosystems be weighed against claims of sovereign rights derived from such features?',
    'Integration of environmental protection protocols into UNCLOS with enforcement mechanisms that can override or limit sovereignty claims based on ecological damage, or a new international treaty prioritizing marine environmental integrity.',
    'If environmental impact is prioritized, the legitimacy of claims based on artificial features would be diminished, potentially reducing the extractiveness of this reading. If not, the environmental costs are externalized onto the global commons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_impact_vs_sovereignty, preference, 'Balancing sovereign claims from artificial features against environmental protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(uncl_tr_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(uncl_tr_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(uncl_tr_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(uncl_tr_t40, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(uncl_be_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(uncl_be_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(uncl_be_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(uncl_be_t40, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 40, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(uncl_su_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(uncl_su_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(uncl_su_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(uncl_su_t40, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__hybrid_effective_control_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, freedom_of_navigation_operations).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, south_china_sea_fishing_rights).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'unclos_maritime_sovereignty' kernel. This 'hybrid effective control' reading acknowledges a graduated system of claims, influencing and coexisting with both the 'strict geographic' and 'expansive construction' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
