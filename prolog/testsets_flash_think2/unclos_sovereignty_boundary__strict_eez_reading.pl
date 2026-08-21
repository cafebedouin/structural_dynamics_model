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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: unclos_sovereignty_boundary__strict_eez_reading
 *   human_readable: UNCLOS Strict EEZ Exclusivity
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'strict EEZ' reading of the UNCLOS
 *   sovereignty boundary kernel. It describes the regime where coastal states
 *   assert exclusive and enforceable rights over resources within their
 *   200-nautical-mile EEZs, with no overlay claims considered valid. This
 *   interpretation, while providing a framework for maritime governance, is
 *   characterized by high extraction from non-coastal states and those with
 *   historical claims, requiring significant active enforcement to maintain.
 *   The claimed type is 'tangled_rope' because it offers a genuine
 *   coordination function (clear boundaries for resource management) but
 *   couples it with substantial asymmetric extraction and requires active
 *   enforcement to hold.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, 0.75).
domain_priors:suppression_score(unclos_sovereignty_boundary__strict_eez_reading, 0.8).
domain_priors:theater_ratio(unclos_sovereignty_boundary__strict_eez_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__strict_eez_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__strict_eez_reading, "UNCLOS Strict EEZ Exclusivity").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__strict_eez_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__strict_eez_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__strict_eez_reading, '5cf1deb2-61dd-437d-a74b-19d1ca8beab5').
narrative_ontology:cs_kernel_codification('5cf1deb2-61dd-437d-a74b-19d1ca8beab5', fixed_text).
narrative_ontology:cs_authority_grounding('5cf1deb2-61dd-437d-a74b-19d1ca8beab5', lineage).
narrative_ontology:cs_interpretation_layer_present('5cf1deb2-61dd-437d-a74b-19d1ca8beab5').
narrative_ontology:cs_reading_relation('5cf1deb2-61dd-437d-a74b-19d1ca8beab5', unclos_sovereignty_boundary__historical_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('5cf1deb2-61dd-437d-a74b-19d1ca8beab5', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coexists_with).
narrative_ontology:cs_axiom('5cf1deb2-61dd-437d-a74b-19d1ca8beab5', foundational, state_sovereignty_over_maritime_resources).
narrative_ontology:cs_axiom_status(state_sovereignty_over_maritime_resources, holdable).
narrative_ontology:cs_axiom_grounding('5cf1deb2-61dd-437d-a74b-19d1ca8beab5', state_sovereignty_over_maritime_resources, conventional).
narrative_ontology:cs_axiom('5cf1deb2-61dd-437d-a74b-19d1ca8beab5', foundational, unclos_as_supreme_maritime_law).
narrative_ontology:cs_axiom_status(unclos_as_supreme_maritime_law, holdable).
narrative_ontology:cs_axiom_grounding('5cf1deb2-61dd-437d-a74b-19d1ca8beab5', unclos_as_supreme_maritime_law, conventional).
narrative_ontology:cs_reference_frame('5cf1deb2-61dd-437d-a74b-19d1ca8beab5', unclos_1982_framework).
narrative_ontology:cs_drift_state('5cf1deb2-61dd-437d-a74b-19d1ca8beab5', contemporary_geopolitical_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5cf1deb2-61dd-437d-a74b-19d1ca8beab5', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, distant_fishing_nations).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, states_with_overlapping_claims).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, non_ratifier_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, international_shipping_industry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states claim and enforce exclusive rights over resources within their 200-nautical-mile Exclusive Economic Zones (EEZs) as defined by UNCLOS Article 57. They benefit from exclusive access to vast marine resources and control over economic activities in these areas, actively defending against any perceived infringements.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, coastal_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Historically reliant on fishing in areas now designated as other states' EEZs, these nations face restricted access or must negotiate costly access agreements. Their traditional fishing grounds are now subject to the exclusive jurisdiction of coastal states, leading to economic losses and diplomatic friction.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, distant_fishing_nations, payer,
    organized, biographical, constrained, global).

% These states have their own EEZ claims that overlap with those of neighboring states, often due to geographical proximity or disputed territories. The strict interpretation of EEZ boundaries forces them into protracted negotiations or confrontations, limiting their access to contested areas.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, states_with_overlapping_claims, payer,
    powerful, biographical, constrained, regional).

% While EEZs restrict resource exploitation, they generally uphold freedom of navigation for transit. The shipping industry benefits from clear, internationally recognized rules for passage through EEZs, reducing uncertainty and potential for interference with commercial routes.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, international_shipping_industry, beneficiary,
    organized, biographical, mobile, global).

% This judicial body, established by UNCLOS, adjudicates disputes concerning the interpretation and application of the Convention. It plays a critical role in upholding the strict EEZ regime by providing legal clarity and enforcement mechanisms, though its authority depends on state adherence.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, international_tribunal_for_the_law_of_the_sea, agenda_setter,
    institutional, civilizational, analytical, universal).

% States that have not ratified UNCLOS (e.g., the United States) often adhere to many of its provisions as customary international law, including aspects of EEZ. However, they may challenge the strict exclusivity in certain contexts, particularly regarding military activities or freedom of navigation, creating friction with coastal states asserting full EEZ control.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, non_ratifier_states, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__strict_eez_reading, non_ratifier_states, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__strict_eez_reading, coastal_states).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__strict_eez_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes clear, internationally recognized maritime boundaries for resource management and jurisdiction, reducing potential conflicts over offshore resources and providing a framework for their sustainable exploitation.
% TRANSFER_FUNCTION: Transfers exclusive rights to marine resources (fish, oil, gas, minerals) and economic activities from a global commons or historical users to coastal states, within their 200-nautical-mile EEZs.
% ABSENT_VOICES: Indigenous communities with traditional fishing rights not explicitly recognized by UNCLOS, states whose geographical limitations prevent them from claiming significant EEZs, and environmental groups advocating for a global commons approach to ocean resources would object to the strict exclusivity.
% DISAPPEARANCE_RATIONALE: If strict EEZ boundaries and their enforceability vanished overnight, there would be widespread, immediate disputes over maritime resources, increased naval confrontations, and a collapse of regulated fishing and resource extraction, leading to overexploitation, environmental degradation, and geopolitical instability. The global maritime order would fundamentally reorganize.
% FOUNDING_PROBLEM: Unregulated exploitation of ocean resources, overlapping claims, and potential for conflict over maritime jurisdiction in the mid-20th century, exacerbated by technological advancements in offshore resource extraction.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, naval strategists, and environmental organizations corroborate the ongoing need for clear maritime governance to prevent conflict and manage resources, even while acknowledging the extractive aspects and ongoing challenges to the strict interpretation. Diplomatic records and UN reports also attest to the historical problems UNCLOS aimed to solve.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__strict_eez_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__strict_eez_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__strict_eez_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(unclos_sovereignty_boundary__strict_eez_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__strict_eez_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.75) reflects the immense value of exclusive resource control granted to coastal states, often at the expense of traditional users. Suppression (0.80) is high due to the constant need for naval patrols, legal challenges, and diplomatic pressure to deter and punish infringements or competing claims. The moderate theater ratio (0.40) acknowledges that while enforcement is real, there's also a performative aspect to sovereignty assertions, with diplomatic posturing and symbolic actions. Accessibility collapse is very high (0.85) as non-coastal states effectively lose access to resources within others' EEZs. Resistance is high (0.70) from various actors challenging the strict interpretation.
 *
 * PERSPECTIVAL GAP:
 *   Coastal states perceive the strict EEZ regime as a legitimate exercise of sovereign rights and a necessary coordination mechanism for resource management. Conversely, distant fishing nations and states with historical or overlapping claims view it as an extractive mechanism that dispossesses them of traditional access or legitimate claims. The engine's computation of per-seat types will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal states are clear beneficiaries and agenda-setters, gaining exclusive resource control. Distant fishing nations and states with overlapping claims are primary targets, bearing the costs of exclusion or contested access. The international shipping industry benefits from clear navigation rules, placing them as secondary beneficiaries. Non-ratifier states, while not fully bound, often find their actions constrained by the regime, making them payers in practice.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_rights_vs_unclos_primacy,
    'To what extent do historical usage rights or traditional claims legitimately override or coexist with the strict UNCLOS EEZ provisions?',
    'International legal precedent from ITLOS or ICJ rulings that explicitly weigh historical rights against UNCLOS articles, or a new international convention that formalizes such a hierarchy.',
    'If historical rights are deemed to override, the strict EEZ reading''s extractiveness would decrease for some victims, and its suppression of alternative claims would be less legitimate, potentially reclassifying it towards a more balanced ''rope'' or even ''scaffold'' if transitional. If they coexist, the ''tangled_rope'' classification would be reinforced, but with a more nuanced understanding of its coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_rights_vs_unclos_primacy, conceptual, 'Ambiguity regarding the primacy of UNCLOS EEZ over historical claims.').

omega_variable(
    non_ratifier_enforcement_impact,
    'How does the enforcement of freedom of navigation by non-ratifier states (e.g., US) affect the practical enforceability and legitimacy of strict EEZ exclusivity?',
    'Empirical analysis of naval incidents, diplomatic protests, and legal challenges involving non-ratifier states within claimed EEZs, assessing the actual erosion or reinforcement of strict EEZ claims.',
    'If non-ratifier enforcement significantly undermines strict EEZ claims, the ''suppression'' metric for this reading would be lower, and its ''extractiveness'' might be less stable, potentially shifting its classification towards a ''piton'' if enforcement becomes purely theatrical, or a ''rope'' if a new, more balanced customary law emerges. If it has negligible impact, the current metrics are reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_ratifier_enforcement_impact, empirical, 'Impact of non-ratifier states'' actions on strict EEZ enforceability.').

omega_variable(
    resource_protection_vs_sovereign_assertion,
    'Is the primary driver for strict EEZ enforcement genuine resource protection and sustainable management, or is it primarily an assertion of sovereign power and economic extraction?',
    'Analysis of coastal states'' actual resource management practices (e.g., fishing quotas, environmental regulations) within their EEZs, compared to their enforcement efforts against foreign vessels. Discrepancies would indicate a stronger extractive motive.',
    'If resource protection is the dominant driver, the ''tangled_rope'' classification leans more towards its coordination function, potentially reducing the perceived ''extractiveness'' for some observers. If sovereign assertion/extraction dominates, the ''snare'' characteristics are amplified, reinforcing the high ''extractiveness'' and ''suppression'' metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_protection_vs_sovereign_assertion, empirical, 'Distinguishing motives for strict EEZ enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__strict_eez_reading, 1982, 2022).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 1982, 0.3).
narrative_ontology:measurement(uncl_tr_t1990, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 1990, 0.33).
narrative_ontology:measurement(uncl_tr_t1998, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 1998, 0.36).
narrative_ontology:measurement(uncl_tr_t2006, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2006, 0.38).
narrative_ontology:measurement(uncl_tr_t2014, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2014, 0.39).
narrative_ontology:measurement(uncl_tr_t2022, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2022, 0.4).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 1982, 0.6).
narrative_ontology:measurement(uncl_be_t1990, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(uncl_be_t1998, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 1998, 0.69).
narrative_ontology:measurement(uncl_be_t2006, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2006, 0.72).
narrative_ontology:measurement(uncl_be_t2014, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2014, 0.74).
narrative_ontology:measurement(uncl_be_t2022, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2022, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 1982, 0.65).
narrative_ontology:measurement(uncl_su_t1990, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(uncl_su_t1998, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 1998, 0.74).
narrative_ontology:measurement(uncl_su_t2006, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2006, 0.77).
narrative_ontology:measurement(uncl_su_t2014, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2014, 0.79).
narrative_ontology:measurement(uncl_su_t2022, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2022, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__strict_eez_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, global_fishing_quotas).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, offshore_oil_gas_exploration).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, freedom_of_navigation_doctrine).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary__historical_rights_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'unclos_sovereignty_boundary' kernel. Each reading has a different ε value and structural profile, reflecting different interpretations of international maritime law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
