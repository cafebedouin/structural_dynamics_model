% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__strict_geographic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__strict_geographic_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: unclos_maritime_sovereignty__strict_geographic_reading
 *   human_readable: UNCLOS Strict Geographic Definition of Islands
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint represents the 'strict geographic reading' of UNCLOS
 *   Article 121, which dictates that only naturally formed features above
 *   water at high tide qualify as islands generating territorial sea and EEZ.
 *   Artificial constructions, regardless of their size or permanence, do not
 *   alter legal status. This reading is foundational to maintaining the
 *   global commons and freedom of navigation, limiting the ability of coastal
 *   states to expand their maritime claims through man-made structures. It is
 *   presented as a Mountain due to its grounding in a widely accepted
 *   international treaty and its objective, natural-law-like criteria, though
 *   it faces political contestation from states seeking to expand their
 *   maritime zones.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, 0.2).
domain_priors:suppression_score(unclos_maritime_sovereignty__strict_geographic_reading, 0.4).
domain_priors:theater_ratio(unclos_maritime_sovereignty__strict_geographic_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__strict_geographic_reading, mountain).
narrative_ontology:human_readable(unclos_maritime_sovereignty__strict_geographic_reading, "UNCLOS Strict Geographic Definition of Islands").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__strict_geographic_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:emerges_naturally(unclos_maritime_sovereignty__strict_geographic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__strict_geographic_reading, '2f2e8aa5-1970-41bd-beeb-6289edec8987').
narrative_ontology:cs_kernel_codification('2f2e8aa5-1970-41bd-beeb-6289edec8987', fixed_text).
narrative_ontology:cs_authority_grounding('2f2e8aa5-1970-41bd-beeb-6289edec8987', lineage).
narrative_ontology:cs_interpretation_layer_present('2f2e8aa5-1970-41bd-beeb-6289edec8987').
narrative_ontology:cs_reading_relation('2f2e8aa5-1970-41bd-beeb-6289edec8987', unclos_maritime_sovereignty__expansive_construction_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f2e8aa5-1970-41bd-beeb-6289edec8987', unclos_maritime_sovereignty__hybrid_effective_control_reading, coexists_with).
narrative_ontology:cs_axiom('2f2e8aa5-1970-41bd-beeb-6289edec8987', foundational, natural_formation_is_prerequisite_for_maritime_zones).
narrative_ontology:cs_axiom_status(natural_formation_is_prerequisite_for_maritime_zones, holdable).
narrative_ontology:cs_axiom_grounding('2f2e8aa5-1970-41bd-beeb-6289edec8987', natural_formation_is_prerequisite_for_maritime_zones, conventional).
narrative_ontology:cs_axiom('2f2e8aa5-1970-41bd-beeb-6289edec8987', foundational, artificial_structures_are_installations_not_islands).
narrative_ontology:cs_axiom_status(artificial_structures_are_installations_not_islands, holdable).
narrative_ontology:cs_axiom_grounding('2f2e8aa5-1970-41bd-beeb-6289edec8987', artificial_structures_are_installations_not_islands, conventional).
narrative_ontology:cs_reference_frame('2f2e8aa5-1970-41bd-beeb-6289edec8987', unclos_original_intent_1982).
narrative_ontology:cs_drift_state('2f2e8aa5-1970-41bd-beeb-6289edec8987', contemporary_geopolitical_contest, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('2f2e8aa5-1970-41bd-beeb-6289edec8987', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, maritime_shipping_industry).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__strict_geographic_reading, freedom_of_navigation).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__strict_geographic_reading, rule_of_law_in_maritime_affairs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a clear, restrictive definition of islands, which preserves freedom of navigation in international waters and limits the expansion of territorial claims by coastal states. This reading aligns with their strategic interests in global mobility.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers, beneficiary,
    institutional, generational, arbitrage, global).

% Benefits from the stability and predictability of maritime boundaries, preventing arbitrary expansion of claims that could restrict access to resources or transit routes. They rely on established international law for dispute resolution.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_states, beneficiary,
    moderate, generational, mobile, global).

% Bears the cost of this strict interpretation as it limits their ability to create new territorial claims or extend existing ones through artificial island construction. They seek to maximize their maritime zones for resource exploitation and strategic control.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states, payer,
    powerful, generational, constrained, regional).

% Interprets and applies UNCLOS provisions, including the definition of islands. This reading provides a clear legal basis for their rulings, reinforcing the stability of international maritime law.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, international_tribunals, agenda_setter,
    institutional, civilizational, analytical, global).

% Benefits from predictable and unencumbered transit through international waters, reducing the risk of disputes or unexpected restrictions on navigation. A clear definition of islands minimizes uncertainty.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, maritime_shipping_industry, beneficiary,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, objective, and universally applicable standard for determining which geographic features generate maritime zones, thereby coordinating expectations and reducing disputes over territorial claims.
% TRANSFER_FUNCTION: Limits the transfer of potential maritime territory (and associated resources/strategic control) from the global commons to individual coastal states, by restricting the criteria for generating such claims.
% ABSENT_VOICES: States with limited natural coastlines or those seeking to expand their influence through artificial construction would advocate for a more permissive interpretation, but their arguments are largely foreclosed by the established legal text and its strict interpretation.
% DISAPPEARANCE_RATIONALE: If this strict geographic reading vanished, the international legal framework for maritime sovereignty would collapse into a free-for-all of competing claims based on artificial construction, leading to widespread geopolitical instability, resource conflicts, and challenges to freedom of navigation. The world would rearrange around a new, highly contested, and unstable set of de facto maritime boundaries.
% FOUNDING_PROBLEM: The problem of preventing arbitrary and expansive claims over the oceans, ensuring freedom of navigation, and establishing a stable legal order for maritime resource management and territorial disputes.
% FOUNDING_PROBLEM_CORROBORATION: Naval powers, international legal scholars, and non-claimant states consistently corroborate that the founding problem of preventing arbitrary maritime expansion remains live, citing ongoing attempts by some coastal states to assert claims based on artificial features. International tribunals' rulings also reinforce the continued relevance of this problem.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__strict_geographic_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__strict_geographic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__strict_geographic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unclos_maritime_sovereignty__strict_geographic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__strict_geographic_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, ExtMetricName, E),
    domain_priors:suppression_score(unclos_maritime_sovereignty__strict_geographic_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(unclos_maritime_sovereignty__strict_geographic_reading),
    narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.2) because this reading primarily prevents extraction by limiting claims, rather than performing it. Suppression is moderate (0.4) as it requires ongoing diplomatic and legal enforcement to counter revisionist interpretations, but it's not coercive in the sense of a Snare. Theater ratio is low (0.1) as the legal principle is genuinely applied, with minimal performative maintenance. Accessibility collapse is high (0.85) because the legal framework severely limits alternative interpretations for generating maritime zones. Resistance is low (0.1) because while some states challenge it, the international legal community largely upholds this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   While the international legal community and major naval powers view this as a stable, objective legal principle (Mountain), expansionist coastal states perceive it as an extractive constraint that limits their sovereign rights and development potential (closer to a Snare from their perspective). The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Naval powers and non-claimant states are beneficiaries, as this reading preserves their access to international waters and prevents arbitrary claims. Expansionist coastal states are payers, as their ambitions for territorial expansion are curtailed. International tribunals act as agenda-setters, upholding and interpreting the law. The maritime shipping industry benefits from the predictability.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate remains highly live: preventing arbitrary maritime claims and ensuring freedom of navigation. There is no evidence of mandatrophy; the ongoing geopolitical contestation confirms its continued relevance, rather than its atrophy. The classification as a Mountain, despite political resistance, reflects its structural role as a foundational principle of international law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_ambiguity,
    'Is the strict geographic definition of islands a genuine natural law (Mountain) derived from physical reality, or a constructed legal constraint (Rope/Tangled Rope) maintained by powerful states for strategic benefit?',
    'Analysis of state practice and legal arguments: if the principle''s persistence relies more on the enforcement capacity of naval powers than on its inherent logical or physical necessity, reclassify towards a constructed type.',
    'If reclassified as constructed, its extractiveness and suppression would be re-evaluated as properties of a human-made system, potentially shifting its classification from Mountain to Rope or Tangled Rope, especially from the perspective of expansionist states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_ambiguity, conceptual, 'Ambiguity between natural law and constructed legal norm.').

omega_variable(
    enforcement_capacity_vs_legal_principle,
    'To what extent does the persistence of this strict reading depend on the enforcement capacity of naval powers (e.g., freedom of navigation operations) versus its inherent legal authority and universal acceptance?',
    'Empirical study of compliance patterns in regions where naval power projection is weak or contested: if violations increase significantly, it suggests enforcement capacity is a primary driver.',
    'If enforcement capacity is the primary driver, the constraint''s ''suppression'' metric might be understated, and its classification could shift towards a Tangled Rope or Snare, as its persistence would be more dependent on coercion than on shared legal understanding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_vs_legal_principle, empirical, 'Role of power projection in maintaining legal interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__strict_geographic_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 1982, 0.05).
narrative_ontology:measurement(uncl_tr_t1995, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 1995, 0.08).
narrative_ontology:measurement(uncl_tr_t2005, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2005, 0.09).
narrative_ontology:measurement(uncl_tr_t2015, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(uncl_tr_t2024, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 1982, 0.15).
narrative_ontology:measurement(uncl_be_t1995, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 1995, 0.18).
narrative_ontology:measurement(uncl_be_t2005, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2005, 0.19).
narrative_ontology:measurement(uncl_be_t2015, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2015, 0.2).
narrative_ontology:measurement(uncl_be_t2024, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2024, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 1982, 0.3).
narrative_ontology:measurement(uncl_su_t1995, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 1995, 0.35).
narrative_ontology:measurement(uncl_su_t2005, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2005, 0.38).
narrative_ontology:measurement(uncl_su_t2015, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement(uncl_su_t2024, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
