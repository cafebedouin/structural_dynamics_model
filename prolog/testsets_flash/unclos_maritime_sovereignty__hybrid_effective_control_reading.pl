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
 *   This constraint represents the 'hybrid effective control' reading of
 *   UNCLOS maritime sovereignty, where natural features generate full
 *   maritime zones, but artificial features can mature into territorial
 *   claims through prolonged, unchallenged effective control. This reading
 *   acknowledges the legal distinction between natural and artificial
 *   features but allows for a de facto evolution of status based on state
 *   practice. It is a Tangled Rope because it provides a coordination
 *   mechanism for managing evolving claims while enabling asymmetric
 *   extraction by states with greater capacity for construction and
 *   enforcement.
 *
 * KEY AGENTS:
 *   - states_with_construction_capacity: Agenda-setter (institutional/mobile) — benefits from ambiguity, expands claims.
 *   - regional_maritime_powers: Beneficiary (powerful/constrained) — enforces claims, benefits from managed instability.
 *   - militarily_weaker_claimants: Payer (powerless/trapped) — loses maritime zones, limited recourse.
 *   - states_without_construction_capacity: Payer (moderate/constrained) — disadvantaged by uneven playing field.
 *   - international_shipping: Payer (organized/constrained) — faces uncertainty, increased costs.
 *   - unclos_arbitration_tribunals: Observer (institutional/analytical) — interprets law, resolves disputes.
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
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__hybrid_effective_control_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__hybrid_effective_control_reading, "UNCLOS Maritime Sovereignty: Hybrid Effective Control Reading").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__hybrid_effective_control_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__hybrid_effective_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'cc9aa511-ce9b-42ea-9eaf-7fdb64899785').
narrative_ontology:cs_kernel_codification('cc9aa511-ce9b-42ea-9eaf-7fdb64899785', formalized).
narrative_ontology:cs_authority_grounding('cc9aa511-ce9b-42ea-9eaf-7fdb64899785', lineage).
narrative_ontology:cs_interpretation_layer_present('cc9aa511-ce9b-42ea-9eaf-7fdb64899785').
narrative_ontology:cs_reading_relation('cc9aa511-ce9b-42ea-9eaf-7fdb64899785', unclos_maritime_sovereignty__strict_geographic_reading, coexists_with).
narrative_ontology:cs_reading_relation('cc9aa511-ce9b-42ea-9eaf-7fdb64899785', unclos_maritime_sovereignty__expansive_construction_reading, coexists_with).
narrative_ontology:cs_axiom('cc9aa511-ce9b-42ea-9eaf-7fdb64899785', foundational, graduated_sovereignty_by_feature_type).
narrative_ontology:cs_axiom_status(graduated_sovereignty_by_feature_type, holdable).
narrative_ontology:cs_axiom_grounding('cc9aa511-ce9b-42ea-9eaf-7fdb64899785', graduated_sovereignty_by_feature_type, conventional).
narrative_ontology:cs_axiom('cc9aa511-ce9b-42ea-9eaf-7fdb64899785', foundational, effective_control_as_claim_maturation_factor).
narrative_ontology:cs_axiom_status(effective_control_as_claim_maturation_factor, holdable).
narrative_ontology:cs_axiom_grounding('cc9aa511-ce9b-42ea-9eaf-7fdb64899785', effective_control_as_claim_maturation_factor, conventional).
narrative_ontology:cs_reference_frame('cc9aa511-ce9b-42ea-9eaf-7fdb64899785', unclos_original_intent_plus_state_practice).
narrative_ontology:cs_drift_state('cc9aa511-ce9b-42ea-9eaf-7fdb64899785', contemporary_maritime_expansion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cc9aa511-ce9b-42ea-9eaf-7fdb64899785', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_capacity).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, regional_maritime_powers).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimants).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_without_construction_capacity).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, international_shipping).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states actively engage in artificial island building and other maritime construction. They benefit from the ambiguity that allows claims to mature through effective control, expanding their maritime zones and resource access. They are the primary enforcers of these evolving claims.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_capacity, agenda_setter,
    institutional, generational, mobile, regional).

% States with significant naval and coast guard capabilities that can project power to enforce claims over both natural and artificial features. They benefit from the stability (or managed instability) this reading provides, allowing for gradual expansion of influence.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, regional_maritime_powers, beneficiary,
    powerful, generational, constrained, regional).

% States with overlapping claims but insufficient military or economic power to challenge the prolonged effective control of larger states. They bear the cost of lost maritime zones and resources, with limited recourse beyond diplomatic protest.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimants, payer,
    powerless, biographical, trapped, regional).

% These states cannot build artificial features to expand their maritime claims. They are disadvantaged by a reading that allows such features to mature into territorial claims, as it creates an uneven playing field in resource competition.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_without_construction_capacity, payer,
    moderate, biographical, constrained, global).

% Commercial and military vessels that rely on freedom of navigation. They face increased uncertainty and potential restrictions as maritime claims expand and become contested, leading to longer routes or higher insurance costs.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, international_shipping, payer,
    organized, immediate, constrained, global).

% International legal bodies tasked with interpreting UNCLOS and resolving maritime disputes. They observe the evolving practices and legal arguments, but their rulings depend on the willingness of states to submit to and abide by arbitration.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_arbitration_tribunals, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for states to assert and manage maritime claims, balancing the inherent rights of natural features with the evolving reality of human modification and effective control in disputed areas.
% TRANSFER_FUNCTION: Transfers potential maritime zones, resource rights, and strategic control from areas previously considered international waters or disputed zones to states capable of establishing and maintaining effective control over features, natural or artificial.
% ABSENT_VOICES: Future generations and the global commons are absent. They would argue for a more restrictive interpretation of maritime claims to preserve freedom of navigation and shared resources, but their interests are not directly represented in current state-centric negotiations.
% DISAPPEARANCE_RATIONALE: If this hybrid reading vanished, the legal status of numerous artificial features and their associated claims would become immediately ambiguous or revert to strict UNCLOS interpretations. This would trigger widespread diplomatic protests, potential military confrontations over disputed zones, and a complete re-evaluation of maritime boundaries and resource access, fundamentally reorganizing geopolitical strategy.
% FOUNDING_PROBLEM: The original UNCLOS framework did not fully anticipate the scale and strategic importance of artificial island building and prolonged effective control in contested maritime areas, leading to ambiguity in how these activities affect sovereignty claims.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars and geopolitical analysts, independent of claimant states, widely corroborate that the UNCLOS framework faces ongoing challenges in adapting to new technologies and state practices in maritime construction and control, confirming the problem is live and evolving.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__hybrid_effective_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__hybrid_effective_control_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.65) because this reading allows powerful states to expand their maritime control beyond strict geographic definitions, capturing resources and strategic advantage. Suppression (0.70) is also high, as militarily weaker states are effectively suppressed from challenging established 'effective control' claims. The theater ratio is moderate (0.20), reflecting that while there's genuine legal debate, some arguments for 'maturation' serve to legitimize pre-existing power dynamics. The metrics show a gradual increase in extractiveness and suppression over time, reflecting the increasing assertiveness of states in claiming maritime zones through construction and control.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of states with construction capacity, this reading offers a pragmatic path to resolving ambiguities and securing legitimate interests in evolving maritime landscapes. For weaker claimants, it is a mechanism by which powerful states incrementally erode established international law to their advantage, creating a de facto 'might makes right' scenario. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   States with construction capacity and regional maritime powers are beneficiaries, as the reading allows them to expand their influence and resource access. Militarily weaker claimants and states without construction capacity are payers, losing out on potential maritime zones. International shipping also acts as a payer due to increased uncertainty and potential restrictions. UNCLOS tribunals are observers, analyzing the situation without direct benefit or cost.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the constraint as a pure Snare by acknowledging the genuine coordination function of managing evolving claims in a complex maritime environment. However, it also highlights how this coordination can be leveraged for asymmetric extraction, preventing it from being mislabeled as a pure Rope. The 'maturation' aspect is a key area for mandatrophy, as the original intent of UNCLOS was not to reward artificial construction with full sovereignty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_artificial_feature_distinction,
    'Is the legal distinction between natural and artificial features, as it pertains to generating maritime zones, fundamentally eroding due to state practice and this ''hybrid'' interpretation?',
    'Analysis of future international court rulings and state practice: if artificial features are consistently treated as generating full zones, the distinction has eroded.',
    'If the distinction erodes, the constraint shifts closer to the ''expansive construction'' reading, increasing extractiveness for states without construction capacity and potentially reclassifying as a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_artificial_feature_distinction, empirical, 'Ambiguity regarding the enduring legal significance of natural vs. artificial features.').

omega_variable(
    effective_control_threshold_ambiguity,
    'What constitutes ''prolonged effective control absent challenge'' sufficient for an artificial feature to mature into a territorial claim, and is this threshold consistently applied or subject to power dynamics?',
    'Detailed case studies of specific claims and challenges, assessing the duration and nature of control, and the capacity of challenging states.',
    'If the threshold is consistently high and objective, the constraint leans more towards a Tangled Rope. If it is low and primarily determined by the power of the claimant, it shifts towards a Snare, as ''absence of challenge'' becomes ''suppression of challenge''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effective_control_threshold_ambiguity, conceptual, 'Uncertainty in the criteria for ''maturation'' of artificial claims.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''hybrid effective control'' reading of UNCLOS maritime sovereignty, or does it more closely align with a strict geographic or expansive construction reading?',
    'Comparative analysis with the core tenets and state practices associated with the strict geographic and expansive construction readings, focusing on the treatment of artificial features and the role of effective control.',
    'Misidentification would lead to an inaccurate classification and mischaracterization of the underlying power dynamics. If it''s closer to strict geographic, extractiveness would be lower; if closer to expansive construction, extractiveness would be higher and suppression more overt.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Verifying the correct kernel reading for this constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__hybrid_effective_control_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(uncl_tr_t1995, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(uncl_tr_t2005, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(uncl_tr_t2015, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(uncl_tr_t2024, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 1982, 0.4).
narrative_ontology:measurement(uncl_be_t1995, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 1995, 0.5).
narrative_ontology:measurement(uncl_be_t2005, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(uncl_be_t2015, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement(uncl_be_t2024, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 1982, 0.45).
narrative_ontology:measurement(uncl_su_t1995, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(uncl_su_t2005, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2005, 0.62).
narrative_ontology:measurement(uncl_su_t2015, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement(uncl_su_t2024, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__hybrid_effective_control_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__expansive_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the UNCLOS maritime sovereignty kernel. This 'hybrid effective control' reading acknowledges both natural features and the maturation of artificial claims, influencing and coexisting with the 'strict geographic' and 'expansive construction' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
