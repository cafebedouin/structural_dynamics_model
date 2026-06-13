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
 *   This constraint describes a reading of UNCLOS (United Nations Convention
 *   on the Law of the Sea) that differentiates between natural and artificial
 *   maritime features for sovereignty claims. Natural features (islands,
 *   rocks) generate full territorial seas and Exclusive Economic Zones
 *   (EEZs). Artificial features (man-made islands, installations) initially
 *   generate only limited 500m safety zones, but can mature into broader
 *   territorial claims if a state exercises prolonged and unchallenged
 *   effective control. This hybrid approach attempts to balance traditional
 *   maritime law with the realities of modern construction and geopolitical
 *   assertion, but creates a gray area that favors states with the capacity
 *   to build and project power.
 *
 * KEY AGENTS:
 *   - states_with_construction_capacity: Primary beneficiary (institutional/arbitrage) — leverages capacity to expand claims.
 *   - regional_maritime_powers: Primary beneficiary (institutional/arbitrage) — benefits from ambiguity and ability to project power.
 *   - militarily_weaker_claimants: Primary victim (powerless/trapped) — unable to challenge claims effectively.
 *   - states_without_construction_capacity: Victim (moderate/constrained) — unable to leverage the 'effective control' clause.
 *   - international_shipping: Victim (organized/constrained) — faces increased restrictions and potential disputes in newly claimed areas.
 *   - unclos_arbitration_bodies: Agenda setter/Observer (institutional/analytical) — interprets and adjudicates disputes, but often after facts on the ground are established.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.6).
domain_priors:suppression_score(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.7).
domain_priors:theater_ratio(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__hybrid_effective_control_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__hybrid_effective_control_reading, "UNCLOS Maritime Sovereignty: Hybrid Effective Control Reading").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__hybrid_effective_control_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__hybrid_effective_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__hybrid_effective_control_reading, '786c6ba5-2c59-473e-b7b9-0ec412bd05dd').
narrative_ontology:cs_kernel_codification('786c6ba5-2c59-473e-b7b9-0ec412bd05dd', formalized).
narrative_ontology:cs_authority_grounding('786c6ba5-2c59-473e-b7b9-0ec412bd05dd', lineage).
narrative_ontology:cs_interpretation_layer_present('786c6ba5-2c59-473e-b7b9-0ec412bd05dd').
narrative_ontology:cs_reading_relation('786c6ba5-2c59-473e-b7b9-0ec412bd05dd', unclos_maritime_sovereignty__strict_geographic_reading, influences).
narrative_ontology:cs_reading_relation('786c6ba5-2c59-473e-b7b9-0ec412bd05dd', unclos_maritime_sovereignty__expansive_construction_reading, influences).
narrative_ontology:cs_axiom('786c6ba5-2c59-473e-b7b9-0ec412bd05dd', foundational, graduated_sovereignty_by_feature_type).
narrative_ontology:cs_axiom_status(graduated_sovereignty_by_feature_type, holdable).
narrative_ontology:cs_axiom_grounding('786c6ba5-2c59-473e-b7b9-0ec412bd05dd', graduated_sovereignty_by_feature_type, conventional).
narrative_ontology:cs_axiom('786c6ba5-2c59-473e-b7b9-0ec412bd05dd', foundational, effective_control_as_legitimizing_factor).
narrative_ontology:cs_axiom_status(effective_control_as_legitimizing_factor, holdable).
narrative_ontology:cs_axiom_grounding('786c6ba5-2c59-473e-b7b9-0ec412bd05dd', effective_control_as_legitimizing_factor, conventional).
narrative_ontology:cs_reference_frame('786c6ba5-2c59-473e-b7b9-0ec412bd05dd', unclos_1982_text_and_early_practice).
narrative_ontology:cs_drift_state('786c6ba5-2c59-473e-b7b9-0ec412bd05dd', contemporary_geopolitical_assertion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('786c6ba5-2c59-473e-b7b9-0ec412bd05dd', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_capacity).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, regional_maritime_powers).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimants).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_without_construction_capacity).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, international_shipping).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'none', 1).

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
 *   The extractiveness (0.6) is substantial because the 'effective control' clause allows powerful states to gradually expand their maritime claims beyond what strict geographic principles would permit, effectively extracting sovereign rights from the global commons or from weaker neighbors. Suppression (0.7) is high as the 'absent challenge' condition implies that states lacking military or diplomatic capacity to protest effectively are suppressed from asserting their rights. Theater ratio (0.2) is moderate; while there's genuine legal interpretation, the 'effective control' aspect often involves performative acts of presence and administration to solidify claims. Accessibility collapse (0.4) is moderate; while some alternatives (e.g., strict adherence to natural features) are conceptually available, the geopolitical reality makes them difficult to pursue. Resistance (0.5) is also moderate, as weaker states often protest diplomatically but rarely militarily.
 *
 * PERSPECTIVAL GAP:
 *   States with construction capacity and regional maritime powers view this reading as a pragmatic evolution of international law, allowing for necessary adaptation to new technologies and security needs. Militarily weaker claimants and states without construction capacity perceive it as a mechanism for powerful states to legitimize land-grabbing and expand their influence, effectively creating a two-tiered system of maritime rights. UNCLOS arbitration bodies attempt to apply the law neutrally, but often find themselves adjudicating disputes where facts on the ground, established through 'effective control,' heavily influence outcomes.
 *
 * DIRECTIONALITY LOGIC:
 *   States with construction capacity and regional maritime powers are clear beneficiaries (d near 0.0) as they gain new territorial claims and strategic advantages. Militarily weaker claimants and states without construction capacity are victims (d near 1.0) as their potential claims are diminished or foreclosed by the actions of more powerful states. International shipping is also a victim, facing increased navigational restrictions. UNCLOS arbitration bodies are analytical observers (d near 0.5), tasked with interpreting and applying the law, but their decisions are influenced by the established facts of 'effective control'.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading of UNCLOS is not experiencing mandatrophy in the traditional sense, as its mandate (governing maritime claims) is still highly relevant. However, the 'effective control' clause introduces a potential for function creep, where a coordination mechanism (defining maritime zones) becomes an extraction mechanism (legitimizing expansion by force or economic power). The classification as a Tangled Rope reflects this hybrid nature, preventing it from being mislabeled as a pure Rope (which would ignore the asymmetric extraction) or a Snare (which would ignore the genuine coordination function for natural features).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_artificial_feature_ambiguity,
    'Is the distinction between ''natural'' and ''artificial'' features sufficiently clear and consistently applied in practice, or does it allow for strategic reinterpretation?',
    'Analysis of international court rulings and state practice over time; if rulings consistently uphold a clear distinction, ambiguity is low. If states frequently reclassify features to their advantage, ambiguity is high.',
    'If ambiguous, the constraint''s effective extractiveness increases for states capable of strategic reinterpretation, as they can leverage the ambiguity to expand claims. If clear, it functions more as a coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_artificial_feature_ambiguity, empirical, 'Ambiguity in defining natural vs. artificial maritime features.').

omega_variable(
    effective_control_threshold_ambiguity,
    'What constitutes ''prolonged effective control absent challenge'' for artificial features to mature into territorial claims, and is this threshold consistently applied?',
    'Review of state protests, diplomatic responses, and military presence in contested areas. A clear, consistently enforced threshold reduces ambiguity; a vague or selectively enforced threshold increases it.',
    'If the threshold is vague, it favors states with greater military and diplomatic capacity to assert and maintain control, increasing extraction from weaker claimants. If clear, it provides a more stable basis for coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effective_control_threshold_ambiguity, empirical, 'Ambiguity in the criteria for ''effective control'' over artificial features.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''hybrid_effective_control_reading'' of the ''unclos_maritime_sovereignty'' kernel. What would change if the ''strict_geographic_reading'' or ''expansive_construction_reading'' were adopted?',
    'Conceptual analysis of legal precedents and state interpretations. The ''strict_geographic_reading'' would reduce the scope of claims from artificial features, decreasing extraction. The ''expansive_construction_reading'' would legitimize broader claims from artificial features, increasing extraction.',
    'Adopting the ''strict_geographic_reading'' would shift the constraint towards a Rope or even Mountain for natural features, and reduce the extractive potential of artificial ones. Adopting the ''expansive_construction_reading'' would push it further towards a Snare, legitimizing more aggressive claims by powerful states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identification of this constraint as one reading of the UNCLOS maritime sovereignty kernel and its implications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(uncl_tr_t5, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(uncl_tr_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(uncl_tr_t15, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(uncl_tr_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(uncl_tr_t25, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement(uncl_tr_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(uncl_be_t5, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(uncl_be_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(uncl_be_t15, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(uncl_be_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(uncl_be_t25, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(uncl_be_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 30, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(uncl_su_t5, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(uncl_su_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(uncl_su_t15, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(uncl_su_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(uncl_su_t25, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 25, 0.75).
narrative_ontology:measurement(uncl_su_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 30, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__hybrid_effective_control_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, freedom_of_navigation_operations).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, south_china_sea_arbitration_rulings).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the UNCLOS maritime sovereignty kernel. Its hybrid approach influences both stricter and more expansive interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
