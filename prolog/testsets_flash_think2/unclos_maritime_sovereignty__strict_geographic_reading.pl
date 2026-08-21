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
 *   constraint_id: unclos_maritime_sovereignty__strict_geographic_reading
 *   human_readable: UNCLOS Strict Geographic Definition of Islands
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint represents the 'strict geographic reading' of UNCLOS
 *   Article 121, which defines what constitutes an island capable of
 *   generating maritime zones (territorial sea, EEZ). This reading holds that
 *   only naturally formed features above water at high tide qualify, and
 *   artificial construction does not alter legal status. It is a contested
 *   interpretation, actively enforced by naval powers and supported by
 *   non-claimant states, against the expansive claims of some coastal states
 *   that build artificial islands.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, 0.25).
domain_priors:suppression_score(unclos_maritime_sovereignty__strict_geographic_reading, 0.65).
domain_priors:theater_ratio(unclos_maritime_sovereignty__strict_geographic_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__strict_geographic_reading, rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__strict_geographic_reading, "UNCLOS Strict Geographic Definition of Islands").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__strict_geographic_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__strict_geographic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__strict_geographic_reading, 'cc158c9e-1259-4d1b-9b8b-69970701076d').
narrative_ontology:cs_kernel_codification('cc158c9e-1259-4d1b-9b8b-69970701076d', fixed_text).
narrative_ontology:cs_authority_grounding('cc158c9e-1259-4d1b-9b8b-69970701076d', lineage).
narrative_ontology:cs_interpretation_layer_present('cc158c9e-1259-4d1b-9b8b-69970701076d').
narrative_ontology:cs_reading_relation('cc158c9e-1259-4d1b-9b8b-69970701076d', unclos_maritime_sovereignty__expansive_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('cc158c9e-1259-4d1b-9b8b-69970701076d', unclos_maritime_sovereignty__hybrid_effective_control_reading, forecloses).
narrative_ontology:cs_axiom('cc158c9e-1259-4d1b-9b8b-69970701076d', foundational, maritime_features_must_be_natural).
narrative_ontology:cs_axiom_status(maritime_features_must_be_natural, holdable).
narrative_ontology:cs_axiom_grounding('cc158c9e-1259-4d1b-9b8b-69970701076d', maritime_features_must_be_natural, conventional).
narrative_ontology:cs_axiom('cc158c9e-1259-4d1b-9b8b-69970701076d', foundational, artificial_structures_are_installations_not_islands).
narrative_ontology:cs_axiom_status(artificial_structures_are_installations_not_islands, holdable).
narrative_ontology:cs_axiom_grounding('cc158c9e-1259-4d1b-9b8b-69970701076d', artificial_structures_are_installations_not_islands, conventional).
narrative_ontology:cs_reference_frame('cc158c9e-1259-4d1b-9b8b-69970701076d', geographic_objectivity_principle).
narrative_ontology:cs_drift_state('cc158c9e-1259-4d1b-9b8b-69970701076d', contemporary_south_china_sea_disputes, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('cc158c9e-1259-4d1b-9b8b-69970701076d', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_states).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, freedom_of_navigation_advocates).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, artificial_island_builders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from clear, limited maritime claims that ensure freedom of navigation and overflight in international waters. They actively conduct freedom of navigation operations (FONOPs) to challenge expansive claims based on artificial features.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers, beneficiary,
    institutional, generational, arbitrage, global).

% Benefit from the stability and predictability of maritime boundaries, which prevents unilateral appropriation of global commons and protects their access to resources and trade routes. They support the strict interpretation in international forums.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_states, beneficiary,
    moderate, biographical, mobile, global).

% Bear the cost of having their claims to territorial sea and EEZ around artificial features rejected. They actively construct artificial islands and assert sovereignty, pushing for an expansive interpretation of UNCLOS. Their exit is abandoning these claims, which is politically costly.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states, payer,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states, agenda_setter).

% Advocate for the principle of freedom of navigation and the global commons, opposing any interpretation of international law that would allow states to enclose vast maritime areas through artificial construction. They provide legal and policy arguments supporting the strict reading.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, freedom_of_navigation_advocates, beneficiary,
    organized, generational, analytical, global).

% Interpret and apply UNCLOS, including the definitions of islands and other maritime features. Their rulings reinforce or challenge specific readings of the convention, providing a formal mechanism for dispute resolution.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, international_tribunals, agenda_setter,
    institutional, generational, analytical, global).

% Are often state-owned or state-directed entities that construct artificial features for geopolitical purposes. They bear the cost of international condemnation and legal challenges when these constructions are not recognized as generating maritime zones. Their exit is ceasing construction, which is against state policy.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, artificial_island_builders, payer,
    powerful, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__strict_geographic_reading, diffuse).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__strict_geographic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes clear, objective criteria for classifying maritime features (islands vs. installations) to prevent arbitrary claims, ensure legal predictability, and maintain freedom of navigation in international waters.
% TRANSFER_FUNCTION: Limits the unilateral transfer of maritime space (territorial sea, EEZ) from the global commons to coastal states, preventing the enclosure of international waters based on artificial construction.
% ABSENT_VOICES: States that benefit from ambiguity or lack the capacity to challenge expansive claims might implicitly prefer a less strict interpretation, as it allows for greater flexibility in their own assertions without direct confrontation. They are not actively shaping this reading but are affected by its enforcement.
% DISAPPEARANCE_RATIONALE: If this strict reading vanished, coastal states would aggressively expand claims based on artificial structures, leading to increased maritime disputes, restricted navigation, and resource conflicts. The global maritime order would become significantly more unstable and contested.
% FOUNDING_PROBLEM: Ambiguity in international law regarding the legal status of artificial structures and low-tide elevations, leading to potential for unilateral expansion of maritime claims and encroachment on the global commons.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, non-claimant states, and naval powers consistently highlight the ongoing disputes (e.g., in the South China Sea) and the need for clear definitions, corroborating the problem's persistence and the continued relevance of this strict interpretation.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__strict_geographic_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__strict_geographic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__strict_geographic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(unclos_maritime_sovereignty__strict_geographic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__strict_geographic_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).
:- end_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because this reading primarily *prevents* extraction from the global commons, rather than performing it. Suppression is moderate-high (0.65) because it requires active enforcement (e.g., FONOPs, diplomatic protests, legal challenges) to counter states attempting to establish de facto sovereignty over artificial features. Resistance is high (0.8) due to the ongoing geopolitical contestation, particularly in regions like the South China Sea. Theater ratio is low (0.1) as the constraint is a legal principle, not a performance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of naval powers, this is a clear, objective rule essential for global maritime order. From the perspective of expansionist coastal states, it is an unfair limitation on their sovereign rights and development efforts, designed to maintain the status quo of existing powers. The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Naval powers and non-claimant states are beneficiaries (low d) as this reading preserves their freedom of navigation and access to the global commons. Expansionist coastal states and artificial island builders are targets (high d) as their efforts to expand maritime claims are directly curtailed by this interpretation. International tribunals act as agenda-setters, interpreting and applying the law.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_enhanced_feature_ambiguity,
    'What constitutes a ''naturally formed'' feature, especially when human activity enhances or stabilizes a naturally occurring low-tide elevation?',
    'Further international legal clarification or a definitive ruling by an international tribunal on specific cases involving enhanced natural features.',
    'If ''enhancement'' is deemed to alter the ''natural'' status, it could narrow the scope of features generating maritime zones; if it''s considered permissible, it could slightly broaden it, but still within the natural-origin principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_enhanced_feature_ambiguity, conceptual, 'Ambiguity in the definition of ''naturally formed'' features.').

omega_variable(
    enforcement_effectiveness_vs_power,
    'How effective is the enforcement of this strict reading against powerful states that continue to assert expansive claims despite international legal challenges?',
    'Longitudinal study of compliance rates and the impact of FONOPs and diplomatic pressure on state behavior over decades.',
    'If enforcement is consistently ineffective against powerful states, the de facto status of artificial features might drift towards recognition, weakening the constraint''s practical application and potentially shifting its classification towards a more extractive type for the global commons.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_effectiveness_vs_power, empirical, 'Effectiveness of enforcement against powerful non-compliant states.').

omega_variable(
    customary_international_law_drift,
    'Could a consistent, unchallenged practice of asserting maritime zones around artificial features by a sufficient number of states eventually lead to a new customary international law that overrides this strict reading?',
    'Analysis of state practice (usus) and belief in legal obligation (opinio juris) over a prolonged period, as assessed by international legal scholars and tribunals.',
    'If a new customary law emerges, this strict reading would be superseded, and the constraint would effectively disappear or be reclassified as a ''piton'' if only theatrically maintained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(customary_international_law_drift, conceptual, 'Potential for customary international law to override the treaty-based strict reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__strict_geographic_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(uncl_tr_t1990, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(uncl_tr_t1998, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 1998, 0.1).
narrative_ontology:measurement(uncl_tr_t2006, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2006, 0.1).
narrative_ontology:measurement(uncl_tr_t2014, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement(uncl_tr_t2024, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 1982, 0.2).
narrative_ontology:measurement(uncl_be_t1990, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 1990, 0.22).
narrative_ontology:measurement(uncl_be_t1998, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 1998, 0.23).
narrative_ontology:measurement(uncl_be_t2006, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2006, 0.24).
narrative_ontology:measurement(uncl_be_t2014, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2014, 0.25).
narrative_ontology:measurement(uncl_be_t2024, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 1982, 0.5).
narrative_ontology:measurement(uncl_su_t1990, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(uncl_su_t1998, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 1998, 0.6).
narrative_ontology:measurement(uncl_su_t2006, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2006, 0.63).
narrative_ontology:measurement(uncl_su_t2014, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2014, 0.65).
narrative_ontology:measurement(uncl_su_t2024, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__strict_geographic_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
