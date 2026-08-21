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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: UNCLOS Strict Geographic Reading of Island Status
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint represents the 'strict geographic' reading of UNCLOS
 *   Article 121, which dictates that only naturally formed features above
 *   water at high tide qualify as islands generating territorial sea and EEZ.
 *   Artificial constructions, regardless of their size or permanence, do not
 *   alter legal status. This reading is favored by naval powers and
 *   non-claimant states to limit expansive territorial claims and preserve
 *   freedom of navigation. It is contested by states seeking to expand their
 *   maritime zones through artificial island building.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, 0.15).
domain_priors:suppression_score(unclos_maritime_sovereignty__strict_geographic_reading, 0.3).
domain_priors:theater_ratio(unclos_maritime_sovereignty__strict_geographic_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__strict_geographic_reading, mountain).
narrative_ontology:human_readable(unclos_maritime_sovereignty__strict_geographic_reading, "UNCLOS Strict Geographic Reading of Island Status").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__strict_geographic_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:emerges_naturally(unclos_maritime_sovereignty__strict_geographic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__strict_geographic_reading, 'f09d00d4-ddc4-4dca-b6d0-4cbce974c6d3').
narrative_ontology:cs_kernel_codification('f09d00d4-ddc4-4dca-b6d0-4cbce974c6d3', fixed_text).
narrative_ontology:cs_authority_grounding('f09d00d4-ddc4-4dca-b6d0-4cbce974c6d3', lineage).
narrative_ontology:cs_interpretation_layer_present('f09d00d4-ddc4-4dca-b6d0-4cbce974c6d3').
narrative_ontology:cs_reading_relation('f09d00d4-ddc4-4dca-b6d0-4cbce974c6d3', unclos_maritime_sovereignty__expansive_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('f09d00d4-ddc4-4dca-b6d0-4cbce974c6d3', unclos_maritime_sovereignty__hybrid_effective_control_reading, forecloses).
narrative_ontology:cs_axiom('f09d00d4-ddc4-4dca-b6d0-4cbce974c6d3', foundational, natural_formation_is_sine_qua_non).
narrative_ontology:cs_axiom_status(natural_formation_is_sine_qua_non, holdable).
narrative_ontology:cs_axiom_grounding('f09d00d4-ddc4-4dca-b6d0-4cbce974c6d3', natural_formation_is_sine_qua_non, deontological).
narrative_ontology:cs_axiom('f09d00d4-ddc4-4dca-b6d0-4cbce974c6d3', foundational, artificial_construction_is_irrelevant_to_status).
narrative_ontology:cs_axiom_status(artificial_construction_is_irrelevant_to_status, holdable).
narrative_ontology:cs_axiom_grounding('f09d00d4-ddc4-4dca-b6d0-4cbce974c6d3', artificial_construction_is_irrelevant_to_status, deontological).
narrative_ontology:cs_reference_frame('f09d00d4-ddc4-4dca-b6d0-4cbce974c6d3', unclos_original_intent_1982).
narrative_ontology:cs_drift_state('f09d00d4-ddc4-4dca-b6d0-4cbce974c6d3', contemporary_artificial_island_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f09d00d4-ddc4-4dca-b6d0-4cbce974c6d3', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, international_shipping_industry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a clear, narrow definition of island status, which limits the territorial claims of coastal states and preserves freedom of navigation in international waters. They leverage this reading to challenge expansive claims.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers, beneficiary,
    institutional, generational, arbitrage, global).

% Benefit from the stability and predictability of maritime boundaries, avoiding disputes over artificially extended zones. They align with the strict reading to maintain the status quo.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_states, beneficiary,
    moderate, generational, mobile, global).

% Bear the cost of this reading by being prevented from extending their territorial sea and EEZ claims through artificial island construction. They actively challenge this interpretation to legitimize their constructions.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states, payer,
    powerful, generational, constrained, regional).

% Interpret and apply UNCLOS provisions, including those related to island status. Their rulings reinforce or challenge specific readings, shaping the legal landscape.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, international_maritime_tribunals, agenda_setter,
    institutional, generational, analytical, global).

% Benefits from clear and stable international waters, reducing transit costs and legal uncertainties. Supports interpretations that limit coastal state jurisdiction.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, international_shipping_industry, beneficiary,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, universally applicable rule for determining which land features generate maritime zones, thereby coordinating international expectations regarding sovereignty and freedom of navigation.
% TRANSFER_FUNCTION: Transfers potential territorial claims and resource access from states that construct artificial features to the international community, preserving common heritage areas and freedom of the seas.
% ABSENT_VOICES: States with limited natural island features, who might otherwise seek to expand their maritime claims through artificial construction, are effectively marginalized in the interpretive process, as their actions are deemed legally irrelevant by this reading.
% DISAPPEARANCE_RATIONALE: If this strict reading vanished, there would be immediate and widespread reinterpretation of maritime boundaries, leading to increased territorial disputes, challenges to freedom of navigation, and a scramble for resource claims around artificial features. The stability of the international maritime order would be severely disrupted.
% FOUNDING_PROBLEM: The need for a clear, universally accepted legal framework to define maritime zones and prevent unilateral expansion of sovereignty, particularly in light of technological advancements allowing for artificial construction.
% FOUNDING_PROBLEM_CORROBORATION: Naval powers, international maritime tribunals, and the international shipping industry consistently corroborate the ongoing need for clear definitions to prevent conflict and ensure freedom of navigation. Independent legal scholars also support the necessity of such a framework.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__strict_geographic_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__strict_geographic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__strict_geographic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unclos_maritime_sovereignty__strict_geographic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__strict_geographic_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is low (0.15) because this reading primarily prevents extraction by others, rather than performing it directly. Suppression is moderate (0.3) as it requires diplomatic and legal challenges to states attempting to assert sovereignty over artificial features. Theater ratio is low (0.1) as the legal principle is generally applied directly, with little performative maintenance. Accessibility collapse is high (0.85) because, within this legal framework, there are virtually no legitimate alternatives for generating maritime zones from artificial features. Resistance is low (0.1) because while some states challenge it, the principle is widely accepted in international law.
 *
 * PERSPECTIVAL GAP:
 *   Naval powers and non-claimant states perceive this as a foundational 'mountain' of international law, ensuring stability and freedom. Expansionist coastal states, however, experience it as a 'snare' that unjustly limits their sovereign rights and development ambitions, forcing them to bear the costs of non-recognition for their artificial features.
 *
 * DIRECTIONALITY LOGIC:
 *   Naval powers and non-claimant states are beneficiaries (d near 0.0) as this reading protects their interests in open seas. Expansionist coastal states are victims (d near 1.0) as it directly curtails their ability to expand maritime claims. International maritime tribunals act as agenda-setters, interpreting and applying the law, thus influencing the constraint's practical effect.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mandatrophy by continuously re-asserting the original intent of UNCLOS regarding natural features, thereby resisting attempts to redefine 'island' based on technological capabilities or de facto control. It ensures the constraint's mandate (preventing unilateral expansion) remains live and functional, rather than atrophying into a mere formality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_artificial_ambiguity,
    'Is the distinction between ''natural'' and ''artificial'' features sufficiently clear and robust to withstand future technological advancements in construction?',
    'Emergence of new construction techniques that blur the line between natural accretion and artificial creation, leading to new legal challenges and interpretations.',
    'If the distinction becomes unworkable, this strict reading could collapse, leading to a more expansive or hybrid interpretation of island status, increasing extractiveness for naval powers and non-claimant states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_artificial_ambiguity, empirical, 'The clarity and future resilience of the natural/artificial distinction.').

omega_variable(
    effective_control_precedent,
    'To what extent does prolonged, unchallenged effective control over an artificial feature create a de facto precedent that erodes the strict geographic reading?',
    'A future international tribunal ruling that grants limited maritime rights to a state based on long-term, unchallenged administration of an artificial feature.',
    'Such a precedent would significantly weaken the strict geographic reading, shifting the constraint towards a ''hybrid_effective_control_reading'' and increasing extractiveness for naval powers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effective_control_precedent, empirical, 'The potential for de facto control to create legal precedent.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''unclos_maritime_sovereignty'' kernel. This specific reading is the ''strict_geographic_reading''. What would change if a sibling reading were adopted?',
    'Analysis of international legal precedent and state practice under alternative interpretations.',
    'If the ''expansive_construction_reading'' were adopted, artificial features could generate full maritime zones, significantly increasing extractiveness for naval powers. If the ''hybrid_effective_control_reading'' were adopted, limited zones might be recognized, leading to a more nuanced but still more expansive outcome than this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identification of this constraint as a specific reading of the UNCLOS maritime sovereignty kernel and the implications of alternative readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__strict_geographic_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 1982, 0.05).
narrative_ontology:measurement(uncl_tr_t1995, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 1995, 0.07).
narrative_ontology:measurement(uncl_tr_t2008, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2008, 0.09).
narrative_ontology:measurement(uncl_tr_t2024, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 1982, 0.1).
narrative_ontology:measurement(uncl_be_t1995, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 1995, 0.12).
narrative_ontology:measurement(uncl_be_t2008, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2008, 0.14).
narrative_ontology:measurement(uncl_be_t2024, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 1982, 0.25).
narrative_ontology:measurement(uncl_su_t1995, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 1995, 0.27).
narrative_ontology:measurement(uncl_su_t2008, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2008, 0.29).
narrative_ontology:measurement(uncl_su_t2024, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__strict_geographic_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__hybrid_effective_control_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
