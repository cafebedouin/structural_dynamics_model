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
 *   domain: international_law/maritime_governance/geopolitics
 *
 * SUMMARY:
 *   This constraint represents the strict geographic reading of UNCLOS
 *   Article 121, which dictates that only naturally formed features above
 *   water at high tide qualify as islands generating territorial sea and
 *   Exclusive Economic Zone (EEZ). Artificial constructions or features
 *   submerged at high tide do not alter legal status. This reading is crucial
 *   for maintaining the integrity of international waters and preventing
 *   arbitrary expansion of national sovereignty, particularly in contested
 *   regions like the South China Sea. It is one reading of the broader
 *   'unclos_maritime_sovereignty' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, 0.7).
domain_priors:suppression_score(unclos_maritime_sovereignty__strict_geographic_reading, 0.6).
domain_priors:theater_ratio(unclos_maritime_sovereignty__strict_geographic_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__strict_geographic_reading, rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__strict_geographic_reading, "UNCLOS Strict Geographic Definition of Islands").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__strict_geographic_reading, "international_law/maritime_governance/geopolitics").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__strict_geographic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__strict_geographic_reading, '4bc924e8-3364-481a-83e1-5b10e9d4fbe0').
narrative_ontology:cs_kernel_codification('4bc924e8-3364-481a-83e1-5b10e9d4fbe0', fixed_text).
narrative_ontology:cs_authority_grounding('4bc924e8-3364-481a-83e1-5b10e9d4fbe0', lineage).
narrative_ontology:cs_interpretation_layer_present('4bc924e8-3364-481a-83e1-5b10e9d4fbe0').
narrative_ontology:cs_reading_relation('4bc924e8-3364-481a-83e1-5b10e9d4fbe0', unclos_maritime_sovereignty__expansive_construction_reading, coexists_with).
narrative_ontology:cs_reading_relation('4bc924e8-3364-481a-83e1-5b10e9d4fbe0', unclos_maritime_sovereignty__hybrid_effective_control_reading, coexists_with).
narrative_ontology:cs_axiom('4bc924e8-3364-481a-83e1-5b10e9d4fbe0', foundational, geographic_objectivity_principle).
narrative_ontology:cs_axiom_status(geographic_objectivity_principle, holdable).
narrative_ontology:cs_axiom_grounding('4bc924e8-3364-481a-83e1-5b10e9d4fbe0', geographic_objectivity_principle, deontological).
narrative_ontology:cs_axiom('4bc924e8-3364-481a-83e1-5b10e9d4fbe0', foundational, non_alteration_by_artifice).
narrative_ontology:cs_axiom_status(non_alteration_by_artifice, holdable).
narrative_ontology:cs_axiom_grounding('4bc924e8-3364-481a-83e1-5b10e9d4fbe0', non_alteration_by_artifice, deontological).
narrative_ontology:cs_reference_frame('4bc924e8-3364-481a-83e1-5b10e9d4fbe0', unclos_1982_framework).
narrative_ontology:cs_drift_state('4bc924e8-3364-481a-83e1-5b10e9d4fbe0', contemporary_south_china_sea_disputes, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('4bc924e8-3364-481a-83e1-5b10e9d4fbe0', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_states).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, international_shipping).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from clear, stable maritime boundaries and freedom of navigation, which this strict reading of UNCLOS upholds by limiting arbitrary claims over vast sea areas. They actively advocate for and enforce this interpretation.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers, beneficiary,
    institutional, civilizational, arbitrage, global).

% Benefit from the preservation of international waters and the prevention of other states' expansive claims, ensuring equitable access to global commons and stable geopolitical order. They support this reading in international forums.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_states, beneficiary,
    organized, generational, mobile, global).

% Relies on predictable and open sea lanes. This reading prevents the proliferation of territorial claims that could impede navigation, thus reducing transit costs and geopolitical risks.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, international_shipping, beneficiary,
    organized, biographical, mobile, global).

% Bear the cost of denied claims to vast maritime zones (territorial sea, EEZ) that they seek to generate from artificial constructions or low-tide elevations. They actively challenge this interpretation through construction and assertion of de facto control.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states, payer,
    institutional, generational, constrained, regional).

% Are tasked with interpreting and applying UNCLOS. They uphold this strict geographic reading in their rulings, providing legal clarity and reinforcing the constraint, despite political pressures.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, international_tribunals, agenda_setter,
    institutional, civilizational, analytical, global).

% Analyze the legal implications and historical precedents of UNCLOS interpretations. Many support the strict geographic reading as consistent with the treaty's original intent and the principle of freedom of the seas.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, maritime_law_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__strict_geographic_reading, diffuse).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__strict_geographic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes clear, objective, and universally applicable criteria for what constitutes an 'island' capable of generating maritime zones, thereby preventing arbitrary claims and ensuring predictable boundaries for international navigation and resource management.
% TRANSFER_FUNCTION: Prevents the transfer of vast maritime areas (territorial sea, EEZ) from the global commons or other states to those constructing artificial features, effectively preserving these areas as international waters or for other legitimate claimants.
% ABSENT_VOICES: States or entities that might develop new technologies for extensive deep-sea construction, or future generations facing extreme resource scarcity who might seek to expand claims via artificial means, are not directly represented in the current UNCLOS framework and would likely object to this strict limitation.
% DISAPPEARANCE_RATIONALE: If this strict definition vanished overnight, states would rapidly construct artificial features on submerged reefs or low-tide elevations to claim vast maritime territories. This would lead to widespread, intense geopolitical disputes, conflicts over resources, severely restricted freedom of navigation, and a fundamental reorganization of global maritime order.
% FOUNDING_PROBLEM: Ambiguity and potential for arbitrary, expansive claims over maritime space, leading to conflict, hindering international navigation, and jeopardizing equitable resource exploitation in the absence of clear, objective criteria.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, major naval powers, and non-claimant states consistently corroborate the ongoing need for clear, objective maritime boundaries to prevent conflict and ensure the global commons. This is evidenced by ongoing disputes and diplomatic efforts to uphold UNCLOS principles.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__strict_geographic_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__strict_geographic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__strict_geographic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(unclos_maritime_sovereignty__strict_geographic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__strict_geographic_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.70) is high from the perspective of expansionist coastal states, as it denies them the ability to claim vast maritime zones based on artificial constructions. Suppression (0.60) is moderate, relying on international legal pressure, diplomatic protest, and potential naval enforcement rather than direct physical coercion. Resistance (0.75) is high, as several states actively challenge this interpretation through construction and assertion of de facto control. Accessibility collapse (0.85) is high because, legally, the alternative of claiming artificial islands as full-status islands is largely foreclosed. Theater ratio (0.10) is low, as the principle is a fundamental legal tenet, not a performative one.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of naval powers and non-claimant states, this constraint is a vital 'rope' for global coordination and stability, preventing conflict and ensuring the global commons. From the perspective of expansionist coastal states, it is an 'snare' or 'tangled_rope' that unfairly limits their sovereign rights and development ambitions, imposing significant costs by denying access to resources and strategic space. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Naval powers, non-claimant states, and international shipping are beneficiaries (low directionality) as they gain from stable, predictable maritime boundaries and freedom of navigation. Expansionist coastal states are clear targets (high directionality) as their claims to expanded maritime zones are denied by this reading. International tribunals act as agenda-setters, interpreting and enforcing the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_feature_ambiguity,
    'What constitutes a ''naturally formed'' feature, especially in cases of human enhancement or restoration of partially submerged features?',
    'Further international legal clarification or specific rulings by international tribunals on hybrid features, establishing a clear threshold for ''naturalness''.',
    'If ''natural'' is interpreted more broadly, it could slightly expand the scope for claims, reducing the constraint''s extractiveness for some states. If interpreted more narrowly, it reinforces the strict reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_feature_ambiguity, conceptual, 'Ambiguity in the definition of ''naturally formed'' features.').

omega_variable(
    de_facto_control_vs_legal_status,
    'Does prolonged, unchallenged de facto control and administration of an artificial feature eventually alter its legal status, despite the strict UNCLOS reading?',
    'A landmark international legal case where a state successfully asserts territorial rights over a long-held artificial feature, or a shift in customary international law through widespread state practice.',
    'If de facto control gains legal recognition, the strict geographic reading''s suppression and extractiveness would diminish, potentially shifting its classification towards a ''tangled_rope'' or ''snare'' for non-claimant states.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(de_facto_control_vs_legal_status, empirical, 'Tension between strict legal status and de facto control over time.').


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
narrative_ontology:measurement(uncl_be_t1982, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 1982, 0.55).
narrative_ontology:measurement(uncl_be_t1990, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(uncl_be_t1998, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 1998, 0.65).
narrative_ontology:measurement(uncl_be_t2006, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2006, 0.68).
narrative_ontology:measurement(uncl_be_t2014, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2014, 0.7).
narrative_ontology:measurement(uncl_be_t2024, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 1982, 0.45).
narrative_ontology:measurement(uncl_su_t1990, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(uncl_su_t1998, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 1998, 0.55).
narrative_ontology:measurement(uncl_su_t2006, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2006, 0.58).
narrative_ontology:measurement(uncl_su_t2014, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2014, 0.6).
narrative_ontology:measurement(uncl_su_t2024, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__strict_geographic_reading, global_infrastructure).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__hybrid_effective_control_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'unclos_maritime_sovereignty' kernel, each representing a distinct interpretation of island status and its implications for maritime claims. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
