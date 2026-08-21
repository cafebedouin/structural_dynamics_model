% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__historical_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__historical_rights_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__historical_rights_reading
 *   human_readable: Historical Rights Override UNCLOS EEZ
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint represents the reading that historical usage and
 *   occupation create sovereign rights that predate and override UNCLOS EEZ
 *   provisions. It is a contested claim, primarily advanced by certain states
 *   to justify expansive maritime claims in areas like the South China Sea.
 *   This reading directly challenges the UNCLOS framework, leading to
 *   increased extractiveness and suppression for coastal states and
 *   international actors whose activities are constrained by these historical
 *   claims. The claimed type is 'tangled_rope' because it coordinates the
 *   interests of claimant states while extracting from others through active
 *   enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, 0.7).
domain_priors:suppression_score(unclos_sovereignty_boundary__historical_rights_reading, 0.65).
domain_priors:theater_ratio(unclos_sovereignty_boundary__historical_rights_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__historical_rights_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__historical_rights_reading, "Historical Rights Override UNCLOS EEZ").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__historical_rights_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__historical_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__historical_rights_reading, '0a41d64d-bd33-458e-b625-2637816c0711').
narrative_ontology:cs_kernel_codification('0a41d64d-bd33-458e-b625-2637816c0711', implicit).
narrative_ontology:cs_authority_grounding('0a41d64d-bd33-458e-b625-2637816c0711', extraction).
narrative_ontology:cs_interpretation_layer_present('0a41d64d-bd33-458e-b625-2637816c0711').
narrative_ontology:cs_reading_relation('0a41d64d-bd33-458e-b625-2637816c0711', unclos_sovereignty_boundary__strict_eez_reading, forecloses).
narrative_ontology:cs_reading_relation('0a41d64d-bd33-458e-b625-2637816c0711', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coexists_with).
narrative_ontology:cs_axiom('0a41d64d-bd33-458e-b625-2637816c0711', foundational, historical_sovereignty_precedes_treaty_law).
narrative_ontology:cs_axiom_status(historical_sovereignty_precedes_treaty_law, holdable).
narrative_ontology:cs_axiom_grounding('0a41d64d-bd33-458e-b625-2637816c0711', historical_sovereignty_precedes_treaty_law, deontological).
narrative_ontology:cs_axiom('0a41d64d-bd33-458e-b625-2637816c0711', foundational, effective_occupation_confers_rights).
narrative_ontology:cs_axiom_status(effective_occupation_confers_rights, holdable).
narrative_ontology:cs_axiom_grounding('0a41d64d-bd33-458e-b625-2637816c0711', effective_occupation_confers_rights, conventional).
narrative_ontology:cs_reference_frame('0a41d64d-bd33-458e-b625-2637816c0711', pre_unclos_customary_maritime_law).
narrative_ontology:cs_drift_state('0a41d64d-bd33-458e-b625-2637816c0711', contemporary_unclos_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('0a41d64d-bd33-458e-b625-2637816c0711', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, international_shipping_companies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States that assert historical rights to maritime areas, often based on ancient maps, traditional fishing grounds, or early exploration. They actively enforce these claims through naval patrols, resource extraction, and administrative declarations, challenging UNCLOS-defined EEZ boundaries. They benefit from expanded resource access and strategic control.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states, agenda_setter,
    institutional, generational, constrained, regional).

% States whose UNCLOS-defined Exclusive Economic Zones are encroached upon by historical claims. They bear the cost of lost resource control, increased security risks, and diplomatic friction. Their options are diplomatic protest, legal challenge (often ignored by claimants), or military deterrence.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states, payer,
    institutional, generational, constrained, regional).

% Companies operating vessels through contested waters. They face increased transit costs, insurance premiums, and risks of harassment or detention by claimant states. Their operations are constrained by ambiguous sovereignty claims, forcing them to navigate complex geopolitical risks.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, international_shipping_companies, payer,
    organized, immediate, constrained, global).

% Analyze the legal validity of historical claims against modern international law, particularly UNCLOS. They provide academic commentary and legal opinions, influencing diplomatic discourse but lacking direct enforcement power.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% The administrative body for UNCLOS, tasked with promoting its uniform application. It is structurally excluded from directly adjudicating or enforcing against states that reject UNCLOS provisions in favor of historical claims, as its authority is derived from the very treaty being challenged.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, unclos_secretariat, excluded,
    institutional, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: For claimant states, it coordinates their historical narratives and national interests into a unified, expansive maritime claim, providing a framework for resource exploitation and strategic projection.
% TRANSFER_FUNCTION: Transfers control over maritime resources (fishing, oil, gas) and strategic sea lanes from UNCLOS-defined EEZ holders to states asserting historical rights, often through unilateral action and naval presence.
% ABSENT_VOICES: The UNCLOS Secretariat and international tribunals, whose authority is derived from the treaty being challenged, are effectively sidelined. They would argue for the primacy of UNCLOS and the peaceful resolution of disputes, but their mechanisms are often rejected by claimant states.
% DISAPPEARANCE_RATIONALE: If the claim of historical rights overriding UNCLOS vanished, claimant states would lose their legal justification for expansive maritime control, leading to a re-assertion of UNCLOS-defined EEZ boundaries. Resource allocation and naval presence would shift dramatically, reorganizing regional maritime governance.
% FOUNDING_PROBLEM: To assert national sovereignty and secure access to vital maritime resources and strategic sea lanes based on long-standing historical presence and traditional activities, predating modern international legal frameworks.
% FOUNDING_PROBLEM_CORROBORATION: Claimant states consistently assert the problem is live, citing historical maps and traditional usage. EEZ-holding coastal states and international legal scholars acknowledge the historical claims exist but dispute their contemporary legal validity, arguing the problem is one of interpretation and power projection, not a genuine gap in international law.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__historical_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__historical_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__historical_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unclos_sovereignty_boundary__historical_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__historical_rights_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.7) because claimant states gain significant control over resources and strategic areas, often at the expense of other nations' UNCLOS-defined rights. Suppression is also high (0.65) due to the active enforcement mechanisms (naval patrols, administrative decrees) used to assert these claims and deter challenges. Resistance is substantial (0.75) from affected coastal states and international bodies. The theater ratio is low (0.2) as the claims are genuinely pursued and enforced, not merely performative. Accessibility collapse is moderate (0.4) as legal and diplomatic alternatives exist but are often ineffective against determined claimant states.
 *
 * PERSPECTIVAL GAP:
 *   Claimant states perceive this as a legitimate assertion of long-standing rights, a 'rope' coordinating their historical claims. Coastal states and international shipping companies experience it as a 'snare' of unilateral extraction and suppression. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Expansive claimant states are clear beneficiaries and agenda-setters, actively shaping and enforcing the constraint for their benefit. EEZ-holding coastal states and international shipping companies are victims, bearing the costs of lost access and increased operational risks. International legal scholars act as observers, analyzing the situation without direct involvement in enforcement or extraction. The UNCLOS Secretariat is excluded, as its authority is undermined by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_evidence_validity,
    'To what extent do the historical claims (maps, usage) meet contemporary international legal standards for establishing sovereign rights?',
    'Adjudication by an international court (e.g., ITLOS, ICJ) if all parties agree to jurisdiction, or a comprehensive, independent historical and legal review.',
    'If historical evidence is deemed insufficient, the claim''s legitimacy collapses, reducing extractiveness and suppression. If deemed valid, it could reshape interpretations of UNCLOS.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_evidence_validity, empirical, 'The empirical and legal strength of historical claims.').

omega_variable(
    unclos_primacy_vs_customary_law,
    'Does UNCLOS, as a codified treaty, supersede customary international law regarding historical maritime claims, or do such claims retain independent legal force?',
    'A definitive ruling by a universally recognized international legal body on the hierarchy of sources of international law in this specific context, or a new global consensus among states.',
    'If UNCLOS is deemed primary, this reading''s legitimacy is severely undermined. If customary law is held to override, this reading gains significant legal weight, increasing its extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unclos_primacy_vs_customary_law, conceptual, 'The conceptual hierarchy between treaty law (UNCLOS) and customary international law.').

omega_variable(
    enforcement_legitimacy,
    'Is the active enforcement of historical claims by claimant states considered legitimate self-defense of sovereign rights, or an illegal act of aggression/coercion under international law?',
    'A UN Security Council resolution, a General Assembly declaration, or a series of consistent rulings by international tribunals on specific enforcement actions.',
    'If enforcement is deemed illegitimate, the suppression metric would be re-evaluated as pure coercion, increasing the ''snare'' characteristics. If legitimate, it reinforces the ''tangled_rope'' aspect of coordinated assertion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_legitimacy, preference, 'The international community''s normative judgment on the legitimacy of enforcement actions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__historical_rights_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(uncl_tr_t1995, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(uncl_tr_t2005, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(uncl_tr_t2015, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(uncl_tr_t2024, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 1982, 0.4).
narrative_ontology:measurement(uncl_be_t1995, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 1995, 0.5).
narrative_ontology:measurement(uncl_be_t2005, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(uncl_be_t2015, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(uncl_be_t2024, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 1982, 0.3).
narrative_ontology:measurement(uncl_su_t1995, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 1995, 0.45).
narrative_ontology:measurement(uncl_su_t2005, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2005, 0.55).
narrative_ontology:measurement(uncl_su_t2015, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2015, 0.62).
narrative_ontology:measurement(uncl_su_t2024, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__historical_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'unclos_sovereignty_boundary' kernel. This reading asserts historical rights override UNCLOS EEZ provisions, directly influencing the 'strict_eez_reading' by challenging its exclusivity and the 'non_ratifier_enforcement_reading' by introducing a competing basis for maritime claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
