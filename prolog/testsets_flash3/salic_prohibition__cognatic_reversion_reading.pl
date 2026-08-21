% ============================================================================
% CONSTRAINT STORY: salic_prohibition__cognatic_reversion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__cognatic_reversion_reading, []).

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
 *   constraint_id: salic_prohibition__cognatic_reversion_reading
 *   human_readable: Salic Law as Frankish Anachronism (Cognatic Reversion Reading)
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   This constraint represents the 'cognatic reversion' reading of Salic Law,
 *   which argues that the law was a specific Frankish custom, never
 *   universally binding, and that in non-Frankish territories, succession
 *   should revert to cognatic primogeniture (allowing female heirs) to ensure
 *   dynastic stability and territorial integrity. This reading emerged as a
 *   pragmatic solution to succession crises in composite monarchies,
 *   contrasting with more rigid interpretations of Salic Law.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, 0.3).
domain_priors:suppression_score(salic_prohibition__cognatic_reversion_reading, 0.4).
domain_priors:theater_ratio(salic_prohibition__cognatic_reversion_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__cognatic_reversion_reading, rope).
narrative_ontology:human_readable(salic_prohibition__cognatic_reversion_reading, "Salic Law as Frankish Anachronism (Cognatic Reversion Reading)").
narrative_ontology:topic_domain(salic_prohibition__cognatic_reversion_reading, "constitutional_law/dynastic_succession/political_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__cognatic_reversion_reading, '0234ff7e-a4d9-4542-b6bb-c0eea603b3d5').
narrative_ontology:cs_kernel_codification('0234ff7e-a4d9-4542-b6bb-c0eea603b3d5', fixed_text).
narrative_ontology:cs_authority_grounding('0234ff7e-a4d9-4542-b6bb-c0eea603b3d5', lineage).
narrative_ontology:cs_interpretation_layer_present('0234ff7e-a4d9-4542-b6bb-c0eea603b3d5').
narrative_ontology:cs_reading_relation('0234ff7e-a4d9-4542-b6bb-c0eea603b3d5', salic_prohibition__immutable_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('0234ff7e-a4d9-4542-b6bb-c0eea603b3d5', salic_prohibition__sovereign_override_reading, coexists_with).
narrative_ontology:cs_axiom('0234ff7e-a4d9-4542-b6bb-c0eea603b3d5', foundational, salic_law_is_frankish_custom_only).
narrative_ontology:cs_axiom_status(salic_law_is_frankish_custom_only, holdable).
narrative_ontology:cs_axiom_grounding('0234ff7e-a4d9-4542-b6bb-c0eea603b3d5', salic_law_is_frankish_custom_only, conventional).
narrative_ontology:cs_axiom('0234ff7e-a4d9-4542-b6bb-c0eea603b3d5', foundational, territorial_integrity_trumps_agnatic_purity).
narrative_ontology:cs_axiom_status(territorial_integrity_trumps_agnatic_purity, holdable).
narrative_ontology:cs_axiom_grounding('0234ff7e-a4d9-4542-b6bb-c0eea603b3d5', territorial_integrity_trumps_agnatic_purity, instrumental).
narrative_ontology:cs_reference_frame('0234ff7e-a4d9-4542-b6bb-c0eea603b3d5', historical_customary_law_diversity).
narrative_ontology:cs_drift_state('0234ff7e-a4d9-4542-b6bb-c0eea603b3d5', enlightenment_era_state_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0234ff7e-a4d9-4542-b6bb-c0eea603b3d5', '').
narrative_ontology:cs_kernel_id(salic_prohibition__cognatic_reversion_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, female_heirs).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, territorial_integrity_advocates).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, agnatic_succession_proponents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, non_frankish_territories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly benefit from the interpretation that allows female succession, securing their claim to the throne. Their position is strengthened by historical arguments against the universal applicability of Salic Law.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, female_heirs, beneficiary,
    moderate, generational, constrained, national).

% Support this reading as it prioritizes the stability and unity of the realm over strict adherence to an archaic succession rule, especially when a female heir might prevent a dynastic union or fragmentation.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, territorial_integrity_advocates, beneficiary,
    organized, generational, mobile, national).

% Bear the cost of this interpretation as it undermines their preferred male-only line of succession. They often represent traditionalist factions within the nobility or clergy who see female rule as illegitimate or weak.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, agnatic_succession_proponents, payer,
    powerful, generational, constrained, national).

% Analyze the historical origins and evolution of Salic Law, providing evidence for its limited original scope and its later misapplication. Their work informs the arguments for cognatic reversion.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, historical_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% Benefit from not being bound by a law historically alien to their customs, allowing them to maintain their own succession traditions or accept female rulers without legal contradiction.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, non_frankish_territories, beneficiary,
    moderate, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for dynastic succession that prioritizes territorial integrity and avoids succession crises by allowing female heirs in non-Frankish territories, thereby coordinating the transfer of power in a way that maintains state unity.
% TRANSFER_FUNCTION: Transfers the right of succession from a strict male-only line to a cognatic line (including females) in specific territories, thereby transferring political power and legitimacy.
% ABSENT_VOICES: The original Frankish tribal assemblies, whose customs formed the basis of Salic Law, are absent. They would likely object to any interpretation that diluted the agnatic principle, but their historical context is no longer directly relevant to the modern state.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal basis for female succession in many European monarchies would be undermined, potentially triggering severe dynastic crises, civil wars, and territorial fragmentation as agnatic claims resurfaced. The political landscape would be fundamentally reshaped.
% FOUNDING_PROBLEM: The problem of ensuring stable dynastic succession and preventing territorial fragmentation in diverse realms that had incorporated territories with different customary laws, where strict agnatic succession could lead to foreign rule or internal conflict.
% FOUNDING_PROBLEM_CORROBORATION: Historical legal precedents, constitutional scholars, and political historians corroborate that the problem of succession stability in composite monarchies was a persistent challenge, and that interpretations allowing female succession often served to preserve the realm's integrity. This is attested by independent academic research and historical state documents, not just by those who benefit from the reading.
narrative_ontology:disappearance_verdict(salic_prohibition__cognatic_reversion_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__cognatic_reversion_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__cognatic_reversion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(salic_prohibition__cognatic_reversion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__cognatic_reversion_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__cognatic_reversion_reading_tests).
:- end_tests(salic_prohibition__cognatic_reversion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate because while it benefits female heirs and state stability, it still imposes a 'cost' on traditional agnatic proponents who see their preferred order disrupted. Suppression is moderate as it requires active legal and political arguments to counter the entrenched 'immutable mandate' view. Theater ratio is low because the arguments for this reading are genuinely functional in resolving succession disputes, not merely performative. Accessibility collapse is moderate as alternatives (like strict agnatic succession) are still debated but often politically unfeasible. Resistance is moderate from traditionalist factions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of female heirs and state stability advocates, this is a pragmatic and just interpretation that resolves real problems. From the perspective of agnatic proponents, it is a deviation from fundamental law, an act of 'extraction' from the traditional order. The engine's classification will reflect these divergent experiences based on their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Female heirs and advocates for territorial integrity are beneficiaries, as this reading secures their claims or goals. Agnatic succession proponents are payers, as their preferred system is undermined. Historical legal scholars and non-Frankish territories are also beneficiaries, as the former provide the intellectual grounding and the latter gain autonomy from an alien law.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_applicability_ambiguity,
    'Was Salic Law ever genuinely considered universally binding across all territories of a composite monarchy, or was its application always contested and geographically limited?',
    'Further historical and legal research into the customary laws and succession practices of non-Frankish territories incorporated into larger realms, focusing on primary sources from the period of initial integration.',
    'If universal applicability is disproven, this reading''s historical grounding is strengthened, making it a more robust ''rope'' or even ''mountain'' of historical fact. If some universal claim is found, the ''immutable mandate'' reading gains ground, increasing the ''snare'' potential of its enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_applicability_ambiguity, empirical, 'Ambiguity regarding the historical scope and binding nature of Salic Law.').

omega_variable(
    territorial_integrity_vs_agnatic_purity,
    'To what extent did the perceived threat to territorial integrity genuinely drive the adoption of cognatic succession, versus being a convenient justification for political expediency?',
    'Comparative historical analysis of succession crises where both options (agnatic purity leading to fragmentation/foreign rule vs. cognatic succession preserving unity) were present, examining the stated motivations and outcomes of decisions.',
    'If territorial integrity was the primary driver, this reading''s coordination function is strongly validated. If political expediency (e.g., a powerful female heir) was the main factor, the ''rope'' classification might shift towards ''tangled_rope'' due to a more self-serving coordination narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_integrity_vs_agnatic_purity, conceptual, 'Ambiguity regarding the true motivations behind adopting cognatic succession.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__cognatic_reversion_reading, 1500, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t1500, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1500, 0.15).
narrative_ontology:measurement(sali_tr_t1600, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1600, 0.18).
narrative_ontology:measurement(sali_tr_t1700, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1700, 0.2).
narrative_ontology:measurement(sali_tr_t1800, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1800, 0.22).
narrative_ontology:measurement(sali_tr_t1900, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1900, 0.2).

% Extraction over time
narrative_ontology:measurement(sali_be_t1500, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1500, 0.25).
narrative_ontology:measurement(sali_be_t1600, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1600, 0.28).
narrative_ontology:measurement(sali_be_t1700, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1700, 0.3).
narrative_ontology:measurement(sali_be_t1800, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1800, 0.32).
narrative_ontology:measurement(sali_be_t1900, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1900, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t1500, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1500, 0.35).
narrative_ontology:measurement(sali_su_t1600, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1600, 0.38).
narrative_ontology:measurement(sali_su_t1700, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1700, 0.4).
narrative_ontology:measurement(sali_su_t1800, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1800, 0.42).
narrative_ontology:measurement(sali_su_t1900, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1900, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__cognatic_reversion_reading, identity_coordination).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__sovereign_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'salic_prohibition' kernel. This 'cognatic_reversion_reading' emphasizes historical context and territorial integrity, contrasting with the 'immutable_mandate_reading' (divine/natural law) and the 'sovereign_override_reading' (positive law subject to legislative change).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
