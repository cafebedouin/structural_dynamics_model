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
 *   constraint_id: salic_prohibition__cognatic_reversion_reading
 *   human_readable: Salic Law as Frankish Anachronism (Cognatic Reversion Reading)
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   This constraint represents the 'cognatic reversion' reading of Salic Law,
 *   which argues that the Salic prohibition on female succession was a
 *   specific Frankish custom, never universally binding, and that
 *   non-Frankish territories should revert to cognatic primogeniture
 *   (succession through the eldest child regardless of sex) to ensure
 *   dynastic stability and territorial integrity. This reading emerged as a
 *   counter-argument to strict agnatic interpretations, particularly in
 *   periods where the lack of male heirs threatened the continuity of a
 *   dynasty or the unity of a realm. It frames Salic Law as an anachronism or
 *   a foreign imposition rather than a fundamental constitutional principle.
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
narrative_ontology:cs_story_uid(salic_prohibition__cognatic_reversion_reading, '0360c880-c389-4615-9c73-2b64867ac528').
narrative_ontology:cs_kernel_codification('0360c880-c389-4615-9c73-2b64867ac528', fixed_text).
narrative_ontology:cs_authority_grounding('0360c880-c389-4615-9c73-2b64867ac528', lineage).
narrative_ontology:cs_interpretation_layer_present('0360c880-c389-4615-9c73-2b64867ac528').
narrative_ontology:cs_reading_relation('0360c880-c389-4615-9c73-2b64867ac528', salic_prohibition__immutable_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('0360c880-c389-4615-9c73-2b64867ac528', salic_prohibition__sovereign_override_reading, coexists_with).
narrative_ontology:cs_axiom('0360c880-c389-4615-9c73-2b64867ac528', foundational, salic_law_is_frankish_custom_only).
narrative_ontology:cs_axiom_status(salic_law_is_frankish_custom_only, holdable).
narrative_ontology:cs_axiom_grounding('0360c880-c389-4615-9c73-2b64867ac528', salic_law_is_frankish_custom_only, conventional).
narrative_ontology:cs_axiom('0360c880-c389-4615-9c73-2b64867ac528', foundational, territorial_integrity_trumps_agnatic_purity).
narrative_ontology:cs_axiom_status(territorial_integrity_trumps_agnatic_purity, holdable).
narrative_ontology:cs_axiom_grounding('0360c880-c389-4615-9c73-2b64867ac528', territorial_integrity_trumps_agnatic_purity, instrumental).
narrative_ontology:cs_reference_frame('0360c880-c389-4615-9c73-2b64867ac528', cognatic_primogeniture_default).
narrative_ontology:cs_drift_state('0360c880-c389-4615-9c73-2b64867ac528', post_napoleonic_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('0360c880-c389-4615-9c73-2b64867ac528', '').
narrative_ontology:cs_kernel_id(salic_prohibition__cognatic_reversion_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, female_heirs).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, territorial_integrity_advocates).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, agnatic_succession_proponents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, non_frankish_nobility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly benefit from the interpretation that allows female succession, securing their claim to the throne. Their position is strengthened by historical arguments against the universal applicability of Salic Law.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, female_heirs, beneficiary,
    moderate, generational, constrained, national).

% Support this reading as it prioritizes the stability and unity of the realm over strict adherence to an imported, potentially destabilizing, succession rule. They see the Salic prohibition as a foreign imposition.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, territorial_integrity_advocates, beneficiary,
    organized, generational, mobile, national).

% Bear the cost of this interpretation as it undermines their preferred male-only line of succession. They argue for the historical and traditional validity of Salic Law as a fundamental principle.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, agnatic_succession_proponents, payer,
    powerful, generational, constrained, national).

% Analyze the historical origins and legal evolution of Salic Law, providing academic arguments for or against its applicability. Their work informs the debate but does not directly enforce the constraint.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, historical_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% Their historical claims to succession are validated by this reading, which asserts that Salic Law never properly applied to their territories, which were not part of the original Frankish domain.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, non_frankish_nobility, beneficiary,
    organized, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__cognatic_reversion_reading, female_heirs).
narrative_ontology:fixing_cost_class(salic_prohibition__cognatic_reversion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, stable rule for dynastic succession that prioritizes the continuity and territorial integrity of the realm by allowing female heirs when necessary, avoiding succession crises that could lead to fragmentation.
% TRANSFER_FUNCTION: Transfers the right of succession from exclusively male lines to include female lines, ensuring a broader pool of eligible heirs and potentially preventing foreign claims through marriage.
% ABSENT_VOICES: The original Frankish jurists who drafted the Salic Law would object, arguing for its foundational and immutable status. Their voices are absent from the contemporary debate, which reinterprets their original intent.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the succession would revert to strict agnatic principles, potentially disinheriting current or future female heirs, triggering dynastic crises, and challenging the legitimacy of past successions based on cognatic principles. The political landscape would be significantly altered.
% FOUNDING_PROBLEM: The problem of ensuring stable dynastic succession in territories that were not originally Frankish, where strict Salic Law could lead to a lack of direct male heirs and subsequent instability or foreign intervention.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of succession disputes in non-Frankish territories and contemporary constitutional debates in monarchies corroborate the ongoing relevance of this problem. Legal scholars and historians outside the immediate dynastic beneficiaries attest to the historical and political challenges of applying a rigid, foreign succession rule.
narrative_ontology:disappearance_verdict(salic_prohibition__cognatic_reversion_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__cognatic_reversion_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__cognatic_reversion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness is moderate (0.3) as it extracts from proponents of strict agnatic succession by denying their claims, but it also provides a coordination function for dynastic stability. Suppression is moderate (0.4) as this reading requires active historical and legal arguments to counter the entrenched 'immutable mandate' view, but it doesn't rely on overt coercion. Theater ratio is low (0.2) as the arguments are genuinely legal and historical, not merely performative. The temporal measurements reflect a period where this interpretation gained traction and became a more established, though still contested, position.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of female heirs and non-Frankish nobility, this reading is a just and necessary correction to an outdated, foreign rule, ensuring stability. From the perspective of agnatic succession proponents, it is a dangerous deviation from fundamental law, threatening tradition and legitimacy. The engine's classification will reflect this divergence based on their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Female heirs and advocates for territorial integrity are beneficiaries, as this reading secures their claims and priorities. Proponents of strict agnatic succession are payers, as their preferred system is undermined. Historical legal scholars and non-Frankish nobility are observers or beneficiaries, respectively, contributing to or benefiting from the reinterpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_applicability_ambiguity,
    'Was Salic Law ever genuinely adopted as fundamental law in non-Frankish territories, or was its application always contested and conditional?',
    'Comprehensive historical-legal analysis of constitutional documents, dynastic treaties, and legal commentaries from the relevant periods and territories, focusing on explicit adoption clauses versus customary practice.',
    'If it was genuinely adopted, the ''cognatic reversion'' reading becomes a ''sovereign override'' or ''legislative amendment'' rather than a ''reversion to original custom'', shifting its claimed type towards a scaffold or tangled rope. If always contested, the rope classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_applicability_ambiguity, empirical, 'Ambiguity regarding the historical scope and adoption of Salic Law outside its Frankish origins.').

omega_variable(
    territorial_integrity_vs_agnatic_purity,
    'Is the prioritization of territorial integrity over agnatic purity a consistent principle across all dynastic constitutional traditions, or a specific normative choice of this reading?',
    'Comparative constitutional history of European monarchies, examining how different realms resolved succession crises involving female heirs or foreign claims, and the explicit justifications given for their choices.',
    'If it''s a consistent principle, the ''cognatic reversion'' reading gains stronger ''mountain-like'' legitimacy for its underlying rationale. If it''s a specific normative choice, the ''preference'' aspect of this reading becomes more prominent, potentially increasing its extractiveness from those who prioritize agnatic purity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_integrity_vs_agnatic_purity, conceptual, 'The normative grounding of prioritizing territorial integrity in dynastic succession.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__cognatic_reversion_reading, 1500, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t1500, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(sali_tr_t1600, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1600, 0.15).
narrative_ontology:measurement(sali_tr_t1700, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1700, 0.2).
narrative_ontology:measurement(sali_tr_t1800, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1800, 0.2).
narrative_ontology:measurement(sali_tr_t1900, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1900, 0.2).

% Extraction over time
narrative_ontology:measurement(sali_be_t1500, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1500, 0.2).
narrative_ontology:measurement(sali_be_t1600, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1600, 0.25).
narrative_ontology:measurement(sali_be_t1700, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1700, 0.3).
narrative_ontology:measurement(sali_be_t1800, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1800, 0.3).
narrative_ontology:measurement(sali_be_t1900, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1900, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t1500, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1500, 0.3).
narrative_ontology:measurement(sali_su_t1600, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1600, 0.35).
narrative_ontology:measurement(sali_su_t1700, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1700, 0.4).
narrative_ontology:measurement(sali_su_t1800, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1800, 0.4).
narrative_ontology:measurement(sali_su_t1900, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1900, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__cognatic_reversion_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
