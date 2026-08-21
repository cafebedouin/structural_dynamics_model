% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__vassal_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__vassal_coordination_reading, []).

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
 *   constraint_id: feudal_oath_reciprocity__vassal_coordination_reading
 *   human_readable: Feudal Oath Reciprocity (Vassal Coordination Reading)
 *   domain: medieval_political_economy/legal_history
 *
 * SUMMARY:
 *   This constraint models the feudal oath from the perspective of vassal
 *   coordination, where the oath and its accompanying charter text establish
 *   fixed, bounded reciprocal obligations between lords and vassals. It is a
 *   reading that emphasizes mutual benefit and the reduction of arbitrary
 *   power, rather than pure extraction. The constraint is claimed as a Rope,
 *   reflecting its primary function as a coordination mechanism that benefits
 *   both parties by providing stability and predictability in a turbulent
 *   era. The metrics reflect a low level of extraction, moderate suppression
 *   (necessary for any feudal system), and low theatricality, as the
 *   charter's terms were generally taken seriously.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__vassal_coordination_reading, 0.15).
domain_priors:suppression_score(feudal_oath_reciprocity__vassal_coordination_reading, 0.4).
domain_priors:theater_ratio(feudal_oath_reciprocity__vassal_coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__vassal_coordination_reading, rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__vassal_coordination_reading, "Feudal Oath Reciprocity (Vassal Coordination Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__vassal_coordination_reading, "medieval_political_economy/legal_history").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__vassal_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__vassal_coordination_reading, 'fb3df8ed-9e9e-432c-9cb6-f498b6856db9').
narrative_ontology:cs_kernel_codification('fb3df8ed-9e9e-432c-9cb6-f498b6856db9', fixed_text).
narrative_ontology:cs_authority_grounding('fb3df8ed-9e9e-432c-9cb6-f498b6856db9', lineage).
narrative_ontology:cs_interpretation_layer_present('fb3df8ed-9e9e-432c-9cb6-f498b6856db9').
narrative_ontology:cs_reading_relation('fb3df8ed-9e9e-432c-9cb6-f498b6856db9', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('fb3df8ed-9e9e-432c-9cb6-f498b6856db9', feudal_oath_reciprocity__ecclesiastical_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('fb3df8ed-9e9e-432c-9cb6-f498b6856db9', foundational, reciprocal_obligation_bounded_by_charter).
narrative_ontology:cs_axiom_status(reciprocal_obligation_bounded_by_charter, holdable).
narrative_ontology:cs_axiom_grounding('fb3df8ed-9e9e-432c-9cb6-f498b6856db9', reciprocal_obligation_bounded_by_charter, conventional).
narrative_ontology:cs_axiom('fb3df8ed-9e9e-432c-9cb6-f498b6856db9', secondary, mutual_benefit_from_stability).
narrative_ontology:cs_axiom_status(mutual_benefit_from_stability, holdable).
narrative_ontology:cs_axiom_grounding('fb3df8ed-9e9e-432c-9cb6-f498b6856db9', mutual_benefit_from_stability, instrumental).
narrative_ontology:cs_reference_frame('fb3df8ed-9e9e-432c-9cb6-f498b6856db9', charter_defined_reciprocity).
narrative_ontology:cs_drift_state('fb3df8ed-9e9e-432c-9cb6-f498b6856db9', late_medieval_period, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('fb3df8ed-9e9e-432c-9cb6-f498b6856db9', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, vassals).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, lords).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive protection, land tenure, and legal standing in exchange for military service and counsel. The oath provides a framework for dispute resolution and limits arbitrary demands from their lord, ensuring a degree of stability and predictability in their obligations.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, vassals, beneficiary,
    moderate, biographical, constrained, local).

% Receive military service, counsel, and loyalty from their vassals, which is essential for maintaining their power and defending their territories. The oath formalizes these relationships, providing a stable base for their authority and a mechanism for enforcing obligations without constant coercion.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, lords, beneficiary,
    powerful, generational, constrained, regional).

% Draft and maintain the written charters that record the specific terms of the feudal oath. Their expertise in legal language and record-keeping ensures the clarity and enforceability of the reciprocal obligations, acting as a neutral party in codifying the agreement.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, charter_scribes_and_notaries, agenda_setter,
    moderate, biographical, mobile, local).

% Are largely outside the direct reciprocal obligations of the feudal oath, though their labor supports the entire system. They are subject to the authority of both vassals and lords, with little formal recourse within the oath's framework. Their voices are not considered in the drafting or interpretation of the charters.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, peasantry, excluded,
    powerless, immediate, trapped, local).

% Observe and sometimes mediate disputes related to feudal oaths, emphasizing the spiritual sanctity of the oath and the moral obligations of both parties. They provide a moral and theological framework that underpins the secular legal structure, but do not directly enforce the charter's terms.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, ecclesiastical_authorities, observer,
    institutional, civilizational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, bounded framework for reciprocal obligations between lords and vassals, defining military service, land tenure, and mutual protection, thereby reducing arbitrary demands and fostering stability in a decentralized political landscape.
% TRANSFER_FUNCTION: Transfers military service, counsel, and loyalty from vassals to lords, and transfers land tenure, protection, and legal standing from lords to vassals. The charter text fixes the scope and limits of these transfers.
% ABSENT_VOICES: The peasantry, who are the ultimate source of wealth and labor, are entirely excluded from the negotiation and formalization of these reciprocal obligations. Their interests are not represented, and their obligations are largely unilateral, not reciprocal.
% DISAPPEARANCE_RATIONALE: If the feudal oath and its charter-based enforcement vanished, the entire medieval political and social order would collapse into widespread anarchy. Land tenure would become insecure, military service unreliable, and the hierarchical structure of power would dissolve, leading to constant warfare and a complete reorganization of governance.
% FOUNDING_PROBLEM: The problem of establishing stable governance, military organization, and land distribution in a post-Roman, decentralized Europe characterized by constant warfare and weak central authority.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars widely corroborate that the specific problems of early medieval decentralization and insecurity were the founding conditions. While some principles of reciprocal obligation persist, the feudal system itself is no longer a live solution to contemporary governance challenges; its founding problem is dead, but its legacy influences later legal thought.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__vassal_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__vassal_coordination_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__vassal_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(feudal_oath_reciprocity__vassal_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__vassal_coordination_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__vassal_coordination_reading_tests).
:- end_tests(feudal_oath_reciprocity__vassal_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because this reading emphasizes the mutual benefits and bounded nature of obligations, where both lords and vassals gain from the stability provided by the oath. Suppression (0.4) is moderate, reflecting the need for enforcement in a pre-modern legal system, but not the overwhelming coercion of a Snare. Theater ratio (0.1) is low, as the charters were generally functional legal documents, not mere performance. The slight increase in extractiveness and suppression over time reflects the gradual hardening of feudal obligations and the increasing power of lords in some regions, before a slight re-stabilization.
 *
 * PERSPECTIVAL GAP:
 *   This 'vassal coordination' reading stands in contrast to other possible readings, such as a 'lord extraction' reading (where the oath is primarily a tool for maximizing lordly power) or an 'ecclesiastical mediation' reading (where the Church's moral authority is the primary constraint on secular power). The engine's classification will highlight how this specific structural interpretation leads to a Rope classification, while other interpretations of the same historical kernel would yield different types.
 *
 * DIRECTIONALITY LOGIC:
 *   Both vassals and lords are declared as beneficiaries in this reading, as the oath provides a framework that benefits both by defining their roles and reducing uncertainty. The charter scribes and notaries act as agenda-setters by codifying the terms. The peasantry is excluded, bearing the costs of the system without formal reciprocal benefits. Ecclesiastical authorities act as observers, providing moral legitimacy and occasional mediation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''vassal coordination'' mechanism, or is that a legitimizing narrative for a fundamentally extractive ''lord extraction'' system?',
    'Comparative historical analysis of charter texts vs. actual practice, focusing on instances of vassal resistance and lordly overreach, and the outcomes of such conflicts. Examination of economic data on surplus extraction from vassals over time.',
    'If the ''lord extraction'' reading is more accurate, the constraint would reclassify as a Tangled Rope or Snare, with significantly higher extractiveness and suppression, and vassals shifting from beneficiaries to victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, empirical, 'Ambiguity between coordination and extraction in the feudal oath.').

omega_variable(
    ecclesiastical_influence_ambiguity,
    'To what extent did ecclesiastical authority genuinely limit secular extraction, as opposed to merely providing moral sanction for the existing power structure?',
    'Analysis of papal and episcopal interventions in feudal disputes, their effectiveness, and the long-term impact on the balance of power between lords and vassals. Comparison of regions with strong vs. weak ecclesiastical influence.',
    'If ecclesiastical influence was a strong, independent constraint, it would suggest a separate, parallel constraint (ecclesiastical_mediation_reading) that influences this one, potentially lowering its effective extractiveness by providing an external check. If weak, this reading''s extractiveness would be more accurate as a standalone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecclesiastical_influence_ambiguity, empirical, 'Role of the Church in constraining feudal power dynamics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__vassal_coordination_reading, 800, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t800, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 800, 0.05).
narrative_ontology:measurement(feud_tr_t900, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 900, 0.08).
narrative_ontology:measurement(feud_tr_t1000, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(feud_tr_t1100, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1100, 0.12).
narrative_ontology:measurement(feud_tr_t1200, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1200, 0.1).

% Extraction over time
narrative_ontology:measurement(feud_be_t800, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 800, 0.1).
narrative_ontology:measurement(feud_be_t900, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 900, 0.12).
narrative_ontology:measurement(feud_be_t1000, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1000, 0.15).
narrative_ontology:measurement(feud_be_t1100, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1100, 0.17).
narrative_ontology:measurement(feud_be_t1200, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1200, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t800, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 800, 0.3).
narrative_ontology:measurement(feud_su_t900, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 900, 0.35).
narrative_ontology:measurement(feud_su_t1000, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1000, 0.4).
narrative_ontology:measurement(feud_su_t1100, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1100, 0.42).
narrative_ontology:measurement(feud_su_t1200, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1200, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__vassal_coordination_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'feudal_oath_reciprocity' kernel. Other readings include 'lord_extraction_reading' and 'ecclesiastical_mediation_reading', which emphasize different structural aspects and would yield different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
