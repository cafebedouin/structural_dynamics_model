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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Feudal Oath as Vassal Coordination
 *   domain: medieval_political_economy/legal_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'vassal coordination' reading of the
 *   feudal oath kernel. From this perspective, the feudal oath, formalized by
 *   charter text, establishes fixed and bounded reciprocal obligations
 *   between lords and vassals. It functions primarily as a coordination
 *   mechanism, providing mutual security and stability in a decentralized
 *   political landscape, with minimal structural extraction. This reading
 *   emphasizes the contractual and mutually beneficial aspects of feudalism,
 *   rather than its potential for lordly extraction or its religious
 *   underpinnings.
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
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__vassal_coordination_reading, rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__vassal_coordination_reading, "Feudal Oath as Vassal Coordination").
narrative_ontology:topic_domain(feudal_oath_reciprocity__vassal_coordination_reading, "medieval_political_economy/legal_history/institutional_analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__vassal_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__vassal_coordination_reading, '13159458-f55e-4b4a-87f4-40336b744bff').
narrative_ontology:cs_kernel_codification('13159458-f55e-4b4a-87f4-40336b744bff', fixed_text).
narrative_ontology:cs_authority_grounding('13159458-f55e-4b4a-87f4-40336b744bff', practice).
narrative_ontology:cs_interpretation_layer_present('13159458-f55e-4b4a-87f4-40336b744bff').
narrative_ontology:cs_reading_relation('13159458-f55e-4b4a-87f4-40336b744bff', feudal_oath_reciprocity__lord_extraction_reading, forecloses).
narrative_ontology:cs_reading_relation('13159458-f55e-4b4a-87f4-40336b744bff', feudal_oath_reciprocity__ecclesiastical_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('13159458-f55e-4b4a-87f4-40336b744bff', foundational, reciprocal_obligations_are_bounded).
narrative_ontology:cs_axiom_status(reciprocal_obligations_are_bounded, holdable).
narrative_ontology:cs_axiom_grounding('13159458-f55e-4b4a-87f4-40336b744bff', reciprocal_obligations_are_bounded, conventional).
narrative_ontology:cs_axiom('13159458-f55e-4b4a-87f4-40336b744bff', foundational, charter_text_defines_terms).
narrative_ontology:cs_axiom_status(charter_text_defines_terms, holdable).
narrative_ontology:cs_axiom_grounding('13159458-f55e-4b4a-87f4-40336b744bff', charter_text_defines_terms, conventional).
narrative_ontology:cs_reference_frame('13159458-f55e-4b4a-87f4-40336b744bff', mutual_fealty_and_protection).
narrative_ontology:cs_drift_state('13159458-f55e-4b4a-87f4-40336b744bff', late_medieval_centralization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('13159458-f55e-4b4a-87f4-40336b744bff', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, vassals).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, lords).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__vassal_coordination_reading, vassals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive land tenure, protection, and justice from their lord in exchange for military service, counsel, and fealty. Their obligations are defined by charter and custom, providing a stable framework for their livelihood and security. Exiting the oath means losing land and protection, risking outlawry.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, vassals, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, vassals, payer).

% Grant land and protection to vassals, receiving military service and fealty in return. The oath provides a stable source of military power and administrative control over their territory, essential for defense and governance. Breaking the oath risks rebellion and loss of authority.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, lords, agenda_setter,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, lords, beneficiary).

% Interpret and record the terms of feudal oaths and charters, contributing to the legal framework that defines reciprocal obligations. They analyze the historical and customary precedents that shape the constraint's operation.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, charter_scribes_and_legal_scholars, observer,
    analytical, generational, analytical, regional).

% Are not directly party to the feudal oath but are affected by the stability and justice it provides (or fails to provide). They would advocate for more direct access to land and justice without the intermediaries of the feudal system, but lack the power to influence its terms.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, free_peasants, excluded,
    powerless, immediate, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable hierarchy of mutual military and economic support, defining land tenure and service obligations to prevent constant warfare and ensure defense and local governance.
% TRANSFER_FUNCTION: Transfers military service, labor, and fealty from vassals to lords in exchange for land tenure, protection, and justice, creating a decentralized system of governance and defense.
% ABSENT_VOICES: Free peasants and unlanded commoners are structurally excluded from the feudal contract; they would object to the indirect nature of their protection and the lack of direct recourse for justice, advocating for more equitable land distribution and direct governance.
% DISAPPEARANCE_RATIONALE: If the feudal oath system and its enforcement vanished overnight, the entire medieval social, military, and economic structure would collapse into widespread anarchy, land disputes, and vulnerability to external threats, requiring a complete reorganization of governance and defense.
% FOUNDING_PROBLEM: Widespread insecurity, lack of centralized authority, and constant warfare in post-Roman Europe, leading to a need for localized defense, resource allocation, and social order.
% FOUNDING_PROBLEM_CORROBORATION: Medieval chronicles, legal texts, and monastic records from outside the immediate lordly beneficiaries corroborate the historical problem of insecurity and the role of feudal contracts in establishing order and mutual obligation. This reading emphasizes the enduring need for stable, reciprocal obligations, even if the specific form changes over time.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__vassal_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__vassal_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__vassal_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The low extractiveness (0.15) reflects the view that the oath's primary function is to coordinate mutual defense and resource allocation, with the 'cost' being the necessary overhead of maintaining a stable social order. Suppression (0.4) is moderate, as enforcement is active but relies on mutual fealty and the threat of social/military collapse rather than pure coercion. The low theater ratio (0.1) indicates that the oath's stated function of reciprocal obligation is genuinely performed from this perspective. Accessibility collapse and resistance are low because the system offers a viable, if constrained, path to security and livelihood for participants.
 *
 * PERSPECTIVAL GAP:
 *   This reading stands in contrast to the 'lord extraction' reading, which would emphasize the coercive aspects and high extraction from vassals, and the 'ecclesiastical mediation' reading, which would highlight the moral and religious bounds on secular power. The engine's classification will reveal how these different structural interpretations lead to divergent classifications for the same underlying kernel.
 *
 * DIRECTIONALITY LOGIC:
 *   From the vassal coordination perspective, both vassals and lords are structural beneficiaries. Vassals gain protection, land, and justice, while lords gain military service and administrative control. Both parties benefit from the stability and order the oath provides, making it a genuinely cooperative arrangement, albeit one with inherent power asymmetries. The 'payer' aspect for vassals refers to their service obligations, which are balanced by the benefits received.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the feudal oath primarily a mechanism for vassal coordination, lordly extraction, or ecclesiastically mediated obligation?',
    'Analysis of historical legal disputes, economic transfers, and religious texts to determine which interpretation held dominant structural force in specific regions and periods.',
    'If the ''lord_extraction_reading'' is structurally dominant, the constraint would reclassify as a Snare or Tangled Rope with significantly higher extraction. If the ''ecclesiastical_mediation_reading'' is dominant, the constraint''s authority grounding and moral bounds would be re-evaluated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity regarding the primary structural function of the feudal oath.').

omega_variable(
    bounded_vs_maximal_extraction,
    'Are the obligations established by the feudal oath genuinely bounded by charter and custom, or do they tend towards maximal extraction limited only by vassal capacity?',
    'Empirical study of historical records of feudal dues, military service demands, and legal challenges to lordly demands across different regions and centuries.',
    'If obligations are found to consistently exceed charter terms or vassal capacity, the extractiveness metric would increase significantly, pushing the classification towards Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bounded_vs_maximal_extraction, empirical, 'Whether feudal obligations are truly bounded or tend towards maximal extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__vassal_coordination_reading, 900, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t900, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 900, 0.1).
narrative_ontology:measurement(feud_tr_t950, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 950, 0.1).
narrative_ontology:measurement(feud_tr_t1000, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(feud_tr_t1050, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1050, 0.1).
narrative_ontology:measurement(feud_tr_t1100, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1100, 0.1).
narrative_ontology:measurement(feud_tr_t1150, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1150, 0.1).
narrative_ontology:measurement(feud_tr_t1200, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1200, 0.1).

% Extraction over time
narrative_ontology:measurement(feud_be_t900, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 900, 0.12).
narrative_ontology:measurement(feud_be_t950, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 950, 0.13).
narrative_ontology:measurement(feud_be_t1000, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1000, 0.14).
narrative_ontology:measurement(feud_be_t1050, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1050, 0.14).
narrative_ontology:measurement(feud_be_t1100, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1100, 0.15).
narrative_ontology:measurement(feud_be_t1150, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1150, 0.15).
narrative_ontology:measurement(feud_be_t1200, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1200, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t900, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 900, 0.35).
narrative_ontology:measurement(feud_su_t950, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 950, 0.37).
narrative_ontology:measurement(feud_su_t1000, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1000, 0.38).
narrative_ontology:measurement(feud_su_t1050, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1050, 0.39).
narrative_ontology:measurement(feud_su_t1100, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1100, 0.4).
narrative_ontology:measurement(feud_su_t1150, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1150, 0.4).
narrative_ontology:measurement(feud_su_t1200, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1200, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__vassal_coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, medieval_land_tenure_system).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, military_mobilization_norms).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity__lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'feudal_oath_reciprocity' kernel, each representing a distinct structural interpretation of the same historical phenomenon. They are linked to capture the contested nature of the feudal oath's function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
