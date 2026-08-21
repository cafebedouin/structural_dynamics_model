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
 *   human_readable: Feudal Oath of Reciprocity (Vassal Coordination Reading)
 *   domain: medieval_political_economy/legal_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story instantiates the 'vassal coordination' reading of
 *   the feudal oath of reciprocity. From this perspective, the oath,
 *   formalized by charter text, primarily functions as a mechanism for mutual
 *   coordination, establishing fixed and bounded reciprocal obligations
 *   between lords and vassals. It solves a genuine collective action problem
 *   of defense and governance in a decentralized era, with both parties being
 *   net beneficiaries. The metrics reflect a functional, low-extraction
 *   coordination mechanism, consistent with a Rope classification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__vassal_coordination_reading, 0.2).
domain_priors:suppression_score(feudal_oath_reciprocity__vassal_coordination_reading, 0.45).
domain_priors:theater_ratio(feudal_oath_reciprocity__vassal_coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__vassal_coordination_reading, rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__vassal_coordination_reading, "Feudal Oath of Reciprocity (Vassal Coordination Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__vassal_coordination_reading, "medieval_political_economy/legal_history/institutional_analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__vassal_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__vassal_coordination_reading, 'c8a68786-d301-4632-8ebe-26888b19eac8').
narrative_ontology:cs_kernel_codification('c8a68786-d301-4632-8ebe-26888b19eac8', fixed_text).
narrative_ontology:cs_authority_grounding('c8a68786-d301-4632-8ebe-26888b19eac8', lineage).
narrative_ontology:cs_interpretation_layer_present('c8a68786-d301-4632-8ebe-26888b19eac8').
narrative_ontology:cs_reading_relation('c8a68786-d301-4632-8ebe-26888b19eac8', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('c8a68786-d301-4632-8ebe-26888b19eac8', feudal_oath_reciprocity__ecclesiastical_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('c8a68786-d301-4632-8ebe-26888b19eac8', foundational, reciprocal_obligations_are_fixed).
narrative_ontology:cs_axiom_status(reciprocal_obligations_are_fixed, holdable).
narrative_ontology:cs_axiom_grounding('c8a68786-d301-4632-8ebe-26888b19eac8', reciprocal_obligations_are_fixed, conventional).
narrative_ontology:cs_axiom('c8a68786-d301-4632-8ebe-26888b19eac8', foundational, charter_defines_bounds).
narrative_ontology:cs_axiom_status(charter_defines_bounds, holdable).
narrative_ontology:cs_axiom_grounding('c8a68786-d301-4632-8ebe-26888b19eac8', charter_defines_bounds, conventional).
narrative_ontology:cs_reference_frame('c8a68786-d301-4632-8ebe-26888b19eac8', feudal_contractual_order).
narrative_ontology:cs_drift_state('c8a68786-d301-4632-8ebe-26888b19eac8', high_medieval_period, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c8a68786-d301-4632-8ebe-26888b19eac8', '').
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

% Receive land tenure and protection from their lord in exchange for military service, counsel, and other feudal dues. They benefit from the stability and order the oath provides, but bear the costs of service and loyalty. Exiting the relationship means losing land and protection, often leading to destitution or seeking a new, potentially less favorable, lord.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, vassals, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, vassals, payer).

% Grant land and protection to vassals, receiving military service and loyalty in return. They benefit from a stable power base and military force. They set the terms of the oath within customary bounds and enforce it. Their exit options include seeking new vassals, consolidating power, or engaging in warfare to expand their domain.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, lords, agenda_setter,
    powerful, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, lords, beneficiary).

% Are not direct parties to the feudal oath but are deeply affected by its stability or breakdown. They provide labor and produce to support the feudal system, receiving protection from their lord. Their options are severely limited, often tied to the land, with little to no ability to exit the system.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, peasants, excluded,
    powerless, immediate, trapped, local).

% Observe and sometimes mediate feudal disputes, emphasizing the moral and spiritual dimensions of oaths. While not directly enforcing the secular terms of the charter, their influence can shape interpretations and encourage adherence to reciprocal duties.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, ecclesiastical_authorities, observer,
    institutional, civilizational, analytical, continental).

% Are responsible for drafting and preserving the written charter texts that formalize feudal obligations. They act as custodians of the legal framework, influencing the precision and interpretation of the 'fixed, bounded' terms, though they do not directly enforce them.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, charter_scribes, observer,
    moderate, biographical, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, hierarchical social and military order by defining mutual obligations between lords and vassals, ensuring land tenure, military service, and protection in a decentralized political landscape.
% TRANSFER_FUNCTION: Transfers military service, counsel, and loyalty from vassals to lords, and transfers land tenure, protection, and justice from lords to vassals, all mediated by the oath and charter.
% ABSENT_VOICES: Peasants and unlanded knights, who bear the ultimate costs of the system's instability or benefit from its order, but have no direct voice in the negotiation or enforcement of the oath. They would advocate for greater security and less arbitrary demands.
% DISAPPEARANCE_RATIONALE: If the feudal oath and its reciprocal obligations vanished overnight, the entire medieval social, political, and military structure would collapse into widespread anarchy, as the primary mechanism for governance, landholding, and defense would cease to exist. Society would reorganize around new forms of authority or descend into chaos.
% FOUNDING_PROBLEM: The problem of widespread anarchy, lack of centralized authority, and the need for mutual defense and stable land tenure in a post-Roman, pre-nation-state Europe.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary legal treatises and historical accounts from chroniclers not directly beholden to a specific lord or vassal attest to the ongoing need for order and defense, supporting the view that the founding problem remained live throughout the feudal period, albeit with evolving manifestations.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__vassal_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__vassal_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__vassal_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(feudal_oath_reciprocity__vassal_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__vassal_coordination_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is low (0.20) because the oath is understood as a reciprocal agreement where both parties gain essential benefits (land tenure/protection for service/loyalty). Suppression is moderate (0.45) as enforcement is active through custom, legal precedent, and the threat of forfeiture, but it's not unilaterally coercive. Theater ratio is low (0.10) because the oath's function is genuinely operational, not performative. Accessibility collapse is moderate (0.40) as alternatives (e.g., seeking another lord, living outside the system) exist but carry significant risks. Resistance is low (0.15) due to the mutual benefits.
 *
 * PERSPECTIVAL GAP:
 *   This 'vassal coordination' reading emphasizes the mutual benefits and bounded nature of the oath. Other readings, such as the 'lord extraction' reading, would emphasize the power asymmetry and coercive aspects, leading to a higher extractiveness score and a different classification. The 'ecclesiastical mediation' reading would highlight the moral and spiritual dimensions of the oath, which are secondary in this secular, charter-focused analysis.
 *
 * DIRECTIONALITY LOGIC:
 *   Both vassals and lords are declared beneficiaries, reflecting the mutual coordination function. Lords, as agenda-setters, have more power and exit options, but vassals also benefit from the stability. There are no direct 'victims' in this reading, as the system is seen as mutually beneficial, even if asymmetric. The engine will derive directionality accordingly, with both parties leaning towards the beneficiary end.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lord_extraction_vs_reciprocity,
    'Is the feudal oath primarily a mechanism for mutual coordination and defense, or does it primarily serve to legitimize the lord''s extraction of resources and service from the vassal?',
    'Analysis of historical records of disputes, legal precedents, and economic flows, particularly focusing on instances where lords unilaterally increased demands beyond charter terms.',
    'If historical evidence strongly supports a pattern of unilateral extraction, the constraint would be reclassified towards a Tangled Rope or Snare, with higher extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lord_extraction_vs_reciprocity, empirical, 'Contest between coordination and extraction as the primary function of the feudal oath.').

omega_variable(
    secular_vs_ecclesiastical_enforcement,
    'To what extent was the enforcement of feudal obligations primarily based on the secular charter text and custom, versus the moral and spiritual authority of the Church and sacramental oaths?',
    'Examination of legal records from secular courts versus church court proceedings, and contemporary theological writings on oath-breaking.',
    'If ecclesiastical authority was dominant, this reading''s focus on secular enforcement is incomplete, and the ''ecclesiastical_mediation_reading'' gains salience, potentially altering the perceived source of suppression and legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_vs_ecclesiastical_enforcement, conceptual, 'Ambiguity regarding the primary source of enforcement for feudal obligations.').

omega_variable(
    boundedness_ambiguity,
    'How ''fixed'' and ''bounded'' were the reciprocal obligations in practice, given the evolving nature of custom and the power asymmetry between lords and vassals?',
    'Detailed case studies of feudal disputes and their resolutions, comparing charter text to actual outcomes and the flexibility of customary law.',
    'If obligations were highly fluid or unilaterally reinterpreted by lords without recourse for vassals, the ''fixed, bounded'' premise of this reading is weakened, pushing towards a more extractive classification (e.g., Tangled Rope) due to the lack of genuine reciprocity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundedness_ambiguity, empirical, 'Uncertainty about the practical limits and enforceability of ''fixed'' obligations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__vassal_coordination_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(feud_tr_t10, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(feud_tr_t20, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(feud_tr_t30, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement(feud_tr_t40, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(feud_tr_t50, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(feud_be_t10, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 10, 0.2).
narrative_ontology:measurement(feud_be_t20, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 20, 0.19).
narrative_ontology:measurement(feud_be_t30, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 30, 0.2).
narrative_ontology:measurement(feud_be_t40, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 40, 0.21).
narrative_ontology:measurement(feud_be_t50, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 50, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(feud_su_t10, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(feud_su_t20, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(feud_su_t30, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 30, 0.44).
narrative_ontology:measurement(feud_su_t40, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(feud_su_t50, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 50, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__vassal_coordination_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'feudal_oath_reciprocity' kernel, alongside 'lord_extraction_reading' and 'ecclesiastical_mediation_reading'. Each reading offers a distinct structural interpretation of the same historical phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
