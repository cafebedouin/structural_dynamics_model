% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__ecclesiastical_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__ecclesiastical_mediation_reading, []).

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
 *   constraint_id: feudal_oath_reciprocity__ecclesiastical_mediation_reading
 *   human_readable: Feudal Oath Reciprocity (Ecclesiastical Mediation Reading)
 *   domain: medieval_political_economy/legal_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'ecclesiastical mediation' reading of
 *   feudal oath reciprocity, where Christian charity and sacramental
 *   obligations are understood to limit secular lords' extractive power. It
 *   functions as a Tangled Rope: it provides a coordination function
 *   (stability, justice) but also involves asymmetric extraction (lords'
 *   power is curtailed, Church gains authority). The metrics reflect a
 *   moderate level of extraction from secular lords, actively enforced by the
 *   Church's moral and spiritual authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.45).
domain_priors:suppression_score(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.6).
domain_priors:theater_ratio(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, tangled_rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "Feudal Oath Reciprocity (Ecclesiastical Mediation Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "medieval_political_economy/legal_history/institutional_analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__ecclesiastical_mediation_reading, '5b9eaa3c-b7fb-423e-b5a7-fa675f1b6643').
narrative_ontology:cs_kernel_codification('5b9eaa3c-b7fb-423e-b5a7-fa675f1b6643', formalized).
narrative_ontology:cs_authority_grounding('5b9eaa3c-b7fb-423e-b5a7-fa675f1b6643', lineage).
narrative_ontology:cs_interpretation_layer_present('5b9eaa3c-b7fb-423e-b5a7-fa675f1b6643').
narrative_ontology:cs_reading_relation('5b9eaa3c-b7fb-423e-b5a7-fa675f1b6643', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b9eaa3c-b7fb-423e-b5a7-fa675f1b6643', feudal_oath_reciprocity__vassal_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('5b9eaa3c-b7fb-423e-b5a7-fa675f1b6643', foundational, oath_bound_by_christian_charity).
narrative_ontology:cs_axiom_status(oath_bound_by_christian_charity, holdable).
narrative_ontology:cs_axiom_grounding('5b9eaa3c-b7fb-423e-b5a7-fa675f1b6643', oath_bound_by_christian_charity, theological).
narrative_ontology:cs_axiom('5b9eaa3c-b7fb-423e-b5a7-fa675f1b6643', foundational, church_has_interpretive_authority).
narrative_ontology:cs_axiom_status(church_has_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('5b9eaa3c-b7fb-423e-b5a7-fa675f1b6643', church_has_interpretive_authority, conventional).
narrative_ontology:cs_reference_frame('5b9eaa3c-b7fb-423e-b5a7-fa675f1b6643', divinely_ordained_reciprocity).
narrative_ontology:cs_drift_state('5b9eaa3c-b7fb-423e-b5a7-fa675f1b6643', late_medieval_secularization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5b9eaa3c-b7fb-423e-b5a7-fa675f1b6643', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_authorities).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassals).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_lords).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, peasantry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce the moral and sacramental obligations of feudal oaths, using the threat of excommunication or interdict to limit secular lords' demands. They gain moral authority and influence over secular affairs.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_authorities, agenda_setter,
    institutional, generational, mobile, continental).

% Bound by oaths to their vassals and to God, their ability to extract resources or demand service is constrained by ecclesiastical interpretations of Christian charity and justice. They face moral and spiritual costs for over-extraction, as well as potential political instability if vassals appeal to the Church.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_lords, payer,
    powerful, biographical, constrained, regional).

% Benefit from the Church's mediation, which provides a check on their lord's power and limits arbitrary demands. Their obligations are still substantial, but the ecclesiastical framework offers a recourse against excessive extraction, improving their security and stability.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassals, beneficiary,
    moderate, biographical, constrained, local).

% Bear the ultimate burden of feudal extraction, but indirectly benefit from any stability or limits on arbitrary demands imposed by the ecclesiastical framework. Their direct interaction is with the lord, but the Church's influence can temper the lord's demands.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, peasantry, payer,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a moral and legal framework for reciprocal obligations between lords and vassals, mediated by the Church, aiming to prevent arbitrary secular power and ensure a degree of justice and stability within the feudal system.
% TRANSFER_FUNCTION: Transfers interpretive authority and moral leverage from secular lords to ecclesiastical authorities, and limits the material extraction (labor, goods, military service) from vassals to lords, in favor of a more 'charitable' or 'just' distribution.
% ABSENT_VOICES: A purely secular legal tradition that would argue for the lord's absolute right to extraction based on military power and land ownership, unconstrained by religious doctrine. This perspective is actively suppressed by the Church's moral authority.
% DISAPPEARANCE_RATIONALE: If the ecclesiastical mediation of feudal oaths vanished, secular lords would likely increase extraction, leading to greater instability, more frequent revolts, and a shift towards a more purely power-based feudal system. The balance of power would fundamentally alter.
% FOUNDING_PROBLEM: The problem of arbitrary secular power and unchecked extraction by lords, leading to social instability, injustice, and moral decay within Christian society.
% FOUNDING_PROBLEM_CORROBORATION: Ecclesiastical chronicles and theological treatises from the period consistently attest to the ongoing problem of secular avarice and the need for moral constraints on power. Vassal petitions and legal records also corroborate the need for external mediation against lordly demands.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__ecclesiastical_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__ecclesiastical_mediation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).
:- end_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because while lords are constrained, they still extract substantial resources. Suppression (0.6) is significant due to the Church's active enforcement through spiritual penalties and moral suasion. Theater ratio (0.2) is low, as the Church's intervention is generally genuine, though it also serves to enhance its own institutional power. The temporal measurements show relative stability, indicating a persistent, if contested, balance of power.
 *
 * PERSPECTIVAL GAP:
 *   Secular lords would experience this as a Snare, as their 'natural' right to maximal extraction is suppressed. Vassals would experience it as a Rope, providing essential protection. The Church, as the agenda-setter, would see it as a necessary Tangled Rope, balancing spiritual and temporal needs. The engine will compute these per-seat classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical authorities are beneficiaries (gain moral authority, influence) and agenda-setters (interpret and enforce). Vassals are also beneficiaries (protected from arbitrary extraction). Secular lords are payers (their extractive power is curtailed). The peasantry are indirect payers, bearing the ultimate costs but also benefiting from any stability the system provides.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ecclesiastical_authority_legitimacy,
    'To what extent was the Church''s authority to mediate feudal oaths genuinely accepted by secular lords, versus merely tolerated due to political expediency or fear of spiritual sanctions?',
    'Analysis of historical records detailing instances of secular resistance to ecclesiastical judgments, and the outcomes of such conflicts. Examination of legal treatises on secular vs. spiritual jurisdiction.',
    'If acceptance was primarily pragmatic, the constraint''s suppression is higher than measured, reflecting a greater underlying coercive force. If genuine, the constraint''s coordination function is stronger, reflecting shared normative commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecclesiastical_authority_legitimacy, empirical, 'The true basis of the Church''s authority in mediating feudal oaths.').

omega_variable(
    charity_vs_justice_interpretation,
    'How did the specific interpretation of ''Christian charity'' and ''justice'' by ecclesiastical authorities evolve, and did it consistently favor vassals, or did it sometimes serve to legitimize lordly demands?',
    'Detailed textual analysis of papal bulls, conciliar decrees, and theological commentaries on feudal obligations over time, comparing them with actual legal outcomes and social conditions.',
    'If interpretations consistently favored vassals, the constraint''s extractiveness from lords is higher. If interpretations were flexible or sometimes favored lords, the constraint''s extractiveness is lower, and its coordination function is more ambiguous.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charity_vs_justice_interpretation, conceptual, 'The evolving and potentially biased interpretation of moral principles within the ecclesiastical framework.').

omega_variable(
    natural_law_vs_constructed_constraint,
    'Is the principle of reciprocal obligation in feudal oaths, as mediated by the Church, a reflection of a deeper natural law, or a socially constructed constraint designed to manage power dynamics?',
    'Philosophical and theological debate on the origins of moral and legal obligations, comparing medieval natural law theories with modern sociological analyses of institutional formation.',
    'If a natural law, the constraint''s ''mountain'' aspect is stronger, reducing its perceived extractiveness. If constructed, its ''tangled_rope'' nature is more pronounced, highlighting the active enforcement and power dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_constraint, preference, 'The fundamental nature of the reciprocal obligation principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(feud_tr_t20, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(feud_tr_t40, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(feud_tr_t60, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 60, 0.19).
narrative_ontology:measurement(feud_tr_t80, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement(feud_tr_t100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(feud_be_t20, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(feud_be_t40, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(feud_be_t60, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 60, 0.43).
narrative_ontology:measurement(feud_be_t80, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 80, 0.44).
narrative_ontology:measurement(feud_be_t100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(feud_su_t20, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(feud_su_t40, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(feud_su_t60, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 60, 0.59).
narrative_ontology:measurement(feud_su_t80, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 80, 0.6).
narrative_ontology:measurement(feud_su_t100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__ecclesiastical_mediation_reading, identity_coordination).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity__lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity__vassal_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'feudal_oath_reciprocity' kernel. This 'ecclesiastical mediation' reading emphasizes the Church's role in limiting secular extraction, distinct from readings focused on lordly power or vassal rights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
