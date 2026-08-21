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
 *   human_readable: Feudal Oath Reciprocity: Ecclesiastical Mediation Reading
 *   domain: medieval_political_economy/legal_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'ecclesiastical mediation' reading of the
 *   feudal oath, where Christian charity and sacramental obligations are
 *   understood to limit secular extraction by lords. The Church, through its
 *   moral authority and interpretive power, acts as a mediating force,
 *   shaping the reciprocal obligations between lords and vassals. This
 *   reading posits the feudal oath as a Tangled Rope: it provides a
 *   coordination function (stable social order, vassal security) but also
 *   involves asymmetric extraction (lords' power is constrained, but they
 *   still extract) and requires active enforcement by the Church to maintain
 *   its specific interpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.55).
domain_priors:suppression_score(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.45).
domain_priors:theater_ratio(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, tangled_rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "Feudal Oath Reciprocity: Ecclesiastical Mediation Reading").
narrative_ontology:topic_domain(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "medieval_political_economy/legal_history/institutional_analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'bf19d94d-8126-4430-a663-78ecc59f6e73').
narrative_ontology:cs_kernel_codification('bf19d94d-8126-4430-a663-78ecc59f6e73', formalized).
narrative_ontology:cs_authority_grounding('bf19d94d-8126-4430-a663-78ecc59f6e73', lineage).
narrative_ontology:cs_interpretation_layer_present('bf19d94d-8126-4430-a663-78ecc59f6e73').
narrative_ontology:cs_reading_relation('bf19d94d-8126-4430-a663-78ecc59f6e73', feudal_oath_reciprocity__lord_extraction_reading, influences).
narrative_ontology:cs_reading_relation('bf19d94d-8126-4430-a663-78ecc59f6e73', feudal_oath_reciprocity__vassal_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('bf19d94d-8126-4430-a663-78ecc59f6e73', foundational, christian_charity_limits_secular_power).
narrative_ontology:cs_axiom_status(christian_charity_limits_secular_power, holdable).
narrative_ontology:cs_axiom_grounding('bf19d94d-8126-4430-a663-78ecc59f6e73', christian_charity_limits_secular_power, deontological).
narrative_ontology:cs_axiom('bf19d94d-8126-4430-a663-78ecc59f6e73', foundational, sacramental_oath_binding_divine_law).
narrative_ontology:cs_axiom_status(sacramental_oath_binding_divine_law, holdable).
narrative_ontology:cs_axiom_grounding('bf19d94d-8126-4430-a663-78ecc59f6e73', sacramental_oath_binding_divine_law, theological).
narrative_ontology:cs_reference_frame('bf19d94d-8126-4430-a663-78ecc59f6e73', papal_supremacy_doctrine).
narrative_ontology:cs_drift_state('bf19d94d-8126-4430-a663-78ecc59f6e73', late_medieval_secularization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bf19d94d-8126-4430-a663-78ecc59f6e73', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_authorities).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassals).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, lords).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce the moral and sacramental limits of the feudal oath, leveraging Christian doctrine and the threat of spiritual sanctions (e.g., excommunication). They gain interpretive authority and influence over secular power dynamics.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Bound by their feudal oath, which is now interpreted through the lens of Christian charity and sacramental obligation. This limits their ability to extract maximal resources or arbitrary service from their vassals, incurring a 'cost' in foregone extraction and autonomy.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, lords, payer,
    powerful, biographical, constrained, regional).

% Benefit from the ecclesiastical mediation, which provides a moral and spiritual check on their lord's power, offering a channel for redress against excessive demands and enhancing their security within the feudal contract.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassals, beneficiary,
    moderate, biographical, constrained, local).

% The ultimate producers of wealth in the feudal system, they are not direct parties to the feudal oath or its ecclesiastical interpretation. Their lives are directly affected by the level of extraction permitted, but they have no formal voice in the constraint's operation.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, peasantry, excluded,
    powerless, immediate, trapped, local).

% Analyze the interplay between feudal custom, canon law, and emerging royal legal systems. They document the effects of ecclesiastical mediation but do not directly participate in its enforcement or benefit from its operation.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_legal_scholars, observer,
    analytical, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for reciprocal obligations between lords and vassals, mediated by a higher moral and spiritual authority, preventing arbitrary secular power and fostering a more stable social order.
% TRANSFER_FUNCTION: Transfers interpretive authority and moral leverage from secular lords to ecclesiastical authorities, and limits the potential for arbitrary material extraction from vassals by lords, in exchange for enhanced social stability and spiritual legitimacy.
% ABSENT_VOICES: The peasantry, who are the ultimate source of wealth extracted by the feudal system, have no direct voice in the interpretation or enforcement of the oath, despite being affected by its outcomes. Their grievances are typically channeled through local lords or, in extreme cases, through peasant revolts.
% DISAPPEARANCE_RATIONALE: If the ecclesiastical mediation of the feudal oath vanished overnight, the feudal system would likely revert to a more purely power-based instrument. Lords would face fewer moral or spiritual checks on their extraction, leading to increased arbitrary demands, greater instability, and potentially more frequent conflicts between lords and vassals, as vassals would have fewer avenues for redress beyond direct resistance.
% FOUNDING_PROBLEM: The inherent tension in feudalism between the lord's power and the vassal's need for security, and the potential for secular power to become tyrannical without moral or spiritual checks, leading to instability and injustice.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary chronicles, theological treatises from monastic orders, and writings by independent scholars (e.g., John of Salisbury) attest to the need for moral limits on secular power and the Church's role in upholding justice, supporting the ongoing relevance of this problem.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__ecclesiastical_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__ecclesiastical_mediation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is moderate (0.55) because while the Church's interpretation limits arbitrary secular power, lords still retain significant rights to service and resources. Suppression is moderate (0.45) as the Church's authority, though significant, is not absolute and often faces resistance from secular rulers. The theater ratio is low (0.15) because the Church's role in mediating and interpreting the oath was a genuine and active function, not merely performative. The temporal measurements show a slight increase in extractiveness and suppression over time, reflecting the ongoing tension between ecclesiastical and secular power, and the Church's need to continually assert its authority against encroaching secular claims.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of ecclesiastical authorities and vassals, this constraint functions as a vital mechanism for justice and stability, a form of coordination. From the perspective of lords seeking maximal autonomy and extraction, it is an external imposition that limits their rightful power. The engine's computation of per-seat classification will reflect this divergence based on the declared roles and positional atoms.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical authorities are beneficiaries as they gain interpretive authority and influence, and vassals are beneficiaries as their security is enhanced and extraction is limited. Lords are the targets/payers, as their potential for maximal extraction is curtailed by the Church's interpretation. The peasantry is excluded, bearing the ultimate costs without direct participation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the feudal oath primarily a mechanism for ecclesiastical mediation, maximal lordly extraction, or fixed vassal coordination?',
    'Analysis of primary sources (charters, canon law, chronicles, theological treatises) to determine the dominant interpretive framework and its practical effects on power distribution and resource flows in specific historical contexts.',
    'If the ''lord_extraction_reading'' is dominant, the constraint would be reclassified as a Snare with higher extraction. If the ''vassal_coordination_reading'' is dominant, it would be a Rope with lower extraction. This reading (ecclesiastical mediation) represents a Tangled Rope, balancing coordination and extraction through moral authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity in the primary function and interpretation of the feudal oath.').

omega_variable(
    effectiveness_of_ecclesiastical_mediation,
    'How consistently and effectively did the Church''s mediation limit secular extraction in practice across different regions and periods?',
    'Quantitative historical analysis of legal disputes, land transfers, and tax records, correlated with periods of strong vs. weak ecclesiastical influence, to measure the actual impact of Church intervention on lordly demands.',
    'If the Church''s influence was consistently weak, the effective extractiveness would be higher, pushing the classification closer to a Snare. If it was consistently strong, extractiveness would be lower, closer to a Rope. Variability would highlight the contingent nature of the Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_ecclesiastical_mediation, empirical, 'Practical efficacy of Church mediation in limiting secular power.').

omega_variable(
    internalized_vs_structural_suppression,
    'To what extent did lords genuinely internalize Christian charity and sacramental obligations, versus merely complying due to external pressures (e.g., fear of excommunication, political pressure from the Church)?',
    'Examination of private correspondence, wills, and confessional manuals for evidence of internalized moral frameworks, contrasted with public acts of defiance or strategic compliance, to gauge the proportion of internalized vs. structural suppression.',
    'If suppression was primarily internalized, the constraint''s persistence would be more robust and less reliant on active enforcement. If primarily structural, the constraint would be more brittle and prone to collapse if external enforcement weakened, potentially increasing effective suppression for targets.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism for lords.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 1000, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t1000, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1000, 0.15).
narrative_ontology:measurement(feud_tr_t1050, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1050, 0.15).
narrative_ontology:measurement(feud_tr_t1100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1100, 0.15).
narrative_ontology:measurement(feud_tr_t1150, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1150, 0.15).
narrative_ontology:measurement(feud_tr_t1200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1200, 0.15).
narrative_ontology:measurement(feud_tr_t1250, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1250, 0.15).
narrative_ontology:measurement(feud_tr_t1300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1300, 0.15).

% Extraction over time
narrative_ontology:measurement(feud_be_t1000, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1000, 0.45).
narrative_ontology:measurement(feud_be_t1050, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1050, 0.48).
narrative_ontology:measurement(feud_be_t1100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1100, 0.5).
narrative_ontology:measurement(feud_be_t1150, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1150, 0.52).
narrative_ontology:measurement(feud_be_t1200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1200, 0.53).
narrative_ontology:measurement(feud_be_t1250, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1250, 0.54).
narrative_ontology:measurement(feud_be_t1300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1300, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t1000, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1000, 0.35).
narrative_ontology:measurement(feud_su_t1050, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1050, 0.37).
narrative_ontology:measurement(feud_su_t1100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1100, 0.39).
narrative_ontology:measurement(feud_su_t1150, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1150, 0.41).
narrative_ontology:measurement(feud_su_t1200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1200, 0.42).
narrative_ontology:measurement(feud_su_t1250, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1250, 0.44).
narrative_ontology:measurement(feud_su_t1300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1300, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__ecclesiastical_mediation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, royal_justice_system).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_charter_law).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity__lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity__vassal_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'feudal_oath_reciprocity' kernel. Each reading has a different ε value and structural profile, reflecting different interpretations of the oath's primary function and authority. This 'ecclesiastical_mediation_reading' emphasizes the Church's role in limiting secular power through moral and sacramental obligations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
