% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__lord_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__lord_extraction_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: feudal_oath_reciprocity__lord_extraction_reading
 *   human_readable: Feudal Oath: Lord's Maximal Extraction Reading
 *   domain: medieval_political_economy/legal_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'lord's maximal extraction' reading of the
 *   feudal oath, where the oath is interpreted as authorizing the lord to
 *   extract as much as possible from vassals and the peasantry, bounded only
 *   by their capacity to provide service or the threshold of open rebellion.
 *   It is a snare from the perspective of the vassals and peasantry, as the
 *   coordination story (protection, order) serves as a cover for continuous,
 *   asymmetric extraction. This reading contrasts sharply with
 *   interpretations emphasizing reciprocal, bounded obligations or
 *   ecclesiastical mediation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, 0.85).
domain_priors:suppression_score(feudal_oath_reciprocity__lord_extraction_reading, 0.9).
domain_priors:theater_ratio(feudal_oath_reciprocity__lord_extraction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__lord_extraction_reading, snare).
narrative_ontology:human_readable(feudal_oath_reciprocity__lord_extraction_reading, "Feudal Oath: Lord's Maximal Extraction Reading").
narrative_ontology:topic_domain(feudal_oath_reciprocity__lord_extraction_reading, "medieval_political_economy/legal_history/institutional_analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__lord_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__lord_extraction_reading, 'ff76e34c-cc1b-4b2a-b201-2eaae2e7625d').
narrative_ontology:cs_kernel_codification('ff76e34c-cc1b-4b2a-b201-2eaae2e7625d', formalized).
narrative_ontology:cs_authority_grounding('ff76e34c-cc1b-4b2a-b201-2eaae2e7625d', extraction).
narrative_ontology:cs_interpretation_layer_present('ff76e34c-cc1b-4b2a-b201-2eaae2e7625d').
narrative_ontology:cs_reading_relation('ff76e34c-cc1b-4b2a-b201-2eaae2e7625d', feudal_oath_reciprocity__vassal_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('ff76e34c-cc1b-4b2a-b201-2eaae2e7625d', feudal_oath_reciprocity__ecclesiastical_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('ff76e34c-cc1b-4b2a-b201-2eaae2e7625d', foundational, lord_holds_absolute_dominion).
narrative_ontology:cs_axiom_status(lord_holds_absolute_dominion, holdable).
narrative_ontology:cs_axiom_grounding('ff76e34c-cc1b-4b2a-b201-2eaae2e7625d', lord_holds_absolute_dominion, conventional).
narrative_ontology:cs_axiom('ff76e34c-cc1b-4b2a-b201-2eaae2e7625d', foundational, vassal_service_is_elastic).
narrative_ontology:cs_axiom_status(vassal_service_is_elastic, holdable).
narrative_ontology:cs_axiom_grounding('ff76e34c-cc1b-4b2a-b201-2eaae2e7625d', vassal_service_is_elastic, empirically_contingent).
narrative_ontology:cs_reference_frame('ff76e34c-cc1b-4b2a-b201-2eaae2e7625d', lordly_prerogative_and_custom).
narrative_ontology:cs_drift_state('ff76e34c-cc1b-4b2a-b201-2eaae2e7625d', late_medieval_period, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('ff76e34c-cc1b-4b2a-b201-2eaae2e7625d', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, feudal_lord).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, vassals).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, peasantry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary beneficiary and enforcer of the feudal oath, interpreting it as a grant of maximal authority to extract resources and service from vassals and the peasantry. The lord's power is limited only by the practical capacity of vassals to resist or provide service, and the threat of rebellion.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, feudal_lord, agenda_setter,
    institutional, generational, arbitrage, regional).

% Bound by the oath to provide military service, counsel, and financial aid to the lord. They experience the oath as a mechanism for continuous extraction, with their obligations expanding to meet the lord's demands. Exit means forfeiture of land and status, often leading to destitution or death.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, vassals, payer,
    powerful, biographical, trapped, local).

% The ultimate source of labor and produce, subject to the extraction demands passed down through the vassals. They have no direct voice in the oath's interpretation and are entirely dependent on the feudal structure for protection, however meager. Exit is virtually impossible, leading to starvation or banditry.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, peasantry, payer,
    powerless, immediate, trapped, local).

% While theoretically holding moral authority over oaths, their interpretation of the feudal oath as bounded by Christian charity is often ignored or actively suppressed by secular lords who prioritize maximal extraction. They are excluded from the practical enforcement of the oath's secular interpretation.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, ecclesiastical_authorities, excluded,
    institutional, generational, constrained, regional).

% Analyze historical legal texts and practices to understand the various interpretations and applications of feudal oaths. They can identify the structural mechanisms of extraction and the limits of resistance within the feudal system.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The oath provides a framework for military mobilization and resource allocation within a hierarchical society, ensuring a degree of order and defense against external threats.
% TRANSFER_FUNCTION: Transfers land, labor, military service, and financial aid from vassals and the peasantry to the feudal lord, in exchange for protection and the maintenance of social order.
% ABSENT_VOICES: The peasantry, who bear the brunt of the extraction, have no voice. Ecclesiastical authorities, who would argue for moral limits on extraction, are often sidelined. Their absence allows the lord's maximalist interpretation to dominate.
% DISAPPEARANCE_RATIONALE: If the feudal oath and its enforcement vanished, the entire social and political structure of medieval society would collapse. Land tenure, military organization, and economic production would cease to function, leading to widespread chaos and the rapid emergence of new power structures.
% FOUNDING_PROBLEM: The problem of organizing a decentralized society for defense and resource management in an era of constant warfare and limited state capacity, establishing a hierarchical system of reciprocal obligations.
% FOUNDING_PROBLEM_CORROBORATION: While the original problem of defense and order was real, the maximal extraction reading of the oath has long outlived its functional necessity. Historical records, legal commentaries from later periods, and the eventual rise of centralized states corroborate that the oath's primary function shifted from coordination to rent-seeking, with the original problem largely solved or superseded by new institutional forms.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__lord_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__lord_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__lord_extraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(feudal_oath_reciprocity__lord_extraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__lord_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feudal_oath_reciprocity__lord_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.85) because the lord's demands are elastic and expand to meet the vassals' capacity, leaving little surplus. Suppression is also very high (0.90) due to the severe penalties for non-compliance (forfeiture, death) and the lack of viable exit options for vassals and peasantry. Theater ratio is low (0.10) because the enforcement is direct and brutal, with little performative pretense; the coordination function is real but secondary to the extraction. Resistance is high (0.75) reflecting frequent, though often localized and suppressed, peasant revolts and baronial challenges to over-extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the lord's perspective, the oath is a legitimate mechanism for maintaining power and securing resources, a 'rope' of necessary social order. From the vassals' and peasantry's perspective, it is a 'snare' of relentless extraction. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The feudal lord is the clear beneficiary and agenda-setter, interpreting and enforcing the oath to their maximal advantage (d=0.0-0.1). Vassals and the peasantry are the primary targets, bearing the full weight of extraction with minimal recourse (d=0.9-1.0). Ecclesiastical authorities, though nominally powerful, are structurally excluded from this reading's practical operation, making them an 'excluded' seat with high d.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (organizing defense and order) became increasingly 'dead' as centralized states emerged, but the extraction mechanism persisted. This reading of the oath, therefore, represents a snare that outlived its original coordination mandate, becoming a pure rent-seeking device. The high extractiveness and suppression, coupled with the 'dead' founding problem status, prevent mislabeling this as a genuine coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maximal_extraction_vs_bounded_service,
    'Is the feudal oath fundamentally a grant of maximal, elastic extraction, or does it imply fixed, bounded obligations of service?',
    'Analysis of historical legal charters and customary law: if charters consistently specify fixed obligations, it supports the bounded service reading; if they are vague or consistently overridden by lordly demands, it supports maximal extraction.',
    'If maximal extraction is the true reading, the constraint is a snare. If bounded service is the true reading, it would be closer to a tangled rope or even a rope, with lower extractiveness and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(maximal_extraction_vs_bounded_service, empirical, 'Ambiguity in the scope of feudal obligations.').

omega_variable(
    ecclesiastical_influence_efficacy,
    'To what extent did ecclesiastical interpretations of the oath (emphasizing charity and moral limits) actually constrain secular lords'' extractive practices?',
    'Case studies of specific disputes involving church intervention: if church mediation consistently reduced extraction, the ecclesiastical reading had practical force; if it was routinely ignored, this reading''s influence was minimal.',
    'If ecclesiastical influence was significant, the effective suppression and extractiveness would be lower, pushing the constraint towards a tangled rope. If minimal, this snare reading is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecclesiastical_influence_efficacy, empirical, 'Efficacy of moral/religious constraints on secular power.').

omega_variable(
    rebellion_threshold_elasticity,
    'How elastic was the ''rebellion threshold'' for vassals and peasantry, and did lords consistently push extraction to that limit?',
    'Quantitative historical analysis of tax burdens, service demands, and frequency/severity of revolts: a consistent pattern of extraction rising to meet the threshold of widespread unrest would corroborate the maximal extraction reading.',
    'If the threshold was highly elastic and consistently met, it reinforces the high extractiveness and suppression. If lords often left significant surplus, it suggests a more bounded system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rebellion_threshold_elasticity, empirical, 'The true practical limit of extraction before revolt.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__lord_extraction_reading, 1000, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t1000, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1000, 0.2).
narrative_ontology:measurement(feud_tr_t1100, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1100, 0.15).
narrative_ontology:measurement(feud_tr_t1200, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(feud_tr_t1300, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1300, 0.08).
narrative_ontology:measurement(feud_tr_t1400, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1400, 0.1).

% Extraction over time
narrative_ontology:measurement(feud_be_t1000, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1000, 0.7).
narrative_ontology:measurement(feud_be_t1100, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1100, 0.78).
narrative_ontology:measurement(feud_be_t1200, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1200, 0.85).
narrative_ontology:measurement(feud_be_t1300, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1300, 0.88).
narrative_ontology:measurement(feud_be_t1400, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1400, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t1000, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1000, 0.75).
narrative_ontology:measurement(feud_su_t1100, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1100, 0.82).
narrative_ontology:measurement(feud_su_t1200, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1200, 0.9).
narrative_ontology:measurement(feud_su_t1300, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1300, 0.92).
narrative_ontology:measurement(feud_su_t1400, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1400, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__lord_extraction_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'feudal_oath_reciprocity' kernel, focusing on the lord's maximal extraction. It is linked to other readings (vassal_coordination_reading, ecclesiastical_mediation_reading) which offer alternative interpretations of the same underlying commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
