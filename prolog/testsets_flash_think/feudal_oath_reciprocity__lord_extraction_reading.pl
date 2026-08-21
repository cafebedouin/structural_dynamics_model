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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: feudal_oath_reciprocity__lord_extraction_reading
 *   human_readable: Feudal Oath as Lord's Maximal Extraction Mechanism
 *   domain: medieval_political_economy/legal_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'lord_extraction_reading' of the feudal
 *   oath reciprocity kernel. It describes the feudal oath not primarily as a
 *   reciprocal coordination mechanism, but as a structural authorization for
 *   maximal extraction by the feudal lord, bounded only by the vassals'
 *   capacity for service and their collective threshold for rebellion. The
 *   oath's language of loyalty and mutual aid serves as a theatrical cover
 *   for a fundamentally extractive arrangement, actively enforced through
 *   military power and legal custom.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, 0.85).
domain_priors:suppression_score(feudal_oath_reciprocity__lord_extraction_reading, 0.78).
domain_priors:theater_ratio(feudal_oath_reciprocity__lord_extraction_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__lord_extraction_reading, snare).
narrative_ontology:human_readable(feudal_oath_reciprocity__lord_extraction_reading, "Feudal Oath as Lord's Maximal Extraction Mechanism").
narrative_ontology:topic_domain(feudal_oath_reciprocity__lord_extraction_reading, "medieval_political_economy/legal_history/institutional_analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__lord_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__lord_extraction_reading, '582ec838-554d-4c18-88d6-565a2c22e94d').
narrative_ontology:cs_kernel_codification('582ec838-554d-4c18-88d6-565a2c22e94d', fixed_text).
narrative_ontology:cs_authority_grounding('582ec838-554d-4c18-88d6-565a2c22e94d', extraction).
narrative_ontology:cs_interpretation_layer_present('582ec838-554d-4c18-88d6-565a2c22e94d').
narrative_ontology:cs_reading_relation('582ec838-554d-4c18-88d6-565a2c22e94d', feudal_oath_reciprocity__ecclesiastical_mediation_reading, coexists_with).
narrative_ontology:cs_reading_relation('582ec838-554d-4c18-88d6-565a2c22e94d', feudal_oath_reciprocity__vassal_coordination_reading, forecloses).
narrative_ontology:cs_axiom('582ec838-554d-4c18-88d6-565a2c22e94d', foundational, lord_holds_absolute_dominion).
narrative_ontology:cs_axiom_status(lord_holds_absolute_dominion, holdable).
narrative_ontology:cs_axiom_grounding('582ec838-554d-4c18-88d6-565a2c22e94d', lord_holds_absolute_dominion, conventional).
narrative_ontology:cs_axiom('582ec838-554d-4c18-88d6-565a2c22e94d', secondary, vassal_service_is_unbounded_by_text).
narrative_ontology:cs_axiom_status(vassal_service_is_unbounded_by_text, holdable).
narrative_ontology:cs_axiom_grounding('582ec838-554d-4c18-88d6-565a2c22e94d', vassal_service_is_unbounded_by_text, conventional).
narrative_ontology:cs_reference_frame('582ec838-554d-4c18-88d6-565a2c22e94d', lordly_prerogative_maximal_extraction).
narrative_ontology:cs_drift_state('582ec838-554d-4c18-88d6-565a2c22e94d', late_medieval_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('582ec838-554d-4c18-88d6-565a2c22e94d', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, feudal_lord).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, vassals).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, peasantry).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__lord_extraction_reading, divine_right_of_lords).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__lord_extraction_reading, feudal_hierarchy_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary beneficiary and enforcer of the feudal oath, interpreting it as authorization for maximal extraction of services, military aid, and tribute from vassals and their lands. Their power is bounded only by the collective capacity for rebellion among their vassals.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, feudal_lord, agenda_setter,
    institutional, generational, mobile, regional).

% Bound by the oath to provide military service, counsel, and financial aid to their lord. They experience the oath as a mechanism for substantial extraction, often exceeding the explicit terms of their charters, with exit options limited to rebellion or seeking protection from a rival lord.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, vassals, payer,
    organized, biographical, constrained, local).

% The ultimate bearers of the extraction, providing labor, produce, and taxes to their immediate lord, who in turn serves the feudal lord. They have virtually no voice in the interpretation of the oath and are trapped by economic dependency and lack of mobility.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, peasantry, payer,
    powerless, immediate, trapped, local).

% Claim moral and spiritual authority over the sanctity of oaths, often advocating for limits on secular power based on Christian charity and justice. From the lord's extraction reading, their influence is a competing moral claim, often circumvented or ignored in practice.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, ecclesiastical_authorities, observer,
    institutional, generational, analytical, continental).

% Interprets historical records and legal texts to understand the structural function of the feudal oath, often identifying the gap between its stated reciprocal ideals and its practical operation as a mechanism for power consolidation and resource extraction.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, analytical_historian, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__lord_extraction_reading, feudal_lord).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__lord_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a hierarchical framework for military organization, land tenure, and local governance in a decentralized medieval state, ostensibly ensuring mutual protection and loyalty.
% TRANSFER_FUNCTION: Moves military service, labor, agricultural produce, and financial tribute from vassals and the peasantry to the feudal lord, in exchange for protection and land tenure.
% ABSENT_VOICES: The peasantry, who bore the brunt of the extraction, had no formal voice in the interpretation or enforcement of the oath. Early proponents of centralized royal authority would also object to the decentralized, extractive power it granted to feudal lords.
% DISAPPEARANCE_RATIONALE: If the feudal oath and its enforcement vanished overnight, the entire social, military, and economic structure of medieval society would collapse. Land tenure would become contested, military organization would dissolve, and widespread conflict would ensue, forcing a complete reorganization of power and governance.
% FOUNDING_PROBLEM: The need to establish a stable system of military service, land allocation, and local governance in the absence of a strong, centralized state following the collapse of empires.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary chronicles and legal texts from the lordly perspective attest to the problem of disorder and the oath's role in establishing order. Modern historians and legal scholars (outside the benefiting parties) corroborate the historical problem but dispute the 'solution's' true function, highlighting its inherent extractive and coercive nature beyond mere coordination.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__lord_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__lord_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__lord_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(feudal_oath_reciprocity__lord_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__lord_extraction_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extraction is high (0.85) because the lord's demands were often open-ended, limited only by what could be extracted without provoking revolt. Suppression (0.78) is substantial, as the system relied on military force and the lack of viable alternatives for vassals and peasantry. Theater ratio (0.4) reflects the performative aspects of loyalty and reciprocal duty, which masked the underlying power asymmetry. Accessibility collapse is high (0.8) as vassals were tied to their land and the system, with few legitimate exit options. Resistance (0.6) is moderate, manifesting as local revolts or collective bargaining, but rarely challenging the system's core legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   From the feudal lord's perspective, the oath is a legitimate exercise of authority, ensuring order and necessary resources. From the vassals' and especially the peasantry's perspective, it is a coercive mechanism for resource transfer, maintained by force and tradition. The engine's per-seat classification will highlight this divergence, with the lord as a beneficiary and vassals/peasantry as targets.
 *
 * DIRECTIONALITY LOGIC:
 *   The feudal lord is the clear beneficiary (d near 0.0), receiving military service, labor, and tribute. Vassals are primary targets (d near 1.0), bearing the direct costs of service and extraction. The peasantry are also targets, bearing the ultimate burden of labor and produce. Ecclesiastical authorities, while claiming moral oversight, are largely observers in this reading, their influence often secondary to the lord's power.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the feudal oath as a pure 'rope' or 'scaffold' by explicitly identifying the high extraction and suppression inherent in its operation from the lord's perspective. It highlights how the coordination narrative (mutual protection) served as a cover for a system designed to concentrate resources and power, rather than a temporary support or a purely beneficial arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oath_as_coordination_vs_extraction,
    'Was the feudal oath primarily a genuine coordination mechanism for mutual defense and governance, or fundamentally a tool for maximal extraction by the lord?',
    'Comparative historical analysis of charter texts versus actual demands and practices, examining the frequency and success of vassal resistance to ''unjust'' demands.',
    'If primarily coordination, the extractiveness and suppression metrics would be lower, potentially reclassifying it as a Tangled Rope or even a Rope. If primarily extraction, the Snare classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oath_as_coordination_vs_extraction, empirical, 'Ambiguity regarding the primary function of the feudal oath.').

omega_variable(
    ecclesiastical_influence_on_extraction,
    'To what extent did ecclesiastical authority and the moral weight of the sacramental oath genuinely limit the lord''s extractive capacity in practice?',
    'Analysis of church court records, papal pronouncements, and instances where ecclesiastical sanctions were applied against lords for excessive demands, and their practical effect.',
    'If ecclesiastical influence was substantial, the effective extractiveness would be lower than measured, as an external constraint on the lord''s power existed. If negligible, the maximal extraction reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecclesiastical_influence_on_extraction, empirical, 'The actual impact of religious/moral constraints on feudal extraction.').

omega_variable(
    rebellion_threshold_as_limit,
    'Was the ''rebellion threshold'' a true, active limit on extraction, or merely a theoretical boundary rarely tested due to the high costs of resistance?',
    'Quantitative historical analysis of the frequency, scale, and success rate of vassal rebellions in response to perceived over-extraction, compared to the frequency of such demands.',
    'If frequently tested and successful, it implies a more dynamic, negotiated constraint on extraction. If rarely tested, it reinforces the high suppression and trapped exit options for vassals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rebellion_threshold_as_limit, empirical, 'The practical efficacy of vassal rebellion as a limit on lordly extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__lord_extraction_reading, 800, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t800, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 800, 0.5).
narrative_ontology:measurement(feud_tr_t900, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 900, 0.45).
narrative_ontology:measurement(feud_tr_t1000, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1000, 0.4).
narrative_ontology:measurement(feud_tr_t1100, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1100, 0.38).
narrative_ontology:measurement(feud_tr_t1200, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1200, 0.4).
narrative_ontology:measurement(feud_tr_t1300, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1300, 0.4).

% Extraction over time
narrative_ontology:measurement(feud_be_t800, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 800, 0.7).
narrative_ontology:measurement(feud_be_t900, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 900, 0.75).
narrative_ontology:measurement(feud_be_t1000, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1000, 0.8).
narrative_ontology:measurement(feud_be_t1100, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1100, 0.83).
narrative_ontology:measurement(feud_be_t1200, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1200, 0.85).
narrative_ontology:measurement(feud_be_t1300, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1300, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t800, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 800, 0.65).
narrative_ontology:measurement(feud_su_t900, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 900, 0.7).
narrative_ontology:measurement(feud_su_t1000, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1000, 0.75).
narrative_ontology:measurement(feud_su_t1100, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1100, 0.77).
narrative_ontology:measurement(feud_su_t1200, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1200, 0.78).
narrative_ontology:measurement(feud_su_t1300, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1300, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__lord_extraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, manorial_system_obligations).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, divine_right_monarchy_legitimacy).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, feudal_land_tenure_system).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
