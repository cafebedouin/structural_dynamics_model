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
 *   constraint_id: feudal_oath_reciprocity__lord_extraction_reading
 *   human_readable: Feudal Oath as Lord's Extraction Mechanism
 *   domain: medieval_political_economy/legal_history
 *
 * SUMMARY:
 *   This constraint story analyzes the feudal oath of reciprocity from the
 *   perspective of the 'lord's extraction reading,' where the oath primarily
 *   serves as a mechanism for maximal resource and labor extraction by the
 *   feudal lord, bounded only by the vassals' and peasantry's capacity to
 *   provide service or their threshold for rebellion. It is one reading of
 *   the broader 'feudal_oath_reciprocity' kernel, which also includes
 *   'vassal_coordination_reading' and 'ecclesiastical_mediation_reading.' The
 *   high extractiveness and suppression reflect the lord's structural power.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, 0.85).
domain_priors:suppression_score(feudal_oath_reciprocity__lord_extraction_reading, 0.8).
domain_priors:theater_ratio(feudal_oath_reciprocity__lord_extraction_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__lord_extraction_reading, snare).
narrative_ontology:human_readable(feudal_oath_reciprocity__lord_extraction_reading, "Feudal Oath as Lord's Extraction Mechanism").
narrative_ontology:topic_domain(feudal_oath_reciprocity__lord_extraction_reading, "medieval_political_economy/legal_history").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__lord_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__lord_extraction_reading, '27254e82-e483-468f-be47-d8e05d8e0132').
narrative_ontology:cs_kernel_codification('27254e82-e483-468f-be47-d8e05d8e0132', formalized).
narrative_ontology:cs_authority_grounding('27254e82-e483-468f-be47-d8e05d8e0132', extraction).
narrative_ontology:cs_interpretation_layer_present('27254e82-e483-468f-be47-d8e05d8e0132').
narrative_ontology:cs_reading_relation('27254e82-e483-468f-be47-d8e05d8e0132', feudal_oath_reciprocity__vassal_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('27254e82-e483-468f-be47-d8e05d8e0132', feudal_oath_reciprocity__ecclesiastical_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('27254e82-e483-468f-be47-d8e05d8e0132', foundational, lord_absolute_dominion).
narrative_ontology:cs_axiom_status(lord_absolute_dominion, holdable).
narrative_ontology:cs_axiom_grounding('27254e82-e483-468f-be47-d8e05d8e0132', lord_absolute_dominion, conventional).
narrative_ontology:cs_axiom('27254e82-e483-468f-be47-d8e05d8e0132', secondary, vassal_service_unbounded).
narrative_ontology:cs_axiom_status(vassal_service_unbounded, holdable).
narrative_ontology:cs_axiom_grounding('27254e82-e483-468f-be47-d8e05d8e0132', vassal_service_unbounded, conventional).
narrative_ontology:cs_reference_frame('27254e82-e483-468f-be47-d8e05d8e0132', lordly_prerogative_supremacy).
narrative_ontology:cs_drift_state('27254e82-e483-468f-be47-d8e05d8e0132', late_medieval_period, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('27254e82-e483-468f-be47-d8e05d8e0132', '').
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

% Controls vast tracts of land, military forces, and local justice. Sets the terms of vassalage and extracts resources, labor, and military service from those bound by oath. Justifies this through inherited right and the need for defense and order.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, feudal_lord, agenda_setter,
    institutional, generational, mobile, regional).

% Bound by oaths of fealty, they provide military service, counsel, and financial aid to their lord. They receive land (fiefs) in return, but face increasing demands for resources and service, often beyond the original terms. Exit means forfeiture of land and status, or rebellion.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, vassals, payer,
    powerful, biographical, identity_locked, local).

% Tied to the land and subject to the lord's jurisdiction, they provide labor, agricultural produce, and various dues. They have virtually no legal recourse against excessive demands and face severe penalties for non-compliance or attempted flight. Their existence is defined by the extraction.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, peasantry, payer,
    powerless, immediate, trapped, local).

% Interpret oaths and mediate disputes, often advocating for Christian charity and justice. However, their own institutional power and landholdings are often intertwined with the feudal system, limiting their ability to challenge the lord's extractive practices directly.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, ecclesiastical_authorities, observer,
    institutional, generational, constrained, regional).

% Analyzes the historical dynamics of power, legitimacy, and extraction within the feudal system, identifying the structural mechanisms that enabled lords to maximize their demands.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, analytical_historian, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a decentralized framework for military organization, land tenure, and local governance in an era lacking strong central states, ensuring defense and basic social order.
% TRANSFER_FUNCTION: Moves agricultural surplus, labor, military service, and loyalty from vassals and peasantry to the feudal lord, in exchange for protection and the right to hold land.
% ABSENT_VOICES: Free peasants (where they existed) who resisted enserfment, urban merchants whose economic interests were often at odds with feudal land-based power, and a more centralized royal authority (in early periods) that would later challenge the lords' autonomy.
% DISAPPEARANCE_RATIONALE: If the feudal oath system vanished overnight, the entire social, economic, and military structure of medieval society would collapse. Land tenure, military recruitment, and local justice would cease to function, leading to widespread chaos and a rapid re-ordering of power and social relations.
% FOUNDING_PROBLEM: The collapse of centralized Roman authority left a power vacuum, necessitating local defense and resource mobilization against external threats (e.g., Viking raids, Magyar incursions) and internal disorder, leading to the emergence of localized power structures.
% FOUNDING_PROBLEM_CORROBORATION: Modern historians and legal scholars attest that the original problem of decentralized defense is long gone, replaced by nation-states. The lords' descendants or apologists might claim it's still relevant for 'tradition' but lack external corroboration.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__lord_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__lord_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__lord_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   Extraction is high (0.85) because the lord's demands were often open-ended, limited more by practical capacity than fixed legal terms. Suppression is also high (0.8) due to the lord's control over justice, military force, and the lack of viable alternatives for vassals and especially the peasantry. Theater ratio is low (0.2) because while the rhetoric of mutual obligation and protection existed, the actual function increasingly became one of one-sided extraction, with the 'reciprocity' becoming a thin justification for power. Accessibility collapse is very high (0.88) as vassals are identity-locked to their fiefs and peasants are trapped by serfdom. Resistance is moderate-high (0.7) reflecting the constant tension and occasional revolts, which were the primary check on lordly power.
 *
 * PERSPECTIVAL GAP:
 *   From the feudal lord's perspective, the oath is a legitimate foundation for their authority and the necessary means to maintain order and defense. From the vassals' perspective, it is a burdensome obligation that often exceeds its original intent, while the peasantry experiences it as pure oppression. The engine's per-seat classification will highlight these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   The feudal lord is the clear beneficiary (d=0.0-0.1) as they collect resources and services without bearing equivalent costs. Vassals and peasantry are the primary targets (d=0.8-1.0), bearing the costs of extraction and having severely constrained exit options. Ecclesiastical authorities occupy a more complex position, often mediating but ultimately benefiting from the stability the system provides, placing them closer to symmetric or slight beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The original mandate of the feudal oath was to provide defense and order in a fragmented post-Roman world. Over time, as centralizing monarchies emerged and external threats diminished, this founding problem became 'dead.' However, the constraint persisted, with the 'reciprocity' narrative becoming a theatrical cover for continued extraction. The system became a 'snare' where the original coordination function atrophied, but the extractive mechanism remained, maintained by active enforcement and suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oath_reciprocity_vs_extraction,
    'To what extent was the feudal oath genuinely reciprocal, and to what extent was it a cover for one-sided extraction?',
    'Detailed analysis of historical charters, legal codes, and chronicles to quantify the balance of obligations and actual enforcement outcomes over time, comparing stated intent with observed practice.',
    'If more reciprocal, the constraint might lean towards a Tangled Rope; if predominantly extractive, it reinforces the Snare classification. This reading assumes the latter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oath_reciprocity_vs_extraction, conceptual, 'Ambiguity between stated reciprocity and actual extractive function.').

omega_variable(
    rebellion_threshold_variability,
    'What was the actual threshold of extraction or oppression that triggered widespread vassal or peasant rebellion in different regions and periods?',
    'Comparative historical analysis of peasant revolts and baronial rebellions, correlating their occurrence with specific increases in taxation, labor demands, or infringements on customary rights.',
    'A lower, more consistent rebellion threshold would indicate stronger, albeit violent, checks on lordly power, potentially reducing the effective extractiveness. A higher, more variable threshold would reinforce the ''maximal extraction'' claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rebellion_threshold_variability, empirical, 'Empirical variability of the ''rebellion threshold'' as a limit on extraction.').

omega_variable(
    ecclesiastical_influence_strength,
    'How effective were ecclesiastical authorities in limiting secular lords'' extractive practices through moral suasion, legal intervention, or spiritual threats?',
    'Case studies of specific interventions by the Church, analyzing their outcomes in terms of reduced demands, negotiated settlements, or lordly defiance, across different regions and periods.',
    'Stronger ecclesiastical influence would suggest a more constrained extractive environment, potentially shifting the classification towards a Tangled Rope or even a degraded Rope. Weak influence reinforces the Snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ecclesiastical_influence_strength, empirical, 'The actual power of the Church to constrain lordly extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__lord_extraction_reading, 800, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t800, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 800, 0.35).
narrative_ontology:measurement(feud_tr_t920, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 920, 0.3).
narrative_ontology:measurement(feud_tr_t1040, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1040, 0.25).
narrative_ontology:measurement(feud_tr_t1160, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1160, 0.22).
narrative_ontology:measurement(feud_tr_t1280, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1280, 0.2).
narrative_ontology:measurement(feud_tr_t1400, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1400, 0.2).

% Extraction over time
narrative_ontology:measurement(feud_be_t800, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 800, 0.65).
narrative_ontology:measurement(feud_be_t920, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 920, 0.72).
narrative_ontology:measurement(feud_be_t1040, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1040, 0.78).
narrative_ontology:measurement(feud_be_t1160, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1160, 0.82).
narrative_ontology:measurement(feud_be_t1280, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1280, 0.85).
narrative_ontology:measurement(feud_be_t1400, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1400, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t800, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 800, 0.6).
narrative_ontology:measurement(feud_su_t920, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 920, 0.68).
narrative_ontology:measurement(feud_su_t1040, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1040, 0.75).
narrative_ontology:measurement(feud_su_t1160, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1160, 0.78).
narrative_ontology:measurement(feud_su_t1280, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1280, 0.8).
narrative_ontology:measurement(feud_su_t1400, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1400, 0.8).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=800, tn=1400
narrative_ontology:measurement(feud_grid_01, feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse(class), 800, 0.85).
narrative_ontology:measurement(feud_grid_02, feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse(class), 1400, 0.55).
narrative_ontology:measurement(feud_grid_03, feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse(individual), 800, 0.9).
narrative_ontology:measurement(feud_grid_04, feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse(individual), 1400, 0.6).
narrative_ontology:measurement(feud_grid_05, feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse(organizational), 800, 0.7).
narrative_ontology:measurement(feud_grid_06, feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse(organizational), 1400, 0.5).
narrative_ontology:measurement(feud_grid_07, feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse(structural), 800, 0.8).
narrative_ontology:measurement(feud_grid_08, feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse(structural), 1400, 0.5).
narrative_ontology:measurement(feud_grid_09, feudal_oath_reciprocity__lord_extraction_reading, resistance(class), 800, 0.05).
narrative_ontology:measurement(feud_grid_10, feudal_oath_reciprocity__lord_extraction_reading, resistance(class), 1400, 0.7).
narrative_ontology:measurement(feud_grid_11, feudal_oath_reciprocity__lord_extraction_reading, resistance(individual), 800, 0.1).
narrative_ontology:measurement(feud_grid_12, feudal_oath_reciprocity__lord_extraction_reading, resistance(individual), 1400, 0.4).
narrative_ontology:measurement(feud_grid_13, feudal_oath_reciprocity__lord_extraction_reading, resistance(organizational), 800, 0.15).
narrative_ontology:measurement(feud_grid_14, feudal_oath_reciprocity__lord_extraction_reading, resistance(organizational), 1400, 0.45).
narrative_ontology:measurement(feud_grid_15, feudal_oath_reciprocity__lord_extraction_reading, resistance(structural), 800, 0.08).
narrative_ontology:measurement(feud_grid_16, feudal_oath_reciprocity__lord_extraction_reading, resistance(structural), 1400, 0.65).
narrative_ontology:measurement(feud_grid_17, feudal_oath_reciprocity__lord_extraction_reading, stakes_inflation(class), 800, 0.8).
narrative_ontology:measurement(feud_grid_18, feudal_oath_reciprocity__lord_extraction_reading, stakes_inflation(class), 1400, 0.62).
narrative_ontology:measurement(feud_grid_19, feudal_oath_reciprocity__lord_extraction_reading, stakes_inflation(individual), 800, 0.85).
narrative_ontology:measurement(feud_grid_20, feudal_oath_reciprocity__lord_extraction_reading, stakes_inflation(individual), 1400, 0.65).
narrative_ontology:measurement(feud_grid_21, feudal_oath_reciprocity__lord_extraction_reading, stakes_inflation(organizational), 800, 0.75).
narrative_ontology:measurement(feud_grid_22, feudal_oath_reciprocity__lord_extraction_reading, stakes_inflation(organizational), 1400, 0.6).
narrative_ontology:measurement(feud_grid_23, feudal_oath_reciprocity__lord_extraction_reading, stakes_inflation(structural), 800, 0.78).
narrative_ontology:measurement(feud_grid_24, feudal_oath_reciprocity__lord_extraction_reading, stakes_inflation(structural), 1400, 0.6).
narrative_ontology:measurement(feud_grid_25, feudal_oath_reciprocity__lord_extraction_reading, suppression(class), 800, 0.75).
narrative_ontology:measurement(feud_grid_26, feudal_oath_reciprocity__lord_extraction_reading, suppression(class), 1400, 0.62).
narrative_ontology:measurement(feud_grid_27, feudal_oath_reciprocity__lord_extraction_reading, suppression(individual), 800, 0.8).
narrative_ontology:measurement(feud_grid_28, feudal_oath_reciprocity__lord_extraction_reading, suppression(individual), 1400, 0.65).
narrative_ontology:measurement(feud_grid_29, feudal_oath_reciprocity__lord_extraction_reading, suppression(organizational), 800, 0.7).
narrative_ontology:measurement(feud_grid_30, feudal_oath_reciprocity__lord_extraction_reading, suppression(organizational), 1400, 0.6).
narrative_ontology:measurement(feud_grid_31, feudal_oath_reciprocity__lord_extraction_reading, suppression(structural), 800, 0.72).
narrative_ontology:measurement(feud_grid_32, feudal_oath_reciprocity__lord_extraction_reading, suppression(structural), 1400, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__lord_extraction_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
