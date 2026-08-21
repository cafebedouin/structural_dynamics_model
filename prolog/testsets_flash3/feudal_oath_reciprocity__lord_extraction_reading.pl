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
 *   human_readable: Feudal Oath as Authorization for Maximal Lordly Extraction
 *   domain: medieval_political_economy/legal_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'lord extraction' reading of the feudal
 *   oath of reciprocity, where the oath is interpreted primarily as an
 *   authorization for the lord to extract maximal resources and services from
 *   their vassals and the peasantry. This reading emphasizes the power
 *   asymmetry inherent in the feudal system, where the lord's interpretation
 *   often prevailed, bounded only by the practical limits of vassal capacity
 *   and the risk of open rebellion. The claimed type is 'snare' because the
 *   coordination story (protection) serves as cover for substantial, actively
 *   enforced extraction from identifiable victims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, 0.85).
domain_priors:suppression_score(feudal_oath_reciprocity__lord_extraction_reading, 0.75).
domain_priors:theater_ratio(feudal_oath_reciprocity__lord_extraction_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__lord_extraction_reading, snare).
narrative_ontology:human_readable(feudal_oath_reciprocity__lord_extraction_reading, "Feudal Oath as Authorization for Maximal Lordly Extraction").
narrative_ontology:topic_domain(feudal_oath_reciprocity__lord_extraction_reading, "medieval_political_economy/legal_history/institutional_analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__lord_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__lord_extraction_reading, '7417364f-2941-42bf-866d-11af61b18308').
narrative_ontology:cs_kernel_codification('7417364f-2941-42bf-866d-11af61b18308', formalized).
narrative_ontology:cs_authority_grounding('7417364f-2941-42bf-866d-11af61b18308', extraction).
narrative_ontology:cs_interpretation_layer_present('7417364f-2941-42bf-866d-11af61b18308').
narrative_ontology:cs_reading_relation('7417364f-2941-42bf-866d-11af61b18308', feudal_oath_reciprocity__vassal_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('7417364f-2941-42bf-866d-11af61b18308', feudal_oath_reciprocity__ecclesiastical_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('7417364f-2941-42bf-866d-11af61b18308', foundational, lord_sovereignty_over_vassal_capacity).
narrative_ontology:cs_axiom_status(lord_sovereignty_over_vassal_capacity, holdable).
narrative_ontology:cs_axiom_grounding('7417364f-2941-42bf-866d-11af61b18308', lord_sovereignty_over_vassal_capacity, conventional).
narrative_ontology:cs_axiom('7417364f-2941-42bf-866d-11af61b18308', foundational, oath_as_unilateral_grant_of_power).
narrative_ontology:cs_axiom_status(oath_as_unilateral_grant_of_power, holdable).
narrative_ontology:cs_axiom_grounding('7417364f-2941-42bf-866d-11af61b18308', oath_as_unilateral_grant_of_power, conventional).
narrative_ontology:cs_reference_frame('7417364f-2941-42bf-866d-11af61b18308', maximal_lordly_prerogative).
narrative_ontology:cs_drift_state('7417364f-2941-42bf-866d-11af61b18308', late_medieval_period, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('7417364f-2941-42bf-866d-11af61b18308', '').
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

% The primary beneficiary and enforcer of the oath, interpreting it as a grant of authority for maximal extraction of resources and military service from vassals, limited only by the practical capacity of the land and the risk of rebellion. Benefits directly from the wealth and labor transferred.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, feudal_lord, agenda_setter,
    institutional, generational, arbitrage, regional).

% Bound by the oath to provide military service, labor, and tribute to the lord. Their interpretation of the oath as reciprocal and bounded is often overridden by the lord's power. Exit means forfeiture of land and status, or open rebellion, both with high costs.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, vassals, payer,
    moderate, biographical, constrained, local).

% The ultimate source of labor and resources, subject to the demands of both the lord and their immediate vassal. They have no direct voice in the oath's interpretation and are largely trapped by economic necessity and lack of mobility. Bear the brunt of increased extraction.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, peasantry, payer,
    powerless, immediate, trapped, local).

% Would argue for a more limited, reciprocal interpretation of the oath, bounded by Christian moral principles and the sanctity of oaths before God. Their mediation attempts are often ignored or circumvented by secular lords seeking maximal power, making them effectively excluded from the practical enforcement of this reading.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, ecclesiastical_authorities, excluded,
    institutional, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__lord_extraction_reading, feudal_lord).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__lord_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a hierarchical military and administrative structure, ensuring a supply of armed men and resources for defense and expansion under the lord's command.
% TRANSFER_FUNCTION: Transfers wealth, labor, and military service from vassals and peasantry to the feudal lord, in exchange for protection and the right to hold land.
% ABSENT_VOICES: Ecclesiastical authorities, who would advocate for a more just and bounded interpretation of the oath, are often sidelined. The peasantry, who bear the heaviest burden, have no institutional voice.
% DISAPPEARANCE_RATIONALE: If the feudal oath and its enforcement vanished, the entire social, military, and economic structure of medieval society would collapse. Land tenure, military organization, and resource allocation would immediately reorganize, likely into a state of widespread conflict or new forms of centralized power.
% FOUNDING_PROBLEM: The problem of organizing defense, administration, and resource extraction in a decentralized post-Roman world, where centralized state power was weak or non-existent.
% FOUNDING_PROBLEM_CORROBORATION: While the lordly class would claim the problem is still live (justifying their continued power), historical analysis and the rise of centralized states attest that the original problem of decentralized governance has been superseded. Independent historians and legal scholars corroborate that the system's original function atrophied into a mechanism for rent extraction.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__lord_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__lord_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__lord_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is high (0.85) because the lord's demands often pushed vassals to their limits, beyond what could be considered a fair exchange for protection. Suppression is also high (0.75) due to the lord's military power and legal authority, which actively prevented vassals from seeking alternative allegiances or refusing service. Theater ratio is low (0.15) because the extraction was direct and functional, not primarily performative; the 'protection' aspect was often a genuine, if unevenly distributed, benefit, but the primary function of the oath, under this reading, was to secure resources for the lord. Accessibility collapse is moderate (0.65) as vassals had limited options for exit without severe consequences, and resistance is high (0.70) reflecting the frequent peasant revolts and baronial challenges to lordly power.
 *
 * PERSPECTIVAL GAP:
 *   From the lord's perspective, this reading of the oath is a legitimate exercise of authority necessary for social order and defense. From the vassals' and peasantry's perspective, it is an oppressive system of extraction. The engine's classification will reflect this divergence, computing a snare for the victims and a more beneficial type for the lord, despite the lord's own 'snare-like' behavior.
 *
 * DIRECTIONALITY LOGIC:
 *   The feudal lord is the clear beneficiary and agenda-setter, dictating the terms of the oath's interpretation and enforcement. Vassals and the peasantry are the primary targets, bearing the costs of extraction and having severely constrained exit options. Ecclesiastical authorities, while powerful in other domains, are excluded from effectively mediating this specific interpretation of the oath, as their moral arguments are often overridden by secular power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of decentralized governance became 'dead' as centralized states emerged, but the feudal oath persisted as a mechanism for lordly extraction. This classification as a snare, rather than a piton, reflects that the extraction was still actively pursued and benefited the lord directly, rather than merely persisting through inertia. The 'contested' status of the founding problem highlights the ongoing dispute over the oath's true function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oath_interpretation_authority,
    'Who held the ultimate authority to interpret the feudal oath''s scope and limits: the lord, the vassals, or ecclesiastical courts?',
    'Analysis of legal precedents, court records, and historical outcomes of disputes over feudal obligations. If lordly interpretations consistently prevailed, it supports this reading.',
    'If interpretation was genuinely contested and mediated, the extractiveness and suppression metrics might be lower, pushing the classification towards a Tangled Rope or even a Rope. If the lord''s interpretation was consistently dominant, it reinforces the Snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oath_interpretation_authority, empirical, 'Ambiguity in the locus of interpretive authority for the feudal oath.').

omega_variable(
    rebellion_threshold_elasticity,
    'How elastic was the ''rebellion threshold'' for vassals and peasantry? Did it vary significantly by region, time period, or specific lord''s behavior?',
    'Comparative historical analysis of peasant revolts and baronial rebellions, correlating their frequency and success with levels of lordly extraction and suppression.',
    'A highly elastic and low threshold would imply that the lord''s effective extraction was more constrained by the risk of resistance, potentially lowering the effective extractiveness. A high, inelastic threshold would reinforce the maximal extraction thesis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rebellion_threshold_elasticity, empirical, 'Variability in the practical limits of lordly extraction due to resistance.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the feudal oath primarily a mechanism for hierarchical control and extraction, or a framework for reciprocal, if unequal, coordination?',
    'This is a conceptual omega. Resolution depends on the analytical frame chosen: this ''lord_extraction_reading'' adopts the former. The ''vassal_coordination_reading'' and ''ecclesiastical_mediation_reading'' adopt the latter.',
    'Choosing a different framing would lead to a different claimed_type and different base_properties, resulting in a different classification (e.g., Tangled Rope for the vassal_coordination_reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Conceptual ambiguity in the primary function of the feudal oath.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__lord_extraction_reading, 1000, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t1000, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1000, 0.2).
narrative_ontology:measurement(feud_tr_t1100, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1100, 0.18).
narrative_ontology:measurement(feud_tr_t1200, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1200, 0.16).
narrative_ontology:measurement(feud_tr_t1300, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1300, 0.15).
narrative_ontology:measurement(feud_tr_t1400, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1400, 0.15).

% Extraction over time
narrative_ontology:measurement(feud_be_t1000, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1000, 0.7).
narrative_ontology:measurement(feud_be_t1100, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1100, 0.75).
narrative_ontology:measurement(feud_be_t1200, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1200, 0.8).
narrative_ontology:measurement(feud_be_t1300, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1300, 0.83).
narrative_ontology:measurement(feud_be_t1400, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1400, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t1000, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1000, 0.6).
narrative_ontology:measurement(feud_su_t1100, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1100, 0.65).
narrative_ontology:measurement(feud_su_t1200, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1200, 0.7).
narrative_ontology:measurement(feud_su_t1300, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1300, 0.73).
narrative_ontology:measurement(feud_su_t1400, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1400, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__lord_extraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, feudal_land_tenure_system).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, medieval_military_organization).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'feudal_oath_reciprocity' kernel. Other readings (vassal_coordination_reading, ecclesiastical_mediation_reading) offer alternative interpretations of the oath's function and limits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
