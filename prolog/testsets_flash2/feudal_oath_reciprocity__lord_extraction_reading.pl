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
 *   oath of reciprocity, where the oath is interpreted primarily as a
 *   mechanism authorizing maximal extraction of resources and service from
 *   vassals by the lord. The extraction is bounded only by the practical
 *   limits of vassal capacity and the risk of open rebellion, rather than by
 *   fixed reciprocal obligations or moral limits. This reading emphasizes the
 *   coercive power dynamic inherent in the feudal system.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, 0.85).
domain_priors:suppression_score(feudal_oath_reciprocity__lord_extraction_reading, 0.75).
domain_priors:theater_ratio(feudal_oath_reciprocity__lord_extraction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__lord_extraction_reading, snare).
narrative_ontology:human_readable(feudal_oath_reciprocity__lord_extraction_reading, "Feudal Oath as Authorization for Maximal Lordly Extraction").
narrative_ontology:topic_domain(feudal_oath_reciprocity__lord_extraction_reading, "medieval_political_economy/legal_history/institutional_analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__lord_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__lord_extraction_reading, 'b3f24153-fce7-402e-9513-22fbbe1c6c70').
narrative_ontology:cs_kernel_codification('b3f24153-fce7-402e-9513-22fbbe1c6c70', formalized).
narrative_ontology:cs_authority_grounding('b3f24153-fce7-402e-9513-22fbbe1c6c70', extraction).
narrative_ontology:cs_interpretation_layer_present('b3f24153-fce7-402e-9513-22fbbe1c6c70').
narrative_ontology:cs_reading_relation('b3f24153-fce7-402e-9513-22fbbe1c6c70', feudal_oath_reciprocity__vassal_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('b3f24153-fce7-402e-9513-22fbbe1c6c70', feudal_oath_reciprocity__ecclesiastical_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('b3f24153-fce7-402e-9513-22fbbe1c6c70', foundational, lord_sovereignty_over_vassal_obligations).
narrative_ontology:cs_axiom_status(lord_sovereignty_over_vassal_obligations, holdable).
narrative_ontology:cs_axiom_grounding('b3f24153-fce7-402e-9513-22fbbe1c6c70', lord_sovereignty_over_vassal_obligations, conventional).
narrative_ontology:cs_axiom('b3f24153-fce7-402e-9513-22fbbe1c6c70', secondary, extraction_limited_only_by_rebellion_risk).
narrative_ontology:cs_axiom_status(extraction_limited_only_by_rebellion_risk, holdable).
narrative_ontology:cs_axiom_grounding('b3f24153-fce7-402e-9513-22fbbe1c6c70', extraction_limited_only_by_rebellion_risk, empirically_contingent).
narrative_ontology:cs_reference_frame('b3f24153-fce7-402e-9513-22fbbe1c6c70', lordly_prerogative_maximal_extraction).
narrative_ontology:cs_drift_state('b3f24153-fce7-402e-9513-22fbbe1c6c70', late_medieval_period, gap(stable, minor, false)).
narrative_ontology:cs_created_at('b3f24153-fce7-402e-9513-22fbbe1c6c70', '').
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

% The primary beneficiary and enforcer of the oath, interpreting it as a grant of authority for maximal extraction of resources and service from vassals, limited only by the practical capacity of the land and the risk of rebellion. Benefits directly from all transfers.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, feudal_lord, agenda_setter,
    institutional, generational, arbitrage, regional).

% Bound by the oath to provide military service, counsel, and financial aid to the lord. Their identity and social standing are tied to their feudal relationship, making exit (breaking the oath) a profound act of social and political suicide, often leading to forfeiture and violence. They bear the brunt of the lord's extractive demands.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, vassals, payer,
    moderate, biographical, identity_locked, local).

% The ultimate source of labor and produce, from whom vassals extract to meet their obligations to the lord. They have virtually no exit options, being tied to the land and subject to the combined extractive power of both lord and vassal. Their costs are maximal.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, peasantry, payer,
    powerless, immediate, trapped, local).

% While theoretically able to mediate or condemn breaches of Christian charity, their actual power to limit secular extraction based on the oath is often constrained by the lord's military power and political influence. Their voice is often sidelined in favor of secular interpretations.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, ecclesiastical_authorities, excluded,
    institutional, generational, constrained, national).

% Analyze historical legal texts and practices to understand the actual operation of feudal oaths. They can identify discrepancies between stated ideals and practical realities of power and extraction.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a hierarchical military and administrative structure for defense and governance, ensuring a supply of armed men and resources for the lord's campaigns and territorial control.
% TRANSFER_FUNCTION: Moves military service, labor, agricultural produce, and financial aid from vassals and peasantry to the feudal lord, in exchange for protection and the right to hold land.
% ABSENT_VOICES: The peasantry, who bear the heaviest burden, have no formal voice in the interpretation or enforcement of the oath. Ecclesiastical authorities, who would argue for moral limits on extraction, are often excluded from effective arbitration by secular power.
% DISAPPEARANCE_RATIONALE: If the feudal oath and its enforcement vanished, the entire medieval social, military, and economic order would collapse. Lords would lose their authority and resources, vassals their land and status, and the peasantry their (limited) protection, leading to widespread chaos and the rapid emergence of new power structures.
% FOUNDING_PROBLEM: The problem of organizing defense, governance, and resource allocation in a decentralized, post-Roman world lacking strong central states.
% FOUNDING_PROBLEM_CORROBORATION: Historical consensus among legal historians and medievalists confirms the founding problem was real. However, the problem of decentralized governance has been largely superseded by the rise of nation-states, rendering the feudal solution obsolete, even if its extractive legacy persisted for centuries.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__lord_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__lord_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__lord_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because the lord's demands are open-ended, limited only by the vassal's ability to provide and the lord's need to avoid provoking revolt. Suppression is also high (0.75) due to the lord's military power and the severe consequences of oath-breaking (forfeiture, violence, social ostracization). Theater ratio is low (0.1) as the oath's function is directly extractive and enforced, with little performative cover for non-existent coordination. Accessibility collapse is high (0.7) because vassals are deeply embedded in the system, with few viable alternatives to their feudal obligations.
 *
 * PERSPECTIVAL GAP:
 *   The lord's perspective would frame the oath as a necessary coordination mechanism for defense and order, justifying extraction as the cost of protection. Vassals and peasantry, however, experience it as a highly extractive and suppressive snare, where the 'protection' is often indistinguishable from the threat. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The feudal lord is the clear beneficiary (d=0.0-0.1), receiving maximal transfers. Vassals are primary targets (d=0.8-0.9), bearing significant costs and having limited, identity-locked exit options. The peasantry are ultimate targets (d=1.0), with virtually no power or exit. Ecclesiastical authorities, while nominally powerful, are structurally excluded from effectively limiting this secular interpretation of the oath, making them constrained observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of decentralized governance is 'dead' in the modern sense, yet the extractive interpretation of the oath persisted for centuries, demonstrating how a constraint can outlive its original mandate and continue to function as a pure extraction mechanism. The 'contested' status of the founding problem reflects the ongoing historical debate about the true nature of feudalism – coordination or extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rebellion_threshold_ambiguity,
    'What was the actual threshold of extraction that would reliably trigger widespread vassal rebellion, and how did it vary by region and time period?',
    'Detailed historical case studies comparing levels of extraction with documented instances of revolt, accounting for local economic conditions and political alliances.',
    'A lower, more consistent rebellion threshold would suggest a more constrained extractive capacity for the lord, potentially shifting the constraint''s extractiveness downward. A higher, more variable threshold would reinforce the maximal extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rebellion_threshold_ambiguity, empirical, 'The practical limit of lordly extraction before provoking armed resistance.').

omega_variable(
    ecclesiastical_influence_efficacy,
    'To what extent did ecclesiastical authorities effectively limit lordly extraction through moral suasion or spiritual sanctions, and how did this vary across regions and periods?',
    'Analysis of church records, papal bulls, and local synod decrees alongside secular legal documents and chronicles, focusing on instances where church intervention demonstrably altered secular extractive practices.',
    'Stronger, more consistent ecclesiastical influence would suggest a lower effective extractiveness for the lord, potentially pushing the constraint towards a ''tangled_rope'' or even ''rope'' classification from the vassal''s perspective, as an external check on power existed. Weak or inconsistent influence reinforces the ''snare'' classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ecclesiastical_influence_efficacy, empirical, 'The actual power of the Church to constrain secular feudal extraction.').

omega_variable(
    oath_interpretation_framing,
    'Is the feudal oath primarily a legal contract, a moral/religious bond, or a political declaration of fealty, and how does this framing affect its extractive potential?',
    'Conceptual analysis of medieval legal treatises, theological texts on oaths, and political theory, alongside historical evidence of how different actors invoked these framings in disputes.',
    'If primarily a legal contract with fixed terms (as in the vassal_coordination_reading), extraction would be more bounded. If primarily a moral/religious bond (ecclesiastical_mediation_reading), it would imply non-secular limits. This reading''s ''maximal extraction'' implies a political declaration of fealty where power defines terms, reinforcing the snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oath_interpretation_framing, conceptual, 'The dominant interpretive frame of the feudal oath and its implications for extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__lord_extraction_reading, 1000, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t1000, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(feud_tr_t1100, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1100, 0.1).
narrative_ontology:measurement(feud_tr_t1200, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(feud_tr_t1300, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1300, 0.1).
narrative_ontology:measurement(feud_tr_t1400, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1400, 0.1).

% Extraction over time
narrative_ontology:measurement(feud_be_t1000, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1000, 0.75).
narrative_ontology:measurement(feud_be_t1100, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1100, 0.8).
narrative_ontology:measurement(feud_be_t1200, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1200, 0.85).
narrative_ontology:measurement(feud_be_t1300, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1300, 0.83).
narrative_ontology:measurement(feud_be_t1400, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1400, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t1000, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1000, 0.65).
narrative_ontology:measurement(feud_su_t1100, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1100, 0.7).
narrative_ontology:measurement(feud_su_t1200, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1200, 0.75).
narrative_ontology:measurement(feud_su_t1300, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1300, 0.73).
narrative_ontology:measurement(feud_su_t1400, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1400, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__lord_extraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity__vassal_coordination_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'feudal_oath_reciprocity' kernel. This 'lord_extraction_reading' emphasizes the lord's power to interpret the oath for maximal gain, contrasting with readings that highlight vassal coordination or ecclesiastical mediation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
