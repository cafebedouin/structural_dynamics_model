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
 *   This constraint represents the 'vassal coordination' reading of the
 *   feudal oath, where the oath establishes fixed, bounded reciprocal
 *   obligations between lord and vassal, primarily enforced through charter
 *   text and custom. It is viewed as a genuine coordination mechanism that
 *   provides mutual benefits and stability, rather than primarily an
 *   extractive tool for the lord. The low extractiveness and moderate
 *   suppression reflect this reading's emphasis on reciprocity and mutual
 *   enforceability.
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
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__vassal_coordination_reading, '558fc205-d409-476d-a783-278ca50ec8c5').
narrative_ontology:cs_kernel_codification('558fc205-d409-476d-a783-278ca50ec8c5', fixed_text).
narrative_ontology:cs_authority_grounding('558fc205-d409-476d-a783-278ca50ec8c5', lineage).
narrative_ontology:cs_interpretation_layer_present('558fc205-d409-476d-a783-278ca50ec8c5').
narrative_ontology:cs_reading_relation('558fc205-d409-476d-a783-278ca50ec8c5', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('558fc205-d409-476d-a783-278ca50ec8c5', feudal_oath_reciprocity__ecclesiastical_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('558fc205-d409-476d-a783-278ca50ec8c5', foundational, mutual_aid_and_counsel_obligation).
narrative_ontology:cs_axiom_status(mutual_aid_and_counsel_obligation, holdable).
narrative_ontology:cs_axiom_grounding('558fc205-d409-476d-a783-278ca50ec8c5', mutual_aid_and_counsel_obligation, conventional).
narrative_ontology:cs_axiom('558fc205-d409-476d-a783-278ca50ec8c5', foundational, bounded_service_for_land_tenure).
narrative_ontology:cs_axiom_status(bounded_service_for_land_tenure, holdable).
narrative_ontology:cs_axiom_grounding('558fc205-d409-476d-a783-278ca50ec8c5', bounded_service_for_land_tenure, conventional).
narrative_ontology:cs_reference_frame('558fc205-d409-476d-a783-278ca50ec8c5', reciprocal_feudal_contract).
narrative_ontology:cs_drift_state('558fc205-d409-476d-a783-278ca50ec8c5', late_medieval_period, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('558fc205-d409-476d-a783-278ca50ec8c5', '').
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

% Receive protection, land tenure, and legal standing in exchange for military service and counsel. The oath provides a framework for their rights and obligations, offering stability against arbitrary lordly power. Exit means forfeiture of land and status, or rebellion.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, vassals, beneficiary,
    moderate, biographical, constrained, local).

% Receive military service, counsel, and loyalty from vassals, enabling them to raise armies and administer their territories. The oath legitimizes their authority and provides a stable source of manpower and revenue. Breaching the oath risks rebellion or loss of reputation.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, lords, agenda_setter,
    powerful, generational, constrained, regional).

% Interpret and record the terms of feudal oaths and charters. Their work provides the textual basis for understanding and enforcing reciprocal obligations, acting as a check on both lordly overreach and vassal defection.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, charter_scribes_and_legal_scholars, observer,
    analytical, generational, analytical, regional).

% Are largely outside the direct reciprocal oath structure, being bound to the land and subject to the lord's authority through the vassal. They benefit indirectly from the stability the oath provides but have no direct voice in its terms or enforcement.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, common_peasants, excluded,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, mutually recognized framework for land tenure, military service, and protection, allowing for the organization of medieval society and defense against external threats.
% TRANSFER_FUNCTION: Transfers military service and counsel from vassals to lords, and protection and land tenure from lords to vassals, within a bounded, reciprocal agreement.
% ABSENT_VOICES: Common peasants, who bear the ultimate burden of feudal society, are excluded from the oath-making process. They would likely advocate for greater personal freedom and reduced obligations if given a voice.
% DISAPPEARANCE_RATIONALE: If the feudal oath vanished, the entire social and political structure of medieval Europe would collapse. Land tenure would become arbitrary, military organization impossible, and the system of protection and obligation would dissolve into chaos, leading to widespread warfare and social breakdown.
% FOUNDING_PROBLEM: The need to organize society, raise armies, and administer land in a decentralized post-Roman world, where central authority was weak and local protection was paramount.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary legal texts, chronicles, and historical analyses from scholars (e.g., Marc Bloch, Susan Reynolds) corroborate the problem of organizing society and defense in the early medieval period, and the role of feudalism in addressing it. While the specific form of feudalism is dead, the underlying problem of organizing decentralized power structures remains relevant in various forms.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__vassal_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__vassal_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__vassal_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low (0.15) because the oath, in this reading, defines clear, limited obligations for both parties, preventing arbitrary demands. Suppression is moderate (0.4) as enforcement relies on social pressure, custom, and the threat of rebellion, rather than overwhelming coercive force. Theater ratio is low (0.1) because the oath's function is genuinely structural and operational, not merely performative. The metrics reflect a system that, while hierarchical, aims for a degree of balance and mutual benefit.
 *
 * PERSPECTIVAL GAP:
 *   The 'vassal coordination' reading emphasizes the mutual benefits and stability provided by the oath, leading to a Rope classification. This contrasts sharply with the 'lord extraction' reading, which would highlight the coercive aspects and high extractiveness from the vassal's perspective, likely leading to a Snare or Tangled Rope classification. The 'ecclesiastical mediation' reading would focus on the moral and religious limits on extraction, potentially shifting the perceived extractiveness and suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Both vassals and lords are beneficiaries in this reading, as the oath provides a stable framework for their respective needs (protection/land for vassals, service/loyalty for lords). Lords, as agenda-setters, have more power but are still bound by the reciprocal nature of the oath. There are no structural victims in this reading, as the system is understood to provide net benefits to its direct participants.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_framing_ambiguity,
    'Is the feudal oath primarily a coordination mechanism for mutual benefit (vassal coordination reading) or a tool for lordly extraction (lord extraction reading)?',
    'Analysis of historical legal disputes and economic outcomes: if disputes primarily concern breaches of reciprocal terms, it supports coordination; if they concern arbitrary demands and resistance to excessive burdens, it supports extraction.',
    'If resolved towards extraction, the constraint''s extractiveness and suppression would be significantly higher, reclassifying it as a Tangled Rope or Snare. If resolved towards coordination, the Rope classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_ambiguity, conceptual, 'Ambiguity in the primary function of the feudal oath.').

omega_variable(
    ecclesiastical_influence_on_extraction,
    'To what extent did the ecclesiastical mediation reading (Christian charity, sacramental oath) genuinely limit secular extraction, versus merely providing a moral veneer?',
    'Comparative historical analysis of regions with stronger vs. weaker ecclesiastical influence on feudal law, examining differences in vassal obligations and lordly demands.',
    'If ecclesiastical influence was genuinely limiting, the effective extractiveness of the ''lord extraction'' reading would be lower than otherwise, and the ''vassal coordination'' reading''s stability would be reinforced by an external moral constraint. If it was merely a veneer, the underlying extraction would be higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecclesiastical_influence_on_extraction, empirical, 'Impact of religious doctrine on the practical limits of feudal extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__vassal_coordination_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(feud_tr_t20, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(feud_tr_t40, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(feud_tr_t60, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 60, 0.09).
narrative_ontology:measurement(feud_tr_t80, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 80, 0.08).
narrative_ontology:measurement(feud_tr_t100, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(feud_be_t20, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(feud_be_t40, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(feud_be_t60, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 60, 0.14).
narrative_ontology:measurement(feud_be_t80, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 80, 0.13).
narrative_ontology:measurement(feud_be_t100, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(feud_su_t20, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(feud_su_t40, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(feud_su_t60, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 60, 0.38).
narrative_ontology:measurement(feud_su_t80, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 80, 0.37).
narrative_ontology:measurement(feud_su_t100, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 100, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__vassal_coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, feudal_land_tenure_system).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, medieval_military_organization).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'feudal_oath_reciprocity' kernel. This 'vassal_coordination_reading' emphasizes mutual benefit and bounded obligations, contrasting with the 'lord_extraction_reading' (focused on coercive power) and the 'ecclesiastical_mediation_reading' (focused on moral limits).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
