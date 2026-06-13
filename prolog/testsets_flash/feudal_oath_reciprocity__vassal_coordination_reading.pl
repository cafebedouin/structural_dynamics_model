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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: feudal_oath_reciprocity__vassal_coordination_reading
 *   human_readable: Feudal Oath Reciprocity (Vassal Coordination Reading)
 *   domain: medieval_political_economy/legal_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story describes the feudal oath from the 'vassal
 *   coordination' reading, where it functions as a mechanism for establishing
 *   fixed, bounded reciprocal obligations between lords and vassals,
 *   primarily enforced through written charters. This reading emphasizes the
 *   mutual benefits and the legal framework that provided stability in a
 *   decentralized era, rather than focusing on the lord's extractive power or
 *   ecclesiastical influence. It is a reading of the
 *   'feudal_oath_reciprocity' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__vassal_coordination_reading, 0.15).
domain_priors:suppression_score(feudal_oath_reciprocity__vassal_coordination_reading, 0.3).
domain_priors:theater_ratio(feudal_oath_reciprocity__vassal_coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__vassal_coordination_reading, rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__vassal_coordination_reading, "Feudal Oath Reciprocity (Vassal Coordination Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__vassal_coordination_reading, "medieval_political_economy/legal_history/institutional_analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__vassal_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__vassal_coordination_reading, '8008aa21-cbba-4347-8f77-17d6e2e0c322').
narrative_ontology:cs_kernel_codification('8008aa21-cbba-4347-8f77-17d6e2e0c322', fixed_text).
narrative_ontology:cs_authority_grounding('8008aa21-cbba-4347-8f77-17d6e2e0c322', lineage).
narrative_ontology:cs_interpretation_layer_present('8008aa21-cbba-4347-8f77-17d6e2e0c322').
narrative_ontology:cs_reading_relation('8008aa21-cbba-4347-8f77-17d6e2e0c322', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('8008aa21-cbba-4347-8f77-17d6e2e0c322', feudal_oath_reciprocity__ecclesiastical_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('8008aa21-cbba-4347-8f77-17d6e2e0c322', foundational, mutual_obligation_is_foundational).
narrative_ontology:cs_axiom_status(mutual_obligation_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('8008aa21-cbba-4347-8f77-17d6e2e0c322', mutual_obligation_is_foundational, conventional).
narrative_ontology:cs_axiom('8008aa21-cbba-4347-8f77-17d6e2e0c322', foundational, charter_text_defines_bounds).
narrative_ontology:cs_axiom_status(charter_text_defines_bounds, holdable).
narrative_ontology:cs_axiom_grounding('8008aa21-cbba-4347-8f77-17d6e2e0c322', charter_text_defines_bounds, conventional).
narrative_ontology:cs_reference_frame('8008aa21-cbba-4347-8f77-17d6e2e0c322', charter_defined_reciprocity).
narrative_ontology:cs_drift_state('8008aa21-cbba-4347-8f77-17d6e2e0c322', late_medieval_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8008aa21-cbba-4347-8f77-17d6e2e0c322', '').
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

% Receive protection and land tenure in exchange for military service and counsel. The oath provides a legal framework for their rights and obligations, offering stability against arbitrary lordly power. Exit means forfeiture of land and status, or seeking a new lord, which is risky.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, vassals, beneficiary,
    moderate, biographical, constrained, local).

% Receive military service, counsel, and loyalty from vassals, which enables them to govern their territories and wage war. The oath formalizes their authority and secures their landholdings. They benefit from a stable, predictable system of obligation, but can seek new vassals or expand their domains.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, lords, beneficiary,
    powerful, generational, mobile, regional).

% Draft and maintain the written charters that codify the specific terms of the feudal oath, ensuring its fixed and bounded nature. Their expertise in legal text is crucial for establishing the reciprocal obligations and providing a basis for dispute resolution.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, charter_scribes, agenda_setter,
    moderate, biographical, constrained, local).

% Serve as potential arbiters in disputes, especially concerning the sacred nature of the oath, but their direct enforcement power over secular feudal obligations is limited. They observe the adherence to the oath's moral and religious dimensions.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, ecclesiastical_courts, observer,
    institutional, generational, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, reciprocal framework for land tenure, military service, and mutual protection between lords and vassals, preventing arbitrary power and ensuring collective defense.
% TRANSFER_FUNCTION: Transfers military service and loyalty from vassals to lords, and land tenure and protection from lords to vassals, all within a legally defined, bounded system.
% ABSENT_VOICES: Peasants and serfs, who are bound to the land and subject to both lord and vassal, are not party to the oath and have no formal voice in its terms, despite being directly affected by the stability or breakdown of the feudal system.
% DISAPPEARANCE_RATIONALE: If the feudal oath and its enforcement vanished, the entire medieval social and political order would collapse. Land tenure would become arbitrary, military service uncoordinated, and mutual protection nonexistent, leading to widespread chaos and a complete reorganization of power structures.
% FOUNDING_PROBLEM: The problem of establishing stable governance, military organization, and land distribution in a decentralized post-Roman world, where central authority was weak and local power was paramount.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars widely corroborate that the specific problems of early medieval decentralization that the feudal oath addressed are no longer live. While principles of reciprocal obligation persist, the feudal system itself is obsolete. Contemporary political scientists and sociologists also attest to the historical specificity of the feudal problem, distinct from modern state-building challenges.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__vassal_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__vassal_coordination_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__vassal_coordination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(feudal_oath_reciprocity__vassal_coordination_reading, 'none', 1).

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
 *   The extractiveness is low (0.15) because this reading emphasizes the reciprocal nature of the oath, where both parties receive significant benefits (protection, land tenure, military service). Suppression is moderate (0.3) as enforcement relies on the lord's power and the threat of forfeiture, but it is bounded by the charter text. Theater ratio is low (0.1) because the oath's function was genuinely central to medieval governance, not merely performative. The metrics reflect a system that, from this perspective, primarily coordinates rather than extracts.
 *
 * PERSPECTIVAL GAP:
 *   Other readings of the feudal oath (e.g., 'lord_extraction_reading') would emphasize higher extractiveness and suppression, with lords as primary beneficiaries and vassals as victims. This 'vassal_coordination_reading' highlights the genuine coordination problem solved and the reciprocal nature of the obligations, leading to a Rope classification from both lord and vassal seats. The engine's per-seat classification would reflect this shared benefit, contrasting with other readings where divergence would be stark.
 *
 * DIRECTIONALITY LOGIC:
 *   Both vassals and lords are declared beneficiaries, reflecting the mutual coordination function. Vassals receive protection and land, while lords receive service and loyalty. The charter scribes act as agenda-setters by codifying the terms. Ecclesiastical courts are observers, as their role is more moral arbitration than direct enforcement of secular obligations. The low extractiveness and mutual benefit lead to directionality values closer to the beneficiary end for both lords and vassals.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'founding_problem_status' is 'dead' while the 'disappearance_verdict' is 'world_rearranges', indicating a historical constraint whose original mandate has atrophied but whose structural role was once foundational. This prevents mislabeling it as a live Snare or Tangled Rope, instead correctly identifying it as a historical Rope that has run its course. The analysis acknowledges its past coordination function without projecting it onto contemporary issues.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_naturalness_vs_construction,
    'Is this ''vassal coordination'' reading an accurate reflection of the historical feudal oath, or a selective interpretation that downplays extractive elements?',
    'Comparative historical analysis of primary sources (charters, legal codes, chronicles) across different regions and periods, specifically seeking evidence of systematic, unreciprocated extraction or widespread vassal resistance not accounted for by this reading.',
    'If the reading is found to be overly benign, the constraint''s true extractiveness and suppression would be higher, potentially shifting its classification towards a Tangled Rope or Snare, even from the vassal''s perspective. If corroborated, it reinforces the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_naturalness_vs_construction, empirical, 'Assessing the historical accuracy and completeness of the ''vassal coordination'' reading.').

omega_variable(
    charter_enforceability_ambiguity,
    'To what extent were the ''fixed, bounded reciprocal obligations'' truly enforceable by vassals against powerful lords, or did the charter text primarily serve as a legitimizing cover for lordly discretion?',
    'Analysis of legal precedents and actual dispute resolutions where vassals successfully challenged lords based on charter terms, or conversely, where lords routinely ignored charter provisions without consequence.',
    'If charters were largely unenforceable by vassals, the constraint''s suppression would be higher (as the ''bounded'' nature was illusory), and extractiveness would increase, pushing it towards a Tangled Rope. If genuinely enforceable, the Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_enforceability_ambiguity, empirical, 'The actual enforceability of charter-defined obligations by vassals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__vassal_coordination_reading, 800, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t800, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 800, 0.08).
narrative_ontology:measurement(feud_tr_t900, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 900, 0.09).
narrative_ontology:measurement(feud_tr_t1000, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(feud_tr_t1100, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1100, 0.11).
narrative_ontology:measurement(feud_tr_t1200, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(feud_tr_t1300, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1300, 0.12).
narrative_ontology:measurement(feud_tr_t1400, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1400, 0.1).

% Extraction over time
narrative_ontology:measurement(feud_be_t800, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 800, 0.1).
narrative_ontology:measurement(feud_be_t900, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 900, 0.12).
narrative_ontology:measurement(feud_be_t1000, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1000, 0.15).
narrative_ontology:measurement(feud_be_t1100, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1100, 0.14).
narrative_ontology:measurement(feud_be_t1200, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1200, 0.16).
narrative_ontology:measurement(feud_be_t1300, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1300, 0.17).
narrative_ontology:measurement(feud_be_t1400, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1400, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t800, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 800, 0.25).
narrative_ontology:measurement(feud_su_t900, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 900, 0.28).
narrative_ontology:measurement(feud_su_t1000, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1000, 0.3).
narrative_ontology:measurement(feud_su_t1100, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1100, 0.29).
narrative_ontology:measurement(feud_su_t1200, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1200, 0.31).
narrative_ontology:measurement(feud_su_t1300, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1300, 0.32).
narrative_ontology:measurement(feud_su_t1400, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1400, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__vassal_coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity__lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'feudal_oath_reciprocity' kernel. This 'vassal_coordination_reading' emphasizes mutual, bounded obligations, while other readings focus on lordly extraction or ecclesiastical influence. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
