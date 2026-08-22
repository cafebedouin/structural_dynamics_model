% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__baronial_privilege_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__baronial_privilege_reading, []).

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
 *   constraint_id: magna_carta_1215__baronial_privilege_reading
 *   human_readable: Magna Carta (1215) as Baronial Privilege
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint story models Magna Carta (1215) through the 'baronial
 *   privilege' reading, which interprets the charter primarily as a feudal
 *   contract between King John and his landowning barons. Under this reading,
 *   the protections and rights granted were specific to the contracting
 *   parties, with 'free men' understood to mean landowning individuals, not a
 *   universal category. The constraint's function was to limit the King's
 *   arbitrary power over his direct vassals, rather than to establish
 *   universal rights. This reading is one of several interpretations of the
 *   Magna Carta kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, 0.2).
domain_priors:suppression_score(magna_carta_1215__baronial_privilege_reading, 0.7).
domain_priors:theater_ratio(magna_carta_1215__baronial_privilege_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__baronial_privilege_reading, rope).
narrative_ontology:human_readable(magna_carta_1215__baronial_privilege_reading, "Magna Carta (1215) as Baronial Privilege").
narrative_ontology:topic_domain(magna_carta_1215__baronial_privilege_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__baronial_privilege_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__baronial_privilege_reading, 'caf80fb0-aafd-4053-94aa-df670d186169').
narrative_ontology:cs_kernel_codification('caf80fb0-aafd-4053-94aa-df670d186169', fixed_text).
narrative_ontology:cs_authority_grounding('caf80fb0-aafd-4053-94aa-df670d186169', lineage).
narrative_ontology:cs_interpretation_layer_present('caf80fb0-aafd-4053-94aa-df670d186169').
narrative_ontology:cs_reading_relation('caf80fb0-aafd-4053-94aa-df670d186169', magna_carta_1215__universal_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('caf80fb0-aafd-4053-94aa-df670d186169', magna_carta_1215__living_document_reading, coexists_with).
narrative_ontology:cs_axiom('caf80fb0-aafd-4053-94aa-df670d186169', foundational, free_men_equals_landowning_barons).
narrative_ontology:cs_axiom_status(free_men_equals_landowning_barons, holdable).
narrative_ontology:cs_axiom_grounding('caf80fb0-aafd-4053-94aa-df670d186169', free_men_equals_landowning_barons, conventional).
narrative_ontology:cs_axiom('caf80fb0-aafd-4053-94aa-df670d186169', foundational, magna_carta_is_feudal_contract).
narrative_ontology:cs_axiom_status(magna_carta_is_feudal_contract, holdable).
narrative_ontology:cs_axiom_grounding('caf80fb0-aafd-4053-94aa-df670d186169', magna_carta_is_feudal_contract, conventional).
narrative_ontology:cs_reference_frame('caf80fb0-aafd-4053-94aa-df670d186169', feudal_contract_framework_1215).
narrative_ontology:cs_drift_state('caf80fb0-aafd-4053-94aa-df670d186169', contemporary_constitutional_discourse, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('caf80fb0-aafd-4053-94aa-df670d186169', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__baronial_privilege_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, landowning_barons).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, king_john).
narrative_ontology:constraint_vindicates(magna_carta_1215__baronial_privilege_reading, feudal_contract_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_1215__baronial_privilege_reading, limited_monarchy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary beneficiaries of Magna Carta under this reading, securing specific feudal rights and protections against arbitrary royal power. Their power derived from land ownership and military capacity, allowing them to negotiate with the King. Exit options were limited to rebellion or submission, both with high costs.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, landowning_barons, beneficiary,
    powerful, generational, constrained, national).

% The primary target of the constraint, forced to concede specific rights and limit his arbitrary power. His options were to accept the charter or face continued baronial rebellion, which threatened his throne. The constraint extracted royal prerogatives and revenue.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, king_john, payer,
    institutional, biographical, constrained, national).

% Under this reading, commoners and non-landowners were largely outside the scope of Magna Carta's protections, as 'free men' was interpreted narrowly. They had no direct voice in its creation and derived little direct benefit, remaining subject to feudal lords and royal authority without new legal recourse.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, commoners_non_landowners, excluded,
    powerless, immediate, trapped, local).

% Successors to King John, who inherited the charter and its limitations on royal power. While they could attempt to reinterpret or ignore it, the charter established a precedent that required ongoing negotiation and enforcement, shaping the future of English monarchy.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, future_monarchs, agenda_setter,
    institutional, generational, constrained, national).

% Analyze Magna Carta's original intent and historical context, contributing to the understanding of its scope and limitations. Their work informs contemporary legal and political theory but does not directly alter the constraint's operation.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a formal agreement between the King and his barons to resolve a feudal crisis, defining specific rights and obligations to prevent arbitrary royal rule and ensure a measure of stability within the feudal system.
% TRANSFER_FUNCTION: Transferred specific feudal rights and legal protections from the King's absolute prerogative to the landowning barons, limiting royal power and securing baronial privileges.
% ABSENT_VOICES: Commoners, women, and non-landowners were largely excluded from the negotiations and the direct benefits of the charter. They would have argued for broader protections and a more inclusive definition of 'free men' if present, but their interests were not represented.
% DISAPPEARANCE_RATIONALE: If Magna Carta vanished, the historical trajectory of English constitutional law would be fundamentally altered. The precedent for limiting royal power and establishing contractual governance would be lost, leading to a different evolution of state power and individual rights.
% FOUNDING_PROBLEM: King John's arbitrary rule, excessive taxation, and abuses of feudal custom led to widespread discontent and rebellion among the English barons, threatening civil war and the stability of the realm.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and political theorists widely corroborate that the specific feudal grievances of 1215 are long dead. The problem of arbitrary state power, however, is still live, leading to later reinterpretations of the charter. Independent historical scholarship from outside the benefiting parties supports this assessment.
narrative_ontology:disappearance_verdict(magna_carta_1215__baronial_privilege_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__baronial_privilege_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__baronial_privilege_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(magna_carta_1215__baronial_privilege_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__baronial_privilege_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__baronial_privilege_reading_tests).
:- end_tests(magna_carta_1215__baronial_privilege_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because it genuinely solved a collective action problem (limiting arbitrary royal power) for its direct beneficiaries (the barons) with relatively low extraction from them. However, its scope was narrow, and it required active enforcement by the barons to hold the King accountable. Extraction is low because the charter primarily redistributed existing rights rather than creating new extractive mechanisms. Suppression is high because the King's power was substantial, and the barons needed to actively suppress his arbitrary actions. Theater ratio is low, as the charter's provisions were largely functional for its intended scope.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the landowning barons, Magna Carta was a vital coordination mechanism that secured their rights and brought stability. From the King's perspective, it was a forced concession that extracted power. From the perspective of commoners, it was largely irrelevant to their daily lives. The engine's per-seat classification would reflect these divergences based on the declared roles and attributes.
 *
 * DIRECTIONALITY LOGIC:
 *   The landowning barons are clear beneficiaries, gaining specific protections. King John is the primary payer, losing some of his arbitrary power and revenue. Commoners and non-landowners are excluded, receiving no direct benefit and remaining subject to existing power structures. Future monarchs act as agenda-setters, inheriting the constraint and its implications for royal authority.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_free_men,
    'Was the term ''free men'' in Magna Carta intended to apply only to landowning barons, or did it implicitly extend to a broader class of individuals?',
    'Further historical linguistic analysis of 13th-century legal terminology and social structures, alongside examination of early judicial interpretations.',
    'If ''free men'' was broader, the constraint''s beneficiary set expands, and its classification might shift towards a more inclusive Rope or even a nascent Scaffold for broader rights, rather than a narrow baronial privilege.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_free_men, empirical, 'Ambiguity in the original scope of ''free men'' in Magna Carta.').

omega_variable(
    feudal_contract_vs_proto_constitution,
    'Is Magna Carta best understood as a specific feudal contract addressing immediate grievances, or as a foundational proto-constitutional document establishing enduring principles?',
    'Analysis of its subsequent reissues and the evolution of its legal and political reception over centuries, particularly its invocation in later constitutional struggles.',
    'If proto-constitutional, its ''claimed_type'' as a Rope might be more robust, and its ''time_horizon'' for beneficiaries would extend, influencing its long-term classification and its role in later legal developments.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(feudal_contract_vs_proto_constitution, conceptual, 'Conceptual framing of Magna Carta''s fundamental nature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__baronial_privilege_reading, 1215, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement(magn_tr_t1300, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1300, 0.12).
narrative_ontology:measurement(magn_tr_t1400, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1400, 0.15).
narrative_ontology:measurement(magn_tr_t1500, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1500, 0.18).
narrative_ontology:measurement(magn_tr_t1600, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1600, 0.2).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1215, 0.2).
narrative_ontology:measurement(magn_be_t1300, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1300, 0.18).
narrative_ontology:measurement(magn_be_t1400, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1400, 0.15).
narrative_ontology:measurement(magn_be_t1500, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1500, 0.12).
narrative_ontology:measurement(magn_be_t1600, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1600, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1215, 0.7).
narrative_ontology:measurement(magn_su_t1300, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1300, 0.65).
narrative_ontology:measurement(magn_su_t1400, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1400, 0.6).
narrative_ontology:measurement(magn_su_t1500, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1500, 0.55).
narrative_ontology:measurement(magn_su_t1600, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1600, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__baronial_privilege_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, english_common_law_development).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, parliamentary_sovereignty_evolution).

% DUAL FORMULATION NOTE:
% This is one reading of the Magna Carta (1215) kernel, focusing on its original intent as a feudal contract for baronial privilege. Other readings (universal rights, living document) are distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
