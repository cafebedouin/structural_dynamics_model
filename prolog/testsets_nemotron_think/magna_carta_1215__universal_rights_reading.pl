% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__universal_rights_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: magna_carta_1215__universal_rights_reading
 *   human_readable: Magna Carta Clause 39 as Universal Due Process Constraint
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint story captures the universal rights reading of Magna
 *   Carta Clause 39 (1215): 'No free man shall be seized or imprisoned...
 *   except by the lawful judgment of his equals or by the law of the land.'
 *   The reading interprets 'free man' as all persons and the clause as
 *   emitting a transhistorical due process constraint binding all state
 *   power. It is one of three contested readings of the Magna Carta kernel.
 *   The claimed type is 'rope' — a genuine coordination mechanism that solves
 *   the collective-action problem of arbitrary state power by substituting
 *   lawful process for executive discretion. The metrics reflect the
 *   constraint's historical evolution: initially high extraction from the
 *   sovereign (who lost arbitrary power) and high theater (the Charter was
 *   repeatedly violated and reissued), declining as due process became
 *   institutionalized, with a recent uptick in extractiveness and theater as
 *   some states erode due process protections while maintaining formal
 *   compliance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__universal_rights_reading, 0.42).
domain_priors:suppression_score(magna_carta_1215__universal_rights_reading, 0.55).
domain_priors:theater_ratio(magna_carta_1215__universal_rights_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__universal_rights_reading, rope).
narrative_ontology:human_readable(magna_carta_1215__universal_rights_reading, "Magna Carta Clause 39 as Universal Due Process Constraint").
narrative_ontology:topic_domain(magna_carta_1215__universal_rights_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__universal_rights_reading, '8395a2e8-0efe-4815-9401-e72631886507').
narrative_ontology:cs_kernel_codification('8395a2e8-0efe-4815-9401-e72631886507', fixed_text).
narrative_ontology:cs_authority_grounding('8395a2e8-0efe-4815-9401-e72631886507', lineage).
narrative_ontology:cs_interpretation_layer_present('8395a2e8-0efe-4815-9401-e72631886507').
narrative_ontology:cs_reading_relation('8395a2e8-0efe-4815-9401-e72631886507', magna_carta_1215__baronial_privilege_reading, forecloses).
narrative_ontology:cs_reading_relation('8395a2e8-0efe-4815-9401-e72631886507', magna_carta_1215__living_document_reading, coexists_with).
narrative_ontology:cs_axiom('8395a2e8-0efe-4815-9401-e72631886507', foundational, clause_39_universal_due_process).
narrative_ontology:cs_axiom_status(clause_39_universal_due_process, holdable).
narrative_ontology:cs_axiom_grounding('8395a2e8-0efe-4815-9401-e72631886507', clause_39_universal_due_process, deontological).
narrative_ontology:cs_axiom('8395a2e8-0efe-4815-9401-e72631886507', foundational, free_men_equals_all_persons).
narrative_ontology:cs_axiom_status(free_men_equals_all_persons, holdable).
narrative_ontology:cs_axiom_grounding('8395a2e8-0efe-4815-9401-e72631886507', free_men_equals_all_persons, deontological).
narrative_ontology:cs_reference_frame('8395a2e8-0efe-4815-9401-e72631886507', universal_rights_charter).
narrative_ontology:cs_drift_state('8395a2e8-0efe-4815-9401-e72631886507', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8395a2e8-0efe-4815-9401-e72631886507', '2026-06-12T14:30:00Z').
narrative_ontology:cs_kernel_id(magna_carta_1215__universal_rights_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, all_persons).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, state_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, state_actors).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, universal_due_process_principle).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, rule_of_law_over_arbitrary_power).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The state (sovereign, government, executive) bears the cost of providing due process institutions (courts, fair trials, legal safeguards) and gives up the power of arbitrary detention and extrajudicial punishment. In return, the state gains legitimacy and social order. The constraint is enforced by legal institutions that the state itself maintains. Exit from the constraint would mean reverting to arbitrary rule, which risks loss of legitimacy and instability.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, state_actors, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__universal_rights_reading, state_actors, beneficiary).

% All individuals within the state's jurisdiction are protected from arbitrary seizure, detention, and punishment without lawful judgment. They cannot easily exit the state's jurisdiction, but the constraint provides a structural protection that follows them within it. The constraint's universality means it applies regardless of status, wealth, or citizenship.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, all_persons, beneficiary,
    organized, biographical, trapped, universal).

% Courts, parliaments, and legal professions administer and interpret the due process constraint. They derive authority from the constraint's precedent and develop its application over time. They are bound by the constraint but also shape its evolution through precedent and statute.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, legal_institutions, agenda_setter,
    institutional, generational, constrained, national).

% The original contracting parties of 1215 who sought specific feudal privileges. In this reading, they are subsumed into 'all_persons' and their particularistic claims are superseded by the universal principle. They would object to the expansion of the constraint beyond their class interests.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, historical_barons, excluded,
    powerful, immediate, arbitrage, local).

% Analysts who interpret the constraint's historical development, theoretical foundations, and contemporary application across jurisdictions. They do not directly bear costs or collect benefits but influence the constraint's intellectual legitimacy and interpretive trajectory.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal procedural constraint on state power: no person may be deprived of liberty except through lawful judgment by peers or by the law of the land. This coordinates the state-individual relationship by replacing arbitrary executive power with a predictable, rule-governed process, providing security to individuals and legitimacy to the state.
% TRANSFER_FUNCTION: Transfers the power of arbitrary detention and punishment from the state to a lawful judicial process. The state gives up discretionary coercive power; individuals gain procedural protection. The cost of operating courts and due process mechanisms falls on the state (taxpayers), while the benefit of security against arbitrary power accrues to all persons.
% ABSENT_VOICES: Those living under regimes that reject due process (authoritarian states, conflict zones, extra-legal detention systems) are structurally excluded from the constraint's protection. Also absent are the original baronial parties whose particularistic reading is superseded; they would argue the constraint was never meant to be universal.
% DISAPPEARANCE_RATIONALE: If the universal due process constraint vanished overnight, states would revert to arbitrary detention and executive punishment without judicial oversight. Legal systems would lose their foundational procedural legitimacy. The relationship between state and individual would reorganize around raw power rather than law, fundamentally altering political order worldwide.
% FOUNDING_PROBLEM: The arbitrary exercise of sovereign power over subjects' lives and liberties without accountability, exemplified by King John's seizure of persons and property without judgment. The barons forced a written constraint to bind the king to lawful process.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the historical record of 1215 (the Charter itself, contemporary chronicles) and by the continuous invocation of Clause 39 in subsequent constitutional struggles (Petition of Right 1628, Habeas Corpus Act 1679, Fifth and Fourteenth Amendments US Constitution, Universal Declaration of Human Rights Article 9). Corroboration comes from legal historians (e.g., Holt, Carpenter) and human rights bodies (UN Human Rights Committee) outside the beneficiary set of any single state.
narrative_ontology:disappearance_verdict(magna_carta_1215__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__universal_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_1215__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__universal_rights_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__universal_rights_reading_tests).
:- end_tests(magna_carta_1215__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the ongoing cost to states of maintaining due process institutions versus the benefit of legitimacy. Suppression (0.55) captures the constraint's active suppression of arbitrary state action through judicial enforcement. Theater ratio (0.28) acknowledges that while due process is substantively realized in many jurisdictions, performative compliance (show trials, procedural formalities without substance) persists. Accessibility collapse (0.82) is high because once the principle of due process is accepted, the alternative of arbitrary rule becomes structurally illegitimate and practically unavailable. Resistance (0.68) remains substantial because states periodically resist due process constraints (emergency powers, security exceptions, extrajudicial detention).
 *
 * PERSPECTIVAL GAP:
 *   The state seat experiences the constraint as a tangled rope (coordination plus extraction of sovereign discretion), while the all_persons seat experiences it as a rope or even mountain (fundamental protection). The engine computes this divergence from the structural power/exit asymmetry: states are institutional but constrained; individuals are organized but trapped.
 *
 * DIRECTIONALITY LOGIC:
 *   State actors are payers (d near target) because they bear the institutional costs and give up arbitrary power, though they also benefit from legitimacy (secondary beneficiary). All persons are beneficiaries (d near beneficiary) because they receive protection without operating the machinery. Legal institutions are agenda_setters (d symmetric) because they administer the constraint and shape its interpretation. Historical barons are excluded — their particularistic interest is superseded by the universal claim. Constitutional scholars are observers (d=0.5 analytical).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (arbitrary sovereign power) remains live — states still detain arbitrarily, as seen in Guantanamo, Xinjiang, emergency detentions. The constraint has not atrophied; its mandate is continually renewed by new violations. The universal reading prevents mandatrophy by expanding the protected class from barons to all persons, keeping the constraint's function aligned with the persistent problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does the universal rights reading instantiate a distinct constraint from the baronial privilege and living document readings, or are they interpretations of the same constraint?',
    'Structural analysis of beneficiary/victim sets, enforcement mechanisms, and claimed type. If the three readings produce different ε, different stakeholder structures, and different claimed types, they are distinct constraints linked by network.affects_constraints.',
    'If distinct, each reading gets its own constraint story with independent classification. If same, they are perspectival variants of one constraint with a single ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings are structurally distinct constraints or perspectival variants.').

omega_variable(
    free_men_semantic_scope,
    'Does ''free men'' in Clause 39 structurally entail universal personhood, or is the universal reading an anachronistic projection?',
    'Historical linguistics of 1215 Latin ''liber homo'', comparative analysis of contemporaneous usage, and the reception history of the clause in subsequent legal instruments that explicitly universalized it.',
    'If the universal scope is anachronistic, the reading''s ε may be higher (more extractive from the state) because it imposes a modern meaning on a medieval text. If semantically grounded, the reading''s ε reflects the constraint''s actual historical operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_men_semantic_scope, empirical, 'Semantic and historical basis for the universal vs. particularistic reading of ''free men''.').

omega_variable(
    state_legitimacy_exchange,
    'Is the state''s compliance with due process a genuine coordination exchange (legitimacy for procedural restraint) or a strategic concession that preserves deeper arbitrary power?',
    'Longitudinal analysis of state behavior: correlation between due process adherence and regime stability/legitimacy, versus cases where due process is formal but arbitrary power persists through other channels (administrative detention, security exceptions).',
    'If strategic concession, the constraint is a snare or tangled rope from the state''s seat. If genuine exchange, it is a rope. The engine computes per-seat types; this omega flags the ambiguity for the state seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_legitimacy_exchange, empirical, 'Whether the state''s due process compliance is coordination or strategic extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__universal_rights_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magna_carta_1215__universal_rights_reading_tr_t0, magna_carta_1215__universal_rights_reading, theater_ratio, 0, 0.7).
narrative_ontology:measurement(magna_carta_1215__universal_rights_reading_tr_t100, magna_carta_1215__universal_rights_reading, theater_ratio, 100, 0.5).
narrative_ontology:measurement(magna_carta_1215__universal_rights_reading_tr_t200, magna_carta_1215__universal_rights_reading, theater_ratio, 200, 0.35).
narrative_ontology:measurement(magna_carta_1215__universal_rights_reading_tr_t400, magna_carta_1215__universal_rights_reading, theater_ratio, 400, 0.25).
narrative_ontology:measurement(magna_carta_1215__universal_rights_reading_tr_t600, magna_carta_1215__universal_rights_reading, theater_ratio, 600, 0.2).
narrative_ontology:measurement(magna_carta_1215__universal_rights_reading_tr_t800, magna_carta_1215__universal_rights_reading, theater_ratio, 800, 0.28).

% Extraction over time
narrative_ontology:measurement(magna_carta_1215__universal_rights_reading_be_t0, magna_carta_1215__universal_rights_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(magna_carta_1215__universal_rights_reading_be_t100, magna_carta_1215__universal_rights_reading, base_extractiveness, 100, 0.55).
narrative_ontology:measurement(magna_carta_1215__universal_rights_reading_be_t200, magna_carta_1215__universal_rights_reading, base_extractiveness, 200, 0.48).
narrative_ontology:measurement(magna_carta_1215__universal_rights_reading_be_t400, magna_carta_1215__universal_rights_reading, base_extractiveness, 400, 0.4).
narrative_ontology:measurement(magna_carta_1215__universal_rights_reading_be_t600, magna_carta_1215__universal_rights_reading, base_extractiveness, 600, 0.35).
narrative_ontology:measurement(magna_carta_1215__universal_rights_reading_be_t800, magna_carta_1215__universal_rights_reading, base_extractiveness, 800, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(magna_carta_1215__universal_rights_reading_su_t0, magna_carta_1215__universal_rights_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(magna_carta_1215__universal_rights_reading_su_t100, magna_carta_1215__universal_rights_reading, suppression_requirement, 100, 0.7).
narrative_ontology:measurement(magna_carta_1215__universal_rights_reading_su_t200, magna_carta_1215__universal_rights_reading, suppression_requirement, 200, 0.6).
narrative_ontology:measurement(magna_carta_1215__universal_rights_reading_su_t400, magna_carta_1215__universal_rights_reading, suppression_requirement, 400, 0.5).
narrative_ontology:measurement(magna_carta_1215__universal_rights_reading_su_t600, magna_carta_1215__universal_rights_reading, suppression_requirement, 600, 0.45).
narrative_ontology:measurement(magna_carta_1215__universal_rights_reading_su_t800, magna_carta_1215__universal_rights_reading, suppression_requirement, 800, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__universal_rights_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__universal_rights_reading, 0.1).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, habeas_corpus_tradition).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, us_constitution_fifth_amendment).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, us_constitution_fourteenth_amendment).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, udhr_article_9).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, echr_article_5).

% DUAL FORMULATION NOTE:
% This constraint (universal_rights_reading) is one of three in the magna_carta_1215 family. The baronial_privilege_reading constrains the same clause to feudal parties; the living_document_reading treats the clause as a seed for evolving interpretive tradition. All three share the kernel but instantiate different constraints with different ε, stakeholders, and claimed types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_1215__universal_rights_reading, institutional, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
