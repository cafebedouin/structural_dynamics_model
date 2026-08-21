% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__originalist_limitation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__originalist_limitation_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: magna_carta_clause_39__originalist_limitation_reading
 *   human_readable: Magna Carta Clause 39: Originalist Limitation Reading
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint represents an originalist reading of Magna Carta Clause
 *   39, which asserts that its limitations apply only to the specific royal
 *   abuses documented in the 1215 context. This reading emphasizes historical
 *   fidelity and limits the scope of the clause, contrasting with more
 *   expansive interpretations. It is presented as a 'mountain' due to its
 *   claim of reflecting an unchangeable historical truth, though its
 *   beneficiaries (scholars, originalist theorists) trigger False Summit
 *   Mountain detection.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__originalist_limitation_reading, 0.3).
domain_priors:suppression_score(magna_carta_clause_39__originalist_limitation_reading, 0.1).
domain_priors:theater_ratio(magna_carta_clause_39__originalist_limitation_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__originalist_limitation_reading, mountain).
narrative_ontology:human_readable(magna_carta_clause_39__originalist_limitation_reading, "Magna Carta Clause 39: Originalist Limitation Reading").
narrative_ontology:topic_domain(magna_carta_clause_39__originalist_limitation_reading, "constitutional_law/legal_history/political_theory").

domain_priors:emerges_naturally(magna_carta_clause_39__originalist_limitation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__originalist_limitation_reading, 'e6e2e8bd-f86a-4ba7-9f8f-94560d3d0919').
narrative_ontology:cs_kernel_codification('e6e2e8bd-f86a-4ba7-9f8f-94560d3d0919', fixed_text).
narrative_ontology:cs_authority_grounding('e6e2e8bd-f86a-4ba7-9f8f-94560d3d0919', lineage).
narrative_ontology:cs_interpretation_layer_present('e6e2e8bd-f86a-4ba7-9f8f-94560d3d0919').
narrative_ontology:cs_reading_relation('e6e2e8bd-f86a-4ba7-9f8f-94560d3d0919', magna_carta_clause_39__liberal_due_process_reading, coexists_with).
narrative_ontology:cs_reading_relation('e6e2e8bd-f86a-4ba7-9f8f-94560d3d0919', magna_carta_clause_39__feudal_prerogative_reading, coexists_with).
narrative_ontology:cs_axiom('e6e2e8bd-f86a-4ba7-9f8f-94560d3d0919', foundational, clause_39_limited_to_1215_grievances).
narrative_ontology:cs_axiom_status(clause_39_limited_to_1215_grievances, holdable).
narrative_ontology:cs_axiom_grounding('e6e2e8bd-f86a-4ba7-9f8f-94560d3d0919', clause_39_limited_to_1215_grievances, conventional).
narrative_ontology:cs_axiom('e6e2e8bd-f86a-4ba7-9f8f-94560d3d0919', foundational, original_intent_is_sole_interpretive_guide).
narrative_ontology:cs_axiom_status(original_intent_is_sole_interpretive_guide, holdable).
narrative_ontology:cs_axiom_grounding('e6e2e8bd-f86a-4ba7-9f8f-94560d3d0919', original_intent_is_sole_interpretive_guide, conventional).
narrative_ontology:cs_reference_frame('e6e2e8bd-f86a-4ba7-9f8f-94560d3d0919', original_public_meaning_1215).
narrative_ontology:cs_drift_state('e6e2e8bd-f86a-4ba7-9f8f-94560d3d0919', contemporary_legal_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e6e2e8bd-f86a-4ba7-9f8f-94560d3d0919', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, historical_scholars).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, originalist_legal_theorists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_clause_39__originalist_limitation_reading, king_john_and_his_successors).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__originalist_limitation_reading, historical_contextualism).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__originalist_limitation_reading, textual_fidelity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a reading that emphasizes historical accuracy and the specific grievances of 1215, reinforcing their methodology and expertise in interpreting historical documents within their original context.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, historical_scholars, beneficiary,
    analytical, generational, analytical, global).

% Utilize this reading to support a broader jurisprudential philosophy that limits constitutional interpretation to the original public meaning or intent, thereby constraining judicial activism and evolving rights claims.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, originalist_legal_theorists, beneficiary,
    institutional, generational, analytical, national).

% Historically, King John and subsequent monarchs were constrained by the specific limitations imposed by Clause 39, preventing arbitrary actions against the nobility as defined by the charter. This reading limits the scope of those constraints to the original context.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, king_john_and_his_successors, payer,
    institutional, generational, constrained, national).

% Are excluded from the direct benefits of Clause 39 under this reading, as its protections are limited to the specific historical context and do not extend to universal individual rights against arbitrary state power in a modern sense. Their claims for due process must be grounded elsewhere.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, modern_citizens_seeking_due_process, excluded,
    powerless, biographical, identity_locked, national).

% Critique this reading for its perceived anachronism and its failure to allow for the evolution of constitutional principles, arguing that it unduly restricts the development of fundamental rights.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, liberal_legal_theorists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a historically precise framework for understanding the original intent and scope of Magna Carta Clause 39, coordinating scholarly and legal discourse around a specific, limited interpretation.
% TRANSFER_FUNCTION: Transfers interpretive authority from evolving contemporary values to historical context, limiting the application of Clause 39 to specific royal abuses documented in 1215, thereby constraining modern expansive readings.
% ABSENT_VOICES: Modern citizens seeking universal due process rights are implicitly excluded from this reading's direct benefits, as their claims are not rooted in the specific historical grievances of 1215. Their voices are present in other readings of Clause 39.
% DISAPPEARANCE_RATIONALE: If this specific originalist reading vanished, the historical text of Magna Carta would remain, but its interpretive weight in legal and academic discourse would shift. Other readings (e.g., liberal due process) would likely gain prominence, but the physical world and legal systems would not fundamentally rearrange overnight.
% FOUNDING_PROBLEM: The problem of anachronistic interpretation of historical legal texts, where modern concepts are retroactively applied, obscuring original intent and context.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholars and originalist legal theorists attest that the problem of anachronism in legal interpretation remains live, requiring constant vigilance to maintain fidelity to original texts. This is corroborated by ongoing debates in constitutional law and legal history.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__originalist_limitation_reading, world_unchanged).
narrative_ontology:founding_problem_status(magna_carta_clause_39__originalist_limitation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__originalist_limitation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(magna_carta_clause_39__originalist_limitation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__originalist_limitation_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__originalist_limitation_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, ExtMetricName, E),
    domain_priors:suppression_score(magna_carta_clause_39__originalist_limitation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(magna_carta_clause_39__originalist_limitation_reading),
    narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(magna_carta_clause_39__originalist_limitation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.3) because this reading primarily extracts from expansive, anachronistic interpretations, rather than directly from individuals. Suppression is low (0.1) as it relies on scholarly argument and legal precedent, not active coercion. Theater ratio is low (0.05) as the reading's proponents genuinely aim for historical accuracy. Accessibility collapse is high (0.88) because once the historical context is accepted as the sole interpretive lens, alternative, broader applications of Clause 39 collapse. Resistance is low (0.05) because the resistance is primarily academic and interpretive, not active opposition to enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap exists between those who view this reading as a faithful historical account (beneficiaries) and those who see it as an overly restrictive interpretation that prevents the evolution of rights (observers/excluded). The engine's classification will highlight whether this 'mountain' of historical truth is, in fact, a constructed constraint benefiting specific interpretive communities.
 *
 * DIRECTIONALITY LOGIC:
 *   Historical scholars and originalist legal theorists are beneficiaries, as this reading validates their methodologies and strengthens their interpretive positions. King John and his successors are historical payers, as the original clause constrained their power. Modern citizens seeking due process are excluded from direct benefit under this reading, as it does not extend universal rights. Liberal legal theorists are observers, analyzing and critiquing this interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_accuracy_vs_legal_utility,
    'Is the primary function of interpreting Magna Carta Clause 39 to achieve historical accuracy, or to provide a foundation for contemporary legal principles?',
    'Analysis of judicial decisions and legislative actions: if modern legal systems consistently prioritize historical context over contemporary application, the former is dominant.',
    'If historical accuracy is paramount, this reading''s ''mountain'' classification is strengthened. If legal utility is prioritized, the constraint may be reclassified as a ''tangled_rope'' or ''snare'' that extracts from modern rights claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_accuracy_vs_legal_utility, conceptual, 'Ambiguity between historical fidelity and modern legal application.').

omega_variable(
    scope_of_original_abuses,
    'How narrowly or broadly should the ''specific royal abuses documented in 1215 context'' be interpreted?',
    'Further historical and textual analysis of contemporary legal documents and chronicles to establish the precise scope of grievances intended by the drafters of Magna Carta.',
    'A narrower interpretation reinforces the limited scope of this reading, potentially increasing its ''mountain'' quality by reducing contestability. A broader interpretation might allow for more expansive application, moving it closer to a ''rope'' or ''tangled_rope'' by admitting more ''victims'' of royal power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_original_abuses, empirical, 'Ambiguity in the precise scope of the original historical limitations.').

omega_variable(
    natural_law_vs_constructed_interpretation,
    'Is this reading a genuine ''mountain'' reflecting an unchangeable historical truth, or a ''tangled_rope'' constructed by specific interpretive communities (historical scholars, originalist legal theorists) to benefit their methodologies and jurisprudential goals?',
    'Analysis of the persistence of this reading in the face of alternative interpretations and its reliance on active defense by its beneficiaries. If it requires continuous intellectual enforcement against competing readings, it leans towards ''tangled_rope''.',
    'If it is a constructed constraint, its classification would shift from ''mountain'' to ''tangled_rope'' or ''snare'', highlighting the extractive nature of maintaining a specific interpretive framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_interpretation, conceptual, 'Is this constraint a genuine natural law, or a constructed constraint that benefits identifiable agents?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__originalist_limitation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(magn_tr_t10, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(magn_tr_t20, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(magn_tr_t30, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(magn_tr_t40, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(magn_tr_t50, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(magn_be_t10, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(magn_be_t20, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(magn_be_t30, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 30, 0.3).
narrative_ontology:measurement(magn_be_t40, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(magn_be_t50, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 50, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(magn_su_t10, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(magn_su_t20, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 20, 0.1).
narrative_ontology:measurement(magn_su_t30, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 30, 0.1).
narrative_ontology:measurement(magn_su_t40, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 40, 0.1).
narrative_ontology:measurement(magn_su_t50, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 50, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
