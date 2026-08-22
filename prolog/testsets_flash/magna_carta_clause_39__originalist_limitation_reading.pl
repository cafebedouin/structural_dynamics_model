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
 *   This constraint represents an originalist reading of Magna Carta's Clause
 *   39, asserting that its protections are strictly limited to the specific
 *   royal abuses documented in 1215 and apply primarily to the feudal barons
 *   who negotiated the charter. It is presented as a 'mountain' by its
 *   proponents, reflecting a belief in the fixed, immutable meaning of
 *   historical texts. The metrics, however, show a low but non-zero
 *   extractiveness, as this reading can limit contemporary rights, benefiting
 *   those who prefer a constrained government.
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
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__originalist_limitation_reading, mountain).
narrative_ontology:human_readable(magna_carta_clause_39__originalist_limitation_reading, "Magna Carta Clause 39: Originalist Limitation Reading").
narrative_ontology:topic_domain(magna_carta_clause_39__originalist_limitation_reading, "constitutional_law/legal_history/political_theory").

domain_priors:emerges_naturally(magna_carta_clause_39__originalist_limitation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__originalist_limitation_reading, '4b820bbf-3418-4c75-ab8d-d8efcfdbb797').
narrative_ontology:cs_kernel_codification('4b820bbf-3418-4c75-ab8d-d8efcfdbb797', fixed_text).
narrative_ontology:cs_authority_grounding('4b820bbf-3418-4c75-ab8d-d8efcfdbb797', lineage).
narrative_ontology:cs_interpretation_layer_present('4b820bbf-3418-4c75-ab8d-d8efcfdbb797').
narrative_ontology:cs_reading_relation('4b820bbf-3418-4c75-ab8d-d8efcfdbb797', magna_carta_clause_39__liberal_due_process_reading, coexists_with).
narrative_ontology:cs_reading_relation('4b820bbf-3418-4c75-ab8d-d8efcfdbb797', magna_carta_clause_39__feudal_prerogative_reading, coexists_with).
narrative_ontology:cs_axiom('4b820bbf-3418-4c75-ab8d-d8efcfdbb797', foundational, original_public_meaning_supremacy).
narrative_ontology:cs_axiom_status(original_public_meaning_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('4b820bbf-3418-4c75-ab8d-d8efcfdbb797', original_public_meaning_supremacy, conventional).
narrative_ontology:cs_axiom('4b820bbf-3418-4c75-ab8d-d8efcfdbb797', foundational, historical_context_limits_scope).
narrative_ontology:cs_axiom_status(historical_context_limits_scope, holdable).
narrative_ontology:cs_axiom_grounding('4b820bbf-3418-4c75-ab8d-d8efcfdbb797', historical_context_limits_scope, empirically_contingent).
narrative_ontology:cs_reference_frame('4b820bbf-3418-4c75-ab8d-d8efcfdbb797', id_1215_feudal_legal_order).
narrative_ontology:cs_drift_state('4b820bbf-3418-4c75-ab8d-d8efcfdbb797', contemporary_legal_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4b820bbf-3418-4c75-ab8d-d8efcfdbb797', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, historical_scholars).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, originalist_legal_theorists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_clause_39__originalist_limitation_reading, contemporary_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the clarity and intellectual rigor of interpreting historical documents within their original context, avoiding anachronism. Their careers and reputations are built on this methodology.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, historical_scholars, beneficiary,
    analytical, generational, analytical, global).

% Utilize this reading to argue for a constrained interpretation of constitutional rights, limiting their expansion beyond the framers' original intent. Their professional identity is tied to this interpretive method.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, originalist_legal_theorists, beneficiary,
    institutional, generational, identity_locked, national).

% Bear the cost of a limited interpretation of rights, potentially being denied protections that a more expansive reading of Clause 39 might afford. Their ability to challenge this interpretation is through political or judicial means, which are often slow and costly.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, contemporary_citizens, payer,
    powerless, biographical, constrained, national).

% Are excluded from the interpretive framework that prioritizes 1215 context over evolving societal norms. They advocate for a 'living constitution' approach, but their arguments are often dismissed by originalist courts.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, liberal_legal_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, historically grounded framework for interpreting foundational legal texts, ensuring consistency with the original intent and preventing arbitrary reinterpretation.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary moral or political considerations to historical evidence and original intent, limiting the scope of rights and governmental power to what was understood in 1215.
% ABSENT_VOICES: Advocates for a 'living constitution' or evolving rights would object, arguing that a purely historical reading fails to address modern injustices. They are excluded by the interpretive methodology itself.
% DISAPPEARANCE_RATIONALE: If this originalist reading vanished, the interpretive landscape of constitutional law would fundamentally shift. Arguments for expansive rights based on evolving societal standards would gain significant ground, leading to a re-evaluation of numerous legal precedents and potentially altering the balance of power between individuals and the state.
% FOUNDING_PROBLEM: The problem of anachronistic interpretation of historical legal texts, where modern values are projected onto past documents, distorting their original meaning and intent.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholars and originalist legal theorists attest to the ongoing problem of anachronism in legal interpretation. Critics of originalism acknowledge the existence of anachronism but dispute its severity or relevance to contemporary constitutional application.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__originalist_limitation_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__originalist_limitation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__originalist_limitation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   The low extractiveness (0.3) reflects that this reading primarily extracts by limiting the scope of potential rights, rather than imposing direct costs. Suppression (0.1) is low because it relies on interpretive authority rather than active coercion. Theater ratio is minimal (0.05) as the academic and legal work supporting this reading is genuinely focused on historical accuracy. Accessibility collapse is high (0.8) because once the originalist framework is accepted, alternative interpretations are largely foreclosed. Resistance is low (0.05) within the originalist framework itself, though it faces external resistance from other interpretive schools.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of originalist scholars, this is a 'mountain' of historical truth. From the perspective of contemporary citizens seeking broader rights, it can feel like a 'snare' that limits their freedoms based on an outdated context. The engine's classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Historical scholars and originalist legal theorists are beneficiaries, as this reading validates their methodology and strengthens their arguments. Contemporary citizens are payers, as their rights may be limited by this interpretation. Liberal legal advocates are excluded, as their interpretive framework is fundamentally at odds with this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_accuracy_vs_contemporary_relevance,
    'Is the primary purpose of interpreting Clause 39 to achieve historical accuracy, or to provide a framework for contemporary justice?',
    'A societal consensus or judicial ruling explicitly prioritizing one goal over the other in constitutional interpretation.',
    'If historical accuracy is paramount, this reading remains a ''mountain'' of fixed meaning. If contemporary relevance is prioritized, its ''mountain'' status would be challenged, potentially reclassifying it as a ''snare'' or ''tangled_rope'' that extracts by limiting modern rights.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_accuracy_vs_contemporary_relevance, conceptual, 'Ambiguity in the purpose of constitutional interpretation.').

omega_variable(
    victim_scope_ambiguity,
    'Are the ''victims'' of this reading truly limited to contemporary citizens whose rights are constrained, or does it also include historical groups whose grievances were not fully addressed in 1215?',
    'Further historical and legal scholarship on the scope of Magna Carta''s protections beyond the immediate feudal context, or a re-evaluation of the concept of ''victimhood'' in historical legal documents.',
    'If the victim scope expands to include historically marginalized groups, the extractiveness of this reading would increase, potentially shifting its classification towards a ''snare'' for those groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_scope_ambiguity, empirical, 'Ambiguity in the scope of those negatively affected by the originalist reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__originalist_limitation_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(magn_be_t1970, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(magn_be_t1985, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1985, 0.27).
narrative_ontology:measurement(magn_be_t2000, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement(magn_be_t2015, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 2015, 0.29).
narrative_ontology:measurement(magn_be_t2024, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1970, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1970, 0.08).
narrative_ontology:measurement(magn_su_t1985, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1985, 0.09).
narrative_ontology:measurement(magn_su_t2000, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(magn_su_t2015, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 2015, 0.1).
narrative_ontology:measurement(magn_su_t2024, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
