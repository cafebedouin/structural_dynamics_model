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
 *   constraint_id: magna_carta_clause_39__originalist_limitation_reading
 *   human_readable: Magna Carta Clause 39 (Originalist Limitation Reading)
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint represents an 'originalist limitation' reading of Magna
 *   Carta's Clause 39, asserting that its protections are strictly bounded by
 *   the specific feudal grievances and legal context of 1215. It limits royal
 *   abuses as understood at the time, primarily benefiting the barons who
 *   negotiated the charter. This reading contrasts sharply with more
 *   expansive interpretations that see Clause 39 as a foundational statement
 *   of universal due process rights. The constraint is claimed as a Rope
 *   because, within its narrow historical scope, it genuinely coordinated
 *   royal power with baronial expectations, providing mutual benefit and
 *   stability.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__originalist_limitation_reading, 0.3).
domain_priors:suppression_score(magna_carta_clause_39__originalist_limitation_reading, 0.2).
domain_priors:theater_ratio(magna_carta_clause_39__originalist_limitation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__originalist_limitation_reading, rope).
narrative_ontology:human_readable(magna_carta_clause_39__originalist_limitation_reading, "Magna Carta Clause 39 (Originalist Limitation Reading)").
narrative_ontology:topic_domain(magna_carta_clause_39__originalist_limitation_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__originalist_limitation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__originalist_limitation_reading, 'd907d3ae-ac2e-4373-9894-a7762b0e1117').
narrative_ontology:cs_kernel_codification('d907d3ae-ac2e-4373-9894-a7762b0e1117', fixed_text).
narrative_ontology:cs_authority_grounding('d907d3ae-ac2e-4373-9894-a7762b0e1117', lineage).
narrative_ontology:cs_interpretation_layer_present('d907d3ae-ac2e-4373-9894-a7762b0e1117').
narrative_ontology:cs_reading_relation('d907d3ae-ac2e-4373-9894-a7762b0e1117', magna_carta_clause_39__liberal_due_process_reading, forecloses).
narrative_ontology:cs_reading_relation('d907d3ae-ac2e-4373-9894-a7762b0e1117', magna_carta_clause_39__feudal_prerogative_reading, coexists_with).
narrative_ontology:cs_axiom('d907d3ae-ac2e-4373-9894-a7762b0e1117', foundational, clause_39_limited_to_1215_grievances).
narrative_ontology:cs_axiom_status(clause_39_limited_to_1215_grievances, holdable).
narrative_ontology:cs_axiom_grounding('d907d3ae-ac2e-4373-9894-a7762b0e1117', clause_39_limited_to_1215_grievances, conventional).
narrative_ontology:cs_axiom('d907d3ae-ac2e-4373-9894-a7762b0e1117', foundational, historical_context_determines_meaning).
narrative_ontology:cs_axiom_status(historical_context_determines_meaning, holdable).
narrative_ontology:cs_axiom_grounding('d907d3ae-ac2e-4373-9894-a7762b0e1117', historical_context_determines_meaning, empirically_contingent).
narrative_ontology:cs_reference_frame('d907d3ae-ac2e-4373-9894-a7762b0e1117', id_1215_feudal_legal_order).
narrative_ontology:cs_drift_state('d907d3ae-ac2e-4373-9894-a7762b0e1117', contemporary_legal_discourse, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('d907d3ae-ac2e-4373-9894-a7762b0e1117', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, id_1215_barons).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, monarchy_legitimacy).
narrative_ontology:constraint_victim(magna_carta_clause_39__originalist_limitation_reading, abusive_royal_officials).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__originalist_limitation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(magna_carta_clause_39__originalist_limitation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__originalist_limitation_reading_tests).
:- end_tests(magna_carta_clause_39__originalist_limitation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is low (0.3 decreasing to 0.1) because this reading only extracts from specific, documented royal abuses, not from a broad class of actions. Suppression is low (0.2 decreasing to 0.1) as it primarily relies on the political power of the barons rather than widespread coercion. Theater ratio is low (0.1) because, within its historical context, the clause was a genuine attempt at legal reform, not mere performance. Accessibility collapse is high (0.7) because, for those seeking broader rights, this reading severely limits the available interpretations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 1215 barons, this was a vital Rope, securing specific, tangible protections. From the perspective of modern citizens, this reading is an 'excluded' constraint, as it denies them the broader due process rights they might associate with Magna Carta. Originalist legal scholars act as agenda-setters, actively enforcing this narrow interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   The 1215 barons are beneficiaries, gaining specific protections. Monarchy legitimacy is also a beneficiary, as the charter helped stabilize the realm. Abusive royal officials are the payers, as their arbitrary actions are constrained. Modern citizens are excluded, as their claims for universal rights are not recognized by this reading. Originalist legal scholars are agenda-setters, actively promoting and enforcing this interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling a historically specific coordination mechanism as a universal due process right. By limiting Clause 39 to its original context, it highlights how a constraint's function can be superseded or reinterpreted over time, avoiding the 'false summit' of claiming universal applicability for a historically contingent agreement. The 'dead' status of the founding problem, combined with the 'world unchanged' disappearance verdict, suggests that while the original problem is resolved, the constraint's historical significance persists, primarily in academic and interpretive debates rather than active legal enforcement for its original purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_due_process,
    'Is Clause 39''s ''law of the land'' or ''lawful judgment of his peers'' limited to 1215 feudal custom, or does it contain an evolving principle of due process?',
    'Analysis of legal developments in the centuries immediately following Magna Carta, examining whether interpretations expanded beyond the specific 1215 grievances without explicit amendment.',
    'If an evolving principle is found, this originalist reading''s extractiveness would be higher (as it suppresses broader rights), and its classification might shift towards a Snare for modern citizens. If strictly limited, its Rope classification holds for its historical context.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_due_process, conceptual, 'Ambiguity regarding the historical scope of Clause 39''s legal principles.').

omega_variable(
    originalist_vs_living_constitution,
    'Is this constraint a genuine historical account of Clause 39''s original intent, or a contemporary interpretive choice (originalism) that benefits certain legal/political agendas?',
    'Examination of the political and jurisprudential motivations of modern originalist proponents, and comparison with non-originalist historical scholarship.',
    'If primarily a contemporary interpretive choice, the ''monarchy_legitimacy'' beneficiary might be re-evaluated as ''originalist_legal_scholars_legitimacy'', and the constraint''s ''emerges_naturally'' status (if claimed) would be challenged, potentially shifting it towards a Tangled Rope or Snare for modern legal systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_vs_living_constitution, preference, 'Whether the originalist reading is a historical fact or a modern interpretive preference.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__originalist_limitation_reading, 1215, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement(magn_tr_t1300, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1300, 0.1).
narrative_ontology:measurement(magn_tr_t1600, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(magn_tr_t1800, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(magn_tr_t2024, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1215, 0.3).
narrative_ontology:measurement(magn_be_t1300, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1300, 0.25).
narrative_ontology:measurement(magn_be_t1600, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1600, 0.2).
narrative_ontology:measurement(magn_be_t1800, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement(magn_be_t2024, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 2024, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1215, 0.2).
narrative_ontology:measurement(magn_su_t1300, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1300, 0.18).
narrative_ontology:measurement(magn_su_t1600, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1600, 0.15).
narrative_ontology:measurement(magn_su_t1800, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1800, 0.12).
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
