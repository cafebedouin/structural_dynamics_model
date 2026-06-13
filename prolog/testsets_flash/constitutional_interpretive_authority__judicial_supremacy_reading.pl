% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/political_theory/jurisprudence
 *
 * SUMMARY:
 *   This constraint describes the 'judicial supremacy' reading of
 *   constitutional interpretive authority, where courts hold the final say on
 *   constitutional meaning, including the power to nullify legislative acts.
 *   It is one of several competing readings of how constitutional authority
 *   is distributed. This reading positions the judiciary as the ultimate
 *   guardian of fundamental rights, often at the expense of legislative will.
 *   The structural delta is that the judiciary enters the beneficiary set for
 *   interpretive authority, the legislature is subordinated, and coercion is
 *   legitimated via rights-compliance rather than democratic will.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, 0.65).
domain_priors:suppression_score(constitutional_interpretive_authority__judicial_supremacy_reading, 0.7).
domain_priors:theater_ratio(constitutional_interpretive_authority__judicial_supremacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__judicial_supremacy_reading, 'b2867636-899d-4f4a-aaf8-375e30bd5954').
narrative_ontology:cs_kernel_codification('b2867636-899d-4f4a-aaf8-375e30bd5954', fixed_text).
narrative_ontology:cs_authority_grounding('b2867636-899d-4f4a-aaf8-375e30bd5954', lineage).
narrative_ontology:cs_interpretation_layer_present('b2867636-899d-4f4a-aaf8-375e30bd5954').
narrative_ontology:cs_reading_relation('b2867636-899d-4f4a-aaf8-375e30bd5954', constitutional_interpretive_authority__parliamentary_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('b2867636-899d-4f4a-aaf8-375e30bd5954', constitutional_interpretive_authority__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('b2867636-899d-4f4a-aaf8-375e30bd5954', foundational, judicial_review_is_final).
narrative_ontology:cs_axiom_status(judicial_review_is_final, holdable).
narrative_ontology:cs_axiom_grounding('b2867636-899d-4f4a-aaf8-375e30bd5954', judicial_review_is_final, conventional).
narrative_ontology:cs_axiom('b2867636-899d-4f4a-aaf8-375e30bd5954', foundational, judiciary_is_rights_guardian).
narrative_ontology:cs_axiom_status(judiciary_is_rights_guardian, holdable).
narrative_ontology:cs_axiom_grounding('b2867636-899d-4f4a-aaf8-375e30bd5954', judiciary_is_rights_guardian, deontological).
narrative_ontology:cs_reference_frame('b2867636-899d-4f4a-aaf8-375e30bd5954', marbury_v_madison_precedent).
narrative_ontology:cs_drift_state('b2867636-899d-4f4a-aaf8-375e30bd5954', contemporary_judicial_activism_debate, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b2867636-899d-4f4a-aaf8-375e30bd5954', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, rights_advocacy_groups).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, executive_branch).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, popular_will).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_interpretive_authority__judicial_supremacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the judiciary's interpretive power can significantly alter policy outcomes and impose costs on other branches and the popular will. Suppression (0.70) is also high, as the finality of judicial decisions actively suppresses alternative interpretations and legislative attempts to circumvent rulings. The theater ratio is low (0.20) because the judiciary's interpretive function is genuinely active, though its 'neutral arbiter' performance may mask some policy-making. The historical measurements show a gradual increase in both extractiveness and suppression as judicial review became more entrenched and expansive over time.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's perspective, this is a necessary 'rope' for constitutional order and rights protection. From the legislature's and executive's perspective, it can feel like a 'snare' that extracts democratic authority. The engine's computation will likely show a tangled_rope or snare from the political branches' seats, while the judiciary's seat might compute as a rope or even a mountain (if its naturalness claim is accepted).
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is the primary beneficiary and agenda-setter, gaining institutional power and influence (low d). Rights advocacy groups also benefit by having a powerful institutional ally (low d). The legislature and executive branch are payers, as their authority is constrained and their actions subject to nullification (high d). The 'popular_will' is a victim, as its direct expression through elected representatives can be overridden (high d).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_legitimacy_source,
    'Is judicial supremacy grounded in an inherent, non-majoritarian constitutional design, or is it a historically contingent assertion of power that could be legitimately challenged?',
    'Comparative constitutional analysis of systems with different interpretive authority distributions, and historical analysis of the origins and evolution of judicial review in specific jurisdictions.',
    'If inherent, the constraint''s extractiveness is a necessary cost of constitutionalism. If contingent, it is a constructed constraint whose legitimacy is open to political contestation and potential re-negotiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_legitimacy_source, conceptual, 'Source of judicial supremacy''s legitimacy: inherent constitutional design vs. historical contingency.').

omega_variable(
    rights_protection_efficacy,
    'Does judicial supremacy genuinely lead to better protection of fundamental rights compared to systems with more politically accountable interpretive authority?',
    'Empirical studies comparing rights outcomes (e.g., civil liberties, social rights) in jurisdictions with judicial supremacy versus those with parliamentary supremacy or coordinate construction.',
    'If rights protection is demonstrably superior, it strengthens the coordination function claim. If not, it weakens the justification for the extraction of legislative authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_protection_efficacy, empirical, 'Empirical efficacy of judicial supremacy in protecting rights.').

omega_variable(
    popular_will_subordination_threshold,
    'At what point does judicial nullification of legislative acts transition from legitimate rights protection to an illegitimate subordination of the popular will?',
    'Normative and political theory analysis, combined with public opinion research on judicial legitimacy and the perceived democratic deficit. This is a preference-driven boundary.',
    'The classification of ''popular_will'' as a victim depends on this threshold. If the threshold is low, more judicial actions are seen as extractive; if high, more are seen as legitimate coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(popular_will_subordination_threshold, preference, 'Defining the boundary between legitimate judicial review and illegitimate judicial overreach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__judicial_supremacy_reading, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1789, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1789, 0.1).
narrative_ontology:measurement(cons_tr_t1865, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1865, 0.15).
narrative_ontology:measurement(cons_tr_t1930, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1930, 0.18).
narrative_ontology:measurement(cons_tr_t1970, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(cons_tr_t2000, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(cons_tr_t2024, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(cons_be_t1789, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1789, 0.3).
narrative_ontology:measurement(cons_be_t1865, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1865, 0.45).
narrative_ontology:measurement(cons_be_t1930, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1930, 0.55).
narrative_ontology:measurement(cons_be_t1970, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(cons_be_t2000, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement(cons_be_t2024, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1789, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1789, 0.4).
narrative_ontology:measurement(cons_su_t1865, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1865, 0.5).
narrative_ontology:measurement(cons_su_t1930, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1930, 0.6).
narrative_ontology:measurement(cons_su_t1970, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(cons_su_t2000, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(cons_su_t2024, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
