% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__pluralist_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__pluralist_balancing, []).

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
 *   constraint_id: common_law_precedent_corpus__pluralist_balancing
 *   human_readable: Common Law Precedent: Pluralist Balancing Reading
 *   domain: legal/jurisprudence
 *
 * SUMMARY:
 *   This constraint describes the 'pluralist balancing' reading of common law
 *   precedent, where the weight of prior judicial decisions is not absolute
 *   but varies by legal domain and context, requiring judges to balance
 *   stability with adaptation on a case-by-case basis. This approach is
 *   claimed as a 'rope' (a flexible coordination mechanism) but operates with
 *   significant extraction and suppression, particularly for litigants and
 *   lower courts facing unpredictable outcomes and high interpretive costs.
 *   The claimed type reflects the ideal, while the metrics reflect the
 *   operational reality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, 0.65).
domain_priors:suppression_score(common_law_precedent_corpus__pluralist_balancing, 0.7).
domain_priors:theater_ratio(common_law_precedent_corpus__pluralist_balancing, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, extractiveness, 0.65).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__pluralist_balancing, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__pluralist_balancing, "Common Law Precedent: Pluralist Balancing Reading").
narrative_ontology:topic_domain(common_law_precedent_corpus__pluralist_balancing, "legal/jurisprudence").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__pluralist_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__pluralist_balancing, '146dd649-7f9d-4852-aad3-bacde25092c0').
narrative_ontology:cs_kernel_codification('146dd649-7f9d-4852-aad3-bacde25092c0', formalized).
narrative_ontology:cs_authority_grounding('146dd649-7f9d-4852-aad3-bacde25092c0', lineage).
narrative_ontology:cs_interpretation_layer_present('146dd649-7f9d-4852-aad3-bacde25092c0').
narrative_ontology:cs_reading_relation('146dd649-7f9d-4852-aad3-bacde25092c0', common_law_precedent_corpus__strict_stare_decisis, coexists_with).
narrative_ontology:cs_reading_relation('146dd649-7f9d-4852-aad3-bacde25092c0', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_axiom('146dd649-7f9d-4852-aad3-bacde25092c0', foundational, contextual_precedent_weight).
narrative_ontology:cs_axiom_status(contextual_precedent_weight, holdable).
narrative_ontology:cs_axiom_grounding('146dd649-7f9d-4852-aad3-bacde25092c0', contextual_precedent_weight, conventional).
narrative_ontology:cs_axiom('146dd649-7f9d-4852-aad3-bacde25092c0', foundational, balancing_stability_adaptation).
narrative_ontology:cs_axiom_status(balancing_stability_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('146dd649-7f9d-4852-aad3-bacde25092c0', balancing_stability_adaptation, instrumental).
narrative_ontology:cs_reference_frame('146dd649-7f9d-4852-aad3-bacde25092c0', flexible_common_law_tradition).
narrative_ontology:cs_drift_state('146dd649-7f9d-4852-aad3-bacde25092c0', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('146dd649-7f9d-4852-aad3-bacde25092c0', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, appellate_judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, legal_profession).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, litigants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, lower_courts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the effective weight of precedent by deciding which cases warrant re-evaluation and how to balance stability with adaptation. Benefits from the flexibility to shape law while maintaining an appearance of continuity.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, appellate_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Must apply precedent but face uncertainty due to the pluralist balancing approach, leading to higher reversal rates or the need for extensive justification for their decisions. Their professional identity is tied to upholding the legal system.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, lower_courts, payer,
    organized, biographical, identity_locked, local).

% Bear the direct costs of legal uncertainty and unpredictable outcomes, especially when precedent weight shifts or is balanced differently across domains. They are bound by the court's decisions with limited recourse.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, litigants, payer,
    powerless, immediate, trapped, local).

% Benefits from the complexity and interpretive demands of a pluralist balancing approach, which increases the need for specialized legal expertise and argument. This can lead to higher fees and sustained demand for their services.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, legal_profession, beneficiary,
    organized, biographical, constrained, national).

% Analyze and critique the application of precedent, identifying inconsistencies and proposing frameworks for balancing stability and adaptation. Their work influences future judicial reasoning but does not directly set precedent.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for judicial decision-making that allows for legal stability while accommodating societal changes and specific contextual demands, preventing rigid adherence to outdated rules.
% TRANSFER_FUNCTION: Transfers interpretive authority and flexibility to higher courts and legal experts, while imposing costs of uncertainty and complexity on lower courts and litigants.
% ABSENT_VOICES: Citizens and communities directly impacted by legal decisions, who often lack the means or standing to influence the balancing act of precedent. They would advocate for greater predictability and accessibility in legal outcomes.
% DISAPPEARANCE_RATIONALE: If the pluralist balancing approach to precedent vanished, the legal system would either become excessively rigid (if strict stare decisis prevailed) or entirely chaotic (if precedent had no weight), fundamentally altering judicial function and legal predictability.
% FOUNDING_PROBLEM: The need to reconcile the common law's demand for consistency with the evolving nature of society and the unique circumstances of individual cases, avoiding both stagnation and arbitrary decision-making.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and historical analyses attest to the enduring tension between stability and adaptation in common law systems. Judicial opinions frequently acknowledge this balancing act, even if the specific outcomes are contested.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__pluralist_balancing, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__pluralist_balancing, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__pluralist_balancing, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(common_law_precedent_corpus__pluralist_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__pluralist_balancing, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__pluralist_balancing_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__pluralist_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) stems from the unpredictable nature of balancing, which creates high costs for those seeking legal certainty. Suppression (0.70) is high because the system actively enforces the interpretive authority of higher courts, limiting the autonomy of lower courts and litigants to challenge precedent. The theater ratio (0.20) is moderate; while there's genuine legal reasoning, some judicial rhetoric about 'balancing' can mask discretionary choices. Accessibility collapse (0.60) is moderate, as alternatives (e.g., legislative action) exist but are often slow or costly. Resistance (0.45) is present from litigants and some legal scholars, but not enough to fundamentally alter the system.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the appellate judiciary, this approach is a necessary and sophisticated mechanism for legal evolution. From the perspective of litigants and lower courts, it can feel like an arbitrary and costly system where the rules are constantly in flux, making legal outcomes less predictable and more expensive. The engine's per-seat classification should reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The appellate judiciary benefits from the flexibility and interpretive power this reading affords, allowing them to shape law. The legal profession also benefits from the increased demand for specialized expertise. Lower courts and litigants bear the costs of uncertainty and complexity, making them targets of extraction. The 'identity_locked' exit for lower courts reflects their professional obligation to apply precedent, even when its weight is ambiguous.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_discretion_vs_arbitrariness,
    'Is the ''balancing'' inherent in this reading a legitimate exercise of judicial discretion or a cover for arbitrary decision-making?',
    'Empirical analysis of judicial outcomes across similar cases and domains: high variance without clear, articulated principles would suggest arbitrariness.',
    'If arbitrary, the extractiveness and suppression would be reclassified higher, indicating a ''snare'' for litigants. If legitimate, the ''tangled_rope'' classification would be reinforced, with extraction seen as a cost of necessary flexibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_discretion_vs_arbitrariness, empirical, 'Distinguishing legitimate judicial discretion from arbitrary application of precedent.').

omega_variable(
    domain_specificity_justification,
    'Are the differences in precedent weight across legal domains genuinely justified by structural differences in those domains, or are they a historical artifact?',
    'Comparative legal analysis across jurisdictions with different approaches to precedent, examining whether domain-specific variations are consistently observed and justified.',
    'If unjustified, the complexity and associated costs for litigants would be seen as unnecessary extraction, pushing the classification closer to a ''snare''. If justified, it supports the ''tangled_rope'' as a necessary adaptation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_specificity_justification, conceptual, 'Whether domain-specific variations in precedent weight are structurally justified.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (e.g., lack of legal aid, high litigation costs) or internalized (e.g., lower courts'' deference to higher courts due to professional identity)?',
    'Post-exit suppression trajectory: if lower courts continue to defer even after structural barriers are reduced, reclassify as partially internalized. If litigants gain more agency with reduced costs, it''s structural.',
    'If internalized, the constraint''s effective suppression on lower courts is higher than the structural measure suggests, as they carry the suppression with them. If structural, remedies focus on external barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for lower courts and litigants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__pluralist_balancing, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1950, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(comm_tr_t1970, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(comm_tr_t1990, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(comm_tr_t2010, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(comm_tr_t2024, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(comm_be_t1950, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(comm_be_t1970, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(comm_be_t1990, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 1990, 0.63).
narrative_ontology:measurement(comm_be_t2010, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement(comm_be_t2024, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1950, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 1950, 0.6).
narrative_ontology:measurement(comm_su_t1970, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(comm_su_t1990, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(comm_su_t2010, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(comm_su_t2024, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__pluralist_balancing, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, evolutionary_framework).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'common_law_precedent_corpus' kernel. This 'pluralist_balancing' reading emphasizes context-dependent weight, contrasting with 'strict_stare_decisis' (absolute binding) and 'evolutionary_framework' (normative adaptation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
