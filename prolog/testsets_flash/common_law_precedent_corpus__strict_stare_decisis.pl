% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__strict_stare_decisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__strict_stare_decisis, []).

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
 *   constraint_id: common_law_precedent_corpus__strict_stare_decisis
 *   human_readable: Strict Stare Decisis in Common Law Precedent
 *   domain: legal/jurisprudence
 *
 * SUMMARY:
 *   This constraint describes the 'strict stare decisis' reading of the
 *   common law precedent kernel, where prior judicial decisions are
 *   considered binding and can only be departed from with extraordinary
 *   justification. This reading emphasizes stability, predictability, and
 *   judicial restraint. It is one of several competing interpretations of how
 *   precedent should operate within common law systems.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, 0.45).
domain_priors:suppression_score(common_law_precedent_corpus__strict_stare_decisis, 0.6).
domain_priors:theater_ratio(common_law_precedent_corpus__strict_stare_decisis, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, extractiveness, 0.45).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__strict_stare_decisis, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__strict_stare_decisis, "Strict Stare Decisis in Common Law Precedent").
narrative_ontology:topic_domain(common_law_precedent_corpus__strict_stare_decisis, "legal/jurisprudence").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__strict_stare_decisis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__strict_stare_decisis, 'af3e03c3-5c21-42e7-a462-4d8f7c037230').
narrative_ontology:cs_kernel_codification('af3e03c3-5c21-42e7-a462-4d8f7c037230', formalized).
narrative_ontology:cs_authority_grounding('af3e03c3-5c21-42e7-a462-4d8f7c037230', lineage).
narrative_ontology:cs_interpretation_layer_present('af3e03c3-5c21-42e7-a462-4d8f7c037230').
narrative_ontology:cs_reading_relation('af3e03c3-5c21-42e7-a462-4d8f7c037230', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_reading_relation('af3e03c3-5c21-42e7-a462-4d8f7c037230', common_law_precedent_corpus__pluralist_balancing, coexists_with).
narrative_ontology:cs_axiom('af3e03c3-5c21-42e7-a462-4d8f7c037230', foundational, precedent_as_binding_rule).
narrative_ontology:cs_axiom_status(precedent_as_binding_rule, holdable).
narrative_ontology:cs_axiom_grounding('af3e03c3-5c21-42e7-a462-4d8f7c037230', precedent_as_binding_rule, conventional).
narrative_ontology:cs_axiom('af3e03c3-5c21-42e7-a462-4d8f7c037230', foundational, judicial_restraint_as_virtue).
narrative_ontology:cs_axiom_status(judicial_restraint_as_virtue, holdable).
narrative_ontology:cs_axiom_grounding('af3e03c3-5c21-42e7-a462-4d8f7c037230', judicial_restraint_as_virtue, deontological).
narrative_ontology:cs_reference_frame('af3e03c3-5c21-42e7-a462-4d8f7c037230', classical_legal_positivism).
narrative_ontology:cs_drift_state('af3e03c3-5c21-42e7-a462-4d8f7c037230', contemporary_legal_discourse, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('af3e03c3-5c21-42e7-a462-4d8f7c037230', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, judicial_legitimacy).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, legal_certainty).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, established_interests).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, litigants_challenging_precedent).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, social_movements_seeking_change).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable and predictable framework for legal decision-making, ensuring consistency across cases and allowing individuals and institutions to plan their actions with reasonable certainty about legal outcomes.
% TRANSFER_FUNCTION: Transfers the burden of proof and argument from established legal principles to those seeking to overturn them, effectively transferring legal certainty and stability to the beneficiaries at the cost of flexibility for challengers.
% ABSENT_VOICES: Future generations and marginalized groups whose interests may not have been adequately represented in past precedents are often absent from the conversation. They would argue for greater flexibility in precedent to address evolving societal norms and injustices.
% DISAPPEARANCE_RATIONALE: If strict stare decisis vanished, the legal system would lose its primary mechanism for consistency. Every case would be decided de novo, leading to chaos, unpredictability, and a collapse of legal certainty. The entire structure of common law jurisprudence would need to be rebuilt.
% FOUNDING_PROBLEM: The problem of arbitrary and inconsistent judicial decisions, leading to a lack of predictability and fairness in legal outcomes, which undermined public trust in the judiciary.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, practicing attorneys, and judicial opinions consistently corroborate the ongoing need for legal certainty and predictability, even while debating the optimal degree of rigidity. The problem of arbitrary justice remains a live concern, though the 'strict' interpretation of stare decisis is contested as the best solution.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__strict_stare_decisis, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__strict_stare_decisis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__strict_stare_decisis, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(common_law_precedent_corpus__strict_stare_decisis, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__strict_stare_decisis_tests).
:- end_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates legal certainty and predictability (beneficiaries: judicial_legitimacy, legal_certainty) while simultaneously extracting from those who seek to challenge established norms (victims: litigants_challenging_precedent, social_movements_seeking_change). The extractiveness (0.45) reflects the high burden of proof and cost associated with overturning precedent. Suppression (0.6) is moderate, as alternatives (new legal arguments, legislative change) exist but are significantly constrained by the binding nature of prior rulings. Theater ratio (0.2) is low, indicating that the justification for stability is largely functional, though some performative adherence to 'settled law' may occur even when its underlying rationale has weakened.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of judicial institutions and those benefiting from legal certainty, strict stare decisis is a necessary Rope, ensuring stability. From the perspective of litigants challenging precedent or social movements seeking legal change, it operates as a Snare, actively suppressing their ability to achieve desired outcomes. The engine's classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Judicial legitimacy and legal certainty are beneficiaries (d near 0.0) as the constraint provides a stable framework. Established interests also benefit from the inertia of prior rulings. Litigants challenging precedent and social movements seeking change are targets (d near 1.0) as they bear the direct costs and face high barriers to success. The constraint actively enforces its rigidity, requiring significant effort to overcome.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (legal stability) is still live, but its application can lead to mandatrophy if it prevents necessary legal evolution in response to changing social conditions. The 'strict stare decisis' reading, by prioritizing stability, risks becoming a Piton if the 'extraordinary justification' threshold becomes performative, or a Snare if it is consistently used to protect entrenched interests. The current classification as Tangled Rope acknowledges both its coordination function and its extractive potential, preventing mislabeling as a pure Rope (ignoring extraction) or pure Snare (ignoring coordination).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strict_stare_decisis_vs_evolutionary_framework,
    'Is this constraint a genuine commitment to legal stability, or a mechanism for preserving existing power structures under the guise of stability?',
    'Analysis of cases where precedent is overturned: if overturning consistently favors new powerful interests over established ones, it suggests a power-dynamic rather than stability-driven mechanism. If it consistently follows a clear, principled legal evolution, it supports the stability claim.',
    'If primarily power-driven, the constraint''s effective extractiveness is higher, and its classification shifts closer to Snare. If genuinely stability-driven, it remains a Tangled Rope with a strong coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strict_stare_decisis_vs_evolutionary_framework, conceptual, 'Ambiguity between genuine legal stability and power preservation.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''strict_stare_decisis'' reading of the ''common_law_precedent_corpus'' kernel. What would change if an ''evolutionary_framework'' or ''pluralist_balancing'' reading were adopted?',
    'Observing judicial practice in jurisdictions that explicitly adopt these alternative readings, particularly regarding the frequency and justification for overturning precedent.',
    'An ''evolutionary_framework'' reading would lower suppression and extractiveness by increasing the accessibility of challenging precedent, potentially shifting the constraint towards a Rope or even a Scaffold (if the evolution is seen as transitional). A ''pluralist_balancing'' reading would introduce more variability in extractiveness and suppression depending on the specific legal domain and context, making the constraint more dynamic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Impact of alternative readings of the common law precedent kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__strict_stare_decisis, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0, 0.25).
narrative_ontology:measurement(comm_tr_t10, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 10, 0.22).
narrative_ontology:measurement(comm_tr_t20, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(comm_be_t10, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(comm_be_t20, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(comm_su_t10, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(comm_su_t20, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__strict_stare_decisis, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__evolutionary_framework).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__pluralist_balancing).

% DUAL FORMULATION NOTE:
% This constraint is the 'strict_stare_decisis' reading of the 'common_law_precedent_corpus' kernel. It is linked to sibling readings that offer alternative interpretations of how precedent should bind.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
