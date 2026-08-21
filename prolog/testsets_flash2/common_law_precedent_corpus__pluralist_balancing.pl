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
 *   human_readable: Common Law Precedent (Pluralist Balancing Reading)
 *   domain: legal/jurisprudence
 *
 * SUMMARY:
 *   This constraint describes the 'pluralist balancing' reading of common law
 *   precedent, where the weight of prior judicial decisions is not absolute
 *   but varies by legal domain, context, and policy considerations. It aims
 *   to balance legal stability with adaptation, but this flexibility
 *   introduces complexity and unpredictability for those subject to it. This
 *   reading is instantiated as a Tangled Rope because it provides a genuine
 *   coordination function (legal evolution) but also involves asymmetric
 *   extraction (unpredictability costs for litigants and lower courts,
 *   interpretive discretion for higher courts).
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
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__pluralist_balancing, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__pluralist_balancing, "Common Law Precedent (Pluralist Balancing Reading)").
narrative_ontology:topic_domain(common_law_precedent_corpus__pluralist_balancing, "legal/jurisprudence").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__pluralist_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__pluralist_balancing, '39d2984a-2125-4141-8318-182d02e3c4f5').
narrative_ontology:cs_kernel_codification('39d2984a-2125-4141-8318-182d02e3c4f5', formalized).
narrative_ontology:cs_authority_grounding('39d2984a-2125-4141-8318-182d02e3c4f5', lineage).
narrative_ontology:cs_interpretation_layer_present('39d2984a-2125-4141-8318-182d02e3c4f5').
narrative_ontology:cs_reading_relation('39d2984a-2125-4141-8318-182d02e3c4f5', common_law_precedent_corpus__strict_stare_decisis, coexists_with).
narrative_ontology:cs_reading_relation('39d2984a-2125-4141-8318-182d02e3c4f5', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_axiom('39d2984a-2125-4141-8318-182d02e3c4f5', foundational, contextual_relevance_of_precedent).
narrative_ontology:cs_axiom_status(contextual_relevance_of_precedent, holdable).
narrative_ontology:cs_axiom_grounding('39d2984a-2125-4141-8318-182d02e3c4f5', contextual_relevance_of_precedent, conventional).
narrative_ontology:cs_axiom('39d2984a-2125-4141-8318-182d02e3c4f5', foundational, balancing_stability_and_adaptation).
narrative_ontology:cs_axiom_status(balancing_stability_and_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('39d2984a-2125-4141-8318-182d02e3c4f5', balancing_stability_and_adaptation, instrumental).
narrative_ontology:cs_reference_frame('39d2984a-2125-4141-8318-182d02e3c4f5', dynamic_common_law_evolution).
narrative_ontology:cs_drift_state('39d2984a-2125-4141-8318-182d02e3c4f5', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('39d2984a-2125-4141-8318-182d02e3c4f5', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, appellate_courts).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, legal_scholars).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, litigants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, lower_courts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, legal_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and apply precedent, balancing stability with adaptation. They decide when to distinguish, limit, or overrule prior cases, often citing policy considerations or evolving social norms. This flexibility allows them to shape legal development but also introduces discretion.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, appellate_courts, agenda_setter,
    institutional, generational, constrained, national).

% Bound by appellate precedent but must interpret its application to diverse factual scenarios. The pluralist balancing approach means they face uncertainty regarding which factors will be deemed decisive by higher courts, leading to increased workload and risk of reversal.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, lower_courts, payer,
    organized, biographical, constrained, regional).

% Seek predictable outcomes but encounter a system where precedent can be reinterpreted or distinguished based on context. This leads to higher legal costs, longer case durations, and less certainty about the outcome, especially in novel or evolving areas of law.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, litigants, payer,
    moderate, immediate, constrained, local).

% Benefit from the intellectual challenge of analyzing the complex interplay of precedent, policy, and context. They contribute to the discourse on legal evolution and influence judicial reasoning, but do not directly enforce the constraint.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, legal_scholars, beneficiary,
    analytical, generational, analytical, global).

% Advise clients and argue cases within a framework where the weight of precedent is not absolute. They must anticipate how courts will balance competing considerations, requiring extensive research and strategic argumentation, increasing the complexity and cost of legal services.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, legal_practitioners, payer,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for legal decision-making that balances the need for stability and predictability in law with the imperative for adaptation to new social, economic, and technological realities, preventing ossification or arbitrary change.
% TRANSFER_FUNCTION: Transfers interpretive authority and discretion to higher courts and legal scholars, while imposing costs of uncertainty and complexity on lower courts, litigants, and legal practitioners.
% ABSENT_VOICES: Citizens seeking clear, unambiguous legal rules would object to the inherent uncertainty and complexity of this balancing act, arguing for greater predictability. Their voices are often diffuse and not directly represented in judicial deliberations.
% DISAPPEARANCE_RATIONALE: If this approach to precedent vanished, the legal system would either become rigidly static (if strict stare decisis prevailed universally) or entirely unpredictable (if every case were decided de novo), leading to a collapse of legal order and massive societal reorganization.
% FOUNDING_PROBLEM: The common law tradition faced the challenge of maintaining legal stability while allowing for necessary evolution in response to changing societal values and circumstances, avoiding both rigid adherence to outdated rules and arbitrary judicial discretion.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and contemporary jurists widely corroborate that balancing stability and adaptation remains a core, ongoing challenge in common law systems, with continuous debate over the appropriate weight given to each. This is attested by academic literature and judicial opinions from outside the immediate beneficiaries.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__pluralist_balancing, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__pluralist_balancing, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__pluralist_balancing, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate-high (0.65) due to the costs imposed by legal uncertainty and the need for extensive argumentation to navigate the balancing act. Suppression (0.70) is also high, as lower courts and litigants are bound by the interpretive discretion of higher courts, with limited avenues to challenge the framework itself. Theater ratio is low (0.20) because the balancing function is genuinely performed, though its outcomes are often contested. The time series shows a gradual increase in extractiveness and suppression, reflecting the growing complexity of legal domains and the increasing scope of judicial discretion over time.
 *
 * PERSPECTIVAL GAP:
 *   Higher courts perceive this as a necessary and legitimate balancing act, a sophisticated mechanism for legal development. Litigants and lower courts, however, experience it as a source of unpredictability and increased burden, where the 'rules' can shift based on subjective judicial weighing of factors. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Appellate courts and legal scholars are beneficiaries (low d) as they gain interpretive authority and intellectual engagement. Lower courts, litigants, and legal practitioners are payers (high d) as they bear the costs of uncertainty, complexity, and the risk of reversal. The system coordinates legal evolution but extracts from those who must navigate its nuances.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_discretion_vs_arbitrariness,
    'At what point does ''balancing'' become indistinguishable from arbitrary judicial discretion, and how is this line policed?',
    'Empirical analysis of judicial decision-making patterns across different courts and domains, assessing consistency and the explicit articulation of balancing factors. Legal scholarship on the limits of judicial review.',
    'If balancing is found to be arbitrary, the constraint''s extractiveness and suppression would be higher, as it would represent pure power rather than reasoned judgment, potentially reclassifying it closer to a Snare for litigants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_discretion_vs_arbitrariness, empirical, 'Ambiguity between legitimate judicial discretion and unconstrained arbitrariness in applying precedent.').

omega_variable(
    domain_specificity_vs_general_principles,
    'To what extent can the ''domain and context'' variations in precedent weight be systematized into general principles, reducing unpredictability?',
    'Development of more refined meta-rules or guidelines by appellate courts, or comprehensive theoretical frameworks by legal scholars, that articulate how different domains should weigh precedent.',
    'Greater systematization would reduce unpredictability, lowering extractiveness for litigants and lower courts, potentially shifting the constraint closer to a Rope. Lack of systematization reinforces its Tangled Rope nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_specificity_vs_general_principles, conceptual, 'Whether context-dependent precedent can be made more predictable through higher-order rules.').

omega_variable(
    pluralist_balancing_vs_strict_stare_decisis_distinction,
    'Is the ''pluralist balancing'' reading truly distinct from ''strict stare decisis'' in practice, or does it merely represent a more nuanced application of the same underlying commitment to precedent?',
    'Comparative legal analysis of judicial opinions and outcomes in jurisdictions explicitly adhering to each reading, focusing on rates of overruling, distinguishing, and the types of justifications provided for departure from precedent.',
    'If the practical distinction is minimal, the ''pluralist balancing'' reading''s unique contribution to legal evolution is overstated, and its classification might converge with a more rigid, albeit still extractive, form of Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pluralist_balancing_vs_strict_stare_decisis_distinction, empirical, 'Distinction between pluralist balancing and strict stare decisis in practical application.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__pluralist_balancing, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1950, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(comm_tr_t1965, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 1965, 0.17).
narrative_ontology:measurement(comm_tr_t1980, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(comm_tr_t1995, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 1995, 0.19).
narrative_ontology:measurement(comm_tr_t2010, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(comm_tr_t2024, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(comm_be_t1950, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(comm_be_t1965, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 1965, 0.58).
narrative_ontology:measurement(comm_be_t1980, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(comm_be_t1995, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(comm_be_t2010, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(comm_be_t2024, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1950, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 1950, 0.6).
narrative_ontology:measurement(comm_su_t1965, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 1965, 0.63).
narrative_ontology:measurement(comm_su_t1980, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(comm_su_t1995, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 1995, 0.67).
narrative_ontology:measurement(comm_su_t2010, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 2010, 0.69).
narrative_ontology:measurement(comm_su_t2024, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__pluralist_balancing, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus__strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus__evolutionary_framework).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'common_law_precedent_corpus' kernel. This 'pluralist_balancing' reading emphasizes context-dependent weight, distinct from 'strict_stare_decisis' (rigid adherence) and 'evolutionary_framework' (adaptive reinterpretation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
