% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__hybrid_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__hybrid_encoding_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_survival__hybrid_encoding_reading
 *   human_readable: Catastrophe Memory Survival: Hybrid Encoding Reading
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes the structural necessity for rituals to operate
 *   on dual registers—symbolic boundary-maintenance and embedded practical
 *   knowledge—for the long-term survival of communities, particularly after
 *   catastrophic events. This 'hybrid encoding' is presented as a natural law
 *   of collective memory and adaptation. The constraint is a specific reading
 *   of the broader 'catastrophe_memory_survival' kernel, emphasizing the
 *   inseparability and co-dependence of symbolic and practical functions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__hybrid_encoding_reading, 0.15).
domain_priors:suppression_score(catastrophe_memory_survival__hybrid_encoding_reading, 0.2).
domain_priors:theater_ratio(catastrophe_memory_survival__hybrid_encoding_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__hybrid_encoding_reading, mountain).
narrative_ontology:human_readable(catastrophe_memory_survival__hybrid_encoding_reading, "Catastrophe Memory Survival: Hybrid Encoding Reading").
narrative_ontology:topic_domain(catastrophe_memory_survival__hybrid_encoding_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:emerges_naturally(catastrophe_memory_survival__hybrid_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__hybrid_encoding_reading, 'e4550308-7e1f-40ea-8c8d-c01f6dfb5292').
narrative_ontology:cs_kernel_codification('e4550308-7e1f-40ea-8c8d-c01f6dfb5292', implicit).
narrative_ontology:cs_authority_grounding('e4550308-7e1f-40ea-8c8d-c01f6dfb5292', practice).
narrative_ontology:cs_interpretation_layer_present('e4550308-7e1f-40ea-8c8d-c01f6dfb5292').
narrative_ontology:cs_reading_relation('e4550308-7e1f-40ea-8c8d-c01f6dfb5292', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('e4550308-7e1f-40ea-8c8d-c01f6dfb5292', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_axiom('e4550308-7e1f-40ea-8c8d-c01f6dfb5292', foundational, symbolic_and_practical_inseparable).
narrative_ontology:cs_axiom_status(symbolic_and_practical_inseparable, holdable).
narrative_ontology:cs_axiom_grounding('e4550308-7e1f-40ea-8c8d-c01f6dfb5292', symbolic_and_practical_inseparable, empirically_contingent).
narrative_ontology:cs_reference_frame('e4550308-7e1f-40ea-8c8d-c01f6dfb5292', integrated_ritual_efficacy).
narrative_ontology:cs_drift_state('e4550308-7e1f-40ea-8c8d-c01f6dfb5292', contemporary_academic_discourse, gap(stable, minor, false)).
narrative_ontology:cs_created_at('e4550308-7e1f-40ea-8c8d-c01f6dfb5292', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, survivor_communities).
narrative_ontology:constraint_victim(catastrophe_memory_survival__hybrid_encoding_reading, reductionist_analysts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities depend on the ritual for both their collective identity and the practical knowledge necessary for long-term survival after a catastrophe. They intuitively maintain the dual registers without needing theoretical separation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, survivor_communities, beneficiary,
    organized, generational, identity_locked, local).

% Academics or researchers who attempt to force ritual into a single, exclusive category (either purely symbolic or purely instrumental). They struggle to account for the observed resilience and adaptive capacity of communities that maintain both registers, leading to incomplete or contradictory theories.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, reductionist_analysts, payer,
    analytical, biographical, constrained, global).

% Individuals within the community responsible for the accurate transmission and performance of the ritual. They embody and transmit the hybrid encoding, ensuring both symbolic fidelity and practical efficacy.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, ritual_practitioners, agenda_setter,
    moderate, biographical, identity_locked, local).

% Scholars or anthropologists who study the phenomenon of catastrophe memory and ritual, seeking to understand its mechanisms and effects without imposing a priori theoretical frameworks. They are positioned to observe the efficacy of the hybrid encoding.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, external_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory and action by embedding both symbolic meaning and practical survival knowledge within a single ritual framework, ensuring intergenerational transmission of critical information and identity.
% TRANSFER_FUNCTION: Transfers collective identity, shared meaning, and practical survival skills (e.g., resource management, hazard recognition, social cohesion strategies) across generations within a community.
% ABSENT_VOICES: The voices of communities that failed to survive due to an inability to maintain this hybrid encoding are absent; their failure is a silent corroboration of the constraint's necessity.
% DISAPPEARANCE_RATIONALE: If the hybrid encoding vanished, communities would lose both their coherent identity and the practical knowledge vital for survival, leading to social fragmentation and eventual collapse in the face of ongoing challenges.
% FOUNDING_PROBLEM: The problem of how to ensure the long-term survival and identity of a community in the aftermath of a catastrophic event, requiring both symbolic cohesion and practical adaptation.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of post-catastrophe societies, historical accounts of resilient communities, and the lived experience of survivor communities themselves corroborate the ongoing necessity of this hybrid encoding for survival. External observers and scholars attest to its continued relevance.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__hybrid_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__hybrid_encoding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__hybrid_encoding_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_survival__hybrid_encoding_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, ExtMetricName, E),
    domain_priors:suppression_score(catastrophe_memory_survival__hybrid_encoding_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(catastrophe_memory_survival__hybrid_encoding_reading),
    narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Mountain because the hybrid encoding appears to be an irreducible structural feature for community survival in certain contexts; it emerges naturally from the adaptive pressures of post-catastrophe environments. Extractiveness is low (0.15) as the 'cost' is primarily the cognitive effort of maintaining complexity, not an asymmetric transfer. Suppression is low (0.2) because the constraint is self-enforcing through its efficacy; resistance is minimal as communities that fail to adopt this encoding do not survive. Accessibility collapse is high (0.8) because viable alternatives for long-term survival are severely limited without this dual function. Theater ratio is low (0.1) as the ritual's functions are genuinely vital.
 *
 * PERSPECTIVAL GAP:
 *   Survivor communities experience this as a natural, life-sustaining practice, while reductionist analysts perceive it as a theoretical challenge or an 'impure' form of ritual. The engine's classification will highlight how the constraint's 'naturalness' for one group imposes a 'cost' on another's analytical framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Survivor communities are the primary beneficiaries (d near 0.0) as the constraint directly enables their continued existence and identity. Reductionist analysts are victims (d near 1.0) because their theoretical frameworks are 'extracted from' by the constraint's refusal to fit into a single, exclusive category, forcing them to confront the limitations of their models. Ritual practitioners are agenda-setters (d near 0.5) as they actively maintain the constraint, but are also subject to its demands for fidelity.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_cultural_construct,
    'Is the necessity of hybrid encoding for catastrophe memory survival a genuine natural law of collective adaptation, or a culturally contingent strategy that could be otherwise?',
    'Comparative studies of diverse post-catastrophe societies across different cultural contexts: if the hybrid encoding consistently emerges as the most resilient strategy, it supports natural law; if other, equally effective, single-register strategies are found, it supports cultural construct.',
    'If a natural law, the classification as Mountain is robust. If a cultural construct, the constraint might be reclassified as a highly effective Rope or even a Tangled Rope if specific agents benefit from its enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_cultural_construct, empirical, 'Ambiguity between natural law and cultural construct for hybrid encoding.').

omega_variable(
    analytical_victimhood_validity,
    'Is the ''victimhood'' of reductionist analysts a genuine structural extraction, or merely a consequence of their chosen analytical framework''s limitations?',
    'Analysis of the career costs and institutional pressures faced by analysts who attempt to maintain reductionist frameworks in the face of contradictory evidence from hybrid-encoded rituals. If there are tangible professional penalties for failing to account for hybridity, it supports extraction.',
    'If genuine extraction, the current classification holds. If merely a limitation of framework, the ''victim'' status would be re-evaluated, potentially lowering the overall extractiveness from this seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(analytical_victimhood_validity, conceptual, 'Validity of analytical frameworks as ''victims'' of complex phenomena.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__hybrid_encoding_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 25, 0.1).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 75, 0.1).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 25, 0.15).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 75, 0.15).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 25, 0.2).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 50, 0.2).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 75, 0.2).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 100, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__hybrid_encoding_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__competence_transmission_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'catastrophe_memory_survival' kernel. This 'hybrid_encoding_reading' emphasizes the co-dependence of symbolic and practical functions, distinguishing it from readings that prioritize one over the other. All three readings are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
