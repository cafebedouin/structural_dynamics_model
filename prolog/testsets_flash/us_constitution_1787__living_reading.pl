% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__living_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__living_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: us_constitution_1787__living_reading
 *   human_readable: US Constitution (Living Reading)
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'living Constitution' reading of the US
 *   Constitution, where its meaning is understood to evolve with societal
 *   values and needs, rather than being fixed at the time of its
 *   ratification. The text serves as an aspirational framework, allowing for
 *   judicial interpretation to adapt it to modern contexts, often expanding
 *   rights and governmental powers. This reading is distinct from originalist
 *   or positivist interpretations, which emphasize fixed intent or strict
 *   textualism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__living_reading, 0.45).
domain_priors:suppression_score(us_constitution_1787__living_reading, 0.3).
domain_priors:theater_ratio(us_constitution_1787__living_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__living_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__living_reading, "US Constitution (Living Reading)").
narrative_ontology:topic_domain(us_constitution_1787__living_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__living_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__living_reading, '780b1022-fcaa-4b29-8464-a3c96fcd2433').
narrative_ontology:cs_kernel_codification('780b1022-fcaa-4b29-8464-a3c96fcd2433', fixed_text).
narrative_ontology:cs_authority_grounding('780b1022-fcaa-4b29-8464-a3c96fcd2433', lineage).
narrative_ontology:cs_interpretation_layer_present('780b1022-fcaa-4b29-8464-a3c96fcd2433').
narrative_ontology:cs_reading_relation('780b1022-fcaa-4b29-8464-a3c96fcd2433', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('780b1022-fcaa-4b29-8464-a3c96fcd2433', us_constitution_1787__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('780b1022-fcaa-4b29-8464-a3c96fcd2433', foundational, constitutional_meaning_evolves).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves, holdable).
narrative_ontology:cs_axiom_grounding('780b1022-fcaa-4b29-8464-a3c96fcd2433', constitutional_meaning_evolves, deontological).
narrative_ontology:cs_axiom('780b1022-fcaa-4b29-8464-a3c96fcd2433', foundational, text_as_aspirational_framework).
narrative_ontology:cs_axiom_status(text_as_aspirational_framework, holdable).
narrative_ontology:cs_axiom_grounding('780b1022-fcaa-4b29-8464-a3c96fcd2433', text_as_aspirational_framework, conventional).
narrative_ontology:cs_reference_frame('780b1022-fcaa-4b29-8464-a3c96fcd2433', evolving_societal_consensus).
narrative_ontology:cs_drift_state('780b1022-fcaa-4b29-8464-a3c96fcd2433', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('780b1022-fcaa-4b29-8464-a3c96fcd2433', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__living_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, judicial_activists).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, social_progressives).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, marginalized_groups).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, originalist_scholars).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, states_rights_advocates).
narrative_ontology:constraint_vindicates(us_constitution_1787__living_reading, evolving_standards_of_decency).
narrative_ontology:constraint_vindicates(us_constitution_1787__living_reading, substantive_due_process).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the Constitution in light of contemporary societal values and evolving norms, often expanding rights or reinterpreting governmental powers. Their decisions shape the practical application of the living Constitution.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, supreme_court_justices, agenda_setter,
    institutional, generational, constrained, national).

% Advocate for judicial interpretations that align with modern social and political developments, benefiting from the flexibility of the living Constitution to achieve policy goals through the courts.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, judicial_activists, beneficiary,
    organized, biographical, mobile, national).

% Benefit from the expansion of rights and protections (e.g., privacy, LGBTQ+ rights) that are not explicitly enumerated in the original text but are recognized through evolving constitutional interpretation.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, social_progressives, beneficiary,
    organized, generational, constrained, national).

% Often find new protections and recognition under a living Constitution, as evolving interpretations address historical injustices and expand the scope of fundamental rights to previously excluded populations.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, marginalized_groups, beneficiary,
    powerless, generational, trapped, national).

% Bear the cost of seeing their interpretive methodology sidelined or explicitly rejected by courts adopting a living constitutionalist approach. Their academic and legal arguments for fixed meaning are often overridden.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, originalist_scholars, payer,
    moderate, generational, constrained, national).

% Experience a diminution of state autonomy as federal judicial power expands through broad interpretations of constitutional clauses, often centralizing authority and limiting state legislative discretion.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, states_rights_advocates, payer,
    organized, generational, constrained, national).

% The literal words of the Constitution, which, under a living reading, are treated as an aspirational framework rather than a strictly binding set of rules. Its original meaning is often subordinated to contemporary relevance.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, constitutional_text, excluded,
    powerless, civilizational, identity_locked, universal).
narrative_ontology:stakeholder_non_agent(us_constitution_1787__living_reading, constitutional_text).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a flexible framework for governance that can adapt to unforeseen societal changes and moral developments without requiring formal amendment, thereby maintaining the Constitution's relevance and legitimacy across generations.
% TRANSFER_FUNCTION: Transfers interpretive authority from the original framers' intent or strict textual meaning to contemporary judicial and societal values, leading to shifts in rights, powers, and limitations between federal and state governments, and between individuals and the state.
% ABSENT_VOICES: The 'dead hand' of the framers, as represented by originalist and textualist arguments, is often treated as less authoritative than contemporary moral and social consensus. Future generations, whose values might diverge, also lack a direct voice in current 'living' interpretations.
% DISAPPEARANCE_RATIONALE: If the 'living' interpretation vanished, the US legal and political system would face immense instability. Many established rights (e.g., privacy, aspects of equality) would lose their constitutional grounding, leading to widespread legal challenges, social unrest, and a fundamental reordering of federal-state relations and individual liberties.
% FOUNDING_PROBLEM: The framers understood that a static document could not govern a dynamic society indefinitely, anticipating the need for a mechanism to adapt the Constitution to future challenges and evolving moral understandings without constant, difficult formal amendments.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, historians, and political scientists from various ideological backgrounds acknowledge the historical challenge of constitutional rigidity. While they may dispute the *method* of adaptation, the problem of a static text in a dynamic society is widely recognized as a live concern, corroborated by the historical record of social change and legal evolution.
narrative_ontology:disappearance_verdict(us_constitution_1787__living_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__living_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__living_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(us_constitution_1787__living_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__living_reading_tests).
:- end_tests(us_constitution_1787__living_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the cost borne by those whose preferred interpretations (e.g., originalist, states' rights) are overridden by evolving judicial consensus. Suppression (0.30) is moderate, as alternative interpretations are not entirely silenced but are often marginalized in legal discourse and judicial outcomes. The theater ratio (0.20) is low, indicating that while there's some performative adherence to 'constitutional principles,' the core function of adaptation is genuine. The metrics show a gradual increase in extractiveness and suppression over time, reflecting the growing power of judicial interpretation and the increasing divergence from originalist views.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries, the living Constitution is a dynamic Rope, enabling necessary adaptation and progress. From the perspective of payers, it can feel like a Tangled Rope or even a Snare, where their foundational commitments are extracted from or suppressed by an unelected judiciary. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Judicial activists, social progressives, and marginalized groups are beneficiaries, as the living reading provides a flexible mechanism for expanding rights and achieving social goals. Originalist scholars and states' rights advocates are payers, as their interpretive frameworks and policy preferences are often subordinated. The Supreme Court justices act as agenda-setters, wielding significant power in shaping the Constitution's evolving meaning.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    elite_capture_of_evolving_norms,
    'To what extent do ''evolving societal norms'' reflect genuine broad consensus versus the preferences of judicial elites or specific advocacy groups?',
    'Empirical sociological studies of public opinion on constitutional issues, compared against judicial outcomes. Analysis of judicial appointments and their ideological alignment with ''evolving norms''.',
    'If ''evolving norms'' are primarily elite-driven, the constraint''s effective extractiveness and suppression for dissenting groups would be higher than measured, potentially reclassifying it closer to a Snare for those groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_capture_of_evolving_norms, empirical, 'Assesses the democratic legitimacy of ''evolving norms'' in constitutional interpretation.').

omega_variable(
    interpretive_stability_vs_flexibility,
    'Does the flexibility of the living Constitution lead to unpredictable and inconsistent legal outcomes, undermining the rule of law, or does it provide necessary stability by preventing constitutional obsolescence?',
    'Longitudinal studies of judicial precedent consistency and public trust in the judiciary across different interpretive eras. Comparative analysis with constitutional systems that employ more rigid amendment processes.',
    'If instability is high, the coordination function is weaker, increasing the effective extractiveness for all actors seeking predictable legal frameworks. If stability is maintained, the Rope-like coordination function is affirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_stability_vs_flexibility, conceptual, 'Examines the trade-off between interpretive flexibility and legal predictability.').

omega_variable(
    boundary_with_amendment_process,
    'At what point does ''evolving interpretation'' become a de facto amendment, bypassing the formal amendment process and undermining democratic legitimacy?',
    'Legal and political theory analysis of the scope of judicial review versus legislative power. Historical case studies where judicial decisions have had effects comparable to constitutional amendments.',
    'If interpretation frequently bypasses formal amendment, the constraint''s suppression of democratic processes is higher, pushing it closer to a Snare for the legislative branch and the populace.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(boundary_with_amendment_process, conceptual, 'Defines the line between judicial interpretation and constitutional amendment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__living_reading, 1787, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1787, us_constitution_1787__living_reading, theater_ratio, 1787, 0.05).
narrative_ontology:measurement(us_c_tr_t1865, us_constitution_1787__living_reading, theater_ratio, 1865, 0.1).
narrative_ontology:measurement(us_c_tr_t1937, us_constitution_1787__living_reading, theater_ratio, 1937, 0.15).
narrative_ontology:measurement(us_c_tr_t1965, us_constitution_1787__living_reading, theater_ratio, 1965, 0.18).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_1787__living_reading, theater_ratio, 2000, 0.19).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_1787__living_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1787, us_constitution_1787__living_reading, base_extractiveness, 1787, 0.1).
narrative_ontology:measurement(us_c_be_t1865, us_constitution_1787__living_reading, base_extractiveness, 1865, 0.25).
narrative_ontology:measurement(us_c_be_t1937, us_constitution_1787__living_reading, base_extractiveness, 1937, 0.35).
narrative_ontology:measurement(us_c_be_t1965, us_constitution_1787__living_reading, base_extractiveness, 1965, 0.4).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_1787__living_reading, base_extractiveness, 2000, 0.43).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_1787__living_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1787, us_constitution_1787__living_reading, suppression_requirement, 1787, 0.1).
narrative_ontology:measurement(us_c_su_t1865, us_constitution_1787__living_reading, suppression_requirement, 1865, 0.2).
narrative_ontology:measurement(us_c_su_t1937, us_constitution_1787__living_reading, suppression_requirement, 1937, 0.25).
narrative_ontology:measurement(us_c_su_t1965, us_constitution_1787__living_reading, suppression_requirement, 1965, 0.28).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_1787__living_reading, suppression_requirement, 2000, 0.29).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_1787__living_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__living_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__positivist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_supreme_court_precedent).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the US Constitution (1787) kernel. It represents the 'living Constitution' interpretation, emphasizing evolving meaning. It structurally influences and is influenced by the 'originalist' and 'positivist' readings, as well as the body of Supreme Court precedent it generates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
