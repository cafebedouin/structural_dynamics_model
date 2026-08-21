% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__strategic_culture_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__strategic_culture_drift, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: total_war_winnability_post1945__strategic_culture_drift
 *   human_readable: Post-1945 Total War Winnability (Strategic Culture Drift Reading)
 *   domain: international_relations_theory/strategic_studies/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint describes the ideational shift in strategic culture
 *   post-1945, where the concept of 'total war winnability' dropped from
 *   elite discourse, not due to structural impossibility or normative
 *   illegitimacy, but due to a change in how strategists and policymakers
 *   *think* about war. It is a Piton because the capacity for total war
 *   remains, but the intellectual infrastructure to seriously consider it has
 *   atrophied, maintained by the inertia of limited war frameworks. The
 *   claimed type (Piton) reflects this atrophy and performative maintenance,
 *   while the metrics capture the low but persistent extraction and
 *   suppression of alternative strategic thought.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__strategic_culture_drift, 0.25).
domain_priors:suppression_score(total_war_winnability_post1945__strategic_culture_drift, 0.4).
domain_priors:theater_ratio(total_war_winnability_post1945__strategic_culture_drift, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, extractiveness, 0.25).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__strategic_culture_drift, piton).
narrative_ontology:human_readable(total_war_winnability_post1945__strategic_culture_drift, "Post-1945 Total War Winnability (Strategic Culture Drift Reading)").
narrative_ontology:topic_domain(total_war_winnability_post1945__strategic_culture_drift, "international_relations_theory/strategic_studies/commitment_system_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__strategic_culture_drift, 'a3992c11-78b1-4463-90d5-32dfd22e70aa').
narrative_ontology:cs_kernel_codification('a3992c11-78b1-4463-90d5-32dfd22e70aa', implicit).
narrative_ontology:cs_authority_grounding('a3992c11-78b1-4463-90d5-32dfd22e70aa', practice).
narrative_ontology:cs_interpretation_layer_present('a3992c11-78b1-4463-90d5-32dfd22e70aa').
narrative_ontology:cs_reading_relation('a3992c11-78b1-4463-90d5-32dfd22e70aa', total_war_winnability_post1945__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('a3992c11-78b1-4463-90d5-32dfd22e70aa', total_war_winnability_post1945__normative_reading_drop, coexists_with).
narrative_ontology:cs_axiom('a3992c11-78b1-4463-90d5-32dfd22e70aa', foundational, strategic_culture_shapes_possibility).
narrative_ontology:cs_axiom_status(strategic_culture_shapes_possibility, holdable).
narrative_ontology:cs_axiom_grounding('a3992c11-78b1-4463-90d5-32dfd22e70aa', strategic_culture_shapes_possibility, empirically_contingent).
narrative_ontology:cs_axiom('a3992c11-78b1-4463-90d5-32dfd22e70aa', secondary, ideational_inertia_is_real).
narrative_ontology:cs_axiom_status(ideational_inertia_is_real, holdable).
narrative_ontology:cs_axiom_grounding('a3992c11-78b1-4463-90d5-32dfd22e70aa', ideational_inertia_is_real, empirically_contingent).
narrative_ontology:cs_reference_frame('a3992c11-78b1-4463-90d5-32dfd22e70aa', post_nuclear_strategic_recalibration).
narrative_ontology:cs_drift_state('a3992c11-78b1-4463-90d5-32dfd22e70aa', contemporary_multipolar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a3992c11-78b1-4463-90d5-32dfd22e70aa', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, limited_war_theorists).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, defense_intellectuals).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, strategic_flexibility).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, military_planners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, political_leaders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Academics and policy analysts who developed and propagated theories of limited war, shaping strategic discourse away from total war concepts. Their careers and intellectual frameworks are invested in the continued salience of these ideas, even if the underlying conditions have shifted.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, limited_war_theorists, agenda_setter,
    institutional, generational, identity_locked, global).

% A broader community of experts who benefit from the established frameworks of limited conflict, as these provide stable intellectual terrain and policy relevance. Challenging the 'unwinnability' of total war would disrupt their professional standing.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, defense_intellectuals, beneficiary,
    organized, biographical, constrained, national).

% The capacity for states to consider and plan for a full spectrum of conflict, including total war, as a theoretical possibility. This capacity is atrophied by the ideational constraint, leading to blind spots in strategic planning.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, strategic_flexibility, payer,
    powerless, generational, trapped, universal).
narrative_ontology:stakeholder_non_agent(total_war_winnability_post1945__strategic_culture_drift, strategic_flexibility).

% Professionals tasked with developing war plans. They are constrained by the prevailing strategic culture that de-emphasizes total war, potentially leading to incomplete or unrealistic contingency planning. Their professional identity is tied to working within accepted doctrines.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, military_planners, payer,
    institutional, biographical, constrained, national).

% Benefit from a strategic discourse that frames total war as unthinkable or unwinnable, as it simplifies decision-making and reduces public pressure for extreme preparations. They can selectively engage with or ignore the constraint as politically convenient.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, political_leaders, beneficiary,
    powerful, immediate, mobile, national).

% Academics who analyze the social construction of strategic concepts and the power dynamics embedded in discourse. They observe and critique the ideational shift, identifying its beneficiaries and victims without being directly subject to its operational effects.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, critical_theorists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates strategic thought and planning around a shared understanding of acceptable and feasible levels of conflict, reducing cognitive load and facilitating communication within the defense establishment.
% TRANSFER_FUNCTION: Transfers intellectual capital and professional legitimacy to those who operate within the limited war framework, while transferring strategic blind spots and reduced planning flexibility to military and political decision-makers.
% ABSENT_VOICES: Advocates for a more comprehensive, 'full-spectrum' strategic realism, including the theoretical possibility of total war, are marginalized or dismissed as anachronistic. Their voices are absent from mainstream strategic discourse, which is dominated by limited war paradigms.
% DISAPPEARANCE_RATIONALE: If the ideational constraint vanished, strategic discourse would immediately broaden, military planning would re-evaluate total war scenarios, and defense intellectuals would face a crisis of relevance, forcing a re-evaluation of fundamental assumptions about conflict.
% FOUNDING_PROBLEM: The existential threat of nuclear weapons post-1945 created a need to conceptualize conflict in ways that avoided global annihilation, leading to the development of limited war theories.
% FOUNDING_PROBLEM_CORROBORATION: The threat of nuclear escalation remains live, as attested by international security experts and non-proliferation advocates outside the immediate beneficiaries of limited war theory. However, the *ideational* shift itself is contested as a sufficient explanation for the absence of total war, with other readings pointing to structural or normative factors.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__strategic_culture_drift, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__strategic_culture_drift, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__strategic_culture_drift, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_winnability_post1945__strategic_culture_drift, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__strategic_culture_drift, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__strategic_culture_drift_tests).
:- end_tests(total_war_winnability_post1945__strategic_culture_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the constraint primarily extracts intellectual flexibility and attention, not direct material resources. Suppression (0.4) is moderate, reflecting the soft power of academic consensus and institutional norms rather than overt coercion. Theater ratio (0.6) is high, as much of the discourse around limited war serves to maintain the illusion of total war's impossibility or irrelevance, rather than genuinely addressing the full spectrum of conflict. Accessibility collapse is low (0.3) because the physical capacity for total war still exists, but the ideational pathways to it are obscured. Resistance is low (0.1) because the ideational shift is subtle and diffuse, making organized resistance difficult.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiaries (limited war theorists, defense intellectuals) perceive this as a beneficial coordination mechanism that prevents catastrophic thinking. The victims (strategic flexibility, military planners) experience it as a subtle but real limitation on their capacity to adapt to evolving threats. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Limited war theorists and defense intellectuals are beneficiaries (d near 0.0) as their professional standing and intellectual frameworks are reinforced. Strategic flexibility and military planners are victims (d near 1.0) as their options and planning scope are curtailed. Political leaders are also beneficiaries (d near 0.0) as the constraint simplifies their decision-making. Critical theorists are observers (d near 0.5) as they analyze the constraint without being directly subject to its operational effects.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was to prevent nuclear annihilation by promoting limited war concepts. While the nuclear threat remains, the ideational constraint has atrophied into a Piton because it now primarily serves to maintain the intellectual dominance of limited war frameworks, rather than actively solving the problem of preventing total war. The 'unwinnability' claim has become theatrical, masking the underlying capacity for total war and extracting strategic flexibility. The classification prevents mislabeling this as a Rope (genuine coordination) or Mountain (natural law) by highlighting its performative and inertial aspects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ideational_vs_structural_causation,
    'To what extent is the absence of total war due to ideational shifts in strategic culture (this reading) versus structural factors like nuclear deterrence (structural_contraction_reading) or normative shifts in international law (normative_reading_drop)?',
    'Comparative historical analysis of strategic decision-making in different eras and political systems, controlling for nuclear capabilities and legal frameworks. Counterfactual analysis of how strategic culture might have evolved under different structural conditions.',
    'If structural factors are dominant, this constraint''s extractiveness and suppression are overstated, and it might be reclassified as a Mountain (structural_contraction_reading). If normative factors are dominant, it might be reclassified as a Rope (normative_reading_drop). If ideational factors are primary, this Piton classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ideational_vs_structural_causation, conceptual, 'Ambiguity regarding the primary causal mechanism for the decline of total war discourse.').

omega_variable(
    identity_lock_of_theorists,
    'How deeply are defense intellectuals'' professional identities fused with limited war frameworks, making it difficult for them to consider alternatives?',
    'Sociological studies of academic and policy communities, analysis of career trajectories, and discourse analysis of resistance to alternative strategic paradigms. Examine how new evidence or geopolitical shifts are absorbed or rejected by the community.',
    'If identity-lock is severe, the constraint''s suppression is effectively higher for these beneficiaries, as their exit options from the ideational framework are more constrained than ''constrained'' suggests. This would amplify the effective extraction from strategic flexibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_of_theorists, empirical, 'Degree of identity fusion for beneficiaries of limited war frameworks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__strategic_culture_drift, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1965, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1965, 0.3).
narrative_ontology:measurement(tota_tr_t1985, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1985, 0.5).
narrative_ontology:measurement(tota_tr_t2005, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2005, 0.6).
narrative_ontology:measurement(tota_tr_t2024, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1945, 0.1).
narrative_ontology:measurement(tota_be_t1965, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1965, 0.2).
narrative_ontology:measurement(tota_be_t1985, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1985, 0.25).
narrative_ontology:measurement(tota_be_t2005, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2005, 0.28).
narrative_ontology:measurement(tota_be_t2024, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1945, 0.1).
narrative_ontology:measurement(tota_su_t1965, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1965, 0.25).
narrative_ontology:measurement(tota_su_t1985, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1985, 0.35).
narrative_ontology:measurement(tota_su_t2005, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(tota_su_t2024, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
