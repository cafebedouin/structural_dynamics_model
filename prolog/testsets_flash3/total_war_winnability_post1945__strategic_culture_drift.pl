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
 *   constraint_id: total_war_winnability_post1945__strategic_culture_drift
 *   human_readable: Post-1945 Total War Winnability (Strategic Culture Drift Reading)
 *   domain: international_relations_theory/strategic_studies/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint describes the ideational shift in strategic culture
 *   post-1945, where the concept of 'total war winnability' dropped from
 *   elite discourse, not due to structural impossibility or normative
 *   illegitimacy, but due to a change in how strategists and policymakers
 *   conceptualized conflict. It is a reading of the
 *   'total_war_winnability_post1945' kernel, focusing on the internal,
 *   ideational dynamics within strategic communities. The sibling readings
 *   are 'structural_contraction_reading' (nuclear weapons made total war
 *   impossible) and 'normative_reading_drop' (total war became normatively
 *   illegitimate).
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
narrative_ontology:cs_story_uid(total_war_winnability_post1945__strategic_culture_drift, 'ec0e4843-36ab-4bf1-a2de-9422fb23677a').
narrative_ontology:cs_kernel_codification('ec0e4843-36ab-4bf1-a2de-9422fb23677a', implicit).
narrative_ontology:cs_authority_grounding('ec0e4843-36ab-4bf1-a2de-9422fb23677a', practice).
narrative_ontology:cs_interpretation_layer_present('ec0e4843-36ab-4bf1-a2de-9422fb23677a').
narrative_ontology:cs_reading_relation('ec0e4843-36ab-4bf1-a2de-9422fb23677a', total_war_winnability_post1945__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec0e4843-36ab-4bf1-a2de-9422fb23677a', total_war_winnability_post1945__normative_reading_drop, coexists_with).
narrative_ontology:cs_axiom('ec0e4843-36ab-4bf1-a2de-9422fb23677a', foundational, strategic_thought_shapes_possibility).
narrative_ontology:cs_axiom_status(strategic_thought_shapes_possibility, holdable).
narrative_ontology:cs_axiom_grounding('ec0e4843-36ab-4bf1-a2de-9422fb23677a', strategic_thought_shapes_possibility, empirically_contingent).
narrative_ontology:cs_axiom('ec0e4843-36ab-4bf1-a2de-9422fb23677a', foundational, total_war_remains_physically_possible).
narrative_ontology:cs_axiom_status(total_war_remains_physically_possible, holdable).
narrative_ontology:cs_axiom_grounding('ec0e4843-36ab-4bf1-a2de-9422fb23677a', total_war_remains_physically_possible, empirically_contingent).
narrative_ontology:cs_reference_frame('ec0e4843-36ab-4bf1-a2de-9422fb23677a', post_nuclear_strategic_consensus).
narrative_ontology:cs_drift_state('ec0e4843-36ab-4bf1-a2de-9422fb23677a', contemporary_strategic_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ec0e4843-36ab-4bf1-a2de-9422fb23677a', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, defense_intellectuals_limited_war).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, limited_war_doctrine_advocates).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, strategic_planners).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, military_strategists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, political_leaders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Academics and policy analysts whose careers and influence are built on frameworks of limited war, deterrence, and crisis management. The idea of total war winnability is anathema to their intellectual edifice, and its absence from discourse reinforces their expertise.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, defense_intellectuals_limited_war, beneficiary,
    organized, biographical, identity_locked, global).

% Military and political leaders who have internalized and promote the strategic culture of limited conflict. They administer training, doctrine, and resource allocation based on these assumptions, effectively setting the agenda for what is considered 'thinkable' in warfare.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, limited_war_doctrine_advocates, agenda_setter,
    institutional, generational, constrained, global).

% Military and civilian personnel tasked with developing comprehensive war plans. They are constrained by the prevailing strategic culture, which limits their ability to explore or even articulate scenarios involving total war, potentially leading to gaps in preparedness for extreme contingencies.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, strategic_planners, payer,
    moderate, generational, constrained, global).

% Professionals whose expertise is shaped by the dominant strategic culture. They may intuitively recognize the continued possibility of total war but find it difficult to gain traction for such analyses within institutions that have largely abandoned the concept, risking professional marginalization.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, military_strategists, payer,
    moderate, biographical, identity_locked, global).

% Benefit from a strategic culture that frames conflicts as manageable and limited, reducing public anxiety and the perceived costs of military action. This allows for greater flexibility in deploying force without immediately invoking existential threats.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, political_leaders, beneficiary,
    powerful, immediate, mobile, national).

% Will bear the consequences if the capacity for total war is underestimated due to ideational drift, leading to catastrophic outcomes. They are excluded from the current discourse that shapes strategic culture.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, future_generations, excluded,
    powerless, civilizational, trapped, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates strategic thought and military planning around a shared understanding of acceptable and feasible levels of conflict, preventing unconstrained escalation in discourse and doctrine.
% TRANSFER_FUNCTION: Transfers intellectual capital and institutional legitimacy to proponents of limited war frameworks, while transferring strategic blind spots and potential vulnerabilities to those responsible for ultimate defense.
% ABSENT_VOICES: Strategists and historians who recognize the enduring possibility and potential winnability of total war, but whose perspectives are marginalized by the dominant strategic culture. Future generations, who would bear the costs of this ideational drift, are also absent.
% DISAPPEARANCE_RATIONALE: If the ideational constraint vanished, strategic discourse would immediately re-engage with the full spectrum of conflict, including total war scenarios. Military doctrines would be re-evaluated, and defense budgets might shift dramatically to account for previously 'unthinkable' contingencies, fundamentally altering international security dynamics.
% FOUNDING_PROBLEM: To manage the existential threat posed by nuclear weapons and prevent a return to the destructive patterns of 20th-century total wars, by fostering a strategic culture focused on limited objectives and deterrence.
% FOUNDING_PROBLEM_CORROBORATION: Defense intellectuals and political leaders attest that the problem of preventing unconstrained conflict remains live. However, critics (e.g., some military strategists and historians) argue that the current strategic culture has overshot, creating a new problem of under-preparedness for extreme but still possible scenarios.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__strategic_culture_drift, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__strategic_culture_drift, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__strategic_culture_drift, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The constraint is classified as a Piton because its primary function (preventing total war by making it unthinkable) has atrophied into a performative maintenance of limited war frameworks. Extractiveness is low (0.25) because it's primarily an ideational constraint, not directly extracting material resources, but it does extract strategic flexibility. Suppression (0.4) is moderate, reflecting the soft power of ideational conformity rather than overt coercion. Theater ratio is high (0.6) as much of the discourse around limited war serves to maintain the illusion that total war is not a live option, even as underlying capabilities persist. The rising theater ratio over time reflects the increasing performativity of this ideational constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of defense intellectuals, this ideational shift is a successful coordination mechanism that prevents catastrophic thinking. From the perspective of strategic planners, it's a constraint that limits their professional scope and potentially leaves them unprepared. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Defense intellectuals and advocates of limited war doctrines are beneficiaries, as their intellectual and institutional capital is enhanced by the constraint. Strategic planners and military strategists are payers, as their professional scope is narrowed and their ability to prepare for all contingencies is hampered. Political leaders benefit from the perceived manageability of conflict. Future generations are excluded, bearing the potential long-term costs of this ideational blind spot.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was to prevent total war. While the problem of total war remains live, the constraint has drifted. It no longer actively prevents total war through robust strategic thought, but rather through a performative suppression of the concept, leading to a piton classification. The mandatrophy is resolved by recognizing that the ideational shift has created a new problem of strategic inflexibility, rather than genuinely solving the original problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ideational_vs_structural_causation,
    'To what extent is the decline of total war discourse due to ideational shifts (strategic culture) versus structural changes (e.g., nuclear weapons, economic interdependence)?',
    'Comparative historical analysis across different strategic cultures and technological contexts, examining cases where ideational shifts occurred independently of major structural changes.',
    'If primarily structural, this reading''s ''piton'' classification might be less accurate, potentially shifting towards a ''mountain'' (structural impossibility) or ''rope'' (structural coordination). If primarily ideational, the piton classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ideational_vs_structural_causation, empirical, 'Distinguishing ideational from structural drivers of strategic change.').

omega_variable(
    strategic_culture_measurement,
    'How can ''strategic culture'' be robustly measured to track ideational drift, beyond elite discourse analysis?',
    'Development of quantitative metrics for doctrinal evolution, military training curricula, and public opinion on conflict escalation, triangulated with qualitative discourse analysis.',
    'Improved measurement would increase confidence in the ''extractiveness'' and ''theater_ratio'' metrics, potentially refining the piton classification or revealing a different type if the ideational shift is less pervasive than assumed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_culture_measurement, empirical, 'Robust measurement of strategic culture''s influence.').

omega_variable(
    kernel_reading_identity,
    'This constraint is the ''strategic_culture_drift'' reading of the ''total_war_winnability_post1945'' kernel. What would change if a sibling reading were adopted?',
    'Analyzing the logical implications of adopting the ''structural_contraction_reading'' (total war is impossible due to nukes) or ''normative_reading_drop'' (total war is illegitimate due to international law) on the classification and stakeholder dynamics.',
    'Adopting the ''structural_contraction_reading'' would likely shift the classification towards ''mountain'' (physical impossibility), with lower extractiveness and suppression. Adopting the ''normative_reading_drop'' would emphasize the role of international law, potentially leading to a ''rope'' or ''tangled_rope'' classification with different beneficiaries (e.g., international legal institutions).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Impact of adopting alternative kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__strategic_culture_drift, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1960, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(tota_tr_t1980, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1980, 0.4).
narrative_ontology:measurement(tota_tr_t2000, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2000, 0.5).
narrative_ontology:measurement(tota_tr_t2024, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1945, 0.1).
narrative_ontology:measurement(tota_be_t1960, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement(tota_be_t1980, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement(tota_be_t2000, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2000, 0.23).
narrative_ontology:measurement(tota_be_t2024, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1945, 0.2).
narrative_ontology:measurement(tota_su_t1960, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1960, 0.3).
narrative_ontology:measurement(tota_su_t1980, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(tota_su_t2000, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 2000, 0.38).
narrative_ontology:measurement(tota_su_t2024, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__strategic_culture_drift, identity_coordination).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, limited_war_doctrine).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, nuclear_deterrence_stability).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_winnability_post1945' kernel. This 'strategic_culture_drift' reading focuses on ideational shifts, distinct from structural or normative explanations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
