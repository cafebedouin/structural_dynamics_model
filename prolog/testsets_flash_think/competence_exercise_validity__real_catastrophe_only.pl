% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__real_catastrophe_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__real_catastrophe_only, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: competence_exercise_validity__real_catastrophe_only
 *   human_readable: Competence Exercise Validity: Real Catastrophe Only Reading
 *   domain: Safety Engineering/Organizational Learning
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, 0.65).
domain_priors:suppression_score(competence_exercise_validity__real_catastrophe_only, 0.7).
domain_priors:theater_ratio(competence_exercise_validity__real_catastrophe_only, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, extractiveness, 0.65).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__real_catastrophe_only, mountain).
narrative_ontology:human_readable(competence_exercise_validity__real_catastrophe_only, "Competence Exercise Validity: Real Catastrophe Only Reading").
narrative_ontology:topic_domain(competence_exercise_validity__real_catastrophe_only, "Safety Engineering/Organizational Learning").

domain_priors:emerges_naturally(competence_exercise_validity__real_catastrophe_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__real_catastrophe_only, '932fe033-149a-4150-a25d-bfa0ad0424c8').
narrative_ontology:cs_kernel_codification('932fe033-149a-4150-a25d-bfa0ad0424c8', implicit).
narrative_ontology:cs_authority_grounding('932fe033-149a-4150-a25d-bfa0ad0424c8', practice).
narrative_ontology:cs_reading_relation('932fe033-149a-4150-a25d-bfa0ad0424c8', competence_exercise_validity__simulation_as_proxy, forecloses).
narrative_ontology:cs_reading_relation('932fe033-149a-4150-a25d-bfa0ad0424c8', competence_exercise_validity__continuous_refresh_hybrid, forecloses).
narrative_ontology:cs_axiom('932fe033-149a-4150-a25d-bfa0ad0424c8', foundational, catastrophe_as_sole_competence_validator).
narrative_ontology:cs_axiom_status(catastrophe_as_sole_competence_validator, holdable).
narrative_ontology:cs_axiom_grounding('932fe033-149a-4150-a25d-bfa0ad0424c8', catastrophe_as_sole_competence_validator, empirically_contingent).
narrative_ontology:cs_axiom('932fe033-149a-4150-a25d-bfa0ad0424c8', secondary, simulation_inherently_insufficient).
narrative_ontology:cs_axiom_status(simulation_inherently_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('932fe033-149a-4150-a25d-bfa0ad0424c8', simulation_inherently_insufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('932fe033-149a-4150-a25d-bfa0ad0424c8', pre_simulation_era_competence_validation).
narrative_ontology:cs_drift_state('932fe033-149a-4150-a25d-bfa0ad0424c8', contemporary_simulation_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('932fe033-149a-4150-a25d-bfa0ad0424c8', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, skeptical_analysts).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, catastrophe_response_teams).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, organizations_relying_on_simulation).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, safety_engineers_using_simulations).
narrative_ontology:constraint_vindicates(competence_exercise_validity__real_catastrophe_only, catastrophe_as_ultimate_test_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for the view that only real events validate competence, often gaining influence and resources by highlighting the inherent unreadiness of systems that rely on simulations. They frame the debate and set the terms for what counts as 'proven' competence.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, skeptical_analysts, agenda_setter,
    institutional, generational, analytical, global).

% Invest heavily in simulations for training and validation, believing them to be effective for competence development. They bear the cost of perceived unreadiness, reputational damage from failures, and the constant critique that their competence is untested by 'real' events.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, organizations_relying_on_simulation, payer,
    organized, biographical, constrained, national).

% Design and implement simulation-based training and validation programs. Their professional identity and career paths are often tied to these methods, making it difficult to exit the paradigm even when their work is devalued by the 'real catastrophe only' perspective.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, safety_engineers_using_simulations, payer,
    moderate, biographical, identity_locked, global).

% Their expertise and critical role are validated and amplified by the belief that only real catastrophes truly test competence. They are the ones called upon when the 'untested' competence of others fails, reinforcing their position.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, catastrophe_response_teams, beneficiary,
    organized, immediate, constrained, local).

% Develop advanced simulation technologies and methodologies. Their work is structurally devalued by this constraint, limiting the perceived utility and market for their innovations, as their tools are deemed insufficient for 'true' competence exercise.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, simulation_developers, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__real_catastrophe_only, diffuse).
narrative_ontology:fixing_cost_class(competence_exercise_validity__real_catastrophe_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a high, albeit often unachievable, standard for competence validation, implicitly coordinating skepticism towards simulated exercises and focusing attention on post-catastrophe learning.
% TRANSFER_FUNCTION: Transfers legitimacy, resources, and attention away from simulation-based competence validation and towards real-world experience, historical precedent, and post-catastrophe analysis. It also transfers a 'cost of unreadiness' to organizations that cannot or do not experience real catastrophes.
% ABSENT_VOICES: Advocates for advanced simulation, synthetic environments, and continuous refresh methodologies are structurally excluded from the core definition of 'true' competence exercise. They would argue for the validity and necessity of non-catastrophic competence exercise but are often dismissed as naive or lacking 'real-world' understanding.
% DISAPPEARANCE_RATIONALE: If this belief vanished, organizations would be free to fully embrace and trust simulation-based competence validation, potentially leading to different investment patterns in training, risk management, and safety protocols. The perceived 'untested' nature of competence would no longer be a default assumption, fundamentally altering how readiness is assessed and managed.
% FOUNDING_PROBLEM: The historical observation that complex systems often fail in unexpected ways despite extensive training and simulated drills, leading to a deep skepticism about the transferability of simulated competence to real-world chaos and emergent conditions.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of major industrial accidents, natural disaster responses, and military engagements where simulated training proved insufficient. Academic research in human factors and organizational psychology often highlights the 'gap' between training and real-world performance. This is attested by independent safety boards and academic researchers, not just those who benefit from the 'real catastrophe only' view.
narrative_ontology:disappearance_verdict(competence_exercise_validity__real_catastrophe_only, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__real_catastrophe_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__real_catastrophe_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(competence_exercise_validity__real_catastrophe_only, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__real_catastrophe_only, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__real_catastrophe_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, ExtMetricName, E),
    domain_priors:suppression_score(competence_exercise_validity__real_catastrophe_only, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(competence_exercise_validity__real_catastrophe_only),
    narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(competence_exercise_validity__real_catastrophe_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine natural law about competence, or a constructed belief that benefits identifiable agents by devaluing alternative forms of competence validation?',
    'Empirical studies demonstrating the transferability of simulated competence to real-world performance, or a shift in professional consensus regarding the definition of ''true'' competence.',
    'If a constructed belief, the constraint would reclassify from Mountain to a more extractive type (e.g., Tangled Rope or Snare), highlighting the social and institutional dynamics at play. This constraint is the ''real_catastrophe_only'' reading of the ''competence_exercise_validity'' kernel; sibling readings include ''simulation_as_proxy'' and ''continuous_refresh_hybrid''. The core disagreement is on the epistemic validity of non-catastrophic competence exercise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity between natural law and constructed belief regarding competence validation.').

omega_variable(
    empirical_testability_of_axiom,
    'Can ''competence'' truly be measured and validated outside of catastrophic events, or is the ''real catastrophe only'' axiom empirically untestable by definition?',
    'Development of robust, predictive metrics for competence in complex adaptive systems that are validated against non-catastrophic real-world performance, or a philosophical re-evaluation of ''competence'' itself.',
    'If empirically untestable, the axiom''s status as ''empirically_contingent'' would be challenged, potentially shifting its grounding_type to ''deontological'' or ''conventional'', which would alter how its foreclosure potential is computed by the engine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_testability_of_axiom, empirical, 'Whether the core axiom is empirically testable or a definitional claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__real_catastrophe_only, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__real_catastrophe_only, theater_ratio, 0, 0.3).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_validity__real_catastrophe_only, theater_ratio, 10, 0.33).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_validity__real_catastrophe_only, theater_ratio, 20, 0.36).
narrative_ontology:measurement(comp_tr_t30, competence_exercise_validity__real_catastrophe_only, theater_ratio, 30, 0.38).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_validity__real_catastrophe_only, theater_ratio, 40, 0.39).
narrative_ontology:measurement(comp_tr_t50, competence_exercise_validity__real_catastrophe_only, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(comp_be_t10, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(comp_be_t20, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(comp_be_t30, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(comp_be_t40, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(comp_be_t50, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(comp_su_t10, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(comp_su_t20, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(comp_su_t30, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 30, 0.69).
narrative_ontology:measurement(comp_su_t40, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(comp_su_t50, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, safety_regulation_design).
narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, organizational_training_budgets).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'competence_exercise_validity' kernel, which decomposes into multiple structurally distinct claims about how competence is exercised and validated. This reading focuses on the necessity of real catastrophe, while sibling readings explore the role of simulation and continuous refresh.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
