% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__simulation_as_proxy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__simulation_as_proxy, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: competence_exercise_validity__simulation_as_proxy
 *   human_readable: Simulation Validity as Proxy-Catastrophe Exercise
 *   domain: safety/organizational
 *
 * SUMMARY:
 *   In safety-critical industries, competence must be maintained for
 *   catastrophic scenarios that rarely occur in practice. The regulatory
 *   constraint 'simulation counts as valid exercise that keeps competence
 *   occupied' solves a coordination problem: how to validate competence
 *   without waiting for catastrophes? The rule standardizes drill metrics as
 *   the competence proxy. This is claimed as genuine coordination (benefits
 *   operators by obviating real-catastrophe risk, benefits organizations by
 *   allowing planned training). The constraint also extracts: it transfers
 *   the risk that simulation-validated competence does not predict
 *   real-competence onto operators, and it extracts from frontline personnel
 *   by validating their capability via a model they do not design. The
 *   theater-ratio measurement (0.67 at interval end) reflects the reality
 *   that drill performance is increasingly a proxy for regulatory compliance
 *   rather than actual safety capability — the function has drifted from
 *   'prove competence' to 'demonstrate administrative participation.' This
 *   story is one reading of the contested kernel
 *   competence_exercise_validity; it instantiates the simulation_as_proxy
 *   reading, which claims drills are adequate proxies for catastrophe.
 *   Sibling readings (real_catastrophe_only, continuous_refresh_hybrid) are
 *   separate constraint stories with different epsilon values and different
 *   stakeholder structures.
 *
 * KEY AGENTS:
 *   - Regulatory authority: sets and enforces the rule; benefits from predictable, scheduled competence validation
 *   - Organization management: coordinates training schedules; benefits from the rule's administrativeibility; pays by accepting simulation-reality divergence
 *   - Frontline operators: carry the existential risk that drill competence does not predict real competence
 *   - Safety-critical personnel (powerless): trapped in the certification model; cannot demand alternative validation
 *   - Actual incident victims (excluded): bear the ground-truth test of the model; have no seat in its design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__simulation_as_proxy, 0.58).
domain_priors:suppression_score(competence_exercise_validity__simulation_as_proxy, 0.62).
domain_priors:theater_ratio(competence_exercise_validity__simulation_as_proxy, 0.67).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, theater_ratio, 0.67).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__simulation_as_proxy, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__simulation_as_proxy, "Simulation Validity as Proxy-Catastrophe Exercise").
narrative_ontology:topic_domain(competence_exercise_validity__simulation_as_proxy, "safety/organizational").

domain_priors:requires_active_enforcement(competence_exercise_validity__simulation_as_proxy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__simulation_as_proxy, '146dcb5e-928c-4c7c-bca9-099287295969').
narrative_ontology:cs_kernel_codification('146dcb5e-928c-4c7c-bca9-099287295969', formalized).
narrative_ontology:cs_authority_grounding('146dcb5e-928c-4c7c-bca9-099287295969', extraction).
narrative_ontology:cs_interpretation_layer_present('146dcb5e-928c-4c7c-bca9-099287295969').
narrative_ontology:cs_reading_relation('146dcb5e-928c-4c7c-bca9-099287295969', competence_exercise_validity__real_catastrophe_only, coexists_with).
narrative_ontology:cs_reading_relation('146dcb5e-928c-4c7c-bca9-099287295969', competence_exercise_validity__continuous_refresh_hybrid, influences).
narrative_ontology:cs_axiom('146dcb5e-928c-4c7c-bca9-099287295969', foundational, simulation_metrics_predict_real_competence).
narrative_ontology:cs_axiom_status(simulation_metrics_predict_real_competence, holdable).
narrative_ontology:cs_axiom_grounding('146dcb5e-928c-4c7c-bca9-099287295969', simulation_metrics_predict_real_competence, empirically_contingent).
narrative_ontology:cs_axiom('146dcb5e-928c-4c7c-bca9-099287295969', secondary, administrative_feasibility_justifies_model_choice).
narrative_ontology:cs_axiom_status(administrative_feasibility_justifies_model_choice, holdable).
narrative_ontology:cs_axiom_grounding('146dcb5e-928c-4c7c-bca9-099287295969', administrative_feasibility_justifies_model_choice, instrumental).
narrative_ontology:cs_reference_frame('146dcb5e-928c-4c7c-bca9-099287295969', simulation_adequacy_for_competence_validation).
narrative_ontology:cs_drift_state('146dcb5e-928c-4c7c-bca9-099287295969', contemporary_incident_analysis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('146dcb5e-928c-4c7c-bca9-099287295969', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, regulatory_authority).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, organization_management).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, frontline_operators).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, safety_critical_personnel).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__simulation_as_proxy, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_validity__simulation_as_proxy, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__simulation_as_proxy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__simulation_as_proxy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__simulation_as_proxy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.58 over the interval (t=0 to t=40), plateauing around t=20. The rise reflects an accumulation of incident post-mortems showing competence-certified personnel failing in real catastrophe in ways their drill performance would not predict. As the model's predictive validity comes into question, the extraction becomes more visible: organizations are still certifying competence via drill metrics, but the regulatory value of those metrics has degraded. Theater ratio rises from 0.48 to 0.67 and plateaus, indicating that drills increasingly serve administrative (demonstrate compliance, maintain budget, satisfy audits) rather than functional (certify real competence) purposes. Suppression remains stable at 0.62 because the constraint continues to be enforced via regulatory requirement, audit penalties, and professional licensing — the external enforcement machinery does not weaken even as the model's validity becomes contested. Accessibility collapse at 0.71 reflects that operators have no realistic alternative to accepting the simulation model: they cannot refuse drills (competence violation), cannot demand real-catastrophe testing (unethical), cannot request alternative validation (outside regulatory framework). They are constrained not by lack of exit physically, but by institutional entrapment: the rule is the ONLY legitimate path to competence certification.
 *
 * PERSPECTIVAL GAP:
 *   The regulatory authority sees the constraint as a coordination solution: 'We cannot wait for rare catastrophes; drills let us validate competence on schedule.' The organization sees it as administratively efficient: 'Drills are cheaper and more predictable than incident-based validation.' Frontline operators increasingly see it as a mismatch: 'Drills don't prepare me for the stress, cascade failures, and real uncertainty of actual catastrophe.' Safety researchers and incident investigators see it as model degradation: 'The metrics are validated against regulatory compliance, not against actual competence.' The payer seats (operators, safety-critical personnel) compute a higher directionality toward target (d near 0.85+) because they bear the risk transfer. The agenda-setter (regulatory authority) computes lower d (near 0.2) because it designed the rule. The beneficiary (organization management) sits between, paying some costs (must run drills) while collecting the convenience benefit. The engine will compute per-seat types from this structural divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory authority: d = 0.1–0.2. Sets the rule, derives authority and predictability from it, faces minimal enforcement burden, experiences no real catastrophe risk (that sits with operators). Full beneficiary position. Frontline operators: d = 0.80–0.90. Required to participate in drills, certified on a model they do not control, carry the risk that the model is invalid, cannot refuse or demand alternatives without professional penalty. Trapped in the certification system. Organization management: d = 0.35–0.45. Pays the cost of running drills (time, resources), benefits from administrative certainty and avoided real-catastrophe litigation (if competence is certified and catastrophe occurs, they have paper). Asymmetric but moderate. Safety-critical personnel (powerless): d = 0.85–0.95. Identical to operators structurally but amplified: they are powerless, so even if they recognize the model is inadequate they cannot organize objection or seek alternative validation. The rule extracts by their entrapment.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was built to solve: 'How do we validate catastrophe competence without waiting for catastrophes?' (founding problem, status=contested). The rule persists because regulatory compliance and organization administration depend on it. But the founding problem's status has shifted: operators and safety researchers increasingly attest that simulation-validated competence DOES NOT predict real competence in the necessary ways. The constraint extracts by continuing to certify competence via a model whose predictive validity is now contested. The theater ratio (0.67) reflects this mandatrophy: drills are increasingly maintained as ritual (satisfy audits, license requirements) rather than function (ensure actual safety capability). The constraint meets the mandatrophy test: (1) founding problem is contested/degraded; (2) constraint persists not because it solves the problem but because regulatory and organizational machinery depend on it; (3) the beneficiaries (regulatory authority, organization management) collect enough convenience/authority to resist reform; (4) the payers (operators, safety personnel) are dispersed enough and identity-locked enough that coalition resistance is difficult. This is not a pure piton (some beneficiaries do collect, unlike a piton's diffuse cost + no beneficiary pattern) — it is a tangled_rope with degraded function: coordination (schedule competence validation) is genuine, but extraction (transfer of unquantified risk to operators) is now visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_validity,
    'Does drill performance under simulation conditions predict actual competence under real catastrophic stress, with the same stressors (fatigue, real danger, institutional pressure, cascading failures)?',
    'Post-incident analysis of personnel who were competence-certified via simulation, comparing their actual response performance to their simulation metrics. Longitudinal data from organizations comparing drill-certified cohorts to cohorts trained under alternative models.',
    'If simulation predicts well, the constraint''s core assumption (proxy-catastrophe is valid) holds and the model is defensible. If prediction is poor or diverges systematically by stress condition, the competence model is invalid and the constraint extracts from operators by transferring unquantified risk to them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_validity, empirical, 'Whether simulation metrics predict real-catastrophe competence.').

omega_variable(
    epistemic_authority_of_model,
    'Who decides what counts as adequate fidelity in simulation, and on what grounds? Is the model validated against empirical data, or is it validated against regulatory compliance and organizational convenience?',
    'Audit of the technical decisions embedded in simulation design: which stressors are included/excluded, how are metrics weighted, what is the evidentiary basis for the fidelity choices? Compare to post-incident analysis of what actually determined performance.',
    'If model validation is empirical and robust, the extraction is justifiable as the cost of coordination. If validation is regulatory/organizational (chosen for feasibility rather than accuracy), the constraint extracts by substituting administrative convenience for safety assurance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_authority_of_model, conceptual, 'What epistemic standard the simulation model is held to.').

omega_variable(
    identity_fusion_in_competence_narrative,
    'Does the constraint create identity fusion where operators internalize the competence model (their professional identity is ''someone who passes drills'') in a way that obscures the gap between simulation and reality?',
    'Ethnographic study of operators'' own assessment of their competence; do they believe their drill certification reflects real catastrophe preparedness, or do they maintain separate models? Interview data from post-incident responses: what did operators know about the gap?',
    'If fusion is high, operators are suppressed not only externally (the rule) but internalized (they believe the rule is valid). The measured suppression of 0.62 would be substantially internalized identity-lock rather than structural barriers. If fusion is low, suppression is purely structural enforcement and operators actively contest the model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_in_competence_narrative, empirical, 'Internalization of the competence proxy model by operators.').

omega_variable(
    dual_kernel_reading_constraint_frontier,
    'Is the contestation between ''simulation_as_proxy'' and ''real_catastrophe_only'' logically a single kernel under dispute, or are they two genuinely distinct constraints with different epsilon values?',
    'Structural analysis: do the two readings converge on the SAME institutional commitment (competence validation framework), just with different fidelity criteria? Or do they diverge on what counts as competence entirely (one is training-based, the other is incident-based)? If they''re the same kernel with different readings, ε is stable (the fidelity disagreement is parameter-level). If they''re different constraints, the real_catastrophe_only framing has a different ε (much higher extraction because it demands continuous incident exposure) and should be authored as a separate file.',
    'If single kernel: reading_relations and axioms correctly decompose the dispute. If different constraints: the sibling_reading declarations are mislabeled and should be network links instead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dual_kernel_reading_constraint_frontier, conceptual, 'Whether the kernel contest is a single commitment with multiple readings or multiple constraints.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression (0.62) maintained by external enforcement (the regulatory rule, penalties for non-compliance) or by internalized acceptance of the competence model (operators believe drills are adequate)?',
    'Comparative analysis: in jurisdictions where regulatory enforcement of simulation-only competence is strict, do operators resist and demand alternatives (structural suppression)? In jurisdictions with lax enforcement, do operators voluntarily drill to the higher standard (internalized acceptance)?',
    'If structural: the constraint could be lifted by regulatory change, though operators might resist losing the convenience of drills. If internalized: removing the rule would not change practice; the constraint carries post-exit suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural enforcement or internalized model acceptance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__simulation_as_proxy, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__simulation_as_proxy, theater_ratio, 0, 0.48).
narrative_ontology:measurement(comp_tr_t5, competence_exercise_validity__simulation_as_proxy, theater_ratio, 5, 0.54).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_validity__simulation_as_proxy, theater_ratio, 10, 0.6).
narrative_ontology:measurement(comp_tr_t15, competence_exercise_validity__simulation_as_proxy, theater_ratio, 15, 0.64).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_validity__simulation_as_proxy, theater_ratio, 20, 0.66).
narrative_ontology:measurement(comp_tr_t25, competence_exercise_validity__simulation_as_proxy, theater_ratio, 25, 0.67).
narrative_ontology:measurement(comp_tr_t30, competence_exercise_validity__simulation_as_proxy, theater_ratio, 30, 0.67).
narrative_ontology:measurement(comp_tr_t35, competence_exercise_validity__simulation_as_proxy, theater_ratio, 35, 0.67).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_validity__simulation_as_proxy, theater_ratio, 40, 0.67).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(comp_be_t5, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(comp_be_t10, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(comp_be_t15, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 15, 0.56).
narrative_ontology:measurement(comp_be_t20, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(comp_be_t25, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 25, 0.59).
narrative_ontology:measurement(comp_be_t30, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 30, 0.59).
narrative_ontology:measurement(comp_be_t35, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 35, 0.58).
narrative_ontology:measurement(comp_be_t40, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(comp_su_t5, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(comp_su_t10, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(comp_su_t15, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(comp_su_t20, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(comp_su_t25, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(comp_su_t30, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(comp_su_t35, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 35, 0.62).
narrative_ontology:measurement(comp_su_t40, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__simulation_as_proxy, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_validity__simulation_as_proxy, 0.12).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity__real_catastrophe_only).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity__continuous_refresh_hybrid).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel competence_exercise_validity. The sibling readings (real_catastrophe_only, continuous_refresh_hybrid) are separate constraint stories in the same family. The readings converge on the same institutional commitment (competence validation framework) but diverge on what counts as valid evidence: simulation metrics only vs. incident-based vs. continuous refresher cycles. The epsilon values differ across readings because the fidelity criteria and victim/beneficiary structures differ. simulation_as_proxy claims simulation is adequate and extracts by risk transfer to operators. real_catastrophe_only claims only real incident response validates competence and would extract from organizations (demand incident exposure). continuous_refresh_hybrid claims both simulation and periodic real-world revalidation are needed and would extract from both organizations (continuous cost) and operators (ongoing validation burden). All three readings are live positions held by different stakeholder coalitions (regulatory authority + organization management support simulation_as_proxy; safety researchers + operators increasingly support real_catastrophe_only or continuous_refresh_hybrid).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_validity__simulation_as_proxy, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
