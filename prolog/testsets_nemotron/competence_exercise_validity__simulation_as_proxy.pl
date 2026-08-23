% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__simulation_as_proxy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: competence_exercise_validity__simulation_as_proxy
 *   human_readable: Simulation as Valid Proxy for Competence Exercise
 *   domain: safety_engineering/organizational_learning/competence_retention
 *
 * SUMMARY:
 *   Safety-critical industries (nuclear, chemical, aviation, rail) require
 *   certified competence. Over decades, regulators accepted high-fidelity
 *   simulation hours as equivalent to live drill requirements. The constraint
 *   is the rule that simulation completion counts as valid competence
 *   exercise. The arrangement coordinates a certification ecosystem:
 *   regulators get auditable compliance, vendors sell platforms, management
 *   meets targets cheaply. But the proxy has drifted — simulation metrics
 *   (scenario completion, score thresholds) have replaced the actual
 *   competence they were meant to proxy. Frontline operators train to the
 *   simulation, not the reality. The theater ratio rises as 'simulation
 *   fidelity' becomes a performative target. The claimed type is
 *   tangled_rope: genuine coordination (scalable certification) fused with
 *   asymmetric extraction (operators and public bear the competence gap).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__simulation_as_proxy, 0.68).
domain_priors:suppression_score(competence_exercise_validity__simulation_as_proxy, 0.55).
domain_priors:theater_ratio(competence_exercise_validity__simulation_as_proxy, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__simulation_as_proxy, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__simulation_as_proxy, "Simulation as Valid Proxy for Competence Exercise").
narrative_ontology:topic_domain(competence_exercise_validity__simulation_as_proxy, "safety_engineering/organizational_learning/competence_retention").

domain_priors:requires_active_enforcement(competence_exercise_validity__simulation_as_proxy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__simulation_as_proxy, 'd1841625-0c84-4a7a-a1e5-e91c21994208').
narrative_ontology:cs_kernel_codification('d1841625-0c84-4a7a-a1e5-e91c21994208', formalized).
narrative_ontology:cs_authority_grounding('d1841625-0c84-4a7a-a1e5-e91c21994208', lineage).
narrative_ontology:cs_interpretation_layer_present('d1841625-0c84-4a7a-a1e5-e91c21994208').
narrative_ontology:cs_reading_relation('d1841625-0c84-4a7a-a1e5-e91c21994208', competence_exercise_validity__continuous_refresh_hybrid, coexists_with).
narrative_ontology:cs_reading_relation('d1841625-0c84-4a7a-a1e5-e91c21994208', competence_exercise_validity__real_catastrophe_only, influences).
narrative_ontology:cs_axiom('d1841625-0c84-4a7a-a1e5-e91c21994208', foundational, simulation_fidelity_metrics_validate_competence).
narrative_ontology:cs_axiom_status(simulation_fidelity_metrics_validate_competence, holdable).
narrative_ontology:cs_axiom_grounding('d1841625-0c84-4a7a-a1e5-e91c21994208', simulation_fidelity_metrics_validate_competence, empirically_contingent).
narrative_ontology:cs_axiom('d1841625-0c84-4a7a-a1e5-e91c21994208', secondary, regulatory_compliance_suffices_for_safety_assurance).
narrative_ontology:cs_axiom_status(regulatory_compliance_suffices_for_safety_assurance, holdable).
narrative_ontology:cs_axiom_grounding('d1841625-0c84-4a7a-a1e5-e91c21994208', regulatory_compliance_suffices_for_safety_assurance, conventional).
narrative_ontology:cs_reference_frame('d1841625-0c84-4a7a-a1e5-e91c21994208', post_three_mile_island_certification_regime).
narrative_ontology:cs_drift_state('d1841625-0c84-4a7a-a1e5-e91c21994208', contemporary_high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d1841625-0c84-4a7a-a1e5-e91c21994208', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, regulatory_bodies).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, safety_certification_authorities).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, training_simulation_vendors).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, operational_management).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, frontline_operators).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, maintenance_crews).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, emergency_response_teams).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, public_safety).
narrative_ontology:constraint_vindicates(competence_exercise_validity__simulation_as_proxy, simulation_fidelity_suffices_for_competence).
narrative_ontology:constraint_vindicates(competence_exercise_validity__simulation_as_proxy, regulatory_compliance_equals_safety).
narrative_ontology:constraint_vindicates(competence_exercise_validity__simulation_as_proxy, proxy_validation_is_valid).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and enforce the rules that accept simulation hours as equivalent to live exercise for certification. They benefit from auditable, low-cost compliance metrics and avoid the political cost of mandating expensive live drills. Their authority rests on the proxy being accepted as valid.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, regulatory_bodies, agenda_setter,
    institutional, generational, arbitrage, national).

% Issue certificates based on simulation completion records. They collect fees and institutional legitimacy from a process that is administratively clean and legally defensible. They have little incentive to challenge the proxy because their business model depends on its acceptance.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, safety_certification_authorities, beneficiary,
    institutional, biographical, constrained, national).

% Sell simulation platforms, scenarios, and validation services. Their revenue scales with the regulatory mandate for simulation hours. They lobby for fidelity standards that favor their products and against live-drill requirements that would shrink their market.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, training_simulation_vendors, beneficiary,
    organized, biographical, mobile, global).

% Meet compliance targets with minimal disruption to production schedules. Simulation fits in shift gaps; live drills require shutdowns. They report 'competence maintained' to regulators and insurers while avoiding the operational cost and risk of realistic exercises.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, operational_management, beneficiary,
    organized, biographical, constrained, local).

% Complete prescribed simulation hours to keep certification. They experience the gap between scripted scenarios and the ambiguity, time pressure, and physical consequence of real events. Their professional identity is fused to the certificate; leaving means losing license and community. They pay in unexercised edge-case competence.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, frontline_operators, payer,
    moderate, biographical, identity_locked, local).

% Train on simulated fault trees that rarely match the compound, cascading failures of reality. Their certification depends on simulation sign-off. When real degradation occurs, the mental models from simulation prove incomplete. They bear the risk when the proxy fails.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, maintenance_crews, payer,
    moderate, biographical, identity_locked, local).

% Drill against tabletop and VR scenarios that sanitize chaos, communication breakdown, and resource exhaustion. Their readiness is measured by simulation completion rates. In actual catastrophe, the coordination patterns rehearsed in the proxy degrade. They pay with untested integration.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, emergency_response_teams, payer,
    moderate, biographical, constrained, regional).

% Relies on the competence of operators, maintainers, and responders. The proxy regime creates a certification illusion: records show compliance, but the competence that would prevent or mitigate catastrophe has not been exercised at fidelity. They bear the consequence when the proxy fails — they cannot exit the system.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, public_safety, payer,
    powerless, generational, trapped, national).

% Analyze incident reports and near-miss data. They see the divergence between simulation-passing units and real-world performance. Their assessments challenge the proxy but lack enforcement power. They occupy the analytical seat seeing the full structure.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, independent_safety_auditors, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, auditable, low-cost mechanism for certifying that safety-critical personnel maintain baseline competence without requiring disruptive, expensive, and risky live exercises. Solves the coordination problem of 'how do we know everyone is trained?' across distributed organizations and regulators.
% TRANSFER_FUNCTION: Moves the burden of competence validation from high-fidelity live exercise (costly, risky, organizationally disruptive) to simulation completion metrics (cheap, auditable, schedulable). Transfers risk from operators/management/regulators (who avoid live-drill cost and liability) to frontline personnel and the public (who bear the competence gap when proxy fails). Transfers revenue to simulation vendors and certification authorities.
% ABSENT_VOICES: Families of potential victims, communities downwind of high-hazard sites, and future operators who will inherit degraded competence norms. They would demand live validation but are structurally excluded from the certification rulemaking process.
% DISAPPEARANCE_RATIONALE: If simulation-equivalence vanished overnight, regulators would have to mandate live drills or accept decertification. Operational management would face shutdown costs and scheduling chaos. Simulation vendors would lose their regulatory moat. Frontline operators would face higher training burden but gain genuine competence exercise. The certification regime would reorganize around live exercise or collapse.
% FOUNDING_PROBLEM: After a series of incidents in the 1980s-90s attributed to operator error, regulators needed a scalable way to ensure ongoing competence without halting critical infrastructure. Live drills were too costly and risky for frequent repetition. Simulation offered a measurable, repeatable proxy.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory histories and vendor literature attest the founding problem was scalability and cost. Independent incident analyses (e.g., Chemical Safety Board reports, nuclear near-miss databases) and frontline operator testimony corroborate that the proxy has drifted from its founding justification — the competence gap it was meant to bridge has widened as simulation fidelity metrics replaced outcome validation.
narrative_ontology:disappearance_verdict(competence_exercise_validity__simulation_as_proxy, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__simulation_as_proxy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__simulation_as_proxy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(competence_exercise_validity__simulation_as_proxy, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__simulation_as_proxy, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) is substantial: the constraint extracts unexercised competence from operators and transfers risk to the public, while beneficiaries collect regulatory convenience and revenue. Suppression (0.55) is moderate: the constraint persists by making simulation the only compliant path — alternative validation (live drills, peer assessment, operational history) is suppressed by rule structure. Theater ratio (0.62) is high: a majority of enforcement activity now polices simulation metrics (hours, scores, scenario coverage) rather than competence outcomes. Accessibility collapse (0.48) is moderate: alternatives exist (live drills, mentored operations) but are structurally discouraged by cost and compliance rules. Resistance (0.42) is moderate: frontline operators and independent auditors resist but lack power to change the rule.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seats, the constraint is a rope: it solves a real coordination problem (scalable certification) with minimal coercion. From the payer seats, it is a snare: the coordination story is cover for extracting competence validation without paying for live exercise. The engine will compute this divergence. The claimed tangled_rope reflects the structural reality that both coordination and extraction are real and fused.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory bodies and certification authorities are structural beneficiaries (d near 0.0): they collect compliance ease and fee revenue. Training vendors are beneficiaries (d ~0.15): they capture the mandated market. Operational management are beneficiaries (d ~0.2): they avoid disruption. Frontline operators, maintenance crews, and emergency responders are payers (d ~0.8-0.9): identity-locked into certification, they bear the competence gap. Public safety is a trapped payer (d ~1.0): no exit, bears the consequence. Independent auditors are analytical observers (d=0.5). The engine computes per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (scalable competence validation) was live but has been substantially solved by simulation infrastructure. The mandate persists because the proxy regime now serves beneficiary interests (vendor revenue, regulatory convenience, management ease) more than the founding problem. The founding problem is contested: regulators and vendors claim it's live; incident data and frontline experience say it's dead for edge cases. Mandatrophy is unresolved — the arrangement has outlived its founding justification but is maintained by the coalition it created.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_threshold,
    'At what fidelity level does simulation cease to be a proxy and become a valid substitute for live exercise of competence?',
    'Controlled studies comparing operators trained exclusively on simulation vs. those with live drill experience, measured against real incident performance. Requires ethical approval for high-hazard domains.',
    'If a fidelity threshold exists and is measurable, the constraint could be a rope (coordination with a clear boundary). If no such threshold exists or is unverifiable, the constraint is structurally extractive — the proxy claim is unfalsifiable cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_threshold, empirical, 'Whether a fidelity threshold for valid substitution exists and is knowable').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly does the structural disagreement between simulation_as_proxy and real_catastrophe_only readings locate — in the definition of ''exercise'', the definition of ''competence'', or the evidentiary standard for validation?',
    'Formal analysis of each reading''s axioms: simulation_as_proxy axiom ''simulation_fidelity_metrics_validate_competence'' vs. real_catastrophe_only axiom ''consequence_exposure_required_for_competence''. The disagreement is in the evidentiary standard for what counts as validation.',
    'If the disagreement is in evidentiary standard, the readings coexist_with (different parties, different standards). If in definition of competence, they may foreclose (one competence concept rules out the other). This determines network.affects_constraints and cs_structure.reading_relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Structural locus of disagreement between kernel readings').

omega_variable(
    internalized_suppression_in_operators,
    'Do frontline operators internalize the simulation proxy as ''real training,'' suppressing their own awareness of the competence gap?',
    'Longitudinal interviews tracking operator confidence vs. assessed competence gap over career. Compare operators in simulation-only regimes vs. mixed regimes.',
    'If suppression is internalized, the constraint''s effective suppression is higher than structural measures suggest — operators carry the suppression with them, resisting live drill advocacy from within. This would increase the constraint''s snare-like character from the payer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_in_operators, empirical, 'Whether suppression operates through internalized belief in the proxy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__simulation_as_proxy, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cev_sim_proxy_tr_t0, competence_exercise_validity__simulation_as_proxy, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cev_sim_proxy_tr_t5, competence_exercise_validity__simulation_as_proxy, theater_ratio, 5, 0.32).
narrative_ontology:measurement(cev_sim_proxy_tr_t10, competence_exercise_validity__simulation_as_proxy, theater_ratio, 10, 0.41).
narrative_ontology:measurement(cev_sim_proxy_tr_t15, competence_exercise_validity__simulation_as_proxy, theater_ratio, 15, 0.49).
narrative_ontology:measurement(cev_sim_proxy_tr_t20, competence_exercise_validity__simulation_as_proxy, theater_ratio, 20, 0.56).
narrative_ontology:measurement(cev_sim_proxy_tr_t25, competence_exercise_validity__simulation_as_proxy, theater_ratio, 25, 0.62).

% Extraction over time
narrative_ontology:measurement(cev_sim_proxy_be_t0, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cev_sim_proxy_be_t5, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(cev_sim_proxy_be_t10, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(cev_sim_proxy_be_t15, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(cev_sim_proxy_be_t20, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(cev_sim_proxy_be_t25, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cev_sim_proxy_su_t0, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cev_sim_proxy_su_t5, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(cev_sim_proxy_su_t10, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(cev_sim_proxy_su_t15, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(cev_sim_proxy_su_t20, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(cev_sim_proxy_su_t25, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 25, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__simulation_as_proxy, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_validity__simulation_as_proxy, 0.12).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity__continuous_refresh_hybrid).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity__real_catastrophe_only).

% DUAL FORMULATION NOTE:
% This constraint is one member of the competence_exercise_validity constraint family. The kernel 'competence must be validated by exercise' admits three readings with different ε values: simulation_as_proxy (ε=0.68, this story), continuous_refresh_hybrid (ε≈0.35, coordination-heavy), real_catastrophe_only (ε≈0.15, mountain-like). The simulation_as_proxy reading is downstream of the kernel's original formulation and upstream of the continuous_refresh_hybrid which attempts to correct its drift.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_validity__simulation_as_proxy, institutional, 0.05).
constraint_indexing:directionality_override(competence_exercise_validity__simulation_as_proxy, organized, 0.15).
constraint_indexing:directionality_override(competence_exercise_validity__simulation_as_proxy, moderate, 0.85).
constraint_indexing:directionality_override(competence_exercise_validity__simulation_as_proxy, powerless, 0.98).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
