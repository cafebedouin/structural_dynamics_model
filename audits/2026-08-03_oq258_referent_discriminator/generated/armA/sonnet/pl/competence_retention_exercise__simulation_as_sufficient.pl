% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__simulation_as_sufficient
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__simulation_as_sufficient, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: competence_retention_exercise__simulation_as_sufficient
 *   human_readable: Simulation-as-Sufficient: High-Fidelity Training as Genuine Catastrophe-Avoidance Competence
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   This constraint instantiates the 'simulation is sufficient' reading of
 *   the competence-retention-exercise kernel: the claim that high-fidelity
 *   simulator performance is structurally equivalent to demonstrated
 *   real-event competence in catastrophe-avoidance roles (nuclear operations,
 *   aviation, maritime, grid control). Under this reading, training
 *   infrastructure becomes the primary and often sole competence-maintenance
 *   mechanism, real catastrophes are actively prevented rather than sought or
 *   tolerated as feedback, and competence is operationally defined as
 *   simulator performance meeting a threshold. This is a genuine coordination
 *   solution to a real problem (you cannot ethically or practically validate
 *   competence by causing real catastrophes), but it is authored here as
 *   tangled_rope because an institutional stack (training directorates,
 *   simulator vendors, and certification bodies) collects durable rents —
 *   budget, market share, defensible compliance posture — from an equivalence
 *   claim whose residual failure risk is borne by frontline operators and the
 *   public, not by the institutions asserting the claim.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__simulation_as_sufficient, 0.42).
domain_priors:suppression_score(competence_retention_exercise__simulation_as_sufficient, 0.38).
domain_priors:theater_ratio(competence_retention_exercise__simulation_as_sufficient, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, extractiveness, 0.42).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__simulation_as_sufficient, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__simulation_as_sufficient, "Simulation-as-Sufficient: High-Fidelity Training as Genuine Catastrophe-Avoidance Competence").
narrative_ontology:topic_domain(competence_retention_exercise__simulation_as_sufficient, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_retention_exercise__simulation_as_sufficient).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__simulation_as_sufficient, '5c11f6fe-734b-428e-931a-9a7126a2f103').
narrative_ontology:cs_kernel_codification('5c11f6fe-734b-428e-931a-9a7126a2f103', formalized).
narrative_ontology:cs_authority_grounding('5c11f6fe-734b-428e-931a-9a7126a2f103', expertise).
narrative_ontology:cs_interpretation_layer_present('5c11f6fe-734b-428e-931a-9a7126a2f103').
narrative_ontology:cs_reading_relation('5c11f6fe-734b-428e-931a-9a7126a2f103', competence_retention_exercise__catastrophe_as_necessary, coexists_with).
narrative_ontology:cs_reading_relation('5c11f6fe-734b-428e-931a-9a7126a2f103', competence_retention_exercise__near_miss_as_bridge, influences).
narrative_ontology:cs_axiom('5c11f6fe-734b-428e-931a-9a7126a2f103', foundational, cognitive_procedural_equivalence_of_simulation).
narrative_ontology:cs_axiom_status(cognitive_procedural_equivalence_of_simulation, holdable).
narrative_ontology:cs_axiom_grounding('5c11f6fe-734b-428e-931a-9a7126a2f103', cognitive_procedural_equivalence_of_simulation, empirically_contingent).
narrative_ontology:cs_axiom('5c11f6fe-734b-428e-931a-9a7126a2f103', secondary, prevention_of_catastrophe_is_compatible_with_competence_verification).
narrative_ontology:cs_axiom_status(prevention_of_catastrophe_is_compatible_with_competence_verification, holdable).
narrative_ontology:cs_axiom_grounding('5c11f6fe-734b-428e-931a-9a7126a2f103', prevention_of_catastrophe_is_compatible_with_competence_verification, instrumental).
narrative_ontology:cs_reference_frame('5c11f6fe-734b-428e-931a-9a7126a2f103', simulator_certification_as_competence_standard).
narrative_ontology:cs_drift_state('5c11f6fe-734b-428e-931a-9a7126a2f103', post_high_fidelity_simulator_maturity_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5c11f6fe-734b-428e-931a-9a7126a2f103', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, simulator_vendors).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, training_directorate_leadership).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, regulatory_certification_bodies).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, operations_management).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, frontline_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, downstream_public_exposed_to_residual_risk).
narrative_ontology:constraint_vindicates(competence_retention_exercise__simulation_as_sufficient, high_fidelity_equivalence_doctrine).
narrative_ontology:constraint_vindicates(competence_retention_exercise__simulation_as_sufficient, simulator_metrics_as_valid_competence_proxy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the certification standard that simulator-passed performance constitutes demonstrated catastrophe-avoidance competence. Administers the training budget, designs the qualification curriculum, and defends the equivalence claim in front of regulators and boards. Its own institutional legitimacy and headcount are built on the premise that simulation is sufficient — abandoning the claim would require justifying continued exposure to real catastrophic risk for validation.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, training_directorate_leadership, agenda_setter,
    institutional, generational, arbitrage, national).

% Sell high-fidelity simulator platforms and certification packages whose commercial value depends entirely on the claim that simulator performance is structurally equivalent to real-event competence. Revenue scales with the number of organizations that adopt simulation as the sole or primary competence-maintenance mechanism; has no exposure to failures of the equivalence claim in the field.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, simulator_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Issues licenses and certifications contingent on simulator-hour requirements rather than field-event exposure, because simulator metrics are auditable, standardizable, and do not require waiting for or causing catastrophic events. Benefits from a tractable, defensible compliance framework; bears little direct cost if the equivalence claim is later found deficient, since liability diffuses to the certified organization.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, regulatory_certification_bodies, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__simulation_as_sufficient, regulatory_certification_bodies, agenda_setter).

% Relies on simulator-based certification to staff safety-critical roles without disrupting operations for live drills or accepting the operational and legal costs of exposure-based training. Gains schedule flexibility, lower training cost, and a defensible paper trail of 'demonstrated competence' if an incident occurs.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, operations_management, beneficiary,
    powerful, biographical, constrained, national).

% Certified as competent based on simulator performance, then bear the actual cognitive and physiological gap between rehearsed scenarios and a live catastrophic event — the surprise, ambiguity, and multi-system cascading failure that simulators cannot fully replicate. If a real event exposes a competence gap the simulator did not test for, the operator personally bears the blame, the trauma, or the fatality risk, while the training design that produced the certification is rarely re-examined with the same scrutiny. Cannot decline simulator-based certification and still hold the job.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, frontline_operators, payer,
    moderate, biographical, constrained, local).

% Lives, works, or travels within the zone of consequence of the safety-critical system (plant, aircraft, grid, vessel) whose operators were certified via simulation. Has no visibility into whether the equivalence claim holds and no channel to demand exposure-validated competence; bears the tail risk if the claim is false in ways simulators structurally cannot surface.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, downstream_public_exposed_to_residual_risk, payer,
    powerless, generational, trapped, regional).

% Would investigate whether simulator performance actually predicts real-event outcomes, but access to post-incident data, near-miss reports, and simulator validation studies is controlled by the same institutions whose legitimacy depends on the equivalence claim. Rarely funded to conduct the specific comparative studies that would test the claim directly.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, independent_safety_researchers, excluded,
    moderate, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a scalable, repeatable, non-destructive way to build and verify procedural competence in catastrophe-avoidance roles without requiring organizations to wait for or manufacture real catastrophic events, which genuinely solves a coordination problem: competence must be built and demonstrated before deployment, not learned live during a first real crisis.
% TRANSFER_FUNCTION: Moves the burden of proof for competence from lived, high-stakes field exposure onto a controlled artifact (the simulator) whose design, fidelity claims, and pass thresholds are set by the training directorate and vendor, while moving the residual risk of any gap between simulated and real competence onto frontline operators and the public they serve.
% ABSENT_VOICES: Independent safety researchers who could test whether simulator performance predicts real-event outcomes are structurally excluded from the data and funding needed to do so; frontline operators who experience the gap between simulated and real events firsthand have no formal channel to contest the equivalence claim without appearing to indict their own certification.
% DISAPPEARANCE_RATIONALE: If the equivalence claim were withdrawn tomorrow, the training directorate, vendors, and regulators would need to justify an entirely different (likely more expensive, slower, and operationally disruptive) competence-verification regime — a genuine rearrangement for them. But whether frontline operators and the public would be materially safer or more at risk is contested: some argue exposure-based validation would close real gaps; others argue simulation genuinely is sufficient and withdrawal would just add cost without safety benefit. The verdict differs by seat, which is why it is authored as contested rather than resolved.
% FOUNDING_PROBLEM: Catastrophic-risk domains (nuclear, aviation, maritime, grid operations) needed a way to build and certify operator competence without incurring the cost, irreversibility, and moral hazard of using real catastrophic events as the training or validation mechanism.
% FOUNDING_PROBLEM_CORROBORATION: Training directorate leadership and simulator vendors attest the problem is fully solved by high-fidelity simulation and that the equivalence claim is settled science within their domain. Independent safety researchers and several post-incident investigation boards (cited in aviation and nuclear near-miss literature) attest the founding problem — verifying that trained competence transfers to real cascading-failure conditions — remains only partially addressed, since simulators are built and validated against known failure modes and cannot fully anticipate novel cascade patterns; this corroboration comes from outside the certifying and training institutions.
narrative_ontology:disappearance_verdict(competence_retention_exercise__simulation_as_sufficient, contested).
narrative_ontology:founding_problem_status(competence_retention_exercise__simulation_as_sufficient, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__simulation_as_sufficient, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_retention_exercise__simulation_as_sufficient, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__simulation_as_sufficient, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__simulation_as_sufficient_tests).
:- end_tests(competence_retention_exercise__simulation_as_sufficient_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate and rising (0.22 to 0.42) reflecting a real but partial coordination function: simulation training genuinely builds procedural competence, but the equivalence claim is stronger than the evidence for it, and the gap between simulated and real cascading-failure conditions is a structural blind spot the simulator cannot self-certify away. Theater ratio rises modestly (0.14 to 0.31) as certification metrics increasingly substitute for direct evidence of real-world competence transfer — a mild but real Goodhart drift where 'passed the simulator' becomes the target rather than the proxy. Suppression is authored moderate (0.38) and structural rather than coercive in the interpersonal sense: it operates through the near-total absence of an alternative validation channel (no organization can ethically run real catastrophes to test the claim) and through institutional control of the data needed to audit the equivalence claim.
 *
 * PERSPECTIVAL GAP:
 *   From the training directorate's seat, this looks like a mature rope: a well-validated, continuously improved coordination mechanism solving a genuine problem. From the frontline operator's seat, especially one who has lived through a real cascading failure the simulator curriculum did not anticipate, the same structure can look like an institutional shield — a certification regime that protects the certifying institutions' legitimacy more reliably than it protects the operator's actual preparedness. The engine's per-seat computation should reflect this asymmetry from the structural data (power, exit, beneficiary/victim role) rather than from either seat's self-report.
 *
 * DIRECTIONALITY LOGIC:
 *   Training directorate leadership, simulator vendors, and regulatory certification bodies sit near the beneficiary end: they set the equivalence standard, collect budget/revenue/legitimacy from it, and bear minimal direct cost if the claim proves partially false in the field. Operations management is a secondary beneficiary through cost and schedule savings. Frontline operators and the downstream public sit near the target end: they are trapped or constrained in their exposure to the residual risk that the equivalence claim, if imperfect, leaves unaddressed — operators cannot decline certification and still work the role, and the public has no exit from the zone of consequence at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (build and verify competence without using real catastrophes as the mechanism) remains genuinely live — this is not a pure zombie mandate. What keeps this from being classified as a clean rope is that the mandate has been extended past its evidentiary warrant: the equivalence claim ('simulator-passed = real-event competent') is treated as settled rather than as an ongoing empirical question, and the institutions most positioned to test that claim rigorously are also the ones whose funding and legitimacy depend on it not being falsified. Classifying this as tangled_rope rather than snare or rope prevents two mislabeling errors: calling it a snare would ignore the genuine coordination value simulation training provides; calling it a pure rope would ignore that the residual risk of an overclaimed equivalence is asymmetrically transferred to operators and the public who have no voice in setting or auditing the claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equivalence_claim_empirical_status,
    'Does simulator-passed performance actually predict real-event catastrophe-avoidance outcomes at a rate close to what the equivalence claim implies, or is there a measurable and consequential gap for novel/cascading failure modes the simulator was not designed to test?',
    'Longitudinal comparison of operators'' simulator certification records against their performance in real incidents and near-misses, ideally conducted or audited by parties outside the training directorate and simulator vendor (independent safety boards, academic human-factors researchers with subpoena-level data access).',
    'If the gap is small, this reading''s extraction is closer to a genuine rope with modest overclaim. If the gap is large and concentrated in novel cascading scenarios, the tangled_rope classification understates the risk transferred to frontline operators and the public, and the constraint drifts toward snare for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equivalence_claim_empirical_status, empirical, 'Whether simulator performance is empirically equivalent to real-event competence, or an overclaimed proxy.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the disagreement between this reading and its siblings (catastrophe_as_necessary, near_miss_as_bridge) actually live — is it a disagreement about what counts as sufficient EVIDENCE for competence, or a disagreement about what competence itself structurally IS?',
    'Conceptual analysis distinguishing an epistemic dispute (what validates a competence claim) from a metaphysical/functional dispute (what competence consists in); could be partially resolved by examining whether proponents of each reading would accept the others'' evidence if it existed, or whether they reject the evidentiary category itself.',
    'If the dispute is purely epistemic, the three readings could in principle converge on a shared validation standard (e.g., simulation validated periodically against near-miss data). If it is a genuine structural disagreement about the nature of competence, the readings are foreclosing rather than merely competing, and the reading_relations should be revisited.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Locating whether the kernel dispute is epistemic (evidence sufficiency) or structural (nature of competence).').

omega_variable(
    institutional_capture_of_validation_data,
    'Is the near-total institutional control over the data needed to test the equivalence claim (post-incident reports, near-miss logs, simulator validation studies) itself part of the extractive structure, or a defensible consequence of safety-sensitive information handling?',
    'Compare data access regimes across jurisdictions/industries with different disclosure requirements (e.g., aviation''s relatively open incident reporting vs. more closed industrial safety regimes) and observe whether more open regimes produce different equivalence-claim outcomes.',
    'If closed data regimes correlate with stronger, less-tested equivalence claims, this supports treating the exclusion of independent researchers as a structural suppression mechanism rather than incidental confidentiality, raising the suppression metric and reinforcing the tangled_rope reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_capture_of_validation_data, empirical, 'Whether data control is protective necessity or structural suppression of falsification capacity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__simulation_as_sufficient, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0, 0.14).
narrative_ontology:measurement(comp_tr_t8, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 8, 0.18).
narrative_ontology:measurement(comp_tr_t16, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 16, 0.22).
narrative_ontology:measurement(comp_tr_t24, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 24, 0.26).
narrative_ontology:measurement(comp_tr_t32, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 32, 0.29).
narrative_ontology:measurement(comp_tr_t40, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(comp_be_t8, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(comp_be_t16, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(comp_be_t24, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(comp_be_t32, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(comp_be_t40, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(comp_su_t8, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 8, 0.25).
narrative_ontology:measurement(comp_su_t16, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 16, 0.29).
narrative_ontology:measurement(comp_su_t24, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 24, 0.33).
narrative_ontology:measurement(comp_su_t32, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 32, 0.36).
narrative_ontology:measurement(comp_su_t40, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__simulation_as_sufficient, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__simulation_as_sufficient, 0.12).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, catastrophe_as_necessary).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, near_miss_as_bridge).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the 'competence retention exercise' kernel per the ε-invariance principle: this reading (simulation_as_sufficient, ε=0.42, tangled_rope) treats simulator infrastructure as the primary and sufficient competence-maintenance mechanism; catastrophe_as_necessary treats real catastrophic events as the only genuine validation mechanism; near_miss_as_bridge treats near-miss incidents as an intermediate, adequate feedback channel. Each carries its own ε, beneficiary/victim structure, and classification rather than averaging across readings — the kernel context and reading_relations record where and how they diverge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
