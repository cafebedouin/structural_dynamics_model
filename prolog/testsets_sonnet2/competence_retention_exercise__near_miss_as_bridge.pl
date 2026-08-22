% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__near_miss_as_bridge
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__near_miss_as_bridge, []).

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
 *   constraint_id: competence_retention_exercise__near_miss_as_bridge
 *   human_readable: Near-Miss Investigation as Bridge Between Simulation and Catastrophe for Competence Retention
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This story addresses whether near-miss incidents and minor failures give
 *   organizations enough real-world signal to keep simulator training valid,
 *   without needing full catastrophes. This is one reading of a contested
 *   kernel about how organizations maintain genuine competence at catastrophe
 *   avoidance over time. The sibling readings — that only catastrophes
 *   provide real learning, or that high-fidelity simulation alone is
 *   structurally sufficient — are separate constraints, not alternatives
 *   folded into this one. This reading holds a middle structural position: it
 *   asserts a hybrid necessity (simulators for routine retention, near-miss
 *   investigation for empirical correction) and treats both pure-simulation
 *   sufficiency and catastrophe-necessity as insufficient accounts on their
 *   own. As the arrangement matures institutionally, it develops a genuine
 *   but partial extraction profile: junior and reporting staff carry
 *   disproportionate disclosure risk to generate the feedback data that funds
 *   organization-wide and regulatory benefits.
 *
 * KEY AGENTS:
 *   - operating_organization: agenda_setter (institutional/arbitrage) — designs the reporting program and captures safety-record and legitimacy benefits
 *   - frontline_operators: beneficiary/payer (moderate/constrained) — receive better-calibrated training but must self-report
 *   - incident_reporting_staff: payer (powerless/trapped) — bear the direct scrutiny cost of disclosure
 *   - junior_operators_under_disclosure_pressure: payer (powerless/trapped) — disproportionately exposed to reportable error while least protected
 *   - regulatory_agencies: beneficiary/observer (institutional/analytical) — use near-miss data as evidence of self-correction without needing catastrophes
 *   - public_and_downstream_communities: excluded (powerless/trapped) — bear ultimate residual risk with no visibility into the feedback loop
 *   - safety_researchers: observer (analytical/analytical) — assess whether the hybrid model tracks real competence or is gamed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__near_miss_as_bridge, 0.38).
domain_priors:suppression_score(competence_retention_exercise__near_miss_as_bridge, 0.42).
domain_priors:theater_ratio(competence_retention_exercise__near_miss_as_bridge, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, extractiveness, 0.38).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__near_miss_as_bridge, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__near_miss_as_bridge, "Near-Miss Investigation as Bridge Between Simulation and Catastrophe for Competence Retention").
narrative_ontology:topic_domain(competence_retention_exercise__near_miss_as_bridge, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__near_miss_as_bridge).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__near_miss_as_bridge, 'a8447baa-cb38-4590-82c9-57c0adf6df08').
narrative_ontology:cs_kernel_codification('a8447baa-cb38-4590-82c9-57c0adf6df08', distributed).
narrative_ontology:cs_authority_grounding('a8447baa-cb38-4590-82c9-57c0adf6df08', practice).
narrative_ontology:cs_interpretation_layer_present('a8447baa-cb38-4590-82c9-57c0adf6df08').
narrative_ontology:cs_reading_relation('a8447baa-cb38-4590-82c9-57c0adf6df08', competence_retention_exercise__simulation_as_sufficient, coexists_with).
narrative_ontology:cs_reading_relation('a8447baa-cb38-4590-82c9-57c0adf6df08', competence_retention_exercise__catastrophe_as_necessary, coexists_with).
narrative_ontology:cs_axiom('a8447baa-cb38-4590-82c9-57c0adf6df08', foundational, empirical_correction_without_catastrophic_cost_is_sufficient).
narrative_ontology:cs_axiom_status(empirical_correction_without_catastrophic_cost_is_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('a8447baa-cb38-4590-82c9-57c0adf6df08', empirical_correction_without_catastrophic_cost_is_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('a8447baa-cb38-4590-82c9-57c0adf6df08', secondary, simulator_content_requires_external_grounding_to_avoid_drift).
narrative_ontology:cs_axiom_status(simulator_content_requires_external_grounding_to_avoid_drift, holdable).
narrative_ontology:cs_axiom_grounding('a8447baa-cb38-4590-82c9-57c0adf6df08', simulator_content_requires_external_grounding_to_avoid_drift, empirically_contingent).
narrative_ontology:cs_reference_frame('a8447baa-cb38-4590-82c9-57c0adf6df08', high_reliability_organization_feedback_model).
narrative_ontology:cs_drift_state('a8447baa-cb38-4590-82c9-57c0adf6df08', contemporary_safety_practice, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('a8447baa-cb38-4590-82c9-57c0adf6df08', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, operating_organization).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, frontline_operators).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, regulatory_agencies).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, incident_reporting_staff).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, junior_operators_under_disclosure_pressure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, frontline_operators).
narrative_ontology:constraint_vindicates(competence_retention_exercise__near_miss_as_bridge, high_reliability_organization_theory).
narrative_ontology:constraint_vindicates(competence_retention_exercise__near_miss_as_bridge, normal_accident_theory_partial_rebuttal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and funds the near-miss reporting and investigation program, decides which incidents feed back into simulator scenario libraries, and sets the disclosure culture (blameless vs. punitive). Captures the safety-record and regulatory-goodwill benefits of a functioning program while bearing the direct cost of running investigations.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, operating_organization, agenda_setter,
    institutional, generational, arbitrage, national).

% Rely on simulator training that is periodically refreshed with real near-miss scenarios to keep their skills current against actual failure modes rather than stale textbook cases. They also bear the burden of self-reporting their own errors and near-misses, which is professionally and psychologically costly even under blameless-reporting policy.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, frontline_operators, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__near_miss_as_bridge, frontline_operators, payer).

% Individuals who self-report or are implicated in a near-miss must submit to investigation, interviews, and documentation. Even under nominally blameless policy, they carry reputational risk, career anxiety, and the emotional cost of scrutiny; they cannot decline participation without violating reporting mandates that are themselves conditions of continued employment.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, incident_reporting_staff, payer,
    powerless, immediate, trapped, local).

% Newer staff face asymmetric exposure: they are more likely to make reportable errors while building tacit competence, and less likely to have the standing to contest how an investigation characterizes their conduct. They pay disproportionately in career risk to generate the feedback data that funds everyone else's improved simulator training.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, junior_operators_under_disclosure_pressure, payer,
    powerless, biographical, trapped, local).

% Use near-miss data as evidence that the industry is self-correcting without requiring catastrophic events to justify new rules. Benefit from a legible, auditable feedback loop that lets them certify training programs as adequate without waiting for accidents; also depend on the organization's honest disclosure, which they cannot fully verify.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, regulatory_agencies, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__near_miss_as_bridge, regulatory_agencies, observer).

% Bear the ultimate risk if the near-miss feedback loop fails to catch a degrading failure mode before it becomes catastrophic, but have no visibility into near-miss investigation content, no seat in scenario-library design decisions, and no standing to demand disclosure of what near-misses revealed.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, public_and_downstream_communities, excluded,
    powerless, generational, trapped, regional).

% Study near-miss reporting programs across industries to assess whether the hybrid model (simulator plus near-miss integration) actually tracks real competence, or whether organizations game reporting thresholds to produce a reassuring but hollow record.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, safety_researchers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuine near-misses and minor failures are systematically investigated, and their causal lessons are fed back into simulator scenario libraries, so that training stays anchored to the actual failure modes the system currently produces rather than drifting toward abstraction or obsolescence. This solves a real problem: pure simulation without empirical refresh degrades into rehearsing yesterday's threat model.
% TRANSFER_FUNCTION: Moves epistemic labor and career/reputational risk from the organization and senior staff onto the individuals who generate or are implicated in reportable events (disproportionately junior operators), in exchange for organization-wide competence gains and regulatory legitimacy that accrue to the organization and the agencies overseeing it.
% ABSENT_VOICES: The public and downstream communities who would bear the cost of an undetected failure mode have no visibility into what near-misses were investigated, what was found, or whether findings were actually integrated into training — they would object to opacity in the feedback loop if they had standing to see inside it.
% DISAPPEARANCE_RATIONALE: If near-miss investigation and integration vanished, simulator scenario libraries would gradually decouple from the system's actual current failure modes, training would drift toward legacy threat models, and the organization would lose its primary non-catastrophic evidence base for demonstrating competence to regulators — pressure would build either toward simulation-only complacency or toward waiting for real catastrophes to force updates.
% FOUNDING_PROBLEM: Pure high-fidelity simulation, however sophisticated, cannot by itself validate that training content still matches the system's actual current failure modes — simulators are built from someone's model of what can go wrong, and that model needs periodic correction against reality, but full catastrophes are too rare, too costly, and too irreversible to serve as the primary correction mechanism.
% FOUNDING_PROBLEM_CORROBORATION: Independent safety researchers studying aviation, nuclear, and chemical-process near-miss reporting systems (e.g. ASRS-style voluntary reporting analyses) corroborate that near-miss data measurably predicts and precedes failure-mode drift in ways pure simulation does not capture, from outside the operating organizations that run the programs. Regulatory agencies also independently attest the problem remains live, though their attestation is not fully independent of the legitimacy they derive from the existing arrangement.
narrative_ontology:disappearance_verdict(competence_retention_exercise__near_miss_as_bridge, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__near_miss_as_bridge, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__near_miss_as_bridge, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_retention_exercise__near_miss_as_bridge, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__near_miss_as_bridge, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__near_miss_as_bridge_tests).
:- end_tests(competence_retention_exercise__near_miss_as_bridge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38 at interval end) is moderate: the coordination function (empirically-anchored training refresh) is real and substantial, but it operates by transferring disclosure risk onto the individuals who generate near-miss data, disproportionately junior staff, which is a genuine asymmetric cost embedded in the same structure that produces the coordination benefit. Suppression (0.42) reflects that reporting is often mandatory, not fully voluntary, and that blameless-reporting policies are frequently only partially honored in practice, creating real pressure to disclose under conditions the discloser cannot fully control. Theater ratio (0.28) is present but not dominant: some near-miss programs do drift toward box-checking documentation that is never actually integrated into scenario libraries, but a meaningful share of the activity is genuine investigation and retraining. Accessibility collapse (0.40) is moderate — alternative competence-maintenance strategies (pure simulation, waiting for catastrophe) remain conceptually available and are actively argued for by the sibling readings, so this reading has not fully foreclosed the alternatives, it has out-competed them empirically in domains with mature safety cultures. Resistance (0.45) reflects genuine pushback from reporting staff and unions over disclosure burden and from researchers questioning whether integration is real.
 *
 * DIRECTIONALITY LOGIC:
 *   The operating organization and regulatory agencies sit near the beneficiary end: they capture legitimacy, safety-record, and reduced-catastrophe-probability benefits while bearing organizational (not personal) costs of running the program. Frontline operators are genuinely dual-positioned — they benefit from better training but personally bear disclosure costs when they are the source of a near-miss, hence the secondary payer role. Incident reporting staff and junior operators are structural targets: they generate the raw feedback data at direct personal cost (career risk, scrutiny, emotional burden) and have the least power to shape how their disclosures are used or protected. The public is excluded entirely from the loop despite bearing the tail-risk consequence of the loop's failure — this is the story's clearest absent-voice finding.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than a clean rope) prevents this reading from being mistaken for pure, cost-free coordination: the near-miss-as-bridge model genuinely solves a real problem (empirical grounding for simulator content) that neither pure simulation nor catastrophe-waiting solves as well, but it does so by imposing a real, asymmetric, and non-trivial cost on the people who supply the raw incidents. Calling it a rope would erase the disclosure burden borne by junior and reporting staff; calling it a snare would erase the genuine, well-evidenced competence-retention function it performs relative to its siblings. The founding problem (simulators drift without empirical correction) remains live and independently corroborated, which distinguishes this from a piton — the mandate has not outlived its function, though the mechanism for satisfying that mandate does impose real, currently under-compensated costs on specific low-power seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    near_miss_representativeness,
    'Do near-misses sample the same failure-mode distribution as actual catastrophes, or do they systematically under-represent rare, high-consequence, multi-factor failure paths that only manifest in full catastrophic events?',
    'Comparative analysis of near-miss corpora against post-hoc catastrophe investigations in the same industry: do catastrophe root causes appear as antecedent near-miss patterns, or are they qualitatively novel combinations that near-miss data could not have surfaced?',
    'If near-misses systematically under-sample rare compound failure modes, the near_miss_as_bridge reading is only locally valid (for common failure modes) and the catastrophe_as_necessary reading retains force for a residual category of risk the hybrid model cannot reach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(near_miss_representativeness, empirical, 'Whether near-miss data adequately samples the failure-mode space that catastrophes reveal.').

omega_variable(
    blameless_reporting_authenticity,
    'Is the blameless-reporting policy that shields incident_reporting_staff and junior_operators genuinely enforced, or is it a stated policy that career consequences quietly override in practice?',
    'Longitudinal tracking of career outcomes for staff who report near-misses versus comparable staff who do not, controlling for incident severity; exit interviews and anonymized surveys of reporting staff.',
    'If blameless reporting is largely nominal, the suppression metric (0.42) understates the true coercive character of the reporting mandate, and the constraint sits closer to tangled_rope-with-substantial-suppression or even snare for the reporting-staff seat specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(blameless_reporting_authenticity, empirical, 'Whether stated blameless-reporting protections actually hold in practice.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the near_miss_as_bridge reading''s claim of sufficiency break down relative to the catastrophe_as_necessary reading — is there a class of systems (extremely rare, extremely high-consequence, e.g. nuclear core-melt scenarios) where near-misses are structurally too infrequent to generate adequate feedback density, making the hybrid model degrade toward simulation_as_sufficient by default?',
    'Cross-domain comparison of near-miss reporting density and catastrophe base rates across industries (aviation vs. nuclear vs. chemical process vs. spaceflight); domains with catastrophe base rates low enough that near-misses are also rare would show the hybrid model collapsing toward pure simulation reliance.',
    'This determines whether near_miss_as_bridge is a general-purpose account or a domain-conditional one that only holds where near-miss incidence is high enough to generate a real data stream — in low-incidence domains the practical difference between this reading and simulation_as_sufficient may vanish.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Whether the reading''s applicability is bounded by near-miss base-rate density, collapsing toward a sibling reading in low-incidence domains.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__near_miss_as_bridge, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0, 0.12).
narrative_ontology:measurement(comp_tr_t4, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 4, 0.15).
narrative_ontology:measurement(comp_tr_t8, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 8, 0.18).
narrative_ontology:measurement(comp_tr_t12, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 12, 0.21).
narrative_ontology:measurement(comp_tr_t16, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 16, 0.24).
narrative_ontology:measurement(comp_tr_t20, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 20, 0.26).
narrative_ontology:measurement(comp_tr_t24, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(comp_be_t4, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 4, 0.26).
narrative_ontology:measurement(comp_be_t8, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 8, 0.3).
narrative_ontology:measurement(comp_be_t12, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 12, 0.32).
narrative_ontology:measurement(comp_be_t16, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 16, 0.35).
narrative_ontology:measurement(comp_be_t20, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(comp_be_t24, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 24, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(comp_su_t4, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 4, 0.29).
narrative_ontology:measurement(comp_su_t8, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(comp_su_t12, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 12, 0.36).
narrative_ontology:measurement(comp_su_t16, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 16, 0.39).
narrative_ontology:measurement(comp_su_t20, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(comp_su_t24, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__near_miss_as_bridge, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__near_miss_as_bridge, 0.12).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, simulation_as_sufficient).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, catastrophe_as_necessary).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the competence_retention_exercise kernel. near_miss_as_bridge claims a hybrid sufficiency (simulator + near-miss integration) that neither simulation_as_sufficient (pure simulation suffices) nor catastrophe_as_necessary (only real catastrophes teach) accepts on its own. Each reading is authored as a separate constraint with its own epsilon, beneficiary/victim structure, and classification; they are linked here rather than merged because their extraction profiles and stakeholder harms differ structurally, not just rhetorically.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
