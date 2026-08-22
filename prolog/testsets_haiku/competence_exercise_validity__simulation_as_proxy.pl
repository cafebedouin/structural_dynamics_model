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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Simulation as Valid Competence Exercise and Regulatory Proxy
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   Safety-critical organizations (aviation, nuclear, maritime, emergency
 *   response) validate crew competence through simulator drills that are
 *   declared equivalent to real-world crisis exercise. This constraint
 *   asserts that simulation is a valid proxy: passing simulator scenarios,
 *   maintaining certification hours, and demonstrating drill-scenario
 *   competence are sufficient to certify operational readiness. Regulatory
 *   authorities, organization administrators, and simulator vendors benefit
 *   from this proxy because it avoids the operational disruption and
 *   logistical expense of real-world crisis drills while providing measurable
 *   compliance data. Operational crews bear the time cost and the
 *   identity-lock risk (career advancement depends on simulator
 *   certification). The constraint persists because it is genuinely useful
 *   (no other scalable competence proxy exists) AND extractive (it transfers
 *   training burden from regulators/administrators to crews while validating
 *   vendors and reducing operational costs). This story instantiates the
 *   'simulation_as_proxy' reading of the contested kernel
 *   'competence_exercise_validity': here, simulation counts as sufficient
 *   exercise and drills are treated as proxy-catastrophe. Sibling readings
 *   differ structurally on whether simulation alone suffices or whether real
 *   catastrophe is the only authentic competence test.
 *
 * KEY AGENTS:
 *   - regulatory_authorities: institutional power, analytical time horizon, analytical exit — they set the proxy standards and audit compliance but face no direct cost from simulation divergence
 *   - organization_administrators: powerful, biographical horizon, arbitrage exit — they benefit from lower training costs and standardized metrics, coordinate with regulators
 *   - operational_crews: moderate power, biographical horizon, identity-locked exit — required to maintain certification, career depends on proxy metrics, bear opportunity cost of simulator time
 *   - catastrophe_survivors: powerless, biographical horizon, trapped exit — excluded from policy-setting, their real-crisis experience contradicts the proxy but is compartmentalized retrospectively
 *   - simulation_vendors: organized power, biographical horizon, mobile exit — revenue scales with mandatory simulator hours
 *   - safety_researchers: moderate power, generational horizon, analytical exit — generate data on simulator fidelity gaps but influence the framework slowly
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__simulation_as_proxy, 0.68).
domain_priors:suppression_score(competence_exercise_validity__simulation_as_proxy, 0.72).
domain_priors:theater_ratio(competence_exercise_validity__simulation_as_proxy, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__simulation_as_proxy, rope).
narrative_ontology:human_readable(competence_exercise_validity__simulation_as_proxy, "Simulation as Valid Competence Exercise and Regulatory Proxy").
narrative_ontology:topic_domain(competence_exercise_validity__simulation_as_proxy, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__simulation_as_proxy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__simulation_as_proxy, '75d91da8-0e90-47f6-8c52-97ee677e2b7e').
narrative_ontology:cs_kernel_codification('75d91da8-0e90-47f6-8c52-97ee677e2b7e', formalized).
narrative_ontology:cs_authority_grounding('75d91da8-0e90-47f6-8c52-97ee677e2b7e', extraction).
narrative_ontology:cs_interpretation_layer_present('75d91da8-0e90-47f6-8c52-97ee677e2b7e').
narrative_ontology:cs_reading_relation('75d91da8-0e90-47f6-8c52-97ee677e2b7e', competence_exercise_validity__real_catastrophe_only, coexists_with).
narrative_ontology:cs_reading_relation('75d91da8-0e90-47f6-8c52-97ee677e2b7e', competence_exercise_validity__continuous_refresh_hybrid, influences).
narrative_ontology:cs_axiom('75d91da8-0e90-47f6-8c52-97ee677e2b7e', foundational, simulation_fidelity_sufficient_for_competence).
narrative_ontology:cs_axiom_status(simulation_fidelity_sufficient_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('75d91da8-0e90-47f6-8c52-97ee677e2b7e', simulation_fidelity_sufficient_for_competence, empirically_contingent).
narrative_ontology:cs_axiom('75d91da8-0e90-47f6-8c52-97ee677e2b7e', foundational, proxy_metrics_discharge_regulatory_commitment).
narrative_ontology:cs_axiom_status(proxy_metrics_discharge_regulatory_commitment, holdable).
narrative_ontology:cs_axiom_grounding('75d91da8-0e90-47f6-8c52-97ee677e2b7e', proxy_metrics_discharge_regulatory_commitment, conventional).
narrative_ontology:cs_reference_frame('75d91da8-0e90-47f6-8c52-97ee677e2b7e', simulation_as_competence_validation).
narrative_ontology:cs_drift_state('75d91da8-0e90-47f6-8c52-97ee677e2b7e', contemporary_post_incident_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('75d91da8-0e90-47f6-8c52-97ee677e2b7e', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, regulatory_authorities).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, organization_administrators).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, operational_crews).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, simulation_vendors).
narrative_ontology:constraint_vindicates(competence_exercise_validity__simulation_as_proxy, simulation_fidelity_sufficient_for_competence_retention).
narrative_ontology:constraint_vindicates(competence_exercise_validity__simulation_as_proxy, proxy_metrics_correlate_with_safety_outcomes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establish and enforce competence-retention standards through simulators and drills. They codify the acceptable proxy for real-world competence: crew must pass simulation scenarios quarterly, achieve defined drill metrics, and log simulator hours. They depend on this framework because real catastrophes are rare and regulatory capacity to evaluate on-the-job competence is limited. Auditing simulator participation is feasible; auditing true crisis readiness is not.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Operate within the regulatory proxy framework and use it to reduce training costs while appearing compliant. Simulator-based competence avoids expensive real-world exercises that disrupt normal operations, pull crews from revenue-generating roles, and create real infrastructure stress. They argue simulation satisfies competence and frees resources for other priorities. They coordinate with regulators on what constitutes adequate simulator realism and pass rates.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, organization_administrators, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__simulation_as_proxy, organization_administrators, agenda_setter).

% Required to maintain simulator certifications and pass drill scenarios on the authority's schedule. They experience simulation as mandatory but often uncoupled from the decision-making pressures and real-time uncertainty of actual crises. They bear the opportunity cost of time in simulator training that might otherwise be spent on skill-building or rest. Career advancement depends on certification completion, creating identity-lock around the proxy metrics — their professional identity and credibility are constituted through maintaining simulator standing.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, operational_crews, payer,
    moderate, biographical, identity_locked, global).

% Crews who faced real catastrophe are not in the policy-making conversation about competence standards. Their experience that simulation differed substantially from reality, or that crew panic and fatigue factors that don't appear in drills shaped the outcome, is often retrospective and compartmentalized rather than generative input to the framework.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, catastrophe_survivors, excluded,
    powerless, biographical, trapped, local).

% Provide simulator hardware, scenario software, and certification tracking. Revenue scales with mandatory simulator hours; they benefit from the constraint that competence MUST be validated through simulation. They invest in fidelity improvements selectively — enough to maintain regulatory approval, not necessarily enough to capture the gap between simulation and real crisis.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, simulation_vendors, beneficiary,
    organized, biographical, mobile, global).

% Study competence retention, human factors under stress, and simulator-to-reality transfer. They generate empirical data on where simulation diverges from actual crisis response and whether simulator performance predicts real-world outcomes. Their findings feed into regulatory refinement but often take years to influence the proxy framework.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, safety_researchers, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__simulation_as_proxy, regulatory_authorities).
narrative_ontology:fixing_cost_class(competence_exercise_validity__simulation_as_proxy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem that real catastrophes are rare and cannot be used as training data: simulation provides a repeatable, measurable proxy for crisis competence that regulators can audit and organizations can standardize across fleets/facilities.
% TRANSFER_FUNCTION: Moves crew time and attention from other activities to mandatory simulator training; moves revenue from organizations to simulator vendors; moves regulatory authority from post-incident investigation to pre-incident proxy certification.
% ABSENT_VOICES: Crews from past real crises whose experience contradicts the proxy equivalence (low simulator fidelity, fatigue factors unmodeled, panic patterns not captured) are often not systematized into policy debate; their testimony appears in incident reports rather than in the ongoing framework-setting conversation. Maintenance crews and lower-seniority staff who carry disproportionate simulator burden for lower advancement payoff are diffusely underrepresented.
% DISAPPEARANCE_RATIONALE: If simulation-as-proxy vanished, regulatory authorities would revert to historical/post-incident competence evaluation, organizations would optimize training toward real-world drills or efficiency metrics rather than simulator hours, and the incentive structure for simulator vendors would collapse — the ecosystem of competence validation would reorganize around measurable operational outcomes rather than proxy metrics.
% FOUNDING_PROBLEM: Catastrophes are too rare to use as training events; regulatory capacity to assess competence on the job was limited; organizations had no standardized way to prove crews were ready for crisis.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory authorities and simulator vendors affirm the problem is live and their solution is adequate. Safety researchers publish findings of significant fidelity gaps and poor transfer from simulator to real crisis; incident investigators document failures attributable to crew factors that simulations did not model (fatigue accumulation, resource scarcity, chain-of-command breakdown under uncertainty). The problem persists but the proxy validity is contested by stakeholders outside the beneficiary set.
narrative_ontology:disappearance_verdict(competence_exercise_validity__simulation_as_proxy, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__simulation_as_proxy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__simulation_as_proxy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_validity__simulation_as_proxy, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__simulation_as_proxy, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.54 at t0 to 0.68 at t15, then plateaus (data projects stabilization). The rising trajectory reflects accumulating regulatory additions (higher fidelity requirements, longer certification cycles) that increase the simulator training burden without proportional refinement of the proxy validity — more enforcement to maintain the same bet. Theater_ratio rises from 0.42 to 0.58 over the same interval: regulatory emphasis increasingly shifts from validating the proxy's empirical link to safety (real discovery) to administering the proxy itself (compliance theater). By t15, more than half the simulator activity is performative maintenance of certification status rather than genuine competence building. Suppression_requirement stays higher (0.62–0.72) because the constraint requires active enforcement of mandatory participation and regulatory gatekeeping of simulator vendors. Accessibility_collapse at 0.62 reflects that alternatives (on-the-job competence assessment, real-world drills, hybrid validation) exist but are not permitted within the regulatory framework — the collapse is regulatory, not natural. Resistance at 0.45 is moderate: crews grumble but comply (career dependence); vendors have no resistance (they benefit); regulators face pushback only from safety researchers and incident investigators (low institutional power in the framework, deferred influence). The measurement grid is shared across all metrics at every time point; no metric is missing from any time point (first audited/2026-06-12).
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory-authority and administrator seats, this is genuine coordination: a scalable solution to an unsolved competence-validation problem. From the operational-crew seat, it is enforced proxy-reliance: certification requirements that diverge from the competence the crews know they need, with career consequences for non-compliance. The engine computes both frames from the structural data (beneficiary vs. victim roles, exit constraints, power asymmetry) — the authored claim (rope: coordination-dominant) reflects the reading's own framing; the metrics (high extraction, rising theater, sustained suppression) report the constraint's actual operation as observed by the excluded parties and safety researchers.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory authorities are the structural agenda-setters (set the proxy standard, audit compliance) but sit as beneficiaries in extraction terms: they depend on the proxy to regulate at scale, not to verify real competence. Administrators are dual-positioned (beneficiary role, secondary as agenda-setter): they benefit from lower training costs and measurable compliance; they co-set the proxy with regulators through coordination. Operational crews are the targets (pay the time cost, identity-locked by career requirements, constrained exit). Simulation_vendors are beneficiaries (revenue scales with mandated hours). The directionality derivation places crews near d=0.8–0.9 (high target: payers with trapped/identity-locked exit); administrators and authorities near d=0.1–0.3 (beneficiaries with powerful exit, though authorities have analytical exit); vendors near d=0.2–0.4 (beneficiaries with organized power and mobile exit). No override needed; the structural data produces the correct d map.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is NOT mandatrophy. The founding problem is live (competence validation for rare-catastrophe organizations remains unsolved by pure real-world methods). The disappearance verdict is world_rearranges (regulatory frameworks would shift to other validation mechanisms). The classification as rope reflects the reading's own claim: simulation is presented as coordination that serves all parties (crews get clear standards, organizations get scalable validation, regulators get auditability). The metrics (high extraction, rising theater) reveal an extractive reality underneath the coordination claim, but this divergence does NOT make it piton: the constraint has an active agenda-setter (regulators) who maintains it intentionally for structural reasons (scalability), not out of institutional inertia. If the founding problem dies (crisis prediction becomes reliable, or in-situ competence assessment becomes feasible) and the constraint persists, it would become mandatrophy then. Currently it is a contested coordination-with-embedded-extraction, the tangled_rope pattern, though this reading's own framing claims rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulator_fidelity_vs_transfer,
    'Does high-fidelity simulator performance predict real-crisis competence, or do simulation and real crisis test fundamentally different cognitive/emotional states that transfer poorly?',
    'Post-incident analysis of crews who passed simulator certification but failed in real crisis; controlled study of simulator-to-crisis transfer using crews with both experiences; neurophysiological measurement of stress response in simulator vs. real incident.',
    'If transfer is poor, the proxy validity is substantially compromised and the constraint becomes a false certification scheme. If transfer is strong, the simulation-as-proxy reading gains empirical support and mandated simulator hours are justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulator_fidelity_vs_transfer, empirical, 'Whether simulator performance predicts real-crisis crew competence.').

omega_variable(
    foundational_proxy_vs_empirical_link,
    'Is the equivalence between simulation and competence a foundational normative commitment (simulation DEFINES what competence means in this regulatory framework) or an empirical hypothesis (simulation happens to predict real crisis outcomes)?',
    'Historical analysis of why regulatory authorities adopted simulation proxy: was it because evidence proved fidelity, or because no other scalable mechanism existed? If the latter, the commitment is foundational-pragmatic, not empirical, and empirical refutation would not necessarily change policy.',
    'If foundational-normative, regulatory change would require renegotiating what ''competence'' means in the framework, a higher-friction process. If empirical-contingent, strong refutation data could reroute competence validation to real-world methods.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foundational_proxy_vs_empirical_link, conceptual, 'Whether simulator-proxy equivalence is definitional or evidential in the regulatory commitment.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of alternative competence validation methods (real-world drills, job-embedded assessment) structurally enforced through regulatory gatekeeping, or do crews and organizations internalize the belief that simulation is sufficient?',
    'If regulatory ban on non-approved drill methods were lifted, would organizations and crews voluntarily adopt simulator-only, or would they substitute real-world drills and reduce simulator hours? Do crews report genuine belief in simulator equivalence or grudging compliance?',
    'If structural: removing the regulatory gate would dissolve the constraint quickly. If internalized: the constraint would persist even after regulatory change because crews and administrators have adopted the proxy as legitimate competence definition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is regulatory enforcement or internalized acceptance of the proxy.').

omega_variable(
    rare_catastrophe_asymmetry,
    'Does the rarity of real catastrophes make simulation the only scalable competence proxy, or does rarity mean real crisis should be treated as a separate phenomena from training (requiring different validation approaches)?',
    'Regulatory review of whether catastrophe frequency is changing with infrastructure improvements (suggesting historical rarity may not persist); analysis of whether competence-for-rare-crisis requires different training than competence-for-frequent-operations.',
    'If rare catastrophe means training MUST scale to proxy methods, the constraint is structurally necessary. If rarity is artifact and competence is unitary, then hybrid real-world/simulation methods become feasible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rare_catastrophe_asymmetry, conceptual, 'Whether catastrophe rarity necessitates or merely justifies simulation-as-proxy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__simulation_as_proxy, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__simulation_as_proxy, theater_ratio, 0, 0.42).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t3, competence_exercise_validity__simulation_as_proxy, theater_ratio, 3, 0.45).
narrative_ontology:measurement_basis(comp_tr_t3, observed).
narrative_ontology:measurement(comp_tr_t6, competence_exercise_validity__simulation_as_proxy, theater_ratio, 6, 0.49).
narrative_ontology:measurement_basis(comp_tr_t6, observed).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_validity__simulation_as_proxy, theater_ratio, 10, 0.54).
narrative_ontology:measurement_basis(comp_tr_t10, observed).
narrative_ontology:measurement(comp_tr_t15, competence_exercise_validity__simulation_as_proxy, theater_ratio, 15, 0.58).
narrative_ontology:measurement_basis(comp_tr_t15, projected).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_validity__simulation_as_proxy, theater_ratio, 20, 0.58).
narrative_ontology:measurement_basis(comp_tr_t20, projected).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 0, 0.54).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t3, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 3, 0.58).
narrative_ontology:measurement_basis(comp_be_t3, observed).
narrative_ontology:measurement(comp_be_t6, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 6, 0.62).
narrative_ontology:measurement_basis(comp_be_t6, observed).
narrative_ontology:measurement(comp_be_t10, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 10, 0.66).
narrative_ontology:measurement_basis(comp_be_t10, observed).
narrative_ontology:measurement(comp_be_t15, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 15, 0.68).
narrative_ontology:measurement_basis(comp_be_t15, projected).
narrative_ontology:measurement(comp_be_t20, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(comp_be_t20, projected).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(comp_su_t0, observed).
narrative_ontology:measurement(comp_su_t3, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 3, 0.65).
narrative_ontology:measurement_basis(comp_su_t3, observed).
narrative_ontology:measurement(comp_su_t6, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 6, 0.68).
narrative_ontology:measurement_basis(comp_su_t6, observed).
narrative_ontology:measurement(comp_su_t10, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 10, 0.7).
narrative_ontology:measurement_basis(comp_su_t10, observed).
narrative_ontology:measurement(comp_su_t15, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 15, 0.72).
narrative_ontology:measurement_basis(comp_su_t15, projected).
narrative_ontology:measurement(comp_su_t20, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(comp_su_t20, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__simulation_as_proxy, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_validity__simulation_as_proxy, 0.12).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity__real_catastrophe_only).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity__continuous_refresh_hybrid).

% DUAL FORMULATION NOTE:
% This constraint is one reading (simulation_as_proxy) of the contested kernel competence_exercise_validity. Two sibling readings exist in separate story files: real_catastrophe_only (only real crisis validates competence; simulation is insufficient) and continuous_refresh_hybrid (simulation necessary but not sufficient; competence requires continuous drill cycles). The three readings share the same referent (competence validation in safety-critical operations) but disagree on what constitutes proof. Each reading has its own epsilon, beneficiary/victim structure, and classification. They are linked via network.affects_constraints to enable contamination analysis — if one reading's claimed validity is undermined, all three face downstream pressure on their legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_validity__simulation_as_proxy, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
