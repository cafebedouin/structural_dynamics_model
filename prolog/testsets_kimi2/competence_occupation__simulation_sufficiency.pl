% ============================================================================
% CONSTRAINT STORY: competence_occupation__simulation_sufficiency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__simulation_sufficiency, []).

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
 *   constraint_id: competence_occupation__simulation_sufficiency
 *   human_readable: Simulation-Sufficient Competence Occupation Claim
 *   domain: organizational/safety
 *
 * SUMMARY:
 *   This constraint is one reading of the contested kernel
 *   competence_occupation: the claim that simulation-based drills are, by
 *   themselves, sufficient to occupy the competence kernel and prevent skill
 *   decay in high-reliability operators. Under this reading, training
 *   compliance becomes the observable proxy for competence, skill decay is
 *   treated as solvable through frequency and fidelity optimization, and the
 *   simulation industry becomes the primary beneficiary. The reading competes
 *   with real_incident_necessity (only actual catastrophic incidents provide
 *   authentic conditions) and hybrid_occupation (continuous multi-mechanism
 *   exercise without consensus on mix). The structural delta is institutional
 *   capture: a genuine coordination function (safe, repeatable practice) is
 *   coupled with asymmetric extraction (budget capture, compliance theater,
 *   suppression of alternative training models).
 *
 * KEY AGENTS:
 *   - simulation_industry: Primary beneficiary (powerful/mobile) â captures training budgets through vendor lock-in and standards influence.
 *   - training_compliance_bureaucracy: Agenda-setter (institutional/constrained) â administers the mandate, scales with it, and controls the competence metric.
 *   - frontline_operators: Primary target/payer (moderate/constrained) â bears time cost, potential skill-masking, and career-gatekeeping.
 *   - operational_management: Secondary beneficiary (powerful/constrained) â collects liability coverage and regulatory defensibility.
 *   - regulatory_authority: Agenda-setter (institutional/analytical) â codifies the sufficiency claim into licensure requirements.
 *   - safety_researchers: Analytical observer (institutional/analytical) â produces evidence that may or may not penetrate the standards process.
 *   - general_public: Excluded (powerless/trapped) â relies on the competence but has no voice in how it is maintained.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, 0.58).
domain_priors:suppression_score(competence_occupation__simulation_sufficiency, 0.62).
domain_priors:theater_ratio(competence_occupation__simulation_sufficiency, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__simulation_sufficiency, tangled_rope).
narrative_ontology:human_readable(competence_occupation__simulation_sufficiency, "Simulation-Sufficient Competence Occupation Claim").
narrative_ontology:topic_domain(competence_occupation__simulation_sufficiency, "organizational/safety").

domain_priors:requires_active_enforcement(competence_occupation__simulation_sufficiency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__simulation_sufficiency, '528756c8-8d9c-4ec9-8461-f2a3ca1d3d46').
narrative_ontology:cs_kernel_codification('528756c8-8d9c-4ec9-8461-f2a3ca1d3d46', formalized).
narrative_ontology:cs_authority_grounding('528756c8-8d9c-4ec9-8461-f2a3ca1d3d46', extraction).
narrative_ontology:cs_interpretation_layer_present('528756c8-8d9c-4ec9-8461-f2a3ca1d3d46').
narrative_ontology:cs_reading_relation('528756c8-8d9c-4ec9-8461-f2a3ca1d3d46', competence_occupation__real_incident_necessity, forecloses).
narrative_ontology:cs_reading_relation('528756c8-8d9c-4ec9-8461-f2a3ca1d3d46', competence_occupation__hybrid_occupation, coexists_with).
narrative_ontology:cs_axiom('528756c8-8d9c-4ec9-8461-f2a3ca1d3d46', foundational, simulation_sufficiency_for_competence).
narrative_ontology:cs_axiom_status(simulation_sufficiency_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('528756c8-8d9c-4ec9-8461-f2a3ca1d3d46', simulation_sufficiency_for_competence, empirically_contingent).
narrative_ontology:cs_axiom('528756c8-8d9c-4ec9-8461-f2a3ca1d3d46', foundational, training_compliance_as_competence_proxy).
narrative_ontology:cs_axiom_status(training_compliance_as_competence_proxy, holdable).
narrative_ontology:cs_axiom_grounding('528756c8-8d9c-4ec9-8461-f2a3ca1d3d46', training_compliance_as_competence_proxy, conventional).
narrative_ontology:cs_reference_frame('528756c8-8d9c-4ec9-8461-f2a3ca1d3d46', formalized_drill_sufficiency).
narrative_ontology:cs_drift_state('528756c8-8d9c-4ec9-8461-f2a3ca1d3d46', contemporary_post_certification_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('528756c8-8d9c-4ec9-8461-f2a3ca1d3d46', '').
narrative_ontology:cs_kernel_id(competence_occupation__simulation_sufficiency, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, simulation_industry).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, training_compliance_bureaucracy).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, operational_management).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, frontline_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, sells, and maintains simulation platforms, curricula, and fidelity benchmarks. Revenue scales directly with mandated training hours and certification requirements. Invests in studies demonstrating simulation efficacy and in standards bodies that codify fidelity metrics favorable to their technology.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, simulation_industry, beneficiary,
    powerful, biographical, mobile, global).

% Administers training mandates, schedules drill frequency, audits fidelity adherence, and maintains compliance records. Budget and headcount scale with the training apparatus. Defines the observable metrics by which competence is judged and defends the sufficiency of simulation against demands for supplementary or alternative mechanisms.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, training_compliance_bureaucracy, agenda_setter,
    institutional, generational, constrained, national).

% Must attend periodic simulation sessions to maintain certification and role eligibility. Time is diverted from operational duties or rest. Performance in controlled simulation becomes the recorded measure of competence, which may mask decay of real-world judgment and improvisation. Alternative demonstration of competence is not accepted by certifying bodies.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, frontline_operators, payer,
    moderate, biographical, constrained, national).

% Receives measurable compliance reports that satisfy regulators, insurers, and oversight boards. Can demonstrate due diligence through simulation logs and standardized metrics. Bears the direct cost of simulation procurement and staff downtime, but avoids the greater liability and operational disruption of real-incident-based or apprenticeship-heavy training models.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, operational_management, beneficiary,
    powerful, biographical, constrained, national).

% Mandates minimum simulation hours and fidelity standards as a condition of professional license or organizational certification. Treats training compliance as a proxy for public safety. Revises standards based on industry-submitted evidence and post-incident investigations, with limited independent empirical capacity to verify sufficiency claims.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, regulatory_authority, agenda_setter,
    institutional, generational, analytical, national).

% Study skill retention and decay across training modalities in high-reliability settings. Some independent studies find persistent decay or negative transfer despite simulation compliance. Their influence depends on access to operational data, publication freedom, and whether their findings are admitted into standards-setting processes dominated by industry-submitted evidence.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, safety_researchers, observer,
    institutional, generational, analytical, global).

% Relies on the competence of frontline operators in aviation, medicine, nuclear power, and emergency response. Has no direct visibility into whether simulation compliance corresponds to actual skill maintenance, and no voice in whether alternative training models should supplement or replace simulation.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, general_public, excluded,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides repeatable, standardized, safe practice environments for high-risk procedures without exposing lives or equipment to actual danger; allows scheduled, scalable, and auditable competence maintenance across large workforces and dispersed geographies.
% TRANSFER_FUNCTION: Moves training budgets and personnel time from operational units to simulation vendors and compliance departments; transfers the observable measure of competence from frontline operational performance to controlled simulation metrics and compliance logs.
% ABSENT_VOICES: Frontline operators who experience skill decay or negative transfer despite simulation compliance; independent safety researchers whose findings challenge sufficiency; operational managers who would prefer experiential line audits but lack authority to deviate from the mandated simulation track; the general public who bear the safety consequences but are not in the training standards room.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, training budgets would shift away from simulation vendors, compliance bureaucracies would shrink or reorient, alternative training mechanisms (apprenticeship, hybrid refreshers, operational audit) would compete for legitimacy, and frontline operators would face a different certification landscape. The organizational world would rearrange around a reopened contest for how competence is maintained.
% FOUNDING_PROBLEM: High-risk industries needed ways to practice emergency and rare-event procedures without endangering lives, patients, or capital equipment; early training relied heavily on apprenticeship and infrequent real events, producing uneven competence distribution and uncontrolled exposure to risk.
% FOUNDING_PROBLEM_CORROBORATION: Aviation and nuclear safety historians corroborate the early uneven-competence problem. Independent human-factors researchers and some regulatory inspectors attest that the founding problem is partially solved by simulation but that the current arrangement has drifted toward treating the training proxy as the competence itself; industry-funded efficacy studies and vendor-submitted fidelity benchmarks argue the problem remains fully live and simulation-dependent.
narrative_ontology:disappearance_verdict(competence_occupation__simulation_sufficiency, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__simulation_sufficiency, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__simulation_sufficiency, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_occupation__simulation_sufficiency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__simulation_sufficiency, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__simulation_sufficiency_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__simulation_sufficiency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.22 to 0.58 over the interval because the simulation industry progressively captures standards-setting: early simulation adoption filled a genuine safety gap, but as the industry matured it entrenched frequency/fidelity metrics that favored its own revenue model. Suppression rises from 0.30 to 0.62 as alternative mechanisms (line audits, hybrid refreshers, incident-based learning) are structurally excluded from counting toward competence. Theater rises from 0.10 to 0.45 as compliance logging and box-checking displace genuine skill interrogation. Accessibility collapse (0.55) reflects that alternatives are technically possible but institutionally closed: an operator cannot substitute real-world apprenticeship for mandated sim hours. Resistance (0.48) is moderate because frontline operators and some researchers object, but the compliance bureaucracy and vendor consortium dominate the standards conversation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (training compliance, regulatory authority) and the beneficiary seat (simulation industry) should compute toward coordination: they experience the constraint as a necessary, scalable, evidence-based safety architecture. The payer seat (frontline operators) should compute toward extraction: they experience the same constraint as a time-consuming mandate that may not correspond to operational competence, enforced by certification gatekeeping. Operational management sits between, experiencing subsidy (liability cover) and cost (budget drain) simultaneously. The engine should produce divergent per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (simulation_industry, training_compliance_bureaucracy, operational_management) receive low directionality: they collect revenue, budget, or liability protection from the constraint and have exit or arbitrage options. The payer (frontline_operators) receives high directionality: they bear the extracted time, potential skill decay masked by compliance, and have constrained exit because certification is mandatory. The excluded agent (general_public) is structurally trapped with no exit from the safety consequences of the arrangement. Directionality is derived from these structural positions without override.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as pure coordination (Rope) by requiring both beneficiaries and victims and active enforcement â the Tangled Rope gate captures that genuine safe-practice value and asymmetric extraction coexist. It prevents mislabeling as pure extraction (Snare) because the coordination function is real and historically grounded: early simulation did solve a genuine uneven-competence problem. The temporal measurements show the drift path from lower extraction to higher extraction, capturing mandatrophy risk without pre-judging the founding problem as dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_sufficiency_gap,
    'Does simulation at current fidelity and frequency actually prevent operational skill decay, or does it produce an illusory competence profile that decouples from real-world performance?',
    'Independent longitudinal studies comparing operational performance outcomes of simulation-only, hybrid, and incident-exposed cohorts, with blinding to training modality during competence assessment.',
    'If simulation does not prevent decay, the extraction is higher than scored and the coordination function is weaker, pushing classification toward Snare. If simulation does prevent decay, the coordination function is stronger and the current score may overstate extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_sufficiency_gap, empirical, 'Whether simulation efficacy matches the sufficiency claim').

omega_variable(
    industry_capture_of_standards,
    'Has the simulation industry captured the standards-setting and regulatory update process, or do independent experts maintain control over sufficiency definitions?',
    'Funding-flow and revolving-door analysis of standards-body membership; comparison of mandated fidelity metrics against independent human-factors research benchmarks.',
    'If capture is confirmed, the directionality of regulatory and compliance seats shifts toward beneficiary, increasing effective extraction for operators. If standards remain independent, the constraint is closer to genuine Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(industry_capture_of_standards, empirical, 'Whether training standards are captured by vendor interests').

omega_variable(
    alternative_mechanism_exclusion,
    'Are hybrid and real-incident learning models structurally excluded by budget allocation and mandate design, or merely less preferred in a competitive training marketplace?',
    'Regulatory review of whether alternative modalities are permitted as substitutes for mandated simulation hours; budget elasticity analysis in jurisdictions with competing training regimes.',
    'If alternatives are structurally excluded, suppression and accessibility_collapse are higher than scored. If alternatives are merely disadvantaged but substitutable, the constraint is less extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_mechanism_exclusion, empirical, 'Whether suppression of alternatives is structural or preferential').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__simulation_sufficiency, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__simulation_sufficiency, theater_ratio, 0, 0.1).
narrative_ontology:measurement(comp_tr_t5, competence_occupation__simulation_sufficiency, theater_ratio, 5, 0.18).
narrative_ontology:measurement(comp_tr_t10, competence_occupation__simulation_sufficiency, theater_ratio, 10, 0.28).
narrative_ontology:measurement(comp_tr_t15, competence_occupation__simulation_sufficiency, theater_ratio, 15, 0.36).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__simulation_sufficiency, theater_ratio, 20, 0.42).
narrative_ontology:measurement(comp_tr_t25, competence_occupation__simulation_sufficiency, theater_ratio, 25, 0.45).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__simulation_sufficiency, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(comp_be_t5, competence_occupation__simulation_sufficiency, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(comp_be_t10, competence_occupation__simulation_sufficiency, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(comp_be_t15, competence_occupation__simulation_sufficiency, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(comp_be_t20, competence_occupation__simulation_sufficiency, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(comp_be_t25, competence_occupation__simulation_sufficiency, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__simulation_sufficiency, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(comp_su_t5, competence_occupation__simulation_sufficiency, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(comp_su_t10, competence_occupation__simulation_sufficiency, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(comp_su_t15, competence_occupation__simulation_sufficiency, suppression_requirement, 15, 0.54).
narrative_ontology:measurement(comp_su_t20, competence_occupation__simulation_sufficiency, suppression_requirement, 20, 0.59).
narrative_ontology:measurement(comp_su_t25, competence_occupation__simulation_sufficiency, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__simulation_sufficiency, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_occupation__simulation_sufficiency, 0.1).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__real_incident_necessity).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__hybrid_occupation).

% DUAL FORMULATION NOTE:
% This constraint is one of three structurally distinct readings of the competence_occupation kernel. Each reading makes a competing claim about the mechanism necessary and sufficient to maintain expert competence. They are linked as a constraint family because they share the same kernel (competence maintenance in high-reliability organizations) but have different epsilon values, stakeholder structures, and beneficiary/victim profiles. Decomposition follows the epsilon-invariance principle: the label 'competence occupation' conflates claims with different empirical statuses and extraction levels.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
