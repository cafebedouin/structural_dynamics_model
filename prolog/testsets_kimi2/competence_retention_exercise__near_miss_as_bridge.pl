% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__near_miss_as_bridge
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: competence_retention_exercise__near_miss_as_bridge
 *   human_readable: Near-Miss as Bridge for Simulator Validation
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   In high-reliability organizations, the doctrine that near-miss incidents
 *   provide sufficient real-world feedback to validate and update simulator
 *   trainingâwithout requiring catastrophesâhas become the dominant
 *   paradigm. This constraint story treats that doctrine as an institutional
 *   arrangement that coordinates safety learning while asymmetrically
 *   distributing costs and risks. It is one reading of the
 *   competence_retention_exercise kernel, positioned between pure-simulation
 *   and catastrophe-necessary readings.
 *
 * KEY AGENTS:
 *   - safety_regulators: Primary agenda-setters (institutional/constrained) â set validation standards and derive legitimacy
 *   - hro_leadership: Primary beneficiaries (institutional/constrained) â collect liability protection and accreditation
 *   - simulator_vendors: Concentrated beneficiaries (powerful/mobile) â capture training revenue from validation contracts
 *   - frontline_operators: Primary targets (moderate/identity_locked) â bear reporting burden and professional risk
 *   - at_risk_public: Diffuse targets (powerless/trapped) â bear residual tail risk if sufficiency claim fails
 *   - catastrophe_memorial_advocates: Excluded voices (moderate/trapped) â excluded from standard-setting
 *   - safety_researchers: Analytical observers (analytical) â study efficacy, some embedded, some critical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__near_miss_as_bridge, 0.45).
domain_priors:suppression_score(competence_retention_exercise__near_miss_as_bridge, 0.55).
domain_priors:theater_ratio(competence_retention_exercise__near_miss_as_bridge, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, extractiveness, 0.45).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__near_miss_as_bridge, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__near_miss_as_bridge, "Near-Miss as Bridge for Simulator Validation").
narrative_ontology:topic_domain(competence_retention_exercise__near_miss_as_bridge, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__near_miss_as_bridge).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__near_miss_as_bridge, 'fa55cd16-b98f-439f-a4ca-cedfed4aedb6').
narrative_ontology:cs_kernel_codification('fa55cd16-b98f-439f-a4ca-cedfed4aedb6', distributed).
narrative_ontology:cs_authority_grounding('fa55cd16-b98f-439f-a4ca-cedfed4aedb6', expertise).
narrative_ontology:cs_interpretation_layer_present('fa55cd16-b98f-439f-a4ca-cedfed4aedb6').
narrative_ontology:cs_reading_relation('fa55cd16-b98f-439f-a4ca-cedfed4aedb6', competence_retention_exercise__simulation_as_sufficient, influences).
narrative_ontology:cs_reading_relation('fa55cd16-b98f-439f-a4ca-cedfed4aedb6', competence_retention_exercise__catastrophe_as_necessary, influences).
narrative_ontology:cs_axiom('fa55cd16-b98f-439f-a4ca-cedfed4aedb6', foundational, near_miss_epistemic_sufficiency).
narrative_ontology:cs_axiom_status(near_miss_epistemic_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('fa55cd16-b98f-439f-a4ca-cedfed4aedb6', near_miss_epistemic_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('fa55cd16-b98f-439f-a4ca-cedfed4aedb6', foundational, hybrid_competence_maintenance).
narrative_ontology:cs_axiom_status(hybrid_competence_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('fa55cd16-b98f-439f-a4ca-cedfed4aedb6', hybrid_competence_maintenance, empirically_contingent).
narrative_ontology:cs_reference_frame('fa55cd16-b98f-439f-a4ca-cedfed4aedb6', hybrid_validation_framework).
narrative_ontology:cs_drift_state('fa55cd16-b98f-439f-a4ca-cedfed4aedb6', contemporary_hro_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fa55cd16-b98f-439f-a4ca-cedfed4aedb6', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, safety_regulators).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, hro_leadership).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, simulator_vendors).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, frontline_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, at_risk_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandate near-miss reporting systems and simulator validation standards across safety-critical industries. Derive regulatory legitimacy and operational manageability from the doctrine that routine near-miss data suffices to keep simulators current. Cannot easily abandon the framework without replacement infrastructure.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, safety_regulators, agenda_setter,
    institutional, generational, constrained, global).

% Implement and administer near-miss programs and simulator update cycles. Collect liability protection and institutional accreditation from demonstrating compliance with the doctrine. Competes on safety metrics within an industry where near-miss volume is treated as proxy for safety culture.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, hro_leadership, beneficiary,
    institutional, generational, constrained, national).

% Sell and maintain training simulators validated against near-miss event libraries. Capture recurring revenue from update contracts tied to near-miss data feeds. Market position depends on the accepted sufficiency of near-miss events for fidelity certification.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, simulator_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Required to report near-misses in structured formats, participate in simulator refresh sessions, and carry the bureaucratic overhead of the learning pipeline. Professional identity is fused with safety-consciousness and compliance; exiting the reporting burden means exiting the profession. Bear blame risk when near-misses are retrospectively judged.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, frontline_operators, payer,
    moderate, biographical, identity_locked, local).

% Bear residual catastrophic tail risk if the doctrine fails to actually validate simulator fidelity for rare events. Do not participate in standard-setting and cannot individually opt out of aviation, chemical processing, or nuclear systems governed by the constraint.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, at_risk_public, payer,
    powerless, biographical, trapped, global).

% Argue that only catastrophes produce the political will and visceral organizational learning required for genuine safety. Excluded from mainstream safety standard-setting bodies that treat near-miss sufficiency as settled doctrine. Their advocacy is treated as grief-driven rather than epistemically valid.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, catastrophe_memorial_advocates, excluded,
    moderate, generational, trapped, national).

% Study the efficacy of near-miss systems and simulator transfer. Some are embedded in the paradigm through funding and professional networks; others maintain critical distance. Provide the empirical surface that could validate or undermine the sufficiency claim.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, safety_researchers, observer,
    analytical, generational, analytical, global).

narrative_ontology:fixing_cost_class(competence_retention_exercise__near_miss_as_bridge, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates organizational learning across safety-critical industries by establishing a shared pipeline from operational near-miss events to simulator validation standards, permitting continuous training updates without waiting for catastrophic failures.
% TRANSFER_FUNCTION: Moves experiential data and compliance effort from frontline operations into simulator update cycles and institutional safety records; moves liability protection, regulatory legitimacy, and training revenue to organizations and vendors.
% ABSENT_VOICES: Catastrophe memorial advocates and frontline skeptics who believe near-misses lack the emotional and structural gravity to validate true catastrophe preparedness; they are excluded from standard-setting bodies that treat near-miss sufficiency as settled.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, safety training regimes would reorganize around either pure high-fidelity simulation or catastrophe-driven validation; frontline reporting burdens would drop, institutional liability exposure would rise, and the simulator update market would contract sharply.
% FOUNDING_PROBLEM: Catastrophe-driven learning is too sparse and politically costly to maintain competence in high-hazard systems; organizations needed a continuous, lower-cost feedback mechanism between routine operations and training that did not require mass casualty events.
% FOUNDING_PROBLEM_CORROBORATION: Independent safety science research (Reason, Perrow, Dekker traditions) attests the need for continuous learning in HROs. However, the SPECIFIC sufficiency claimâthat near-misses alone validate simulatorsâis contested by researchers outside the funded training-industry complex, and frontline unions periodically dispute the doctrine.
narrative_ontology:disappearance_verdict(competence_retention_exercise__near_miss_as_bridge, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__near_miss_as_bridge, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__near_miss_as_bridge, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_retention_exercise__near_miss_as_bridge, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__near_miss_as_bridge, 0.45, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness at 0.45 reflects moderate but real extraction: frontline operators supply reporting labor and cognitive load, institutions collect legitimacy, and the public carries unpriced tail risk. It is not higher because the coordination benefitâgenuine safety improvements, catastrophe avoidanceâis structurally real. Suppression at 0.55 captures the marginalization of catastrophe-driven learning advocates and the institutional pressure to treat near-miss volume as proxy for safety culture. Theater at 0.40 reflects the tendency of mature near-miss programs to substitute reporting volume and bureaucratic compliance for actual organizational learning. Resistance at 0.35 is modest: frontline unions and critical researchers raise episodic objections, but the paradigm is institutionally entrenched.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator and executive seat, the constraint appears as a successful coordination mechanism that prevents catastrophes at manageable cost. From the frontline operator seat, it appears as an ongoing compliance tax with unclear local safety returns and non-zero blame exposure. From the public seat, the constraint is largely invisible until a catastrophic failure reveals a simulator gap. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Safety regulators and HRO leadership sit near the beneficiary end: they collect legitimacy, liability reduction, and operational manageability. Simulator vendors sit even nearer the beneficiary end due to market mobility. Frontline operators sit near the target end due to identity-locked exit and direct reporting burden. The at-risk public sits at the extreme target end: powerless, trapped in the infrastructure, and paying through unpriced residual risk. Catastrophe advocates are excluded entirely, their exclusion constituting part of the suppression structure.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a pure rope would miss the asymmetric reporting burden on operators and the diffuse risk transfer to the public. Classifying it as a pure snare would miss the genuine coordination function: near-miss systems have demonstrably prevented catastrophes and standardized learning across organizations. Tangled rope is the only category that admits both the real coordination and the real extraction. If the near-miss pipeline were someday shown to be epistemically hollowâpure theater with no learning transferâthe constraint would degrade toward piton or snare; until then, the coordination function is structurally inseparable from the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sufficiency_empirical_status,
    'Do near-miss events actually provide sufficient information to validate simulator fidelity for rare catastrophic scenarios, or does the doctrine create epistemic overreach and false confidence?',
    'Longitudinal outcome studies comparing safety outcomes in organizations using near-miss validation against those using alternative frameworks; simulator fidelity tests against actual catastrophic parameters where ethically possible.',
    'If near-misses are insufficient, the constraint''s extraction (false confidence, residual public risk) is higher than modeled and the coordination function is weaker; if sufficient, extraction is lower and coordination dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_empirical_status, empirical, 'Empirical status of the near-miss sufficiency claim').

omega_variable(
    reporting_burden_asymmetry,
    'Is the compliance burden of near-miss reporting symmetrically distributed across organizational levels, or asymmetrically concentrated on frontline operators?',
    'Time-allocation and incident-reporting flow studies within HROs; comparison of reporting burden hours and disciplinary exposure by organizational level.',
    'If asymmetrically concentrated, the constraint''s directionality toward frontline operators is stronger, supporting the tangled_rope classification; if symmetric, it moves toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reporting_burden_asymmetry, empirical, 'Distribution of reporting burden across organizational levels').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of a contested kernel. How would classification change if the simulation_as_sufficient reading were adopted instead, eliminating the near-miss reporting requirement?',
    'Track institutional adoption of sibling readings; a shift to simulation_as_sufficient would remove operator reporting extraction but might increase simulator vendor capture.',
    'Sibling reading adoption would alter the beneficiary-victim structureâpotentially shifting classification toward snare (pure vendor extraction) or rope (if genuinely costless coordination)âand would change the directionality map entirely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural impact of sibling reading adoption').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__near_miss_as_bridge, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0, 0.15).
narrative_ontology:measurement(comp_tr_t5, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 5, 0.2).
narrative_ontology:measurement(comp_tr_t10, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 10, 0.25).
narrative_ontology:measurement(comp_tr_t15, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 15, 0.3).
narrative_ontology:measurement(comp_tr_t20, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 20, 0.35).
narrative_ontology:measurement(comp_tr_t25, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 25, 0.38).
narrative_ontology:measurement(comp_tr_t30, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(comp_be_t5, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(comp_be_t10, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(comp_be_t15, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 15, 0.39).
narrative_ontology:measurement(comp_be_t20, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(comp_be_t25, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 25, 0.44).
narrative_ontology:measurement(comp_be_t30, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(comp_su_t5, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(comp_su_t10, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(comp_su_t15, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(comp_su_t20, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(comp_su_t25, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 25, 0.55).
narrative_ontology:measurement(comp_su_t30, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__near_miss_as_bridge, identity_coordination).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__near_miss_as_bridge, 0.08).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, simulation_as_sufficient).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, catastrophe_as_necessary).

% DUAL FORMULATION NOTE:
% This constraint is one member of the competence_retention_exercise kernel family. The three readingsâcatastrophe_as_necessary, near_miss_as_bridge, and simulation_as_sufficientâare structurally distinct constraints with different epsilon values, stakeholder maps, and victim sets. They are linked as siblings, not as one constraint with measurement parameters.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
