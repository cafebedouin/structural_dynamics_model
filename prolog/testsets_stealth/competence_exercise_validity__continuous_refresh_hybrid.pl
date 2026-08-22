% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__continuous_refresh_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__continuous_refresh_hybrid, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: competence_exercise_validity__continuous_refresh_hybrid
 *   human_readable: Continuous Drill-Cycle Competence Retention Mandate (Hybrid Reading)
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   Safety-critical industries — commercial aviation, nuclear operations,
 *   hospital resuscitation teams, industrial firefighting — run mandated
 *   recurring exercise cycles: full-motion simulator sessions,
 *   emergency-procedure drills, timed team rehearsals, and currency checks.
 *   The arrangement exists because one-time validation decays: a crew
 *   certified at hire does not reliably execute the same rare procedures at
 *   year ten, and no invoice ever arrives for the skill that quietly eroded.
 *   This file instantiates ONE reading of the contested
 *   competence_exercise_validity kernel — the continuous_refresh_hybrid
 *   reading: simulation is necessary but not sufficient, and retention
 *   requires continuous drill cycles rather than one-time validation. The
 *   sibling readings (simulation_as_proxy, real_catastrophe_only) are
 *   separate constraints with their own epsilon values, beneficiary
 *   structures, and classifications, linked through
 *   network.affects_constraints rather than merged, per the
 *   epsilon-invariance principle. Epsilon's referent is the standing
 *   continuous-drill mandate as this reading assesses it: the reading holds
 *   the core purchase genuine, so epsilon sits mid-range even though real
 *   rents flow through the same structure. Claim and metrics are independent
 *   authored facts — the claimed type states this reading's structural
 *   judgment; the metrics describe observed operation, including its theater
 *   drift.
 *
 * KEY AGENTS:
 *   - - safety_regulators: Agenda setter (institutional/analytical) — writes the cycle rules, audits compliance, holds the operating certificate
 *   - - operational_frontline_crews: Primary payer and incidental beneficiary (organized/constrained) — supplies the drill hours and spends the retained competence in emergencies
 *   - - training_simulator_industry: Concentrated beneficiary (institutional/arbitrage) — bills every mandated hour; interval length is its market size
 *   - - internal_training_departments: Beneficiary and internal co-administrator (organized/constrained) — headcount and budget scale with program scope
 *   - - operating_companies: Payer and secondary beneficiary (powerful/constrained) — funds the apparatus, carries the liability, buys insurability
 *   - - protected_third_parties: Beneficiary without participation (powerless/trapped) — receives the residual risk reduction, appears only through proxies
 *   - - alternative_modality_providers: Excluded challenger (moderate/trapped) — high-frequency formats earn no currency credit under device-qualification rules
 *   - - competence_researchers: Analytical observer (moderate/analytical) — owns the decay and spacing evidence every faction selectively cites
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__continuous_refresh_hybrid, 0.53).
domain_priors:suppression_score(competence_exercise_validity__continuous_refresh_hybrid, 0.63).
domain_priors:theater_ratio(competence_exercise_validity__continuous_refresh_hybrid, 0.37).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, extractiveness, 0.53).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 0.63).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 0.37).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, resistance, 0.46).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__continuous_refresh_hybrid, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__continuous_refresh_hybrid, "Continuous Drill-Cycle Competence Retention Mandate (Hybrid Reading)").
narrative_ontology:topic_domain(competence_exercise_validity__continuous_refresh_hybrid, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__continuous_refresh_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__continuous_refresh_hybrid, '82379986-d473-44bf-a0b0-24142be105b9').
narrative_ontology:cs_kernel_codification('82379986-d473-44bf-a0b0-24142be105b9', distributed).
narrative_ontology:cs_authority_grounding('82379986-d473-44bf-a0b0-24142be105b9', distributed).
narrative_ontology:cs_reading_relation('82379986-d473-44bf-a0b0-24142be105b9', competence_exercise_validity__simulation_as_proxy, forecloses).
narrative_ontology:cs_reading_relation('82379986-d473-44bf-a0b0-24142be105b9', competence_exercise_validity__real_catastrophe_only, forecloses).
narrative_ontology:cs_axiom('82379986-d473-44bf-a0b0-24142be105b9', foundational, competence_retention_is_process_dependent).
narrative_ontology:cs_axiom_status(competence_retention_is_process_dependent, holdable).
narrative_ontology:cs_axiom_grounding('82379986-d473-44bf-a0b0-24142be105b9', competence_retention_is_process_dependent, empirically_contingent).
narrative_ontology:cs_axiom('82379986-d473-44bf-a0b0-24142be105b9', foundational, simulation_is_necessary_not_sufficient).
narrative_ontology:cs_axiom_status(simulation_is_necessary_not_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('82379986-d473-44bf-a0b0-24142be105b9', simulation_is_necessary_not_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('82379986-d473-44bf-a0b0-24142be105b9', secondary, calendar_cycles_are_administrative_proxy_for_competency).
narrative_ontology:cs_axiom_status(calendar_cycles_are_administrative_proxy_for_competency, holdable).
narrative_ontology:cs_axiom_grounding('82379986-d473-44bf-a0b0-24142be105b9', calendar_cycles_are_administrative_proxy_for_competency, conventional).
narrative_ontology:cs_reference_frame('82379986-d473-44bf-a0b0-24142be105b9', continuous_spaced_drill_baseline).
narrative_ontology:cs_drift_state('82379986-d473-44bf-a0b0-24142be105b9', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('82379986-d473-44bf-a0b0-24142be105b9', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, operational_frontline_crews).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, training_simulator_industry).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, internal_training_departments).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, operating_companies).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, protected_third_parties).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, operational_frontline_crews).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, operating_companies).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, alternative_modality_providers).
narrative_ontology:constraint_vindicates(competence_exercise_validity__continuous_refresh_hybrid, spacing_effect_skill_retention).
narrative_ontology:constraint_vindicates(competence_exercise_validity__continuous_refresh_hybrid, skill_decay_under_disuse).
narrative_ontology:constraint_vindicates(competence_exercise_validity__continuous_refresh_hybrid, process_dependent_competence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and enforces the recurrent-training rules: mandated simulator cycles, minimum hours, instructor qualifications, and certificate currency requirements. Funds inspector corps to audit compliance and justifies intervals from accident history and decay research. Its leverage is the operating certificate itself; its exposure is public blame when a licensed crew fails in daylight.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, safety_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Flies the line, stands the watches, runs the codes. Several days each year go to mandated recurrent sessions: full-motion simulator stints, emergency-procedure drills, written checks. The sessions keep hand-flown abnormal procedures and rare-event responses available under stress — competence the crew members personally spend in emergencies. The price is schedule disruption, time away from rest and revenue work, and the career consequence of a failed session. Leaving the profession forfeits licensure and accumulated seniority.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, operational_frontline_crews, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__continuous_refresh_hybrid, operational_frontline_crews, beneficiary).

% Builds and operates the full-flight simulators, training centers, and courseware the rules require. Every mandated hour is billable; interval length and device-qualification criteria define its market size. It sells to whichever jurisdiction's rules prevail, advises rulemakers on training standards, and can redeploy its capital across customers and regions at will.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, training_simulator_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Staffs the academies inside each operator: instructors, curriculum writers, scheduling and records staff. Department headcount and budget scale with mandated program scope; members co-design syllabi, run the cycles, and certify completion upward to the regulator. Their work continues only as long as the programs do, which shapes what they propose.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, internal_training_departments, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__continuous_refresh_hybrid, internal_training_departments, agenda_setter).

% Airlines, reactor licensees, hospital systems. They fund the simulators, the academy payroll, and the crew-hours diverted from revenue service, and they carry the liability when competence fails. They receive insurability, regulatory goodwill, and crews who handle the rare bad day. Shrinking the program invites audit findings; relocating to laxer registries costs market access.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, operating_companies, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__continuous_refresh_hybrid, operating_companies, beneficiary).

% Passengers, patients, plant neighbors. They supply no hours and pay no fees; they receive the residual risk reduction that maintained competence produces. They cannot opt out of being flown, treated, or living downwind, and they enter the rulemaking record only through advocacy proxies.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, protected_third_parties, beneficiary,
    powerless, biographical, trapped, global).

% Vendors of VR-based continuous practice, embedded line-oriented micro-drills, and adaptive refresher software. Their formats deliver high-frequency, low-cost repetition but earn little or no credit toward mandated currency, because qualification rules were written around full-motion devices and classroom hours. Their market access runs through the same rulebooks their formats fall outside of.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, alternative_modality_providers, excluded,
    moderate, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__continuous_refresh_hybrid, alternative_modality_providers, payer).

% Learning scientists and human-factors researchers measuring decay curves, spacing effects, and simulator transfer. They publish the evidence every faction selectively cites; they hold no mandate authority and collect no program funds.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, competence_researchers, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__continuous_refresh_hybrid, training_simulator_industry).
narrative_ontology:fixing_cost_class(competence_exercise_validity__continuous_refresh_hybrid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts an invisible, individually unpriced hazard — the silent decay of rarely used emergency skills — into a scheduled, observable, auditable practice event. It synchronizes readiness across crews and shifts, keeps rare abnormal procedures occupiable under stress, and gives regulators and insurers an inspectable token of maintained capability.
% TRANSFER_FUNCTION: Moves paid crew-hours and operating budget from revenue service into the training apparatus (simulator centers, instructor corps, courseware, records systems), and moves assurance outward from operators to regulators, insurers, and the public.
% ABSENT_VOICES: Providers of high-frequency, low-cost formats have no seat in the device-qualification rulemaking that decides whether their formats count toward currency. Veteran operators arguing for competency-triggered rather than calendar-triggered refresh testify but do not vote. Passengers and patients appear only through advocacy proxies.
% DISAPPEARANCE_RATIONALE: If the mandated cycles vanished overnight, the training-industrial complex would contract within quarters; operators under production pressure would cut self-initiated practice first; decay would accumulate silently because no counterfactual ever presents an invoice; and the deferred cost would surface as clustered failures in rare-emergency events years later, re-teaching the founding lesson at accident prices.
% FOUNDING_PROBLEM: Certified-but-decayed competence: crews and teams passed initial validation yet failed the same rare emergency procedures years later, because a one-time demonstration proves nothing about performance at year ten. Mid-century hull-loss clusters and early reactor incident investigations kept finding hands that once knew the procedure.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the learning-science literature on the spacing effect and motor-skill decay (a century of laboratory and field results), accident investigation board findings attributing outcomes to lapsed manual and procedural skills, and actuarial loss data correlating training currency with incident rates. No serious party outside the training apparatus disputes that decay occurs; the live dispute is over dosage and modality, not existence.
narrative_ontology:disappearance_verdict(competence_exercise_validity__continuous_refresh_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__continuous_refresh_hybrid, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__continuous_refresh_hybrid, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_validity__continuous_refresh_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__continuous_refresh_hybrid, 0.53, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__continuous_refresh_hybrid_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__continuous_refresh_hybrid, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__continuous_refresh_hybrid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.53: the mandate's costs are real (crew-days, simulator capital, academy payroll) and a growing slice of spend buys audit tokens rather than exercise, but this reading's own lights hold the core purchase genuine — hence mid-range, not high. Suppression 0.63: compliance is enforced through certificate currency, audit findings, and career consequence; skipping or substituting formats is closed by rule, not by physics, and the enforcement machinery (records systems, inspector corps, escalation paths) visibly hardened across the interval — which is why suppression_requirement is authored as a rising series alongside the other metrics. Theater 0.37: recertification sessions increasingly resemble validation rituals, the exact failure mode this reading warns against, since a passed check is not a retained skill. Accessibility collapse 0.32: understanding the arrangement does not collapse alternatives — distributed micro-practice, VR repetition, and competency-triggered refresh remain conceivable, which is precisely why the excluded-challenger seat exists. Resistance 0.46: operators lobby on cost, unions negotiate relief, vendors resist interval compression; nobody mobilizes to abolish the requirement itself. Cyclical pattern: the extractiveness and theater series dip at t=12 and t=24 — post-incident reform waves that briefly restore genuine content and compress ritual — then resume bureaucratic accretion. The oscillation is partly an extraction mechanism: each reform wave re-legitimates the mandate and enlarges its scope, so the calm phase harvests the enlarged baseline. Base-property metrics are authored at interval end (t=36), the post-accretion state.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical facts. From the vendor seat the mandate is a demand floor: every mandated hour is billable and interval length is market size. From the crew seat the same hour is a career tax that doubles as personal insurance — they pay the time and they are the ones who spend the retained competence in the emergency. From the regulator seat it is an assurance instrument that converts an invisible hazard into an inspectable token. From the operator seat it is a liability hedge priced in crew-hours. The engine derives these divergences from the declared structure; the divergence between seats, not any single seat's verdict, is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (crew-retained competence, vendor revenue, department budgets, operator insurability, public risk reduction) pull those seats toward the beneficiary end; victim declarations (crew time burden, operator spend, the challengers' foreclosed market access) pull toward the target end. The dual-declared seats — crews and operating companies — derive to mid-range values, which matches their genuinely mixed positions, so no directionality overrides are authored: the structural derivation already lands them correctly, and per-power-atom overrides would collide here, since the powerful atom is shared by the vendor (pure collector) and the operator (mixed payer-beneficiary), and the organized atom by the crew (mixed) and the internal department (collector). An atom-level correction would misplace one member of each pair. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live — decay persists, and cockpit and plant automation regenerate skill fade faster than curricula adapt — so the mandate has not outlived its function and mandatrophy is not resolved. The classification guards two mislabels. First, reading the rising theater ratio as a dead mandate walking: the function the arrangement performs is corroborated from outside the benefiting parties by decay science and accident findings, so theatrical accretion is symptom, not verdict. Second, reading the vendor rents as pure extraction behind a coordination cover story: the same structure that bills the hours delivers the readiness good, which is what makes this a hybrid rather than a cover. The watch-item is the theater trajectory: if theater crosses one-half while the functional share flattens, the administrative shell is drifting toward inertial maintenance even though the underlying requirement stays justified — at that point the remedy is content reform, not abolition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_delta,
    'This constraint is the continuous_refresh_hybrid reading of the competence_exercise_validity kernel; what would the sibling readings (simulation_as_proxy, real_catastrophe_only) change structurally if instantiated instead?',
    'Author the sibling stories and compare computed classifications: simulation_as_proxy would shrink the victim set (fewer mandated live hours, lower crew time burden) and concentrate gains wholly in the simulator trade; real_catastrophe_only would strip simulation of exercise-validity entirely, redirecting retention legitimacy to operational exposure and incident review.',
    'The mandate''s cost profile flips across readings: under the proxy reading the live-drill premium above simulation is near-pure rent; under the catastrophe-only reading the entire simulated apparatus is misdirected spend. The disagreement is located in the validity quantum assigned to simulation: none, partial-and-necessary, or sufficient.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_delta, conceptual, 'Committer structure: one reading of the competence_exercise_validity kernel; sibling readings change the victim set and the locus of valid exercise.').

omega_variable(
    simulation_transfer_ceiling,
    'Is there a fidelity-by-frequency ceiling beyond which additional simulated exercise stops transferring to real-event performance — that is, does the necessary-but-not-sufficient boundary sit where this reading places it?',
    'Longitudinal transfer studies linking recurrent simulator dosage to line-check and real-event performance; natural experiments from jurisdictions that altered intervals or device mix.',
    'If high-dosage simulation saturates transfer, this reading collapses toward simulation_as_proxy and the mandated live-drill premium is rent; if transfer is bounded below sufficiency, the extra-cycle requirement is functionally warranted and the extraction component shrinks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_transfer_ceiling, empirical, 'Empirical location of the sufficiency boundary for simulated exercise.').

omega_variable(
    drill_theater_share,
    'What share of mandated drill activity is genuine exercise versus compliance ritual performed to satisfy audit?',
    'Outcome-linked audit: correlate drill content fidelity with subsequent emergency-performance indicators rather than counting completed sessions.',
    'A theater share above one-half would drive the administrative shell piton-ward even while the underlying requirement stays justified; a low share supports the coordination reading of the whole arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drill_theater_share, empirical, 'Functional versus performative composition of mandated drill activity.').

omega_variable(
    interval_calibration_source,
    'Are the mandated cycle intervals calibrated to measured skill-decay curves, or negotiated among regulators, operators, unions, and the training industry?',
    'Compare published decay-curve estimates per skill class against the intervals actually codified, and trace rulemaking dockets for the bargaining record.',
    'Political calibration would relocate part of the measured cost from coordination overhead to rent, strengthening the tangled characterization; scientific calibration would support a purer coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interval_calibration_source, empirical, 'Whether cycle intervals track decay science or institutional bargaining.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__continuous_refresh_hybrid, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cev_hybrid_tr_t0, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 0, 0.22).
narrative_ontology:measurement(cev_hybrid_tr_t6, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 6, 0.27).
narrative_ontology:measurement(cev_hybrid_tr_t12, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 12, 0.25).
narrative_ontology:measurement(cev_hybrid_tr_t18, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 18, 0.33).
narrative_ontology:measurement(cev_hybrid_tr_t24, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 24, 0.31).
narrative_ontology:measurement(cev_hybrid_tr_t30, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 30, 0.39).
narrative_ontology:measurement(cev_hybrid_tr_t36, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 36, 0.37).

% Extraction over time
narrative_ontology:measurement(cev_hybrid_be_t0, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(cev_hybrid_be_t6, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 6, 0.46).
narrative_ontology:measurement(cev_hybrid_be_t12, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(cev_hybrid_be_t18, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 18, 0.51).
narrative_ontology:measurement(cev_hybrid_be_t24, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 24, 0.49).
narrative_ontology:measurement(cev_hybrid_be_t30, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(cev_hybrid_be_t36, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 36, 0.53).

% Suppression requirement over time
narrative_ontology:measurement(cev_hybrid_su_t0, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(cev_hybrid_su_t6, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 6, 0.52).
narrative_ontology:measurement(cev_hybrid_su_t12, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(cev_hybrid_su_t18, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 18, 0.58).
narrative_ontology:measurement(cev_hybrid_su_t24, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(cev_hybrid_su_t30, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(cev_hybrid_su_t36, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 36, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__continuous_refresh_hybrid, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity__simulation_as_proxy).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity__real_catastrophe_only).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'simulation-based training maintains competence' conflates three structurally distinct claims about exercise validity, each with its own stable epsilon, beneficiary structure, and classification. This file is the hybrid reading (simulation necessary, insufficient; continuous cycles required). The proxy reading is cited upstream by the simulator trade as evidence for its market; the catastrophe-only reading is cited by traditionalist practitioners against simulator reliance; this reading's safety-record evidence is cited against both siblings. Each story links the other two through network.affects_constraints; none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
