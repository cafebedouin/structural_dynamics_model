% ============================================================================
% CONSTRAINT STORY: competence_occupation__simulation_sufficiency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Simulation-Sufficiency Doctrine for Competence-Kernel Occupation
 *   domain: organizational/safety_training/high_reliability_operations
 *
 * SUMMARY:
 *   The standing arrangement under contest is the regulatory-training complex
 *   built on the premise that simulator-based drills constitute sufficient
 *   exercise to occupy the competence kernel: mandated drill hours, audited
 *   completion records, fidelity-upgrade cycles, and licensing weight placed
 *   on training compliance. The claim/metric gap is deliberate and
 *   load-bearing: the reading CLAIMS simulation sufficiency (its own framing,
 *   and the regime's official doctrine), while the authored metrics describe
 *   what the regime's operation actually shows — a genuine coordination core
 *   (safe rehearsal of unrehearsable events) with extraction accumulating on
 *   top as compliance-counting substitutes for competence measurement and
 *   vendor revenue scales with the mandate. Epsilon's referent is this
 *   standing simulation-centric regime, assessed by this reading's own
 *   lights: because this reading is the incumbent doctrine, its lights
 *   partially endorse the arrangement's core, so epsilon sits at
 *   moderate-high (0.60) rather than extreme — the extraction measured here
 *   is what rides ON the sufficiency claim (rent-seeking, metric
 *   substitution, crowding-out of alternative occupation mechanisms), not
 *   disagreement with the claim itself. This file instantiates ONE reading of
 *   the competence_occupation kernel; the sibling readings
 *   (real_incident_necessity, hybrid_occupation) are separate constraints
 *   with their own epsilon values, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - simulation_vendors: Primary beneficiary (institutional/arbitrage) — collects revenue scaled to the mandate; co-authors the standard it sells against
 *   - safety_regulators: Agenda-setter (institutional/constrained) — mandates and audits the compliance observable; legitimacy invested in the metric
 *   - corporate_training_departments: Administrator-beneficiary (organized/identity_locked) — runs the regime internally; professional identity fused with program volume
 *   - operating_organizations: Dual-positioned (institutional/constrained) — collects the liability-shielding compliance artifact, pays when decayed competence meets real events
 *   - frontline_operators: Primary payer with partial benefit (moderate/constrained) — supplies the hours, gains scripted rehearsal, bears the undrilled gap
 *   - host_communities: Excluded risk-bearer (powerless/trapped) — holds the residual risk, holds no seat
 *   - skill_decay_researchers: Analytical observer (moderate/analytical) — documents the transfer gap without authority to alter the mandate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, 0.6).
domain_priors:suppression_score(competence_occupation__simulation_sufficiency, 0.58).
domain_priors:theater_ratio(competence_occupation__simulation_sufficiency, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, extractiveness, 0.6).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__simulation_sufficiency, tangled_rope).
narrative_ontology:human_readable(competence_occupation__simulation_sufficiency, "Simulation-Sufficiency Doctrine for Competence-Kernel Occupation").
narrative_ontology:topic_domain(competence_occupation__simulation_sufficiency, "organizational/safety_training/high_reliability_operations").

domain_priors:requires_active_enforcement(competence_occupation__simulation_sufficiency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__simulation_sufficiency, '908233ec-1a09-48f0-a07a-cad30826c143').
narrative_ontology:cs_kernel_codification('908233ec-1a09-48f0-a07a-cad30826c143', formalized).
narrative_ontology:cs_authority_grounding('908233ec-1a09-48f0-a07a-cad30826c143', expertise).
narrative_ontology:cs_interpretation_layer_present('908233ec-1a09-48f0-a07a-cad30826c143').
narrative_ontology:cs_reading_relation('908233ec-1a09-48f0-a07a-cad30826c143', competence_occupation__real_incident_necessity, forecloses).
narrative_ontology:cs_reading_relation('908233ec-1a09-48f0-a07a-cad30826c143', competence_occupation__hybrid_occupation, forecloses).
narrative_ontology:cs_axiom('908233ec-1a09-48f0-a07a-cad30826c143', foundational, simulated_exercise_transfers_to_field_performance).
narrative_ontology:cs_axiom_status(simulated_exercise_transfers_to_field_performance, holdable).
narrative_ontology:cs_axiom_grounding('908233ec-1a09-48f0-a07a-cad30826c143', simulated_exercise_transfers_to_field_performance, empirically_contingent).
narrative_ontology:cs_axiom('908233ec-1a09-48f0-a07a-cad30826c143', secondary, training_compliance_evidences_kernel_occupation).
narrative_ontology:cs_axiom_status(training_compliance_evidences_kernel_occupation, holdable).
narrative_ontology:cs_axiom_grounding('908233ec-1a09-48f0-a07a-cad30826c143', training_compliance_evidences_kernel_occupation, conventional).
narrative_ontology:cs_reference_frame('908233ec-1a09-48f0-a07a-cad30826c143', simulation_competence_equivalence).
narrative_ontology:cs_drift_state('908233ec-1a09-48f0-a07a-cad30826c143', contemporary_transfer_research_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('908233ec-1a09-48f0-a07a-cad30826c143', '').
narrative_ontology:cs_kernel_id(competence_occupation__simulation_sufficiency, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, simulation_vendors).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, corporate_training_departments).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, safety_regulators).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, operating_organizations).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, host_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, operating_organizations).
narrative_ontology:constraint_vindicates(competence_occupation__simulation_sufficiency, transfer_of_training_assumption).
narrative_ontology:constraint_vindicates(competence_occupation__simulation_sufficiency, compliance_evidences_competence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sells simulators, scenario libraries, fidelity upgrades, and compliance-tracking platforms to regulated industries. Revenue scales with mandated drill hours and hardware refresh cycles. Shapes the industry working groups that define what counts as adequate exercise, so the standard and the product line co-evolve. Exit is realistic: the same product stack sells into healthcare simulation, defense, and maritime training.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, simulation_vendors, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(competence_occupation__simulation_sufficiency, simulation_vendors, agenda_setter).

% Mandates minimum drill frequencies and durations, audits completion records, and weighs training compliance heavily in licensing decisions. Gains a legible, countable observable that survives legal challenge. Conceding that the count does not measure competence would expose decades of prior oversight to retrospective attack, so the metric is defended even as its adequacy is questioned.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, safety_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Schedules and runs the drills, owns completion metrics, and reports compliance upward. Departmental headcount and budget scale with program volume. Careers, professional credentials, and internal standing are built on administering the exercise regime; acknowledging that the regime may not occupy the competence it certifies would dissolve the department's reason to exist in its current form.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, corporate_training_departments, agenda_setter,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__simulation_sufficiency, corporate_training_departments, beneficiary).

% Funds the programs and collects the compliance artifact, which serves as evidence of diligence to regulators, insurers, plaintiffs, and courts. The artifact shields liability while the residual risk of undetected skill decay stays on the organization's own books. Cannot exit the regime without surrendering its license to operate.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, operating_organizations, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_occupation__simulation_sufficiency, operating_organizations, payer).

% Spends mandated hours in drills calibrated to compliance counting. Gains genuine rehearsal of scripted scenarios, crew routines, and emergency procedures. Bears the gap when real events couple failures in ways the scenario library did not anticipate, and cannot decline participation without employment consequences; changing employers only moves between equivalent regimes.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, frontline_operators, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(competence_occupation__simulation_sufficiency, frontline_operators, beneficiary).

% Lives downstream, downwind, or alongside facilities operated on the strength of training-compliance records. Bears the residual accident risk that drilled-but-incompletely-occupied competence leaves uncovered. Has no seat where training adequacy is defined and would demand demonstrated competence rather than documented drill hours.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, host_communities, excluded,
    powerless, generational, trapped, local).

% Studies transfer of training, retention curves, and the decoupling of simulated from field performance under stress. Publishes findings that both vendors and critics cite selectively. Holds no formal seat in standard-setting bodies and cannot alter mandates, only document their effects.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, skill_decay_researchers, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__simulation_sufficiency, simulation_vendors).
narrative_ontology:fixing_cost_class(competence_occupation__simulation_sufficiency, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives a geographically distributed workforce recurrent, safe exposure to rare-event and emergency conditions that cannot be rehearsed on operating systems without creating the event; standardizes exercise content, duration, and frequency across sites so that qualification is portable, comparable, and auditable.
% TRANSFER_FUNCTION: Moves operator hours and organizational training budgets toward simulator vendors and internal training departments; moves completion records from operating organizations to regulators, insurers, and courts as evidence of diligence; leaves the residual skill decay that drills do not occupy with operators, host communities, and the organizations' own loss accounts.
% ABSENT_VOICES: Host communities bear the uncovered residual risk but have no seat where training adequacy is defined. Frontline operators' field experience enters only through filtered debrief forms, not as evidence about what the drills failed to occupy. Independent transfer-of-training researchers have no vote in the industry working groups that set fidelity and frequency standards.
% DISAPPEARANCE_RATIONALE: If the simulation-sufficiency regime vanished overnight, training would reorganize around mixed mechanisms — line audits, procedural reinforcement, structured incident review — because the founding problem (safe rehearsal of rare events) is real and unsolved by other means. Vendor revenue would contract sharply, compliance auditing would lose its object, and licensing frameworks built on hour counts would require wholesale rewrite.
% FOUNDING_PROBLEM: Catastrophic and rare-event conditions cannot be practiced on live systems, and early simulators demonstrated dramatic value for procedural and emergency training; the founding problem was how to give distributed workforces recurrent exposure to those conditions safely and at scale.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigation boards and the independent human-factors literature attest the founding problem from outside the beneficiary set: post-event reports repeatedly find that crews performed drilled sequences competently while failing on undrilled couplings, confirming both the need for off-line rehearsal and the incompleteness of simulation alone. No source outside the vendor and training-establishment set attests the sufficiency claim itself.
narrative_ontology:disappearance_verdict(competence_occupation__simulation_sufficiency, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__simulation_sufficiency, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__simulation_sufficiency, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_occupation__simulation_sufficiency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__simulation_sufficiency, 0.6, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness (0.60) reflects extraction that is real but bounded: the coordination core is genuine, so the arrangement is not a pure rent machine, yet the sufficiency overclaim converts a competence question into a procurement schedule — revenue tracks mandated hours, not demonstrated retention. Suppression (0.58) is structural first: participation is mandated, audits are consequential, and licensure rides on completion; roughly seventy percent of the suppressive force is external (mandate, audit, career consequence) and thirty percent internalized (operators calibrate confidence to drill pass/fail — see the calibration omega). Theater ratio (0.47) is the Goodhart signal: a growing share of drill activity repeats scripted scenarios to log countable hours, and the series shows steady substitution of the proxy for the goal. Accessibility collapse (0.55): once the sufficiency premise is accepted inside a regulated frame, alternative occupation mechanisms (line audits, procedural reinforcement, structured incident review) collapse to token status — but they remain conceptually available, and the hybrid reading keeps them live, so collapse is incomplete. Resistance (0.40): operator grumbling, critical transfer literature, and occasional post-incident findings of drilled-response failure constitute real but non-mass resistance. The three temporal series share one grid (1994, 2001, 2008, 2015, 2020, 2026) so every metric is authored at every examined point; the suppression series is included because enforcement capacity visibly ratcheted — each major incident horizon (circa 2010-2011: Deepwater Horizon, Fukushima) added mandates and audit intensity, producing the visible step between 2008 and 2015 before plateauing.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different constraints from identical structure. From the vendor seat the arrangement is a coordination service it supplies and continuously improves; from the regulator seat it is auditable order imposed on an unruly risk landscape; from the training-department seat it is a profession; from the operator seat it is mandatory hours whose adequacy is asserted rather than shown; from the host-community seat it is paperwork standing in for protection. Three actors share the institutional power atom — vendors, regulators, operating organizations — yet experience opposite directionalities because their exits differ: vendors hold arbitrage (the regime is one market among several), while regulators and licensees are constrained (their legitimacy and licenses are inside the regime). That exit asymmetry, not raw power, drives the per-seat divergence the engine computes.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low-directionality seats: vendors (pure collector, arbitrage exit — nearest the beneficiary pole), regulators (collect a legible observable), training departments (collect budget and standing), operating organizations (collect the compliance artifact). Victim declarations map to high-directionality seats: frontline_operators supply the hours and bear the undrilled gap, though their secondary beneficiary role (genuine scripted-scenario rehearsal) pulls them short of the full-target pole; host_communities bear pure residual risk with trapped exit and no compensating flow — effectively the deepest-target seat despite holding the least power. Operating organizations sit near symmetric: the artifact they collect is funded by the budgets they pay, and the residual risk they retain offsets the liability shield. No directionality overrides were authored: the derivation from role declarations plus exit options already separates the three institutional actors (via arbitrage versus constrained exit) and the two victim-side actors (via secondary benefit versus none), which is the differentiation an override would otherwise have been used for.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Reading the arrangement as pure snare would erase the genuine coordination function — catastrophic scenarios genuinely cannot be rehearsed live, and centralized curriculum standardization solves a real collective-action problem no single organization could solve alone. Reading it as pure rope would launder the sufficiency overclaim, under which the observable (completed hours) has been progressively substituted for the goal (occupied competence) and a concentrated vendor class captures revenue indexed to the mandate rather than to retention. The tangled-rope classification holds both facts: coordination function present, asymmetric extraction riding the same structure, active enforcement required to keep the compliance observable authoritative. The founding problem is live (attested from outside the beneficiary set), so this is not mandatrophy resolution territory; the forward risk is piton drift — if theater_ratio continues its climb and the compliance ritual fully detaches from retention, the arrangement degrades toward administered performance. The theater_ratio series is the tripwire for that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (simulation_sufficiency) of the competence_occupation kernel; what structurally changes if a sibling reading displaces it?',
    'Track adoption of competency-based assessment frameworks replacing hour counts (hybrid_occupation ascendant) or post-incident findings that no simulated preparation predicted field performance (real_incident_necessity ascendant).',
    'Under hybrid_occupation displacement, extraction diffuses across multiple mechanism providers and the single-vendor capture structure dissolves; under real_incident_necessity displacement, training adequacy becomes unmeasurable ex ante and the entire audit apparatus loses its object — a categorically different constraint with a different victim set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: this file instantiates one of three mutually exclusive readings; sibling files carry the others.').

omega_variable(
    transfer_validity,
    'Does simulated performance on known scenarios predict field performance under novel, coupled, high-stress conditions — the conditions that actually arrive?',
    'Longitudinal studies correlating drill scores with subsequent field-event performance, and natural experiments where drilled crews faced unprecedented event configurations.',
    'If transfer is weak, the arrangement maintains the observable rather than the competence and effective extraction rises sharply toward snare territory; if transfer is strong for the skill classes that matter, epsilon falls toward rope and the sufficiency claim is vindicated on its own terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_validity, empirical, 'The empirical hinge of the sufficiency axiom: transfer of training from simulator to field.').

omega_variable(
    counterfactual_occupation_attribution,
    'What share of occupied competence-kernel capacity is attributable to simulation specifically, versus routine line operations and procedural reinforcement that would occur under any regime?',
    'Comparative analysis across organizations and eras varying simulation intensity while holding line experience roughly constant, isolating the marginal contribution of drill hours.',
    'If most occupation comes from ordinary work, the constraint takes credit for occupancy it does not cause — its coordination function shrinks, its extraction share grows, and the classification slides toward snare; genuine marginal contribution stabilizes the tangled-rope reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_occupation_attribution, empirical, 'Attribution problem: how much kernel occupation the drills actually cause.').

omega_variable(
    metric_substitution_lock_in,
    'Is hour-count compliance a transitional scaffold toward outcome-measured competence, or a self-reinforcing equilibrium in which every actor''s incentives favor keeping the countable proxy?',
    'Observe whether any major regulator completes a shift from completion-hour metrics to demonstrated-competency assessment without an intervening catastrophe forcing it.',
    'Transitional dynamics support eventual rope-like maturation; lock-in dynamics predict continued theater_ratio growth and eventual piton drift as the ritual detaches from function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_substitution_lock_in, empirical, 'Whether the compliance observable is a bridge or a trap.').

omega_variable(
    calibration_internalization,
    'Is the suppressive force on operators purely structural (mandate, audit, career consequence), or has an internalized layer formed — confidence calibrated to drill pass/fail such that operators believe themselves occupied because they passed?',
    'Post-regime confidence and performance tracking: if operators who scored well on drills show miscalibrated field readiness that persists even where mandates relax, the internalized layer is real.',
    'If internalized, effective suppression exceeds the structural measure — operators carry the miscalibration with them across employers, and removing the mandate would not remove the constraint''s grip on self-assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(calibration_internalization, empirical, 'Structural versus internalized suppression in professional drill compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__simulation_sufficiency, 1994, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t1994, competence_occupation__simulation_sufficiency, theater_ratio, 1994, 0.24).
narrative_ontology:measurement_basis(comp_tr_t1994, observed).
narrative_ontology:measurement(comp_tr_t2001, competence_occupation__simulation_sufficiency, theater_ratio, 2001, 0.29).
narrative_ontology:measurement_basis(comp_tr_t2001, observed).
narrative_ontology:measurement(comp_tr_t2008, competence_occupation__simulation_sufficiency, theater_ratio, 2008, 0.34).
narrative_ontology:measurement_basis(comp_tr_t2008, observed).
narrative_ontology:measurement(comp_tr_t2015, competence_occupation__simulation_sufficiency, theater_ratio, 2015, 0.4).
narrative_ontology:measurement_basis(comp_tr_t2015, observed).
narrative_ontology:measurement(comp_tr_t2020, competence_occupation__simulation_sufficiency, theater_ratio, 2020, 0.44).
narrative_ontology:measurement_basis(comp_tr_t2020, observed).
narrative_ontology:measurement(comp_tr_t2026, competence_occupation__simulation_sufficiency, theater_ratio, 2026, 0.47).
narrative_ontology:measurement_basis(comp_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(comp_be_t1994, competence_occupation__simulation_sufficiency, base_extractiveness, 1994, 0.36).
narrative_ontology:measurement_basis(comp_be_t1994, observed).
narrative_ontology:measurement(comp_be_t2001, competence_occupation__simulation_sufficiency, base_extractiveness, 2001, 0.42).
narrative_ontology:measurement_basis(comp_be_t2001, observed).
narrative_ontology:measurement(comp_be_t2008, competence_occupation__simulation_sufficiency, base_extractiveness, 2008, 0.49).
narrative_ontology:measurement_basis(comp_be_t2008, observed).
narrative_ontology:measurement(comp_be_t2015, competence_occupation__simulation_sufficiency, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement_basis(comp_be_t2015, observed).
narrative_ontology:measurement(comp_be_t2020, competence_occupation__simulation_sufficiency, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement_basis(comp_be_t2020, observed).
narrative_ontology:measurement(comp_be_t2026, competence_occupation__simulation_sufficiency, base_extractiveness, 2026, 0.6).
narrative_ontology:measurement_basis(comp_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t1994, competence_occupation__simulation_sufficiency, suppression_requirement, 1994, 0.38).
narrative_ontology:measurement_basis(comp_su_t1994, observed).
narrative_ontology:measurement(comp_su_t2001, competence_occupation__simulation_sufficiency, suppression_requirement, 2001, 0.43).
narrative_ontology:measurement_basis(comp_su_t2001, observed).
narrative_ontology:measurement(comp_su_t2008, competence_occupation__simulation_sufficiency, suppression_requirement, 2008, 0.49).
narrative_ontology:measurement_basis(comp_su_t2008, observed).
narrative_ontology:measurement(comp_su_t2015, competence_occupation__simulation_sufficiency, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement_basis(comp_su_t2015, observed).
narrative_ontology:measurement(comp_su_t2020, competence_occupation__simulation_sufficiency, suppression_requirement, 2020, 0.57).
narrative_ontology:measurement_basis(comp_su_t2020, observed).
narrative_ontology:measurement(comp_su_t2026, competence_occupation__simulation_sufficiency, suppression_requirement, 2026, 0.58).
narrative_ontology:measurement_basis(comp_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__simulation_sufficiency, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, real_incident_necessity).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, hybrid_occupation).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'keeping operators competent against rare events' decomposes into three structurally distinct constraints — competing readings of the competence_occupation kernel. This file (simulation_sufficiency) carries the incumbent reading: single-mechanism sufficiency, compliance-as-observable, vendor-primary capture; moderate-high epsilon reflecting extraction riding a genuine coordination core. The sibling real_incident_necessity (only real catastrophes authentically occupy the kernel) carries a different epsilon structure entirely — its verification problem is unsolvable ex ante, making its arrangement unfalsifiable by design. The sibling hybrid_occupation (multi-mechanism necessity, unsettled configuration) carries diffuse beneficiaries and no single capturer. The upstream reading influences the downstream ones: the sufficiency doctrine's institutional victory defines the baseline against which the other two readings argue, and its compliance observable is precisely what the hybrid reading proposes to replace. Each member links the others via network.affects_constraints; contamination propagates across the family because all three draw on the same training-budget pool and the same regulatory attention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
