% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__simulation_as_adequate_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__simulation_as_adequate_exercise, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: competence_exercise_requirement__simulation_as_adequate_exercise
 *   human_readable: Simulation with High Fidelity and Debriefing as Adequate Competence Exercise
 *   domain: safety/organizational_learning/regulatory
 *
 * SUMMARY:
 *   High-reliability organizations (aviation, nuclear, healthcare) maintain
 *   pilot and safety-critical competence through mandatory recurrent
 *   simulation training with fidelity requirements and structured debriefing.
 *   The simulation-reading claims this mechanism—combined with
 *   catastrophe-free decades—adequately exercises and validates the
 *   competence kernel. Regulatory authorities (FAA, EASA) enforce this
 *   reading via certification mandates, fidelity standards, and training-hour
 *   requirements. Airlines and simulation vendors benefit directly from the
 *   mandate; flight crews and trainees bear compliance costs. The kernel
 *   contest is empirically consequential: if real-world incidents (accidents,
 *   near-misses) recur despite strict simulation compliance, that evidence
 *   contradicts the reading; if incidents remain rare, the reading claims
 *   vindication. This story instantiates the simulation-adequacy reading
 *   only, not the catastrophe-anchor or hybrid-dependency siblings.
 *
 * KEY AGENTS:
 *   - Regulatory authorities (FAA, EASA): set and enforce the mandate; incentivized to find simulation-adequate framework to reduce training liability and cost
 *   - Airline operators: benefit from schedule efficiency and reduced line-training burden; simultaneously constrained by simulator costs
 *   - Flight crews: locked into recurrent simulator cycles as career maintenance; experience constraint as identity-constitutive
 *   - Pilot trainees: entire entry pathway simulator-gated; cannot advance without simulator credential accumulation
 *   - Simulation vendors: directly capture regulatory-mandate revenue; incentivized to raise fidelity requirements
 *   - Line operations pilots (excluded): would argue real-world operations necessary for competence in novel/degraded conditions
 *   - Accident investigation community (excluded): empirical evidence of competence gaps in certain skill domains
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__simulation_as_adequate_exercise, 0.68).
domain_priors:suppression_score(competence_exercise_requirement__simulation_as_adequate_exercise, 0.54).
domain_priors:theater_ratio(competence_exercise_requirement__simulation_as_adequate_exercise, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0.54).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__simulation_as_adequate_exercise, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__simulation_as_adequate_exercise, "Simulation with High Fidelity and Debriefing as Adequate Competence Exercise").
narrative_ontology:topic_domain(competence_exercise_requirement__simulation_as_adequate_exercise, "safety/organizational_learning/regulatory").

domain_priors:requires_active_enforcement(competence_exercise_requirement__simulation_as_adequate_exercise).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__simulation_as_adequate_exercise, '23a2606b-c069-4264-be00-2e4ea691e0b4').
narrative_ontology:cs_kernel_codification('23a2606b-c069-4264-be00-2e4ea691e0b4', fixed_text).
narrative_ontology:cs_authority_grounding('23a2606b-c069-4264-be00-2e4ea691e0b4', extraction).
narrative_ontology:cs_interpretation_layer_present('23a2606b-c069-4264-be00-2e4ea691e0b4').
narrative_ontology:cs_reading_relation('23a2606b-c069-4264-be00-2e4ea691e0b4', competence_exercise_requirement__catastrophe_as_necessary_anchor, forecloses).
narrative_ontology:cs_reading_relation('23a2606b-c069-4264-be00-2e4ea691e0b4', competence_exercise_requirement__hybrid_dependency, influences).
narrative_ontology:cs_axiom('23a2606b-c069-4264-be00-2e4ea691e0b4', foundational, high_fidelity_simulation_sufficient_for_competence_exercise).
narrative_ontology:cs_axiom_status(high_fidelity_simulation_sufficient_for_competence_exercise, holdable).
narrative_ontology:cs_axiom_grounding('23a2606b-c069-4264-be00-2e4ea691e0b4', high_fidelity_simulation_sufficient_for_competence_exercise, empirically_contingent).
narrative_ontology:cs_axiom('23a2606b-c069-4264-be00-2e4ea691e0b4', secondary, transfer_of_training_completes_across_all_skill_domains).
narrative_ontology:cs_axiom_status(transfer_of_training_completes_across_all_skill_domains, holdable).
narrative_ontology:cs_axiom_grounding('23a2606b-c069-4264-be00-2e4ea691e0b4', transfer_of_training_completes_across_all_skill_domains, empirically_contingent).
narrative_ontology:cs_reference_frame('23a2606b-c069-4264-be00-2e4ea691e0b4', simulation_centered_training_sufficiency).
narrative_ontology:cs_drift_state('23a2606b-c069-4264-be00-2e4ea691e0b4', contemporary_post_30_years_compliance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('23a2606b-c069-4264-be00-2e4ea691e0b4', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_authorities).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, airline_operators).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, simulation_vendors).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, flight_crews).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, pilot_trainees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, flight_crews).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, airline_operators).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__simulation_as_adequate_exercise, transfer_validity_hypothesis).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__simulation_as_adequate_exercise, cognitive_fidelity_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establish and enforce mandates on pilot training and competence maintenance. Set standards for what counts as adequate competence exercise (simulator hours, fidelity requirements, debriefing protocols). Justify the simulation-first framework as evidence-based and cost-efficient. Avoid mandating catastrophe-proxies or real-world incident exposure. Benefit from reduced training costs and risk liability from incident normalization.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Operate fleets under regulatory mandates and maintain pilot competence via scheduled simulator cycles. Directly benefit from reduced training time, lower line operations costs, and predictable simulator availability. Simultaneously bear costs: maintain simulator hardware/software, schedule pilots for recurrent training, pay simulator facility fees. Benefits exceed costs in peacetime; costs rise sharply during fleet emergencies or atypical operational demands.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, airline_operators, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__simulation_as_adequate_exercise, airline_operators, payer).

% Required to complete recurrent simulator training on defined schedules to maintain currency and competence certification. Experience the constraint as mandatory recurrent expense of their career. Benefit from realistic training environment that reduces line errors and operational surprises. The constraint binds their career certification to simulator performance, making exit from the arrangement impossible without leaving the profession.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, flight_crews, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__simulation_as_adequate_exercise, flight_crews, beneficiary).

% Undergo initial type-rating and competence certification via high-fidelity simulation. Entire entry pathway is constructed around simulator gates: hours must be simulator-logged, evaluations must be simulator-based, certification sign-off occurs in simulator. No alternative entry exists within the system. Identity as a certified pilot is constituted through and dependent on simulator-credential accumulation. Cannot exit without abandoning the pilot identity itself.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, pilot_trainees, payer,
    powerless, immediate, identity_locked, global).

% Supply full-motion, high-fidelity simulator hardware, software, and maintenance services. Directly capture revenue from regulatory mandate for fidelity standards, training hour requirements, and periodic recertification cycles. Benefit from regulatory entrenchment: fidelity requirements exclude low-cost alternatives, recurring maintenance contracts guarantee cash flow, and regulatory changes (increased fidelity, more training hours) expand addressable market.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, simulation_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Real-world operational incidents (near-misses, accidents) are the empirical falsification path for this reading. Under this constraint's framing, they are deliberately excluded from the competence-maintenance story: if catastrophes have become rarer, the reading claims that validates the simulation adequacy axiom. If catastrophes recur despite decades of simulation compliance, that evidence contradicts the reading. The incident class is not seated as an agent but is structurally central to the kernel contest.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, operational_incidents_class, excluded,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(competence_exercise_requirement__simulation_as_adequate_exercise, operational_incidents_class).

% Operational pilots and crews running actual scheduled flights encounter novel scenarios, equipment degradations, and real-weather complexity that no simulator can replicate with full fidelity. They would argue that regular line operations (not incident-proxies) are necessary for maintaining problem-solving competence under actual operational load. They are excluded from the formal competence mandate: line experience is not credited in the regulatory framework; only simulator hours count.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, line_operations_seat, excluded,
    moderate, biographical, constrained, global).

% Study transfer of training from simulator to line, skill decay under non-use, and competence maintenance mechanisms. Generate empirical and theoretical evidence about what constitutes adequate exercise. Operate outside the regulatory mandate; their findings are cited to justify the constraint but they do not enforce it.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, human_factors_researchers, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__simulation_as_adequate_exercise, simulation_vendors).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__simulation_as_adequate_exercise, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a scalable, standardized, and cost-efficient system for maintaining pilot and crew competence across large, heterogeneous fleet populations. Centralizes competence evaluation, prevents degradation from flight-hour gaps, provides repeatable and auditable training. Solves the problem: 'how to ensure minimal competence across a population that does not fly frequently enough to maintain skills through line operations alone.'
% TRANSFER_FUNCTION: Transfers pilot time from scheduled line operations (revenue-productive, valuable) to simulator facilities (non-productive, expensive). Transfers regulatory oversight from individual airline decisions to standardized fidelity requirements and hour mandates. Extracts revenue to simulation vendors for equipment, software, and maintenance. Extracts authority-maintenance benefit to regulators (liability shield, documented compliance framework).
% ABSENT_VOICES: Line-operation pilots and crews whose experiential knowledge of real-world competence maintenance is structurally excluded from the formal mandate; human factors researchers who have documented limits to simulator transfer and the empirical necessity of real-world anchoring in certain domains; accident investigators whose post-incident analysis reveals competence gaps tracing to simulator-only training history; operational pilots in emergency or non-standard conditions (high-altitude airports, severe-weather exposure, degraded-aircraft operations) where simulator fidelity limits constrain readiness.
% DISAPPEARANCE_RATIONALE: If the simulation-adequacy mandate disappeared, airlines would rebuild competence maintenance around line operations, mentorship, and selective simulator use for high-risk procedures. Simulator vendors would lose regulatory mandate revenue and consolidate around premium-market customers. Regulatory authorities would face liability exposure for competence assurance without a documented framework. Pilot training economics would reorganize within months.
% FOUNDING_PROBLEM: Pilot competence must be maintained across large, heterogeneous fleets where individual pilot flying hours are insufficient to maintain all required skills; real-world operations cannot serve as primary training venue because learning-by-failure risk is unacceptable; prior to mandated simulation, competence degradation during low-flying-hour periods was documented, and no reliable mechanism existed to certify minimum competence across a fleet.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory authorities and airline operators attest the founding problem remains live: fleet heterogeneity, schedule variation, and safety-criticality make line-only competence maintenance inadequate. Human factors researchers and accident investigators attest the founding problem is PARTIALLY solved: simulation effectively maintains procedural competence, but empirical evidence shows simulator-only training leaves gaps in domains requiring environmental fidelity (weather decision-making, system degradation diagnosis, novel-scenario problem-solving under information constraints). Line-operation pilots and fleet emergency handlers attest the founding problem is substantially solved and the mandate persists as regulatory/vendor convenience rather than safety necessity.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__simulation_as_adequate_exercise, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__simulation_as_adequate_exercise, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_requirement__simulation_as_adequate_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__simulation_as_adequate_exercise_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__simulation_as_adequate_exercise, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_requirement__simulation_as_adequate_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) and RISING over the interval because: (1) simulator fidelity requirements and training hour mandates accumulate regulatory overhead (real cost), (2) vendor margin on certified equipment and maintenance compounds, (3) pilots and trainees bear rising compliance cost as recurrency intervals tighten and fidelity standards increase. The theater_ratio is moderate (0.42, rising from 0.28) because the simulators do provide real competence training, but an increasing share of the training time is ceremonial—compliance documentation, checklist performance for certification, scheduled hours that exceed functional necessity. Suppression is moderate (0.54) because the constraint is enforced via certification gates (pilots lose ratings without compliance) and regulatory audit, but the suppression is not coercive in the jeopardy sense: pilots are not threatened with injury or forced labor; they are threatened with career gatekeeping. This is structural suppression (exit is career-end), not coercive suppression (immediate harm). The measurement series shows PLATEAU around t=35–40: extractiveness levels off, theater ratio stabilizes, suppression stays flat. This plateau is the diagnostic signature: the constraint has matured into its stable state. Continued catastrophe-freedom (no major incidents despite decades of simulation-only compliance) is the reading's empirical defense; any cluster of incidents (particularly those tracing to competence gaps in novel conditions) would move this plateau downward and shift the classification.
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory and vendor seat, the simulation mandate is a success story: decades without major competence-gap incidents, cost-controlled training, repeatable fidelity standards. From the airline operations and line-crew seat, the constraint is increasingly performative: actual line competence is maintained by line operations experience and crew mentorship; the mandate enforces simulator hours that are necessary for regulatory checkbox, not for operational readiness in novel conditions. From the trainee seat, the constraint is constitutive of professional identity but also extractive of entry-cost. The engine will compute these divergences from the power/exit data: institutional regulatory seats will compute as rope-beneficiary; moderate-power operational seats will compute as tangled_rope symmetric; powerless trainees will compute as snare-target. The perspectival gap is structural in the data, not claimed.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory authorities and vendors sit near the beneficiary end of the directionality spectrum (d ~ 0.2–0.3): they collect rents from the mandate and face minimal exit pressure. Airline operators sit near symmetric (d ~ 0.5): they benefit from schedule efficiency and reduced line-risk but pay simulator costs and face fleet emergency pressure that makes the mandate inconvenient. Flight crews and trainees sit near the target end (d ~ 0.75–0.85): they bear full compliance cost (time, scheduler impact, certification pressure), have constrained exit (career gatekeeping), and the benefit to them is diffuse (safer operations, employer confidence) rather than direct income. Trainees especially face identity-locked exit: abandoning compliance means abandoning the pilot identity, not just this job. No directionality overrides are needed: the structural derivation from beneficiary/victim + exit produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows early-stage mandatrophy signals: (1) the founding problem (competence maintenance across heterogeneous schedules) was genuinely live in the 1980s–2000s when simulator fidelity was lower and the mandate was newer; (2) catastrophe-free decades since strong compliance measures were established suggest the founding problem is substantially SOLVED for procedural/checklist competencies; (3) yet the mandate intensifies (fidelity requirements rising, hour requirements rising, vendor market expanding) not because the founding problem worsened but because the regulatory and vendor machinery became self-perpetuating. The theater_ratio rising from 0.28 to 0.42 is the mandatrophy signature: real training (low theater) is gradually replaced by ceremonial compliance (high theater). However, this is NOT yet full piton status because: (a) the mandate still provides real benefit (procedural competence is maintained), (b) a plausible alternative reading (the catastrophe-anchor reading) contests whether simulation-only is adequate, and (c) no seat has fully abandoned the mandate—beneficiaries still collect, payers still comply. The correct classification is tangled_rope trending toward piton, NOT piton yet. The claim/metric gap (claimed tangled_rope, metrics describe rising theater and plateau) is intentional and diagnostically appropriate: the engine's per-seat computation will flag the theater drift as a mandatrophy candidate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transfer_validity_boundary,
    'Does high-fidelity simulation with realistic debriefing TRANSFER all critical competencies to line operations, or are there skill domains (crisis decision-making, equipment degradation diagnosis, novel-scenario problem-solving) that require real-world anchoring to maintain?',
    'Meta-analysis of transfer studies from human factors literature; post-incident investigation analysis correlating competence-gap findings with pilots'' history (simulation-only vs. hybrid experience); longitudinal studies of skill decay in specific domains across simulator-only and mixed-exposure cohorts.',
    'If transfer is complete across all domains, the simulation-adequacy reading stands. If transfer gaps exist in specific domains (particularly those involving environmental fidelity, novel scenarios, or degraded-information problem-solving), the hybrid-dependency reading gains empirical support and the extractiveness of simulation-only mandates increases because they exclude necessary competence maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_validity_boundary, empirical, 'Whether simulator fidelity boundaries map to competence transfer boundaries.').

omega_variable(
    catastrophe_frequency_baseline,
    'Is the observed catastrophe-free or low-incident record from 1990 onwards attributable to the simulation mandate itself, or to confounding improvements (better aircraft design, systems redundancy, air-traffic management automation, weather forecasting)?',
    'Incident trend analysis: disaggregate crew-competence-attribution incidents from systems/design/ATC incidents; compare trend slopes before and after simulation-mandate phase-in; study incidents in jurisdictions with and without strict simulation mandates; natural experiments from fleet transitions or emergency operations.',
    'If catastrophe-free decades are attributable to simulation, the reading''s empirical claim is validated. If incidents decline regardless of simulation intensity, the reading''s vindication evaporates and the mandate shifts from necessary-for-safety to performative-for-liability. This becomes the mandatrophy resolution path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_frequency_baseline, empirical, 'Whether the catastrophe-free record validates the reading or confounds it.').

omega_variable(
    regulatory_authority_incentive_structure,
    'Does the regulatory authority''s maintenance of the simulation-adequacy reading serve primarily to ensure actual safety, or does it serve to reduce the authority''s liability exposure by creating a documented, repeatable compliance framework (whether or not it optimizes for real competence)?',
    'Regulatory decision-history analysis: examine cases where evidence of competence gaps in simulator-only contexts was raised to regulatory bodies; trace whether the evidence prompted mandate revision or was dismissed; study regulatory response to incidents traced to competence-domain limits of simulation; compare simulation-fidelity requirements across jurisdictions and detect whether they correlate with measured safety improvements or with organizational convenience.',
    'If regulatory maintenance of the reading is evidence-responsive, it is a genuine coordination mechanism with extraction overlay. If regulatory maintenance is evidence-resistant, the extraction element dominates and the constraint shifts toward snare (the reading persists because it shields the authority from liability, not because it maintains competence).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_authority_incentive_structure, conceptual, 'Whether the regulatory reading is evidence-tracking or evidence-resistant.').

omega_variable(
    simulation_fidelity_frontier,
    'Is the current generation of high-fidelity simulators sufficiently faithful to the operational environment to maintain competence in all critical skill domains, or are there engineering or physics limits to simulator fidelity that create irreducible gaps for certain competencies?',
    'Simulation engineering analysis of fidelity boundaries: motion cueing limits, environmental-disturbance realism, system degradation breadth, novel-scenario branching capacity; research on skill transfer from simulator scenarios to novel line scenarios; pilot feedback on simulator-to-line transition difficulty and realism gaps.',
    'If fidelity limits are fundamental and create competence gaps, the simulation-adequacy reading is false even under current mandate intensity. If fidelity can be improved to close gaps, the reading remains viable. If fidelity improvements are technically possible but economically prohibitive, extraction rises (the mandate persists below competence optimality because vendors cannot afford the improvement investment).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(simulation_fidelity_frontier, empirical, 'Whether high-fidelity simulation has engineering-based limits that foreclose adequate exercise in some domains.').

omega_variable(
    kernel_contest_logical_structure,
    'Does this reading (simulation-as-adequate) foreclose the catastrophe-anchor reading, coexist with it, or influence it? Does it foreclose hybrid-dependency or influence it?',
    'Logical analysis of the axioms: if simulation-adequacy is true, can catastrophe-necessity be true in the same framework? If simulation alone maintains competence, does that logically exclude the claim that real-world anchoring is necessary? Or do the readings occupy different parties'' commitments and represent a genuine institutional contest?',
    'If this reading forecloses the others, one reading is logically true and the others are false by construction. If coexistence, the readings represent a real institutional debate with empirical consequences. If influence (this reading creates pressure that shapes others'' viability), the kernel contest is asymmetric in ways that favor the simulation reading''s institutionalization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_logical_structure, conceptual, 'Logical structure of relationships among the three competence-requirement readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__simulation_as_adequate_exercise, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t5, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(comp_tr_t5, observed).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(comp_tr_t10, observed).
narrative_ontology:measurement(comp_tr_t15, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(comp_tr_t15, observed).
narrative_ontology:measurement(comp_tr_t25, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(comp_tr_t25, observed).
narrative_ontology:measurement(comp_tr_t35, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(comp_tr_t35, observed).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(comp_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t5, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 5, 0.55).
narrative_ontology:measurement_basis(comp_be_t5, observed).
narrative_ontology:measurement(comp_be_t10, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(comp_be_t10, observed).
narrative_ontology:measurement(comp_be_t15, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(comp_be_t15, observed).
narrative_ontology:measurement(comp_be_t25, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(comp_be_t25, observed).
narrative_ontology:measurement(comp_be_t35, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(comp_be_t35, observed).
narrative_ontology:measurement(comp_be_t40, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(comp_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(comp_su_t0, observed).
narrative_ontology:measurement(comp_su_t5, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 5, 0.48).
narrative_ontology:measurement_basis(comp_su_t5, observed).
narrative_ontology:measurement(comp_su_t10, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 10, 0.5).
narrative_ontology:measurement_basis(comp_su_t10, observed).
narrative_ontology:measurement(comp_su_t15, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 15, 0.52).
narrative_ontology:measurement_basis(comp_su_t15, observed).
narrative_ontology:measurement(comp_su_t25, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 25, 0.54).
narrative_ontology:measurement_basis(comp_su_t25, observed).
narrative_ontology:measurement(comp_su_t35, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 35, 0.54).
narrative_ontology:measurement_basis(comp_su_t35, observed).
narrative_ontology:measurement(comp_su_t40, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 40, 0.54).
narrative_ontology:measurement_basis(comp_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__simulation_as_adequate_exercise, resource_allocation).
narrative_ontology:boltzmann_floor_override(competence_exercise_requirement__simulation_as_adequate_exercise, 0.12).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement__catastrophe_as_necessary_anchor).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement__hybrid_dependency).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the competence_exercise_requirement kernel. The kernel itself is a stabilized commitment: 'pilot competence must be continuously exercised and validated.' Three readings instantiate structurally distinct constraints from this kernel: (1) simulation_as_adequate_exercise (THIS story): fidelity + debriefing suffices; decades-without-incidents validates the reading. (2) catastrophe_as_necessary_anchor: only real catastrophic events provide irreducible exercise; simulation is a false positive. (3) hybrid_dependency: simulation necessary but insufficient; real-world anchoring also required. Each reading has different beneficiary/victim structures, different omegas, and different empirical falsification paths. The readings coexist institutionally (different regulatory jurisdictions, different airline policies, different research schools). This story does not claim to resolve the contest—it instantiates one reading with honest metrics and structural data. The engine's per-seat computation may diverge from the reading's claim, which is diagnostic: disagreement signals a reading whose empirical support is weaker than the institutional commitment to it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
