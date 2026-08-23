% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__hybrid_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__hybrid_dependency, []).

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
 *   constraint_id: competence_exercise_requirement__hybrid_dependency
 *   human_readable: Hybrid Competence Exercise Requirement: Simulation Foundation Plus Periodic Real-World Anchoring
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   Civil aviation certification requires that crew competence be maintained
 *   through a hybrid regimen: a simulator foundation (mandatory device hours,
 *   LOFT, emergency and upset rehearsal) PLUS periodic real-world anchoring
 *   (line checks, non-jeopardy line audits such as LOSA, and logged
 *   actual-aircraft time). This story instantiates ONE reading of the
 *   competence_exercise_requirement kernel — hybrid_dependency — as a clean,
 *   epsilon-invariant constraint per DP-001: simulation is necessary but
 *   insufficient, and the line-contact components bridge the fidelity gap.
 *   The sibling readings are separate constraint files:
 *   competence_exercise_requirement__simulation_as_adequate_exercise
 *   (high-fidelity simulation plus debriefing constitutes adequate exercise)
 *   and competence_exercise_requirement__catastrophe_as_necessary_anchor
 *   (only real catastrophic events or near-misses provide irreducible
 *   exercise). All three decompose the colloquial label 'how pilots stay
 *   competent,' which conflates three structurally distinct claims with
 *   different epsilon values over the SAME standing referent (the existing
 *   certification arrangement, assessed by each reading's own lights): this
 *   reading authors moderate extraction (0.46) reflecting genuine safety
 *   production beneath a thickening prescriptive layer; the sim-adequate
 *   sibling would author the line-component cost stratum as redundant
 *   overhead on that referent; the catastrophe sibling would treat the regime
 *   as suppressing the only true anchor, with a different victim set. The
 *   claim/metric relationship is deliberately unreconciled: claimed_type is
 *   authored from structural belief, metrics from descriptive belief, and the
 *   engine computes per-seat classifications.
 *
 * KEY AGENTS:
 *   - civil_aviation_authorities: agenda setter (institutional/identity_locked) — writes and enforces the exercise-mix rules; its budget and mandate scale with the oversight the rules generate
 *   - major_airlines: primary payer with secondary benefit (institutional/constrained) — funds the dual-track apparatus at scale, gains safety standing and insurability
 *   - regional_and_low_cost_carriers: concentrated cost bearer (organized/trapped) — identical requirements on thin margins; cannot exit the certificated market
 *   - flight_crews: exercised population (organized/constrained) — supplies the time and jeopardy exposure, depends on the competence maintained
 *   - full_flight_simulator_manufacturers: equipment-mandate beneficiary (institutional/arbitrage) — metered revenue from every mandated device hour
 *   - training_compliance_industry: prescriptive-layer recipient (organized/arbitrage) — bills each added form, audit cycle, and mandated module to captive customers
 *   - flying_public: diffuse beneficiary (powerless/mobile) — receives the accident-rate record, pays indirectly via fares
 *   - aviation_insurers: pricing observer with benefit (institutional/arbitrage) — converts the safety record into premium structures and holds a quiet veto
 *   - ebt_and_sim_adequacy_advocates: excluded voice (moderate/constrained) — argues the line layer outlives its evidence; kept out of binding text
 *   - hro_academic_observers: analytical observer (analytical/analytical) — studies the regime without collecting or paying
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__hybrid_dependency, 0.46).
domain_priors:suppression_score(competence_exercise_requirement__hybrid_dependency, 0.33).
domain_priors:theater_ratio(competence_exercise_requirement__hybrid_dependency, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, extractiveness, 0.46).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0.33).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, accessibility_collapse, 0.24).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__hybrid_dependency, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__hybrid_dependency, "Hybrid Competence Exercise Requirement: Simulation Foundation Plus Periodic Real-World Anchoring").
narrative_ontology:topic_domain(competence_exercise_requirement__hybrid_dependency, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_exercise_requirement__hybrid_dependency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__hybrid_dependency, '4c6fcf5a-b534-4987-ae68-6e10da651467').
narrative_ontology:cs_kernel_codification('4c6fcf5a-b534-4987-ae68-6e10da651467', formalized).
narrative_ontology:cs_authority_grounding('4c6fcf5a-b534-4987-ae68-6e10da651467', expertise).
narrative_ontology:cs_interpretation_layer_present('4c6fcf5a-b534-4987-ae68-6e10da651467').
narrative_ontology:cs_reading_relation('4c6fcf5a-b534-4987-ae68-6e10da651467', competence_exercise_requirement__simulation_as_adequate_exercise, forecloses).
narrative_ontology:cs_reading_relation('4c6fcf5a-b534-4987-ae68-6e10da651467', competence_exercise_requirement__catastrophe_as_necessary_anchor, forecloses).
narrative_ontology:cs_axiom('4c6fcf5a-b534-4987-ae68-6e10da651467', foundational, simulation_necessary_but_insufficient_for_competence).
narrative_ontology:cs_axiom_status(simulation_necessary_but_insufficient_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('4c6fcf5a-b534-4987-ae68-6e10da651467', simulation_necessary_but_insufficient_for_competence, empirically_contingent).
narrative_ontology:cs_axiom('4c6fcf5a-b534-4987-ae68-6e10da651467', foundational, non_jeopardy_real_world_anchoring_supplies_residual_exercise).
narrative_ontology:cs_axiom_status(non_jeopardy_real_world_anchoring_supplies_residual_exercise, holdable).
narrative_ontology:cs_axiom_grounding('4c6fcf5a-b534-4987-ae68-6e10da651467', non_jeopardy_real_world_anchoring_supplies_residual_exercise, empirically_contingent).
narrative_ontology:cs_axiom('4c6fcf5a-b534-4987-ae68-6e10da651467', secondary, hybrid_regime_minimizes_total_competence_risk).
narrative_ontology:cs_axiom_status(hybrid_regime_minimizes_total_competence_risk, holdable).
narrative_ontology:cs_axiom_grounding('4c6fcf5a-b534-4987-ae68-6e10da651467', hybrid_regime_minimizes_total_competence_risk, instrumental).
narrative_ontology:cs_reference_frame('4c6fcf5a-b534-4987-ae68-6e10da651467', dual_track_competence_maintenance).
narrative_ontology:cs_drift_state('4c6fcf5a-b534-4987-ae68-6e10da651467', contemporary_post_waiver_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('4c6fcf5a-b534-4987-ae68-6e10da651467', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, flying_public).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, civil_aviation_authorities).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, full_flight_simulator_manufacturers).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, training_compliance_industry).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, aviation_insurers).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, major_airlines).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, regional_and_low_cost_carriers).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, flight_crews).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, major_airlines).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, flight_crews).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues and enforces the certification rules spelling out how many simulator sessions, line checks, non-jeopardy audits, and logged aircraft hours a crew needs to stay qualified. Staffs inspector corps, runs oversight programs, and publishes advisory material interpreting the standard. Budget and headcount scale with the oversight workload the rules generate, and the authority's mandate and international standing rest on continuously administering this machinery; stepping back from it would dissolve the function the institution has become. Harmonization obligations tie deviations to bilateral-agreement friction.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, civil_aviation_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Operates the fleets whose crews cycle through simulator centers, check rides, and audit programs. Funds the dual-track apparatus directly — simulator leases, instructor and check-airman payroll, records systems — while drawing the safety record, insurance terms, and certificated route authority the system sustains. Scale lets it spread fixed compliance cost and place staff on the committees drafting the standards. Leaving the certificated market would forfeit its entire operating basis, so it works the margins of the rule text rather than exiting.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, major_airlines, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__hybrid_dependency, major_airlines, beneficiary).

% Carries identical certification requirements on thin margins and smaller fleets, so fixed training, auditing, and records costs weigh far heavier per departure. Cannot absorb the dual-track expense easily, cannot pass all of it through fares on price-sensitive routes, and cannot leave the certificated market it exists to serve. Consolidation, training outsourcing, and lobbying through trade associations are the realistic adjustments available.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, regional_and_low_cost_carriers, payer,
    organized, biographical, trapped, regional).

% Spends recurring weeks in simulators, flies check rides with careers riding on the outcome, hosts audit observers on revenue flights, and logs the aircraft hours the rules count. Unionized, crews negotiate the cadence and consequence rules of the checking system. Their licenses, incomes, and physical safety depend on the competence the regimen maintains, making them simultaneously contributors to and dependents of it. Leaving the profession forfeits seniority, type ratings, and sunk training investment.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, flight_crews, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__hybrid_dependency, flight_crews, beneficiary).

% Boards flights whose crews it cannot inspect, relying on the certification system as an invisible guarantee. Pays the regimen's costs indirectly inside ticket prices and receives the accident-rate record the system helps sustain. Individual passengers have no channel into rulemaking; their protection is mediated entirely by regulators and insurers. Mode substitution is available mainly on short-haul routes.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, flying_public, beneficiary,
    powerless, immediate, mobile, global).

% Builds and sells the full-flight simulator devices, updates, and service contracts that certification rules require operators to buy and run by the hour. Every mandated simulator session is metered demand for hardware and support. Order books track the credited-hours language of the rule text, and the manufacturers' engineering input shapes fidelity standards from inside the working groups.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, full_flight_simulator_manufacturers, beneficiary,
    institutional, generational, arbitrage, global).

% Approved training organizations, audit consultancies, records-platform vendors, and contract check-instructor firms selling the courses, audits, documentation systems, and personnel the recurring requirements call for. Revenue scales with the prescriptive layer — each added form, audit cycle, or mandated module is billable. Customers are effectively captive: operators must purchase from approved providers to remain certificated, and providers can relocate to wherever the rules are strictest.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, training_compliance_industry, beneficiary,
    organized, biographical, arbitrage, global).

% Prices hull and liability cover against the accident record the training regimen helps produce, and audits operator training programs as a condition of terms. Observes the system closely enough to shift premiums or withdraw capacity when competence signals degrade, exercising a quiet veto that no passenger or crew holds. Capital reallocates freely across risks.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, aviation_insurers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__hybrid_dependency, aviation_insurers, observer).

% Human-factors researchers, evidence-based-training specialists, and some operator training managers arguing that modern devices plus structured debriefing deliver most of what the line-based components deliver, at lower cost and risk. They publish, sit on advisory panels, and file docket comments, but standing rule text and committee gatekeeping keep their proposals out of binding requirements; their influence arrives mainly after accidents reopen the question. Their access channels are limited and slow.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, ebt_and_sim_adequacy_advocates, excluded,
    moderate, biographical, constrained, global).

% Researchers of high-reliability organizations studying the regimen as a case in institutional learning: how rare-event competence is kept alive, how announced audits reshape behavior, how rules accrete after accidents. They take no side in operations, carry none of the costs, and collect none of the fees; their seat is the written record.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, hro_academic_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__hybrid_dependency, training_compliance_industry).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__hybrid_dependency, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sets a common, verifiable floor for crew competence maintenance across competing operators: every certificated crew rehearses rare and emergency events that line service cannot safely provide on demand (simulator volume), and periodically re-contacts the uncontrolled operational environment whose residual variables no device fully reproduces (line checks, non-jeopardy audits, logged aircraft time). Prevents a training-investment race to the bottom in which each operator's cheapest acceptable program exports risk onto the whole system.
% TRANSFER_FUNCTION: Moves operator revenue and crew time into the certification apparatus: simulator lease hours and instructor labor flow to training providers and device makers; audit and documentation labor flows to compliance vendors and regulator inspectorates; fare pass-through spreads the residual cost to passengers. In return, operators receive certificates and insurability, insurers receive a ratable population, and the public receives the accident-rate record.
% ABSENT_VOICES: Evidence-based-training and simulator-adequacy advocates argue the line-hour and audit components outlive their demonstrated marginal value; their docket comments reach rulemaking but rarely the binding text. Line pilots experiencing announced non-jeopardy audits as behavior-shaping decline candid reporting, entering the record only anonymized. Operators in cost-constrained markets and aspiring pilots priced toward the profession's margins hold no committee seat. Veterans holding that only real events truly teach speak in hangar folklore, not dockets.
% DISAPPEARANCE_RATIONALE: Training economies would reorganize around whichever pole the surviving actors preferred: operators would expand simulator credit and shed line-based cost until an accident cluster or insurer repricing forced re-anchoring; simulator manufacturers would accelerate fidelity roadmaps; regulator inspectorates would shrink or refocus. The 2020-2021 waiver episode previews the drift — suspending enforcement immediately shifted behavior toward sim-only compliance, and restoration required dedicated re-verification campaigns. Demonstrably, arrangements depend on the requirement persisting.
% FOUNDING_PROBLEM: Jet-age operations confronted crews with emergencies too rare and too dangerous to practice in service: ordinary line flying never exercises an engine failure at V1 or a stall recovery, and deliberately staging such events in live aircraft kills people. Early simulator optimism collided with accidents traceable to skill fade and automation complacency, forcing the question of what combination of synthetic rehearsal and real-environment contact actually maintains competence.
% FOUNDING_PROBLEM_CORROBORATION: Accident-investigation findings from outside the benefiting parties attest the problem remains open: the NTSB's Colgan Air 3407 report documents manual-handling and stall-recovery deficits despite recurrent simulator compliance, and the BEA's AF447 final report attributes crew-response breakdown to degraded manual-flying practice under automation reliance. Peer-reviewed skill-decay literature and post-waiver unstable-approach statistics corroborate independently — notably from constituencies that dispute the arrangement's current shape, since simulator manufacturers reject the insufficiency claim and training vendors profit regardless of the answer.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__hybrid_dependency, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__hybrid_dependency, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__hybrid_dependency, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_requirement__hybrid_dependency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__hybrid_dependency, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__hybrid_dependency_tests).
:- end_tests(competence_exercise_requirement__hybrid_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are independent authored facts. I claim tangled_rope because the arrangement must possess both a genuine coordination function (a common competence floor preventing training-investment races to the bottom; rare-event rehearsal that no operator can safely self-provide) and asymmetric extraction (a prescriptive compliance layer whose costs concentrate on trapped carriers and crews while revenue meters to vendors and regulator headcount), held together by active enforcement (certificates, check consequences, market-access conditionality). Descriptively: extractiveness 0.46 — real safety value produced, with a compliance stratum growing faster than demonstrated safety yield (series rises 0.26 to 0.46 across the interval). Suppression 0.33 is a raw structural property, unscaled by power or scope: the coercive mechanism is certificate suspension and market exclusion, not force; the measurement series traces enforcement CAPACITY (the story's tracked enforcement dynamic) including the 2020 waiver collapse to 0.22 and the post-waiver re-verification rebound to 0.41 before settling — authorization for authoring suppression_requirement rather than leaving the static scalar alone. Theater_ratio 0.29: announced audits reshape observed behavior, check rides get flown to published profiles, and the waiver period spiked paper-only compliance (0.40) while the functional core — detecting real degradation — persisted. Accessibility_collapse 0.24 is LOW because alternatives are conspicuously alive: this kernel is openly contested, competency-based programs operate as sanctioned variants, and sim-credit expansion proceeds incrementally — nobody who understands the arrangement finds the alternatives unthinkable. Resistance 0.35: sustained lobbying for simulator credit, regional-carrier cost complaints, slow institutional absorption of EBT. All three metrics feed the engine; certification is the engine's call.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute divergent types from identical structure. From the agenda-setter seat the arrangement is stewardship it built and administers; from the trapped regional-carrier seat it is a fixed-cost wall sized for someone else's balance sheet; from the vendor seats it is metered demand whose every rule addition is billable; from the crew seat it is simultaneously career jeopardy and personal survival insurance. The crew seat is the sharpest dual case: the derivation reads flight_crews in the victims array (high target-directionality) while their survival and licensure stakes pull materially toward benefit — the engine resolves this per seat rather than by authorial decree. Coalition note: regional carriers hold little individual leverage, but trade-association coalitions have moved docket outcomes, which caps how far their effective powerlessness extends. Insurers hold more practical veto power over training policy than any passenger ever will, despite holding no agenda-setter role.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real collection points: the flying public collects the accident-rate record diffusely; authorities collect budget, headcount, and mandate; simulator manufacturers collect metered device demand with arbitrage-grade exit pushing them toward the beneficiary pole; insurers collect a ratable population. Victim declarations map to the cost-bearing seats: major airlines (large absolute transfers, partially offset by standing gains — hence secondary_role beneficiary), regional carriers (highest relative burden, zero exit), crews (time, jeopardy exposure, sunk-career lock). The receipt surface is narrower than the benefit surface: gain_flow names training_compliance_industry because the recurring, metered extraction stream — audit cadence, documentation platforms, mandated courseware, contract check-instructors — bills through that complex, whereas insurers and the public benefit WITHOUT receiving the extraction and authorities collect appropriations rather than accrued gain. Receipt is not benefit; the distinction is load-bearing here. No directionality overrides were authored: the derivation chain from beneficiary/victim declarations plus exit options produces the right directional structure, and the dual-positioned agents are handled through secondary_role rather than override arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: rare-event rehearsal remains unsolvable at either pole — pure real-world exercise is ethically and practically impossible (staging V1 cuts in service kills people), and the post-waiver record keeps the pure-simulation fragility question empirically open. The R5 mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges: aligned, no zombie flag. Mandatrophy discipline prevents two opposite mislabels: calling the whole regimen pure coordination would hide the vendor-rent stratum accumulating on top (visible in the rising extraction series); calling it pure extraction would erase the demonstrated anchoring function the waiver natural experiment tested. The forward risk is atrophy rather than capture: if competency-based conversion succeeds broadly, the prescriptive documentation layer could outlive its function and slide toward piton character with a rising theater_ratio — the tracked series is the early-warning instrument for that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Which reading of the competence_exercise_requirement kernel governs certification architecture: this hybrid_dependency reading, simulation_as_adequate_exercise, or catastrophe_as_necessary_anchor?',
    'Rulemaking dockets on simulator-credit expansion and evidence-based-training conversion; ICAO Doc 9868 revision cycles; post-accident rule responses.',
    'Adoption of the sim-adequate sibling would strip the real-world-anchoring cost layer and its associated extraction from the standing arrangement; adoption of the catastrophe sibling would raise suppression sharply and convert crews and operations into deliberate exercise subjects. Each sibling is a separate constraint file with its own epsilon and victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame routing: this story is one reading of the competence_exercise_requirement kernel; sibling readings are structurally distinct constraints.').

omega_variable(
    simulator_transfer_gap_closure,
    'Does advancing simulator fidelity plus structured debriefing close the residual competence gap that periodic real-world anchoring currently covers?',
    'Controlled comparisons of hybrid-regime versus enhanced-simulation cohorts on manual-handling and upset-recovery metrics; longitudinal tracking of post-2020 waiver cohorts trained under reduced line contact.',
    'If the gap closes, this reading''s foundational insufficiency axiom erodes and the line-component cost layer becomes pure overhead on the same referent, raising effective extraction; if the gap persists, the anchoring mandate hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulator_transfer_gap_closure, empirical, 'Whether the fidelity frontier eliminates the need for real-world anchoring.').

omega_variable(
    waiver_period_degradation_signal,
    'Did the 2020-2021 waiver period, during which line flying collapsed and oversight was suspended, produce measurable competence degradation relative to the prior hybrid baseline?',
    'Unstable-approach rate series, line-audit finding trends 2021-2025, and insurance loss curves for the affected cohort.',
    'An affirmative degradation signal empirically certifies the anchoring-necessity claim at the heart of this reading; a null result arms the sim-adequate sibling reading with its strongest natural experiment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waiver_period_degradation_signal, empirical, 'Natural-experiment test of the hybrid dependency claim via the pandemic waiver episode.').

omega_variable(
    prescriptive_layer_cost_allocation,
    'What share of the compliance layer''s cost purchases safety-relevant exercise versus documentation and audit rent?',
    'Cost-effectiveness comparison of legacy hours-based programs against competency-based conversions at equivalent measured safety output.',
    'A high rent share validates the extraction limb of the tangled_rope structure and confirms training_compliance_industry as the receipt seat; a low rent share pushes the classification toward pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prescriptive_layer_cost_allocation, empirical, 'Decomposition of compliance cost into exercise value versus rent.').

omega_variable(
    authority_grounding_framing,
    'Is the certification authority''s legitimacy grounded in technical expertise that adjudicates evidence, or in accident-lineage continuity (rules written after hull losses)?',
    'Compare rule-revision latency after near-miss and research findings against revision latency after fatal accidents; examine whether preventive proposals reach binding text without a triggering event.',
    'Under a lineage framing, the drift_state reads as authority_erosion rather than practice_drift, and the catastrophe_as_necessary_anchor sibling gains structural appeal because accident lineage is its native evidence class.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'CS-framing under-determination: expertise versus lineage grounding of the certification authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__hybrid_dependency, 1994, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(competence_hybrid_exercise_tr_t1994, competence_exercise_requirement__hybrid_dependency, theater_ratio, 1994, 0.2).
narrative_ontology:measurement(competence_hybrid_exercise_tr_t2001, competence_exercise_requirement__hybrid_dependency, theater_ratio, 2001, 0.24).
narrative_ontology:measurement(competence_hybrid_exercise_tr_t2009, competence_exercise_requirement__hybrid_dependency, theater_ratio, 2009, 0.3).
narrative_ontology:measurement(competence_hybrid_exercise_tr_t2015, competence_exercise_requirement__hybrid_dependency, theater_ratio, 2015, 0.34).
narrative_ontology:measurement(competence_hybrid_exercise_tr_t2020, competence_exercise_requirement__hybrid_dependency, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(competence_hybrid_exercise_tr_t2023, competence_exercise_requirement__hybrid_dependency, theater_ratio, 2023, 0.31).
narrative_ontology:measurement(competence_hybrid_exercise_tr_t2026, competence_exercise_requirement__hybrid_dependency, theater_ratio, 2026, 0.29).

% Extraction over time
narrative_ontology:measurement(competence_hybrid_exercise_be_t1994, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 1994, 0.26).
narrative_ontology:measurement(competence_hybrid_exercise_be_t2001, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 2001, 0.3).
narrative_ontology:measurement(competence_hybrid_exercise_be_t2009, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 2009, 0.36).
narrative_ontology:measurement(competence_hybrid_exercise_be_t2015, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(competence_hybrid_exercise_be_t2020, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 2020, 0.43).
narrative_ontology:measurement(competence_hybrid_exercise_be_t2023, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 2023, 0.45).
narrative_ontology:measurement(competence_hybrid_exercise_be_t2026, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 2026, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(competence_hybrid_exercise_su_t1994, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 1994, 0.3).
narrative_ontology:measurement(competence_hybrid_exercise_su_t2001, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 2001, 0.34).
narrative_ontology:measurement(competence_hybrid_exercise_su_t2009, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 2009, 0.38).
narrative_ontology:measurement(competence_hybrid_exercise_su_t2015, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 2015, 0.37).
narrative_ontology:measurement(competence_hybrid_exercise_su_t2020, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 2020, 0.22).
narrative_ontology:measurement(competence_hybrid_exercise_su_t2023, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 2023, 0.41).
narrative_ontology:measurement(competence_hybrid_exercise_su_t2026, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 2026, 0.33).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__hybrid_dependency, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement__simulation_as_adequate_exercise).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement__catastrophe_as_necessary_anchor).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'competence maintenance for rare events' conflates three structurally distinct claims — (1) simulation provides necessary foundation (upstream, high-confidence, low extraction), (2) simulation alone is sufficient (contested downstream, would remove the line-cost stratum), (3) catastrophe is the necessary anchor (contrarian downstream, different victim set and much higher suppression). This file authors claim (1)+(2)-negation: the hybrid requirement as a standing institutional arrangement. Family members are linked via affects_constraints so contamination propagation and cross-reading foreclosure analysis operate on the connected graph.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
