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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: competence_occupation__simulation_sufficiency
 *   human_readable: Simulation-Sufficiency Doctrine for Competence Maintenance
 *   domain: organizational/safety-training
 *
 * SUMMARY:
 *   Across high-reliability domains — aviation, anesthesiology and surgery,
 *   nuclear control rooms, offshore operations — the doctrine that scheduled
 *   simulation-based drills sufficiently occupy the competence kernel has
 *   become the operative standard for maintaining rare-event readiness. Under
 *   this reading, training compliance is the observable: a crew is current
 *   when its drill file is complete, and skill decay is treated as an
 *   engineering parameter tuned through drill frequency and scenario
 *   fidelity. The claim is genuinely useful — simulation is the only ethical
 *   way to rehearse catastrophe — and genuinely incomplete: a growing
 *   research record documents transfer limits and decay profiles that drill
 *   compliance cannot see. This file instantiates the simulation_sufficiency
 *   reading of the competence_occupation kernel as a clean, epsilon-invariant
 *   constraint; the sibling readings (real_incident_necessity,
 *   hybrid_occupation) are separate stories linked through
 *   network.affects_constraints and are neither described nor averaged inside
 *   this one. Claimed type and metrics are authored independently: the
 *   constraint is claimed as tangled_rope, and the metrics describe its
 *   actual operation without being tuned toward any predicted engine verdict.
 *
 * KEY AGENTS:
 *   - safety_regulators: Agenda setter (institutional/analytical) — mandates recurrent simulation recertification and accepts drill-compliance files as evidence of currency
 *   - simulation_training_industry: Primary beneficiary (powerful/arbitrage) — collects procurement, licensing, courseware, and instructor-certification revenue that scales with the mandate
 *   - safety_training_departments: Internal administrator and incidental beneficiary (organized/identity_locked) — runs the drill program its own budget and career ladders depend on
 *   - frontline_operators: Primary target (moderate/constrained) — surrenders duty and rest hours to mandatory drills and bears unmeasured decay risk
 *   - service_end_users: Silent target (powerless/trapped) — passengers, patients, and plant neighbors carrying residual risk invisible to the compliance record
 *   - liability_underwriters: Secondary beneficiary (powerful/mobile) — prices risk off the compliance artifact and can re-price or withdraw if its credibility fails
 *   - safety_science_researchers: Excluded voice (moderate/analytical) — documents transfer limits and decay heterogeneity outside the standard-setting rooms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, 0.66).
domain_priors:suppression_score(competence_occupation__simulation_sufficiency, 0.58).
domain_priors:theater_ratio(competence_occupation__simulation_sufficiency, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, extractiveness, 0.66).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__simulation_sufficiency, tangled_rope).
narrative_ontology:human_readable(competence_occupation__simulation_sufficiency, "Simulation-Sufficiency Doctrine for Competence Maintenance").
narrative_ontology:topic_domain(competence_occupation__simulation_sufficiency, "organizational/safety-training").

domain_priors:requires_active_enforcement(competence_occupation__simulation_sufficiency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__simulation_sufficiency, '35b7cc37-1867-4b0f-b4b7-c0ab258d860d').
narrative_ontology:cs_kernel_codification('35b7cc37-1867-4b0f-b4b7-c0ab258d860d', formalized).
narrative_ontology:cs_authority_grounding('35b7cc37-1867-4b0f-b4b7-c0ab258d860d', expertise).
narrative_ontology:cs_interpretation_layer_present('35b7cc37-1867-4b0f-b4b7-c0ab258d860d').
narrative_ontology:cs_reading_relation('35b7cc37-1867-4b0f-b4b7-c0ab258d860d', competence_occupation__real_incident_necessity, forecloses).
narrative_ontology:cs_reading_relation('35b7cc37-1867-4b0f-b4b7-c0ab258d860d', competence_occupation__hybrid_occupation, forecloses).
narrative_ontology:cs_axiom('35b7cc37-1867-4b0f-b4b7-c0ab258d860d', foundational, simulated_rehearsal_transfers_to_operational_competence).
narrative_ontology:cs_axiom_status(simulated_rehearsal_transfers_to_operational_competence, holdable).
narrative_ontology:cs_axiom_grounding('35b7cc37-1867-4b0f-b4b7-c0ab258d860d', simulated_rehearsal_transfers_to_operational_competence, empirically_contingent).
narrative_ontology:cs_axiom('35b7cc37-1867-4b0f-b4b7-c0ab258d860d', secondary, decay_gap_closed_by_frequency_fidelity_optimization).
narrative_ontology:cs_axiom_status(decay_gap_closed_by_frequency_fidelity_optimization, holdable).
narrative_ontology:cs_axiom_grounding('35b7cc37-1867-4b0f-b4b7-c0ab258d860d', decay_gap_closed_by_frequency_fidelity_optimization, instrumental).
narrative_ontology:cs_reference_frame('35b7cc37-1867-4b0f-b4b7-c0ab258d860d', drill_compliance_as_competence_standard).
narrative_ontology:cs_drift_state('35b7cc37-1867-4b0f-b4b7-c0ab258d860d', contemporary_post_decay_literature, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('35b7cc37-1867-4b0f-b4b7-c0ab258d860d', '').
narrative_ontology:cs_kernel_id(competence_occupation__simulation_sufficiency, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, safety_regulators).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, simulation_training_industry).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, safety_training_departments).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, liability_underwriters).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, service_end_users).
narrative_ontology:constraint_vindicates(competence_occupation__simulation_sufficiency, scheduled_rehearsal_prevents_skill_decay).
narrative_ontology:constraint_vindicates(competence_occupation__simulation_sufficiency, drill_compliance_evidences_operational_currency).
narrative_ontology:constraint_vindicates(competence_occupation__simulation_sufficiency, skill_decay_is_frequency_fidelity_optimizable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates recurrent simulation-based recertification for licensed operators, publishes drill-hour and scenario standards, and audits completion records. Accepts a training file as evidence that a crew remains current. Gains a uniform, inspectable artifact that replaces case-by-case competence judgment; also inherits the investigation burden and public blame when an organization with a perfect drill record is involved in a decay-linked event.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, safety_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Manufactures and operates simulators, sells courseware, certifies instructors, and books recurring refresher contracts. Revenue scales with mandated drill hours and with each tightening of recurrency standards. Its product line can be redirected to whichever training modality future standards favor, so its commercial position survives changes in doctrine even though current sales depend on this one.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, simulation_training_industry, beneficiary,
    powerful, generational, arbitrage, global).

% Schedules, delivers, and documents the internal drill program; manages simulator procurement and instructor staffing. Department headcount, budget, and seniority ladders are built around the simulator curriculum, and training officers' professional identities are bound to the program they run. When a drill schedule slips or an incident raises readiness questions, the department absorbs the internal criticism and the remediation workload.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, safety_training_departments, agenda_setter,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__simulation_sufficiency, safety_training_departments, beneficiary).

% Pilots, clinicians, control-room crews, and similar licensed operators surrender scheduled duty and rest hours to mandatory drills. They gain rehearsal of rare, high-consequence scenarios they could not safely practice live, and they lose the varied, unscripted operational experience the drills replace when training time is finite. Opting out is unavailable; moving to another employer lands them in the same industry-wide recurrency regime.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, frontline_operators, payer,
    moderate, biographical, constrained, global).

% Passengers, patients, and plant-neighborhood residents depend on operators whose competence is maintained — or not — behind a compliance record they cannot read. They bear the consequences of any gap between what the drills measured and what the situation demanded. Their only exits are avoiding the service altogether or accepting the risk; they cannot select operators by verified competence.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, service_end_users, payer,
    powerless, immediate, trapped, regional).

% Prices premiums and sets reserve requirements using documented drill compliance as a legible proxy for operational risk. The proxy lowers assessment costs; if its credibility collapses, underwriters can re-price or withdraw from lines faster than regulated operators can change training systems.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, liability_underwriters, beneficiary,
    powerful, biographical, mobile, global).

% Publishes findings on skill-decay rates, simulation-to-live transfer limits, and the heterogeneity of decay across skill classes. Results circulate in journals and conference panels but rarely reach the standard-setting committees that fix recurrency requirements; participation in those rooms is limited to regulators, operators, and vendors.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, safety_science_researchers, excluded,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__simulation_sufficiency, simulation_training_industry).
narrative_ontology:fixing_cost_class(competence_occupation__simulation_sufficiency, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of exercising rare, high-consequence skills safely and uniformly: simulation lets crews rehearse catastrophic scenarios that cannot be staged live, on a schedule every site and shift can meet, to a standard every auditor can check. Shared drill standards also coordinate expectations among regulator, operator, insurer, and crew about what being current means.
% TRANSFER_FUNCTION: Moves training-budget money from operating organizations to simulator vendors, courseware providers, and instructor-certification chains; moves scheduled hours from frontline crews' production and rest time into scripted drill scenarios; and converts open-ended competence uncertainty into a closed, filed compliance artifact that shifts accountability from the organization's operational judgment to its training paperwork.
% ABSENT_VOICES: Safety-science researchers who document transfer limits and decay heterogeneity would object that the sufficiency claim outruns the evidence; they sit outside the standard-setting rooms. Frontline veterans who report that predictable scripted drills stop producing adaptation are heard as gripes, not data. Future victims of decay-linked failures at fully compliant organizations are absent by definition — they exist only after the failure the record said was prevented.
% DISAPPEARANCE_RATIONALE: If the sufficiency doctrine and its mandated drill regime vanished overnight, training budgets would reroute toward mixed programs combining simulation with line audits, supervised live exposure, and procedural reinforcement; simulator procurement would slow; regulators would need a new observable for currency; training departments would restructure around outcome validation. The underlying need — keeping rarely-used, high-consequence skills exercised — would not disappear, so the world reorganizes rather than simply reverting.
% FOUNDING_PROBLEM: Skills for rare, catastrophic events decay between uses, and those events cannot be ethically or practically rehearsed live; organizations needed a safe, schedulable, verifiable way to keep emergency competencies occupied.
% FOUNDING_PROBLEM_CORROBORATION: Accident-investigation bodies and the peer-reviewed skill-decay literature attest from outside the beneficiary set that decay of infrequently used critical skills is real and ongoing. Notably, no source outside the beneficiary set attests that simulation alone resolves the problem — the sufficiency step, as opposed to the founding problem itself, is corroborated mainly by the training establishment and its suppliers.
narrative_ontology:disappearance_verdict(competence_occupation__simulation_sufficiency, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__simulation_sufficiency, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__simulation_sufficiency, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_occupation__simulation_sufficiency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__simulation_sufficiency, 0.66, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.66: converting a partial tool into a declared-complete substitute creates a measured-by-design gap between what compliance records show and what competence requires; revenue from closing that gap on paper accrues to identifiable seats while its risk lands on operators and end users. Suppression 0.58: persistence rests on mandated recurrency, audit consequences, and budget paths that crowd out hybrid alternatives — real coercive machinery, though short of banning dissent. Theater_ratio 0.48: a large and growing share of drill activity runs to satisfy the checklist — predictable scenarios, teaching-to-the-form, sign-off choreography — while well-run programs still deliver genuine rehearsal. Accessibility_collapse 0.52: once the doctrine is adopted, procurement and staffing paths collapse toward simulator-centered programs; hybrid options survive only where regulators or unions force them. Resistance 0.42: crew fatigue complaints, researcher critique, and post-incident inquiries are persistent but unorganized. Temporal shape: the series oscillate around a rising ratchet — adoption and ritualization (t0-12), a decay-linked accident triggering reform (t16: enforcement spikes to 0.62, theater dips as drills briefly re-functionalize, marginal rents are trimmed), then renewed accumulation (t20-36) with the enforcement machinery retained. Each reform cycle re-legitimizes the compliance observable, so the oscillation itself deepens long-run capture — intermittent reinforcement, not noise. base_properties were measured at t=36, the late-accumulation phase. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by the engine, from directionality and spatial scope.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator and vendor seats the arrangement computes as sound engineering: an auditable, continuously improvable system in which decay is a tunable parameter. From the frontline seat the same structure takes scheduled hours and returns scripted certainty — drills optimized for sign-off rather than surprise. From the end-user seat it is an invisible wager that a paperwork proxy tracks bodily risk. The training-department seat adds identity-lock dynamics: the department's budget, promotion ladder, and self-concept have fused with the simulator program ('we train, therefore we are safe'), so evidence against sufficiency is experienced as an attack on the department rather than as data; if that identity frame broke, the department could pivot to outcome-validation work and its computed position would move from defended beneficiary toward neutral administrator. End users cannot form an effective coalition because the harm presents as isolated human error rather than a systemic decay signature — each failure is narrated as an individual lapse, which is precisely what the compliance record makes credible.
 *
 * DIRECTIONALITY LOGIC:
 *   Vendors sit nearest the beneficiary pole: revenue scales with the mandate and their exit is arbitrage-grade (they sell the means, not the outcome). Liability underwriters benefit secondarily through cheaper risk assessment and remain mobile. Frontline operators sit near the target pole: they surrender time, bear decay risk, and retain only constrained exit, since the recurrency regime is industry-wide. End users sit at the full-target pole: no direct coordination benefit, full residual-risk bearing, trapped — exit means leaving the service. Two overrides correct derivations that would misplace dual-positioned seats. Safety regulators are declared beneficiaries (they collect the auditability good), so structural derivation would place them deep in subsidy (~0.15); the override to 0.45 reflects their near-symmetric true position: they gain a legible artifact but pay enforcement costs and inherit public blame when fully compliant organizations fail. Safety training departments derive as beneficiaries (~0.15) from budget dependence; the override to 0.32 reflects that they also administer the program, absorb incident fallout and workforce fatigue, and are identity-fused with its outcomes. The engine owns the chi arithmetic; these declarations only fix who stands where.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the arrangement as pure rope would erase the asymmetry: the sufficiency claim converts a real tool into a revenue stream and hides an unmeasured risk transfer behind a compliance artifact. Reading it as pure snare would erase the coordination: safe rehearsal of rare catastrophes is a genuine, hard-won capability that no serious alternative abolishes. Tangled rope holds both — crews and regulators are coordinated around a real rehearsal function while vendors, departments, and underwriters collect through the same structure that pushes residual risk onto operators and the public. On the R5 interview the founding problem (decay of rarely-used critical skills) is live and externally corroborated, so this is not a resolved mandatrophy and the mismatch consumer should find no zombie flag: status=live with verdict=world_rearranges. What has converted is the function's margin — from competence maintenance toward compliance production — which is the drift the measurement series records, not a dead mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint instantiates the simulation_sufficiency reading of the competence_occupation kernel; how would classification change under the sibling readings real_incident_necessity or hybrid_occupation?',
    'Cross-reading comparison of beneficiary/victim structure and epsilon under each reading''s own observable, authored as separate constraint stories and compared through the network edges.',
    'Under hybrid_occupation the drill-compliance observable loses primacy and gains redistribute across a multi-vendor training complex; under real_incident_necessity the entire compliance apparatus becomes performance around an exercise that cannot substitute for the real thing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Committer-frame routing: this story is one reading of a contested kernel, not the topic whole.').

omega_variable(
    compliance_competence_proxy_validity,
    'Does the drill-compliance record measure maintained competence, or merely completed drills?',
    'Validation studies correlating compliance records with blinded real-performance assessment and with decay-linked error rates across operators.',
    'If the proxy is weak, the authored extractiveness understates true risk transfer and the classification comes under pressure toward snare; if strong, the coordination reading gains ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_competence_proxy_validity, empirical, 'Whether the operative observable tracks the quantity it certifies.').

omega_variable(
    decay_regime_heterogeneity,
    'Does a single frequency/fidelity optimum exist across skill classes (motor, cognitive, team-coordination, rare compound emergencies), or do decay regimes differ so widely that no uniform drill schedule occupies them all?',
    'Stratified decay-curve studies by skill class, domain, and interval-since-last-use.',
    'If decay is strongly heterogeneous, the reading''s optimization promise fails for compound skills and the sufficiency claim narrows toward hybrid_occupation territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decay_regime_heterogeneity, empirical, 'Whether the engineering-optimization premise survives skill-class stratification.').

omega_variable(
    doctrine_origin_capture_direction,
    'Did the sufficiency doctrine originate in genuine engineering constraints (live rehearsal of catastrophe is unethical and impractical) with industry capture arriving afterward, or did supplier interests shape the standard from the outset?',
    'Standards-committee genealogy, procurement records, and lobbying archives contrasted with contemporaneous safety-science input that was or was not admitted to the drafting rooms.',
    'A capture-first genealogy raises effective extraction and supports snare-leaning review; a constraint-first genealogy supports tangled_rope with secondary capture layered on a real function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrine_origin_capture_direction, conceptual, 'Direction of causation between the doctrine and its principal commercial beneficiary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__simulation_sufficiency, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__simulation_sufficiency, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t4, competence_occupation__simulation_sufficiency, theater_ratio, 4, 0.22).
narrative_ontology:measurement_basis(comp_tr_t4, observed).
narrative_ontology:measurement(comp_tr_t8, competence_occupation__simulation_sufficiency, theater_ratio, 8, 0.3).
narrative_ontology:measurement_basis(comp_tr_t8, observed).
narrative_ontology:measurement(comp_tr_t12, competence_occupation__simulation_sufficiency, theater_ratio, 12, 0.37).
narrative_ontology:measurement_basis(comp_tr_t12, observed).
narrative_ontology:measurement(comp_tr_t16, competence_occupation__simulation_sufficiency, theater_ratio, 16, 0.33).
narrative_ontology:measurement_basis(comp_tr_t16, observed).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__simulation_sufficiency, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(comp_tr_t20, observed).
narrative_ontology:measurement(comp_tr_t24, competence_occupation__simulation_sufficiency, theater_ratio, 24, 0.44).
narrative_ontology:measurement_basis(comp_tr_t24, observed).
narrative_ontology:measurement(comp_tr_t28, competence_occupation__simulation_sufficiency, theater_ratio, 28, 0.47).
narrative_ontology:measurement_basis(comp_tr_t28, observed).
narrative_ontology:measurement(comp_tr_t32, competence_occupation__simulation_sufficiency, theater_ratio, 32, 0.48).
narrative_ontology:measurement_basis(comp_tr_t32, observed).
narrative_ontology:measurement(comp_tr_t36, competence_occupation__simulation_sufficiency, theater_ratio, 36, 0.48).
narrative_ontology:measurement_basis(comp_tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__simulation_sufficiency, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t4, competence_occupation__simulation_sufficiency, base_extractiveness, 4, 0.42).
narrative_ontology:measurement_basis(comp_be_t4, observed).
narrative_ontology:measurement(comp_be_t8, competence_occupation__simulation_sufficiency, base_extractiveness, 8, 0.47).
narrative_ontology:measurement_basis(comp_be_t8, observed).
narrative_ontology:measurement(comp_be_t12, competence_occupation__simulation_sufficiency, base_extractiveness, 12, 0.52).
narrative_ontology:measurement_basis(comp_be_t12, observed).
narrative_ontology:measurement(comp_be_t16, competence_occupation__simulation_sufficiency, base_extractiveness, 16, 0.49).
narrative_ontology:measurement_basis(comp_be_t16, observed).
narrative_ontology:measurement(comp_be_t20, competence_occupation__simulation_sufficiency, base_extractiveness, 20, 0.55).
narrative_ontology:measurement_basis(comp_be_t20, observed).
narrative_ontology:measurement(comp_be_t24, competence_occupation__simulation_sufficiency, base_extractiveness, 24, 0.6).
narrative_ontology:measurement_basis(comp_be_t24, observed).
narrative_ontology:measurement(comp_be_t28, competence_occupation__simulation_sufficiency, base_extractiveness, 28, 0.63).
narrative_ontology:measurement_basis(comp_be_t28, observed).
narrative_ontology:measurement(comp_be_t32, competence_occupation__simulation_sufficiency, base_extractiveness, 32, 0.65).
narrative_ontology:measurement_basis(comp_be_t32, observed).
narrative_ontology:measurement(comp_be_t36, competence_occupation__simulation_sufficiency, base_extractiveness, 36, 0.66).
narrative_ontology:measurement_basis(comp_be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__simulation_sufficiency, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(comp_su_t0, observed).
narrative_ontology:measurement(comp_su_t4, competence_occupation__simulation_sufficiency, suppression_requirement, 4, 0.36).
narrative_ontology:measurement_basis(comp_su_t4, observed).
narrative_ontology:measurement(comp_su_t8, competence_occupation__simulation_sufficiency, suppression_requirement, 8, 0.43).
narrative_ontology:measurement_basis(comp_su_t8, observed).
narrative_ontology:measurement(comp_su_t12, competence_occupation__simulation_sufficiency, suppression_requirement, 12, 0.5).
narrative_ontology:measurement_basis(comp_su_t12, observed).
narrative_ontology:measurement(comp_su_t16, competence_occupation__simulation_sufficiency, suppression_requirement, 16, 0.62).
narrative_ontology:measurement_basis(comp_su_t16, observed).
narrative_ontology:measurement(comp_su_t20, competence_occupation__simulation_sufficiency, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(comp_su_t20, observed).
narrative_ontology:measurement(comp_su_t24, competence_occupation__simulation_sufficiency, suppression_requirement, 24, 0.56).
narrative_ontology:measurement_basis(comp_su_t24, observed).
narrative_ontology:measurement(comp_su_t28, competence_occupation__simulation_sufficiency, suppression_requirement, 28, 0.57).
narrative_ontology:measurement_basis(comp_su_t28, observed).
narrative_ontology:measurement(comp_su_t32, competence_occupation__simulation_sufficiency, suppression_requirement, 32, 0.58).
narrative_ontology:measurement_basis(comp_su_t32, observed).
narrative_ontology:measurement(comp_su_t36, competence_occupation__simulation_sufficiency, suppression_requirement, 36, 0.58).
narrative_ontology:measurement_basis(comp_su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__simulation_sufficiency, identity_coordination).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__real_incident_necessity).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__hybrid_occupation).

% DUAL FORMULATION NOTE:
% The colloquial label 'competence occupation' conflates three structurally distinct claims about what exercise suffices to maintain rare-event readiness. This file authors only the simulation_sufficiency reading, with its own epsilon (referent: the standing drill-compliance arrangement, assessed by that reading's own lights), its own beneficiary structure (vendor-, department-, and insurer-facing gains; operator- and public-facing risk), and its own classification. The sibling readings — real_incident_necessity and hybrid_occupation — instantiate different constraints with different victim sets and different observables; both are linked here via network.affects_constraints. Downstream structure runs from this reading outward: drill-compliance standards are cited as the settled baseline against which the necessity claim is dismissed and beyond which the hybrid reading argues.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_occupation__simulation_sufficiency, institutional, 0.45).
constraint_indexing:directionality_override(competence_occupation__simulation_sufficiency, organized, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
