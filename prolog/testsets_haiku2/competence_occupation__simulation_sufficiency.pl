% ============================================================================
% CONSTRAINT STORY: competence_occupation__simulation_sufficiency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: competence_occupation__simulation_sufficiency
 *   human_readable: Simulation Sufficiency for Competence Occupation
 *   domain: safety/organizational/epistemology
 *
 * SUMMARY:
 *   In low-incident, high-consequence operational domains (aviation, nuclear
 *   power, maritime, healthcare), competence maintenance during extended
 *   benign periods has been reformulated as a solvable problem via
 *   simulation-based drills with optimized frequency and fidelity. This
 *   reading treats simulation as sufficient to occupy the competence kernel —
 *   the core epistemic and procedural capabilities required to operate
 *   safely. The constraint's persistence depends on regulatory adoption of
 *   simulation-sufficiency as the authorized standard for training compliance
 *   and on suppression of evidence from incident investigations and
 *   safety-culture research that documents gaps in simulation-trained
 *   competence for contextual, novel, and high-stress scenarios. The
 *   simulation industry benefits directly from this framing (sustained demand
 *   for platforms), and regulatory agencies benefit from having a measurable,
 *   auditable compliance metric. Operational line personnel and organizations
 *   with real incident data bear the actual risk that the occupancy mechanism
 *   may be insufficient.
 *
 * KEY AGENTS:
 *   - simulation_industry: institutional beneficiary/agenda-setter (d ≈ 0.1, arbitrage exit) — designs standards and marketing narratives
 *   - regulatory_agencies: institutional beneficiary/agenda-setter (d ≈ 0.15, constrained exit) — enforces compliance and simplified audit
 *   - operational_line_personnel: powerless payers (d ≈ 0.95, trapped exit) — undergo mandated training, bear risk of gaps
 *   - organizations_with_incident_data: moderate payers (d ≈ 0.72, constrained exit) — fund training, carry outcome risk
 *   - safety_culture_practitioners: excluded (d ≈ 0.5 if seated, analytical exit) — have contradicting evidence, no voice in standards
 *   - incident_investigation_bodies: observers (d ≈ 0.5, analytical) — generate evidence contradicting the claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, 0.68).
domain_priors:suppression_score(competence_occupation__simulation_sufficiency, 0.71).
domain_priors:theater_ratio(competence_occupation__simulation_sufficiency, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__simulation_sufficiency, tangled_rope).
narrative_ontology:human_readable(competence_occupation__simulation_sufficiency, "Simulation Sufficiency for Competence Occupation").
narrative_ontology:topic_domain(competence_occupation__simulation_sufficiency, "safety/organizational/epistemology").

domain_priors:requires_active_enforcement(competence_occupation__simulation_sufficiency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__simulation_sufficiency, '18ec442f-a72e-4ac0-a919-21c30abb3829').
narrative_ontology:cs_kernel_codification('18ec442f-a72e-4ac0-a919-21c30abb3829', formalized).
narrative_ontology:cs_authority_grounding('18ec442f-a72e-4ac0-a919-21c30abb3829', extraction).
narrative_ontology:cs_interpretation_layer_present('18ec442f-a72e-4ac0-a919-21c30abb3829').
narrative_ontology:cs_reading_relation('18ec442f-a72e-4ac0-a919-21c30abb3829', competence_occupation__real_incident_necessity, coexists_with).
narrative_ontology:cs_reading_relation('18ec442f-a72e-4ac0-a919-21c30abb3829', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('18ec442f-a72e-4ac0-a919-21c30abb3829', foundational, simulation_fidelity_sufficient_for_occupancy).
narrative_ontology:cs_axiom_status(simulation_fidelity_sufficient_for_occupancy, holdable).
narrative_ontology:cs_axiom_grounding('18ec442f-a72e-4ac0-a919-21c30abb3829', simulation_fidelity_sufficient_for_occupancy, empirically_contingent).
narrative_ontology:cs_axiom('18ec442f-a72e-4ac0-a919-21c30abb3829', foundational, training_compliance_measurability_establishes_competence).
narrative_ontology:cs_axiom_status(training_compliance_measurability_establishes_competence, holdable).
narrative_ontology:cs_axiom_grounding('18ec442f-a72e-4ac0-a919-21c30abb3829', training_compliance_measurability_establishes_competence, instrumental).
narrative_ontology:cs_reference_frame('18ec442f-a72e-4ac0-a919-21c30abb3829', simulation_based_occupancy_regime).
narrative_ontology:cs_drift_state('18ec442f-a72e-4ac0-a919-21c30abb3829', contemporary_high_reliability_evidence_accumulation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('18ec442f-a72e-4ac0-a919-21c30abb3829', '').
narrative_ontology:cs_kernel_id(competence_occupation__simulation_sufficiency, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, simulation_industry).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, regulatory_agencies_adopting_simulation_standards).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, operational_line_personnel).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, organizations_with_incident_frequency_data).
narrative_ontology:constraint_vindicates(competence_occupation__simulation_sufficiency, training_compliance_measurability).
narrative_ontology:constraint_vindicates(competence_occupation__simulation_sufficiency, skill_decay_prevention_through_frequency_optimization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, manufactures, and distributes high-fidelity simulation platforms (full-scale control room mockups, procedural trainers, desktop sims) and charges training organizations per seat-hour or licensing fees. Benefits from regulatory acceptance of simulation as a sufficient occupancy mechanism because it creates sustained demand for their platforms and prevents shift to live-exercise alternatives. Has vested interest in demonstrating simulation achieves competence goals, which justifies premium pricing and reduces pressure toward cheaper alternatives or real-incident learning.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, simulation_industry, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(competence_occupation__simulation_sufficiency, simulation_industry, agenda_setter).

% Adopt simulation-sufficiency doctrine into training regulations (FAA, NRC, IMO, etc.), creating uniform, auditable training pathways based on documented sim hours and scenario completion. Benefits by gaining a measurable, scalable compliance metric that replaces harder-to-verify incident-based or hybrid approaches. Can enforce training rules via inspection and licensing; reduces regulatory variance and creates predictable training pipelines. Internal pressure to show training effectiveness through frequency/fidelity metrics rather than post-incident investigation.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, regulatory_agencies_adopting_simulation_standards, beneficiary,
    institutional, generational, constrained, national).

% Undergo recurring simulation-based training mandated by regulation, often at the expense of other professional development or operational time. Face live situations where simulation training may not have prepared them for full contextual complexity, novel failure modes, or stress conditions divergent from sim fidelity. Bear the actual cost of skill gaps if competence occupation proves insufficient: lives at risk in aviation, nuclear, maritime, and healthcare contexts. No voice in defining what counts as sufficient occupancy; compliance is non-negotiable.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, operational_line_personnel, payer,
    powerless, biographical, trapped, global).

% Operating organizations (airlines, utilities, shipping companies, hospitals) that track actual safety outcomes and incident rates in their domains. Some accumulate evidence that simulation-only training does not fully occupy the competence kernel for their operational contexts, but face regulatory pressure to rely on simulation data as the authorized metric. Cannot easily exit the simulation-sufficiency framework without regulatory challenge. Pay for simulation training, internal refresher programs, and carry the risk when incidents reveal gaps.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, organizations_with_incident_frequency_data, payer,
    moderate, biographical, constrained, national).

% Researchers, human-factors specialists, and incident investigators who accumulate data on skill decay mechanisms, transfer fidelity from simulator to line operations, and contextual learning from actual incidents. Often document that competence occupation is multi-mechanism (hybrid) and simulation frequency alone is an imperfect proxy. Systematically excluded from regulatory standard-setting and from designing training curricula; their findings are used selectively to validate simulation-sufficiency claims when convenient and suppressed when they suggest insufficiency.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, safety_culture_practitioners, excluded,
    moderate, biographical, constrained, national).

% Inspectors and auditors who certify training compliance by counting simulator hours, verifying scenario completion logs, and confirming fidelity metrics against regulatory checklists. Do not observe actual competence or incident outcomes directly; their job is to verify that documentation of simulation activity meets regulatory thresholds. Have incentive to treat simulation records as sufficient evidence of occupancy because it makes audits efficient, scalable, and defensible.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, regulatory_compliance_auditors, agenda_setter,
    powerful, biographical, mobile, national).

% Post-incident investigators (NTSB, TSB, marine accident inquiries) who analyze actual failures and often find competence gaps that simulation training did not address. Generate evidence contradicting simulation-sufficiency claims but lack power to alter training standards directly. Their findings are cited selectively in policy discourse; agencies adopting simulation-sufficiency standards do not formally update standards based on incident investigation evidence.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, incident_investigation_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__simulation_sufficiency, simulation_industry).
narrative_ontology:fixing_cost_class(competence_occupation__simulation_sufficiency, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform, auditable, scalable training pathway: organizations can document competence occupation through standardized simulation scenarios and frequency metrics, regulators can verify compliance via inspection of logs, and the training industry can deploy consistent platforms across domains. Solves the coordination problem of how to certify that personnel remain competent when live-incident frequency is low and traditional apprenticeship models are economically infeasible.
% TRANSFER_FUNCTION: Moves resources from operational organizations and line personnel (training budgets, seat-hours, professional time away from operations) to the simulation industry (per-platform licensing, per-seat-hour fees, proprietary scenario libraries) and to regulatory agencies (standardized audit procedures, simplified compliance verification). Also transfers decision-making power about what constitutes competence from organizations and practitioners with incident data to regulators and simulation vendors with vested interests in simulation-sufficiency claims.
% ABSENT_VOICES: Safety culture researchers, human-factors specialists, and incident investigators who have documented multi-mechanism competence occupation and skill-decay pathways are structurally excluded from regulatory standard-setting. Organizations with incident frequency data that contradicts simulation-sufficiency claims have no formal channel to challenge adopted standards. Line personnel whose actual competence gaps emerged post-incident have no voice in defining training adequacy.
% DISAPPEARANCE_RATIONALE: If the constraint (regulatory mandate for simulation-sufficiency as the occupancy criterion) were removed, training regimens would shift immediately: organizations would reintegrate incident-based learning, refresher cycles, and hybrid mechanisms; the simulation industry would face competitive pressure and contraction; regulatory inspection procedures would need overhaul. The arrangement would not simply dissolve — its removal requires reorganization of authority over what counts as competence and how it is verified.
% FOUNDING_PROBLEM: Low-frequency, high-consequence domains (aviation, nuclear, maritime) cannot rely on live-incident repetition for training because incidents are rare and catastrophic. Traditional apprenticeship-and-incident-response models break down when organizations operate for years without a serious incident. Simulation emerged as a solution to maintain competence during benign operating periods without waiting for actual failures.
% FOUNDING_PROBLEM_CORROBORATION: The simulation industry, regulatory agencies, and major operators (airlines, utilities) all affirm that the founding problem — maintaining competence in low-incident environments — is live and critical. However, independent human-factors research and incident investigation bodies attest that while the founding problem is real, the proposed solution (simulation sufficiency as a complete occupancy mechanism) is contested; evidence from actual incidents shows gaps in simulation-trained competence for contextual, novel, and high-stress scenarios.
narrative_ontology:disappearance_verdict(competence_occupation__simulation_sufficiency, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__simulation_sufficiency, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__simulation_sufficiency, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_occupation__simulation_sufficiency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__simulation_sufficiency, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.68 at interval end) is high because the constraint transfers resources (training budgets, seat-hours, proprietary licensing fees) from operational organizations to the simulation industry and regulatory agencies, and because it authorizes regulators to define competence via a measurable proxy (sim hours) that benefits beneficiaries regardless of actual competence outcomes. The transfer is asymmetric: beneficiaries collect from the arrangement (sim vendors get revenue, regulators get audit simplicity) without bearing the full cost of gaps; payers bear training costs and operational risk. Suppression (0.71) is high because alternatives (hybrid training, incident-based refresher, real-scenario exercises) are excluded by regulation, and because organizations with incident data that contradicts the claim cannot easily surface that evidence within the regulatory process. Theater (0.58 and rising) reflects that an increasing share of enforcement effort goes toward documenting training compliance (counting sim hours, logging scenarios) rather than verifying actual competence; the training system becomes self-referential (simulator certifies competence by definition, not by outcome). Accessibility_collapse (0.42) is moderate-low because real-incident learning, hybrid regimens, and refresher mechanisms remain available in principle but are suppressed by regulatory mandate; they have not become conceptually unavailable. Resistance (0.64) is substantial because organizations and practitioners generate evidence that the simulation-sufficiency frame does not capture how competence actually decays and is maintained, and because high-reliability communities have accumulated decades of research suggesting multi-mechanism occupancy is necessary. The measurement series show extractiveness and theater rising and plateauing, with suppression rising slightly and resistance declining slightly over the interval — consistent with the constraint maturing (becoming more entrenched) as regulatory adoption spreads and the simulation industry consolidates.
 *
 * PERSPECTIVAL GAP:
 *   From the simulation industry and regulatory agencies (beneficiary seats), the constraint is genuine coordination that solves a real founding problem (maintaining competence in low-incident domains). From operational line personnel and incident investigators (payer/excluded seats), the same structure is extractive rent-seeking using coordination as cover — the founding problem is real, but the solution is insufficient and persists for institutional reasons. The engine computes this divergence per-seat from the structural data: beneficiaries get low d (derive subsidy-like effective extraction), payers get high d (carry the target-like load), and excluded parties with contradicting evidence have no seat in the derived classification. The claim/metric gap is intentional: the constraint is CLAIMED as tangled rope (genuine coordination + asymmetric extraction), and the authored metrics (high extractiveness, high theater, suppressed alternatives) are consistent with that claim. The engine's per-seat classification should show beneficiary seats computing rope-like (coordination-dominant), payer seats computing snare-like (extraction-dominant), and excluded seats computing snare if they were seated.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulation industry: d ≈ 0.1 (full beneficiary) — receives revenue directly from the constraint, controls the technical definition of fidelity and sufficiency, has arbitrage-grade exit (can shift to other domains if simulation loses mandate). Regulatory agencies: d ≈ 0.15 (nearly beneficiary) — gain audit simplicity, standardized compliance procedures, and reduced variance; constrained exit (cannot easily revert without political cost). Operational personnel: d ≈ 0.95 (nearly full target) — fund training, undergo mandated drills, bear risk of competence gaps, trapped exit (cannot opt out without losing certification/employment). Organizations with incident data: d ≈ 0.72 (strongly targeted) — pay for training, carry safety risk, cannot easily challenge standards, constrained exit (can form coalitions but at regulatory risk). Safety-culture practitioners: excluded (no seat in the constraint, but if they had a seat it would be d ≈ 0.5 by default — moderate target, since their evidence contradicts the regime). Incident investigators: observers (analytical exit, d = 0.5 by default). The directionality structure is driven by the beneficiary/victim declarations, exit options, and power asymmetry; no overrides are necessary because the structural derivation captures the true relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining competence in low-incident domains) is live and acknowledged by all seats. However, the founding_problem_status is contested: beneficiaries assert the founding problem is solved by simulation-sufficiency, while excluded practitioners and incident investigators attest the problem is partially solved but the solution is incomplete. The constraint is not yet a piton (it has clear beneficiaries who maintain it actively), but it carries piton-risk: if incident investigation evidence continues to document gaps unaddressed by increased sim frequency, the constraint may calcify into a degraded form where the founding problem becomes functionally dead (organizations will have optimized simulation to regulatory thresholds but incident rates may persist), yet the constraint persists due to institutional inertia and regulatory path-dependence. Theater_ratio rising to 0.58 and approaching the piton threshold (0.65+) signals this risk: as the measurement system becomes decoupled from competence outcomes, the constraint transitions toward pure compliance theater. The mandatrophy scenario is: high-reliability organizations accumulate incident data showing simulation-trained crews failed in ways hybrid/incident-informed crews did not, but the regulatory standard does not update because (a) incident data is considered site-specific rather than systematic, (b) the simulation industry lobbies against standard revision, and (c) the agencies have institutional investment in the current compliance regime. The constraint then persists as a zombie — regulatory compliance without functional competence occupation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_sufficiency,
    'Is the fidelity threshold of contemporary simulators (in their highest-cost configurations) sufficient to occupy the competence kernel for all operational contexts and failure modes in the domain, or do certain classes of contextual, novel, or high-stress scenarios require real-incident or high-consequence near-miss exposure?',
    'Systematic comparison of competence profiles (knowledge, procedural execution, decision-making under stress, novel-scenario response) across line personnel trained simulation-only vs. those with real-incident exposure or hybrid regimens. Post-incident analysis documenting whether trained personnel''s gaps align with simulation-fidelity limits.',
    'If fidelity is sufficient, simulation-sufficiency is structurally defensible; if gaps exist in stress response or contextual reasoning, the constraint misidentifies the occupancy mechanism and the authorization to treat simulation as complete is false. The classification would shift from tangled_rope (coordination + asymmetric extraction) toward snare (extraction with coordination as cover story).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_sufficiency, empirical, 'Whether simulation fidelity is sufficient to occupy competence kernel for all required scenarios.').

omega_variable(
    reading_decomposition_empirical,
    'This constraint is one reading of a contested kernel (competence_occupation). The sibling readings — real_incident_necessity and hybrid_occupation — are structurally distinct constraints with different ε values, beneficiary structures, and victim sets. Are the three readings genuinely incommensurable (each with its own ε), or is there an underlying unitary constraint that all three are observing from different angles?',
    'Test the ε-invariance principle: can the simulation-sufficiency reading be authored with a materially different ε than the hybrid or incident-necessity readings when all three are measured against the same standing arrangement (competence occupation in low-incident domains)? If yes, they are separate constraints (kernel readings, different stories). If no, they collapse to one constraint with measurement-basis disagreement (an error).',
    'If ε differs across readings (expected), the three are properly decomposed as separate constraint stories, each with its own narrative, metrics, and stakeholders; the kernel-reading apparatus (reading_relations, axioms, cs_structure) applies to each. If ε collapses to one value, the three are measurement-basis disputes within a single constraint, not decomposable readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_decomposition_empirical, conceptual, 'Whether the simulation-sufficiency reading is a genuine kernel reading or a measurement disagreement collapsed into multiple framings.').

omega_variable(
    authorization_mechanism_structure,
    'Who is authorized to declare what counts as sufficient competence occupation, and does that authority derive from competence data (incident investigation, skill assessments) or from institutional power (regulatory capacity to enforce standards)?',
    'Track the causal chain of standard-setting: do regulators update simulation-sufficiency standards in response to incident investigation evidence or post-incident competence failures, or do standards persist and incident findings are filed separately? Does the regulatory authority ever reverse a simulation-sufficiency decision based on competing evidence?',
    'If standards are evidence-responsive, the authorization is partly epistemic and the constraint''s persistence depends on a genuine factual claim. If standards are institution-driven regardless of evidence, authorization is purely political and the constraint is maintained by suppression of contradicting evidence. This affects whether the beneficiary/victim structure is asymmetric extraction (snare) or coordination with asymmetric distribution (tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorization_mechanism_structure, empirical, 'Whether regulatory authority over competence standards is evidence-responsive or politically locked.').

omega_variable(
    suppression_of_contrary_evidence,
    'Is suppression in this constraint structural (alternatives genuinely unavailable due to cost or technical barriers) or internalized (organizations and practitioners accept the simulation-sufficiency frame and suppress their own contrary evidence)?',
    'Post-deregulation experiments: if a jurisdiction permits organizations to design hybrid or incident-based training regimens as an alternative to mandated simulation, do organizations adopt the alternative, or do they continue simulation-dominant regimens despite regulatory permission? Do practitioners and organizational safety personnel openly acknowledge doubt about simulation sufficiency when surveyed anonymously?',
    'If suppression is structural, operationals have genuinely limited exit (constrained/trapped); if internalized, they carry suppression even when structural barriers are removed. Internalized suppression raises the effective extraction burden and the persistence mechanisms that maintain the constraint. Indicates the constraint may be more extractive than base_suppression score alone suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_of_contrary_evidence, empirical, 'Whether suppression of alternatives to simulation-sufficiency is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__simulation_sufficiency, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__simulation_sufficiency, theater_ratio, 0, 0.42).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t5, competence_occupation__simulation_sufficiency, theater_ratio, 5, 0.46).
narrative_ontology:measurement_basis(comp_tr_t5, observed).
narrative_ontology:measurement(comp_tr_t10, competence_occupation__simulation_sufficiency, theater_ratio, 10, 0.51).
narrative_ontology:measurement_basis(comp_tr_t10, observed).
narrative_ontology:measurement(comp_tr_t15, competence_occupation__simulation_sufficiency, theater_ratio, 15, 0.54).
narrative_ontology:measurement_basis(comp_tr_t15, observed).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__simulation_sufficiency, theater_ratio, 20, 0.56).
narrative_ontology:measurement_basis(comp_tr_t20, observed).
narrative_ontology:measurement(comp_tr_t30, competence_occupation__simulation_sufficiency, theater_ratio, 30, 0.58).
narrative_ontology:measurement_basis(comp_tr_t30, observed).
narrative_ontology:measurement(comp_tr_t40, competence_occupation__simulation_sufficiency, theater_ratio, 40, 0.58).
narrative_ontology:measurement_basis(comp_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__simulation_sufficiency, base_extractiveness, 0, 0.51).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t5, competence_occupation__simulation_sufficiency, base_extractiveness, 5, 0.55).
narrative_ontology:measurement_basis(comp_be_t5, observed).
narrative_ontology:measurement(comp_be_t10, competence_occupation__simulation_sufficiency, base_extractiveness, 10, 0.6).
narrative_ontology:measurement_basis(comp_be_t10, observed).
narrative_ontology:measurement(comp_be_t15, competence_occupation__simulation_sufficiency, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(comp_be_t15, observed).
narrative_ontology:measurement(comp_be_t20, competence_occupation__simulation_sufficiency, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(comp_be_t20, observed).
narrative_ontology:measurement(comp_be_t30, competence_occupation__simulation_sufficiency, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(comp_be_t30, observed).
narrative_ontology:measurement(comp_be_t40, competence_occupation__simulation_sufficiency, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(comp_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__simulation_sufficiency, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(comp_su_t0, observed).
narrative_ontology:measurement(comp_su_t5, competence_occupation__simulation_sufficiency, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(comp_su_t5, observed).
narrative_ontology:measurement(comp_su_t10, competence_occupation__simulation_sufficiency, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(comp_su_t10, observed).
narrative_ontology:measurement(comp_su_t15, competence_occupation__simulation_sufficiency, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(comp_su_t15, observed).
narrative_ontology:measurement(comp_su_t20, competence_occupation__simulation_sufficiency, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(comp_su_t20, observed).
narrative_ontology:measurement(comp_su_t30, competence_occupation__simulation_sufficiency, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(comp_su_t30, observed).
narrative_ontology:measurement(comp_su_t40, competence_occupation__simulation_sufficiency, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(comp_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(comp_grid_01, competence_occupation__simulation_sufficiency, accessibility_collapse(class), 0, 0.48).
narrative_ontology:measurement(comp_grid_02, competence_occupation__simulation_sufficiency, accessibility_collapse(class), 40, 0.5).
narrative_ontology:measurement(comp_grid_03, competence_occupation__simulation_sufficiency, accessibility_collapse(individual), 0, 0.32).
narrative_ontology:measurement(comp_grid_04, competence_occupation__simulation_sufficiency, accessibility_collapse(individual), 40, 0.36).
narrative_ontology:measurement(comp_grid_05, competence_occupation__simulation_sufficiency, accessibility_collapse(organizational), 0, 0.42).
narrative_ontology:measurement(comp_grid_06, competence_occupation__simulation_sufficiency, accessibility_collapse(organizational), 40, 0.45).
narrative_ontology:measurement(comp_grid_07, competence_occupation__simulation_sufficiency, accessibility_collapse(structural), 0, 0.35).
narrative_ontology:measurement(comp_grid_08, competence_occupation__simulation_sufficiency, accessibility_collapse(structural), 40, 0.38).
narrative_ontology:measurement(comp_grid_09, competence_occupation__simulation_sufficiency, resistance(class), 0, 0.7).
narrative_ontology:measurement(comp_grid_10, competence_occupation__simulation_sufficiency, resistance(class), 40, 0.68).
narrative_ontology:measurement(comp_grid_11, competence_occupation__simulation_sufficiency, resistance(individual), 0, 0.54).
narrative_ontology:measurement(comp_grid_12, competence_occupation__simulation_sufficiency, resistance(individual), 40, 0.52).
narrative_ontology:measurement(comp_grid_13, competence_occupation__simulation_sufficiency, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(comp_grid_14, competence_occupation__simulation_sufficiency, resistance(organizational), 40, 0.6).
narrative_ontology:measurement(comp_grid_15, competence_occupation__simulation_sufficiency, resistance(structural), 0, 0.48).
narrative_ontology:measurement(comp_grid_16, competence_occupation__simulation_sufficiency, resistance(structural), 40, 0.46).
narrative_ontology:measurement(comp_grid_17, competence_occupation__simulation_sufficiency, stakes_inflation(class), 0, 0.68).
narrative_ontology:measurement(comp_grid_18, competence_occupation__simulation_sufficiency, stakes_inflation(class), 40, 0.7).
narrative_ontology:measurement(comp_grid_19, competence_occupation__simulation_sufficiency, stakes_inflation(individual), 0, 0.52).
narrative_ontology:measurement(comp_grid_20, competence_occupation__simulation_sufficiency, stakes_inflation(individual), 40, 0.55).
narrative_ontology:measurement(comp_grid_21, competence_occupation__simulation_sufficiency, stakes_inflation(organizational), 0, 0.62).
narrative_ontology:measurement(comp_grid_22, competence_occupation__simulation_sufficiency, stakes_inflation(organizational), 40, 0.65).
narrative_ontology:measurement(comp_grid_23, competence_occupation__simulation_sufficiency, stakes_inflation(structural), 0, 0.55).
narrative_ontology:measurement(comp_grid_24, competence_occupation__simulation_sufficiency, stakes_inflation(structural), 40, 0.58).
narrative_ontology:measurement(comp_grid_25, competence_occupation__simulation_sufficiency, suppression(class), 0, 0.72).
narrative_ontology:measurement(comp_grid_26, competence_occupation__simulation_sufficiency, suppression(class), 40, 0.73).
narrative_ontology:measurement(comp_grid_27, competence_occupation__simulation_sufficiency, suppression(individual), 0, 0.58).
narrative_ontology:measurement(comp_grid_28, competence_occupation__simulation_sufficiency, suppression(individual), 40, 0.6).
narrative_ontology:measurement(comp_grid_29, competence_occupation__simulation_sufficiency, suppression(organizational), 0, 0.68).
narrative_ontology:measurement(comp_grid_30, competence_occupation__simulation_sufficiency, suppression(organizational), 40, 0.71).
narrative_ontology:measurement(comp_grid_31, competence_occupation__simulation_sufficiency, suppression(structural), 0, 0.64).
narrative_ontology:measurement(comp_grid_32, competence_occupation__simulation_sufficiency, suppression(structural), 40, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__simulation_sufficiency, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_occupation__simulation_sufficiency, 0.18).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__real_incident_necessity).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__hybrid_occupation).

% DUAL FORMULATION NOTE:
% The competence_occupation kernel decomposes into three structurally distinct readings: simulation_sufficiency (this story), real_incident_necessity, and hybrid_occupation. Each reading has its own ε (the standing arrangement measured through the lens of what each reading claims is necessary for competence). The simulation_sufficiency reading authors high ε because it identifies competence-gap risk and the extraction of resources to the simulation industry despite that risk. The real_incident reading would author high ε because it identifies the same competence problem but attributes it to suppression of incident-based learning. The hybrid reading would author lower ε because it acknowledges the coordination value of simulation while requiring additional mechanisms. All three link back to the shared kernel (competence_occupation) but are separate constraints with separate stakeholder structures. Decomposition follows the ε-invariance principle: if changing how you measure the constraint (what you require for occupancy) changes ε, you have two constraints. Here, the reading IS the measurement basis, so separate stories are required.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_occupation__simulation_sufficiency, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
