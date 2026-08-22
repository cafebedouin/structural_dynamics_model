% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__catastrophe_as_necessary_anchor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__catastrophe_as_necessary_anchor, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: competence_exercise_requirement__catastrophe_as_necessary_anchor
 *   human_readable: Catastrophe-Anchored Competence Maintenance in High-Reliability Organizations
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   The competence-exercise kernel concerns how high-reliability
 *   organizations (aviation, nuclear power, surgery, emergency medicine)
 *   maintain the irreducible skills required to handle catastrophic failure.
 *   This constraint instantiates ONE READING of that kernel: the
 *   catastrophe-as-necessary-anchor reading. Under this reading, motor
 *   skills, decision-making under genuine jeopardy, and organizational muscle
 *   memory cannot be adequately maintained through simulation, non-jeopardy
 *   audits, or procedural exercises alone — only real high-stakes events (or
 *   near-misses) provide the irreducible exercise that keeps competence
 *   sharp. The constraint extracts from operational personnel and engineers
 *   by implicitly devaluing their simulation-based training as theater; it
 *   benefits post-incident learning institutions by positioning incident
 *   investigation as the authoritative source of competence knowledge.
 *   SIBLING READINGS: 'simulation_as_adequate_exercise' (high-fidelity
 *   simulation with rigorous debriefing constitutes adequate competence
 *   maintenance) and 'hybrid_dependency' (simulation is necessary but
 *   insufficient; competence requires both simulation AND periodic real-world
 *   anchoring). This constraint does NOT describe those readings — it
 *   instantiates only the catastrophe-anchored reading.
 *
 * KEY AGENTS:
 *   - operational_personnel (pilots, operators, surgeons): carry identity_locked exit; bear the cost of implicit doubt about simulator-based competence during quiet periods
 *   - safety_engineers and simulation researchers: moderate power; constrained exit; their work is treated as theater unless validated by real events
 *   - safety_culture_institutions (FAA, IAEA, NTSB, TSB): institutional power; collect authority from incident-driven learning models
 *   - post_incident_learning networks: organized power; derive funding and professional ecosystem from real-event study
 *   - regulatory bodies: institutional agenda-setters; maintain the catastrophe-anchored frame through competence-validation standards
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.68).
domain_priors:suppression_score(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.71).
domain_priors:theater_ratio(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__catastrophe_as_necessary_anchor, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__catastrophe_as_necessary_anchor, "Catastrophe-Anchored Competence Maintenance in High-Reliability Organizations").
narrative_ontology:topic_domain(competence_exercise_requirement__catastrophe_as_necessary_anchor, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_requirement__catastrophe_as_necessary_anchor).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'e504b169-a28b-47e1-8596-487bc2bb4c63').
narrative_ontology:cs_kernel_codification('e504b169-a28b-47e1-8596-487bc2bb4c63', formalized).
narrative_ontology:cs_authority_grounding('e504b169-a28b-47e1-8596-487bc2bb4c63', extraction).
narrative_ontology:cs_interpretation_layer_present('e504b169-a28b-47e1-8596-487bc2bb4c63').
narrative_ontology:cs_reading_relation('e504b169-a28b-47e1-8596-487bc2bb4c63', competence_exercise_requirement__simulation_as_adequate_exercise, forecloses).
narrative_ontology:cs_reading_relation('e504b169-a28b-47e1-8596-487bc2bb4c63', competence_exercise_requirement__hybrid_dependency, influences).
narrative_ontology:cs_axiom('e504b169-a28b-47e1-8596-487bc2bb4c63', foundational, real_events_sole_irreducible_validator).
narrative_ontology:cs_axiom_status(real_events_sole_irreducible_validator, holdable).
narrative_ontology:cs_axiom_grounding('e504b169-a28b-47e1-8596-487bc2bb4c63', real_events_sole_irreducible_validator, empirically_contingent).
narrative_ontology:cs_axiom('e504b169-a28b-47e1-8596-487bc2bb4c63', secondary, simulation_inherently_incomplete_proxy).
narrative_ontology:cs_axiom_status(simulation_inherently_incomplete_proxy, holdable).
narrative_ontology:cs_axiom_grounding('e504b169-a28b-47e1-8596-487bc2bb4c63', simulation_inherently_incomplete_proxy, empirically_contingent).
narrative_ontology:cs_reference_frame('e504b169-a28b-47e1-8596-487bc2bb4c63', validated_competence_through_incident_exposure).
narrative_ontology:cs_drift_state('e504b169-a28b-47e1-8596-487bc2bb4c63', contemporary_high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e504b169-a28b-47e1-8596-487bc2bb4c63', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, safety_culture_institutions).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, post_incident_learning_networks).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, operational_personnel).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, safety_engineers_in_quiet_periods).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_fidelity_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Aviation pilots, nuclear plant operators, surgical teams: must maintain perishable motor and cognitive skills under the constraint that only real high-stakes events provide irreducible learning and competence verification. During quiet periods (months or years without incidents), their formal training and simulation-based exercises are treated as theater by this reading's logic — the muscle memory atrophies silently. When an actual event occurs, the gap between their simulated readiness and actual performance becomes catastrophically visible. They are locked into their professional identity and cannot exit the domain without career abandonment.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, operational_personnel, payer,
    moderate, biographical, identity_locked, global).

% Systems engineers and safety specialists who design and maintain simulation programs, non-jeopardy audits, and procedure refinements during periods without incidents. Under the catastrophe-anchored reading, their work is implicitly devalued as an adequate vehicle for maintaining organizational competence. Their investments in high-fidelity simulation and procedural rigor are judged sufficient only if real events validate them; the lack of incidents over long periods shifts the narrative to their work being 'untested' or 'merely theatrical.' They pay the cost of institutional doubt about whether their simulation-based training actually maintains competence.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, safety_engineers_in_quiet_periods, payer,
    powerful, generational, constrained, global).

% Regulatory bodies (FAA, IAEA, surgical boards), accident investigation agencies (NTSB, TSB), and industry safety consortia that accumulate authority and funding through incident-driven investigations and post-event learning mandates. The catastrophe-anchored reading legitimizes their role as the adjudicators of 'real' competence validation; their authority grows after incidents and their evidentiary model (real-world failure analysis) is treated as the ground truth for what competence actually means. They collect institutional power, research funding, and legitimacy from the frame that only catastrophic events reveal what truly matters.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, safety_culture_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Communities of practice built around investigating and learning from actual incidents: accident investigation specialists, root-cause analysis teams, incident debriefing networks, and the professional ecosystem around aviation safety (accident reconstruction, human factors research, cockpit procedures after crashes). These networks derive their authority, research opportunities, and organizational resources from the existence and study of real events. The catastrophe-anchored reading positions real incidents as the only legitimate source of competence knowledge, which sustains their centrality in safety systems and their ability to mandate procedural changes based on post-event analysis.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, post_incident_learning_networks, beneficiary,
    organized, generational, arbitrage, global).

% Academic and industry researchers working to increase simulation fidelity, develop virtual-reality training, and measure training effectiveness. Under the catastrophe-anchored reading, their work is positioned as potentially false economy — high-fidelity simulation may be theater if it is never tested against real catastrophic events. Their research is constrained to serve institutions that doubt its adequacy. They face pressure to frame their work as 'preparation for the inevitable,' not as a complete alternative to real-event learning.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_fidelity_researchers, payer,
    moderate, biographical, constrained, global).

% The agencies that set and enforce competence standards (FAA, IAEA, ICAO, medical licensing boards) must make budget and policy choices about training mandates. The catastrophe-anchored reading commits them to a framework where only real incidents validate competence, creating de facto pressure to tolerate longer incident-free periods (because nothing except incidents proves what matters) and to fund incident investigation infrastructure as the primary source of learning. They administer the constraint by refusing to accept simulation or non-jeopardy audits as sufficient evidence of competence maintenance, at least implicitly.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, regulatory_bodies_budget_offices, agenda_setter,
    institutional, generational, arbitrage, national).

% Airlines, nuclear utilities, hospital systems, and military commands that operate under safety regulations. They observe the constraint's effects on their workforce, their training budgets, and their incident-response obligations. They are neither pure beneficiaries nor pure victims — they must comply with regulations built on the catastrophe-anchored reading, but they also bear the cost of incidents when they occur and experience the atrophy problem during quiet periods.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, organizational_operators, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared epistemology of 'what counts as real competence validation' for high-reliability organizations: the constraint coordinates around a commitment that motor skills, decision-making under stress, and organizational muscle memory can only be reliably measured and maintained through exposure to actual high-stakes events or their closest proxies.
% TRANSFER_FUNCTION: Moves authority over competence definition away from simulation engineers and training designers toward incident investigators and post-event learning institutions. Moves professional credibility from 'competent simulator' toward 'tested by real events.' Moves funding and research resources toward accident investigation and post-incident forensics.
% ABSENT_VOICES: Operators and personnel in organizations that have had zero major incidents over decades (perfect safety records); their competence is implicitly under suspicion under this reading because it has never been tested by real catastrophe. Also absent: simulation engineers and trainers who might argue their work is adequate (they are present but devalued). Also absent: operators from high-reliability cultures where incident-free operation is taken as evidence of competence rather than doubt about it.
% DISAPPEARANCE_RATIONALE: If the constraint vanished and organizations accepted that high-fidelity simulation, non-jeopardy audits, and regular procedure exercises constitute adequate competence maintenance, the entire funding and organizational structure of post-incident learning would contract. Accident investigation agencies would shrink; training would be funded as a continuous competence maintenance activity rather than a pre-incident and post-incident ritual. Personnel would be relieved of the shadow doubt that their simulated readiness is 'merely theatrical.' Regulatory focus would shift from incident mandates to simulation validation.
% FOUNDING_PROBLEM: In the mid-20th century, high-consequence technical operations (aviation, nuclear power) experienced catastrophic failures where personnel who had trained extensively in simulations and procedures failed critically under real stress. Incidents like early commercial aviation crashes revealed gaps between simulator-trained responses and actual human performance under genuine jeopardy. The founding problem: how can we know if our training actually maintains competence when the real test is catastrophic failure?
% FOUNDING_PROBLEM_CORROBORATION: Post-incident investigation agencies (NTSB, TSB, Civil Aviation Medical Institute) attest that the founding problem is live — every major incident reveals training gaps or procedure misunderstandings. Simulation researchers and training designers argue the problem is substantially solved — modern simulators detect and correct performance gaps that were invisible in the 1950s, and procedures are now refined to match simulator fidelity. Major near-miss event repositories and human factors research from outside incident-investigation circles document that simulator fidelity has improved dramatically, yet the catastrophe-anchored reading persists in regulatory and cultural framing.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__catastrophe_as_necessary_anchor, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__catastrophe_as_necessary_anchor, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the constraint imposes a permanent shadow doubt on simulation-based competence — personnel and engineers work under the epistemological assumption that their efforts are preliminary until validated by real catastrophe. Suppression is slightly higher (0.71) because alternatives (accepting simulation as adequate, rewarding incident-free operation as evidence of competence) are actively discouraged by regulatory and institutional inertia. Theater is moderate (0.42) because the constraint genuinely coordinates around a real problem (simulator-reality gaps did exist and still exist in some domains) but the degree to which simulation has solved that problem is suppressed — the real historical improvement in simulator fidelity is downplayed in favor of the catastrophe-anchored narrative. Accessibility collapse is high (0.78) because once the constraint's framing is accepted, alternatives become nearly invisible: the only way to prove competence is to wait for catastrophe or near-miss, making exit from the frame extremely difficult. The measurement series shows extractiveness rising over the interval (0.42 → 0.68) as incident-free periods accumulate and the implicit doubt about untested competence grows; theater ratio rises initially then stabilizes, indicating the constraint settles into a steady-state theatrical maintenance around time 15-20 (regulatory compliance with simulation without genuine belief in its sufficiency).
 *
 * PERSPECTIVAL GAP:
 *   From the seat of safety-culture institutions and post-incident learning agencies, this constraint is seen as necessary coordination — the only defensible epistemology for maintaining competence in high-consequence domains. From the seat of operational personnel and simulation engineers, it is extractive: they work under a standard of proof they can never satisfy except through catastrophe. The engine computes per-seat divergence from the structural data (beneficiaries vs. payers, power asymmetry, exit options); this gap shows why the same constraint reads as rope-coordination from one seat and snare-extraction from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Safety-culture institutions have high power (institutional), low cost to exit (arbitrage: they can shift to other safety domains), and clear benefit (authority accumulation). Their directionality is near-beneficiary (d ≈ 0.2). Operational personnel have moderate power (pilots are moderately powerful within aviation, but aviation is one domain), identity-locked exit (career abandonment if they leave), and clear cost (implicit competence doubt, atrophy risk during quiet periods, mandatory theater work). Their directionality is near-target (d ≈ 0.8). Simulation engineers and safety researchers fall between: powerful enough to push back on the constraint (some regulatory bodies are funding high-fidelity simulation), but constrained in exit (their careers depend on the institutions that use the catastrophe-anchored frame) and bearing real cost (their work is devalued). Directionality: d ≈ 0.6 (leaning toward target).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint CLAIMS tangled_rope (a real coordination function — the founding problem of simulator-reality gaps — combined with asymmetric extraction). The metrics support this claim: beneficiaries (incident-investigation institutions) benefit from the constraint's persistence; payers (operational personnel, simulation engineers) bear costs not shared by beneficiaries; active enforcement exists (regulatory standards and institutional inertia maintain the catastrophe-anchored frame despite improvements in simulator fidelity). The constraint avoids pure snare classification because a genuine coordination problem (how to maintain competence) and a genuinely asymmetric solution (only real events validate competence) exist. But the extraction is high because the constraint persists despite objective improvements in simulation fidelity — the beneficiaries have captured the epistemological definition of what counts as 'real competence validation.' Mandatrophy would resolve if: (1) organizations accepted that high-fidelity simulation plus regular non-jeopardy audits constitute adequate competence maintenance (the hybrid_dependency or simulation_as_adequate_exercise reading prevailed), (2) incident-investigation institutions lost funding or authority and could no longer enforce the catastrophe-anchored frame, or (3) a long incident-free period combined with demonstrable simulator-driven competence improvement created unbearable institutional dissonance. Currently, the founding problem status is contested and extractiveness is stable, preventing mandatrophy resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulator_fidelity_improvement_undetected,
    'Has simulator fidelity improved enough to close the gap between simulated and actual performance, making real-event validation unnecessary?',
    'Longitudinal analysis comparing simulator-trained performance to actual incident response across decades: if incident-free-period operators demonstrate competence equivalent to post-incident-trained operators, the founding problem has been substantially solved.',
    'If true, the constraint would degrade from tangled_rope (genuine coordination + extraction) to piton (atrophied coordination, persistence by institutional inertia). Extractiveness would reclassify as theater maintained for institutional prestige rather than functional necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulator_fidelity_improvement_undetected, empirical, 'Whether the objective simulator-to-reality gap has been closed by technology improvements.').

omega_variable(
    catastrophe_versus_near_miss_boundary,
    'Do near-misses without actual harm provide the same irreducible exercise as genuine catastrophic events?',
    'Compare competence trajectories after near-miss events vs. after actual incidents vs. after simulation-only periods. If near-misses produce equivalent learning and competence maintenance as catastrophes, the constraint''s boundary condition shifts.',
    'If near-misses suffice, the constraint becomes more permissive (regulators can mandate near-miss induction and audits instead of waiting for actual incidents) and extractiveness drops substantially — the shadow doubt about untested competence is relieved by near-miss evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_versus_near_miss_boundary, empirical, 'Whether catastrophe is categorically necessary or near-miss approximations suffice.').

omega_variable(
    institutional_capture_of_epistemology,
    'Is the catastrophe-anchored reading itself an instance of institutional capture, where beneficiary organizations (safety agencies, incident-investigation networks) have captured the definition of what counts as ''real competence'' to sustain their funding and authority?',
    'Comparative institutional analysis: do organizations outside the incident-investigation ecosystem (independent safety consultancies, simulator manufacturers, operational safety councils with no incident-investigation role) converge on the catastrophe-anchored reading? Or do they converge on simulation-as-adequate or hybrid readings?',
    'If capture is present, the constraint reclassifies from tangled_rope toward snare: the coordination function (simulator-reality gap) is real, but the persistent asymmetry (only incidents validate competence) is sustained by beneficiary control of the definition, not by structural necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_capture_of_epistemology, conceptual, 'Whether the catastrophe-anchored reading reflects genuine epistemological necessity or institutional power over the definition of competence.').

omega_variable(
    identity_lock_atrophy_during_quiet_periods,
    'During incident-free periods, does implicit doubt about untested competence cause actual cognitive or motor atrophy, or is atrophy primarily a narrative effect?',
    'Longitudinal cognitive testing and motor-skill assessment of personnel in incident-free vs. incident-exposed organizations over decades. If incident-free-period personnel show measurable atrophy despite simulation, the constraint''s extraction is real. If atrophy is minimal and attributable to aging rather than doubt, the extraction is largely narrative.',
    'If atrophy is real and measurable, suppression and extractiveness scores are justified by objective decay. If atrophy is minimal, the constraint operates primarily as theater and institutional power play, and theater_ratio should rise.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_atrophy_during_quiet_periods, empirical, 'Whether the catastrophe-anchored constraint causes actual cognitive/motor atrophy or is primarily a narrative/institutional phenomenon.').

omega_variable(
    kernel_reading_under_determination,
    'Is the choice between catastrophe-anchored, hybrid_dependency, and simulation-as-adequate readings determined by evidence about what maintains competence, or by pre-existing institutional commitments that evidence cannot overturn?',
    'Historical case study: have regulatory bodies ever shifted readings when presented with contradicting evidence (e.g., empirical data showing incident-free organizations maintained competence, or simulator-trained personnel performed equivalently to incident-exposed personnel)? Or do they reinterpret evidence to preserve the catastrophe-anchored frame?',
    'If readings are evidence-determined, the constraint can reclassify through learning. If readings are pre-commitment, the kernel exhibits path-dependency and the constraint is sustained by institutional inertia (piton characteristics) rather than functional necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_under_determination, conceptual, 'Whether readings are empirically falsifiable or institutionally entrenched.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t5, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(comp_tr_t5, observed).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(comp_tr_t10, observed).
narrative_ontology:measurement(comp_tr_t15, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 15, 0.42).
narrative_ontology:measurement_basis(comp_tr_t15, observed).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 20, 0.45).
narrative_ontology:measurement_basis(comp_tr_t20, observed).
narrative_ontology:measurement(comp_tr_t25, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(comp_tr_t25, observed).
narrative_ontology:measurement(comp_tr_t30, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 30, 0.43).
narrative_ontology:measurement_basis(comp_tr_t30, observed).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(comp_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t5, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(comp_be_t5, observed).
narrative_ontology:measurement(comp_be_t10, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(comp_be_t10, observed).
narrative_ontology:measurement(comp_be_t15, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(comp_be_t15, observed).
narrative_ontology:measurement(comp_be_t20, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(comp_be_t20, observed).
narrative_ontology:measurement(comp_be_t25, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 25, 0.71).
narrative_ontology:measurement_basis(comp_be_t25, observed).
narrative_ontology:measurement(comp_be_t30, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(comp_be_t30, observed).
narrative_ontology:measurement(comp_be_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(comp_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(comp_su_t0, observed).
narrative_ontology:measurement(comp_su_t5, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(comp_su_t5, observed).
narrative_ontology:measurement(comp_su_t10, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(comp_su_t10, observed).
narrative_ontology:measurement(comp_su_t15, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(comp_su_t15, observed).
narrative_ontology:measurement(comp_su_t20, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(comp_su_t20, observed).
narrative_ontology:measurement(comp_su_t25, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(comp_su_t25, observed).
narrative_ontology:measurement(comp_su_t30, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(comp_su_t30, observed).
narrative_ontology:measurement(comp_su_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(comp_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__catastrophe_as_necessary_anchor, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.12).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement__simulation_as_adequate_exercise).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement__hybrid_dependency).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, regulatory_mandate_for_incident_investigation).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, organizational_learning_from_near_miss_systems).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the competence_exercise_requirement kernel. The catastrophe-anchored reading asserts that only real high-stakes events provide irreducible competence exercise. Sibling readings (simulation_as_adequate_exercise, hybrid_dependency) instantiate different epistemologies of the same kernel commitment. All three stories share the same foundational problem (how to maintain perishable high-consequence skills) but author different ε values and beneficiary/victim structures because they propose different solutions. The catastrophe-anchored reading has the highest extractiveness (ε=0.68) because it sustains institutional authority through incident-dependent validation. The hybrid reading would have moderate extractiveness. The simulation-as-adequate reading would have lower extractiveness (no shadow doubt). Each story's epsilon is an intrinsic property of that reading's claim about what counts as adequate competence maintenance; the readings are not observables of a single constraint with measurement ambiguity — they are structurally distinct constraints grounded in different epistemologies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_requirement__catastrophe_as_necessary_anchor, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
