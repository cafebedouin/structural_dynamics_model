% ============================================================================
% CONSTRAINT STORY: competence_occupation__real_incident_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__real_incident_necessity, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: competence_occupation__real_incident_necessity
 *   human_readable: Real Incident Necessity for Competence Kernel Occupation
 *   domain: organizational/safety/high_reliability
 *
 * SUMMARY:
 *   This constraint story captures the reading that only actual catastrophic
 *   incidents — not simulations, not drills, not tabletop exercises — can
 *   genuinely occupy the competence kernel in high-reliability organizations.
 *   The reading presents this as a natural law of human performance under
 *   extreme stress: the neurophysiological, cognitive, and organizational
 *   dynamics of real catastrophe are irreducibly different from any
 *   simulation. The structural consequence is an unresolvable competence
 *   maintenance problem: HROs must maintain readiness for events that, by
 *   definition, must not happen. The constraint extracts the possibility of
 *   verified competence without catastrophe, suppresses alternative training
 *   paradigms by definitional fiat, and creates a theater of preparation
 *   where elaborate simulations are performed while the 'real test' remains
 *   an unacceptable horizon. No party benefits — catastrophes are
 *   unacceptable to operators, workers, regulators, and the public alike —
 *   yet the constraint persists as a framing that shapes training investment,
 *   regulatory standards, and professional identity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, 0.78).
domain_priors:suppression_score(competence_occupation__real_incident_necessity, 0.82).
domain_priors:theater_ratio(competence_occupation__real_incident_necessity, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, extractiveness, 0.78).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, mountain).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Real Incident Necessity for Competence Kernel Occupation").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "organizational/safety/high_reliability").

domain_priors:emerges_naturally(competence_occupation__real_incident_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, 'e04c7a16-e832-4343-ae05-3370c32cfade').
narrative_ontology:cs_kernel_codification('e04c7a16-e832-4343-ae05-3370c32cfade', distributed).
narrative_ontology:cs_authority_grounding('e04c7a16-e832-4343-ae05-3370c32cfade', practice).
narrative_ontology:cs_interpretation_layer_present('e04c7a16-e832-4343-ae05-3370c32cfade').
narrative_ontology:cs_reading_relation('e04c7a16-e832-4343-ae05-3370c32cfade', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('e04c7a16-e832-4343-ae05-3370c32cfade', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('e04c7a16-e832-4343-ae05-3370c32cfade', foundational, only_catastrophe_provides_authentic_conditions).
narrative_ontology:cs_axiom_status(only_catastrophe_provides_authentic_conditions, holdable).
narrative_ontology:cs_axiom_grounding('e04c7a16-e832-4343-ae05-3370c32cfade', only_catastrophe_provides_authentic_conditions, empirically_contingent).
narrative_ontology:cs_axiom('e04c7a16-e832-4343-ae05-3370c32cfade', secondary, simulation_irreducible_authenticity_gap).
narrative_ontology:cs_axiom_status(simulation_irreducible_authenticity_gap, holdable).
narrative_ontology:cs_axiom_grounding('e04c7a16-e832-4343-ae05-3370c32cfade', simulation_irreducible_authenticity_gap, empirically_contingent).
narrative_ontology:cs_reference_frame('e04c7a16-e832-4343-ae05-3370c32cfade', authentic_competence_requires_catastrophe).
narrative_ontology:cs_drift_state('e04c7a16-e832-4343-ae05-3370c32cfade', contemporary_simulation_advance_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e04c7a16-e832-4343-ae05-3370c32cfade', '').
narrative_ontology:cs_kernel_id(competence_occupation__real_incident_necessity, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, high_reliability_organizations).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, safety_critical_workforces).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, public_at_risk).
narrative_ontology:constraint_vindicates(competence_occupation__real_incident_necessity, competence_requires_authentic_stress).
narrative_ontology:constraint_vindicates(competence_occupation__real_incident_necessity, simulation_cannot_replicate_catastrophe_conditions).
narrative_ontology:constraint_vindicates(competence_occupation__real_incident_necessity, skill_decay_inevitable_without_real_events).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Nuclear operators, aviation authorities, chemical plants, and healthcare systems that must maintain readiness for catastrophic scenarios that occur once per decade or less. They invest heavily in simulation and drills but are told by this constraint that only actual catastrophes genuinely exercise the competence kernel. They bear the cost of maintaining shadow competence structures while knowing the 'real test' is an unacceptable event.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, high_reliability_organizations, payer,
    institutional, generational, constrained, global).

% Control room operators, airline pilots, surgical teams, and emergency responders whose professional identity fuses with the competence kernel. They cannot exit the requirement without abandoning their professional self-concept. They experience the constraint as a demand to be 'battle-tested' by events that would mean failure of their core mission.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, safety_critical_workforces, payer,
    organized, biographical, identity_locked, global).

% Populations living near nuclear facilities, under flight paths, or dependent on complex healthcare systems. They bear the consequence when competence decays because the 'authentic test' never arrives. They have no exit from the risk and no voice in how competence is maintained.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, public_at_risk, payer,
    powerless, immediate, trapped, regional).

% NRC, FAA, IAEA, and national healthcare accreditation bodies that set training standards. They must adjudicate between this reading (only real incidents suffice) and sibling readings (simulation sufficiency, hybrid occupation). Their standards determine what counts as 'competence occupation' for licensing and certification.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, training_regulators, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__real_incident_necessity, training_regulators, observer).

% Full-mission simulator manufacturers, VR training developers, and scenario design firms whose entire product premise is that high-fidelity simulation CAN occupy the competence kernel. They are structurally excluded by this reading's core premise. They would argue for simulation sufficiency but are kept out of the 'authenticity' definition.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, simulation_industry, excluded,
    organized, biographical, constrained, global).

% Academic researchers in human factors, organizational reliability, and skill acquisition who study whether simulation transfers to real catastrophe performance. They see the full structural landscape but have no authority to set standards. Their evidence is cited by all sides.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, competence_researchers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint claims to coordinate by identifying the single authentic mechanism for occupying the competence kernel: exposure to actual catastrophic conditions. It presents this as a natural law of competence maintenance rather than a choice.
% TRANSFER_FUNCTION: Transfers the burden of competence maintenance from proactive, continuous exercise (simulation, drills, audits) to dependence on rare, uncontrolled catastrophic events. Moves the cost from daily training investment to the unacceptable price of actual disasters. The public bears the risk transfer when competence decays between catastrophes.
% ABSENT_VOICES: Simulation industry practitioners who have built high-fidelity training for decades; frontline operators who maintain competence through continuous exercise without ever experiencing a catastrophe; training innovators developing stress-inoculation protocols that claim to bridge the authenticity gap. They are excluded by the definitional claim that 'only real incidents count.'
% DISAPPEARANCE_RATIONALE: If the claim that only real incidents suffice vanished overnight, HROs would restructure training budgets toward continuous multi-mechanism exercise (simulation + refresher + procedural reinforcement + line audits) as the primary competence maintenance strategy. Regulatory standards would shift from 'catastrophe experience' to 'demonstrated proficiency under validated stress conditions.' The simulation industry would become central rather than peripheral to competence certification.
% FOUNDING_PROBLEM: How to maintain genuine, deep competence for catastrophic scenarios that are too rare to practice, too dangerous to rehearse live, and too complex to fully specify in procedures — the 'black swan' readiness problem in high-reliability domains.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by multiple independent sources: the 1979 Three Mile Island investigation (Kemeny Commission) which identified 'lack of operating experience with severe accidents' as a root cause; the 1986 Challenger disaster (Rogers Commission) which found 'erosion of safety margins' without flight experience of O-ring failure; and the 2011 Fukushima Daiichi investigation (NAIIC) which cited 'insufficient severe accident management training.' These are outside the beneficiary set (there are no beneficiaries) and corroborate the problem's reality while disputing this reading's solution.
narrative_ontology:disappearance_verdict(competence_occupation__real_incident_necessity, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__real_incident_necessity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__real_incident_necessity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_occupation__real_incident_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__real_incident_necessity, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__real_incident_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, ExtMetricName, E),
    domain_priors:suppression_score(competence_occupation__real_incident_necessity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(competence_occupation__real_incident_necessity),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(competence_occupation__real_incident_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint extracts the ability to verify competence without catastrophe — HROs invest billions in training that this reading declares fundamentally inauthentic. Suppression is very high (0.82) because the 'only real incidents' claim definitionally excludes simulation-based validation, making alternative competence demonstrations illegible to the authority structure. Theater ratio (0.45) reflects that simulation infrastructure is built and maintained but treated as 'not the real thing' — a performative layer over the claimed necessity. Accessibility collapse (0.88) is near-mountain level because the constraint asserts a natural boundary: once you accept the premise, no alternative path to the competence kernel exists. Resistance (0.4) is moderate — HROs do invest heavily in simulation despite this reading, but the reading's authority in accident investigations keeps it structurally dominant.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (HROs, workforces, public) experience this as an extraction trap: they must prepare for what must not happen, using tools the constraint declares inauthentic. The agenda-setter seat (regulators) experiences it as a boundary condition for licensing: 'has this operator managed a real catastrophe?' The excluded seat (simulation industry) experiences it as a market closure. The observer seat (researchers) sees the empirical contest but has no leverage. The engine will compute these as different effective extraction values from the same base ε.
 *
 * DIRECTIONALITY LOGIC:
 *   All identified stakeholders are payers (victims) — the constraint has no beneficiaries, which is structurally unusual. Catastrophes are unacceptable to every party. HROs bear the cost of maintaining competence under a paradigm that declares their primary tools inauthentic. Workforces bear identity-locked exposure to an unattainable authenticity standard. The public bears risk transfer when competence decays. Regulators bear the agenda-setting burden of adjudicating an unresolvable standard. The simulation industry is excluded rather than coordinated. The absence of beneficiaries makes this a candidate for false summit mountain detection if emerges_naturally is claimed but the constraint operates extractively.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining black-swan readiness) is real and corroborated by multiple independent accident investigations. However, this reading's solution — 'only real catastrophes suffice' — has outlived its function. Modern simulation fidelity, stress-inoculation protocols, and cross-domain learning (aviation CRM → healthcare, nuclear → chemical) provide evidence that competence kernel occupation CAN be substantially achieved without catastrophe. The constraint persists because accident investigations repeatedly cite 'lack of severe accident experience' as a causal factor, creating a self-reinforcing loop: the only way to get the experience is to have the catastrophe. This is mandatrophy — the mandate (maintain competence) has been captured by a measurement standard (catastrophe experience) that makes the mandate unfulfillable without failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_boundary_empirical,
    'Is there an irreducible authenticity gap between even the highest-fidelity simulation and actual catastrophic conditions, or does the gap close asymptotically with simulation fidelity and stress-inoculation design?',
    'Longitudinal studies of operators who transition from simulation-only training to real catastrophic events (rare but existent: e.g., aviation crews experiencing hull loss, nuclear operators managing beyond-design-basis events). Measure competence transfer fidelity.',
    'If the gap is irreducible, this reading''s mountain claim gains empirical support. If the gap closes asymptotically, the constraint is a constructed barrier protecting the ''catastrophe experience'' credential.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authenticity_boundary_empirical, empirical, 'Whether the simulation-real gap is structural or technological.').

omega_variable(
    natural_law_vs_constructed_constraint,
    'Is the claim ''only real incidents occupy the competence kernel'' a discovery of human performance limits (natural law) or a constructed constraint that serves institutional interests in maintaining catastrophe-experience as a gatekeeping credential?',
    'Trace the genealogical origin of the claim: does it emerge from independent cognitive science, or from post-accident institutional narratives that protect existing authority structures? Compare with domains where simulation HAS been accepted as sufficient (e.g., commercial aviation upset recovery training).',
    'If constructed, this is a false summit mountain (FSM candidate) — emerges_naturally claimed but beneficiaries exist (credentialing bodies, accident investigation authorities). If natural, it is a genuine mountain with tragic implications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_constraint, conceptual, 'Whether the necessity claim is discovered or constructed.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative training paradigms structural (regulatory standards that require catastrophe experience) or internalized (professional identity that rejects ''simulation-only'' competence as inauthentic)?',
    'Compare suppression in domains with identical technical simulation capability but different regulatory regimes (e.g., US nuclear vs. French nuclear; commercial aviation vs. experimental test pilot training). If suppression varies by regime, it is structural. If suppression persists across regimes, it is internalized.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the workforce carries the suppression with them. This would increase effective extraction for the identity_locked workforce seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in interpersonal/institutional constraints.').

omega_variable(
    reading_relations_disagreement_location,
    'Where exactly does the structural disagreement between real_incident_necessity and simulation_sufficiency locate — in the definition of ''competence kernel,'' the threshold for ''occupation,'' the transfer metric, or the authority to adjudicate?',
    'Map the precise propositions each reading affirms/denies. The disagreement location determines whether relations are forecloses, coexists_with, or influences.',
    'If disagreement is on ''competence kernel'' definition → forecloses (different ontologies). If on ''occupation threshold'' → influences (same ontology, different calibration). If on ''authority to adjudicate'' → coexists_with (different governance frames).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_relations_disagreement_location, conceptual, 'Structural location of disagreement between sibling readings of the competence_occupation kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__real_incident_necessity, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(competence_occupation_real_incident_necessity_tr_t1970, competence_occupation__real_incident_necessity, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(competence_occupation_real_incident_necessity_tr_t1980, competence_occupation__real_incident_necessity, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(competence_occupation_real_incident_necessity_tr_t1990, competence_occupation__real_incident_necessity, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(competence_occupation_real_incident_necessity_tr_t2000, competence_occupation__real_incident_necessity, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(competence_occupation_real_incident_necessity_tr_t2010, competence_occupation__real_incident_necessity, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(competence_occupation_real_incident_necessity_tr_t2020, competence_occupation__real_incident_necessity, theater_ratio, 2020, 0.43).
narrative_ontology:measurement(competence_occupation_real_incident_necessity_tr_t2025, competence_occupation__real_incident_necessity, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(competence_occupation_real_incident_necessity_be_t1970, competence_occupation__real_incident_necessity, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(competence_occupation_real_incident_necessity_be_t1980, competence_occupation__real_incident_necessity, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(competence_occupation_real_incident_necessity_be_t1990, competence_occupation__real_incident_necessity, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(competence_occupation_real_incident_necessity_be_t2000, competence_occupation__real_incident_necessity, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(competence_occupation_real_incident_necessity_be_t2010, competence_occupation__real_incident_necessity, base_extractiveness, 2010, 0.72).
narrative_ontology:measurement(competence_occupation_real_incident_necessity_be_t2020, competence_occupation__real_incident_necessity, base_extractiveness, 2020, 0.76).
narrative_ontology:measurement(competence_occupation_real_incident_necessity_be_t2025, competence_occupation__real_incident_necessity, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(competence_occupation_real_incident_necessity_su_t1970, competence_occupation__real_incident_necessity, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(competence_occupation_real_incident_necessity_su_t1980, competence_occupation__real_incident_necessity, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(competence_occupation_real_incident_necessity_su_t1990, competence_occupation__real_incident_necessity, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(competence_occupation_real_incident_necessity_su_t2000, competence_occupation__real_incident_necessity, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(competence_occupation_real_incident_necessity_su_t2010, competence_occupation__real_incident_necessity, suppression_requirement, 2010, 0.78).
narrative_ontology:measurement(competence_occupation_real_incident_necessity_su_t2020, competence_occupation__real_incident_necessity, suppression_requirement, 2020, 0.8).
narrative_ontology:measurement(competence_occupation_real_incident_necessity_su_t2025, competence_occupation__real_incident_necessity, suppression_requirement, 2025, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__real_incident_necessity, identity_coordination).
narrative_ontology:boltzmann_floor_override(competence_occupation__real_incident_necessity, 0.08).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__hybrid_occupation).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, nuclear_operator_licensing).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, aviation_upset_recovery_training).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, healthcare_simulation_accreditation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the competence_occupation kernel. The kernel decomposes because 'competence kernel occupation' conflates: (1) the ontological claim about what constitutes authentic competence conditions (this reading), (2) the empirical claim about simulation transfer sufficiency (simulation_sufficiency), and (3) the pragmatic claim about multi-mechanism exercise without consensus (hybrid_occupation). Each has different ε, different stakeholder structures, and different classifications. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_occupation__real_incident_necessity, institutional, 0.35).
constraint_indexing:directionality_override(competence_occupation__real_incident_necessity, organized, 0.65).
constraint_indexing:directionality_override(competence_occupation__real_incident_necessity, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
