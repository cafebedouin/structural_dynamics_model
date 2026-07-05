% ============================================================================
% CONSTRAINT STORY: competence_occupation__real_incident_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Real-Incident Necessity Reading of Competence Occupation
 *   domain: high_reliability_organizations/safety_training
 *
 * SUMMARY:
 *   This constraint is one reading of a contested kernel about what it takes
 *   to genuinely occupy 'competence' in high-reliability organizations
 *   (nuclear power, aviation, emergency medicine). This reading asserts that
 *   only actual catastrophic incidents provide the authentic conditions to
 *   occupy the competence kernel — simulations, drills, and procedural
 *   reinforcement are structurally insufficient because they cannot replicate
 *   the physiological stakes, ambiguity, and organizational strain of a real
 *   event. The reading is descriptively coherent (there is real evidence that
 *   some competencies degrade or fail unpredictably under authentic crisis
 *   stress in ways drills miss) but structurally unworkable as an
 *   organizational policy: it makes the achievement of safety (few or no
 *   incidents) the very condition that starves the organization of the
 *   'authentic' material the reading says competence requires. As
 *   theater_ratio rises across the measured interval, this reflects
 *   organizations increasingly performing incident-derived reverence
 *   (elevating survivor narratives, ritualizing after-action reviews,
 *   valorizing 'real-world' war stories) as a substitute for resolving the
 *   underlying tension, rather than the tension itself being resolved.
 *
 * KEY AGENTS:
 *   - veteran_operators_with_incident_experience: primary beneficiary of the reading's status economy
 *   - incident_investigation_bodies: institutional beneficiary and agenda-setter defining what counts as authentic
 *   - frontline_operators_without_incident_exposure: structurally unable to satisfy the reading no matter their effort
 *   - organizations_between_incidents: bear an unresolvable competence-maintenance bind as a direct consequence of their own safety success
 *   - regulators: analytical observers who must certify competence without being able to adjudicate the reading's premise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, 0.31).
domain_priors:suppression_score(competence_occupation__real_incident_necessity, 0.22).
domain_priors:theater_ratio(competence_occupation__real_incident_necessity, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, extractiveness, 0.31).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, mountain).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Real-Incident Necessity Reading of Competence Occupation").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "high_reliability_organizations/safety_training").

domain_priors:emerges_naturally(competence_occupation__real_incident_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, '0e2d185e-d8a6-459d-9822-738024a5c8ae').
narrative_ontology:cs_kernel_codification('0e2d185e-d8a6-459d-9822-738024a5c8ae', distributed).
narrative_ontology:cs_authority_grounding('0e2d185e-d8a6-459d-9822-738024a5c8ae', practice).
narrative_ontology:cs_interpretation_layer_present('0e2d185e-d8a6-459d-9822-738024a5c8ae').
narrative_ontology:cs_reading_relation('0e2d185e-d8a6-459d-9822-738024a5c8ae', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('0e2d185e-d8a6-459d-9822-738024a5c8ae', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('0e2d185e-d8a6-459d-9822-738024a5c8ae', foundational, authenticity_requires_genuine_stakes).
narrative_ontology:cs_axiom_status(authenticity_requires_genuine_stakes, holdable).
narrative_ontology:cs_axiom_grounding('0e2d185e-d8a6-459d-9822-738024a5c8ae', authenticity_requires_genuine_stakes, empirically_contingent).
narrative_ontology:cs_axiom('0e2d185e-d8a6-459d-9822-738024a5c8ae', secondary, rehearsed_conditions_cannot_replicate_crisis_phenomenology).
narrative_ontology:cs_axiom_status(rehearsed_conditions_cannot_replicate_crisis_phenomenology, holdable).
narrative_ontology:cs_axiom_grounding('0e2d185e-d8a6-459d-9822-738024a5c8ae', rehearsed_conditions_cannot_replicate_crisis_phenomenology, empirically_contingent).
narrative_ontology:cs_reference_frame('0e2d185e-d8a6-459d-9822-738024a5c8ae', post_incident_review_tradition).
narrative_ontology:cs_drift_state('0e2d185e-d8a6-459d-9822-738024a5c8ae', contemporary_low_incident_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0e2d185e-d8a6-459d-9822-738024a5c8ae', '').
narrative_ontology:cs_kernel_id(competence_occupation__real_incident_necessity, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__real_incident_necessity, incident_investigation_bodies).
narrative_ontology:constraint_beneficiary(competence_occupation__real_incident_necessity, veteran_operators_with_incident_experience).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, frontline_operators_without_incident_exposure).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, organizations_between_incidents).
narrative_ontology:constraint_vindicates(competence_occupation__real_incident_necessity, authenticity_of_experience_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Having lived through an actual catastrophic event (a reactor scram, a refinery fire, a near-miss collision), these operators are treated by their organizations as the only people who 'really' hold the competence the kernel demands. Their status, promotions, and informal authority accrue from having occupied the kernel the hard way, even though they did not choose the incident and often bear its trauma.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, veteran_operators_with_incident_experience, beneficiary,
    moderate, biographical, constrained, national).

% Boards that investigate catastrophic incidents (NTSB-style bodies, internal review panels) derive their entire evidentiary base and institutional relevance from the existence of real incidents. Their findings become the canonical record of what 'real' competence looks like, reinforcing the doctrine that only actual events generate authentic learning.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, incident_investigation_bodies, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__real_incident_necessity, incident_investigation_bodies, agenda_setter).

% The overwhelming majority of operators in a genuinely high-reliability organization will never personally experience a catastrophic incident, precisely because the organization is functioning. Under this reading, their competence is structurally suspect no matter how much they train, drill, or study — they cannot manufacture the authentic conditions the kernel requires. They pay in perpetual professional insecurity and in being passed over for the tacit authority that only incident-survivors carry.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, frontline_operators_without_incident_exposure, payer,
    powerless, biographical, trapped, local).

% Entire organizations — nuclear plants, air traffic control centers, hospital ICUs — that succeed at their core mission of preventing catastrophe are, under this reading, left with an unresolvable competence maintenance problem: the safer they are, the less authentic material they have to occupy the kernel with. They cannot buy, simulate, or legislate their way to the condition the reading demands, and no amount of investment converts absence-of-incident into presence-of-competence.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, organizations_between_incidents, payer,
    organized, generational, trapped, national).

% Regulatory bodies (safety authorities, licensing boards) watch this dynamic and must decide whether to certify competence based on drills and procedures (which this reading treats as inauthentic) or to accept that most operators will never meet the reading's bar. They observe the structural bind without being able to resolve it from outside.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, regulators, observer,
    institutional, generational, analytical, national).

% The entire simulation, drill, and procedural-reinforcement infrastructure that organizations rely on daily is, under this reading, definitionally unable to occupy the kernel — its designers and instructors would object that decades of simulator-trained pilots and operators demonstrably retain and exercise competence, but this reading's premise excludes their evidence from counting as authentic occupation.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, simulation_and_procedural_training_apparatus, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__real_incident_necessity, diffuse).
narrative_ontology:fixing_cost_class(competence_occupation__real_incident_necessity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reading coordinates institutional attention and prestige around a genuine epistemic worry: that rehearsed, low-stakes exercises may fail to replicate the physiological, decision-theoretic, and organizational conditions of a real catastrophic event, so competence claims grounded purely in drills could be systematically overconfident.
% TRANSFER_FUNCTION: Moves institutional authority, promotion pathways, and narrative legitimacy from operators and organizations who have never experienced an incident to those who have (or to the bodies that investigate incidents), regardless of whether the incident-experienced operators actually perform better prospectively.
% ABSENT_VOICES: The simulation and procedural-training apparatus, and the large majority of operators who will (fortunately) never be tested by a real catastrophe, would object that the reading renders the vast bulk of safety-relevant preparation structurally second-class no matter its demonstrated effectiveness — but they are not the ones writing incident reports or standing in the after-action spotlight, so their evidentiary contribution is undercounted.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, veteran incident-survivors would lose a specific form of institutional deference, and investigation bodies would lose their privileged claim to define 'real' competence — some organizational status hierarchies would rearrange. But safety practice itself (drills, procedures, audits) would likely continue unchanged, since it does not depend on this reading being correct. The parties dispute how much actually rests on it versus how much is prestige epiphenomenon.
% FOUNDING_PROBLEM: High-reliability organizations needed a way to distinguish operators who could actually perform under real catastrophic stress from those whose competence was untested, after several incidents revealed that drilled procedures broke down under authentic crisis conditions (fixation, panic, communication breakdown) in ways tabletop exercises had not predicted.
% FOUNDING_PROBLEM_CORROBORATION: Incident investigation bodies and veteran operators attest the problem remains live, citing specific cases where simulator-trained crews failed under real conditions. Human-factors researchers and training-effectiveness auditors outside the beneficiary set (academic safety-science literature, cross-industry benchmarking studies) report mixed evidence: some real-incident failures are attributable to simulator fidelity gaps that have since been closed, suggesting the founding problem is partially solved by simulation improvements rather than requiring real incidents per se.
narrative_ontology:disappearance_verdict(competence_occupation__real_incident_necessity, contested).
narrative_ontology:founding_problem_status(competence_occupation__real_incident_necessity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__real_incident_necessity, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_occupation__real_incident_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__real_incident_necessity, 0.31, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__real_incident_necessity_tests).

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
 *   Extractiveness is authored moderate-low (0.31) because the reading does not extract resources or labor in a conventional sense — its cost is borne mostly in status, promotion, and narrative legitimacy rather than material transfer. Suppression is low (0.22) because no one is coercively prevented from pursuing alternative competence-demonstration routes; the constraint operates through prestige allocation rather than exclusion. Theater ratio is authored as substantial and rising (0.35 to 0.58) because, absent real incidents to supply authentic material, organizations increasingly perform reverence for incident-derived competence (survivor testimonials, ritualized war-story sharing in briefings) as a substitute for a resolvable competence-verification mechanism — this is Goodhart-style metric substitution: performing deference to 'real experience' replaces demonstrating operational competence. Accessibility collapse is high (0.72): once an organization accepts the premise that only real incidents count, alternative competence-demonstration pathways (simulation certification, procedural audit, peer review) are experientially devalued even where they remain formally available. Resistance is substantial (0.61) because training designers, simulation vendors, and the majority of operators who will never face a real incident have strong reason to contest the reading's implicit judgment on their preparation.
 *
 * PERSPECTIVAL GAP:
 *   From the veteran-operator and investigation-body seats, this reading describes something they experienced as true: their competence was tested and proven in a way that no drill ever tested theirs. From the frontline-operator-without-incident-exposure seat, the same reading is experienced as a standing, uncloseable gap — a bar they cannot clear through any amount of diligence, because clearing it requires an event that (by design, if the organization is any good) will not occur. This is the structural core of the reading: it is a mountain-shaped claim (no human choice makes a drill into a real event) sitting atop a genuinely contestable empirical and policy question about whether the resulting competence-maintenance bind is a fact organizations must simply accept or a symptom of misspecifying what 'occupying the kernel' actually requires.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (veteran operators, investigation bodies) are declared low-d because the reading's operation elevates their status without requiring them to have sought the underlying catastrophe — they benefit from a structural sorting mechanism they did not design. Victims (frontline operators without incident exposure, and organizations between incidents) are declared high-d because they bear the cost of an unmeetable bar through no fault or choice of their own; their exit options are trapped/constrained because there is no route to 'more authentic' competence available to them within the reading's own terms. The simulation apparatus is excluded rather than a direct victim because its stake is evidentiary standing rather than direct cost, though its exclusion is structurally analogous.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists a simple mandatrophy verdict because its founding problem (drilled competence sometimes fails catastrophically under real stress) is empirically live in specific documented cases, but the reading's totalizing form — that ONLY real incidents count — outruns what the evidence supports and instead functions increasingly as status theater once safety improves and real incidents become rarer. The founding_problem_status is authored 'contested' precisely because whether the mandate has outlived its function depends on unresolved empirical questions about simulator fidelity gains, which is exactly the sibling-reading disagreement (simulation_sufficiency, hybrid_occupation) this kernel decomposition is meant to isolate rather than resolve within a single story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_criterion_ambiguity,
    'Is ''authenticity'' of the competence-occupying condition a genuine, measurable property of an event (distinguishing real incidents from high-fidelity simulations), or is it a socially constructed status marker that tracks survivor narrative rather than any demonstrable competence difference?',
    'Controlled comparison of post-incident performance between operators with real-incident exposure and operators with only high-fidelity simulation exposure, matched on role and tenure, across multiple high-reliability domains.',
    'If authenticity tracks a real performance difference, this reading captures a genuine (if organizationally unresolvable) constraint. If it tracks only narrative status, the reading is a false summit dressed as natural necessity — the ''mountain'' claim (emerges_naturally) would be undermined by a demonstrable beneficiary structure (status accrual to incident-survivors) with no corresponding competence gain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authenticity_criterion_ambiguity, empirical, 'Whether real-incident authenticity is a genuine competence determinant or a status-tracking artifact.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly do the three sibling readings (real_incident_necessity, simulation_sufficiency, hybrid_occupation) locate their disagreement — is it about the SUFFICIENCY of simulation, the NECESSITY of real incidents, or the CONFIGURATION of multiple mechanisms — and can an organization coherently hold more than one reading across different competence domains simultaneously?',
    'Cross-domain audit of how different high-reliability sectors (aviation vs. nuclear vs. emergency medicine) actually certify competence, checking whether sector-level practice maps cleanly onto one reading or blends elements of all three.',
    'If organizations already blend readings by domain, that argues for treating hybrid_occupation as the actually-operative reading in practice, with real_incident_necessity functioning mainly as a prestige overlay on top of a de facto hybrid system.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Where the three kernel readings'' disagreement is structurally located and whether they are mutually exclusive in practice.').

omega_variable(
    false_summit_beneficiary_check,
    'Given this mountain-claimed constraint declares beneficiaries (veteran operators, investigation bodies), is this a genuine structural feature of competence epistemics, or a constructed doctrine that happens to benefit those who already possess incident experience by devaluing alternative competence pathways?',
    'Track promotion and deference outcomes over time: if incident-survivor status predicts institutional authority independent of measured operational performance, that is evidence of constructed status allocation rather than natural epistemic necessity.',
    'Confirms or disconfirms the false-summit hypothesis (FSM) — if beneficiary capture is demonstrated independent of performance, the engine''s reclassification toward tangled_rope would be the structurally correct read despite the mountain claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_beneficiary_check, empirical, 'False-summit check: does incident-survivor status track real competence or captured institutional deference.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__real_incident_necessity, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__real_incident_necessity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(comp_tr_t8, competence_occupation__real_incident_necessity, theater_ratio, 8, 0.41).
narrative_ontology:measurement(comp_tr_t16, competence_occupation__real_incident_necessity, theater_ratio, 16, 0.46).
narrative_ontology:measurement(comp_tr_t24, competence_occupation__real_incident_necessity, theater_ratio, 24, 0.51).
narrative_ontology:measurement(comp_tr_t32, competence_occupation__real_incident_necessity, theater_ratio, 32, 0.55).
narrative_ontology:measurement(comp_tr_t40, competence_occupation__real_incident_necessity, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__real_incident_necessity, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(comp_be_t8, competence_occupation__real_incident_necessity, base_extractiveness, 8, 0.21).
narrative_ontology:measurement(comp_be_t16, competence_occupation__real_incident_necessity, base_extractiveness, 16, 0.24).
narrative_ontology:measurement(comp_be_t24, competence_occupation__real_incident_necessity, base_extractiveness, 24, 0.27).
narrative_ontology:measurement(comp_be_t32, competence_occupation__real_incident_necessity, base_extractiveness, 32, 0.29).
narrative_ontology:measurement(comp_be_t40, competence_occupation__real_incident_necessity, base_extractiveness, 40, 0.31).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(competence_occupation__real_incident_necessity, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__real_incident_necessity, identity_coordination).
narrative_ontology:boltzmann_floor_override(competence_occupation__real_incident_necessity, 0.1).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__hybrid_occupation).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language concept 'competence kernel occupation' per the ε-invariance principle. real_incident_necessity asserts real catastrophic incidents are the ONLY authentic occupying condition (foreclosing simulation_sufficiency's core premise that simulation alone suffices, since the two cannot both be true of the same organization's certification standard). It stands in an influences relationship to hybrid_occupation, since accepting any weight for real-incident authenticity changes the resource allocation and legitimacy conditions hybrid_occupation's multi-mechanism configuration must satisfy, without foreclosing hybrid_occupation's own claim that no single mechanism suffices. Each of the three stories carries its own epsilon, beneficiary/victim structure, and classification; they are linked here rather than merged into one variable-epsilon story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
