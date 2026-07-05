% ============================================================================
% CONSTRAINT STORY: competence_occupation__simulation_sufficiency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: competence_occupation__simulation_sufficiency
 *   human_readable: Simulation-Sufficiency Reading of Competence Occupation
 *   domain: high_reliability_organizations/safety_training
 *
 * SUMMARY:
 *   High-reliability organizations (aviation, nuclear operations, emergency
 *   medicine, offshore drilling, military readiness) face a structural
 *   problem: the catastrophic events that most reveal true competence are
 *   rare, and cannot ethically or practically be used as training occasions.
 *   The simulation-sufficiency reading resolves this by declaring that
 *   sufficiently frequent, sufficiently high-fidelity simulator drills occupy
 *   the same functional role as real exposure — that skill decay is a
 *   frequency/fidelity optimization problem, solvable by buying more
 *   simulator hours and better hardware. This declaration converts a
 *   contested epistemic question (does simulated stress transfer to real
 *   stress?) into an auditable compliance metric (hours logged, scenarios
 *   passed), and in doing so creates a large, durable market for simulation
 *   vendors and a legible certification artifact for regulators — while the
 *   actual transfer question remains empirically contested by the
 *   safety-research literature the compliance apparatus does not weight.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, 0.58).
domain_priors:suppression_score(competence_occupation__simulation_sufficiency, 0.51).
domain_priors:theater_ratio(competence_occupation__simulation_sufficiency, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, accessibility_collapse, 0.44).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__simulation_sufficiency, tangled_rope).
narrative_ontology:human_readable(competence_occupation__simulation_sufficiency, "Simulation-Sufficiency Reading of Competence Occupation").
narrative_ontology:topic_domain(competence_occupation__simulation_sufficiency, "high_reliability_organizations/safety_training").

domain_priors:requires_active_enforcement(competence_occupation__simulation_sufficiency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__simulation_sufficiency, '526a375e-3778-4c64-9226-ace4d185b5f9').
narrative_ontology:cs_kernel_codification('526a375e-3778-4c64-9226-ace4d185b5f9', formalized).
narrative_ontology:cs_authority_grounding('526a375e-3778-4c64-9226-ace4d185b5f9', extraction).
narrative_ontology:cs_interpretation_layer_present('526a375e-3778-4c64-9226-ace4d185b5f9').
narrative_ontology:cs_reading_relation('526a375e-3778-4c64-9226-ace4d185b5f9', competence_occupation__real_incident_necessity, forecloses).
narrative_ontology:cs_reading_relation('526a375e-3778-4c64-9226-ace4d185b5f9', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('526a375e-3778-4c64-9226-ace4d185b5f9', foundational, simulated_exposure_is_functionally_equivalent_to_real_exposure).
narrative_ontology:cs_axiom_status(simulated_exposure_is_functionally_equivalent_to_real_exposure, holdable).
narrative_ontology:cs_axiom_grounding('526a375e-3778-4c64-9226-ace4d185b5f9', simulated_exposure_is_functionally_equivalent_to_real_exposure, empirically_contingent).
narrative_ontology:cs_axiom('526a375e-3778-4c64-9226-ace4d185b5f9', secondary, skill_decay_is_a_frequency_fidelity_optimization_problem).
narrative_ontology:cs_axiom_status(skill_decay_is_a_frequency_fidelity_optimization_problem, holdable).
narrative_ontology:cs_axiom_grounding('526a375e-3778-4c64-9226-ace4d185b5f9', skill_decay_is_a_frequency_fidelity_optimization_problem, instrumental).
narrative_ontology:cs_reference_frame('526a375e-3778-4c64-9226-ace4d185b5f9', procedural_proficiency_via_repeated_exposure).
narrative_ontology:cs_drift_state('526a375e-3778-4c64-9226-ace4d185b5f9', post_simulator_industry_maturation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('526a375e-3778-4c64-9226-ace4d185b5f9', '').
narrative_ontology:cs_kernel_id(competence_occupation__simulation_sufficiency, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, simulation_vendor_industry).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, training_departments).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, regulatory_compliance_officers).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, incident_response_teams).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, downstream_public_safety).
narrative_ontology:constraint_vindicates(competence_occupation__simulation_sufficiency, training_hours_predict_competence).
narrative_ontology:constraint_vindicates(competence_occupation__simulation_sufficiency, fidelity_optimization_solves_decay).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and mandate the drill schedule, select simulation vendors, and certify operators as competent based on simulator hours and scenario completion rates. They control the metric that defines compliance and are evaluated by regulators on documented training throughput, not on real incident outcomes.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, training_departments, agenda_setter,
    institutional, generational, arbitrage, national).

% Sell simulator hardware, scenario libraries, and certification software. Revenue scales directly with mandated drill frequency and fidelity upgrades. Has no exposure to the consequences if simulated competence fails to transfer to a real event; contracts renew regardless of downstream performance.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, simulation_vendor_industry, beneficiary,
    organized, generational, arbitrage, global).

% Audit training records against simulator-hour thresholds because those thresholds are auditable, unlike tacit skill retention. Their professional standing depends on a checkable proxy existing; they benefit from the proxy's legibility even when they privately doubt its sufficiency.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, regulatory_compliance_officers, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__simulation_sufficiency, regulatory_compliance_officers, agenda_setter).

% Spend mandated hours in simulators that reset to baseline conditions after each session and never carry the physiological stress, ambiguity, or irreversible stakes of a real event. They are certified competent by the metric while privately reporting the simulator does not replicate the freeze, tunnel vision, or improvisation demands of an actual crisis. Their license and pay depend on passing the drills, not on contesting their sufficiency.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, frontline_operators, payer,
    moderate, biographical, trapped, local).

% Arrive at actual incidents having only ever executed the scenario library the simulator vendor scripted, which cannot anticipate a genuinely novel failure mode. They pay the cost of the gap between rehearsed and real, sometimes fatally, while the institution that certified them bears no correlated liability if the gap is treated as unforeseeable.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, incident_response_teams, payer,
    moderate, immediate, trapped, local).

% The public served by the certified operators has no visibility into whether competence certification tracks real capability. They bear the consequences of any decay the simulation regime fails to catch, without any voice in how sufficiency is defined.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, downstream_public_safety, payer,
    powerless, generational, trapped, regional).

% Publish evidence that simulator-transfer to real high-stress performance is empirically contested and that fidelity gains plateau against cost, but their findings compete against a training-industrial complex with lobbying and procurement relationships the researchers cannot match. They are cited in appendices, not in policy.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, hri_safety_researchers, excluded,
    moderate, civilizational, constrained, global).

% Reviews the full record of simulator certification against real incident outcomes to assess whether the compliance proxy correlates with the underlying competence it claims to measure.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, external_auditor, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a scalable, repeatable, low-cost, non-lethal mechanism to expose operators to procedural sequences and decision trees so that baseline procedural knowledge is genuinely refreshed across a large workforce without waiting for real incidents to occur.
% TRANSFER_FUNCTION: Moves training budget from institutions to simulation vendors, and moves certification risk from the institution (which sets the sufficiency standard) to frontline operators and incident response teams (who bear the consequences if the standard is wrong), while regulatory legibility accrues to compliance officers as an auditable artifact.
% ABSENT_VOICES: HRI safety researchers who have published on the empirical gap between simulated and real-incident performance are cited but not weighted in procurement or standard-setting; downstream public safety has no seat in defining what 'sufficient' exercise means for the people whose competence protects them.
% DISAPPEARANCE_RATIONALE: Training departments and vendors would say the world rearranges catastrophically — no scalable substitute exists and real-incident-only training is neither ethical nor available on demand. Incident response teams and researchers would say the world barely changes for actual competence, because the simulation regime was never shown to be the thing producing it; only the compliance paperwork would visibly change. The dispute is exactly the kernel contest this constraint is one reading of.
% FOUNDING_PROBLEM: High-reliability organizations cannot ethically or practically wait for real catastrophic incidents to occur in order to train operators, and cannot allow operators to enter high-stakes roles with zero rehearsal; some repeatable exercise mechanism was needed to occupy the competence-maintenance function between rare real events.
% FOUNDING_PROBLEM_CORROBORATION: Training departments and the simulation industry attest the founding problem remains fully live and solved by frequency/fidelity optimization. Independent HRI safety researchers and several post-incident review boards (outside the training-vendor relationship) attest that the founding problem — genuine competence occupation — is only partially addressed, and that simulator-hour compliance has drifted into a self-certifying proxy decoupled from measured real-world transfer; no party outside training departments, compliance officers, and vendors affirms sufficiency without qualification.
narrative_ontology:disappearance_verdict(competence_occupation__simulation_sufficiency, contested).
narrative_ontology:founding_problem_status(competence_occupation__simulation_sufficiency, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__simulation_sufficiency, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_occupation__simulation_sufficiency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__simulation_sufficiency, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.58) and theater ratio (0.62) both rise over the interval because the reading's own success — increasing adoption, increasing budget capture by vendors, increasing regulatory reliance on the hours-logged proxy — deepens the gap between what is measured (simulator throughput) and what was meant to be produced (transferable competence). Suppression (0.51) is moderate: operators are not physically coerced into compliance, but licensure and employment are conditioned on passing simulator certification, and dissenting safety research is structurally kept out of procurement decisions rather than actively silenced. Accessibility collapse (0.44) is moderate-low because alternative or supplementary training mechanisms (real drills, hybrid regimes) remain conceptually available and are actively argued for by researchers and some incident review boards — the alternatives have not been suppressed so much as out-competed for institutional budget and legibility. Resistance (0.47) reflects real, sustained pushback from incident response teams and safety researchers, even though that pushback has not yet altered the dominant standard.
 *
 * DIRECTIONALITY LOGIC:
 *   Training departments and simulation vendors sit at the beneficiary end: the drill regime is the source of budget, market, and legible compliance artifacts for them, and neither bears the cost if simulated competence fails to transfer. Regulatory compliance officers are a secondary beneficiary — the proxy gives them an auditable object, which is professionally valuable to them independent of whether it tracks real competence. Frontline operators and incident response teams are targets: they are certified by, and their careers depend on, a metric they may privately doubt, and they are the ones who pay when the gap between simulated and real performance manifests during an actual event. Downstream public safety is a diffuse, powerless, trapped payer with no seat in defining sufficiency at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (some repeatable, ethical, scalable mechanism must occupy the competence-maintenance function between rare real events) remains genuinely live — this is not a pure zombie mandate, and the coordination function is real. What has drifted is the *sufficiency claim* riding on top of that genuine function: frequency/fidelity optimization has become the treated-as-solved answer to a question the safety literature treats as open. Classifying this as tangled_rope rather than snare or piton preserves that distinction — there is a real coordination problem being solved (workforce-scale procedural exposure), and a real asymmetric extraction layered on top (compliance-legible training budget capture that outpaces demonstrated transfer benefit), both requiring active enforcement (licensure conditioning) to hold. Classifying it as snare would erase the genuine coordination value; classifying it as piton would wrongly imply no one is currently profiting from its maintenance, when the vendor industry demonstrably is.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulator_transfer_validity,
    'Does performance in high-fidelity simulators reliably predict performance under genuine catastrophic-incident stress, or does the simulator''s inherent reversibility and absence of true consequence produce a systematically different cognitive-performance profile than a real event?',
    'Longitudinal comparison of operators'' simulator certification scores against their documented performance in actual incidents they subsequently faced, controlled for incident severity and novelty relative to the trained scenario library.',
    'If transfer validity is low, the simulation-sufficiency reading is a false summit: the compliance proxy is being treated as equivalent to the underlying competence it claims to measure, without evidence, while a real extraction (vendor revenue, regulatory legibility) rides on the untested equivalence claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulator_transfer_validity, empirical, 'Whether simulator performance transfers to real-incident performance.').

omega_variable(
    kernel_reading_contest_location,
    'This constraint is one reading (simulation_sufficiency) of the competence_occupation kernel; the sibling readings (real_incident_necessity, hybrid_occupation) locate the disagreement differently. Where exactly does the contest sit — is it about whether ANY simulated exercise can occupy the kernel at all, or only about whether simulation ALONE (without supplementary mechanisms) is sufficient?',
    'Structural comparison of the three sibling constraint files: real_incident_necessity forecloses simulation''s adequacy entirely; hybrid_occupation accepts simulation as a necessary-but-insufficient component. Determining which structural element carries the actual dispute (kernel-adequacy vs. component-sufficiency) requires cross-reading analysis across all three files.',
    'If the dispute is really about component-sufficiency (this reading vs. hybrid_occupation) rather than kernel-adequacy (this reading vs. real_incident_necessity), then the beneficiary structure identified here (simulation vendor capture of the full training budget) is the more consequential axis of contest, since hybrid_occupation would only partially displace vendor revenue while real_incident_necessity would eliminate the simulation market''s centrality entirely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Where the kernel contest is structurally located among the three sibling readings.').

omega_variable(
    compliance_metric_capture,
    'Has the simulator-hours compliance metric been substantively captured by vendor and training-department interests (i.e., is the metric''s definition itself shaped by parties who profit from higher thresholds), or does it remain an independently-set proxy that happens to also benefit those parties?',
    'Trace the standard-setting history: who sat on the committees that defined minimum simulator hours and fidelity requirements, and what financial relationships did they hold with simulation vendors at the time.',
    'Direct capture would sharpen the classification toward the extractive pole (supporting tangled_rope or even snare-leaning read); independent proxy-setting that happens to align with vendor interest would support a more benign coordination-dominant reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compliance_metric_capture, empirical, 'Whether the compliance threshold itself was set independently or under vendor influence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__simulation_sufficiency, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__simulation_sufficiency, theater_ratio, 0, 0.28).
narrative_ontology:measurement(comp_tr_t4, competence_occupation__simulation_sufficiency, theater_ratio, 4, 0.35).
narrative_ontology:measurement(comp_tr_t8, competence_occupation__simulation_sufficiency, theater_ratio, 8, 0.42).
narrative_ontology:measurement(comp_tr_t12, competence_occupation__simulation_sufficiency, theater_ratio, 12, 0.49).
narrative_ontology:measurement(comp_tr_t16, competence_occupation__simulation_sufficiency, theater_ratio, 16, 0.55).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__simulation_sufficiency, theater_ratio, 20, 0.59).
narrative_ontology:measurement(comp_tr_t24, competence_occupation__simulation_sufficiency, theater_ratio, 24, 0.62).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__simulation_sufficiency, base_extractiveness, 0, 0.31).
narrative_ontology:measurement(comp_be_t4, competence_occupation__simulation_sufficiency, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(comp_be_t8, competence_occupation__simulation_sufficiency, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(comp_be_t12, competence_occupation__simulation_sufficiency, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(comp_be_t16, competence_occupation__simulation_sufficiency, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(comp_be_t20, competence_occupation__simulation_sufficiency, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(comp_be_t24, competence_occupation__simulation_sufficiency, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__simulation_sufficiency, suppression_requirement, 0, 0.33).
narrative_ontology:measurement(comp_su_t4, competence_occupation__simulation_sufficiency, suppression_requirement, 4, 0.37).
narrative_ontology:measurement(comp_su_t8, competence_occupation__simulation_sufficiency, suppression_requirement, 8, 0.41).
narrative_ontology:measurement(comp_su_t12, competence_occupation__simulation_sufficiency, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(comp_su_t16, competence_occupation__simulation_sufficiency, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(comp_su_t20, competence_occupation__simulation_sufficiency, suppression_requirement, 20, 0.49).
narrative_ontology:measurement(comp_su_t24, competence_occupation__simulation_sufficiency, suppression_requirement, 24, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__simulation_sufficiency, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_occupation__simulation_sufficiency, 0.12).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__real_incident_necessity).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__hybrid_occupation).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the competence_occupation kernel. simulation_sufficiency (this file) claims frequency/fidelity-optimized simulation alone occupies the kernel; real_incident_necessity claims only genuine catastrophic exposure can occupy it, foreclosing simulation's adequacy in principle; hybrid_occupation claims no single mechanism suffices and multiple mechanisms must run concurrently without a settled optimal mix. Each reading carries its own ε, beneficiary/victim structure, and classification; they are linked here via affects_constraints rather than merged into one story, per the ε-invariance decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
