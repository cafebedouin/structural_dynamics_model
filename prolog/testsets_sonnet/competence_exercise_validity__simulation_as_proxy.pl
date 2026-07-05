% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__simulation_as_proxy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__simulation_as_proxy, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: competence_exercise_validity__simulation_as_proxy
 *   human_readable: Simulation-as-Sufficient-Exercise Doctrine for Competence Retention
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint captures one reading of a contested doctrine about what
 *   validly exercises and verifies operational competence in
 *   high-consequence, low-frequency-catastrophe domains (nuclear operations,
 *   aviation, emergency response, industrial process safety). This reading
 *   holds that structured simulation — drills, tabletop exercises, simulator
 *   hours — constitutes sufficient exercise of competence, that safety
 *   records built under simulation-validated regimes demonstrate adequacy,
 *   and that regulatory compliance keyed to simulation completion is a
 *   sufficient evidentiary standard. Over the interval, the doctrine has
 *   drifted from a genuine coordination solution (simulation as scalable,
 *   safe alternative to impossible real-event training) toward an
 *   increasingly theatrical compliance artifact, where the auditability of
 *   simulation-hour logs displaces the underlying question of whether
 *   competence actually transferred.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__simulation_as_proxy, 0.58).
domain_priors:suppression_score(competence_exercise_validity__simulation_as_proxy, 0.52).
domain_priors:theater_ratio(competence_exercise_validity__simulation_as_proxy, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__simulation_as_proxy, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__simulation_as_proxy, "Simulation-as-Sufficient-Exercise Doctrine for Competence Retention").
narrative_ontology:topic_domain(competence_exercise_validity__simulation_as_proxy, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__simulation_as_proxy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__simulation_as_proxy, '39725223-c9d9-4ccf-9924-b92a743396de').
narrative_ontology:cs_kernel_codification('39725223-c9d9-4ccf-9924-b92a743396de', distributed).
narrative_ontology:cs_authority_grounding('39725223-c9d9-4ccf-9924-b92a743396de', practice).
narrative_ontology:cs_interpretation_layer_present('39725223-c9d9-4ccf-9924-b92a743396de').
narrative_ontology:cs_reading_relation('39725223-c9d9-4ccf-9924-b92a743396de', competence_exercise_validity__real_catastrophe_only, forecloses).
narrative_ontology:cs_reading_relation('39725223-c9d9-4ccf-9924-b92a743396de', competence_exercise_validity__continuous_refresh_hybrid, coexists_with).
narrative_ontology:cs_axiom('39725223-c9d9-4ccf-9924-b92a743396de', foundational, simulation_metrics_constitute_sufficient_proof_of_competence).
narrative_ontology:cs_axiom_status(simulation_metrics_constitute_sufficient_proof_of_competence, holdable).
narrative_ontology:cs_axiom_grounding('39725223-c9d9-4ccf-9924-b92a743396de', simulation_metrics_constitute_sufficient_proof_of_competence, empirically_contingent).
narrative_ontology:cs_axiom('39725223-c9d9-4ccf-9924-b92a743396de', secondary, safety_record_under_simulation_regime_demonstrates_adequacy).
narrative_ontology:cs_axiom_status(safety_record_under_simulation_regime_demonstrates_adequacy, holdable).
narrative_ontology:cs_axiom_grounding('39725223-c9d9-4ccf-9924-b92a743396de', safety_record_under_simulation_regime_demonstrates_adequacy, instrumental).
narrative_ontology:cs_reference_frame('39725223-c9d9-4ccf-9924-b92a743396de', pre_simulation_apprenticeship_verification).
narrative_ontology:cs_drift_state('39725223-c9d9-4ccf-9924-b92a743396de', contemporary_simulator_certification_regime, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('39725223-c9d9-4ccf-9924-b92a743396de', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, simulation_vendor_industry).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, facility_operators).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, regulatory_compliance_officers).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, frontline_operators).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, emergency_responders).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, downstream_public_in_catastrophe_zones).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the training and drill curriculum, decides which simulation vendors and scenarios count as certified exercise, and reports simulation completion rates to regulators as evidence of maintained competence. Avoids the cost, disruption, and liability exposure of running exercises against real degraded conditions. Captures cost savings directly.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, facility_operators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__simulation_as_proxy, facility_operators, beneficiary).

% Sells simulator hours, scenario packages, and certification software to facilities and regulators. Revenue depends on simulation being accepted as the validated unit of competence exercise rather than one component among several. Lobbies for regulatory language that names simulation hours as the compliance metric.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, simulation_vendor_industry, beneficiary,
    organized, biographical, arbitrage, national).

% Administers audits keyed to simulation-hour logs and drill-completion checklists because these are auditable and defensible in court; the audit trail itself becomes the object of compliance, decoupled from whether the exercises transferred real capability. Their institutional legitimacy depends on the audit framework being treated as sufficient.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, regulatory_compliance_officers, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__simulation_as_proxy, regulatory_compliance_officers, agenda_setter).

% Runs the simulated drills, accumulates certification hours, and is told this constitutes maintained readiness. Cannot decline the drills without losing certification, and cannot force exposure to conditions the simulator does not model (cascading failure, communication breakdown, panic dynamics). Bears the gap between rehearsed competence and real competence at the moment of an actual event.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, frontline_operators, payer,
    moderate, biographical, trapped, local).

% Arrives at incidents assuming facility personnel are competent per certification records. Discovers in real events that simulated competence does not transfer cleanly to conditions the simulation could not or did not model — sensory overload, equipment degradation, communication failure. Cannot audit the gap in advance; only discovers it during live response.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, emergency_responders, payer,
    moderate, immediate, trapped, regional).

% Bears the consequence if the competence gap manifests during an actual catastrophic event — the population whose safety the entire competence framework claims to protect, but who has no seat in deciding what counts as valid exercise and no visibility into simulation fidelity.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, downstream_public_in_catastrophe_zones, excluded,
    powerless, civilizational, trapped, regional).

% Studies post-incident reports and simulation-fidelity gaps across industries. Documents cases where simulation-validated personnel underperformed in real events relative to predicted competence, but has no enforcement authority over the certification regime.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, safety_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__simulation_as_proxy, facility_operators).
narrative_ontology:fixing_cost_class(competence_exercise_validity__simulation_as_proxy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine problem: real catastrophic events are too rare, too costly, and too dangerous to use as the primary training mechanism, so simulation lets an organization exercise procedures, build muscle memory, and coordinate large numbers of personnel without waiting for or manufacturing real disasters.
% TRANSFER_FUNCTION: Moves the cost of competence verification from expensive, risky, hard-to-audit real-world testing to cheap, auditable, vendor-supplied simulation hours; moves the residual risk of any capability gap from the certifying institutions onto frontline operators and, ultimately, the public present during an actual event.
% ABSENT_VOICES: Downstream public in catastrophe zones has no representation in curriculum design or fidelity standards; safety researchers documenting simulation-reality gaps are heard in academic venues but rarely given standing in the certification process itself.
% DISAPPEARANCE_RATIONALE: If simulation-as-sufficient-exercise certification vanished overnight, facilities would need to either fund substantially more expensive live-condition testing, accept explicit uncertainty about competence levels, or face regulatory gaps in compliance frameworks — the entire audit and vendor ecosystem built around simulation-hour metrics would need to be reconstructed around a different evidentiary standard.
% FOUNDING_PROBLEM: Real catastrophic events (reactor failures, structural collapses, mass-casualty incidents) are too rare and too destructive to use as routine competence tests, so organizations needed a repeatable, safe, scalable way to exercise emergency procedures and verify personnel readiness.
% FOUNDING_PROBLEM_CORROBORATION: Facility operators and simulation vendors attest the founding problem remains fully live and adequately solved by current simulation fidelity. Independent safety researchers and post-incident investigation boards (NTSB-style bodies, academic human-factors studies) attest that the founding problem has partially shifted — the coordination need for repeatable exercise is real and unresolved by simulation alone, while the verification function has been substituted by an auditable proxy that does not close the fidelity gap it claims to close.
narrative_ontology:disappearance_verdict(competence_exercise_validity__simulation_as_proxy, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__simulation_as_proxy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__simulation_as_proxy, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_validity__simulation_as_proxy, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__simulation_as_proxy, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__simulation_as_proxy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__simulation_as_proxy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__simulation_as_proxy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) and theater_ratio (0.61) both rose across the interval as the doctrine matured from an honest training innovation into an entrenched compliance metric — vendor lock-in, audit-defensibility, and institutional liability-shielding increasingly drive curriculum design rather than fidelity to real-event conditions. Suppression (0.52) reflects that frontline operators cannot opt out of simulation-based certification even when they judge it inadequate, and cannot force exposure to higher-fidelity or real-condition testing. Accessibility_collapse (0.48) and resistance (0.44) are moderate rather than extreme: alternative validation methods (live-condition drills, cross-facility incident exchanges, red-team exercises) exist and are used in some jurisdictions, so the collapse is partial, not total — this is not a mountain, it is a maintained arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Facility operators and regulatory compliance officers sit near the beneficiary end: they capture cost savings, legal defensibility, and institutional legitimacy from a simulation-sufficient standard, and their exit options (arbitrage across jurisdictions, discretion over compliance framework design) are wide. The simulation vendor industry benefits directly and structurally from simulation being named as sufficient rather than supplementary. Frontline operators and emergency responders are trapped payers — they cannot decline certification requirements and cannot manufacture the real-condition testing that would close the fidelity gap; the downstream public bears the tail risk with zero voice in curriculum design, which is why they are declared excluded rather than payer despite ultimately absorbing the largest potential cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — real catastrophe is too rare and too dangerous to use as the primary training mechanism — remains partially live: simulation genuinely solves a real coordination problem that has no easy substitute. Classifying this as tangled_rope rather than snare or piton preserves that genuine function while still registering the asymmetric extraction: the audit-defensibility function has grown to serve compliance officers' and vendors' institutional interests independent of whether it closes the actual competence gap. This is not mislabeled pure extraction (it retains real coordination value) nor mislabeled pure coordination (it requires active enforcement — mandatory certification, vendor-favorable regulatory language — and produces identifiable victims who bear a gap the beneficiaries do not disclose or bear themselves).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_ceiling,
    'Does current simulation technology have an irreducible fidelity ceiling that structurally cannot model cascading failure, sensory overload, and panic dynamics present in real catastrophic events, or is the gap merely a matter of insufficient current investment in simulation quality?',
    'Comparative post-incident analysis: track performance of simulation-certified personnel during actual catastrophic events against their simulation-predicted competence scores, across multiple industries and multiple simulation vendors, over a sufficiently long interval to accumulate statistical power.',
    'If the ceiling is structural (unfixable by better simulation), the simulation_as_proxy reading is a durable false summit — improved simulation cannot close the gap and the doctrine''s sufficiency claim is permanently unsupportable. If the gap is investment-driven and closeable, the reading could converge toward legitimate coordination as simulation fidelity improves, weakening the case for tangled_rope classification over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_ceiling, empirical, 'Whether simulation''s competence-validation gap is structural or merely underinvested.').

omega_variable(
    sibling_reading_disagreement_locus,
    'The three kernel readings (simulation_as_proxy, real_catastrophe_only, continuous_refresh_hybrid) disagree about where the burden of proof for competence adequacy should sit — is it located in the frequency of exercise, the fidelity of exercise, or the outcome record under simulation-based regimes? This constraint instantiates the outcome-record answer; the disagreement is over which observable legitimately settles the question.',
    'Structured elicitation across regulatory bodies, safety researchers, and frontline personnel unions to identify whether the disagreement is resolvable by better data (empirical) or is a genuine values disagreement about acceptable residual risk (preference).',
    'If the disagreement is empirical and resolvable, one reading will eventually dominate the regulatory consensus. If it is a preference disagreement about acceptable risk tolerance, all three readings may persist indefinitely as coexisting positions held by different regulatory jurisdictions and institutional cultures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_locus, conceptual, 'Whether the kernel''s reading-disagreement is resolvable by evidence or is an irreducible values dispute.').

omega_variable(
    coupling_between_theater_and_vendor_lobbying,
    'Is the rising theater_ratio observed over the interval driven primarily by simulation vendors lobbying regulators to enshrine simulation-hour metrics as the compliance standard, or by an independent institutional drift toward auditability-over-substance that would occur even absent vendor influence?',
    'Trace regulatory rule-making history for documented vendor lobbying activity correlated against theater_ratio inflection points in the measurement series.',
    'If vendor-driven, the tangled_rope classification is reinforced with a specific, addressable extraction mechanism (regulatory capture); if institutionally intrinsic, the extraction is more diffuse and harder to remedy through vendor-focused regulation alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coupling_between_theater_and_vendor_lobbying, empirical, 'Whether rising theater in the certification regime traces to vendor capture or intrinsic institutional drift.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__simulation_as_proxy, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__simulation_as_proxy, theater_ratio, 0, 0.3).
narrative_ontology:measurement(comp_tr_t4, competence_exercise_validity__simulation_as_proxy, theater_ratio, 4, 0.37).
narrative_ontology:measurement(comp_tr_t8, competence_exercise_validity__simulation_as_proxy, theater_ratio, 8, 0.44).
narrative_ontology:measurement(comp_tr_t12, competence_exercise_validity__simulation_as_proxy, theater_ratio, 12, 0.5).
narrative_ontology:measurement(comp_tr_t16, competence_exercise_validity__simulation_as_proxy, theater_ratio, 16, 0.55).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_validity__simulation_as_proxy, theater_ratio, 20, 0.59).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_validity__simulation_as_proxy, theater_ratio, 24, 0.61).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(comp_be_t4, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(comp_be_t8, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(comp_be_t12, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(comp_be_t16, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(comp_be_t20, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(comp_be_t24, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(comp_su_t4, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(comp_su_t8, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(comp_su_t12, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 12, 0.47).
narrative_ontology:measurement(comp_su_t16, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(comp_su_t20, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(comp_su_t24, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__simulation_as_proxy, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_validity__simulation_as_proxy, 0.12).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity__real_catastrophe_only).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity__continuous_refresh_hybrid).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the competence_exercise_validity kernel. simulation_as_proxy (this file) claims sufficiency of simulation-based validation; real_catastrophe_only claims simulation is categorically insufficient and only genuine catastrophic exposure exercises competence; continuous_refresh_hybrid claims simulation is necessary but requires continuous cyclical renewal rather than one-time or periodic sufficiency. Each reading has its own epsilon, its own beneficiary/victim structure, and its own classification — they are not the same constraint measured differently; they are three structurally distinct claims sharing a kernel (what validly exercises and verifies competence retention in high-consequence domains).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
