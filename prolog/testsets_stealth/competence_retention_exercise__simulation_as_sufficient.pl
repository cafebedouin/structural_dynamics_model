% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__simulation_as_sufficient
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__simulation_as_sufficient, []).

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
 *   constraint_id: competence_retention_exercise__simulation_as_sufficient
 *   human_readable: Simulation-Sufficiency Doctrine in Catastrophe-Avoidance Competence Maintenance
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   Over the last three decades, high-fidelity simulation became the backbone
 *   of competence maintenance in aviation, nuclear operations, surgery, and
 *   other high-hazard fields: regulators mandate simulator hours, accept
 *   check-ride scores as primary evidence of continued competence, and
 *   organize oversight around the artifacts the simulators produce. The
 *   load-bearing claim of this arrangement is the one this story
 *   instantiates: that the cognitive and procedural demands of the simulator
 *   are structurally equivalent to those of real events, so exercising them
 *   in simulation IS exercising catastrophe-avoidance competence. The
 *   claim/metric gap is deliberate and independent: the constraint is CLAIMED
 *   as tangled_rope (genuine rehearsal function plus real asymmetric
 *   position-taking), while the metrics are authored from the arrangement's
 *   observable operation — rising metric ritualization, accumulating
 *   vendor-side gains, and hardening certification machinery. Per the
 *   epsilon-invariance principle this file decomposes the colloquial label
 *   'how organizations keep catastrophe-avoidance competence' into one
 *   reading of a contested kernel; the sibling readings are separate files
 *   linked through network.affects_constraints, and epsilon here refers ONLY
 *   to the standing simulation-centric arrangement as this reading assesses
 *   it.
 *
 * KEY AGENTS:
 *   - certification_authorities: agenda setter (institutional/constrained) — mandates simulator hours and accepts check scores as competence evidence; oversight burden falls as competence reduces to a score
 *   - simulator_vendor_industry: primary beneficiary (organized/arbitrage) — collects the mandated training spend; sells across aviation, nuclear, medical, maritime
 *   - training_departments: beneficiary with agenda-setter reach (organized/identity_locked) — administers the metric their profession runs on; careers formed inside the simulation paradigm
 *   - insurers_and_underwriters: secondary beneficiary (powerful/arbitrage) — prices tail risk on the auditable compliance artifact
 *   - line_operators: primary target (organized/trapped) — credentials, scheduling, and promotion ride on check outcomes; fidelity doubts are career-costly
 *   - public_near_high_hazard_operations: diffuse victim (powerless/trapped) — bears residual catastrophe risk priced on the equivalence assumption; no seat in standard-setting
 *   - near_miss_analysis_programs: excluded voice (moderate/constrained) — holds the incident data that could test the equivalence claim; demoted to scenario fodder
 *   - operating_organizations: dual-positioned payer/beneficiary (institutional/constrained) — funds the apparatus and collects compliance value, insurance effects, and avoided first-exposure losses
 *   - transfer_of_training_researchers: analytical observer (moderate/analytical) — measures the transfer gap; influences advisory text only after long lag
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__simulation_as_sufficient, 0.55).
domain_priors:suppression_score(competence_retention_exercise__simulation_as_sufficient, 0.58).
domain_priors:theater_ratio(competence_retention_exercise__simulation_as_sufficient, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, extractiveness, 0.55).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, resistance, 0.46).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__simulation_as_sufficient, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__simulation_as_sufficient, "Simulation-Sufficiency Doctrine in Catastrophe-Avoidance Competence Maintenance").
narrative_ontology:topic_domain(competence_retention_exercise__simulation_as_sufficient, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_retention_exercise__simulation_as_sufficient).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__simulation_as_sufficient, 'ff2b336d-f9f7-4d22-bd06-c951358044f7').
narrative_ontology:cs_kernel_codification('ff2b336d-f9f7-4d22-bd06-c951358044f7', formalized).
narrative_ontology:cs_authority_grounding('ff2b336d-f9f7-4d22-bd06-c951358044f7', expertise).
narrative_ontology:cs_interpretation_layer_present('ff2b336d-f9f7-4d22-bd06-c951358044f7').
narrative_ontology:cs_reading_relation('ff2b336d-f9f7-4d22-bd06-c951358044f7', competence_retention_exercise__catastrophe_as_necessary, forecloses).
narrative_ontology:cs_reading_relation('ff2b336d-f9f7-4d22-bd06-c951358044f7', competence_retention_exercise__near_miss_as_bridge, coexists_with).
narrative_ontology:cs_axiom('ff2b336d-f9f7-4d22-bd06-c951358044f7', foundational, simulation_demands_structurally_equivalent_to_real_events).
narrative_ontology:cs_axiom_status(simulation_demands_structurally_equivalent_to_real_events, holdable).
narrative_ontology:cs_axiom_grounding('ff2b336d-f9f7-4d22-bd06-c951358044f7', simulation_demands_structurally_equivalent_to_real_events, empirically_contingent).
narrative_ontology:cs_axiom('ff2b336d-f9f7-4d22-bd06-c951358044f7', secondary, simulator_performance_validly_measures_catastrophe_avoidance_competence).
narrative_ontology:cs_axiom_status(simulator_performance_validly_measures_catastrophe_avoidance_competence, holdable).
narrative_ontology:cs_axiom_grounding('ff2b336d-f9f7-4d22-bd06-c951358044f7', simulator_performance_validly_measures_catastrophe_avoidance_competence, instrumental).
narrative_ontology:cs_reference_frame('ff2b336d-f9f7-4d22-bd06-c951358044f7', full_fidelity_equivalence_standard).
narrative_ontology:cs_drift_state('ff2b336d-f9f7-4d22-bd06-c951358044f7', contemporary_manual_skill_decay_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ff2b336d-f9f7-4d22-bd06-c951358044f7', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, simulator_vendor_industry).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, training_departments).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, certification_authorities).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, insurers_and_underwriters).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, line_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, public_near_high_hazard_operations).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, near_miss_analysis_programs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, operating_organizations).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, operating_organizations).
narrative_ontology:constraint_vindicates(competence_retention_exercise__simulation_as_sufficient, simulation_structural_equivalence_hypothesis).
narrative_ontology:constraint_vindicates(competence_retention_exercise__simulation_as_sufficient, positive_transfer_of_training_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promulgates the regulations that require qualifying simulator hours, approves devices and curricula, and accepts check outcomes as primary evidence of continued competence. Audits training providers and publishes compliance statistics. When competence reduces to a measurable score, its oversight burden falls and its jurisdiction grows with the apparatus it certifies; it is bound by statute and international harmonization agreements and cannot simply stop accepting the evidence its own rules created.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, certification_authorities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__simulation_as_sufficient, certification_authorities, beneficiary).

% Designs, builds, and services full-mission simulators and sells recurring training programs whose demand is created by the certification rules. Revenue depends on simulator hours satisfying competence requirements; a small number of firms dominate a consolidating global market and can shift sales across aviation, nuclear, medical, and maritime customers if any single sector's demand softens.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, simulator_vendor_industry, beneficiary,
    organized, biographical, arbitrage, global).

% Design curricula, run recurrent checks, and report pass rates to regulators. Budgets, headcount, and professional standing scale with the simulation paradigm; instructors' careers, methods, and status were formed inside it. Day-to-day they administer the standard, deciding which scenarios are rehearsed and how checks are scored. Leaving the paradigm would mean repudiating their own methodological formation, so criticism of fidelity tends to arrive as requests for more simulation rather than less.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, training_departments, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__simulation_as_sufficient, training_departments, agenda_setter).

% Accept documented simulator compliance as evidence of duty of care when pricing coverage for airlines, utilities, and hospital systems. A standardized, auditable artifact lowers assessment cost and creates a clean paper trail for claims defense. Tail risk remains on their books but is priced off the certificate rather than off observed field performance.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, insurers_and_underwriters, beneficiary,
    powerful, generational, arbitrage, global).

% Pilots, reactor operators, and surgical teams pass recurrent simulator checks to keep credentials; scheduling, promotion, and continued employment hinge on check outcomes. Their accumulated real-environment handling record weighs less in certification than simulator scores, and open doubt about fidelity is career-costly inside a system their union simultaneously negotiates within. There is no license path that bypasses the check.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, line_operators, payer,
    organized, biographical, trapped, global).

% Communities under flight paths, beside reactors, and around major hospitals bear the residual catastrophe risk that the competence regime exists to suppress. The adequacy of that regime is assumed to be guaranteed by simulator-based certification; they have no seat in training-standard bodies and cannot individually exit shared airspace, watersheds, or grids. They learn of fidelity gaps, if ever, from accident reports.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, public_near_high_hazard_operations, payer,
    powerless, generational, trapped, regional).

% Collect and analyze incident and near-miss data — the one stream of real-event evidence that accumulates without catastrophe. Under the sufficiency doctrine their output feeds scenario design as raw material rather than serving as an evidentiary check on simulator validity. They would argue for mandatory reconciliation of simulator predictions against near-miss records, but hold marginal seats in the committees where training standards are written.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, near_miss_analysis_programs, excluded,
    moderate, biographical, constrained, national).

% Airlines, nuclear utilities, and hospital systems purchase simulators, fund training departments, and absorb the mandated spend. In return they receive certifiable compliance, favorable insurance treatment, regulatory goodwill, and protection from first-exposure catastrophe during rare events. Their net position sits near the middle with a slight tilt toward gain, because avoided-loss and compliance value plausibly exceed direct outlay; they cannot exit the regime without exiting their license to operate.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, operating_organizations, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__simulation_as_sufficient, operating_organizations, beneficiary).

% Academic human-factors and training-science researchers who study whether simulator-acquired skill survives contact with real events, publishing on fidelity gaps, manual-skill decay, and startle response. They hold no enforcement power; their findings enter the regulatory system slowly, through advisory circulars and working groups, long after the practices they assess are entrenched.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, transfer_of_training_researchers, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__simulation_as_sufficient, simulator_vendor_industry).
narrative_ontology:fixing_cost_class(competence_retention_exercise__simulation_as_sufficient, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a safe, repeatable, schedulable venue in which rare high-consequence failure sequences are rehearsed, emergency procedures are drilled to fluency, and crew coordination is practiced under stress — solving the problem that catastrophe-avoidance skill cannot be maintained by waiting for catastrophes, and standardizing responses across thousands of dispersed teams.
% TRANSFER_FUNCTION: Moves mandated training budgets and tuition from operating organizations to simulator vendors and training providers; moves assurable evidence of competence from individual operators (in the form of check scores) to regulators and insurers; moves the risk of first-contact errors from live operations into the simulator, where they are cheap.
% ABSENT_VOICES: Near-miss analysts hold the only growing stock of real-event evidence and are demoted to scenario suppliers; veteran operators who doubt fidelity speak at career risk; communities bearing the residual tail risk have no seat at all; independent transfer researchers reach advisory text only after long lag. Unanimity in favor of the sufficiency doctrine arises partly because its strongest potential critics were never in the room where standards are set.
% DISAPPEARANCE_RATIONALE: If the simulation-sufficiency commitment vanished overnight, certification regimes would lose their primary evidence basis; hundreds of thousands of credential holders would hold a currency of competence no longer recognized; the vendor industry and training pipelines would contract abruptly; and operating organizations would scramble to rebuild validation from line experience, near-miss programs, and unscheduled live drills — a multi-year reorganization of safety-critical staffing and oversight.
% FOUNDING_PROBLEM: Catastrophes were historically the only teacher of catastrophe avoidance: organizations learned procedures, coordination, and respect for failure modes from crashes, fires, and meltdowns, paying in lives. The founding problem was how to build and keep catastrophe-avoidance competence without purchasing each lesson with a disaster.
% FOUNDING_PROBLEM_CORROBORATION: Independent accident-investigation bodies repeatedly recommend more frequent and more realistic scenario training in the aftermath of loss events; the peer-reviewed human-factors literature documents both the demonstrated value and the limits of skill transfer; labor unions representing line operators campaign for training investment. These sources corroborate from outside the vendor/training complex that the founding problem is live — while disputing whether simulation alone discharges it, which is precisely the kernel contest this story is one side of.
narrative_ontology:disappearance_verdict(competence_retention_exercise__simulation_as_sufficient, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__simulation_as_sufficient, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__simulation_as_sufficient, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_retention_exercise__simulation_as_sufficient, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__simulation_as_sufficient, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__simulation_as_sufficient_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_retention_exercise__simulation_as_sufficient, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_retention_exercise__simulation_as_sufficient_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits mid-range (0.55) because the arrangement genuinely solves the rehearsal-without-catastrophe problem while simultaneously routing large mandated flows to vendors and converting competence into a vendor-adjacent metric. Suppression (0.58) is authored as a raw structural property — it is NOT scaled by power or scope in the engine's computation — and reflects the closed alternative set: a line operator cannot validate competence through accumulated real-event handling when the credentialing gate accepts only simulator evidence, and questioning fidelity inside a training department is professionally costly. Theater ratio (0.38) reflects documented check-ride ritualization: known profiles rehearsed to pass, scenario variety narrowing toward what the check samples. Accessibility collapse is moderate-low (0.42) because alternatives persist — near-miss programs, line-oriented experience, unscheduled live drills — they are merely demoted rather than eliminated. Resistance (0.46) comes from veteran operators ('the aircraft does not behave like the box'), safety researchers publishing transfer-gap findings, and post-accident investigations citing manual-skill decay in crews current in the simulator. All three temporal series share one grid (points 0, 6, 12, 18, 24, 30) so every metric is authored at every examined time point; the trajectories show extraction accumulation (vendor consolidation plus metric primacy), Goodhart drift (theater rising past a third of activity), and enforcement intensification (certification machinery embedding simulator data ever deeper into oversight, e.g. advanced qualification and evidence-based-training programs).
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very different types from identical structural data. From the vendor and certification seats the arrangement is a coordination triumph: the commercial-air safety record improved dramatically across precisely the decades simulation became primary (attribution contested, but the correlation anchors their conviction). From the line-operator seat the same structure operates as a gatekeeping ritual distinct from skill maintenance — passing the check is the job; the check is not the hazard. From the near-miss-analyst seat the core claim is unfalsifiable by design, since the arrangement discounts exactly the data that could refute it. From the public seat it is an unpriced tail risk held by strangers. Same-power divergence is instructive: line_operators and training_departments both hold 'organized' power, yet occupy opposite structural positions, differentiated by exit (trapped versus identity_locked) and role — the department's professional self-concept is constituted by the paradigm the operator must submit to.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for simulator_vendor_industry (arbitrage-grade exit across industries pushes it nearest the beneficiary pole), training_departments (identity lock cements a low-d position), certification_authorities (collect administrative simplicity and expanded jurisdiction), and insurers (collect a cheap, auditable underwriting artifact). Victim declarations drive high directionality for line_operators (trapped: no credential path bypasses the check) and public_near_high_hazard_operations (powerless and trapped relative to shared airspace, watersheds, and grids). near_miss_analysis_programs sit high-d as an excluded voice whose corrective channel the arrangement devalues. operating_organizations are genuinely dual-positioned — they fund the apparatus yet collect compliance value, insurance effects, and avoided first-exposure catastrophe — netting near-symmetric with a slight beneficiary tilt; the role-plus-secondary_role declaration carries this without a directionality override, since the derivation reads the declared dual position. No overrides are used: the structural data produces the correct relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — building catastrophe-avoidance competence without paying for it in catastrophes — remains live, so this is not a resolved-mandate case. The classification work is prospective: the drift record shows the early signature of metric substitution (theater ratio climbing toward half of activity, check scores increasingly sampled by the taught). If decoupling completes, this reading degenerates WITHIN ITS OWN FRAME into an inertial case — ritual checks maintaining a competence nobody verifies — and the engine's lifecycle detection should catch that transition from the measurement series, not from relabeling. Conversely, the genuine rehearsal function (rare, dangerous, multi-failure scenarios drilled safely and repeatably) is real and irreplaceable by the catastrophe-based alternative at any acceptable cost; classifying the whole apparatus as pure extraction would discard that function, while classifying it as pure coordination would ignore the concentrated vendor gains, the demoted falsification channel, and the unpriced public tail. Tangled rope is the structurally honest claim, and the per-seat computation is expected to diverge sharply across seats — that divergence is the datum.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transfer_fidelity_gap,
    'Does competence exercised and measured in high-fidelity simulation survive contact with real events under startle, surprise, degraded cues, and compound novel failures?',
    'Longitudinal linkage of simulator check outcomes to blinded line observations, LOSA-style audit datasets, and crew performance during actual abnormal events; natural experiments where crews with strong simulator records encounter real upsets.',
    'If transfer is substantially partial, the equivalence axiom weakens, the arrangement''s cost (payment for certified confidence rather than retained competence) rises, and this reading drifts toward its catastrophe_as_necessary rival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_fidelity_gap, empirical, 'Whether simulated competence transfers to real-event performance.').

omega_variable(
    check_metric_decoupling,
    'Has simulator check performance decoupled from field competence as recurrent checks became high-stakes career gates?',
    'Time-series comparison of check-pass rates and scores against independent line-audit performance data across the interval.',
    'Rising decoupling converts the metric into ritual, pushing the arrangement from hybrid coordination/extraction toward inertial-theatrical dynamics and confirming the Goodhart arm of the drift record.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(check_metric_decoupling, empirical, 'Whether the simulator metric still tracks the competence it certifies.').

omega_variable(
    kernel_reading_position,
    'This constraint is the simulation_as_sufficient reading of the competence_retention_exercise kernel; how would the victim set and epsilon change under the sibling readings?',
    'Author and compile the sibling stories (catastrophe_as_necessary, near_miss_as_bridge) and compare computed classifications over the same referent arrangement.',
    'Under catastrophe_as_necessary the simulation-centric apparatus itself becomes the misallocation (epsilon rises sharply, vendors become pure rentiers); under near_miss_as_bridge simulator metrics are demoted to hypotheses requiring external validation, redistributing position toward whoever controls near-miss data. The disagreement is located at whether real-event contact carries non-reproducible competence-maintenance content.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    residual_risk_allocation_legitimacy,
    'Is the allocation legitimate under which those who set and profit from the equivalence standard bear none of the tail risk it underprices?',
    'Governance analysis: whether risk-bearing constituencies gain standing in training-standard bodies; comparison with regimes that seat public representatives on certification panels.',
    'If judged illegitimate, enforcement redesign (risk-bearer seats, mandatory disclosure of fidelity limits) changes the arrangement''s coercive profile without touching its rehearsal function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_risk_allocation_legitimacy, preference, 'Normative acceptability of the risk-bearing asymmetry that stabilizes the arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__simulation_as_sufficient, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t6, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 6, 0.22).
narrative_ontology:measurement_basis(comp_tr_t6, observed).
narrative_ontology:measurement(comp_tr_t12, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 12, 0.27).
narrative_ontology:measurement_basis(comp_tr_t12, observed).
narrative_ontology:measurement(comp_tr_t18, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 18, 0.31).
narrative_ontology:measurement_basis(comp_tr_t18, observed).
narrative_ontology:measurement(comp_tr_t24, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 24, 0.35).
narrative_ontology:measurement_basis(comp_tr_t24, observed).
narrative_ontology:measurement(comp_tr_t30, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(comp_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t6, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 6, 0.45).
narrative_ontology:measurement_basis(comp_be_t6, observed).
narrative_ontology:measurement(comp_be_t12, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 12, 0.49).
narrative_ontology:measurement_basis(comp_be_t12, observed).
narrative_ontology:measurement(comp_be_t18, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 18, 0.52).
narrative_ontology:measurement_basis(comp_be_t18, observed).
narrative_ontology:measurement(comp_be_t24, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 24, 0.54).
narrative_ontology:measurement_basis(comp_be_t24, observed).
narrative_ontology:measurement(comp_be_t30, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 30, 0.55).
narrative_ontology:measurement_basis(comp_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0, 0.44).
narrative_ontology:measurement_basis(comp_su_t0, observed).
narrative_ontology:measurement(comp_su_t6, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 6, 0.49).
narrative_ontology:measurement_basis(comp_su_t6, observed).
narrative_ontology:measurement(comp_su_t12, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 12, 0.53).
narrative_ontology:measurement_basis(comp_su_t12, observed).
narrative_ontology:measurement(comp_su_t18, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 18, 0.57).
narrative_ontology:measurement_basis(comp_su_t18, observed).
narrative_ontology:measurement(comp_su_t24, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 24, 0.6).
narrative_ontology:measurement_basis(comp_su_t24, observed).
narrative_ontology:measurement(comp_su_t30, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(comp_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__simulation_as_sufficient, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise__catastrophe_as_necessary).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise__near_miss_as_bridge).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'what keeps catastrophe-avoidance competence alive' decomposes into three structurally distinct constraints — one per reading of the competence_retention_exercise kernel. Each member has its own epsilon over the SAME referent arrangement (the standing simulation-centric competence-maintenance regime), its own beneficiary/victim structure, and its own classification; they are linked here and in the sibling files via affects_constraints. The upstream/downstream pressure runs from this reading outward: because simulation_as_sufficient currently governs certification practice, it shapes the resource environment of both siblings — the catastrophe_as_necessary reading survives only as critique (its prescribed mechanism is legally unusable), and the near_miss_as_bridge reading operates as a subordinate calibration channel. Epsilon differs across members because each reading locates the arrangement's costs differently: this reading prices vendor rents and metric ritualization; catastrophe_as_necessary prices the entire apparatus as misallocated confidence; near_miss_as_bridge prices the missing validation loop.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
