% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__near_miss_as_bridge
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__near_miss_as_bridge, []).

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
 *   constraint_id: competence_retention_exercise__near_miss_as_bridge
 *   human_readable: Near-Miss-to-Simulator Validation Loop (Hybrid Competence Maintenance)
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   The standing arrangement in high-reliability industries — commercial
 *   aviation, nuclear power, anesthesiology and surgery, petrochemical
 *   processing — is a hybrid competence-maintenance machine: full-motion
 *   simulators and procedural rehearsal preserve routine skill, while
 *   confidential near-miss reporting systems, mandatory event reporting, and
 *   investigation review boards convert thousands of small real-world
 *   failures into the signal that decides what the simulators rehearse next.
 *   This story instantiates ONE reading of the competence_retention_exercise
 *   kernel — near_miss_as_bridge: near-miss feedback suffices to validate and
 *   update simulation, so catastrophes are neither necessary nor sufficient
 *   as teachers. The sibling readings (simulation_as_sufficient,
 *   catastrophe_as_necessary) are separate stories linked through
 *   network.affects_constraints; their contest is carried in omega variables,
 *   not inside this constraint's body. The epsilon referent is the standing
 *   hybrid arrangement itself, assessed by this reading's own lights: the
 *   loop delivers real learning value, and it also levies real uncompensated
 *   costs — reporting labor and career exposure on crew, compliance burdens
 *   on small operators, budget capture by the training apparatus, and
 *   residual tail risk silently allocated to the public. Time units are years
 *   from a 1990 baseline (t=0 approx 1990, t=36 approx 2026).
 *
 * KEY AGENTS:
 *   - hro_operators: Agenda-setter and primary beneficiary (institutional/constrained) — administers the loop and collects the avoided-loss returns
 *   - frontline_incident_reporters: Primary supplier seat (organized/constrained) — files the reports, bears collection costs and career exposure
 *   - clinical_frontline_reporters: Secondary supplier seat (moderate/trapped) — weaker just-culture protection, higher blame exposure
 *   - simulator_training_vendors: Concentrated budget beneficiary (powerful/arbitrage)
 *   - insurance_underwriters: Data-subsidy beneficiary (institutional/arbitrage)
 *   - protected_public: Diffuse beneficiary, indirect payer, and residual risk-bearer (powerless/trapped)
 *   - punished_reporters: Excluded voice — the proof cases the loop prefers not to learn from
 *   - small_regional_operators: Fixed-cost bearers priced against thin margins (moderate/trapped)
 *   - accident_investigation_boards: Analytical observer — sees both the near-miss record and the catastrophes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__near_miss_as_bridge, 0.48).
domain_priors:suppression_score(competence_retention_exercise__near_miss_as_bridge, 0.35).
domain_priors:theater_ratio(competence_retention_exercise__near_miss_as_bridge, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, extractiveness, 0.48).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__near_miss_as_bridge, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__near_miss_as_bridge, "Near-Miss-to-Simulator Validation Loop (Hybrid Competence Maintenance)").
narrative_ontology:topic_domain(competence_retention_exercise__near_miss_as_bridge, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_retention_exercise__near_miss_as_bridge).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__near_miss_as_bridge, 'c3a831b4-8f87-4b0f-b5f9-ea3524b61c1f').
narrative_ontology:cs_kernel_codification('c3a831b4-8f87-4b0f-b5f9-ea3524b61c1f', distributed).
narrative_ontology:cs_authority_grounding('c3a831b4-8f87-4b0f-b5f9-ea3524b61c1f', expertise).
narrative_ontology:cs_interpretation_layer_present('c3a831b4-8f87-4b0f-b5f9-ea3524b61c1f').
narrative_ontology:cs_reading_relation('c3a831b4-8f87-4b0f-b5f9-ea3524b61c1f', competence_retention_exercise__simulation_as_sufficient, forecloses).
narrative_ontology:cs_reading_relation('c3a831b4-8f87-4b0f-b5f9-ea3524b61c1f', competence_retention_exercise__catastrophe_as_necessary, forecloses).
narrative_ontology:cs_axiom('c3a831b4-8f87-4b0f-b5f9-ea3524b61c1f', foundational, near_miss_feedback_sufficient_for_validation).
narrative_ontology:cs_axiom_status(near_miss_feedback_sufficient_for_validation, holdable).
narrative_ontology:cs_axiom_grounding('c3a831b4-8f87-4b0f-b5f9-ea3524b61c1f', near_miss_feedback_sufficient_for_validation, empirically_contingent).
narrative_ontology:cs_axiom('c3a831b4-8f87-4b0f-b5f9-ea3524b61c1f', secondary, catastrophe_tuition_not_required).
narrative_ontology:cs_axiom_status(catastrophe_tuition_not_required, holdable).
narrative_ontology:cs_axiom_grounding('c3a831b4-8f87-4b0f-b5f9-ea3524b61c1f', catastrophe_tuition_not_required, empirically_contingent).
narrative_ontology:cs_reference_frame('c3a831b4-8f87-4b0f-b5f9-ea3524b61c1f', validated_sim_nearmiss_learning_loop).
narrative_ontology:cs_drift_state('c3a831b4-8f87-4b0f-b5f9-ea3524b61c1f', contemporary_data_rich_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c3a831b4-8f87-4b0f-b5f9-ea3524b61c1f', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, hro_operators).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, safety_regulators).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, simulator_training_vendors).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, insurance_underwriters).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, protected_public).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, safety_science_profession).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, frontline_incident_reporters).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, clinical_frontline_reporters).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, punished_reporters).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, small_regional_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, protected_public).
narrative_ontology:constraint_vindicates(competence_retention_exercise__near_miss_as_bridge, organizational_learning_loop_doctrine).
narrative_ontology:constraint_vindicates(competence_retention_exercise__near_miss_as_bridge, just_culture_reporting_principle).
narrative_ontology:constraint_vindicates(competence_retention_exercise__near_miss_as_bridge, high_reliability_organization_theory).
narrative_ontology:constraint_vindicates(competence_retention_exercise__near_miss_as_bridge, simulator_transfer_validity_method).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Airlines, nuclear plant operators, hospital systems, and chemical processors run the hybrid: they fund simulator fleets, operate confidential reporting programs, convene investigation review boards, and decide which findings reshape training curricula. They collect the arrangement's principal returns — competence maintained without catastrophe tuition, regulator goodwill, insurability — while setting how much investigative rigor the loop actually receives. Leaving the arrangement would mean abandoning structured competence maintenance entirely; no operator does.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, hro_operators, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__near_miss_as_bridge, hro_operators, beneficiary).

% Mandate event reporting, define just-culture protections, and audit training programs. They gain systemic risk visibility decades earlier than accident-driven rulemaking would provide. They cannot withdraw from oversight, and their credibility now rides on the reporting stream they compel others to feed.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, safety_regulators, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__near_miss_as_bridge, safety_regulators, agenda_setter).

% Build and update the simulator fleet and courseware; every near-miss finding that mandates a new scenario is a product line. They sell across industries and jurisdictions and can shift their mix if one sector's training budgets contract.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, simulator_training_vendors, beneficiary,
    powerful, biographical, arbitrage, global).

% Price premiums off incident and near-miss data streams; the richer the reported record, the better their actuarial position. They can withdraw capacity from poorly reporting sectors — leverage without operational entanglement.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, insurance_underwriters, beneficiary,
    institutional, biographical, arbitrage, global).

% Passengers, patients, and communities downstream of plants receive the safety dividend of maintained competence and pay for the apparatus through fares, bills, taxes, and premiums. They also hold the residual tail risk for the rare events the feedback stream under-samples; they neither observe the loop nor consent to that allocation. Exit from shared airspace, grids, and hospital systems is effectively unavailable.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, protected_public, beneficiary,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__near_miss_as_bridge, protected_public, payer).

% Human-factors researchers, resilience engineers, and investigation methodologists whose careers, journals, and consultancies are built on near-miss methodology. Mobile across academia and industry; their livelihood depends on the loop being treated as productive knowledge work.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, safety_science_profession, beneficiary,
    organized, generational, mobile, global).

% Pilots, controllers, and flight crews who file the reports and then fly the validated profiles. They contribute the raw material — time, candor, self-incrimination risk — protected by just-culture agreements of varying strength and backed by licensing-linked unions. Exit means leaving a licensed career. They also benefit from the competence the loop maintains, but they bear its collection costs first.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, frontline_incident_reporters, payer,
    organized, biographical, constrained, global).

% Nurses, anesthesiologists, and residents in institutions where reporting protections are weaker and blame cultures persist. They supply incident data under higher personal exposure than their aviation counterparts, with fewer organizational defenses and less portable reputations. Many simply stop filing; their silence never enters the record.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, clinical_frontline_reporters, payer,
    moderate, biographical, trapped, national).

% Individuals whose reports or involved incidents led to discipline, litigation exposure, or quiet blacklisting despite nominal protections. They would testify that the loop runs on trust the system periodically breaks; they are absent from curriculum-review bodies, and their cases are settled confidentially, removing their experience from the very record the loop learns from.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, punished_reporters, excluded,
    powerless, biographical, trapped, national).

% Regional carriers, community hospitals, and small process plants that bear the fixed costs of investigation infrastructure and simulator compliance against thin margins, and that feed data into systems whose learning benefits concentrate in larger peers. They cannot exit their regulated markets.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, small_regional_operators, payer,
    moderate, biographical, trapped, regional).

% Independent boards that see both the near-miss record and the catastrophes. They publish precursor analyses that periodically indict the adequacy of the feedback stream, and their findings carry evidentiary weight across every other seat. Nothing material flows to them from the arrangement's operation.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, accident_investigation_boards, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__near_miss_as_bridge, hro_operators).
narrative_ontology:fixing_cost_class(competence_retention_exercise__near_miss_as_bridge, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts thousands of private micro-failures into shared industry intelligence: standardized reporting taxonomies aggregate incidents no single firm would see enough of on its own, and the aggregated signal selects and validates which scenarios simulator curricula rehearse — maintaining rare-event competence without waiting for catastrophes to teach.
% TRANSFER_FUNCTION: Moves incident information and its embedded learning value upward from frontline reporters to operators, regulators, and training pipelines; moves training and investigation budgets from operators to vendors and safety departments; and, where the sufficiency assumption outruns the evidence, moves residual tail risk onto frontline staff and the public without their consent.
% ABSENT_VOICES: Punished reporters whose cases left the record through confidential settlement; clinical staff in blame-culture institutions whose incidents are never filed; communities hosting hazardous facilities whose tail-risk exposure is allocated without representation; and independent validators of simulator-to-reality transfer who sit outside the vendor-operator relationship.
% DISAPPEARANCE_RATIONALE: Without the coupled loop, industries would split into the two remaining strategies: pure simulation with unvalidated transfer, where skills decay silently against real failure modes, or catastrophe tuition, where relearning is paid in hulls, cores, and patients. Insurance markets would reprice on the added uncertainty, regulators would lose their early-warning stream, and the reporting commons that took decades to build would not spontaneously reassemble.
% FOUNDING_PROBLEM: Catastrophes are too rare and too expensive to serve as a routine teacher, yet competence against them decays without exercise — the early generations of aviation and nuclear regulation were literally written in blood, one disaster per rule.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: accident investigation board reports repeatedly document missed precursor signals preceding major events; the peer-reviewed human-factors and resilience-engineering literature documents skill decay without realistic exercise; insurer loss curves show premium spikes following lapses in drill and reporting regimes. Operator self-attestation alone would establish nothing — the corroborating seats collect no training budgets.
narrative_ontology:disappearance_verdict(competence_retention_exercise__near_miss_as_bridge, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__near_miss_as_bridge, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__near_miss_as_bridge, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_retention_exercise__near_miss_as_bridge, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__near_miss_as_bridge, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__near_miss_as_bridge_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_retention_exercise__near_miss_as_bridge, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_retention_exercise__near_miss_as_bridge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claim — tangled_rope — comes from structure: the loop solves a genuine collective-action problem (no single firm sees enough failures to train against rare events; the reporting commons aggregates them) AND carries asymmetric extraction through the same pipes (uncompensated reporting labor, blame exposure, budget capture, transferred tail risk), held together by active enforcement (mandatory reporting rules, certificate and accreditation consequences). Metrics describe operation at interval end: extractiveness 0.48 — substantial but bounded by the loop's real delivered value; suppression 0.35 — enforcement machinery exists (compelled reporting, licensure exposure) but just-culture norms damp punitive application; theater_ratio 0.41 — a growing minority of investigation activity produces closure paperwork rather than curriculum change; accessibility_collapse 0.35 — rival strategies (full-scale integrated drills, pure-simulator reliance) remain live and lawful, so understanding the loop collapses few alternatives; resistance 0.40 — union campaigns against punitive flight-data monitoring, managerial resistance to findings, and post-accident revivals of catastrophe-first doctrine. All three temporal series share one ten-point grid (t=0 to t=36, step 4); base_properties state the t=36 values. The suppression series traces one full enforcement wave — mandatory-reporting buildout (t=0-12), just-culture normalization (t=12-24), a post-accident enforcement spike (t=28), partial relaxation — driven by catastrophe salience; the oscillation itself functions as intermittent reinforcement, keeping reporter trust and program funding unstable between events. Extractiveness and theater rise monotonically beneath that wave: formalization layered bureaucracy onto the loop faster than it layered learning.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structure. From the operator seat the loop is a hard-won coordination achievement it funds and administers; from the reporter seats the same pipes are where their candor, time, and career safety are collected; from the vendor seat it is demand; from the public seat it is invisible — received as safety, paid as fare, and carrying an unpriced tail. Same-level lateral divergence: aviation and clinical frontline reporters hold similar nominal positions but different computed situations — organized, license-backed unions with just-culture contracts versus moderately powered staff in blame-prone institutions with weaker protection — so exit and protection, not rank, drive their divergent classifications. Institutional bystanders (regulators, insurers) experience the loop as an information subsidy that lowers their own cost of knowing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (operators, regulators, vendors, insurers, safety science, protected public) derive low directionality for those seats; victim declarations (frontline reporters, clinical reporters, punished reporters, small operators) derive high directionality. Reporter seats sit slightly inside the pure-target end because they also consume the competence the loop maintains — they fly the validated profiles — but their net position remains target-heavy. The protected public was considered for a directionality override (beneficiary declaration versus genuine tail-risk bearing); the override was rejected because overrides key on power atoms, and the powerless atom is shared with punished_reporters, whose derived target-side position is correct and must not be diluted. Suppression is authored as a raw structural property and is deliberately not scaled by power or scope; only extractiveness is scaled, amplifying for trapped targets (clinical staff, small operators, the public) and damping for mobile beneficiaries (vendors, insurers).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — catastrophe tuition is too rare and costly to be a routine teacher while competence decays without exercise — is live, so the arrangement is not mandatrophy-resolved, and the tangled_rope classification matters in both directions: it blocks a pure-extraction reading that would discard a functioning learning commons, and it blocks a pure-coordination reading that would excuse the capture riding on it. The threat trajectory is component-level piton drift: theater_ratio climbs steadily across the interval as investigation output shifts toward closure paperwork; if simulation fidelity ever closed the transfer gap, the near-miss apparatus would persist as ritual. The investigation-quality omega and the theater series are the tripwires. Receipt and cost: the gains demonstrably accrue to the operator seat (avoided losses, goodwill, data assets), and fundamental repair — compensated reporting, decaptured budgets, honest tail-risk accounting — is prohibitive for the fixers because its costs are concentrated on them while its benefits diffuse across everyone else, which is why known defects persist unfixed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the near_miss_as_bridge reading of the competence_retention_exercise kernel; how would classification shift if the story were authored under a sibling reading?',
    'Author and compile the sibling stories (competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise__catastrophe_as_necessary) and compare per-seat classifications over the same referent arrangement.',
    'Under catastrophe_as_necessary the standing arrangement reads as chronically under-exercised, with understated risk transfer to crew and public; under simulation_as_sufficient the near-miss apparatus reads as redundant overhead inflating measured extraction. The referent stays fixed; the reading-indexed epsilon and victim sets move.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame omega: this story is one reading of a three-reading kernel; sibling readings are separate constraints.').

omega_variable(
    precursor_distribution_representativeness,
    'Do near-miss and minor-failure populations statistically represent the precursors of full catastrophes, or do catastrophes draw on novel interaction pathways that near-misses systematically under-sample?',
    'Longitudinal linkage studies matching investigated near-miss populations to subsequent severe-event populations within the same systems, and comparison of precursor taxonomies across severity strata.',
    'If unrepresentative, the sufficiency premise over-closes the loop and the true uncompensated tail risk shifted to crew and public exceeds the authored extractiveness; the reading would drift toward catastrophe_as_necessary on the margin.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precursor_distribution_representativeness, empirical, 'Whether the bridge samples the right distribution of failure precursors.').

omega_variable(
    investigation_quality_ambiguity,
    'What fraction of near-miss investigations produce genuine curriculum and procedure change versus closure paperwork that satisfies audit without altering training?',
    'Audit tracing investigation outputs forward to documented syllabus revisions and observed crew-performance deltas.',
    'A higher paperwork share pushes theater_ratio toward piton-drift territory: the loop''s coordination function becomes increasingly performative while its costs continue unchanged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(investigation_quality_ambiguity, empirical, 'Functional versus performative share of the learning loop''s output.').

omega_variable(
    reporting_suppression_mechanism,
    'Is under-reporting in weak-protection organizations structural (disciplinary and legal-discovery exposure) or internalized (professional norms against burdening the system, self-blame)?',
    'Reporting-rate trajectories following just-culture policy adoption: persistent shortfalls after protection removal indicate internalized components.',
    'Internalized suppression travels with reporters across employers and understates the coercive content of the arrangement; structural suppression responds to policy intervention, internalized suppression does not.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reporting_suppression_mechanism, empirical, 'Structural versus internalized suppression of the loop''s raw material.').

omega_variable(
    training_budget_capture_extent,
    'How much of the budget flow the hybrid justifies purchases measurable competence versus rents captured by vendors and safety bureaucracies?',
    'Cost-effectiveness benchmarking of simulator-hour and investigation spending against validated outcome metrics across comparable operators.',
    'High capture raises the burden on operators and the public without raising delivered safety; the arrangement would shade toward pure extraction in its budget dimension while retaining its informational function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(training_budget_capture_extent, empirical, 'Rent share of the training-industrial budget flow.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__near_miss_as_bridge, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0, 0.18).
narrative_ontology:measurement(comp_tr_t4, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 4, 0.22).
narrative_ontology:measurement(comp_tr_t8, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 8, 0.26).
narrative_ontology:measurement(comp_tr_t12, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 12, 0.3).
narrative_ontology:measurement(comp_tr_t16, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 16, 0.33).
narrative_ontology:measurement(comp_tr_t20, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 20, 0.35).
narrative_ontology:measurement(comp_tr_t24, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 24, 0.37).
narrative_ontology:measurement(comp_tr_t28, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 28, 0.38).
narrative_ontology:measurement(comp_tr_t32, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 32, 0.4).
narrative_ontology:measurement(comp_tr_t36, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 36, 0.41).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(comp_be_t4, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(comp_be_t8, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(comp_be_t12, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 12, 0.39).
narrative_ontology:measurement(comp_be_t16, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 16, 0.41).
narrative_ontology:measurement(comp_be_t20, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 20, 0.43).
narrative_ontology:measurement(comp_be_t24, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 24, 0.44).
narrative_ontology:measurement(comp_be_t28, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 28, 0.45).
narrative_ontology:measurement(comp_be_t32, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 32, 0.47).
narrative_ontology:measurement(comp_be_t36, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 36, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(comp_su_t4, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(comp_su_t8, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(comp_su_t12, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(comp_su_t16, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(comp_su_t20, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(comp_su_t24, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 24, 0.33).
narrative_ontology:measurement(comp_su_t28, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 28, 0.38).
narrative_ontology:measurement(comp_su_t32, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 32, 0.36).
narrative_ontology:measurement(comp_su_t36, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 36, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__near_miss_as_bridge, information_standard).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise__simulation_as_sufficient).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise__catastrophe_as_necessary).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'how do organizations train for rare disasters' decomposes into three structurally distinct claims with different epsilon values — simulation_as_sufficient (simulation is structurally equivalent to real events), catastrophe_as_necessary (only real disasters teach), and this file's near_miss_as_bridge (near-miss feedback validates simulation; catastrophes are neither necessary nor sufficient alone). Each is a separate story with its own beneficiaries, victims, and classification, linked through network.affects_constraints. Contamination propagates along the family: a demonstrated simulator-transfer failure degrades simulation_as_sufficient's purity and pressures revival of catastrophe_as_necessary; conversely, a validated near-miss-driven curriculum fix strengthens this reading against both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
