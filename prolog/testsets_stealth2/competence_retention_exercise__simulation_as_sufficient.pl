% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__simulation_as_sufficient
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
 *   human_readable: Simulation-Equivalence Doctrine in Catastrophe-Avoidance Certification
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   Across aviation, nuclear operations, surgery, and maritime transport, a
 *   codified doctrine treats qualified high-fidelity simulation as genuine
 *   exercise of catastrophe-avoidance competence: device time satisfies the
 *   same regulatory checkboxes as operational experience, checkrides and
 *   recurrency run on simulator performance, and insurers underwrite
 *   readiness from device records. Around the doctrine stands an
 *   infrastructure — device manufacturers, leased training centers, examiner
 *   corps, qualification standards — whose scale tracks every mandated hour.
 *   Live full-scale exercises and supervised field exposure persist but have
 *   steadily lost budget share and assessment authority to device-based
 *   certification. The epsilon referent is the standing device-based
 *   competence-maintenance arrangement, assessed by this reading's own
 *   lights: the reading endorses the equivalence, and the metrics below
 *   describe how the arrangement built on it actually operates. Claim and
 *   metrics are authored independently — claimed_type states the structure I
 *   believe true; the metric values state what I believe descriptively so.
 *   This story is one reading of a contested kernel; the committer structure
 *   lives in the omega variables, commentary.kernel_context, and the sibling
 *   story files.
 *
 * KEY AGENTS:
 *   - simulation_training_industry: primary beneficiary and agenda-shaper (institutional/arbitrage) — sells the mandated infrastructure and drafts the qualification criteria it must meet
 *   - safety_regulators: agenda setter (institutional/constrained) — codifies equivalence into certification law; collects auditability, bears liability
 *   - hro_executives: beneficiary (powerful/mobile) — harvests cost savings and clean compliance artifacts
 *   - internal_training_departments: beneficiary (organized/constrained) — headcount and authority scale with mandated device hours
 *   - insurers: beneficiary (institutional/arbitrage) — underwrites on the auditable proxy the doctrine produces
 *   - junior_operators: primary target (moderate/identity_locked) — career and competence self-concept denominated in simulator scores
 *   - veteran_operators: secondary target (organized/constrained) — retains a field-era baseline against which device time can be compared
 *   - accident_exposed_public: ultimate risk bearer (powerless/trapped) — absorbs whatever the simulation-reality gap releases
 *   - live_exercise_advocates: excluded voice (organized/constrained) — lost the budget share and the vote
 *   - safety_transfer_researchers: analytical observer (moderate/analytical) — measures transfer, holds no lever
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__simulation_as_sufficient, 0.6).
domain_priors:suppression_score(competence_retention_exercise__simulation_as_sufficient, 0.62).
domain_priors:theater_ratio(competence_retention_exercise__simulation_as_sufficient, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, extractiveness, 0.6).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__simulation_as_sufficient, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__simulation_as_sufficient, "Simulation-Equivalence Doctrine in Catastrophe-Avoidance Certification").
narrative_ontology:topic_domain(competence_retention_exercise__simulation_as_sufficient, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_retention_exercise__simulation_as_sufficient).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__simulation_as_sufficient, '66d93e45-3a4c-48e3-b033-9025c77f47a6').
narrative_ontology:cs_kernel_codification('66d93e45-3a4c-48e3-b033-9025c77f47a6', formalized).
narrative_ontology:cs_authority_grounding('66d93e45-3a4c-48e3-b033-9025c77f47a6', expertise).
narrative_ontology:cs_interpretation_layer_present('66d93e45-3a4c-48e3-b033-9025c77f47a6').
narrative_ontology:cs_reading_relation('66d93e45-3a4c-48e3-b033-9025c77f47a6', competence_retention_exercise__catastrophe_as_necessary, forecloses).
narrative_ontology:cs_reading_relation('66d93e45-3a4c-48e3-b033-9025c77f47a6', competence_retention_exercise__near_miss_as_bridge, influences).
narrative_ontology:cs_axiom('66d93e45-3a4c-48e3-b033-9025c77f47a6', foundational, simulator_demand_equivalence).
narrative_ontology:cs_axiom_status(simulator_demand_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('66d93e45-3a4c-48e3-b033-9025c77f47a6', simulator_demand_equivalence, empirically_contingent).
narrative_ontology:cs_axiom('66d93e45-3a4c-48e3-b033-9025c77f47a6', secondary, simulator_metrics_track_field_competence).
narrative_ontology:cs_axiom_status(simulator_metrics_track_field_competence, holdable).
narrative_ontology:cs_axiom_grounding('66d93e45-3a4c-48e3-b033-9025c77f47a6', simulator_metrics_track_field_competence, empirically_contingent).
narrative_ontology:cs_reference_frame('66d93e45-3a4c-48e3-b033-9025c77f47a6', qualified_simulator_parity).
narrative_ontology:cs_drift_state('66d93e45-3a4c-48e3-b033-9025c77f47a6', post_tail_risk_incident_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('66d93e45-3a4c-48e3-b033-9025c77f47a6', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, simulation_training_industry).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, hro_executives).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, internal_training_departments).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, safety_regulators).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, insurers).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, frontline_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, accident_exposed_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, veteran_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, junior_operators).
narrative_ontology:constraint_vindicates(competence_retention_exercise__simulation_as_sufficient, training_transfer_equivalence_hypothesis).
narrative_ontology:constraint_vindicates(competence_retention_exercise__simulation_as_sufficient, device_qualification_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, manufactures, leases, and services the high-fidelity devices and curricula that certification rules require; sits on the standards working groups where device qualification criteria are drafted; revenue scales with every mandated hour and every new device class written into regulation. Selling across aviation, medicine, energy, and maritime diversifies its exposure to any single regulator's revision cycle.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, simulation_training_industry, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__simulation_as_sufficient, simulation_training_industry, agenda_setter).

% Writes and enforces the rules that count qualified device time toward certification; qualifies devices against published test criteria; audits training records. Collects administrative benefit from auditable, standardized compliance artifacts, and bears institutional liability whenever crews certified under its rules fail visibly. Revising the equivalence standard would require reopening qualification rulemaking, renegotiating international harmonization agreements, and revalidating every approved syllabus — a multi-year undertaking with no budget line attached.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, safety_regulators, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__simulation_as_sufficient, safety_regulators, beneficiary).

% Runs airlines, hospitals, plants, and shipping lines whose training cost lines and audit outcomes depend on device-based certification; device hours are cheaper and schedulable than live exercises or supervised field exposure, and produce clean compliance artifacts for boards and insurers. Executive tenures rarely span the tail events where any gap between device and field would surface.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, hro_executives, beneficiary,
    powerful, biographical, mobile, global).

% Staffs the academies and simulation centers; headcount, facilities, and curricula scale with mandated device hours; instructor sign-off authority over checkrides concentrates assessment power in this seat. Defending the device-based syllabus is defending the department's existence.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, internal_training_departments, beneficiary,
    organized, biographical, constrained, regional).

% Underwrites fleet, facility, and malpractice risk using auditable simulator-compliance records as a primary readiness proxy; the records are standardized, comparable across insureds, and far cheaper to verify than field-performance histories. Premium models embed the equivalence premise.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, insurers, beneficiary,
    institutional, biographical, arbitrage, global).

% Trained partly in eras when line experience, live drills, and unrehearsed surprises carried more of the load; holds union and association positions; periodically petitions standards bodies after incidents where crews with clean device records mishandled real events. Cannot abandon the certification system without leaving the profession.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, veteran_operators, payer,
    organized, biographical, constrained, global).

% Entire formation — selection, instruction, checking, recurrency — runs through devices; professional standing, pay progression, and self-assessed competence are denominated in simulator scores; has no field-era baseline against which to notice what device time omits. Leaving the profession would forfeit the identity the scores constitute.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, junior_operators, payer,
    moderate, biographical, identity_locked, global).

% Flies on, is treated by, and lives downstream of crews and facilities certified under the device-based regime; bears the residual consequences whenever real events exceed what devices rehearsed; has no mechanism to observe or influence the fidelity assumptions embedded in certification, entering the process only through post-hoc investigation reports.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, accident_exposed_public, payer,
    powerless, biographical, trapped, global).

% Instructors, former investigators, and operators who press for retaining full-scale live drills, supervised line exposure, and consequence-bearing exercises alongside device time; their programs lost budget share as device hours came to satisfy the same regulatory checkboxes; they speak at conferences and in comment periods but hold no vote in qualification rulemaking.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, live_exercise_advocates, excluded,
    organized, biographical, constrained, global).

% Studies whether device performance predicts field performance, publishes training-transfer findings, and advises investigation boards; holds no enforcement or budget authority; results enter the operating system only when an incident forces attention.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, safety_transfer_researchers, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__simulation_as_sufficient, simulation_training_industry).
narrative_ontology:fixing_cost_class(competence_retention_exercise__simulation_as_sufficient, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a safe, repeatable, scalable medium for rehearsing rare high-consequence events, and a standardized, auditable instrument for assessing and certifying catastrophe-avoidance competence across sites, cohorts, and organizations.
% TRANSFER_FUNCTION: Moves training budgets and certification authority toward device ownership and simulator-metric assessment; moves residual operational risk from the training context onto real operations, where any gap between simulated and actual demands lands on crews and the public; moves the formation of operational judgment from accumulated field exposure to logged device time.
% ABSENT_VOICES: Operators and instructors who trained before device-based certification dominated, and the staff of defunded live-exercise programs, would contest the equivalence premise; they sit outside the standards committees and qualification boards where 'equivalent fidelity' is defined. Communities exposed to facility accidents hold no seat in curriculum design; their interests arrive only through investigation reports after the fact.
% DISAPPEARANCE_RATIONALE: Certification pipelines would stall — no accepted instrument would remain for signing off competence; training budgets, device fleets, examiner corps, and audit regimes would lose their organizing principle; insurers would lose their underwriting proxy; cohorts mid-training would face suspended licensure until alternative validation channels were rebuilt.
% FOUNDING_PROBLEM: Real catastrophes are too rare, dangerous, and expensive to train on directly: early rehearsal killed trainees and destroyed equipment, and organizations lacked any auditable way to certify readiness for events no examiner had personally survived.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigation boards and the peer-reviewed training-transfer literature — both outside the benefiting parties — attest the founding problem remains live: investigation reports repeatedly cite inadequate rehearsal of rare compound failures, and transfer studies continue to probe where device preparation falls short. No serious party disputes that the rehearsal-without-catastrophe problem exists; the live dispute is confined to whether the current instrument discharges it.
narrative_ontology:disappearance_verdict(competence_retention_exercise__simulation_as_sufficient, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__simulation_as_sufficient, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__simulation_as_sufficient, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_retention_exercise__simulation_as_sufficient, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__simulation_as_sufficient, 0.6, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.60: the arrangement's core is a real coordination achievement — rehearsing rare, lethal events without staging them — but layered onto it are mandated device purchases and lease hours priced well above marginal service cost, careers gated on simulator scores, and the quiet defunding of alternative validation channels whose budgets migrate to device procurement. Suppression 0.62: persistence depends on active machinery — qualification rulemaking, mandated hour minimums, audit regimes — not on participant preference; the mechanism is roughly 60% structural (mandates, budget allocation, harmonization treaties) and 40% internalized (a cohort that has never known another assessment route treats device scores as what competence is — see the suppression_structural_vs_internalized omega). Theater_ratio 0.40: genuine skill acquisition coexists with teaching-to-the-checkride, predictable scenario banks, and signature rituals; the series approaches the 0.5 Goodhart substitution threshold by interval end. Accessibility_collapse 0.52: alternatives (live drills, line exposure, near-miss-driven curricula) survive but are demoted to supplements. Resistance 0.48: episodic union petitions and researcher critique after visible incidents, never sustained revolt — the targets are divided. Identity-lock dynamics: junior operators' professional self-concept is constituted by simulator scores, so a constituency of targets defends the arrangement; breaking the equivalence frame would reprice their credential capital overnight, which is precisely why the frame is defended hardest by those it gates. The measurement series share one grid (interval indexes years from roughly 1985 wide device deployment to 2025) and drift monotonically — this is not a cyclical constraint; enforcement ratchets rather than oscillates. Coalition note: the powerless seat (accident_exposed_public) organizes only episodically, post-incident, when investigation reports momentarily hand it standing; between incidents its coalition potential is latent, not mounted.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats compute a different arrangement than the collector seats. From junior operators and the exposed public, the structure is an enforced gate: mandatory hours, score-gated careers, residual risk transferred to whoever meets the real event. From vendors, executives, training departments, and insurers, the same structure is the coordination achievement that made scalable, auditable readiness possible at all. Regulators straddle: they administer the gate, collect administrative benefit from its auditability, and bear the liability when certified crews fail visibly. Same-level divergence: veteran and junior operators hold the same nominal profession at comparable organized/moderate power, yet different exit profiles (constrained vs identity_locked) and different informational baselines — veterans can compare device time against remembered field time; juniors have no such baseline and therefore no experiential grounds for skepticism. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (vendors, executives, training departments, insurers, regulators) place those seats near the subsidy end; victim declarations (frontline operators, exposed public) place them near the target end. Exit modulation spreads each pole: arbitrage-grade exit (vendors selling across four industries, insurers rebalancing portfolios) sits nearest the beneficiary end; identity_locked juniors and the trapped public sit nearest the full-target end; constrained veterans and constrained regulators fall between. Receipt note: the gains land demonstrably on the vendor seat — mandated hours convert directly into contracted device sales and lease revenue — while executive savings are indirect and dispersed, which is why gain_flow names simulation_training_industry rather than the nominally larger executive seat. No directionality overrides are authored: the derivation chain already separates same-power actors through exit options (institutional arbitrage vs institutional constraint; organized constraint vs moderate identity lock), so an override keyed only to power atoms would blur distinctions the structural data already draws.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — rehearsing catastrophe without staging one — is live and permanently so, so no mandatrophy resolution is declared and the scaffold frame does not apply: the arrangement justifies itself as steady state, not transition. The live risks are drift-shaped rather than obsolescence-shaped. First, Goodhart drift: as device metrics become the goal, theater_ratio climbs toward the substitution threshold; the temporal series is the watchdog. Second, hardening: the receipt surface names a concentrated capturer (the device industry) and fixing is prohibitive for the only actor positioned to fix it (regulators facing rulemaking, harmonization renegotiation, and fleet-wide revalidation costs) — a profile consistent with a coordination-plus-extraction arrangement hardening its grip while its coordination core remains genuinely load-bearing. If a future interval showed theater_ratio sustained above 0.5 alongside flat field-transfer evidence, the arrangement would warrant review toward theatrical maintenance; nothing in the current data supports that verdict, and the still-substantial coordination function is what keeps this a hybrid rather than a pure extraction story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint instantiates the simulation_as_sufficient reading of the competence_retention_exercise kernel; the three readings disagree on which structural element is load-bearing for competence maintenance — device-mediated demand equivalence (this reading), the irreducible visceral stakes of real catastrophe (catastrophe_as_necessary), or near-miss feedback as the validating bridge (near_miss_as_bridge). Which element actually carries the classification?',
    'Comparative classification across the three sibling story files: adopt each reading''s referent in turn and observe which structural declarations (victim sets, exit profiles, enforcement dependencies) survive the swap.',
    'If catastrophe_as_necessary were adopted, the epsilon referent shifts to real-event exposure arrangements and this doctrine''s mandated-infrastructure costs read as suppression of the necessary channel; if near_miss_as_bridge were adopted, part of this story''s measured extraction reclassifies as the price of an unvalidated instrument awaiting field correction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one reading of a three-reading kernel; disagreement located in the load-bearing competence-maintenance mechanism.').

omega_variable(
    tail_transfer_validity_gap,
    'Does simulator performance predict real-event catastrophe-avoidance performance at the tails — rare, compound, surprising events outside the rehearsed scenario bank?',
    'Longitudinal linkage of simulator check records to subsequent real-event crew and facility performance; natural experiments where real events exceeded the trained envelope.',
    'If tail transfer fails, the doctrine''s effective extraction is higher than the scalar suggests (certification signals a competence it does not deliver, and the gap lands on crews and public); if it holds, most measured extraction is coordination cost rather than overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tail_transfer_validity_gap, empirical, 'Whether the equivalence premise survives at the event tail where catastrophe avoidance is actually tested.').

omega_variable(
    metric_gaming_extent,
    'How much simulator activity optimizes the metric rather than the competence — predictable scenario banks, teaching to the checkride, instructor signature culture?',
    'Blinded audits correlating scenario variability and pass-rate patterns against subsequent field incident data; comparison of checked performance with unobserved line performance.',
    'Higher gaming raises theater_ratio above the authored 0.40 and pushes the arrangement toward theatrical maintenance dynamics; lower gaming supports the coordination-cost reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_gaming_extent, empirical, 'Extent of Goodhart substitution of simulator scores for the competence they certify.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the closure of alternative validation channels structural (mandates, budget allocation, harmonization treaties) or internalized (a cohort that treats device scores as what competence is)?',
    'Post-liberalization trajectory: if live-exercise and field-exposure proposals still fail after regulatory openings remove the structural barriers, internalization dominates; rapid program revival indicates structural suppression.',
    'If internalized, effective suppression exceeds the structural measure and persists after any mandate is relaxed — the targets carry the gate with them; if structural, deregulation would rapidly reopen alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Split of the suppression mechanism between external barriers and fused professional identity.').

omega_variable(
    cross_sector_epsilon_uniformity,
    'Does the equivalence doctrine operate with uniform epsilon across the high-reliability sectors it governs, or does extraction concentrate where event rarity and device cost are highest?',
    'Sector-stratified replication of this story (aviation, nuclear, medicine, maritime) with per-sector victim and exit declarations; if per-sector epsilon diverges materially, decompose into a constraint family per the epsilon-invariance principle.',
    'Uniform epsilon supports single-story treatment; divergent epsilon means this file under-describes medicine (weakest device fidelity to real operative stress) and over-describes aviation (most mature device qualification).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cross_sector_epsilon_uniformity, empirical, 'Whether one epsilon legitimately covers all governed sectors or the label conceals several constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__simulation_as_sufficient, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crx_sim_suff_tr_t0, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(crx_sim_suff_tr_t0, observed).
narrative_ontology:measurement(crx_sim_suff_tr_t8, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 8, 0.22).
narrative_ontology:measurement_basis(crx_sim_suff_tr_t8, observed).
narrative_ontology:measurement(crx_sim_suff_tr_t16, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 16, 0.27).
narrative_ontology:measurement_basis(crx_sim_suff_tr_t16, observed).
narrative_ontology:measurement(crx_sim_suff_tr_t24, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 24, 0.32).
narrative_ontology:measurement_basis(crx_sim_suff_tr_t24, observed).
narrative_ontology:measurement(crx_sim_suff_tr_t32, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 32, 0.36).
narrative_ontology:measurement_basis(crx_sim_suff_tr_t32, observed).
narrative_ontology:measurement(crx_sim_suff_tr_t40, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 40, 0.4).
narrative_ontology:measurement_basis(crx_sim_suff_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(crx_sim_suff_be_t0, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(crx_sim_suff_be_t0, observed).
narrative_ontology:measurement(crx_sim_suff_be_t8, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 8, 0.44).
narrative_ontology:measurement_basis(crx_sim_suff_be_t8, observed).
narrative_ontology:measurement(crx_sim_suff_be_t16, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 16, 0.5).
narrative_ontology:measurement_basis(crx_sim_suff_be_t16, observed).
narrative_ontology:measurement(crx_sim_suff_be_t24, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 24, 0.55).
narrative_ontology:measurement_basis(crx_sim_suff_be_t24, observed).
narrative_ontology:measurement(crx_sim_suff_be_t32, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 32, 0.58).
narrative_ontology:measurement_basis(crx_sim_suff_be_t32, observed).
narrative_ontology:measurement(crx_sim_suff_be_t40, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 40, 0.6).
narrative_ontology:measurement_basis(crx_sim_suff_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(crx_sim_suff_su_t0, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(crx_sim_suff_su_t0, observed).
narrative_ontology:measurement(crx_sim_suff_su_t8, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 8, 0.48).
narrative_ontology:measurement_basis(crx_sim_suff_su_t8, observed).
narrative_ontology:measurement(crx_sim_suff_su_t16, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 16, 0.53).
narrative_ontology:measurement_basis(crx_sim_suff_su_t16, observed).
narrative_ontology:measurement(crx_sim_suff_su_t24, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 24, 0.57).
narrative_ontology:measurement_basis(crx_sim_suff_su_t24, observed).
narrative_ontology:measurement(crx_sim_suff_su_t32, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 32, 0.6).
narrative_ontology:measurement_basis(crx_sim_suff_su_t32, observed).
narrative_ontology:measurement(crx_sim_suff_su_t40, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(crx_sim_suff_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__simulation_as_sufficient, identity_coordination).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise__catastrophe_as_necessary).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise__near_miss_as_bridge).

% DUAL FORMULATION NOTE:
% The colloquial label 'competence retention exercise' covers three structurally distinct claims about what maintains catastrophe-avoidance competence. Per the epsilon-invariance principle, this story authors only the simulation_as_sufficient reading: epsilon is fixed for the standing device-based arrangement as this reading assesses it. Sibling stories (catastrophe_as_necessary, near_miss_as_bridge) carry their own epsilon, victim sets, and classifications; the citation gradient runs from the mature device-qualification literature upstream toward the contested sufficiency claim downstream.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
