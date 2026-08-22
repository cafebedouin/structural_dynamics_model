% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__hybrid_degradation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__hybrid_degradation_reading, []).

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
 *   constraint_id: catastrophe_proxy_sufficiency__hybrid_degradation_reading
 *   human_readable: Simulation-Based Catastrophe Rehearsal Regime (Hybrid Degradation Reading)
 *   domain: safety engineering/organizational learning/high-reliability organizations
 *
 * SUMMARY:
 *   Since the mid-twentieth century, high-reliability industries — commercial
 *   aviation, nuclear power, emergency management, military operations — have
 *   replaced the impossible curriculum of real catastrophes with
 *   simulator-based recurrent training, and certification regimes convert
 *   logged simulator hours into legally recognized readiness. This story
 *   instantiates one reading of that arrangement: simulation genuinely
 *   maintains procedural fluency, while the capacities that only contact with
 *   genuine uncertainty builds — tacit situational judgment and physiological
 *   stress-response — decay across workforce generations, invisibly to the
 *   metrics that certify readiness. The arrangement solves a real collective
 *   problem (safe mass rehearsal of unrehearsable events) while layered on
 *   top of it sit recurring mandated revenues and a deferred, unpriced
 *   transfer of risk to future cohorts and neighboring publics. Family note:
 *   the colloquial label 'simulation suffices for catastrophe readiness'
 *   decomposes into four structurally distinct claims (see
 *   network.dual_formulation_note); this file authors only the
 *   hybrid-degradation reading, with epsilon 0.62 assessed over the standing
 *   simulation-certification arrangement by this reading's lights. Claimed
 *   type and metrics are authored independently: the type states what I
 *   believe structurally true; the metrics describe observed operation.
 *
 * KEY AGENTS:
 *   - regulatory_certification_authorities: Agenda-setter (institutional/constrained) — mandates recurrent simulation hours and accepts them as readiness evidence
 *   - commercial_simulation_training_industry: Primary beneficiary (organized/arbitrage) — collects recurring mandated training revenue
 *   - licensed_operating_organizations: Dual-positioned beneficiary and payer (institutional/constrained) — buys compliance and cost savings, absorbs tail losses after real events
 *   - current_line_operators: Payer with partial benefit (moderate/constrained) — supplies training hours, carries degraded stress-response capacity into real events
 *   - future_operator_cohorts: Deferred payer (powerless/constrained) — inherits a thinned experiential baseline with no seat in training design
 *   - high_consequence_facility_neighbors: Pure tail-risk bearer (powerless/trapped) — lives downstream of margin erosion they neither purchase nor observe
 *   - veteran_operators_with_event_experience: Excluded voice (moderate/identity_locked) — holds the decaying knowledge; testimony structurally marginal to compliance metrics
 *   - resilience_engineering_researchers: Analytical observer (moderate/analytical) — documents the decay dynamics no compliance metric registers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.62).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.47).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "Simulation-Based Catastrophe Rehearsal Regime (Hybrid Degradation Reading)").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "safety engineering/organizational learning/high-reliability organizations").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency__hybrid_degradation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__hybrid_degradation_reading, '9731d8ff-a26d-4041-b921-c44de60be0c4').
narrative_ontology:cs_kernel_codification('9731d8ff-a26d-4041-b921-c44de60be0c4', distributed).
narrative_ontology:cs_authority_grounding('9731d8ff-a26d-4041-b921-c44de60be0c4', expertise).
narrative_ontology:cs_interpretation_layer_present('9731d8ff-a26d-4041-b921-c44de60be0c4').
narrative_ontology:cs_reading_relation('9731d8ff-a26d-4041-b921-c44de60be0c4', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, forecloses).
narrative_ontology:cs_reading_relation('9731d8ff-a26d-4041-b921-c44de60be0c4', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('9731d8ff-a26d-4041-b921-c44de60be0c4', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, coexists_with).
narrative_ontology:cs_axiom('9731d8ff-a26d-4041-b921-c44de60be0c4', foundational, competence_is_multichannel).
narrative_ontology:cs_axiom_status(competence_is_multichannel, holdable).
narrative_ontology:cs_axiom_grounding('9731d8ff-a26d-4041-b921-c44de60be0c4', competence_is_multichannel, empirically_contingent).
narrative_ontology:cs_axiom('9731d8ff-a26d-4041-b921-c44de60be0c4', foundational, unsimulated_capacities_decay_generationally).
narrative_ontology:cs_axiom_status(unsimulated_capacities_decay_generationally, holdable).
narrative_ontology:cs_axiom_grounding('9731d8ff-a26d-4041-b921-c44de60be0c4', unsimulated_capacities_decay_generationally, empirically_contingent).
narrative_ontology:cs_reference_frame('9731d8ff-a26d-4041-b921-c44de60be0c4', partial_proxy_with_generational_decay).
narrative_ontology:cs_drift_state('9731d8ff-a26d-4041-b921-c44de60be0c4', contemporary_hours_based_compliance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9731d8ff-a26d-4041-b921-c44de60be0c4', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, commercial_simulation_training_industry).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, regulatory_certification_authorities).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, licensed_operating_organizations).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, current_line_operators).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, future_operator_cohorts).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, high_consequence_facility_neighbors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, current_line_operators).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, licensed_operating_organizations).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__hybrid_degradation_reading, transfer_of_training_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__hybrid_degradation_reading, logged_hours_readiness_equivalence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the number and kind of simulator hours a license holder must log, audits compliance, and accepts documented simulator performance as evidence of readiness. Its institutional standing rests on administering a legible, measurable readiness regime; after real accidents it adjusts scenario libraries and adds required maneuvers rather than questioning the underlying substitution. Stepping outside the regime would mean dismantling its own primary instrument of oversight.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, regulatory_certification_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Builds and operates simulators and training academies under multi-year contracts that renew automatically because the mandates renew. Revenue scales with mandated hours, so growth in requirements flows directly to order books. Capital is mobile in principle, but the business exists only inside the mandate structure that generates its demand.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, commercial_simulation_training_industry, beneficiary,
    organized, biographical, arbitrage, global).

% Airlines, reactor licensees, and emergency-services agencies buy simulator hours to satisfy mandates at far lower cost than live full-scale exercises, and present compliance certificates to regulators, courts, and insurers as proof of diligence. They also employ the people who will face the real event, so losses from unreadiness land on their balance sheets and reputations after the fact.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, licensed_operating_organizations, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__hybrid_degradation_reading, licensed_operating_organizations, payer).

% Spend recurring duty days in simulators practicing scripted emergencies until procedures are fluent, and most value the rehearsal. What the simulator cannot supply is the physiological signature of a real event — compressed time, ambiguous instrumentation, fear — so their stress-response repertoire is built from imagination and secondhand accounts. Changing professions mid-career forfeits seniority and licensure investment.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, current_line_operators, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__hybrid_degradation_reading, current_line_operators, beneficiary).

% Will qualify through the same pipeline after the last generation with live event experience has retired. They enter with excellent procedural scores and no inherited store of experiential judgment, and they hold no seat in the committees that design the training they will receive.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, future_operator_cohorts, payer,
    powerless, biographical, constrained, global).

% Live downwind, downstream, or underneath reactors, chemical complexes, and dense flight paths. Their protection depends on response capacity they neither purchase nor observe; the erosion in question is invisible to them until an event tests it. Relocating away from the hazard is possible in theory and ruinous in practice for most households.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, high_consequence_facility_neighbors, payer,
    powerless, biographical, trapped, local).

% Retiring captains, shift supervisors, and incident commanders who personally experienced major anomalies or trained under people who did. They report what the simulator lacks — the noise, the disbelief, the crew freezing — but their accounts arrive as anecdotes, resist quantification in audit metrics, and age out of the workforce. Their professional identity is fused with the craft; they do not leave the trade, they leave the payroll.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, veteran_operators_with_event_experience, excluded,
    moderate, biographical, identity_locked, national).

% Study how organizations retain and lose readiness across generations: transfer-of-training results, resilience engineering, post-accident analyses. They publish the decay evidence and command no enforcement power; uptake of their findings depends on the very apparatus whose premises the findings unsettle.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, resilience_engineering_researchers, observer,
    moderate, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__hybrid_degradation_reading, commercial_simulation_training_industry).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__hybrid_degradation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives every licensed operator, across thousands of shifts and many jurisdictions, rehearsal of rare high-consequence events that cannot be scheduled, survived, or afforded as real experiences — addressing the collective problem that crews meeting an unprecedented emergency for the first time fail in characteristic, predictable ways.
% TRANSFER_FUNCTION: Moves fee income from licensed operating organizations (passed through to fares, tariffs, and ratepayers) to simulator manufacturers and training academies; moves documented readiness evidence upward to regulators, courts, and insurers; and moves unpriced risk — the decay of tacit knowledge and stress-response capacity — forward onto future cohorts and neighboring publics.
% ABSENT_VOICES: Veteran operators with live event experience would testify that simulation does not reproduce the physiological and cognitive conditions of a real catastrophe; they are absent because their knowledge is anecdotal, unquantifiable in audit metrics, and retiring out of the workforce. Front-line operators' qualitative reports of stress gaps surface mainly inside debriefs administered by the same compliance apparatus that certifies the training.
% DISAPPEARANCE_RATIONALE: If mandated simulation-based certification vanished overnight, training would revert to fragmented apprenticeship and occasional live drills, the simulator and training-academy market would collapse, insurers would lose the readiness evidence they price against, and licenses could not be issued or renewed under current rules — the entire readiness-assurance economy would have to be rebuilt around some other evidence of competence.
% FOUNDING_PROBLEM: Real catastrophes are too rare, too dangerous, and too expensive to serve as routine training events, while crews facing an unprecedented emergency for the first time fail in characteristic ways; the arrangement was built so that every operator could rehearse events no one expects to live through twice.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigation reports and the human-factors research literature — seats that neither sell training nor administer licenses — corroborate that first-contact emergencies produce characteristic novice-pattern failures and that rehearsal changes outcomes. Corroboration from outside the benefiting parties is therefore available, though the training industry also cites the same sources commercially.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__hybrid_degradation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__hybrid_degradation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__hybrid_degradation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__hybrid_degradation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_proxy_sufficiency__hybrid_degradation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.62: the arrangement delivers a real service (mass safe rehearsal) while rents accumulate on top of it — mandated hours guarantee demand regardless of marginal training value, and the principal harm (capacity decay) is deferred, probabilistic, and priced by no one. Suppression is 0.47 and structural, not internalized: licensure gates and mandate compliance compel participation, and alternatives (full-scale live exercises, event-immersion apprenticeship) are crowded out economically rather than banned — hence accessibility_collapse 0.35, with real alternatives persisting at prohibitive cost. Resistance is 0.38: operators negotiate training burdens, unions push back on mandate growth, and researchers publish critiques, but no constituency organizes against the substitution itself because its cost is invisible until a real event. Theater_ratio 0.41 reflects growing check-ride optimization: simulator sessions increasingly rehearse the examinable script rather than the uncertain event. The measurement series run on one shared time grid (every tracked metric authored at every point 0-60) so no end-state value is silently substituted backward. The suppression_requirement series is stepped rather than smooth: each real accident historically triggered a ratchet of additional mandated scenarios and hours — enforcement intensification layered onto the regime after the fact — which is why enforcement-capacity change is tracked here alongside extraction and theater. The veteran seat is identity_locked: craft mastery fused with professional identity means exit is aging out, not conversion, so the carriers of the decaying knowledge cannot relocate it.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the agenda-setter and beneficiary seats (regulator, training industry, operating organizations in their compliance posture), the arrangement is functioning infrastructure: legible, auditable, cheaper than the alternative, and credited with decades without certain classes of accident. From the payer seats (current operators, future cohorts, facility neighbors), the same structure operates as compulsory payment plus an uninsured bet placed on their behalf — procedural scores are excellent precisely where the degraded capacities are unmeasured. The researcher seat sees the mechanism itself: a regime whose evidence of readiness is generated by the same instruments whose insufficiency it presupposes. The excluded veteran voice marks the sharpest gap: holders of the atrophying knowledge describe what simulation lacks, but their testimony arrives as anecdote, fails audit quantification, and exits the workforce faster than it can accumulate institutional weight.
 *
 * DIRECTIONALITY LOGIC:
 *   The training industry sits nearest the beneficiary pole (d near 0.05): it receives the mandate's revenue flow and bears none of the deferred risk. The regulator is a beneficiary with a partial offset (d around 0.15): it collects institutional justification and fee-supported oversight, but its reputation absorbs damage when a real event exposes unreadiness. Licensed operating organizations are net beneficiaries with material exposure (d around 0.35): they save enormously against live exercises and purchase litigation-ready compliance, yet employ the people who meet the real event. Current line operators sit past symmetric (d around 0.6): they pay recurring hours and carry the degraded stress-response repertoire, offset by genuine procedural skill received. Future cohorts sit further toward target (d around 0.75): they receive the thinned baseline and had no voice in designing it. Facility neighbors are near-full targets (d around 0.85): they supply nothing, receive nothing from the training arrangement, and hold the tail risk. Researchers are observers near symmetric by construction. Beneficiary and victim declarations map onto these relationships directly; no directionality overrides were needed because the derivation from declared structure reproduces these positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — rehearsing events that cannot be scheduled, survived, or afforded as real experiences — remains live, so this is not a resolved-mandatrophy case: the arrangement has not outlived its function, it has accumulated a second, unacknowledged function (revenue and compliance production) alongside the first. The tangled_rope classification prevents both symmetrical mislabelings: a pure-coordination reading would erase the layered rents and the unpriced intergenerational risk transfer; a pure-extraction reading would erase the genuine achievement that mass rehearsal changed first-contact outcomes. The R5 interview supports the live finding from outside the benefiting parties: accident investigators and the human-factors literature attest both the founding problem and the reality of first-contact failure patterns. The mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds no zombie flag — consistent with the claim. The trajectory worth watching is theater_ratio: if it continues climbing past roughly 0.5 while the founding problem stays live, the arrangement is drifting toward performance-maintained operation, and the classification should be revisited on later measurements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (hybrid_degradation_reading) of the catastrophe_proxy_sufficiency kernel; what would switch to a sibling reading change structurally, and where exactly is the disagreement located?',
    'Not resolvable by data alone: the readings are alternative framings of the same standing arrangement, each instantiated as its own constraint file. Resolution is a framing commitment; the empirical input that pressures the choice is transfer-of-training evidence on the stress-response and tacit channels.',
    'Adopting simulation_as_proxy_catastrophe_reading collapses epsilon toward the coordination-cost floor and dissolves the deferred-victim set; adopting catastrophe_necessity_reading raises epsilon sharply and widens victims to all simulation-trained cohorts; adopting simulation_fidelity_threshold makes sufficiency a technology parameter rather than a categorical property. The disagreement is located in whether simulator practice transfers to stress-response and tacit-knowledge channels, and over what timescale.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame omega: this story instantiates one reading of a four-reading kernel; sibling readings are separate constraints.').

omega_variable(
    tacit_decay_measurability,
    'Can tacit-knowledge and stress-response decay be detected prospectively, before a real event reveals it, or only retrospectively?',
    'Longitudinal cohort studies comparing sim-only and mixed-exposure crews on anomaly-response measures; natural experiments supplied by rare real events and by organizations that retain live-exercise programs.',
    'Prospective detectability would let the regime internalize the decay cost and support remediation within the existing structure; retrospective-only detectability means the harm is systematically externalized onto the very event that reveals it, strengthening the extraction reading of the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_decay_measurability, empirical, 'Whether the degradation mechanism this reading posits is observable in advance or only in the aftermath of real events.').

omega_variable(
    counterfactual_baseline_identification,
    'Is the procedural competence the regime maintains actually attributable to simulation, or would selection, adjacent real anomalies, and general experience sustain it anyway?',
    'Cross-jurisdiction and cross-era comparison of organizations with different simulation intensities, matched for traffic density, technology, and workforce experience.',
    'If baseline competence persists under light simulation reliance, the genuine coordination share shrinks and the rent share of the measured extraction rises; if simulation is doing real causal work, the coordination function claim strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_baseline_identification, empirical, 'Identification problem: no organization is observed without the regime, so attribution of maintained competence to simulation is inferential.').

omega_variable(
    certification_rent_vs_service,
    'Does recurrent-training pricing track the delivered cost of simulator provision and instruction, or a dependency rent sustained by the mandate structure?',
    'Cost-structure disclosure, competitive tendering experiments, and price comparison across jurisdictions with differently structured mandates.',
    'A wide cost-to-price gap establishes the industry seat as collecting rent through manufactured regulatory dependency; a narrow gap supports a service-payment framing and lowers the extraction attribution to that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certification_rent_vs_service, empirical, 'Whether the training industry''s recurring revenue is compensation for service or capture of the mandate.').

omega_variable(
    turnover_masking_timescale,
    'Does workforce turnover reset or mask generational decay, or does decay compound as the last event-experienced generation retires out?',
    'Cohort modeling against retirement curves in long-tenure licensed professions versus short-tenure sectors, correlated with post-event performance data.',
    'Compounding decay confirms the generational timescale central to this reading and implies accelerating risk as veteran cohorts exit; turnover masking would push the effective timescale beyond institutional planning horizons and weaken near-term classification consequences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(turnover_masking_timescale, empirical, 'How workforce renewal interacts with the posited generational decay curve.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(cata_tr_t10, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(cata_tr_t20, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(cata_tr_t30, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(cata_tr_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement(cata_tr_t50, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 50, 0.36).
narrative_ontology:measurement(cata_tr_t60, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 60, 0.41).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(cata_be_t10, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(cata_be_t20, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(cata_be_t30, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement(cata_be_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 40, 0.56).
narrative_ontology:measurement(cata_be_t50, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 50, 0.59).
narrative_ontology:measurement(cata_be_t60, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 60, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(cata_su_t10, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(cata_su_t20, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(cata_su_t30, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 30, 0.36).
narrative_ontology:measurement(cata_su_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement(cata_su_t50, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 50, 0.43).
narrative_ontology:measurement(cata_su_t60, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 60, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__hybrid_degradation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'simulation suffices for catastrophe readiness' conflates four structurally distinct claims, decomposed per the epsilon-invariance principle into four stories sharing the kernel catastrophe_proxy_sufficiency. The proxy-catastrophe reading is the regime's self-description (upstream: cited as justification for the hours-based architecture); the necessity reading is the veteran/skeptic pole; this hybrid-degradation reading occupies the mediating position (partial transfer, generational decay); the fidelity-threshold reading parameterizes the dispute technologically. Epsilon differs across members because each reading assesses the same standing arrangement by different lights: near-floor for proxy-sufficiency, highest for necessity, intermediate with deferred-harm structure for this reading. Each member links to the others here; sibling files reciprocate the edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
