% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__simulation_fidelity_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__simulation_fidelity_threshold, []).

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
 *   constraint_id: catastrophe_proxy_sufficiency__simulation_fidelity_threshold
 *   human_readable: Simulation Fidelity Threshold Doctrine for Catastrophe-Proxy Sufficiency
 *   domain: safety engineering/organizational learning/high-reliability organizations
 *
 * SUMMARY:
 *   In high-reliability industries — aviation, nuclear power, surgery,
 *   increasingly autonomous systems — the operative doctrine is that
 *   simulator-based training retains rare-event competence provided
 *   simulation fidelity crosses a threshold at which experienced stress and
 *   uncertainty match real catastrophe. Sufficiency is therefore a moving
 *   technological target, not a categorical property of simulation: better
 *   machines move the threshold within reach; worse ones leave crews
 *   procedurally drilled but stress-naive. This file instantiates ONE reading
 *   of the contested kernel catastrophe_proxy_sufficiency (the
 *   technology-indexed conditional reading) as a clean, epsilon-invariant
 *   constraint; the three sibling readings — categorical sufficiency,
 *   categorical necessity, hybrid degradation — are separate constraints with
 *   their own epsilon, beneficiary structures, and classifications, linked
 *   through network.affects_constraints. Claim and metrics are authored
 *   independently: claimed_type rope reflects the structure I believe true
 *   (pooled-investment coordination solving a genuine collective-action
 *   problem, net beneficiaries, minimal coercion); the metrics describe
 *   actual operation, including a growing vendor-rent layer and a rising
 *   compliance-theater share. Any divergence between claim and computed type
 *   is the measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - simulation_technology_vendors: Primary beneficiary (institutional/arbitrage) — collects the training-capital flows; the threshold framing sustains demand because sufficiency must be purchased and re-purchased as standards tighten
 *   - high_reliability_operators: Net beneficiary and payer (powerful/constrained) — buy competence insurance against rare-event losses; cannot exit their safety obligations
 *   - safety_regulators: Agenda setter (institutional/constrained) — define and certify what counts as qualifying fidelity; the threshold doctrine gives their hour-counts and device levels their force
 *   - line_operators: Beneficiary-practitioners (organized/constrained) — gain safe exposure to lethal failure modes; carry the residual gap if the threshold is not actually crossed
 *   - real_event_training_advocates: Excluded voice (moderate/trapped) — hold the categorical-insufficiency position with no seat in device-qualification or curriculum bodies
 *   - training_effectiveness_researchers: Analytical observer (moderate/analytical) — produce the transfer evidence that would locate the threshold; funding partly shaped by vendors and operators
 *   - aviation_insurers: Secondary beneficiary (institutional/arbitrage) — loss rates fall under simulation-backed competence; they reinforce the arrangement through premium structures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.28).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.14).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, extractiveness, 0.28).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 0.14).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, resistance, 0.32).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, "Simulation Fidelity Threshold Doctrine for Catastrophe-Proxy Sufficiency").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, "safety engineering/organizational learning/high-reliability organizations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, '60065024-ddf4-4773-8ef4-dc3fd77ff515').
narrative_ontology:cs_kernel_codification('60065024-ddf4-4773-8ef4-dc3fd77ff515', formalized).
narrative_ontology:cs_authority_grounding('60065024-ddf4-4773-8ef4-dc3fd77ff515', expertise).
narrative_ontology:cs_interpretation_layer_present('60065024-ddf4-4773-8ef4-dc3fd77ff515').
narrative_ontology:cs_reading_relation('60065024-ddf4-4773-8ef4-dc3fd77ff515', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, forecloses).
narrative_ontology:cs_reading_relation('60065024-ddf4-4773-8ef4-dc3fd77ff515', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('60065024-ddf4-4773-8ef4-dc3fd77ff515', catastrophe_proxy_sufficiency__hybrid_degradation_reading, coexists_with).
narrative_ontology:cs_axiom('60065024-ddf4-4773-8ef4-dc3fd77ff515', foundational, stress_fidelity_crossable_by_technology).
narrative_ontology:cs_axiom_status(stress_fidelity_crossable_by_technology, holdable).
narrative_ontology:cs_axiom_grounding('60065024-ddf4-4773-8ef4-dc3fd77ff515', stress_fidelity_crossable_by_technology, empirically_contingent).
narrative_ontology:cs_axiom('60065024-ddf4-4773-8ef4-dc3fd77ff515', secondary, sufficiency_tracks_state_of_the_art).
narrative_ontology:cs_axiom_status(sufficiency_tracks_state_of_the_art, holdable).
narrative_ontology:cs_axiom_grounding('60065024-ddf4-4773-8ef4-dc3fd77ff515', sufficiency_tracks_state_of_the_art, instrumental).
narrative_ontology:cs_reference_frame('60065024-ddf4-4773-8ef4-dc3fd77ff515', technology_indexed_sufficiency).
narrative_ontology:cs_drift_state('60065024-ddf4-4773-8ef4-dc3fd77ff515', contemporary_startle_research_era, gap(axiom_overriding, minor, true)).
narrative_ontology:cs_created_at('60065024-ddf4-4773-8ef4-dc3fd77ff515', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, high_reliability_operators).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, line_operators).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, aviation_insurers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, high_reliability_operators).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, transfer_of_training_validity).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, fidelity_certification_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and sell full-fidelity simulators, scenario libraries, and instructor curricula to regulated industries. Revenue scales with the strictness of fidelity standards and with the doctrine that sufficiency requires crossing them, so each tightening of qualification levels expands the addressable market. Product lines port across aviation, energy, medical, and defense customers, giving them easy exit from any single regulatory regime.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors, beneficiary,
    institutional, generational, arbitrage, global).

% Airlines, nuclear utilities, and hospital systems purchase simulation capacity as insurance against rare-event incompetence. They bear the capital and operating costs of simulator fleets and recurrent training hours, and receive avoided catastrophe losses, certification continuity, and insurability in return. They cannot exit their safety obligations; the largest operators partially internalize the function through owned training academies, which shifts them from purchasers to co-administrors of the standard.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, high_reliability_operators, beneficiary,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, high_reliability_operators, payer).

% Define simulator qualification levels, certify individual devices against those levels, and mandate recurrent hours credited toward competence. The threshold doctrine is what gives their hour-counts and device tiers normative force: without a sufficiency standard, simulator time could not stand in for exposure. They rely on vendor-submitted engineering evidence for device qualification and face political cost from both under-regulation after an accident and over-regulation before one.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, safety_regulators, agenda_setter,
    institutional, generational, constrained, continental).

% Pilots, reactor operators, and surgical teams train inside simulators, gaining repeatable exposure to failure modes that would be lethal or catastrophic live. They cannot decline recurrent simulation without losing certification and employment, though they can change employers and aircraft types. Many report a residual difference between simulator stress and real-event startle, and union structures give them collective voice over training load even though their subjective stress reports are not an input to fidelity metrics.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, line_operators, beneficiary,
    organized, biographical, constrained, global).

% Upset-recovery specialists, veteran instructors, and former accident investigators who hold that no simulator reproduces mortal-stakes arousal and that categorical reliance on simulation breeds fragile competence. Their position has no seat in device-qualification committees or curriculum-standard bodies; they publish and speak at conferences, but the certification machinery they criticize does not require their assent and has no channel for their evidence.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, real_event_training_advocates, excluded,
    moderate, biographical, trapped, global).

% Human-factors and training-science academics who measure transfer of training and would, in principle, locate the fidelity threshold empirically. Their research agendas are partly funded by vendors and operators, which shapes which questions get asked; their findings feed standards committees but carry no enforcement weight of their own.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, training_effectiveness_researchers, observer,
    moderate, generational, analytical, global).

% Price premiums against fleet-level loss histories. Simulation-backed competence lowers claim frequency, so insurers reinforce the arrangement through premium structures and underwriting requirements favoring operators with accredited simulation programs. They can reallocate portfolios across industries and regions if loss dynamics shift.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, aviation_insurers, beneficiary,
    institutional, biographical, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of maintaining rare-event competence without generating the rare events: industry-wide investment is pooled into fidelity technology, and a standardized sufficiency threshold converts catastrophes into repeatable, safe practice stimuli so that no single organization must cause or await real disasters to keep its people competent.
% TRANSFER_FUNCTION: Moves training capital from operators and public regulators to simulation technology vendors and standards infrastructure; moves risk exposure from live operations into simulated environments; and moves certification authority to the fidelity-standard setters who define what counts as enough.
% ABSENT_VOICES: Real-event training advocates and practitioners reporting residual stress-response gaps sit outside device-qualification and curriculum committees. Front-line crews' subjective stress reports are not an input to fidelity metrics. Passengers, patients, and the public, who bear the residual risk if the threshold is not actually crossed, have no seat anywhere in the arrangement.
% DISAPPEARANCE_RATIONALE: If the threshold doctrine vanished overnight, certification regimes would lose their basis for crediting simulator time in lieu of real exposure: operators would face a choice between returning to live-risk training (ethically and financially untenable) and suspending competence assurance outright; the vendor industry's demand structure would evaporate; insurers would reprice; and every recurrent-training calendar in aviation, nuclear, and surgery would be void pending replacement.
% FOUNDING_PROBLEM: Competence for rare catastrophic events was historically purchased with the events themselves — hull losses, meltdowns, patient deaths — because no other practice stimulus existed at realistic stress intensity. The founding problem was how to build and retain rare-event competence without waiting for, or causing, real catastrophes.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigation boards and international incident-review bodies attest from outside the vendor-operator benefiting set that pre-simulation eras showed systematically higher loss rates and that competence measurably decays across long no-event intervals; actuarial loss-rate series corroborate the same pattern independently of any party that collects from the arrangement.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__simulation_fidelity_threshold_tests).
:- end_tests(catastrophe_proxy_sufficiency__simulation_fidelity_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low-moderate (0.28) because the dominant flow is payment for a service whose value dwarfs its cost — a single avoided hull loss or meltdown exceeds decades of simulation spend — but a real rent layer exists: vendor margins scale with standards strictness, and the threshold's uncertain location sustains open-ended upgrade demand. Suppression is low (0.14) because no exit is blocked by this doctrine; the categorical alternative (training on real catastrophes) is closed by ethics and law, not by this arrangement, and mandated-hours enforcement lives in adjacent certification constraints rather than in the sufficiency claim itself. Theater ratio (0.24) reflects recurring-recurrency drift toward ritual completion while line-oriented scenario work remains functional. Accessibility collapse (0.50): once the threshold logic is accepted, 'wait for real events' collapses as a live alternative, but hybrid practices — post-incident debriefing, cross-domain analog training, surprise-scenario injection — persist as partial substitutes. Resistance (0.32) comes from practitioner skepticism about the startle gap, budget contests over simulator acquisition, and periodic simulator-complacency critiques. The measurement series run on one shared time grid (all tracked metrics authored at every examined point) showing mild extraction accumulation and theater growth over roughly four decades of simulator maturation.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the vendor seat the threshold is a demand engine attached to a genuine safety contribution — a rope that happens to pay them. From the operator seat it is an insurance premium with a compelling expected-value case — near-symmetric costs and benefits. From the line-operator seat it is simultaneously sanctuary (lethal failures made survivable to practice) and exposure (if the threshold is not truly crossed, the gap surfaces during a real event they cannot exit). From the excluded advocate's seat the entire arrangement is a category error — mortal-stakes arousal allegedly cannot be synthesized — and had they a seat, the computed classification would shift. The engine computes these divergences from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries map to low directionality: simulation_technology_vendors (beneficiary, arbitrage exit across regulated industries) sit nearest the beneficiary end; aviation_insurers likewise. high_reliability_operators are dual-positioned (beneficiary collecting avoided-loss value, payer bearing capital and operating cost) with constrained exit, placing them mid-range. line_operators are beneficiaries with constrained exit — low-to-mid d, with mild target coloring from the residual-gap risk they carry. safety_regulators administer the threshold rather than collecting from it; their stake is routed through vindicated_propositions (fidelity_certification_authority), not the beneficiary arrays, since a vindicated doctrine collects no rents. No victims are declared: no seat sits near the full-target end, so effective extraction stays low after directionality and scope scaling — consistent with the rope claim. Suppression is authored as a raw structural property and is deliberately NOT scaled; only extractiveness carries the directionality and scope modifiers in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — retaining rare-event competence without purchasing it with real catastrophes — remains live: rare events still occur, generational turnover continuously drains experiential capacity, and new high-consequence domains arrive with no baseline at all. Mandatrophy is not resolved and no sunset applies. The rope classification guards against both mislabelings: the visible vendor-rent layer must not flip the arrangement to snare (there is no victim class, participation is net-positive for every seated party, and no exit is suppressed), while the genuine coordination function must not launder the rising theater share and the threshold-ratchet incentive — both are tracked in the measurement series and held open in the vendor_standard_capture and threshold_identifiability omegas.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Which of the four readings of the catastrophe_proxy_sufficiency kernel is structurally correct: categorical sufficiency (any simulation works), categorical necessity (only real catastrophes work), technology-dependent threshold sufficiency (this reading), or procedural sufficiency with generational tacit decay?',
    'Longitudinal transfer-of-training studies across organizations with long no-event intervals, combined with natural experiments where high-fidelity simulation replaced live training decades ago.',
    'If the necessity reading is right, this arrangement is a costly futility and the constraint family restructures around an unavoidable natural limit; if the proxy reading is right, the threshold rent component vanishes and the rope purifies; if the hybrid reading is right, a latent victim class (future generations facing degraded tacit capacity) emerges and the arrangement drifts toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'This story is one reading of a four-way contested kernel; sibling readings instantiate different constraints with different epsilon and beneficiary structures.').

omega_variable(
    threshold_identifiability,
    'Is the fidelity threshold at which simulation stress/uncertainty matches real catastrophe empirically identifiable ex ante, or is it asymptotic and unverifiable?',
    'Dose-response curves relating graded fidelity metrics to measured transfer of stress-response and decision performance under incident reconstruction.',
    'A measurable threshold stabilizes the rope: investment stops at sufficiency. An unmeasurable threshold makes the binary condition unfalsifiable, sustaining open-ended vendor demand without verification — the extraction component grows and the arrangement drifts toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_identifiability, empirical, 'Whether the binary sufficiency condition is verifiable or permanently open.').

omega_variable(
    stress_transfer_validity,
    'Does above-threshold simulation transfer genuine stress-response capacity (startle management, cognitive tunneling resistance under mortal stakes), or only procedural competence?',
    'Instrumented comparison of physiological and decision markers between simulator performance and performance in subsequent real incidents, drawn from flight-data and incident-report corpora.',
    'Full transfer validates this reading''s foundational axiom; partial transfer means a structural residual gap is carried by line operators, supporting the hybrid sibling reading and raising the effective risk the arrangement externalizes onto the public.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stress_transfer_validity, empirical, 'Whether the threshold captures the stress/uncertainty dimension or only the procedural dimension of catastrophe competence.').

omega_variable(
    vendor_standard_capture,
    'Do simulation technology vendors shape fidelity-standard setting such that qualification thresholds ratchet upward beyond safety-justified levels?',
    'Disclosure analysis of vendor participation in device-qualification and curriculum-standard committees, plus cost-benefit audits of mandated simulator upgrades against measured safety outcomes.',
    'Demonstrated capture would convert part of the coordination overhead into directed rent, raising effective extraction on operator payers and pushing the computed classification toward tangled_rope despite the absence of a coerced victim class.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vendor_standard_capture, empirical, 'Whether the threshold''s upward drift tracks safety evidence or vendor commercial interest.').

omega_variable(
    novel_domain_calibration,
    'Can the fidelity threshold be calibrated for emerging high-consequence domains (autonomous vehicle fleets, orbital operations, AI-supervised infrastructure) that have no accumulated real-catastrophe baseline to calibrate against?',
    'Cross-domain validation of transfer metrics using analog domains with known baselines, before the new domain accumulates its own event history.',
    'If uncalibratable, the doctrine extends into new domains as unfalsifiable assurance, expanding vendor rents and creating populations whose residual risk is unknowable — a structural seed for the hybrid reading''s generational-decay victim class.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(novel_domain_calibration, conceptual, 'Whether the threshold framework generalizes to domains lacking a real-event calibration baseline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0, 42).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t6, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 6, 0.12).
narrative_ontology:measurement(cata_tr_t12, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 12, 0.15).
narrative_ontology:measurement(cata_tr_t18, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 18, 0.18).
narrative_ontology:measurement(cata_tr_t24, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 24, 0.2).
narrative_ontology:measurement(cata_tr_t30, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 30, 0.22).
narrative_ontology:measurement(cata_tr_t36, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 36, 0.23).
narrative_ontology:measurement(cata_tr_t42, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 42, 0.24).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cata_be_t6, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 6, 0.2).
narrative_ontology:measurement(cata_be_t12, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 12, 0.22).
narrative_ontology:measurement(cata_be_t18, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 18, 0.24).
narrative_ontology:measurement(cata_be_t24, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 24, 0.26).
narrative_ontology:measurement(cata_be_t30, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 30, 0.27).
narrative_ontology:measurement(cata_be_t36, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 36, 0.28).
narrative_ontology:measurement(cata_be_t42, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 42, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, resource_allocation).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, hybrid_degradation_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'simulation maintains catastrophe competence' conflates four structurally distinct claims with different epsilon values, failure modes, and beneficiary structures. Categorical sufficiency (proxy reading) implies negligible threshold rent; categorical necessity implies the entire arrangement is futile cost; hybrid degradation introduces a latent generational victim class; this file's conditional reading makes sufficiency an empirical variable and locates a bounded rent layer in the gap between achieved and certified fidelity. The proxy and necessity readings function as upstream optimism and upstream skepticism respectively; this reading mediates between them; the hybrid reading is downstream refinement absorbing evidence against all three. Every family member links the others through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
