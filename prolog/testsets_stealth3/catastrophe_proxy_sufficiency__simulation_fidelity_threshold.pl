% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__simulation_fidelity_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
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
 *   human_readable: Simulation Fidelity Threshold Standard for Catastrophe Competence Retention
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   Regulated high-hazard industries rehearse rare catastrophes in simulators
 *   because the real events are too infrequent and too costly to learn from.
 *   The arrangement under contest is the fidelity-governed
 *   simulation-training regime: the claim that competence retention hinges on
 *   simulated stress and uncertainty matching real-event levels, which makes
 *   sufficiency a moving technological target rather than a settled fact. The
 *   ε referent is that standing regime, assessed by this reading's own lights
 *   — this reading endorses the mechanism, so its ε is not zero by
 *   construction; the ratchet and certification-cliff effects are real
 *   features of the regime this reading itself describes. KEY AGENTS (by
 *   structural relationship): simulation_technology_vendors — primary
 *   beneficiary (organized/arbitrage), captures the upgrade cycle and
 *   co-drafts the criterion; regulatory_certification_bodies — agenda setter
 *   (institutional/constrained), administers the threshold;
 *   high_resource_operators — dual-positioned beneficiary-payer
 *   (institutional/constrained), funds and profits from the bar it clears;
 *   low_resource_operators — primary target (moderate/trapped), faces a
 *   sufficiency bar priced beyond reach; frontline_response_teams —
 *   protected-and-taxed seat (organized/constrained);
 *   insurers_and_underwriters and training_transfer_researchers — observers;
 *   service_populations — silent residual-risk bearer (powerless/mobile).
 *   Claim and metrics are authored independently: claimed_type reflects the
 *   judged structure (genuine coordination carrying an embedded rent stream
 *   under active enforcement), and the metrics describe observed operation.
 *   Per the kernel-reading rules, sibling readings of this kernel live in
 *   their own files; this story contains only this reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.42).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.42).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, extractiveness, 0.42).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, tangled_rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, "Simulation Fidelity Threshold Standard for Catastrophe Competence Retention").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency__simulation_fidelity_threshold).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'df5f9988-f9c3-4cba-96e4-17d50f5e39fa').
narrative_ontology:cs_kernel_codification('df5f9988-f9c3-4cba-96e4-17d50f5e39fa', formalized).
narrative_ontology:cs_authority_grounding('df5f9988-f9c3-4cba-96e4-17d50f5e39fa', expertise).
narrative_ontology:cs_interpretation_layer_present('df5f9988-f9c3-4cba-96e4-17d50f5e39fa').
narrative_ontology:cs_reading_relation('df5f9988-f9c3-4cba-96e4-17d50f5e39fa', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('df5f9988-f9c3-4cba-96e4-17d50f5e39fa', catastrophe_proxy_sufficiency__hybrid_degradation_reading, influences).
narrative_ontology:cs_reading_relation('df5f9988-f9c3-4cba-96e4-17d50f5e39fa', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, coexists_with).
narrative_ontology:cs_axiom('df5f9988-f9c3-4cba-96e4-17d50f5e39fa', foundational, sufficiency_tracks_fidelity_frontier).
narrative_ontology:cs_axiom_status(sufficiency_tracks_fidelity_frontier, holdable).
narrative_ontology:cs_axiom_grounding('df5f9988-f9c3-4cba-96e4-17d50f5e39fa', sufficiency_tracks_fidelity_frontier, empirically_contingent).
narrative_ontology:cs_axiom('df5f9988-f9c3-4cba-96e4-17d50f5e39fa', secondary, measured_stress_equivalence_certifies_readiness).
narrative_ontology:cs_axiom_status(measured_stress_equivalence_certifies_readiness, holdable).
narrative_ontology:cs_axiom_grounding('df5f9988-f9c3-4cba-96e4-17d50f5e39fa', measured_stress_equivalence_certifies_readiness, empirically_contingent).
narrative_ontology:cs_reference_frame('df5f9988-f9c3-4cba-96e4-17d50f5e39fa', technology_frontier_sufficiency).
narrative_ontology:cs_drift_state('df5f9988-f9c3-4cba-96e4-17d50f5e39fa', contemporary_procurement_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('df5f9988-f9c3-4cba-96e4-17d50f5e39fa', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, high_resource_operators).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, frontline_response_teams).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, service_populations).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, low_resource_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, high_resource_operators).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, frontline_response_teams).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, transfer_of_training_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and sell full-scope simulators, scenario libraries, motion and visual upgrade packages to regulated industries. Vendor representatives sit on the technical committees where training-adequacy criteria are drafted, and the fidelity benchmarks the criteria cite come largely from vendor publications. Each revision of what counts as adequate practice opens a new procurement cycle across every regulated operator simultaneously.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors, beneficiary,
    organized, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors, agenda_setter).

% Write and administer the rules deciding which training programs count as preparation for severe events: qualify simulators, mandate recurring scenario hours, audit compliance, and expand requirements after accidents. Statutory mandates and post-accident investigation recommendations drive their agenda; they revisit the adequacy criterion itself mainly after publicized failures, not on evidence schedules of their own choosing.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, regulatory_certification_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Major airlines, nuclear utilities, and academic medical centers that own threshold-grade simulator fleets and run in-house training academies. They fund the largest training budgets and recover much of the cost through fares, rates, and service pricing. Demonstrated top-end preparedness also functions as market positioning: recruiting, insurance pricing, and public confidence all track it, which raises the bar competitors must clear.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, high_resource_operators, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, high_resource_operators, payer).

% Regional carriers, rural hospitals, volunteer fire districts, and small municipal utilities for whom a threshold-grade program exceeds the entire annual training budget. They buy scarce shared simulator hours, run abbreviated scenarios, or file documented noncompliance and accept the liability and inspection consequences. Exiting the regulated market is not a realistic option, so the practical choice runs between debt and recorded shortfall.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, low_resource_operators, payer,
    moderate, biographical, trapped, regional).

% Pilots, reactor operators, critical-care nurses, and incident commanders who train in the simulators and staff the real events. Recurrent scenario hours are mandatory working time governed by union agreements; the scenarios build skills that cannot be acquired safely any other way. They gain capability and surrender schedule in the same sessions.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, frontline_response_teams, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, frontline_response_teams, payer).

% Price liability coverage partly on documented training compliance. Certification records give them a legible risk signal, and premium schedules reward operators meeting the current adequacy bar. They neither draft the criterion nor deliver any training; they read its outputs and price accordingly.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, insurers_and_underwriters, observer,
    institutional, biographical, mobile, global).

% Human-factors and adult-learning scientists studying whether simulator performance predicts field performance under stress. Standards committees cite their findings when criteria are revised; their null results tend to arrive years after the equipment they question has already been purchased and installed across the industry.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, training_transfer_researchers, observer,
    moderate, generational, analytical, global).

% Passengers, patients, and residents near industrial facilities — the people whose outcomes depend on crews performing correctly during rare severe events. They hold no seat in any standards process; their interests reach the table indirectly through regulators, litigation, and insurance pricing. When preparation falls short of the event, they absorb the difference.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, service_populations, beneficiary,
    powerless, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, auditable criterion for when rehearsal counts as preparation for events too rare and dangerous to learn from directly: regulators certify against it, insurers price against it, operators plan procurement against it, so the field is not forced to discover readiness gaps through actual catastrophes.
% TRANSFER_FUNCTION: Moves procurement capital from operating-organization budgets to simulator vendors and training centers; moves recurring crew time into simulated scenarios; moves certification authority and audit activity to the bodies administering the threshold; moves premium relief to compliant operators. Operating budgets and crew hours are the sources; vendor revenue, training infrastructure, and certification apparatus are the destinations.
% ABSENT_VOICES: Communities served by under-resourced responders — rural patients, regional-airline passengers — hold no seat anywhere in the standards process yet inherit whatever the criterion misses. Practitioners who argue that only real events teach real stress participate solely as research dissent. Small operators attend public consultations but cannot sustain representation on the technical committees where threshold values are actually drafted.
% DISAPPEARANCE_RATIONALE: Certification regimes would lose their adequacy instrument and revert to raw hours-counting or ad-hoc post-incident reform; the upgrade cycle that structures vendor demand would collapse; operators would face unpriceable uncertainty about preparedness claims; insurers would reprice or withdraw from segments lacking a legible training signal; multiple standing training mandates cite the criterion explicitly and would lapse into incoherence.
% FOUNDING_PROBLEM: Severe accidents are too infrequent and too costly to train on directly, yet competence decays without rehearsal. The founding problem was finding a rehearsal method whose stress and uncertainty match the real event closely enough to preserve skill, together with a defensible way to certify that the rehearsal worked.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigation boards outside the training industry — NTSB and CSB findings on surprise, startle response, and degraded-mode handling — repeatedly attribute outcomes to gaps between trained conditions and encountered ones. The transfer-of-training literature documents persistently open questions about fidelity-to-field generalization. Insurance loss data continues to register severe events across fully compliant operators. No party outside the vendor-regulatory complex attests that the founding problem is solved.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.42, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.42: the regime delivers a real collective good — an auditable readiness standard that replaces catastrophe-priced learning — while embedding two bounded rent streams: a technology-indexed upgrade cycle (each fidelity generation redefines sufficiency, reopening procurement everywhere at once) and a certification cliff that prices small operators into shared-hour scarcity. Suppression 0.42: compliance regimes, licensure dependencies, and insurance prerequisites raise the cost of alternative pathways substantially, but rival approaches are crowded out rather than forcibly excluded, and the competing readings of this kernel remain publishable research positions. Theater 0.28: recurring mandated scenarios drift toward scripted repetition — crews memorize the scenario arc — while high-fidelity programs still do substantive work, keeping performative share below half. Accessibility_collapse 0.45: once the threshold frame is adopted, the open question 'is our training adequate?' collapses into the procurable question 'have we bought the current bar?', but the alternative framings stay intellectually reachable, so collapse is partial. Resistance 0.35: budget pushback from small operators, practitioner drill-fatigue complaints, and periodic null-result literature meet the criterion without displacing it. Suppression is authored as a raw structural property and enters the engine's computation unscaled; only extractiveness is scaled by directionality and spatial scope. The measurement series run on one shared seven-point grid so every tracked metric carries an authored value at every examined time point. The trajectories are step-ratchets rather than smooth curves: post-accident investigation cycles drive discrete mandate expansions (visible in the suppression_requirement steps at t=8 and t=16), and each expansion lifts the procurement baseline that the extraction series tracks.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently, and the divergence is structural, not attitudinal. From the vendor seat the arrangement is a demand engine it helped draft, experienced as ordinary business. From the low-resource operator seat the same criterion arrives as an unfunded mandate enforced by licensure and liability, with exit closed — the highest-directionality seat in the story. From the regulator seat it is an administrable proxy for an unobservable quantity (readiness) — governance made possible. From the researcher seat it is a large undervalidated purchase whose evidence trail lags deployment. From the frontline seat it is mandatory time that genuinely builds skill. The engine derives these divergent per-seat classifications from the declared roles, power, and exit options; this commentary only explains why they must diverge.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive derivation: low_resource_operators, declared victim and structurally trapped (exit blocked by licensure), sit nearest the full-target end, so effective extraction amplifies for them; simulation_technology_vendors, declared beneficiary with arbitrage-grade exit, sit nearest the beneficiary end, so extraction dampens toward subsidy. high_resource_operators derive a low d from their beneficiary declaration, tempered by their secondary payer position and pass-through economics. frontline_response_teams sit near symmetric — skill gained and schedule surrendered in the same hour. service_populations sit near the beneficiary end: protected without paying the transfer directly. regulatory_certification_bodies, insurers_and_underwriters, and training_transfer_researchers carry no beneficiary or victim declaration; they take canonical fallback directionality, which is correct, since they neither collect the transfer nor bear it. No directionality_overrides are authored: the declarations plus exit options already differentiate the institutional seats (agenda-setter regulator versus beneficiary operator versus observer insurer carry different roles, which the derivation reads directly), and an override keyed to the institutional power atom would distort all three at once.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is corroborated as live by sources outside the benefiting parties (investigation-board findings, transfer literature, loss data), so this is not a mandate outliving its function and no mandatrophy_resolved flag is declared; no sunset clause is appropriate because the problem has no natural terminus. The tangled_rope classification prevents two opposite mislabels. Reading the arrangement as pure rope would bless the technology-indexed ratchet and the certification cliff as mere coordination overhead, erasing the vendor-side capture and the equity cost borne by low-resource operators. Reading it as a snare would discard a genuine collective-action achievement — rehearsal without catastrophe-priced discovery — that no serious participant wants abolished. Piton is excluded by the low theater ratio and the live function; scaffold is excluded by the absence of any sunset logic; mountain is excluded by constructed origin and active enforcement. The residual risk this classification flags is directional: if transfer validity above the threshold fails empirically, the coordination component shrinks and the same structure re-reads snare-ward, which is exactly what the temporal series and the transfer_validity_above_threshold omega are positioned to detect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This story instantiates only the simulation_fidelity_threshold reading of kernel catastrophe_proxy_sufficiency — how would the sibling readings (catastrophe_necessity_reading, hybrid_degradation_reading, simulation_as_proxy_catastrophe_reading) restructure it if instantiated?',
    'Comparative classification of the sibling stories once generated: necessity reading relocates beneficiaries toward incident-experienced practitioners and makes ε sensitive to event scarcity; hybrid reading splits the family into procedural versus tacit sub-constraints with different persistence laws; categorical-proxy reading removes the upgrade ratchet by declaring sufficiency already achieved.',
    'Each sibling changes the beneficiary set, the victim set, and the ε referent; this story''s classification is valid only within its reading, and cross-reading comparisons must route through the linked family, not through re-parameterizing this file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer-frame omega recording that this constraint is one reading of a contested kernel and locating the disagreement at the purchasability of stress/uncertainty parity.').

omega_variable(
    transfer_validity_above_threshold,
    'Does crossing measured fidelity thresholds actually predict field performance under real-event stress, or does the correlation break somewhere above the currently purchased bar?',
    'Longitudinal linkage of simulator assessment scores to field event outcomes across operators at different fidelity tiers, controlling for selection effects in who owns high-fidelity equipment.',
    'Broken or plateauing transfer converts the threshold logic from coordination into demand manufacture, shrinking the genuine-coordination component and pushing the structure snare-ward; intact transfer secures the rope core and stabilizes the tangled_rope verdict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_validity_above_threshold, empirical, 'Whether the threshold criterion tracks a real competence-transfer boundary.').

omega_variable(
    threshold_authorship_provenance,
    'Who authors the threshold values — independent engineering evidence trails, or vendor benchmarks flowing through committee seats into regulatory criteria?',
    'Audit of standards-committee records: citation graphs from vendor whitepapers into adopted criteria, voting records on threshold revisions, and disclosure of financial interests among technical-committee members.',
    'Demonstrated vendor authorship raises effective extraction and strengthens capture readings; demonstrably independent authorship supports treating the criterion as neutral coordination infrastructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_authorship_provenance, empirical, 'Provenance of the threshold numbers that calibrate the entire arrangement.').

omega_variable(
    sufficiency_ratchet_vs_science_pace,
    'Is ''sufficiency is technology-dependent'' a legitimate epistemic stance (the bar moves because fidelity science genuinely advances) or a manufactured-obsolescence cover (the bar is paced to product cycles regardless of demonstrated competence return)?',
    'Compare demonstrated competence-return per fidelity generation — controlled transfer studies at each tier — against the cadence and pricing of upgrade sales cycles; divergence between the two paces indicts the ratchet.',
    'Science-paced movement keeps the extraction bounded and coordination-dominant; product-paced movement identifies the moving target itself as the extraction mechanism, raising the snare component of the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_ratchet_vs_science_pace, conceptual, 'Whether the technology-dependence clause encodes advancing knowledge or scheduled obsolescence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catprox_simfid_tr_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 0, 0.16).
narrative_ontology:measurement(catprox_simfid_tr_t4, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 4, 0.19).
narrative_ontology:measurement(catprox_simfid_tr_t8, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 8, 0.21).
narrative_ontology:measurement(catprox_simfid_tr_t12, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 12, 0.23).
narrative_ontology:measurement(catprox_simfid_tr_t16, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 16, 0.25).
narrative_ontology:measurement(catprox_simfid_tr_t20, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 20, 0.27).
narrative_ontology:measurement(catprox_simfid_tr_t24, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(catprox_simfid_be_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(catprox_simfid_be_t4, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 4, 0.32).
narrative_ontology:measurement(catprox_simfid_be_t8, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(catprox_simfid_be_t12, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(catprox_simfid_be_t16, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(catprox_simfid_be_t20, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(catprox_simfid_be_t24, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(catprox_simfid_su_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(catprox_simfid_su_t4, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 4, 0.25).
narrative_ontology:measurement(catprox_simfid_su_t8, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 8, 0.29).
narrative_ontology:measurement(catprox_simfid_su_t12, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 12, 0.32).
narrative_ontology:measurement(catprox_simfid_su_t16, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(catprox_simfid_su_t20, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 20, 0.39).
narrative_ontology:measurement(catprox_simfid_su_t24, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, information_standard).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_proxy_sufficiency__catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_proxy_sufficiency__hybrid_degradation_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'does simulation suffice for catastrophe competence?' per the ε-invariance principle: the label conflates four structurally distinct claims, each with its own ε referent, beneficiary structure, and classification. This file instantiates the threshold (conditional, technology-indexed) reading. The categorical-proxy reading is upstream optimism feeding procurement defaults; the necessity reading is upstream skepticism feeding mandate floors and real-event rotation arguments; this reading mediates between them and supplies the evidential environment downstream for the hybrid-degradation reading. All family members are mutually linked via affects_constraints; no story averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
