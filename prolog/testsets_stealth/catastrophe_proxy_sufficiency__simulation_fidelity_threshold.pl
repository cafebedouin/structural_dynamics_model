% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__simulation_fidelity_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   human_readable: Simulation Fidelity Threshold Doctrine for Catastrophe Competence Retention
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This story instantiates the simulation_fidelity_threshold reading of the
 *   catastrophe_proxy_sufficiency kernel (see commentary.kernel_context and
 *   the committer omegas). The constraint described: the doctrine, embedded
 *   in certification regimes and procurement practice across high-reliability
 *   industries, that competence for rare catastrophes can be retained through
 *   simulation provided simulation technology crosses a fidelity threshold at
 *   which induced stress and uncertainty match catastrophe conditions —
 *   making sufficiency a matter of technological attainment rather than
 *   category. The standing arrangement the epsilon value describes is
 *   threshold-governed simulation procurement and mandated recurrent training
 *   as it operates today. Family decomposition note: the four readings share
 *   this referent but author reading-indexed epsilon over it — this reading
 *   authors moderate epsilon (~0.36: functional coordination carrying bounded
 *   vendor margin and unverifiable-claim risk); the catastrophe_necessity
 *   reading would author high epsilon over the same arrangement (futile
 *   expenditure); the simulation_as_proxy reading would author near-zero
 *   epsilon (sufficiency already achieved); the hybrid_degradation reading
 *   would author moderate-high epsilon with a decay term. The claim/metric
 *   gap is deliberate: claimed_type is rope (this reading's structural
 *   verdict) while the metrics describe the arrangement's actual mixed
 *   operation — the engine measures the divergence; the claim is not
 *   reconciled to the metrics.
 *
 * KEY AGENTS:
 *   - - simulation_technology_vendors: Primary beneficiary (organized/arbitrage) — sells the threshold-crossing; captures the training-capital flow
 *   - - high_reliability_operators: Net beneficiary and payer (powerful/constrained) — buys assurance of retained competence for events its crews will rarely meet
 *   - - hro_certification_regulators: Agenda setter (institutional/constrained) — mandates how much simulator time counts and at what fidelity class
 *   - - international_standards_bodies: Agenda setter (institutional/constrained) — defines the fidelity metrics the threshold is expressed in
 *   - - frontline_trainees: Beneficiary and payer (moderate/constrained) — acquires the competence; bears the engineered acute stress
 *   - - insurance_pools: Beneficiary (institutional/mobile) — prices catastrophe tail risk against declared rehearsal
 *   - - academic_transfer_researchers: Analytical observer (institutional/analytical) — tests whether fidelity actually transfers to real performance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.36).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.28).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, extractiveness, 0.36).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, "Simulation Fidelity Threshold Doctrine for Catastrophe Competence Retention").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency__simulation_fidelity_threshold).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'edda0d52-18bb-4d90-8896-17ef2ba421be').
narrative_ontology:cs_kernel_codification('edda0d52-18bb-4d90-8896-17ef2ba421be', formalized).
narrative_ontology:cs_authority_grounding('edda0d52-18bb-4d90-8896-17ef2ba421be', expertise).
narrative_ontology:cs_interpretation_layer_present('edda0d52-18bb-4d90-8896-17ef2ba421be').
narrative_ontology:cs_reading_relation('edda0d52-18bb-4d90-8896-17ef2ba421be', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('edda0d52-18bb-4d90-8896-17ef2ba421be', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, forecloses).
narrative_ontology:cs_reading_relation('edda0d52-18bb-4d90-8896-17ef2ba421be', catastrophe_proxy_sufficiency__hybrid_degradation_reading, coexists_with).
narrative_ontology:cs_axiom('edda0d52-18bb-4d90-8896-17ef2ba421be', foundational, stress_uncertainty_equivalence_attainable).
narrative_ontology:cs_axiom_status(stress_uncertainty_equivalence_attainable, holdable).
narrative_ontology:cs_axiom_grounding('edda0d52-18bb-4d90-8896-17ef2ba421be', stress_uncertainty_equivalence_attainable, empirically_contingent).
narrative_ontology:cs_axiom('edda0d52-18bb-4d90-8896-17ef2ba421be', foundational, sufficiency_is_graded_not_categorical).
narrative_ontology:cs_axiom_status(sufficiency_is_graded_not_categorical, holdable).
narrative_ontology:cs_axiom_grounding('edda0d52-18bb-4d90-8896-17ef2ba421be', sufficiency_is_graded_not_categorical, empirically_contingent).
narrative_ontology:cs_reference_frame('edda0d52-18bb-4d90-8896-17ef2ba421be', progressive_fidelity_sufficiency).
narrative_ontology:cs_drift_state('edda0d52-18bb-4d90-8896-17ef2ba421be', contemporary_psychophysiology_era, gap(axiom_overriding, minor, true)).
narrative_ontology:cs_created_at('edda0d52-18bb-4d90-8896-17ef2ba421be', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, high_reliability_operators).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, frontline_trainees).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, insurance_pools).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, high_reliability_operators).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, frontline_trainees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and sells full-flight simulators, control-room replicas, and immersive disaster trainers; markets products by proximity to catastrophe-grade stress realism; revenue depends on operators continuing to buy fidelity upgrades toward the declared threshold; can redirect engineering capacity into defense training or entertainment markets if safety-simulation demand shifts.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Airlines, nuclear plant operators, offshore and petrochemical majors, metropolitan fire and emergency services: buy simulator capacity and send crews through recurrent high-fidelity scenarios because regulators credit simulator time toward certification and insurers ask for evidence of rehearsal; cannot exit their safety obligations and have few substitutes for declared competence; bear the capital and operating cost of the fidelity ladder.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, high_reliability_operators, beneficiary,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, high_reliability_operators, payer).

% Set how much simulator time counts toward licenses and what fidelity class qualifies; audit training centers; after major accidents raise required fidelity classes and hour minimums; bound by statute and treaty obligations, they cannot simply stop certifying, and their credibility rides on the arrangements they endorse.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, hro_certification_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Maintain the technical definitions — fidelity levels, motion cueing criteria, visual system specifications, scenario-stress rubrics — that turn realism into a purchasable specification; committees staffed by vendors, operators, and regulators; revision cycles move slowly by design, which stabilizes the metric the threshold is expressed in.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, international_standards_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Pilots, reactor operators, incident commanders: spend recurring blocks of career time inside scenarios engineered to produce genuine stress and uncertainty; acquire the competence the system exists to preserve; absorb the acute psychological load of engineered catastrophe rehearsal; can change employers but cannot escape recurrent training while practicing.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, frontline_trainees, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, frontline_trainees, payer).

% Pool catastrophe liabilities for the industries involved; require evidence of rehearsal before writing or renewing cover; benefit when declared competence holds and reprice sharply when it fails; can withdraw from lines or jurisdictions that look uninsurable, shifting residual risk back to operators and the public.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, insurance_pools, beneficiary,
    institutional, generational, mobile, continental).

% Human-factors and transfer-of-training scientists measure whether simulator performance predicts real-world performance under stress; publish dose-response curves between fidelity and retention; depend on site access that operators and vendors control; hold no enforcement power over procurement or certification.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, academic_transfer_researchers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools sector-wide investment in shared high-fidelity simulation infrastructure and standardized fidelity metrics so that competence for catastrophes too rare for experiential learning can be built and maintained collectively; the threshold converts open-ended preparedness anxiety into a bounded engineering target that procurement, certification, and research can all aim at.
% TRANSFER_FUNCTION: Moves training capital from operators, public agencies, and their budgets to simulation technology vendors and training-center infrastructure; moves acute stress exposure onto trainees; returns assurance artifacts — certification credit, audit evidence, insurability — to operators, regulators, and insurers.
% ABSENT_VOICES: Fenceline communities bearing residual catastrophe risk around hazardous facilities have no seat in fidelity-standard setting; trainee mental-health representatives object to escalating stress dosing from outside the standards committees; small operators priced out of top-tier fidelity would contest a regime that makes safety sufficiency a function of procurement depth.
% DISAPPEARANCE_RATIONALE: If the threshold doctrine vanished overnight, fidelity standards would lose their anchoring claim; procurement would fragment between fatalism (nothing prepares you) and indiscriminate purchasing (buy anything labeled realistic); vendor roadmaps, regulator certification credit, and insurer evidence requirements would all reorganize around whatever doctrine replaced the threshold — the arrangement, not merely its label, is load-bearing across four industries.
% FOUNDING_PROBLEM: Mid-century disaster investigations in aviation, nuclear power, and petrochemical operations showed crews failing from stress-degraded performance rather than missing knowledge — and real catastrophes are too rare for any crew to learn from experientially. The field needed a way to rehearse the unrehearsable: a substitute stressor that trains the response without waiting for the event.
% FOUNDING_PROBLEM_CORROBORATION: Accident-investigation bodies (NTSB, IAEA incident reviews, chemical safety boards) and crew unions corroborate the founding problem — stress-degraded performance in rare events — from outside the benefiting parties. The threshold-sufficiency claim specifically is weaker corroborated: validation studies are substantially vendor-commissioned, independent replication of fidelity-to-transfer dose-response curves remains thin, and no fully independent body attests the threshold's location; that asymmetry is itself signal and feeds the threshold_verifiability omega.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.36, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.36: under this reading the bulk of the training-capital flow purchases a real capability (stress-matched rehearsal), leaving vendor margin, marketing-driven over-specification, and crowding-out of cheaper formats as the extractive residue; the series shows that residue growing as the market matured (0.22 to 0.36 across the interval). Suppression 0.28: participation is compelled indirectly — regulators credit simulator time toward certification and insurers demand rehearsal evidence, so opting out carries license and premium costs — but no alternative format is banned, and live drills, on-the-job exposure, and academic study persist. Theater_ratio 0.30: recurrent sessions and showcase training centers carry a performative compliance layer (hours logged, demonstrations staged for audits) atop a functioning training core. Accessibility_collapse 0.42: accepting the threshold doctrine demotes alternatives to inadequacy without eliminating them — friction, not closure, which is the rope-typical middle band. Resistance 0.30: budget holders resist procurement scale, practitioner cohorts periodically contest simulation's realism, and small operators contest a sufficiency regime priced by procurement depth. All three tracked series share one six-point grid (t=0..50) so no metric row is sampled against a substituted end-state scalar. suppression_requirement is authored deliberately rather than left static: the interval tracks a real enforcement build-up — successive post-accident mandates raised required fidelity classes and simulator-hour credit — so the enforcement trajectory is part of this story, not merely its endpoint.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical metrics. From the vendor seat the arrangement is a market it legitimately serves: directionality sits near the beneficiary pole and the constraint presents as opportunity. From the operator seat it is purchased assurance — a net-beneficiary position paying coordination costs, mid-low on the target-beneficiary axis. From the trainee seat the same arrangement contains an uncompensated component (engineered acute stress borne in body and career time) that pushes its directionality upward relative to the seat paying the bills. From the regulator and standards-body seats it is an administrable standard with no collection function attached. From the researcher seat the salient feature is that the threshold's location is asserted far more often than it is measured. The engine derives these divergences from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: simulation_technology_vendors collect the monetary flow directly and sit near the full-beneficiary pole; high_reliability_operators and frontline_trainees appear as beneficiaries but both pay — operators in capital, trainees in stress load and career time — placing them mid-low rather than at the pole; insurance_pools benefit through reduced tail losses and sit low. No victims are declared because, within this reading, no group bears uncompensated extraction: costs enter as coordination overhead priced into a capability the payers judge worth buying. The nearest thing to a victim-position is the trainee stress component, carried here as a payer-side cost and watched through the generational_decay omega rather than declared as victimhood. Regulators and standards bodies administer without collecting; researchers observe. Receipt is not benefit: vendors receive the flow, but this reading judges the purchases net-beneficial to the payers — which is exactly the rope structure the claim asserts and the moderate metrics qualify. Scope amplification is modest: certification regimes operate nationally while vendor markets and operator fleets are global, so effective extraction scales mildly with the global procurement surface.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — rehearsing the unrehearsable — is live: rare catastrophes continue and no substitute stressor has displaced the question, so no mandatrophy is declared. The rope classification does preventive work in both directions. Against mountain-mislabeling: the threshold doctrine is constructed and enforced (emerges_naturally stays false; the enforcement machinery visibly built up over the interval), so treating it as a law of learning would immunize vendor sufficiency claims from scrutiny. Against snare-mislabeling: the coordination function is genuine, participants are net beneficiaries, and alternatives persist, so reading the arrangement as pure vendor predation would discard the real capability it purchases. The drift risk this story flags is inertia-shaped rather than predation-shaped: if the threshold recedes or proves unverifiable (see receding_threshold_dynamics and threshold_verifiability), procurement could persist as ritual while the function quietly fails — the theater_ratio series and the founding_problem_status x disappearance_verdict mismatch consumer are the tripwires for that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position_fidelity_threshold,
    'This story instantiates the simulation_fidelity_threshold reading of the catastrophe_proxy_sufficiency kernel: is the threshold premise — that stress/uncertainty equivalence with real catastrophe is technologically attainable, making sufficiency graded rather than categorical — the correct resolution, or does a sibling reading (catastrophe_necessity_reading, simulation_as_proxy_catastrophe_reading, hybrid_degradation_reading) describe the structure?',
    'Longitudinal transfer evidence: correlate fidelity-class metrics with real-incident crew performance across operators and adjudicate against sibling predictions — the necessity reading predicts no fidelity level ever suffices, the proxy reading predicts flat sufficiency across current practice, the hybrid reading predicts generational decay despite fidelity.',
    'If a sibling prevails, this constraint recomputes: the necessity reading collapses the threshold into permanent insufficiency (the arrangement becomes expenditure without coordination payoff and epsilon rises sharply); the proxy reading dissolves the threshold (the arrangement becomes ordinary procurement with near-zero epsilon); the hybrid reading caps sufficiency duration and adds a decay term.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position_fidelity_threshold, conceptual, 'Committer structure: this file is one reading of the catastrophe_proxy_sufficiency kernel; sibling readings would change the beneficiary structure, the epsilon, and the type.').

omega_variable(
    threshold_verifiability,
    'Can threshold-crossing be verified ex ante, given that no organization can ethically or practically run the real catastrophe to test whether its simulation has become sufficient?',
    'Adversarial validation regimes plus natural experiments: accumulate dose-response transfer data from incidents occurring at operators whose simulation had been declared threshold-crossing, and commission independent (non-vendor) replication of fidelity-to-retention curves.',
    'If unverifiable, vendor sufficiency claims become unfalsifiable marketing; effective extraction rises toward the tangled-rope boundary as payment decouples from demonstrated function, and the rope claim degrades.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_verifiability, empirical, 'Whether the binary sufficiency condition the threshold creates is checkable by anyone other than the seller.').

omega_variable(
    receding_threshold_dynamics,
    'Is the fidelity threshold a fixed, crossable target, or does each fidelity gain expose new stress-realism gaps — an uncanny valley that recedes as technology approaches, so sufficiency is perpetually one procurement cycle away?',
    'Historical analysis of declared-sufficient systems: do upgrade cycles terminate when sufficiency is declared, or continue indefinitely at constant or increasing spend?',
    'A receding threshold converts the arrangement into a perpetual-purchase treadmill: extraction accumulates over time, the temporal series would show sustained base_extractiveness growth past the point of declared sufficiency, and the accumulation hypothesis would fire for investigation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(receding_threshold_dynamics, empirical, 'Whether the threshold is a destination or a horizon.').

omega_variable(
    generational_tacit_decay_boundary,
    'Does fidelity-threshold simulation halt generational degradation of tacit knowledge and stress-response capacity, as this reading assumes, or does degradation proceed on timescales simulation cannot touch — the hybrid_degradation sibling''s claim?',
    'Multi-decade cohort studies comparing real-incident performance of generations trained exclusively above-threshold against generations that also held real-event exposure.',
    'If decay proceeds regardless of fidelity, threshold sufficiency is time-limited: the arrangement would need periodic real-event recalibration, raising the suppression required to mandate exposure, or accepting bounded competence — either outcome raises effective extraction per unit of retained capability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generational_tacit_decay_boundary, empirical, 'Timescale boundary between what simulation fidelity can preserve and what only real events maintain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cps_fid_thresh_tr_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cps_fid_thresh_tr_t10, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 10, 0.16).
narrative_ontology:measurement(cps_fid_thresh_tr_t20, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 20, 0.2).
narrative_ontology:measurement(cps_fid_thresh_tr_t30, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 30, 0.24).
narrative_ontology:measurement(cps_fid_thresh_tr_t40, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 40, 0.27).
narrative_ontology:measurement(cps_fid_thresh_tr_t50, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(cps_fid_thresh_be_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cps_fid_thresh_be_t10, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(cps_fid_thresh_be_t20, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 20, 0.29).
narrative_ontology:measurement(cps_fid_thresh_be_t30, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 30, 0.32).
narrative_ontology:measurement(cps_fid_thresh_be_t40, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 40, 0.34).
narrative_ontology:measurement(cps_fid_thresh_be_t50, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 50, 0.36).

% Suppression requirement over time
narrative_ontology:measurement(cps_fid_thresh_su_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 0, 0.14).
narrative_ontology:measurement(cps_fid_thresh_su_t10, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 10, 0.17).
narrative_ontology:measurement(cps_fid_thresh_su_t20, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 20, 0.2).
narrative_ontology:measurement(cps_fid_thresh_su_t30, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 30, 0.23).
narrative_ontology:measurement(cps_fid_thresh_su_t40, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 40, 0.26).
narrative_ontology:measurement(cps_fid_thresh_su_t50, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 50, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, resource_allocation).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_proxy_sufficiency__catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_proxy_sufficiency__hybrid_degradation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the catastrophe_proxy_sufficiency kernel decomposes into four readings, this file being simulation_fidelity_threshold. Shared referent — threshold-governed simulation-based competence retention in high-reliability organizations — but each member authors its own reading-indexed epsilon over that referent (see narrative_context for the epsilon deltas). Upstream/downstream structure: transfer-of-training research and vendor validation work feed this reading's threshold claim; this reading in turn pressures the proxy reading's legitimacy (if sufficiency requires crossing a threshold, unqualified equivalence claims lose ground) and supplies the quantitative target the necessity reading attacks. All four members link one another via affects_constraints; orphaning any member would hide the kernel contest from contamination propagation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
