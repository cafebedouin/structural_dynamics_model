% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__hybrid_degradation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: Simulation-Based Certification as Catastrophe Proxy — Hybrid Degradation Reading
 *   domain: safety engineering/organizational learning/high-reliability organizations
 *
 * SUMMARY:
 *   Since the late 1970s, high-hazard industries — aviation, nuclear power,
 *   petrochemical processing, emergency medicine — have progressively
 *   replaced experience with real events by simulation-based training
 *   embedded in mandatory certification. The arrangement solved a genuine
 *   problem: catastrophes cannot be rehearsed live, and regulators needed an
 *   auditable proxy for readiness. This story instantiates the
 *   hybrid_degradation_reading of the catastrophe_proxy_sufficiency kernel:
 *   on that reading, the standing arrangement maintains the procedural layer
 *   of competence (checklists, crew coordination, instrument fluency
 *   measurably improve and persist) while tacit knowledge and stress-response
 *   capacity — historically built by surviving real events and apprenticing
 *   under those who had — decay over generational timescales as the veteran
 *   generation retires and prevention succeeds. The certification industry
 *   collects recurring revenue for maintaining the visible layer while the
 *   invisible layer erodes unpriced; the shortfall lands on future cohorts
 *   and on the publics exposed to rare events. Per the epsilon-invariance
 *   principle, the colloquial label 'simulation suffices for
 *   catastrophe-readiness' decomposes into four structurally distinct
 *   readings; this file authors only this one, with epsilon assessed over the
 *   standing simulation-certification arrangement as this reading sees it.
 *   Claimed type and metrics are authored independently: the type states my
 *   structural belief; the metrics state what I take to be descriptively
 *   true.
 *
 * KEY AGENTS:
 *   - - certification_training_industry: Primary beneficiary (organized/arbitrage) — collects recurring certification and training revenue on every mandated cycle
 *   - - simulator_vendors: Secondary beneficiary (institutional/arbitrage) — converts mandated hours and fidelity standards into equipment orders
 *   - - safety_regulators: Agenda-setter (institutional/identity_locked) — defines and enforces the simulation-hour currency; its institutional self-concept is fused with the currency it administers
 *   - - junior_operator_cohorts: Primary target (moderate/constrained) — pays recurring fees; inherits procedural competence without the tacit layer
 *   - - public_near_hazardous_facilities: Ultimate target (powerless/constrained) — bears the difference between certified and actual readiness when rare events occur
 *   - - veteran_operators: Excluded carrier (moderate/constrained) — holds the depreciating tacit asset; testimony uncertifiable; aging out
 *   - - catastrophe_insurers: Exposed outsider (institutional/arbitrage) — prices the tail risk using the regime's own output
 *   - - hro_researchers: Analytical observer (moderate/analytical) — documents decay; no enforcement lever
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.65).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.58).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "Simulation-Based Certification as Catastrophe Proxy — Hybrid Degradation Reading").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "safety engineering/organizational learning/high-reliability organizations").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency__hybrid_degradation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 'fe7ffca7-1fb4-44e9-b27d-54d7b544f6d5').
narrative_ontology:cs_kernel_codification('fe7ffca7-1fb4-44e9-b27d-54d7b544f6d5', distributed).
narrative_ontology:cs_authority_grounding('fe7ffca7-1fb4-44e9-b27d-54d7b544f6d5', distributed).
narrative_ontology:cs_reading_relation('fe7ffca7-1fb4-44e9-b27d-54d7b544f6d5', catastrophe_proxy_sufficiency__simulation_as_proxy_reading, forecloses).
narrative_ontology:cs_reading_relation('fe7ffca7-1fb4-44e9-b27d-54d7b544f6d5', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe7ffca7-1fb4-44e9-b27d-54d7b544f6d5', catastrophe_proxy_sufficiency__fidelity_threshold_reading, coexists_with).
narrative_ontology:cs_axiom('fe7ffca7-1fb4-44e9-b27d-54d7b544f6d5', foundational, generational_tacit_decay_without_real_events).
narrative_ontology:cs_axiom_status(generational_tacit_decay_without_real_events, holdable).
narrative_ontology:cs_axiom_grounding('fe7ffca7-1fb4-44e9-b27d-54d7b544f6d5', generational_tacit_decay_without_real_events, empirically_contingent).
narrative_ontology:cs_axiom('fe7ffca7-1fb4-44e9-b27d-54d7b544f6d5', foundational, procedural_layer_genuinely_maintained).
narrative_ontology:cs_axiom_status(procedural_layer_genuinely_maintained, holdable).
narrative_ontology:cs_axiom_grounding('fe7ffca7-1fb4-44e9-b27d-54d7b544f6d5', procedural_layer_genuinely_maintained, empirically_contingent).
narrative_ontology:cs_reference_frame('fe7ffca7-1fb4-44e9-b27d-54d7b544f6d5', hybrid_partial_substitution_baseline).
narrative_ontology:cs_drift_state('fe7ffca7-1fb4-44e9-b27d-54d7b544f6d5', contemporary_post_veteran_generation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fe7ffca7-1fb4-44e9-b27d-54d7b544f6d5', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_training_industry).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulator_vendors).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, junior_operator_cohorts).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, public_near_hazardous_facilities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, junior_operator_cohorts).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__hybrid_degradation_reading, procedural_competence_auditability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the number of simulator hours, scenario types, and recertification intervals required for licenses in aviation, nuclear operations, and process industries. Accredits training providers and accepts their certificates as evidence of readiness. Their audits can read hours and checkride results but have no instrument for the experience that retired operators carried. Revising the framework would mean telling legislatures and the public that the existing currency was incomplete — a step that would unsettle the agency's own self-concept as the guarantor of verifiable safety.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, safety_regulators, agenda_setter,
    institutional, generational, identity_locked, national).

% Operates academies and recurrent-training programs under multi-year contracts with airlines, plant operators, and hospital systems. Revenue recurs on every recertification cycle; growth comes from expanding mandated hour requirements and new scenario modules. Its instructor corps is drawn increasingly from simulation-native cohorts. Pivoting to other corporate-training markets is straightforward if demand shifts.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_training_industry, beneficiary,
    organized, immediate, arbitrage, global).

% Builds and leases full-motion simulators, virtual-reality rigs, and scenario software. Sales track regulatory mandates — each expansion of required hours or fidelity standards converts directly into orders. Upgrade cycles synchronize to recertification rule revisions. Markets span every high-hazard industry, so demand shocks in one sector are absorbed elsewhere.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulator_vendors, beneficiary,
    institutional, biographical, arbitrage, global).

% Enter cockpits, control rooms, and emergency teams having logged hundreds of simulator hours and zero real emergencies. They pay for initial type ratings and pay again on every recertification cycle, often through payroll deductions or personal debt. What they receive is real: standardized procedures, crew-resource habits, instrument fluency. What they did not receive is the pattern library that came from living through events — and the colleagues who had it are retiring. Leaving the profession means forfeiting the license investment.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, junior_operator_cohorts, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__hybrid_degradation_reading, junior_operator_cohorts, beneficiary).

% Lives downwind, downtrack, or downtown from the systems these crews operate. Funds the regime indirectly through fares, rates, premiums, and taxes, and absorbs the difference between certified readiness and actual readiness when a rare event finds the gap. Relocation is possible but costly and socially disruptive; most have no practical exit from the risk envelope.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, public_near_hazardous_facilities, payer,
    powerless, generational, constrained, regional).

% The cohort that learned from real events — incidents, near-misses, in many cases one genuine catastrophe apiece. Their judgment lives in stories, habits, and pattern recognition that no scenario script encodes. Certification frameworks treat their testimony as anecdote: it cannot be audited, scheduled, or billed. They are aging out of the workforce faster than their experience is being converted into anything the regime can carry forward.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, veteran_operators, excluded,
    moderate, biographical, constrained, global).

% Prices the tail risk these facilities and fleets generate. Loss models ingest certification statistics as readiness signals, which makes them consumers of the regime's output rather than participants in its design. When losses depart from models, they reprice or withdraw coverage rather than revise the training framework. They have standing to challenge the readiness currency but no seat in the committees that define it.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_insurers, excluded,
    institutional, biographical, arbitrage, global).

% Studies how organizations detect and respond to the unexpected — the literature on mindfulness, normalization of deviance, and drift into failure. Publishes the cohort comparisons and post-accident analyses that bear on the decay question, and holds no enforcement power over certification standards. Findings enter the regime as citations, not requirements.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, hro_researchers, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_training_industry).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__hybrid_degradation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives regulators, employers, and insurers a common, auditable currency of readiness — standardized hours, scenarios, and checkrides — and lets thousands of dispersed operators rehearse dangerous procedures without anyone having to cause the danger. Solves the scale problem of competence assurance: how to verify readiness across a global industry without waiting for events that must not happen.
% TRANSFER_FUNCTION: Moves money from operators and their customers (fares, rates, premiums, taxes) to training providers and simulator makers on every certification cycle; moves institutional confidence from demonstrated performance under real conditions to certified completion of prescribed scenarios; moves the burden of rare-event readiness from experienced people onto documented procedure.
% ABSENT_VOICES: Veteran operators with real-event experience would testify that the scenarios miss what mattered, but their knowledge is uncertifiable and they are retiring; catastrophe insurers would challenge the readiness currency their own models consume; residents near hazardous facilities bear the tail risk and appear only after failures, as claimants or witnesses.
% DISAPPEARANCE_RATIONALE: Overnight removal would strip licensing of its competence currency: airlines, plants, and hospitals could not demonstrate legally recognized readiness, hiring and insurance would reprice around direct observation and apprenticeship, and the training industry's revenue base would evaporate. The rearrangement would be chaotic but real — the arrangement is load-bearing for how modern high-hazard industries prove competence.
% FOUNDING_PROBLEM: Mid-century disasters and near-misses left regulators needing a way to train people for rare, high-consequence events that could not ethically or economically be rehearsed live, and needing an auditable substitute for the scarce experience of operators who had lived through such events.
% FOUNDING_PROBLEM_CORROBORATION: The training industry attests the problem is solved and the currency sound. Corroboration from outside the benefiting parties cuts the other way: accident investigation boards repeatedly document crews who were fully current yet failed under novel conditions, and the high-reliability-organization research literature documents experience decay in event-free periods — both attesting the founding problem is live and only partly solved. No party outside the industry attests that the problem is closed.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__hybrid_degradation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__hybrid_degradation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is 0.65: the arrangement transfers substantial recurring payment for a readiness product that, on this reading, covers less than it certifies over generational horizons — the gap between certified currency and actual capability is the extracted margin, and it widens as the veteran generation exits. Suppression is 0.58 and is authored as a raw structural property, unscaled by power or scope: recertification gates careers, regulators credit only accredited simulation hours, and the decisive alternative — real-event exposure — is unavailable by design because prevention succeeds; what remains of the alternative space (near-miss immersion, live drills, apprenticeship credit) is structurally uncredited rather than banned. Theater ratio 0.45: a large and growing share of recurrent training repeats anticipated scenarios and accumulates auditable hours rather than injecting genuine surprise — functional, but increasingly ritual. Accessibility collapse 0.62: once the decay mechanism is granted, alternatives narrow sharply — real catastrophes cannot be manufactured, and the partial substitutes that remain are marginal and uncredited, though not zero. Resistance 0.38: high-reliability-organization scholarship, accident-board findings, and periodic regulatory dissent resist the currency, but the evidence accumulates on a generational clock while certification cycles renew annually, so resistance stays persistent and weak. The measurement series share one six-point grid (1980-2025) across all three tracked metrics; all three rise — extraction accumulates as rent layers onto a real coordination function, theater grows as recertification ritualizes, and the suppression requirement climbs as the enforcement machinery (mandated hours, accreditation, audit) matured and hardened across industries.
 *
 * PERSPECTIVAL GAP:
 *   Seats should classify differently. From the certification industry's and vendors' seats the arrangement is a service they deliver and a market they serve — coordination with a healthy margin. From the junior-cohort seat the same structure is a toll booth on a career: real payments, real procedural benefit, and an inherited deficit they did not choose and cannot see. From the regulator's seat the arrangement is auditability itself — the only readable form competence has ever taken — so the unreadable decay registers as noise, not signal. The public seat experiences nothing until a rare event converts the invisible deficit into casualties. Identity-lock dynamics concentrate in the regulator seat: the fusion mechanism is institutional identity — the organization has become its function, 'the agency that certifies readiness' — so revision pressure routes into expanding the currency (more hours, higher fidelity mandates) rather than questioning it; if that frame broke and the regulator conceded the currency incomplete, enforcement posture would shift from expansion to supplementation (crediting live drills and apprenticeship), raising the accessibility of alternatives and lowering suppression. The engine computes these divergences from the structural data; the divergence between the industry seat's computed type and the payer seats' computed type is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (certification_training_industry, simulator_vendors) derive directionality near the beneficiary pole: they collect on every cycle, hold arbitrage-grade exit across industries, and bear none of the deferred cost. Declared victims map to the target pole: junior_operator_cohorts are career-gated with constrained exit and pay on every cycle; public_near_hazardous_facilities are diffuse, effectively immobile, and bear the tail of the deficit. Safety_regulators sit between poles: they administer rather than collect, but their legitimacy is invested in the currency, which pulls their effective position off pure neutrality — noted here rather than forced with an override, since the derivation from agenda-setting plus identity-locked exit already places them mid-range. Veteran_operators are the anomalous seat: they hold the asset the arrangement quietly liquidates, but they are exiting the system by retirement regardless, so their structural position reads as excluded voice rather than payable target. Catastrophe_insurers are exposed to the deficit financially but hold repricing power, keeping them nearer the beneficiary end than their rhetorical opposition suggests.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — rehearse the unrehearsable, auditably — is still live, so this is not a dead-mandate zombie: the mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges and finds no dead-problem flag, correctly. The mandatrophy risk here is subtler: the arrangement's original justification (no live alternative exists) has been supplemented by a self-perpetuating revenue logic that expands mandated hours because mandated hours are the revenue base. Classifying this as tangled_rope rather than rope prevents the coordination story from laundering the asymmetry — someone is coordinated (crews get a common procedural standard) and someone pays through the same structure (cohorts and publics carry an unpriced decay). Classifying it as tangled_rope rather than snare preserves the real coordination achievement: early simulation regimes measurably reduced procedural error, and no live alternative exists to suppress. Resolution paths run through the omegas: if the proxy reading wins (no generational decay), the story should relax toward rope; if the necessity reading wins (simulation maintains nothing durable), it should harden toward snare; if a fidelity threshold proves reachable, the extraction becomes a transitional cost — scaffold-shaped — pending technology.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is the hybrid_degradation_reading the correct instantiation of the catastrophe_proxy_sufficiency kernel, or do the proxy, necessity, or fidelity-threshold readings better describe the standing arrangement?',
    'Longitudinal cohort studies comparing operators trained and maintained purely in simulation against cohorts with real-event exposure, controlling for automation and staffing; convergence of accident-board findings across jurisdictions.',
    'If the proxy reading wins, this story''s extraction relaxes toward coordination cost and the type trends rope; if the necessity reading wins, extraction hardens and the type trends snare; if the fidelity-threshold reading wins, the extraction becomes transitional and scaffold-shaped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of the catastrophe-proxy kernel correctly describes the arrangement.').

omega_variable(
    tacit_capacity_measurability,
    'Can tacit knowledge and stress-response capacity be measured independently of the rare real events in which alone their absence becomes visible?',
    'Surrogate instruments: decision-latency under injected surprise in undisclosed scenarios, near-miss response quality audits, cross-cohort comparison in the aftermath of any real event.',
    'If unmeasurable, the decay mechanism is unfalsifiable inside the regime and the arrangement persists by epistemic closure — the paying seats cannot even in principle discover what they are not getting, which strengthens the extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_capacity_measurability, empirical, 'Measurability of the capacities this reading claims decay.').

omega_variable(
    generational_attribution_confounding,
    'Is observed competence decay attributable to simulation reliance specifically, or confounded by cockpit and control-room automation, staffing reductions, and organizational change over the same decades?',
    'Matched comparison of facilities that retained live-drill and apprenticeship programs against pure-simulation regimes, holding automation levels constant.',
    'If confounded, part of the measured extraction belongs to broader organizational dynamics and the certification regime''s share of epsilon falls; if clean, the regime owns the decay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_attribution_confounding, empirical, 'Attribution of decay to the simulation regime versus concurrent industry changes.').

omega_variable(
    decay_floor_vs_monotonic,
    'Does tacit and stress-response capacity decay monotonically toward failure, or stabilize at a floor as simulation-native cohorts accumulate their own near-misses and scenario realism improves?',
    'Multi-decade cohort tracking across successive simulation-native generations; comparison of second-generation simulator-trained crews against the first.',
    'A floor bounds the extraction and supports a stable tangled-rope verdict; monotonic decay implies widening unpriced liability and eventual drift toward snare as margins erode without limit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decay_floor_vs_monotonic, empirical, 'Shape of the decay curve over generational time.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t1980, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement_basis(cata_tr_t1980, observed).
narrative_ontology:measurement(cata_tr_t1990, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 1990, 0.22).
narrative_ontology:measurement_basis(cata_tr_t1990, observed).
narrative_ontology:measurement(cata_tr_t2000, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement_basis(cata_tr_t2000, observed).
narrative_ontology:measurement(cata_tr_t2010, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 2010, 0.36).
narrative_ontology:measurement_basis(cata_tr_t2010, observed).
narrative_ontology:measurement(cata_tr_t2020, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 2020, 0.41).
narrative_ontology:measurement_basis(cata_tr_t2020, observed).
narrative_ontology:measurement(cata_tr_t2025, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 2025, 0.45).
narrative_ontology:measurement_basis(cata_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t1980, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement_basis(cata_be_t1980, observed).
narrative_ontology:measurement(cata_be_t1990, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement_basis(cata_be_t1990, observed).
narrative_ontology:measurement(cata_be_t2000, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement_basis(cata_be_t2000, observed).
narrative_ontology:measurement(cata_be_t2010, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 2010, 0.56).
narrative_ontology:measurement_basis(cata_be_t2010, observed).
narrative_ontology:measurement(cata_be_t2020, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 2020, 0.61).
narrative_ontology:measurement_basis(cata_be_t2020, observed).
narrative_ontology:measurement(cata_be_t2025, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 2025, 0.65).
narrative_ontology:measurement_basis(cata_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t1980, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement_basis(cata_su_t1980, observed).
narrative_ontology:measurement(cata_su_t1990, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 1990, 0.46).
narrative_ontology:measurement_basis(cata_su_t1990, observed).
narrative_ontology:measurement(cata_su_t2000, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 2000, 0.51).
narrative_ontology:measurement_basis(cata_su_t2000, observed).
narrative_ontology:measurement(cata_su_t2010, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement_basis(cata_su_t2010, observed).
narrative_ontology:measurement(cata_su_t2020, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 2020, 0.57).
narrative_ontology:measurement_basis(cata_su_t2020, observed).
narrative_ontology:measurement(cata_su_t2025, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 2025, 0.58).
narrative_ontology:measurement_basis(cata_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__hybrid_degradation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency__simulation_as_proxy_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency__catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency__fidelity_threshold_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial claim 'simulation suffices for catastrophe-readiness' fails the epsilon-invariance test as a single story — measuring sufficiency by procedural outcomes yields negligible extraction, measuring it by tacit and stress-response retention over generational time yields substantial extraction. Decomposed into four readings sharing one referent (the standing simulation-certification arrangement): simulation_as_proxy_catastrophe_reading, catastrophe_necessity_reading, simulation_fidelity_threshold, and this hybrid_degradation_reading. Upstream/downstream: the proxy reading is the regime's official self-description and supplies the legitimacy the certification industry monetizes; the necessity reading is the maximal critique; this hybrid reading sits between and inherits evidence from both. Edges declared to all three siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
