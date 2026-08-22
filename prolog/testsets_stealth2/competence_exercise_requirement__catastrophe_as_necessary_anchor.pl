% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__catastrophe_as_necessary_anchor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__catastrophe_as_necessary_anchor, []).

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
 *   constraint_id: competence_exercise_requirement__catastrophe_as_necessary_anchor
 *   human_readable: Catastrophe-Anchored Competence Maintenance Doctrine
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   Mature high-hazard industries — commercial aviation, nuclear operations,
 *   surgery, offshore energy — maintain operational competence through a
 *   learning loop anchored to real events: an accident, serious incident, or
 *   near-miss triggers investigation; findings become procedures,
 *   regulations, and training revisions; the organization treats each event
 *   as the price of calibrated competence. Simulation exists throughout the
 *   system but is institutionally subordinated to the real-event anchor ('you
 *   cannot simulate panic'; 'the board learns what no simulator teaches').
 *   This story instantiates ONE reading of the
 *   competence_exercise_requirement kernel: the
 *   catastrophe_as_necessary_anchor reading, which holds that only real
 *   catastrophic events or near-misses provide the irreducible exercise
 *   maintaining competence, that the competence kernel atrophies during
 *   catastrophe-free periods despite simulation, and that the first real
 *   event reveals the decay — the gap between muscle memory and
 *   knowing-about. The sibling readings, simulation_as_adequate_exercise and
 *   hybrid_dependency, are separate constraints with their own epsilon,
 *   beneficiary structures, and classifications; they are not averaged into
 *   this file. The claim/metric independence rule applies: the arrangement is
 *   CLAIMED here as tangled_rope, while the metrics below are authored
 *   independently as descriptive of its actual operation.
 *
 * KEY AGENTS:
 *   - - accident_investigation_bodies: Agenda-setter (institutional/constrained) — runs the event-to-lesson conversion loop; each event feeds its mandate, budget, and epistemic authority
 *   - - regulatory_agencies: Beneficiary with agenda-setting duties (institutional/constrained) — converts findings into binding rules; legitimacy built on the visible disaster-inquiry-reform sequence
 *   - - post_incident_training_industry: Beneficiary (organized/mobile) — sells curricula and certifications manufactured from event-derived material
 *   - - veteran_event_operators: Beneficiary (moderate/identity_locked) — authority premium rests on lived event experience; professional self-concept fused with the anchor
 *   - - catastrophe_casualties: Payer (powerless/trapped) — passengers, patients, and workers killed or injured in the events serving as the system's exercises
 *   - - frontline_operators: Payer with beneficiary position (organized/constrained) — staff the real events, bearing jeopardy and collecting calibrated competence
 *   - - simulation_research_community: Excluded (organized/mobile) — produces fidelity and transfer evidence; holds no seat in the allocation loop
 *   - - safety_science_analysts: Observer (analytical/analytical) — tracks whether competence tracks event exposure or training investment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.68).
domain_priors:suppression_score(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.58).
domain_priors:theater_ratio(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__catastrophe_as_necessary_anchor, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__catastrophe_as_necessary_anchor, "Catastrophe-Anchored Competence Maintenance Doctrine").
narrative_ontology:topic_domain(competence_exercise_requirement__catastrophe_as_necessary_anchor, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_requirement__catastrophe_as_necessary_anchor).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'b0c7fdc0-09db-4023-b129-e44b31616456').
narrative_ontology:cs_kernel_codification('b0c7fdc0-09db-4023-b129-e44b31616456', distributed).
narrative_ontology:cs_authority_grounding('b0c7fdc0-09db-4023-b129-e44b31616456', practice).
narrative_ontology:cs_interpretation_layer_present('b0c7fdc0-09db-4023-b129-e44b31616456').
narrative_ontology:cs_reading_relation('b0c7fdc0-09db-4023-b129-e44b31616456', competence_exercise_requirement__simulation_as_adequate_exercise, forecloses).
narrative_ontology:cs_reading_relation('b0c7fdc0-09db-4023-b129-e44b31616456', competence_exercise_requirement__hybrid_dependency, coexists_with).
narrative_ontology:cs_axiom('b0c7fdc0-09db-4023-b129-e44b31616456', foundational, real_event_exposure_irreplaceable_for_kernel_exercise).
narrative_ontology:cs_axiom_status(real_event_exposure_irreplaceable_for_kernel_exercise, holdable).
narrative_ontology:cs_axiom_grounding('b0c7fdc0-09db-4023-b129-e44b31616456', real_event_exposure_irreplaceable_for_kernel_exercise, empirically_contingent).
narrative_ontology:cs_axiom('b0c7fdc0-09db-4023-b129-e44b31616456', foundational, procedural_memory_requires_genuine_arousal).
narrative_ontology:cs_axiom_status(procedural_memory_requires_genuine_arousal, holdable).
narrative_ontology:cs_axiom_grounding('b0c7fdc0-09db-4023-b129-e44b31616456', procedural_memory_requires_genuine_arousal, empirically_contingent).
narrative_ontology:cs_reference_frame('b0c7fdc0-09db-4023-b129-e44b31616456', catastrophe_anchored_competence).
narrative_ontology:cs_drift_state('b0c7fdc0-09db-4023-b129-e44b31616456', contemporary_simulation_maturity_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b0c7fdc0-09db-4023-b129-e44b31616456', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, accident_investigation_bodies).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, regulatory_agencies).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, post_incident_training_industry).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, veteran_event_operators).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, catastrophe_casualties).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, frontline_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, frontline_operators).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__catastrophe_as_necessary_anchor, skill_decay_without_real_event_exposure_hypothesis).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__catastrophe_as_necessary_anchor, failure_driven_learning_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the statutory mandate to investigate every significant event in its domain. Each investigation produces findings that become regulations, procedures, and training requirements. Its budget, jurisdiction, and epistemic authority have historically expanded after major events. Abandoning the event-anchored learning model would dissolve the body's own reason for existence, so exit from the model is not realistically available to it.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, accident_investigation_bodies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__catastrophe_as_necessary_anchor, accident_investigation_bodies, beneficiary).

% Converts investigation findings into binding rules and mandatory training standards. Each catastrophe opens a rulemaking window that enlarges its remit. Its public legitimacy is built on the visible sequence of disaster, inquiry, and reform, which makes it a durable collector from the arrangement even though it does not run the investigations itself.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, regulatory_agencies, beneficiary,
    institutional, generational, constrained, national).

% Sells curricula, case-study libraries, and certifications manufactured from event-derived material. Its current product line is priced on the premise that event-derived content is superior to synthetic scenarios. It could pivot to simulation products — its exit is genuinely mobile — but its existing inventory and brand are invested in the anchor's exclusivity.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, post_incident_training_industry, beneficiary,
    organized, biographical, mobile, global).

% Survivors and participants of landmark events whose testimony anchors investigations and whose standing in the profession rests on having been present when it happened. Their authority premium cannot be re-authenticated through simulator hours; their professional self-concept is constituted by the lived event. Defending the anchor's exclusivity is, for them, indistinguishable from defending their own standing.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, veteran_event_operators, beneficiary,
    moderate, biographical, identity_locked, global).

% Passengers, patients, and workers killed or injured in the events that serve as the system's exercises. They bear the full cost of the calibration their harm purchases, cannot consent or decline in advance, and cannot collect the learning that results. Being dead, injured, or dispersed, they have no seat and no successor organization capable of aggregating their claim.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, catastrophe_casualties, payer,
    powerless, immediate, trapped, regional).

% Pilots, surgeons, reactor crews, and offshore crews who staff the real events. They are exposed to genuine jeopardy, trauma, and post-event blame, and they simultaneously collect the calibrated competence the events produce. Unionized and vocal inside the system, yet bound to the profession by licensing and sunk training investment; leaving is possible only at prohibitive personal cost.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, frontline_operators, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__catastrophe_as_necessary_anchor, frontline_operators, beneficiary).

% Human-factors and simulation scientists producing fidelity, arousal, and transfer-of-training evidence. They publish critiques of catastrophe-dependent learning and design non-jeopardy alternatives, but hold no vote inside the investigation-rulemaking loop that allocates exercise resources. Their evidence tends to enter the loop only after real events retroactively confirm it.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_research_community, excluded,
    organized, biographical, mobile, global).

% High-reliability-organization researchers and historians tracking, across industries and decades, whether operational competence tracks real-event exposure or training investment. They see the whole loop, including who pays for it and who staffs it, from outside any of the paying or collecting positions.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, safety_science_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__catastrophe_as_necessary_anchor, accident_investigation_bodies).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__catastrophe_as_necessary_anchor, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Real events surface failure modes that no exercise designer anticipated — unknown-unknowns, startle degradation, organizational breakdown under irreversible consequence. The arrangement solves the calibration problem: it uses the system's actual failures as the ground-truth test of whether competence is real, and routes what the failures reveal into procedures, rules, and training across the entire fleet.
% TRANSFER_FUNCTION: Moves the cost of competence calibration — death, injury, trauma, destroyed assets — from unconsenting system users to the organizations, professions, and regulators who convert each event into procedures, jurisdiction, budgets, and expert standing.
% ABSENT_VOICES: The casualties are absent by definition — dead, injured, or dispersed, with no seat and no successor organization to speak for the next cohort of payers. Prospective users cannot consent to being enrolled in someone else's exercise. The simulation research community publishes but holds no vote inside the investigation-rulemaking loop that allocates exercise resources; its evidence enters mainly after events validate it retroactively.
% DISAPPEARANCE_RATIONALE: Overnight removal would force every high-hazard organization to choose between accepting uncalibrated competence drift and rapidly building simulation-and-audit infrastructure at scale; investigation bodies would lose their statutory reason for existence, the disaster-inquiry-reform political economy would lose its engine, and training markets built on event-derived material would collapse or pivot.
% FOUNDING_PROBLEM: Immature high-hazard systems failed in ways nobody had imagined; before enough real events accumulated, neither designers nor operators knew the failure modes, and event-driven investigation was the only available teacher.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties on both sides: the academic human-factors and skill-decay literature, independent of the investigation bodies, attests the founding problem was real for immature systems; industry safety-data analyses conducted outside the loop attest that informational yield per event has declined sharply in mature systems. No party wholly outside the dispute attests that the problem is fully dead or fully live — the contest itself is the finding.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__catastrophe_as_necessary_anchor, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__catastrophe_as_necessary_anchor, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 because the arrangement's exercise is funded by harms borne by people who neither consented nor collect: each calibration cycle consumes casualties, and as systems mature the informational yield per event declines while the human cost stays constant, so the same tragedy buys less learning over time. Suppression is 0.58 and is authored as a raw structural property, unscaled by power or scope: the anchor defends itself by defunding rivals — simulation budgets are framed as inadequate preparation, near-miss data is hoarded as proprietary experience, and careers arguing for simulation-first regimes stall. Theater is 0.41: a substantial share of post-event activity now produces reports, hearings, and memorial recommendations that change little operationally, though the core conversion of findings into procedure remains real. Accessibility collapse is low (0.35) because the alternatives are visible and workable — high-fidelity simulation and hybrid regimes operate credibly in adjacent industries — so understanding the constraint does not close exits. Resistance is moderate (0.55): human-factors researchers, parts of the regulator community, and segments of the training market actively push simulation-first and hybrid designs. The measurement series share one time grid — every tracked metric is authored at every examined point — and show monotonic accumulation rather than oscillation: extraction and theater rise together as yield-per-event decays, and the suppression requirement rises as maturing simulation makes the anchor progressively harder to defend.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat (investigation bodies, agencies) the arrangement is indispensable pedagogy they administer: without real events the system flies blind, and their stewardship is the mechanism that converts tragedy into safety. From the casualty seat the same structure is non-consensual tuition: their harm purchases competence they will not live to use. From the frontline-operator seat it is both at once — genuine jeopardy borne and genuine competence collected — which is why that seat carries a dual role. From the excluded simulation-research seat the arrangement is a dogma that discounts their evidence until events retroactively validate it. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. Catastrophe_casualties sit at the full-target end: declared victims, powerless, and trapped — deceased, injured, or dispersed, with the classic coalition failure that keeps an aggregate-magnitude payer seat structurally mute. Accident_investigation_bodies and regulatory_agencies sit near the beneficiary end: they collect mandate, budget, and authority from each event and administer the loop. Post_incident_training_industry collects revenue with arbitrage-grade mobility — it could pivot to simulation products, which tempers its commitment to the anchor. Veteran_event_operators are beneficiaries with identity_locked exit: their authority premium and professional self-concept are constituted by having been present at real events; simulator hours cannot re-authenticate that standing, so defending the anchor is identity defense. Frontline_operators are declared victims but carry a genuine secondary beneficiary position — they pay in jeopardy and collect in competence — placing them mid-scale rather than at the target extreme. Spatial scope runs national-to-global, modestly amplifying effective extraction for everyone downstream of the investigation chokepoint because verification of what was actually learned grows harder with scale.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — failure modes invisible until reality reveals them — was live and urgent for immature systems: early aviation and nuclear power genuinely could not anticipate their failure modes, and event-driven learning was the only available teacher. For mature systems the status is contested: the unknown-unknown reservoir has thinned, and independent safety-data analyses outside the investigation bodies attest diminishing informational yield per event. Because the founding problem is contested while the world still rearranges around the arrangement (disappearance_verdict: world_rearranges), the mismatch consumer should expect a partial-capture signal rather than a clean zombie flag: the loop still performs real conversion work, but a growing share of its persistence is institutional self-maintenance. The tangled_rope classification prevents both mislabelings: a pure-extraction reading would erase the genuine calibration function (real events do surface what simulation misses), and a pure-coordination reading would erase the subsidy structure (the exercise is paid for by people who never enrolled). Mandatrophy resolution here is partial and ongoing: the arrangement is transitioning from necessity toward habit, and the rate of that transition is exactly what the temporal series measures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the competence_exercise_requirement kernel — the catastrophe_as_necessary_anchor reading. What would change structurally if a sibling reading were adopted instead?',
    'Adoption of simulation_as_adequate_exercise would empty the victim set (no real events required; extraction falls toward the coordination-cost floor). Adoption of hybrid_dependency would retain a reduced victim set (periodic real anchoring) and split the cost structure between simulation infrastructure and residual event exposure. The disagreement is located in whether the competence kernel''s exercise requires genuine jeopardy contact or whether high-fidelity simulation reproduces it.',
    'Sibling adoption rewrites the beneficiary/victim structure and reclassifies the arrangement; this file''s metrics are valid only for the catastrophe-exclusive reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this story is one of three readings of the competence-exercise kernel.').

omega_variable(
    skill_decay_attribution_confound,
    'Does competence measurably atrophy during catastrophe-free periods because real-event exercise is absent, or do confounds — workforce aging, procedural drift, complacency, staffing churn — produce the observed decay?',
    'Longitudinal matched-cohort comparison of comparable organizations differing in real-event exposure while controlling for staffing, technology, and training hours.',
    'If decay is confounded, the anchor''s necessity premise fails empirically and the arrangement re-reads as rent preservation around investigation authority; if clean, the reading''s core axiom strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_decay_attribution_confound, empirical, 'Whether observed post-calm decay is caused by missing real-event exercise.').

omega_variable(
    near_miss_substitution_sufficiency,
    'Can engineered near-miss density — line checks, non-jeopardy audits, instrumented drills that stop short of harm — substitute for full catastrophes as the anchoring exercise?',
    'Compare competence retention in organizations with rich near-miss regimes against those relying on rare severe events, using blind performance audits.',
    'If near-misses suffice, the necessary toll collapses toward zero and the arrangement''s victim-funded character becomes indefensible; if not, the reading retains force with a far cheaper anchor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(near_miss_substitution_sufficiency, empirical, 'Whether the reading''s own near-miss clause can displace catastrophes.').

omega_variable(
    simulation_fidelity_ceiling,
    'Is there a categorical fidelity ceiling — startle, genuine jeopardy, irreversible consequence — that simulation cannot cross, or does simulation adequacy asymptotically approach the real-event benchmark?',
    'Transfer-of-training meta-analyses and psychophysiological arousal studies comparing simulated versus real-event performance under matched conditions.',
    'A hard ceiling supports the reading''s exclusivity axiom; demonstrated asymptotic approach supports the simulation-adequate sibling and erodes this reading''s victim-funded structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_ceiling, empirical, 'Whether simulation can in principle reproduce the exercising effect of real jeopardy.').

omega_variable(
    victim_tuition_consent_legitimacy,
    'Even if real events are the irreplaceable exercise, is non-consensual exposure of third parties a legitimate way to fund organizational learning, or does legitimacy require the payers'' consent or compensation?',
    'Not resolvable by data alone: turns on whether the learning produced is a public good justifying imposed risk, and what compensation standard would apply to those harmed.',
    'If illegitimate, the arrangement is condemned regardless of the empirical necessity question and the remedy shifts to consent-based exercise designs; if legitimate, part of the measured extraction is ratified rather than remediable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_tuition_consent_legitimacy, preference, 'Normative status of unconsenting third parties bearing the learning cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catastrophe_anchor_tr_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(catastrophe_anchor_tr_t0, observed).
narrative_ontology:measurement(catastrophe_anchor_tr_t5, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(catastrophe_anchor_tr_t5, observed).
narrative_ontology:measurement(catastrophe_anchor_tr_t10, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(catastrophe_anchor_tr_t10, observed).
narrative_ontology:measurement(catastrophe_anchor_tr_t15, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 15, 0.3).
narrative_ontology:measurement_basis(catastrophe_anchor_tr_t15, observed).
narrative_ontology:measurement(catastrophe_anchor_tr_t20, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 20, 0.34).
narrative_ontology:measurement_basis(catastrophe_anchor_tr_t20, observed).
narrative_ontology:measurement(catastrophe_anchor_tr_t25, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 25, 0.38).
narrative_ontology:measurement_basis(catastrophe_anchor_tr_t25, observed).
narrative_ontology:measurement(catastrophe_anchor_tr_t30, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(catastrophe_anchor_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(catastrophe_anchor_be_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(catastrophe_anchor_be_t0, observed).
narrative_ontology:measurement(catastrophe_anchor_be_t5, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 5, 0.49).
narrative_ontology:measurement_basis(catastrophe_anchor_be_t5, observed).
narrative_ontology:measurement(catastrophe_anchor_be_t10, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(catastrophe_anchor_be_t10, observed).
narrative_ontology:measurement(catastrophe_anchor_be_t15, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(catastrophe_anchor_be_t15, observed).
narrative_ontology:measurement(catastrophe_anchor_be_t20, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 20, 0.63).
narrative_ontology:measurement_basis(catastrophe_anchor_be_t20, observed).
narrative_ontology:measurement(catastrophe_anchor_be_t25, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(catastrophe_anchor_be_t25, observed).
narrative_ontology:measurement(catastrophe_anchor_be_t30, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(catastrophe_anchor_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(catastrophe_anchor_su_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(catastrophe_anchor_su_t0, observed).
narrative_ontology:measurement(catastrophe_anchor_su_t5, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 5, 0.37).
narrative_ontology:measurement_basis(catastrophe_anchor_su_t5, observed).
narrative_ontology:measurement(catastrophe_anchor_su_t10, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 10, 0.42).
narrative_ontology:measurement_basis(catastrophe_anchor_su_t10, observed).
narrative_ontology:measurement(catastrophe_anchor_su_t15, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 15, 0.47).
narrative_ontology:measurement_basis(catastrophe_anchor_su_t15, observed).
narrative_ontology:measurement(catastrophe_anchor_su_t20, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 20, 0.51).
narrative_ontology:measurement_basis(catastrophe_anchor_su_t20, observed).
narrative_ontology:measurement(catastrophe_anchor_su_t25, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 25, 0.55).
narrative_ontology:measurement_basis(catastrophe_anchor_su_t25, observed).
narrative_ontology:measurement(catastrophe_anchor_su_t30, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(catastrophe_anchor_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__catastrophe_as_necessary_anchor, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_as_adequate_exercise).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, hybrid_dependency).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'what maintains competence in high-hazard work' decomposes into three structurally distinct constraints per the epsilon-invariance principle. This file authors the catastrophe_as_necessary_anchor reading (victim-funded real-event anchor; epsilon 0.68). simulation_as_adequate_exercise authors the simulation-sufficiency claim (victim set near-empty; epsilon near coordination floor). hybrid_dependency authors the mixed claim (partial victim set; intermediate epsilon). The catastrophe reading historically influenced the hybrid reading — hybrid concedes the anchor while denying exclusivity — and simulation-adequacy evidence exerts downstream pressure on this reading's exclusivity axiom. Each member links to the others via network.affects_constraints; none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
