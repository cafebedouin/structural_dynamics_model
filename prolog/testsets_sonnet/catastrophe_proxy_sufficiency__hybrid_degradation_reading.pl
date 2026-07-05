% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__hybrid_degradation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: catastrophe_proxy_sufficiency__hybrid_degradation_reading
 *   human_readable: Simulation-Sustained Competence with Generational Tacit-Knowledge Decay
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This story instantiates the hybrid_degradation_reading of the
 *   catastrophe_proxy_sufficiency kernel: simulation genuinely maintains
 *   observable procedural competence (checklist execution, scenario
 *   compliance) while a separate, less visible layer — tacit judgment and
 *   physiological/psychological stress-response capacity — degrades across
 *   generational timescales precisely because no real catastrophe forces its
 *   renewal. The coordination function (standardized training without
 *   manufacturing disasters) is real. The extraction is that a
 *   certification-and-training apparatus profits from declaring the proxy
 *   sufficient, while the cost of the proxy's incompleteness is deferred onto
 *   future operators and the public who will face the next real crisis with a
 *   thinner margin than certification metrics suggest.
 *
 * KEY AGENTS:
 *   - certification_and_training_industry: primary beneficiary (institutional/arbitrage) — collects recurring revenue from the proxy's presumed sufficiency
 *   - regulatory_bodies: agenda_setter (institutional/constrained) — mandates the proxy because it is auditable, has weak means to detect the tacit-decay mechanism
 *   - junior_operators: primary target (powerless/trapped) — inherit an unverified competence gap
 *   - veteran_operators: partial observer/payer (moderate/constrained) — sense the gap but are structurally overruled
 *   - downstream_public: excluded target (powerless/trapped) — bears undisclosed tail risk
 *   - safety_researchers: analytical observer (analytical/analytical) — studies the divergence but cannot force resolution given the rarity of real test cases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.58).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.47).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "Simulation-Sustained Competence with Generational Tacit-Knowledge Decay").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency__hybrid_degradation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__hybrid_degradation_reading, '57e12256-b841-4692-9f87-48e2781bdfe5').
narrative_ontology:cs_kernel_codification('57e12256-b841-4692-9f87-48e2781bdfe5', distributed).
narrative_ontology:cs_authority_grounding('57e12256-b841-4692-9f87-48e2781bdfe5', expertise).
narrative_ontology:cs_interpretation_layer_present('57e12256-b841-4692-9f87-48e2781bdfe5').
narrative_ontology:cs_reading_relation('57e12256-b841-4692-9f87-48e2781bdfe5', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, coexists_with).
narrative_ontology:cs_reading_relation('57e12256-b841-4692-9f87-48e2781bdfe5', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('57e12256-b841-4692-9f87-48e2781bdfe5', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('57e12256-b841-4692-9f87-48e2781bdfe5', foundational, procedural_and_tacit_competence_are_structurally_separable).
narrative_ontology:cs_axiom_status(procedural_and_tacit_competence_are_structurally_separable, holdable).
narrative_ontology:cs_axiom_grounding('57e12256-b841-4692-9f87-48e2781bdfe5', procedural_and_tacit_competence_are_structurally_separable, empirically_contingent).
narrative_ontology:cs_axiom('57e12256-b841-4692-9f87-48e2781bdfe5', foundational, generational_decay_is_intrinsic_not_fidelity_remediable).
narrative_ontology:cs_axiom_status(generational_decay_is_intrinsic_not_fidelity_remediable, holdable).
narrative_ontology:cs_axiom_grounding('57e12256-b841-4692-9f87-48e2781bdfe5', generational_decay_is_intrinsic_not_fidelity_remediable, empirically_contingent).
narrative_ontology:cs_reference_frame('57e12256-b841-4692-9f87-48e2781bdfe5', apprenticeship_era_catastrophe_exposed_competence).
narrative_ontology:cs_drift_state('57e12256-b841-4692-9f87-48e2781bdfe5', contemporary_simulator_centric_training_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('57e12256-b841-4692-9f87-48e2781bdfe5', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_and_training_industry).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, regulatory_liability_shields).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, future_operating_workforce).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, downstream_public_safety_margin).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, veteran_operators).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, junior_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, sells, and administers simulator-based recertification programs. Revenue is recurring and scales with mandated training hours, not with any independently verified competence outcome. Has structural incentive to declare simulation sufficient because that declaration is what makes its product the whole solution rather than one component of it.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_and_training_industry, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_and_training_industry, agenda_setter).

% Mandates simulator hours as the auditable proxy for competence because it is measurable, litigation-defensible, and does not require waiting for or engineering real catastrophic exposure. Enforces compliance through licensing; has weak independent means of detecting whether tacit judgment is actually eroding beneath passing simulator scores.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, regulatory_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Carry tacit knowledge acquired during an earlier era that included real catastrophic or near-catastrophic events. As this cohort retires, the accumulated stress-calibrated judgment leaves the workforce with no equivalent replacement mechanism; they often flag that current simulator drills feel qualitatively different from a live crisis but have no formal channel that outweighs certification metrics.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, veteran_operators, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__hybrid_degradation_reading, veteran_operators, observer).

% Trained almost entirely on simulation, they pass every procedural checkpoint but have never faced an unscripted, physiologically real crisis. They inherit whatever gap exists between simulated and actual stress-response capacity, and they have no way to know how large that gap is until it is tested by a real event, at which point the cost lands on them and on the public they serve.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, junior_operators, payer,
    powerless, biographical, trapped, national).

% Passengers, patients, or residents downstream of the high-reliability system have no visibility into the training regime and no voice in whether simulation-only competence is adequate. They bear the tail risk of an undetected capability gap without having agreed to it or being able to price it.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, downstream_public, excluded,
    powerless, generational, trapped, national).

% Study incident data and near-miss records across organizations to assess whether simulator-trained cohorts perform differently under genuine stress than catastrophe-exposed cohorts. Their findings are contested and slow to accumulate because genuine catastrophic test cases are, by design, rare.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, safety_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_and_training_industry).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__hybrid_degradation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Simulation training solves the real problem that organizations cannot deliberately manufacture actual catastrophes to train personnel, and it does successfully standardize and maintain procedural competence — checklist execution, standard-scenario response, regulatory compliance — across a large workforce without waiting for disaster.
% TRANSFER_FUNCTION: Moves recurring training-and-certification revenue from operating organizations to the certification/training industry, and moves the residual risk of an undetected tacit-competence and stress-response gap from the present (where it is cheap to address) onto future operators and the downstream public (where it surfaces, if at all, as catastrophic failure).
% ABSENT_VOICES: Junior operators who have never faced a live crisis have no standing to contest whether their training is sufficient — they cannot know what they don't know. The downstream public bears the tail risk entirely outside the conversation. Veteran operators who sense the gap are structurally overruled by passing certification metrics that carry regulatory and legal weight their qualitative judgment does not.
% DISAPPEARANCE_RATIONALE: If simulator-based recertification vanished overnight, regulators and organizations would have no scalable alternative and would either revert to ad hoc apprenticeship (slower, less standardized, but arguably more tacit-knowledge-rich) or operate with reduced compliance infrastructure. The certification industry would collapse as a revenue category. Whether the underlying safety outcome improves or worsens is exactly the open empirical question this reading identifies — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: High-reliability organizations needed a way to maintain and verify operator competence at scale without relying on the occurrence of real catastrophic events, which are rare, uncontrollable, and too costly to use as a training mechanism.
% FOUNDING_PROBLEM_CORROBORATION: The certification industry and regulators attest the founding problem is solved — simulator hours are treated as a valid, auditable proxy for competence. Independent safety researchers studying incident response across simulator-only versus catastrophe-exposed cohorts report inconclusive but concerning divergence in stress-response performance, and several veteran operators on record in post-incident reviews have flagged that simulated drills do not reproduce the physiological and judgment conditions of a real unscripted crisis. No party outside the training-and-certification apparatus has affirmatively corroborated that the founding problem is fully solved rather than procedurally papered over.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__hybrid_degradation_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__hybrid_degradation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__hybrid_degradation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises across the interval (0.34 to 0.58) as generational turnover replaces catastrophe-calibrated veterans with simulator-only cohorts, widening the gap between certified competence and actual stress-response capacity while the training apparatus's revenue and regulatory entrenchment grow. Theater ratio rises in parallel (0.22 to 0.52) because an increasing share of training activity is procedural compliance-signaling rather than genuine capability-building — this is the Goodhart signature the hybrid reading predicts: the proxy becomes the target. Suppression rises modestly (0.30 to 0.47) as certification requirements harden into licensing gates that make dissenting judgment (a veteran's qualitative concern) legally and professionally costless to override. Accessibility collapse is moderate (0.40) rather than high because organizations retain the option to supplement simulation with deliberate high-fidelity stress exposure — the alternative is not foreclosed, merely under-incentivized. Resistance is moderate-low (0.35): veteran operators resist informally but rarely have institutional standing to block certification-based staffing decisions.
 *
 * PERSPECTIVAL GAP:
 *   From the certification industry's and regulator's seats, the constraint should classify as much closer to rope: a genuine, actively-used coordination solution to an intractable problem (you cannot manufacture real disasters for training). From junior operators' and the downstream public's seats, the same structure should classify as tangled — real coordination function riding alongside an extraction mechanism that discounts their long-term exposure. The engine's per-seat computation is expected to diverge along exactly this line; that divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The certification-and-training industry sits at the beneficiary end: it collects recurring, procedurally-guaranteed revenue regardless of whether tacit competence is actually maintained, and has structural incentive against admitting insufficiency. Regulatory bodies are a secondary beneficiary of legal defensibility (auditable compliance shields them from liability) but also bear some structural exposure if the gap becomes visible after a failure. Junior operators and the downstream public sit at the target end: they inherit the unmeasured deficit with no mechanism to price or contest it, and their exit options are trapped/constrained respectively. Veteran operators are directionally mixed — partial payers (their judgment is discounted) and partial observers (they retain comparative knowledge the system has no channel for).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (train competence without real disasters) is only partially dead: procedural competence transfer is genuinely still being solved, but the deeper problem — maintaining tacit judgment and stress calibration — was arguably never solved by simulation and is now masked by the proxy's apparent success. Classifying this as tangled_rope rather than snare or mountain prevents two mislabeling errors: treating it as pure extraction (which would ignore the real, non-trivial coordination value simulation provides) and treating it as settled natural necessity (which would hide the certification industry's incentive to never test whether the proxy is actually sufficient).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tacit_decay_measurability,
    'Is generational tacit-knowledge and stress-response decay actually measurable and distinguishable from noise, or is this reading itself an unfalsifiable claim that happens to be intuitively plausible?',
    'Longitudinal comparison of incident-response performance between cohorts trained predominantly on simulation versus cohorts with catastrophe exposure, controlled for domain and simulator fidelity generation; requires rare real-event data that accumulates slowly.',
    'If decay is not measurably real, this reading collapses toward simulation_as_proxy_catastrophe_reading (simulation is sufficient) and the tangled_rope classification loses its victim-side justification, moving toward rope. If decay is confirmed and quantifiable, the tangled_rope reading strengthens and may deserve reclassification toward snare as evidence accumulates that the certification industry actively obscures it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_decay_measurability, empirical, 'Whether the hybrid reading''s core decay mechanism is empirically real or an unfalsifiable narrative.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the sibling readings of catastrophe_proxy_sufficiency disagree — is it about whether decay exists, whether it matters, or whether it is addressable within simulation technology?',
    'This reading (hybrid_degradation) holds that decay exists AND is currently unaddressed by fidelity improvements alone, because the gap is generational and structural (loss of a catastrophe-exposed cohort), not a fidelity engineering problem. simulation_fidelity_threshold holds the gap IS addressable by fidelity engineering. catastrophe_necessity_reading holds no fidelity level can substitute. simulation_as_proxy_catastrophe_reading holds there is no gap to address. Resolving requires isolating whether stress-response degradation tracks simulator fidelity (supporting fidelity_threshold) or persists regardless of fidelity (supporting hybrid_degradation or catastrophe_necessity).',
    'If degradation tracks fidelity closely, this reading should defer to simulation_fidelity_threshold and the tangled_rope classification here may be an artifact of currently-inadequate simulator technology rather than an intrinsic feature of the proxy relationship.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Committer structure: locating exactly where this reading''s premise diverges from its three siblings within the shared kernel.').

omega_variable(
    certification_industry_incentive_strength,
    'How strong is the certification-and-training industry''s actual incentive to suppress evidence of tacit-knowledge decay, versus genuinely believing in the proxy''s sufficiency?',
    'Examine whether training providers fund or suppress independent research into simulator-vs-catastrophe performance divergence, and whether contract structures reward continued certification volume over demonstrated real-world outcome improvement.',
    'Strong suppression incentive supports treating this as closer to snare-adjacent tangled_rope with active extraction; weak incentive (genuine belief, no suppression) supports a more benign tangled_rope reading closer to rope with an honest blind spot.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(certification_industry_incentive_strength, empirical, 'Whether the beneficiary''s stake in proxy-sufficiency is actively defended or merely structurally convenient.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(cata_tr_t8, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(cata_tr_t16, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(cata_tr_t24, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 24, 0.43).
narrative_ontology:measurement(cata_tr_t32, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 32, 0.48).
narrative_ontology:measurement(cata_tr_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 40, 0.52).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(cata_be_t8, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(cata_be_t16, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement(cata_be_t24, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 24, 0.51).
narrative_ontology:measurement(cata_be_t32, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(cata_be_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cata_su_t8, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(cata_su_t16, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 16, 0.38).
narrative_ontology:measurement(cata_su_t24, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 24, 0.42).
narrative_ontology:measurement(cata_su_t32, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 32, 0.45).
narrative_ontology:measurement(cata_su_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 40, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__hybrid_degradation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the catastrophe_proxy_sufficiency kernel. simulation_as_proxy_catastrophe_reading claims simulation is categorically sufficient (would classify closer to rope — no hidden victim). catastrophe_necessity_reading claims simulation is categorically insufficient regardless of fidelity (would identify a different, starker victim structure — near-snare if organizations knowingly substitute simulation for necessary exposure). simulation_fidelity_threshold locates sufficiency as a technology-dependent threshold rather than a categorical or generational question. This reading (hybrid_degradation) is distinguished by locating the decay specifically at the tacit-knowledge/stress-response layer, on a generational timescale, coexisting with genuine procedural sufficiency — hence tangled_rope rather than snare or rope. Each reading is a distinct ε and distinct classification; none averages into the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
