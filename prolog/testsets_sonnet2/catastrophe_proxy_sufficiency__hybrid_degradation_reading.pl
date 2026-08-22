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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Simulation-Based Competence Maintenance With Generational Tacit-Knowledge Decay
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   High-reliability organizations (nuclear plants, aviation, grid operators,
 *   hospital critical care) rely on simulation to train and re-certify
 *   operators because real catastrophes are too costly and rare to train
 *   against directly. Over successive generations without a real catastrophic
 *   event, the workforce's procedural competence — measured, certified,
 *   renewed — remains intact and auditable. But this reading holds that a
 *   second, unmeasured competence dimension (tacit judgment under genuine
 *   uncertainty, physiological and cognitive stress-response calibrated by
 *   real stakes) decays because simulation, however well-designed, cannot
 *   replicate the irreducible uncertainty and consequence-weight of an actual
 *   catastrophic event. The gap is invisible under the current measurement
 *   regime because certification only tests the dimension simulation does
 *   preserve.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.58).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.47).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "Simulation-Based Competence Maintenance With Generational Tacit-Knowledge Decay").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency__hybrid_degradation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__hybrid_degradation_reading, '6fb3e6e6-92c8-47cc-81ed-532545549988').
narrative_ontology:cs_kernel_codification('6fb3e6e6-92c8-47cc-81ed-532545549988', distributed).
narrative_ontology:cs_authority_grounding('6fb3e6e6-92c8-47cc-81ed-532545549988', expertise).
narrative_ontology:cs_interpretation_layer_present('6fb3e6e6-92c8-47cc-81ed-532545549988').
narrative_ontology:cs_reading_relation('6fb3e6e6-92c8-47cc-81ed-532545549988', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, coexists_with).
narrative_ontology:cs_reading_relation('6fb3e6e6-92c8-47cc-81ed-532545549988', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('6fb3e6e6-92c8-47cc-81ed-532545549988', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('6fb3e6e6-92c8-47cc-81ed-532545549988', foundational, competence_is_structurally_bimodal).
narrative_ontology:cs_axiom_status(competence_is_structurally_bimodal, holdable).
narrative_ontology:cs_axiom_grounding('6fb3e6e6-92c8-47cc-81ed-532545549988', competence_is_structurally_bimodal, empirically_contingent).
narrative_ontology:cs_axiom('6fb3e6e6-92c8-47cc-81ed-532545549988', foundational, tacit_stress_calibration_requires_genuine_consequence_weight).
narrative_ontology:cs_axiom_status(tacit_stress_calibration_requires_genuine_consequence_weight, holdable).
narrative_ontology:cs_axiom_grounding('6fb3e6e6-92c8-47cc-81ed-532545549988', tacit_stress_calibration_requires_genuine_consequence_weight, empirically_contingent).
narrative_ontology:cs_reference_frame('6fb3e6e6-92c8-47cc-81ed-532545549988', procedural_certification_as_full_competence_proxy).
narrative_ontology:cs_drift_state('6fb3e6e6-92c8-47cc-81ed-532545549988', post_generational_catastrophe_absence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6fb3e6e6-92c8-47cc-81ed-532545549988', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_and_training_industry).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, regulatory_agencies_citing_compliance_metrics).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, future_frontline_operators).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, public_exposed_to_low_frequency_high_consequence_events).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__hybrid_degradation_reading, procedural_competence_is_measurable_via_simulation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, sells, and administers the simulation curricula and certification cycles that operators must complete to remain licensed. Revenue is recurring and tied to the simulation regime remaining the accepted sufficiency standard. Has no structural incentive to fund research showing simulation insufficiency for tacit/stress competence, since that would require either costly real-event exposure programs or an admission that its product is incomplete.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_and_training_industry, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_and_training_industry, agenda_setter).

% Mandates simulation-hours and certification renewal as the legally sufficient proxy for operational readiness. Benefits from a clean, auditable compliance metric that discharges liability and satisfies political demand for 'action' after any incident, without having to justify the harder, costlier question of whether tacit competence is actually being preserved across a generation with no real catastrophe.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, regulatory_agencies_citing_compliance_metrics, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__hybrid_degradation_reading, regulatory_agencies_citing_compliance_metrics, beneficiary).

% Enter the profession after the last real catastrophe has receded beyond living memory within the workforce. Pass every simulation-based certification cycle yet inherit a stress-response and tacit-judgment gap they cannot detect from inside the system, since the certification signal reads as competent. Cannot exit the profession's training regime without leaving the profession entirely; cannot individually generate the missing tacit knowledge, since it depends on organizational memory and real-event exposure they do not control.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, future_frontline_operators, payer,
    moderate, biographical, constrained, national).

% Lives downstream of the facilities, aircraft, grids, or systems the operators run. Bears the tail-risk cost if degraded stress-response capacity manifests during a genuine crisis, but has no visibility into simulation fidelity, no seat in certification design, and no way to independently verify that the compliance metric tracks real competence rather than procedural memorization.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, public_exposed_to_low_frequency_high_consequence_events, payer,
    powerless, generational, trapped, regional).

% Carry tacit knowledge and stress-tested judgment from the last real catastrophe or near-miss but have limited formal channels to transmit it, since the certification system values documented procedural compliance over informal mentorship time. Their professional identity is bound to a competence standard the current system does not fully capture or reward, and their warnings about the gap are often read as institutional nostalgia rather than signal.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, veteran_operators_nearing_retirement, excluded,
    moderate, biographical, identity_locked, national).

% Study high-reliability organization theory and post-incident reviews across industries to assess whether simulation regimes preserve genuine readiness or merely procedural fluency. Can name the decay mechanism but has no enforcement power over certification standards or regulatory mandates.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, organizational_safety_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_and_training_industry).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__hybrid_degradation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Simulation training genuinely solves the coordination problem of teaching procedures, checklists, and standard-response sequences at scale, safely, repeatably, and without waiting for or manufacturing a real catastrophe to train against.
% TRANSFER_FUNCTION: Moves certification revenue and compliance-discharge value from operators and the public to the training/certification industry and the regulators who cite it, while moving safety margin risk from the present (visible, auditable) to the future (invisible until a real event exposes the gap).
% ABSENT_VOICES: Veteran operators with real-catastrophe experience have limited formal transmission channels; future operators who will face the degraded stress-response gap are not yet in a position to know what they don't know; the public bearing tail risk has no technical seat in certification design at all.
% DISAPPEARANCE_RATIONALE: If simulation-based certification vanished overnight, the certification industry and much of the regulatory compliance architecture would collapse immediately (world_rearranges for those seats). But whether frontline operational safety itself would meaningfully change in the short term is contested: the procedural competence genuinely transferred by simulation would persist for a generation before its absence bit, and the tacit/stress-response gap this reading identifies would only become visible after that same generational lag — so the disappearance test itself is confounded by the same slow-clock mechanism the constraint describes.
% FOUNDING_PROBLEM: Real catastrophes are too costly, too dangerous, and too rare to serve as the primary training mechanism for procedural competence; simulation was built to teach and re-certify standard responses without manufacturing real harm.
% FOUNDING_PROBLEM_CORROBORATION: The certification industry and regulators attest the founding problem is fully solved by current simulation fidelity. Independent high-reliability-organization researchers and veteran operators from outside the certification-industry beneficiary set attest that the founding problem has only been partially solved — procedural competence is preserved but tacit judgment and stress-response capacity, which the founding problem also implicitly required, is not verified by any current instrument and is hypothesized to decay across a generation without real-event exposure.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__hybrid_degradation_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__hybrid_degradation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction is authored as moderate-rising (0.30 to 0.58) because the coordination function (procedural training) is genuinely real at every time point, but the beneficiary side (certification industry, compliance-citing regulators) accrues steady rent from a sufficiency claim that becomes increasingly divorced from the underlying reality as the generational gap since the last real catastrophe widens. Theater ratio rises faster (0.25 to 0.62) because as the gap widens, an increasing share of certification activity becomes performative — renewing documentation and hours-logged rather than validating the actual competence dimension at risk. Suppression is authored as moderate and only slowly rising (0.35 to 0.47): there is no strong coercive apparatus forcing operators to stay silent, but there is a structural suppression of the question itself, since no current instrument can even measure the tacit/stress-response gap, which makes organized resistance to the sufficiency claim difficult to mount on evidentiary grounds.
 *
 * PERSPECTIVAL GAP:
 *   From the certification industry and regulatory seat, the arrangement reads as successful, ongoing coordination — competence is measured, certified, and improving on paper. From the future-operator and public seat, the same arrangement is a slow-accumulating extraction: present institutional confidence is purchased by deferring an unmeasured risk onto a future crisis. The engine should compute these as structurally different seat classifications from the same base data — that divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The certification and training industry sits nearest the beneficiary end: it collects recurring revenue tied to the simulation regime remaining the accepted standard, and has no exit cost from the arrangement continuing. Regulatory agencies sit close behind: they benefit from a clean compliance metric that discharges institutional liability. Future frontline operators are the clearest target: they are certified competent by a metric that does not test the dimension this reading holds is decaying, and they cannot individually detect or correct the gap. The public is the most powerless payer: fully trapped, bearing tail risk with zero visibility into simulation fidelity or certification design. Veteran operators are excluded rather than positioned as victims or beneficiaries — they hold the missing knowledge but lack a transmission channel the certification system rewards.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (train operators without manufacturing real catastrophic harm) has genuinely been solved for procedural competence — that mandate is live and functioning. But if this reading is correct, a second, unstated mandate (preserve tacit judgment and stress-calibration) was never actually discharged by simulation and has been silently substituted with a proxy (certification hours) that measures a different thing. This is precisely the mandatrophy-prevention case: classifying the arrangement as tangled_rope rather than pure rope or pure snare prevents both over-crediting simulation (mislabeling extraction as pure coordination) and dismissing it entirely (mislabeling genuine procedural-training coordination as pure extraction). The coordination function is real and should not be discarded; the hidden decay mechanism is also real and should not be waved away as inevitable overhead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tacit_decay_measurability,
    'Is the hypothesized decay of tacit knowledge and stress-response capacity over generational timescales an empirically detectable phenomenon, or is it currently unfalsifiable because no instrument exists to measure it separately from procedural competence?',
    'Longitudinal studies comparing operator performance during genuine unplanned emergencies (not drills) across cohorts with varying generational distance from the last real catastrophe, controlling for simulation hours and certification currency; physiological stress-response studies comparing simulation exposure to real-incident exposure.',
    'If measurable and confirmed, this reading''s tangled_rope classification is strongly supported and the certification regime should be treated as actively concealing a growing safety deficit. If the decay proves undetectable or negligible, this reading collapses toward simulation_as_proxy_catastrophe_reading and the tangled_rope classification would be unwarranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_decay_measurability, empirical, 'Whether generational tacit/stress-response decay is empirically real and measurable, or an unfalsifiable hypothesis.').

omega_variable(
    kernel_reading_selection_basis,
    'Among the four sibling readings of the catastrophe_proxy_sufficiency kernel, what distinguishes the hybrid_degradation_reading (partial sufficiency with hidden decay) from simulation_fidelity_threshold (partial sufficiency contingent on engineerable fidelity)?',
    'Determine whether the gap this reading identifies is closable by improving simulation technology (favoring simulation_fidelity_threshold) or is structurally irreducible because it depends on genuine consequence-weight and uncertainty that no simulation, however fidelity-matched, can replicate without actually being a real catastrophe (favoring hybrid_degradation_reading).',
    'If the gap is closable by fidelity improvements, resources should flow to simulation R&D and the certification industry''s beneficiary status is more defensible as a genuine, improvable coordination function. If the gap is structurally irreducible regardless of fidelity, no amount of simulation investment closes it and the tangled_rope''s extractive component is a permanent structural feature, not a temporary technology lag.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether the identified competence gap is a fidelity-engineering problem or a structurally irreducible feature of simulation-as-such.').

omega_variable(
    certification_industry_capture_extent,
    'To what extent has the certification and training industry actively shaped the definition of ''sufficient competence'' to favor measurable procedural criteria over the harder-to-certify tacit/stress dimension, versus simply operating within a pre-existing regulatory framework it did not design?',
    'Historical analysis of how certification standards were set, including industry lobbying records, regulatory rulemaking comment history, and comparison of certification criteria evolution against independent safety-science recommendations over the same period.',
    'If the industry actively shaped standards to favor measurable proxies, the beneficiary classification is stronger and closer to regulatory capture; if the industry inherited rather than shaped the framework, its beneficiary status is more incidental and the extraction is better attributed to the framework''s original design limitations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(certification_industry_capture_extent, empirical, 'Whether certification industry benefit reflects active standard-shaping or incidental inheritance of a pre-existing measurement framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cata_tr_t8, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement(cata_tr_t16, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 16, 0.43).
narrative_ontology:measurement(cata_tr_t24, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 24, 0.51).
narrative_ontology:measurement(cata_tr_t32, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 32, 0.57).
narrative_ontology:measurement(cata_tr_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cata_be_t8, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(cata_be_t16, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(cata_be_t24, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement(cata_be_t32, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(cata_be_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cata_su_t8, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 8, 0.37).
narrative_ontology:measurement(cata_su_t16, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 16, 0.4).
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
% This story is one of four sibling readings of the catastrophe_proxy_sufficiency kernel, each instantiating a structurally distinct claim about whether and how simulation substitutes for real catastrophic exposure in maintaining operational competence. simulation_as_proxy_catastrophe_reading claims full sufficiency (near-zero extraction, closer to rope); catastrophe_necessity_reading claims categorical insufficiency (simulation as pure theater, closer to snare or piton); simulation_fidelity_threshold claims contingent, technology-dependent sufficiency (closer to scaffold, since fidelity thresholds could in principle sunset the gap); this hybrid_degradation_reading claims structural, generationally-accumulating partial insufficiency that is not resolved by fidelity improvements alone (tangled_rope). Each reading authors its own ε against the same underlying practice — the difference in ε across the four stories IS the kernel contest, not a modeling inconsistency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
