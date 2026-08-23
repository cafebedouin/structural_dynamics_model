% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__performative_tool_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__performative_tool_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: doomsday_clock_metric__performative_tool_reading
 *   human_readable: Doomsday Clock as Performative Policy Tool
 *   domain: science communication / normative epistemology / risk governance
 *
 * SUMMARY:
 *   The Bulletin of the Atomic Scientists' Doomsday Clock is read here as a
 *   performative tool whose setting is strategically chosen to maximize
 *   policy impact and mobilize collective action. This reading treats the
 *   clock not as an objective index of existential risk nor as an irreducibly
 *   entangled hybrid, but as an instrument whose primary function is
 *   political. The beneficiary is the policy activism network; the victim is
 *   epistemic credibility â the trust that scientific risk assessments are
 *   driven by evidence rather than theatrical timing. This constraint story
 *   instantiates the performative_tool_reading of the doomsday_clock_metric
 *   kernel.
 *
 * KEY AGENTS:
 *   - bulletin_organization: Agenda-setter (institutional/arbitrage) â administers the metric and controls its symbolic dissemination.
 *   - policy_activists: Primary beneficiary (organized/mobile) â receives ready-made mobilization resource.
 *   - risk_assessment_scientists: Primary payer (organized/constrained) â bears cost to scientific credibility.
 *   - policy_publics: Secondary payer (powerless/constrained) â bears cost of distorted risk perception and depleted trust.
 *   - competing_risk_communicators: Excluded (moderate/constrained) â crowded out of discourse.
 *   - science_communication_scholars: Analytical observer (analytical/analytical) â documents epistemic corrosion.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, 0.72).
domain_priors:suppression_score(doomsday_clock_metric__performative_tool_reading, 0.55).
domain_priors:theater_ratio(doomsday_clock_metric__performative_tool_reading, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, theater_ratio, 0.75).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__performative_tool_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__performative_tool_reading, "Doomsday Clock as Performative Policy Tool").
narrative_ontology:topic_domain(doomsday_clock_metric__performative_tool_reading, "science communication / normative epistemology / risk governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__performative_tool_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__performative_tool_reading, '0cace6f0-8537-4e29-851f-f226633a2ce8').
narrative_ontology:cs_kernel_codification('0cace6f0-8537-4e29-851f-f226633a2ce8', formalized).
narrative_ontology:cs_authority_grounding('0cace6f0-8537-4e29-851f-f226633a2ce8', extraction).
narrative_ontology:cs_interpretation_layer_present('0cace6f0-8537-4e29-851f-f226633a2ce8').
narrative_ontology:cs_reading_relation('0cace6f0-8537-4e29-851f-f226633a2ce8', doomsday_clock_metric__objective_index_reading, forecloses).
narrative_ontology:cs_reading_relation('0cace6f0-8537-4e29-851f-f226633a2ce8', doomsday_clock_metric__hybrid_legitimacy_reading, coexists_with).
narrative_ontology:cs_axiom('0cace6f0-8537-4e29-851f-f226633a2ce8', foundational, metric_manipulation_for_policy_impact_legitimate).
narrative_ontology:cs_axiom_status(metric_manipulation_for_policy_impact_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('0cace6f0-8537-4e29-851f-f226633a2ce8', metric_manipulation_for_policy_impact_legitimate, instrumental).
narrative_ontology:cs_axiom('0cace6f0-8537-4e29-851f-f226633a2ce8', foundational, epistemic_credibility_subordinate_to_existential_action).
narrative_ontology:cs_axiom_status(epistemic_credibility_subordinate_to_existential_action, holdable).
narrative_ontology:cs_axiom_grounding('0cace6f0-8537-4e29-851f-f226633a2ce8', epistemic_credibility_subordinate_to_existential_action, instrumental).
narrative_ontology:cs_reference_frame('0cace6f0-8537-4e29-851f-f226633a2ce8', strategic_policy_mobilization_tool).
narrative_ontology:cs_drift_state('0cace6f0-8537-4e29-851f-f226633a2ce8', contemporary_media_ecosystem, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0cace6f0-8537-4e29-851f-f226633a2ce8', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, policy_activists).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, risk_assessment_scientists).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, policy_publics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the Doomsday Clock through its Science and Security Board, controls the symbolic announcement, and disseminates it globally. Derives institutional relevance, media attention, and fundraising capacity from the clock's continued symbolic power. Could reform or retire the metric but instead sets it strategically to maximize policy impact and media reach.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, bulletin_organization, agenda_setter,
    institutional, generational, arbitrage, global).

% Use the clock's announcements as a dramatic, media-ready symbolic resource to mobilize support for nuclear disarmament, climate action, and biosecurity governance. Receive a globally recognized countdown metaphor that condenses complex policy demands into an intuitive, headline-friendly marker without requiring granular technical argumentation.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, policy_activists, beneficiary,
    organized, biographical, mobile, global).

% Their collective credibility and disciplinary authority are leveraged to legitimize the clock setting even when its relationship to empirical indicators is strained. Face social and professional pressure to participate in or remain silent about the Bulletin's announcements. Bear the long-term cost when publics discover the metric was strategically manipulated, degrading trust in scientific risk assessment broadly.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, risk_assessment_scientists, payer,
    organized, civilizational, constrained, global).

% Receive oversimplified, dramatized risk signals that may distort understanding of relative threat priorities and timeline urgency. Their trust in scientific risk communication is consumed by the clock's performative swings. They have no institutional voice in setting the metric and limited ability to evaluate its empirical basis against more nuanced alternatives.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, policy_publics, payer,
    powerless, biographical, constrained, global).

% Develop probabilistic, domain-specific, or quantitatively rigorous risk assessments but are crowded out of media attention and policy discourse by the clock's singular, dramatic symbolic power. Would advocate for richer, actionable risk communication if given access to the Bulletin-level media platform.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, competing_risk_communicators, excluded,
    moderate, biographical, constrained, global).

% Study the tension between epistemic accuracy and persuasive impact in risk communication. Observe the strategic manipulation of the metric and document its corrosive effects on public trust in science, without themselves setting or being bound by the clock.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, science_communication_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__performative_tool_reading, diffuse).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__performative_tool_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mobilizes collective action and policy attention around existential risks by providing a simple, dramatic, globally recognizable symbolic marker that condenses complex threat assessments into an intuitive countdown metaphor.
% TRANSFER_FUNCTION: Moves epistemic authority and public attention from the scientific community's nuanced risk assessments to policy activists and the Bulletin, converting credibility into political mobilization.
% ABSENT_VOICES: Competing risk metric developers, probabilistic forecasters, and domain-specific experts who would advocate for granular, actionable risk indicators are structurally excluded from the Bulletinâs setting process and crowded out of the media cycle the clock dominates.
% DISAPPEARANCE_RATIONALE: If the clock and its performative use vanished, the Bulletin would lose its primary media leverage, policy activists would need to build mobilization on substantive argumentation rather than symbolic drama, and alternative risk communication frameworks would gain discursive space â the global risk governance conversation would reorganize around more nuanced metrics.
% FOUNDING_PROBLEM: Post-World War II need to alert policymakers and publics to the novel, civilization-scale danger of nuclear weapons in a form comprehensible to non-experts and resistant to normalization.
% FOUNDING_PROBLEM_CORROBORATION: Historians of science and independent nuclear-security scholars attest the founding problem was real in 1947. Contemporary security analysts and science communication researchers outside the Bulletin attest the nuclear landscape is now managed through complex arms-control frameworks, proliferation treaties, and deterrence dynamics that a single minute-hand cannot represent; the clock persists as a mobilization device rather than a warning system.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__performative_tool_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__performative_tool_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__performative_tool_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(doomsday_clock_metric__performative_tool_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__performative_tool_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__performative_tool_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(doomsday_clock_metric__performative_tool_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the constraint systematically converts scientific credibility into activist mobilization with diminishing fidelity to empirical indicators. Suppression is moderate (0.55) because alternative risk framings are not banned but are crowded out by the Bulletin's media monopoly on the symbolic announcement moment. Theater ratio is high (0.75) because the clock's entire function is symbolic performance â the minute-hand is a stage prop for policy theater. Accessibility collapse is moderate (0.45) because probabilistic alternatives exist but cannot break through the noise floor of the annual announcement. Resistance is moderate (0.40) because a visible segment of scientists and science communicators actively criticizes the metric's manipulation, though they lack comparable media access. The founding problem â alerting a naive postwar public to nuclear danger â is dead, but the arrangement persists because new beneficiaries (policy activists and the Bulletin itself) capture the extraction, preventing piton decay.
 *
 * PERSPECTIVAL GAP:
 *   The Bulletin and policy activist seats experience the constraint as a necessary tool for cutting through policy inertia; the risk-assessment scientist seat experiences it as a parasitic draw on disciplinary credibility; the public seat experiences it as alarming but increasingly illegible theater. The engine computes this divergence from the structural asymmetry in exit options and beneficiary-victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bulletin and policy activists sit near the beneficiary pole (low d): they collect attention and mobilization. Risk assessment scientists and policy publics sit near the target pole (high d): they pay in depleted credibility and distorted risk perception. The scientists' d is slightly lower than the public's because they retain limited professional exit options (they can dissent publicly), whereas the public is more fully exposed and has no comparable voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â alerting a naive postwar public to nuclear danger â is dead. The arrangement has not become a piton because concentrated beneficiaries (policy activists) and an active agenda-setter (the Bulletin) continue to profit from its operation. A dead founding problem with live beneficiaries and active enforcement is the signature of a tangled rope that has migrated from its original coordination function into sustained asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_foreclosure_validity,
    'Does the performative_tool_reading''s logical foreclosure of the objective_index_reading hold, or can the two be reconciled as layers of a single institutional process?',
    'Ethnographic or documentary analysis of Bulletin deliberations to determine whether strategic impact considerations operate as an explicit override, an implicit weighting, or a post-hoc rationalization of empirically driven judgment.',
    'If reconcilable, the performative reading overstates the extraction and the constraint may recompute as hybrid legitimacy; if strictly contradictory, foreclosure stands and the objective reading is a cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_validity, conceptual, 'Whether performative and objective readings are strictly contradictory or layered.').

omega_variable(
    credibility_erosion_rate,
    'At what rate does strategic manipulation of the clock degrade the long-term credibility of scientific risk communication, and is this degradation reversible?',
    'Longitudinal public-trust surveys tracking correlation between clock-announcement exposure and trust in scientific risk institutions, combined with natural-experiment designs in jurisdictions with varying media coverage of the clock.',
    'If degradation is steep and irreversible, the extraction term dominates the coordination term and the constraint may drift toward snare classification over time; if shallow or reversible, the tangled-rope balance persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credibility_erosion_rate, empirical, 'Rate and reversibility of epistemic credibility loss from metric manipulation.').

omega_variable(
    coordination_extraction_balance,
    'Does the clock''s mobilization function produce policy outcomes that justify the credibility cost, or has the extraction become the dominant term?',
    'Policy-outcome tracing comparing legislative or behavioral changes attributable to clock announcements against the counterfactual of alternative risk communication strategies.',
    'If outcomes justify costs, the coordination function remains structurally significant; if not, the constraint is extraction-dominant and the tangled-rope classification tilts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_balance, empirical, 'Whether mobilization outcomes offset epistemic credibility losses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__performative_tool_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t0, doomsday_clock_metric__performative_tool_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(doom_tr_t5, doomsday_clock_metric__performative_tool_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement(doom_tr_t10, doomsday_clock_metric__performative_tool_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(doom_tr_t15, doomsday_clock_metric__performative_tool_reading, theater_ratio, 15, 0.55).
narrative_ontology:measurement(doom_tr_t20, doomsday_clock_metric__performative_tool_reading, theater_ratio, 20, 0.65).
narrative_ontology:measurement(doom_tr_t25, doomsday_clock_metric__performative_tool_reading, theater_ratio, 25, 0.71).
narrative_ontology:measurement(doom_tr_t30, doomsday_clock_metric__performative_tool_reading, theater_ratio, 30, 0.75).

% Extraction over time
narrative_ontology:measurement(doom_be_t0, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(doom_be_t5, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(doom_be_t10, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(doom_be_t15, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(doom_be_t20, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(doom_be_t25, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(doom_be_t30, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t0, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(doom_su_t5, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(doom_su_t10, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(doom_su_t15, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 15, 0.46).
narrative_ontology:measurement(doom_su_t20, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(doom_su_t25, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 25, 0.53).
narrative_ontology:measurement(doom_su_t30, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__performative_tool_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
