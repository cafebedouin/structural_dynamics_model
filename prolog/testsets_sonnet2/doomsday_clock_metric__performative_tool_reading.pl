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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Doomsday Clock as Strategic Mobilization Instrument (Performative Tool Reading)
 *   domain: science communication/normative epistemology/risk governance
 *
 * SUMMARY:
 *   This story instantiates the performative-tool reading of the Doomsday
 *   Clock kernel: the annual minute-hand setting is authored here as a
 *   communications instrument whose value is chosen partly for maximum policy
 *   impact and public mobilization, not solely as a calibrated synthesis of
 *   risk indicators. Under this reading the Bulletin's board functions as a
 *   strategic communicator with legitimate coordination value (concentrating
 *   diffuse expert concern into an actionable public signal) but also as an
 *   agenda-setter who benefits institutionally from dramatic settings, while
 *   the metric's own credibility as an objective index is what gets spent to
 *   buy that mobilization. This is a distinct constraint from the
 *   objective_index_reading (which would author near-zero extraction,
 *   treating the setting as a faithful expert synthesis) and the
 *   hybrid_legitimacy_reading (which would treat the entanglement of fact and
 *   value as irreducible rather than as a tolerated strategic choice). Each
 *   reading has its own ε and its own file; they are linked only through the
 *   shared kernel_id in commentary, per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - bulletin_science_security_board: agenda-setter and beneficiary — sets the number, gains convening power and visibility
 *   - policy_advocacy_coalitions: beneficiary — uses dramatic settings as mobilization fuel
 *   - science_communication_field: payer — inherits reputational skepticism when the pattern becomes visible
 *   - future_public_epistemic_trust: non-agent payer — diffuse credibility capital drawn down over time
 *   - general_public_audience: excluded — receives the number without visibility into strategic weighting
 *   - rival_risk_assessment_bodies: excluded — produce more transparent but less visible alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, 0.62).
domain_priors:suppression_score(doomsday_clock_metric__performative_tool_reading, 0.28).
domain_priors:theater_ratio(doomsday_clock_metric__performative_tool_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__performative_tool_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__performative_tool_reading, "Doomsday Clock as Strategic Mobilization Instrument (Performative Tool Reading)").
narrative_ontology:topic_domain(doomsday_clock_metric__performative_tool_reading, "science communication/normative epistemology/risk governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__performative_tool_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__performative_tool_reading, '5480658c-ba05-41c8-b7bc-81c3cb6325c7').
narrative_ontology:cs_kernel_codification('5480658c-ba05-41c8-b7bc-81c3cb6325c7', distributed).
narrative_ontology:cs_authority_grounding('5480658c-ba05-41c8-b7bc-81c3cb6325c7', expertise).
narrative_ontology:cs_interpretation_layer_present('5480658c-ba05-41c8-b7bc-81c3cb6325c7').
narrative_ontology:cs_reading_relation('5480658c-ba05-41c8-b7bc-81c3cb6325c7', doomsday_clock_metric__objective_index_reading, coexists_with).
narrative_ontology:cs_reading_relation('5480658c-ba05-41c8-b7bc-81c3cb6325c7', doomsday_clock_metric__hybrid_legitimacy_reading, influences).
narrative_ontology:cs_axiom('5480658c-ba05-41c8-b7bc-81c3cb6325c7', foundational, strategic_impact_weighting_is_contingent_and_costly).
narrative_ontology:cs_axiom_status(strategic_impact_weighting_is_contingent_and_costly, holdable).
narrative_ontology:cs_axiom_grounding('5480658c-ba05-41c8-b7bc-81c3cb6325c7', strategic_impact_weighting_is_contingent_and_costly, empirically_contingent).
narrative_ontology:cs_axiom('5480658c-ba05-41c8-b7bc-81c3cb6325c7', secondary, metric_credibility_is_a_depletable_shared_resource).
narrative_ontology:cs_axiom_status(metric_credibility_is_a_depletable_shared_resource, holdable).
narrative_ontology:cs_axiom_grounding('5480658c-ba05-41c8-b7bc-81c3cb6325c7', metric_credibility_is_a_depletable_shared_resource, instrumental).
narrative_ontology:cs_reference_frame('5480658c-ba05-41c8-b7bc-81c3cb6325c7', cold_war_urgency_signal).
narrative_ontology:cs_drift_state('5480658c-ba05-41c8-b7bc-81c3cb6325c7', contemporary_multipolar_risk_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5480658c-ba05-41c8-b7bc-81c3cb6325c7', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, bulletin_science_security_board).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, policy_advocacy_coalitions).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, future_public_epistemic_trust).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, science_communication_field).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the minute-hand position each January in closed deliberation, explicitly weighing what setting will generate the most media coverage and policy pressure alongside the underlying risk indicators. Controls the sole channel through which the metric is produced and can adjust methodology or emphasis year to year without external audit. Gains institutional visibility, funding relevance, and continued convening power from the clock's persistence as a headline event.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, bulletin_science_security_board, agenda_setter,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__performative_tool_reading, bulletin_science_security_board, beneficiary).

% Use the clock setting as a ready-made mobilization hook for arms-control, climate, and biosecurity campaigns. Benefit whenever the hand moves toward midnight because it supplies a dramatic, pre-packaged narrative for fundraising and legislative pressure, regardless of whether the setting change reflects a genuinely tractable shift in risk.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, policy_advocacy_coalitions, beneficiary,
    organized, biographical, mobile, global).

% Inherits the reputational cost when journalists, skeptics, or rival experts note that the clock has never moved backward proportionally to genuinely reduced risks and ask whether the number is signal or theater. Science communicators outside the Bulletin must work against accumulated public skepticism about expert risk metrics generally, a cost they did not choose and cannot easily exit since the clock is treated as emblematic of the genre.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, science_communication_field, payer,
    moderate, generational, constrained, global).

% Represents the diffuse, not-yet-realized capacity of future publics to trust expert-synthesized risk indicators. Each strategically timed setting that trades calibration accuracy for media impact draws down this capacity; it cannot object now and has no seat at the annual announcement, only the eventual erosion of credibility when the pattern becomes legible.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, future_public_epistemic_trust, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(doomsday_clock_metric__performative_tool_reading, future_public_epistemic_trust).

% Receives the annual announcement as an authoritative-seeming number without visibility into the board's internal weighting of communicative impact versus measured indicators. Has no channel to question the methodology and generally treats the setting as a scientific reading rather than a strategic communication choice.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, general_public_audience, excluded,
    powerless, biographical, trapped, global).

% Produce competing existential-risk indices (e.g. probabilistic forecasting consortia) that use disclosed methodologies but receive far less media attention than the clock's single dramatic number. Would prefer the discourse shift toward transparent probabilistic estimates but lack the clock's decades of brand recognition and cannot dislodge it as the public's default reference point.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, rival_risk_assessment_bodies, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__performative_tool_reading, bulletin_science_security_board).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__performative_tool_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, memorable, recurring public signal that concentrates diffuse expert concern about existential risk into a moment advocacy groups, journalists, and policymakers can rally around annually.
% TRANSFER_FUNCTION: Moves attention, funding, and legislative urgency toward causes the announcement highlights in a given year, at the cost of the metric's own claim to be a calibrated instrument — that credibility capital is drawn down and transferred into short-term mobilization value.
% ABSENT_VOICES: Rival probabilistic risk-forecasting bodies and methodologists who would argue for transparent, auditable indices are not part of the announcement process; the general public receives the number without visibility into the board's communicative-impact weighting.
% DISAPPEARANCE_RATIONALE: The Bulletin and allied advocacy coalitions would say the world rearranges sharply — a decades-old mobilization anchor vanishes and existential-risk causes lose their most recognizable rallying device. Rival forecasting bodies and skeptical science communicators would say the world is largely unchanged or even improved, since attention would redistribute to disclosed-methodology alternatives; the verdict depends on which seat is asked, which is itself part of what this reading claims about the metric.
% FOUNDING_PROBLEM: In 1947, scientists who had built the atomic bomb needed a way to communicate a felt, urgent, hard-to-quantify sense of civilizational danger to a public and political class without technical training in nuclear physics.
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin's own historians attest the founding problem — communicating urgency across an expertise gap — remains live. Independent risk-forecasting researchers and several science journalists outside the Bulletin's board attest that the mechanism has drifted from urgency-communication toward annual media-cycle production, and that the setting's correlation with disclosed indicator changes has weakened over recent cycles; this corroboration comes from parties who do not benefit from the clock's continued prominence.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__performative_tool_reading, contested).
narrative_ontology:founding_problem_status(doomsday_clock_metric__performative_tool_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__performative_tool_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(doomsday_clock_metric__performative_tool_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__performative_tool_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (ε = 0.62) is authored as substantial but not extreme: the clock retains genuine coordination value (mobilizing attention that would otherwise stay diffuse) even as a growing share of its output is optimized for narrative impact over calibration fidelity. Theater ratio (0.71) is authored high and rising, since the performative-tool reading specifically claims the annual production has increasingly substituted media-cycle salience for auditable indicator tracking — this is the central Goodhart-drift claim of the reading. Suppression is authored low (0.28) and only mildly rising: no one is coerced into believing the setting, and open critique exists, but the board's sole control of the production process and absence of external audit constitutes a soft suppressive floor. Accessibility collapse (0.35) is authored low-moderate: alternative risk indices exist and are known to specialists, so alternatives have not collapsed, only failed to achieve comparable salience. Resistance (0.55) reflects real, growing methodological pushback from forecasting researchers and science journalists.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bulletin board sits closest to full beneficiary: it authors the metric, controls its production, and gains institutional standing from dramatic settings — arbitrage-grade exit from any external accountability mechanism. Policy advocacy coalitions are secondary beneficiaries who did not create the tool but capture its mobilization value. Science communication field and future epistemic trust are targets: they bear the diffuse, delayed cost of credibility erosion without having chosen the strategic framing. The non-agent stakeholder (future_public_epistemic_trust) is included for completeness but excluded from beneficiary/victim derivation per the agent:false convention — it names a real cost-bearing capacity without treating an abstraction as a collecting actor.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is deliberately chosen over snare because a genuine coordination function persists — the clock does concentrate diffuse expert concern into actionable public attention, which is a real service to a genuine collective-action problem (undercommunicated existential risk). What makes it tangled rather than pure rope is the asymmetric extraction layered onto that coordination: the board's own strategic-impact weighting draws down a shared epistemic resource (the credibility of expert risk synthesis generally) that it does not bear the full cost of losing, while the field-wide reputational cost lands on communicators and future publics who never consented to the trade. Classifying this as pure rope would erase the real credibility cost; classifying it as pure snare would erase the real mobilization value the tool has provided historically — the tangled_rope label holds both facts open at once.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strategic_weighting_detectability,
    'Can the degree of strategic-impact weighting in any given year''s setting be distinguished, after the fact, from a genuine reassessment of risk indicators?',
    'Retrospective comparison of the board''s internal deliberation records (where available) against the disclosed indicator changes cited in the announcement; convergence with independent forecasting indices over the same period would support the objective_index_reading instead.',
    'If strategic weighting is empirically undetectable and the setting tracks independent indices closely, this reading''s high ε is unsupported and the constraint collapses toward objective_index_reading; if a consistent gap is found, this reading''s extraction claim is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_weighting_detectability, empirical, 'Whether strategic-impact weighting is separable from genuine risk reassessment in the historical record.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the disagreement between the three kernel readings live — is it in what the board actually does (an empirical question about their deliberation process), or in what any risk-communication metric of this kind necessarily is (a conceptual question about whether fact/value separation is even possible in existential-risk synthesis)?',
    'This is the committer-structure question routed here per Rule 2: it cannot be resolved by this story alone since it concerns the relationship between readings, not this reading''s internal facts. Resolution would require comparing this story''s authored ε and beneficiary structure against the hybrid_legitimacy_reading and objective_index_reading files and asking which best fits the disclosed historical process.',
    'If the hybrid_legitimacy_reading''s framing is correct (entanglement is irreducible), then this reading''s claim that strategic manipulation is a separable, contingent choice — rather than a structural feature of any such metric — is itself contestable, which would not change this story''s ε but would change how much weight the corpus should give to this reading versus its siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Locates the kernel-level disagreement between the three sibling readings rather than resolving it within this story.').

omega_variable(
    credibility_erosion_causal_attribution,
    'Is the science-communication field''s reputational cost genuinely caused by the Bulletin''s strategic weighting, or is it caused by broader public science skepticism for which the clock is merely a visible proxy?',
    'Survey research isolating public trust in the Bulletin''s clock specifically versus trust in expert risk communication generally, tracked over the same interval as the measurement series.',
    'If the erosion is substantially attributable to broader trends rather than the clock''s own strategic choices, the victim declaration (science_communication_field) overstates this constraint''s causal contribution and the extraction estimate should be revised downward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credibility_erosion_causal_attribution, empirical, 'Whether the field-wide credibility cost is caused by this constraint specifically or by external science-trust trends.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__performative_tool_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t0, doomsday_clock_metric__performative_tool_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(doom_tr_t8, doomsday_clock_metric__performative_tool_reading, theater_ratio, 8, 0.45).
narrative_ontology:measurement(doom_tr_t16, doomsday_clock_metric__performative_tool_reading, theater_ratio, 16, 0.53).
narrative_ontology:measurement(doom_tr_t24, doomsday_clock_metric__performative_tool_reading, theater_ratio, 24, 0.61).
narrative_ontology:measurement(doom_tr_t32, doomsday_clock_metric__performative_tool_reading, theater_ratio, 32, 0.67).
narrative_ontology:measurement(doom_tr_t40, doomsday_clock_metric__performative_tool_reading, theater_ratio, 40, 0.71).

% Extraction over time
narrative_ontology:measurement(doom_be_t0, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(doom_be_t8, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(doom_be_t16, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement(doom_be_t24, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(doom_be_t32, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(doom_be_t40, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t0, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(doom_su_t8, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 8, 0.18).
narrative_ontology:measurement(doom_su_t16, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 16, 0.2).
narrative_ontology:measurement(doom_su_t24, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 24, 0.23).
narrative_ontology:measurement(doom_su_t32, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 32, 0.26).
narrative_ontology:measurement(doom_su_t40, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 40, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__performative_tool_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(doomsday_clock_metric__performative_tool_reading, 0.1).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric__objective_index_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric__hybrid_legitimacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the doomsday_clock_metric kernel. objective_index_reading authors the setting as faithful expert synthesis (near-mountain ε); hybrid_legitimacy_reading authors the fact/value entanglement as irreducible rather than as a contingent strategic choice; this performative_tool_reading authors the setting as a communications instrument whose strategic-impact weighting is a real, costly, contingent choice that trades calibration credibility for mobilization value. Each carries its own ε, beneficiary/victim structure, and classification; they are linked here rather than merged into one observer-relative story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
