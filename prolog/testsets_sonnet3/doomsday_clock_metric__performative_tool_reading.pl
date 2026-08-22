% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__performative_tool_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Doomsday Clock as Strategic Mobilization Instrument
 *   domain: science communication / normative epistemology / risk governance
 *
 * SUMMARY:
 *   This story instantiates the performative-tool reading of the Doomsday
 *   Clock kernel: the annual minute-hand setting is understood as a
 *   strategically chosen communication device, selected by the Bulletin's
 *   board with explicit attention to what setting will maximize policy
 *   salience and mobilize collective action, rather than as a strictly
 *   indicator-derived measurement. On this reading, the clock functions as a
 *   genuine coordination device for attention and legislative window-opening,
 *   but that function is bought at the cost of the public's ability to treat
 *   the number as calibrated — a cost that compounds each time the setting
 *   appears to track news cycles more closely than risk trajectories. Two
 *   sibling readings of the same kernel exist as separate constraints: the
 *   objective_index_reading treats the setting as expert synthesis of
 *   measurable indicators (much lower ε, closer to a rope or
 *   mountain-adjacent claim), and the hybrid_legitimacy_reading treats the
 *   entanglement of scientific judgment and normative stakes as irreducible
 *   rather than as strategic manipulation. This story's ε (0.62) is authored
 *   strictly for the performative-tool reading's own view of the standing
 *   arrangement — the clock as currently set and used — not for any reformed,
 *   purely-indicator-based alternative this reading might endorse.
 *
 * KEY AGENTS:
 *   - bulletin_of_atomic_scientists_leadership: agenda_setter (institutional/arbitrage) — sets the clock and administers the closed deliberation
 *   - allied_advocacy_organizations: beneficiary (organized/mobile) — leverages dramatic settings for mobilization
 *   - policy_activism_coalitions: beneficiary (organized/mobile) — uses clock movements to open legislative windows
 *   - future_public_epistemic_trust: payer (powerless/trapped) — diffuse credibility stock drawn down
 *   - science_communication_field: payer (moderate/constrained) — inherits spillover credibility damage
 *   - general_public_risk_literacy: payer (powerless/trapped) — consumes an unauditable proxy number
 *   - skeptical_risk_analysts: excluded (moderate/constrained) — argue for transparent fixed-model index, unheard
 *   - policy_researchers: observer (analytical) — traces correlation between settings and policy/news cycles
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, 0.62).
domain_priors:suppression_score(doomsday_clock_metric__performative_tool_reading, 0.35).
domain_priors:theater_ratio(doomsday_clock_metric__performative_tool_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__performative_tool_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__performative_tool_reading, "Doomsday Clock as Strategic Mobilization Instrument").
narrative_ontology:topic_domain(doomsday_clock_metric__performative_tool_reading, "science communication / normative epistemology / risk governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__performative_tool_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__performative_tool_reading, '9262e9c6-6b31-4ec7-884d-a5b8cd67a898').
narrative_ontology:cs_kernel_codification('9262e9c6-6b31-4ec7-884d-a5b8cd67a898', implicit).
narrative_ontology:cs_authority_grounding('9262e9c6-6b31-4ec7-884d-a5b8cd67a898', practice).
narrative_ontology:cs_interpretation_layer_present('9262e9c6-6b31-4ec7-884d-a5b8cd67a898').
narrative_ontology:cs_reading_relation('9262e9c6-6b31-4ec7-884d-a5b8cd67a898', doomsday_clock_metric__objective_index_reading, coexists_with).
narrative_ontology:cs_reading_relation('9262e9c6-6b31-4ec7-884d-a5b8cd67a898', doomsday_clock_metric__hybrid_legitimacy_reading, influences).
narrative_ontology:cs_axiom('9262e9c6-6b31-4ec7-884d-a5b8cd67a898', foundational, salience_optimization_is_legitimate_communication_craft).
narrative_ontology:cs_axiom_status(salience_optimization_is_legitimate_communication_craft, holdable).
narrative_ontology:cs_axiom_grounding('9262e9c6-6b31-4ec7-884d-a5b8cd67a898', salience_optimization_is_legitimate_communication_craft, instrumental).
narrative_ontology:cs_axiom('9262e9c6-6b31-4ec7-884d-a5b8cd67a898', secondary, mobilization_value_may_permissibly_outweigh_indicator_fidelity).
narrative_ontology:cs_axiom_status(mobilization_value_may_permissibly_outweigh_indicator_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('9262e9c6-6b31-4ec7-884d-a5b8cd67a898', mobilization_value_may_permissibly_outweigh_indicator_fidelity, instrumental).
narrative_ontology:cs_reference_frame('9262e9c6-6b31-4ec7-884d-a5b8cd67a898', founding_reach_mandate).
narrative_ontology:cs_drift_state('9262e9c6-6b31-4ec7-884d-a5b8cd67a898', contemporary_media_saturated_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9262e9c6-6b31-4ec7-884d-a5b8cd67a898', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, bulletin_of_atomic_scientists_leadership).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, allied_advocacy_organizations).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, policy_activism_coalitions).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, future_public_epistemic_trust).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, science_communication_field).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, general_public_risk_literacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the minute-hand position each year through a closed board deliberation, explicitly weighing what setting will generate media coverage and legislative attention alongside any read of underlying risk indicators. Controls the sole channel through which the metric is produced and retains discretion over how much weight strategic messaging receives versus indicator synthesis. Gains institutional visibility, funding relevance, and continued convening authority from the clock's persistence as a headline-generating device.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, bulletin_of_atomic_scientists_leadership, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__performative_tool_reading, bulletin_of_atomic_scientists_leadership, beneficiary).

% Cite clock movements in campaign materials, funding appeals, and legislative testimony because a dramatic setting change is easier to mobilize around than a stable indicator series. Benefit whenever the setting moves toward midnight regardless of whether the movement tracks a genuine indicator shift, and face no cost if the setting is later judged to have overstated risk.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, allied_advocacy_organizations, beneficiary,
    organized, biographical, mobile, global).

% Use the clock as a rhetorical trigger to open legislative windows on nuclear and climate policy. Their capacity to act is enhanced by a metric optimized for salience, and they have no mechanism or incentive to audit whether the setting was chosen for accuracy or for impact.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, policy_activism_coalitions, beneficiary,
    organized, biographical, mobile, national).

% Represents the diffuse, not-yet-realized capacity of the public to trust future risk communications. Every setting justified partly by mobilization value rather than pure indicator synthesis draws down this stock; the cost lands on whoever needs the public to believe a future warning, not on the current agenda-setter.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, future_public_epistemic_trust, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(doomsday_clock_metric__performative_tool_reading, future_public_epistemic_trust).

% Other scientific communicators inherit a credibility environment shaped by the clock's reputation. When journalists or skeptics characterize the clock as activism dressed as measurement, the discrediting spills over onto adjacent risk-communication efforts (climate indices, pandemic risk dashboards) that did not make the same strategic choices but are tarred by association. They have no seat in how the clock is set and limited ability to publicly disentangle their own indices from it without appearing to attack allies.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, science_communication_field, payer,
    moderate, generational, constrained, global).

% Consumes the clock's annual announcement as a proxy for 'how close is doom,' with no visibility into the closed-door weighting of mobilization value against indicator synthesis. Cannot verify the setting against underlying data and has no practical alternative source with equivalent cultural salience; their risk perception is shaped by a number they cannot audit.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, general_public_risk_literacy, payer,
    powerless, biographical, trapped, global).

% Quantitative risk researchers who would argue the setting should be derived transparently from a fixed indicator model, publishing critiques that the clock is not a measurement instrument at all. They are not represented on the board and their critiques rarely receive the same media pickup as the annual announcement itself.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, skeptical_risk_analysts, excluded,
    moderate, biographical, constrained, national).

% Study the clock's history, correlate settings against actual indicator data and against media/legislative cycles, and can trace whether setting changes better predict subsequent news cycles than subsequent risk trajectories.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, policy_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__performative_tool_reading, bulletin_of_atomic_scientists_leadership).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__performative_tool_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The clock coordinates public and legislative attention onto existential-risk topics at moments the Bulletin's board judges most likely to produce policy traction — solving the genuine problem that expert risk assessments, published as technical reports, routinely fail to reach mass audiences or move legislative agendas.
% TRANSFER_FUNCTION: Moves attention, media coverage, and legislative window-opening capacity toward the Bulletin and allied advocacy organizations, funded implicitly by drawing down the public's ability to calibrate trust in future risk announcements — a cost paid by whoever needs a future warning to be believed.
% ABSENT_VOICES: Skeptical risk analysts who favor a transparent, fixed-model index are not on the board and rarely receive comparable media coverage for their critiques; the general public, who treat the setting as if it were a calibrated instrument, are not consulted on whether they'd prefer a less dramatic but more auditable metric.
% DISAPPEARANCE_RATIONALE: The Bulletin and allied advocacy groups would lose a signature mobilization device and legislative window-opener — for them the world clearly rearranges. But policy researchers and skeptical analysts dispute whether public risk behavior or policy outcomes would actually change if the clock vanished, since its correlation with concrete policy shifts is itself contested; the verdict differs by which seat is asked.
% FOUNDING_PROBLEM: In 1947, the Bulletin needed a simple, visually arresting device to communicate the existential stakes of nuclear weapons to a non-specialist public and to legislators who would not read technical assessments — the problem was audience reach for an urgent but abstract risk.
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin's own leadership attests the founding problem (audience reach for abstract existential risk) remains fully live. Independent science-communication researchers and skeptical risk analysts, outside the Bulletin's donor and advocacy network, attest that the original reach problem has been substantially solved by modern media and specialized risk-communication tools, and that the clock's continued strategic (rather than purely indicator-driven) setting now serves institutional visibility and advocacy mobilization more than the original audience-reach problem.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__performative_tool_reading, contested).
narrative_ontology:founding_problem_status(doomsday_clock_metric__performative_tool_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__performative_tool_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.62) reflects that a substantial share of the setting choice is authored, on this reading, for mobilization value rather than indicator fidelity — real but partial, since some genuine indicator input remains. Theater ratio (0.68) is high and rising because the annual announcement has increasingly become a media event whose format (press conference, dramatic minute movements) outpaces the marginal informational content of the change; the 1990-2024 trajectory shows theater_ratio climbing from 0.30 to 0.68 as the announcement format professionalized around media cycles. Suppression (0.35) is moderate rather than high: no one is coerced into believing the clock, but the closed-board process and absence of a competing salience-matched metric make it hard for skeptical readings to gain equal traction. Accessibility collapse (0.40) is moderate-low: alternative risk indices exist and are used by specialists, but none has comparable cultural reach, so alternatives are marginalized rather than eliminated. Resistance (0.55) reflects the real and growing skeptical-analyst critique documented in science communication literature.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter's seat, strategic setting choice looks like responsible communication craft — meeting the audience where it is. From the payer seats (future trust, general public, adjacent science communicators), the same choice looks like credibility being spent on behalf of goals (mobilization) the payers never authorized and cannot audit. The engine's per-seat computation should reflect this asymmetry structurally, not because either party is wrong about their own experience, but because they occupy genuinely different structural positions relative to who decides and who bears the calibration cost.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bulletin's leadership and allied advocacy organizations are declared beneficiaries because the clock's persistence and cultural salience directly serve their institutional visibility, funding relevance, and mobilization capacity — low d, benefit-side. Future epistemic trust, the broader science communication field, and general public risk literacy are declared victims because the cost of strategic (rather than purely indicator-driven) setting is a slow depletion of calibration trust that lands on parties who did not choose the strategy and cannot decline to bear it — high d, target-side. Skeptical risk analysts are excluded rather than victimized in the direct extraction sense; their cost is exclusion from the process, which the six_questions absent_voices field carries rather than the victims array.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reaching a non-specialist audience with an urgent, abstract existential risk — is contested rather than flatly dead: nuclear and existential risk remain real, so the underlying substantive problem persists, but the specific communication bottleneck the clock was built to solve (no alternative reach mechanism existed in 1947) has been substantially reduced by modern media ecosystems, dedicated risk dashboards, and social media reach. Classifying this reading as tangled_rope rather than snare or pure rope prevents two mislabelings: calling it a pure snare would deny the real coordination function (it does successfully open policy windows that indicator-only reports do not); calling it a pure rope would deny the asymmetric, uncompensated cost imposed on public epistemic trust and adjacent science communicators who bear reputational spillover without any say in the setting process.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strategic_intent_vs_indicator_synthesis,
    'Is the annual clock setting genuinely determined by weighting mobilization value against indicator synthesis, or does the Bulletin board''s internal deliberation actually track indicators closely and the ''strategic'' framing is an outside observer''s uncharitable read?',
    'Access to board deliberation records, minutes, or structured interviews with Science and Security Board members about how much weight media/policy impact receives relative to indicator changes in the final setting decision.',
    'If deliberations show indicator synthesis dominates and mobilization value is incidental, this reading would collapse toward the objective_index_reading and its ε should be substantially lower than 0.62. If deliberations confirm explicit mobilization weighting, the tangled_rope classification and current ε are well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_intent_vs_indicator_synthesis, empirical, 'Whether board process evidence supports the performative-tool framing over the objective-index framing.').

omega_variable(
    credibility_depletion_reversibility,
    'Is the epistemic-trust cost this reading assigns to ''future_public_epistemic_trust'' actually irreversible, or can the Bulletin restore calibration trust through transparency reforms without abandoning the mobilization function?',
    'Track public trust survey data and media fact-checking commentary following any future methodology-transparency reforms the Bulletin might adopt; compare pre/post reform trust trajectories.',
    'If trust proves recoverable through transparency reform alone, the victim designation for future_public_epistemic_trust should be read as a mitigable rather than structurally locked-in cost, softening the tangled_rope classification toward scaffold (transitional, reformable) rather than a stable extraction pattern.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credibility_depletion_reversibility, empirical, 'Whether the credibility cost this reading identifies is reversible through reform.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the choice to read the clock-setting practice as ''performative tool'' (this story) versus ''objective index'' or ''hybrid legitimacy'' (siblings) itself determined by the evidence, or does it depend on priors about whether existential risk communication can ever be purely technical?',
    'None fully resolves this — it is partly conceptual. Comparative discourse analysis of how the Bulletin itself describes its process (technical vs. advocacy framing in its own public statements) across decades would provide partial empirical grounding.',
    'If the Bulletin''s own self-description shifts markedly toward advocacy framing over time, this reading gains empirical support as the historically dominant one; if it shifts toward technical framing, the objective_index_reading gains ground. The three-way kernel split may itself reflect an irreducible framing choice rather than a fact awaiting discovery — which would favor treating hybrid_legitimacy_reading as the more honest account.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the reading choice among the three kernel siblings is itself under-determined by available evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__performative_tool_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t1990, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement_basis(doom_tr_t1990, observed).
narrative_ontology:measurement(doom_tr_t1998, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1998, 0.38).
narrative_ontology:measurement_basis(doom_tr_t1998, observed).
narrative_ontology:measurement(doom_tr_t2006, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2006, 0.48).
narrative_ontology:measurement_basis(doom_tr_t2006, observed).
narrative_ontology:measurement(doom_tr_t2014, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2014, 0.58).
narrative_ontology:measurement_basis(doom_tr_t2014, observed).
narrative_ontology:measurement(doom_tr_t2020, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2020, 0.64).
narrative_ontology:measurement_basis(doom_tr_t2020, observed).
narrative_ontology:measurement(doom_tr_t2024, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2024, 0.68).
narrative_ontology:measurement_basis(doom_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(doom_be_t1990, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1990, 0.32).
narrative_ontology:measurement_basis(doom_be_t1990, observed).
narrative_ontology:measurement(doom_be_t1998, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1998, 0.4).
narrative_ontology:measurement_basis(doom_be_t1998, observed).
narrative_ontology:measurement(doom_be_t2006, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2006, 0.47).
narrative_ontology:measurement_basis(doom_be_t2006, observed).
narrative_ontology:measurement(doom_be_t2014, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2014, 0.53).
narrative_ontology:measurement_basis(doom_be_t2014, observed).
narrative_ontology:measurement(doom_be_t2020, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement_basis(doom_be_t2020, observed).
narrative_ontology:measurement(doom_be_t2024, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2024, 0.62).
narrative_ontology:measurement_basis(doom_be_t2024, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(doomsday_clock_metric__performative_tool_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__performative_tool_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(doomsday_clock_metric__performative_tool_reading, 0.1).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, objective_index_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, hybrid_legitimacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'the Doomsday Clock metric' per the ε-invariance principle. objective_index_reading claims the setting tracks measurable risk through expert synthesis (much lower ε, closer to rope/mountain); hybrid_legitimacy_reading claims the entanglement of scientific judgment and normative stakes is irreducible (its own distinct ε and structure, neither pure measurement nor pure performance); this story (performative_tool_reading) claims the setting is strategically chosen for policy impact (tangled_rope, ε=0.62). All three describe the same kernel practice — the annual Bulletin clock-setting — read differently. They are linked via affects_constraints rather than merged because ε genuinely differs across the readings and forcing one ε would violate ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
