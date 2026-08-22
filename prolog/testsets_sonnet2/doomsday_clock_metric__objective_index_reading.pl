% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__objective_index_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__objective_index_reading, []).

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
 *   constraint_id: doomsday_clock_metric__objective_index_reading
 *   human_readable: Doomsday Clock as Objective Existential-Risk Index
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   This story instantiates the OBJECTIVE-INDEX READING of the Doomsday Clock
 *   kernel: the claim that the annual minute-hand setting is a measurement —
 *   an expert synthesis of empirical indicators (warhead counts, emissions
 *   trajectories, biosecurity incidents, disruptive-technology trend lines)
 *   that tracks a real underlying quantity, existential risk level. On this
 *   reading the clock's authority rests on its claim to be reporting a
 *   finding, not making a normative judgment. The structural cost of this
 *   framing is that it treats an irreducibly value-laden commensuration — how
 *   much nuclear risk equals how much climate risk equals how much AI risk —
 *   as if it were metrology, which forecloses the normal channels through
 *   which the underlying value tradeoffs would otherwise be publicly
 *   contested. This is a distinct constraint from the
 *   hybrid_legitimacy_reading (which holds the entanglement of science and
 *   norms is irreducible and does not attempt to suppress it) and from the
 *   performative_tool_reading (which holds the setting is frankly chosen for
 *   maximal mobilization effect, not measurement fidelity). Each reading has
 *   its own epsilon and its own beneficiary/victim structure; they are linked
 *   here only via network edges, not merged.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, 0.58).
domain_priors:suppression_score(doomsday_clock_metric__objective_index_reading, 0.71).
domain_priors:theater_ratio(doomsday_clock_metric__objective_index_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__objective_index_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__objective_index_reading, "Doomsday Clock as Objective Existential-Risk Index").
narrative_ontology:topic_domain(doomsday_clock_metric__objective_index_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__objective_index_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__objective_index_reading, 'deb05f04-80ae-4f98-9fc4-d69fdc463a6a').
narrative_ontology:cs_kernel_codification('deb05f04-80ae-4f98-9fc4-d69fdc463a6a', formalized).
narrative_ontology:cs_authority_grounding('deb05f04-80ae-4f98-9fc4-d69fdc463a6a', expertise).
narrative_ontology:cs_interpretation_layer_present('deb05f04-80ae-4f98-9fc4-d69fdc463a6a').
narrative_ontology:cs_reading_relation('deb05f04-80ae-4f98-9fc4-d69fdc463a6a', doomsday_clock_metric__hybrid_legitimacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('deb05f04-80ae-4f98-9fc4-d69fdc463a6a', doomsday_clock_metric__performative_tool_reading, forecloses).
narrative_ontology:cs_axiom('deb05f04-80ae-4f98-9fc4-d69fdc463a6a', foundational, risk_synthesis_is_measurement_not_judgment).
narrative_ontology:cs_axiom_status(risk_synthesis_is_measurement_not_judgment, holdable).
narrative_ontology:cs_axiom_grounding('deb05f04-80ae-4f98-9fc4-d69fdc463a6a', risk_synthesis_is_measurement_not_judgment, empirically_contingent).
narrative_ontology:cs_axiom('deb05f04-80ae-4f98-9fc4-d69fdc463a6a', secondary, expert_commensuration_of_incommensurable_risks_is_epistemically_valid).
narrative_ontology:cs_axiom_status(expert_commensuration_of_incommensurable_risks_is_epistemically_valid, holdable).
narrative_ontology:cs_axiom_grounding('deb05f04-80ae-4f98-9fc4-d69fdc463a6a', expert_commensuration_of_incommensurable_risks_is_epistemically_valid, instrumental).
narrative_ontology:cs_reference_frame('deb05f04-80ae-4f98-9fc4-d69fdc463a6a', id_1947_nuclear_arms_race_signaling_practice).
narrative_ontology:cs_drift_state('deb05f04-80ae-4f98-9fc4-d69fdc463a6a', contemporary_multidomain_composite_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('deb05f04-80ae-4f98-9fc4-d69fdc463a6a', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, bulletin_science_security_board).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, credentialed_risk_science_establishment).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, public_deliberative_capacity).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, lay_publics_excluded_from_weighting_choices).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, elected_policy_bodies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, science_journalists_and_science_communicators).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, science_journalists_and_science_communicators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the minute-hand position annually, presenting the number as a direct readout of measured indicators (nuclear posture, climate metrics, biosecurity, disruptive tech). Controls which indicators enter the synthesis, how they are weighted against each other, and the final composite figure. Frames the outcome as a finding rather than a judgment, which forecloses public contestation of the weighting choices.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, bulletin_science_security_board, agenda_setter,
    institutional, generational, arbitrage, global).

% Physicists, climate scientists, and security scholars whose expert standing is what makes the clock's number legible as measurement rather than opinion. They gain deference, media platform, and policy access precisely because the index-reading frame treats their synthesis as objective rather than as one normative weighting among possible others.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, credentialed_risk_science_establishment, beneficiary,
    organized, generational, arbitrage, global).

% Receives an annual number with no visible mechanism for contesting how nuclear risk was traded off against climate risk against biosecurity risk in the composite. Under the objective-index framing, disagreeing with the setting looks like disagreeing with a measurement rather than a value judgment, which closes off the normal channels (public comment, electoral pressure, competing expert panels) through which contested value tradeoffs would ordinarily be litigated.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, public_deliberative_capacity, payer,
    powerless, biographical, trapped, global).

% Bear the downstream consequences of whichever risk domain the board privileges in a given year (nuclear vs. climate vs. AI) without having participated in the tradeoff. The objective-index reading treats this weighting as a technical output of expert synthesis, not a political choice open to their input, even though the underlying commensuration of incommensurable risks is itself a value act.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, lay_publics_excluded_from_weighting_choices, payer,
    powerless, generational, trapped, global).

% Legislatures and executive risk offices are expected to respond to the clock's setting as though it were an external empirical fact analogous to a thermometer reading, which constrains their ability to contest the underlying value tradeoffs through ordinary democratic processes without appearing to reject science itself. Their exit is constrained: ignoring the clock costs political credibility; contesting its methodology is read as risk denial.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, elected_policy_bodies, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__objective_index_reading, elected_policy_bodies, excluded).

% Amplify the annual setting as a clean, quotable index because the objective-index frame makes it easy to report without needing to unpack the normative weighting embedded in the synthesis. Benefit from a simple story; also constrained by it, since reporting the number as contested methodology rather than as fact is a harder story to sell.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, science_journalists_and_science_communicators, beneficiary,
    moderate, immediate, constrained, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__objective_index_reading, science_journalists_and_science_communicators, payer).

% Study how the clock's composite methodology commensurates fundamentally different risk domains (probability, magnitude, reversibility, time horizon) into one dial position, and document that this commensuration necessarily embeds value choices the objective-index framing suppresses.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, science_and_technology_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__objective_index_reading, bulletin_science_security_board).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__objective_index_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synthesizes dispersed expert assessments of nuclear, climate, biological, and technological risk into a single, memorable, comparable annual signal, solving the real coordination problem that no lay public or single policymaker can independently track and weigh dozens of technical indicators across incommensurable domains.
% TRANSFER_FUNCTION: Moves interpretive authority over existential risk tradeoffs from democratic and pluralistic deliberation to a closed expert board; moves public attention and media coverage toward whichever risk domain the board privileges in a given year's synthesis.
% ABSENT_VOICES: Publics and elected bodies who would contest how nuclear risk was traded off against climate or AI risk in the composite are not part of the synthesis process; ethicists and affected communities who might weigh incommensurable harms differently than physicists and security scholars are absent from the weighting room.
% DISAPPEARANCE_RATIONALE: The board and much of the science-communication apparatus would say the world loses a valuable synthesis tool and public risk literacy would decline; critics of the objective-index framing would say the world regains contestable, pluralistic deliberation over risk tradeoffs that the single-number format currently forecloses. Whether the clock's disappearance rearranges anything material depends on which reading of what the clock actually does is correct — which is exactly the kernel dispute this story is one reading of.
% FOUNDING_PROBLEM: In 1947, atomic scientists needed a way to communicate the urgency of nuclear risk to a lay public and policymakers who lacked the technical background to evaluate weapons science directly.
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin's own board attests the founding problem (making existential risk legible to non-experts) remains fully live and has only grown given added domains. Independent STS scholars and some risk-communication researchers attest that the original narrow nuclear-signaling problem has been supplanted by a much broader, contestable commensuration exercise across incommensurable risk domains, and that the objective-index framing was never validated as measurement by any external metrological standard — this corroboration comes from outside the Bulletin's own board and affiliated science-security establishment.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__objective_index_reading, contested).
narrative_ontology:founding_problem_status(doomsday_clock_metric__objective_index_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__objective_index_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(doomsday_clock_metric__objective_index_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__objective_index_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__objective_index_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(doomsday_clock_metric__objective_index_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58 at interval end) reflects the transfer of interpretive authority away from democratic deliberation toward a closed expert board, growing over time as the clock's domain coverage expanded (from a narrow nuclear-arms-race index in 1947 to a multi-domain composite by the 2020s), which increased the scope of value judgments smuggled under the measurement label. Suppression (0.71) is high because the objective-index framing is precisely what makes contesting the setting look like contesting science rather than contesting a value weighting — the suppression is conceptual/framing-based rather than coercive in the legal sense, but it is real: dissent gets recoded as risk denial. Theater ratio (0.42) is moderate-high: a real synthesis of indicators occurs, but an increasing share of the annual ritual (press conference, symbolic hand movement, media cycle) functions as legitimation theater for a number whose precise composite weighting is not independently audited or reproducible by outside panels. Accessibility collapse (0.62) is substantial: once a lay audience accepts the 'this is a measurement' frame, alternative framings (that this is one contestable synthesis among several possible normative weightings) become hard to access. Resistance (0.48) is moderate — STS scholars, some scientists, and risk-communication researchers actively contest the framing, which keeps resistance from being negligible the way it would be for a genuine mountain.
 *
 * PERSPECTIVAL GAP:
 *   From the board's seat, the annual setting is a professional, good-faith synthesis exercise defended by decades of institutional practice — coordination, not extraction. From the seat of an excluded public or a constrained elected body, the same act operates as a closed-door value judgment dressed in the authority of measurement, with no mechanism to contest the weighting short of appearing to reject science. The engine computes these as different seat classifications from the same structural data; the claimed_type (tangled_rope) is authored as the structurally true middle reading, independent of either seat's self-perception.
 *
 * DIRECTIONALITY LOGIC:
 *   The board and the credentialed risk-science establishment sit near the beneficiary end: they collect deference, platform, and policy access that flows specifically from the measurement frame being accepted, and their exit options are excellent (arbitrage — they can always retreat to more hedged or academic framings without losing standing). Public deliberative capacity and excluded lay publics sit near the full-target end: trapped, with no structural channel to contest the weighting, and bearing the downstream cost of whichever risk domain gets privileged. Elected policy bodies are constrained rather than trapped — they retain formal authority but pay a political-credibility cost for open contestation, which is why they are marked payer/excluded rather than simple payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (translating technical nuclear-arms-race risk into a legible public signal in 1947) is genuinely contested as either still fully live or substantially transformed. Because the founding_problem_status is 'contested' rather than cleanly 'dead', this does not read as a simple zombie institution — but the mismatch worth flagging is that the board's own account of the founding problem (status: still live, growing) is the ONLY account that fully endorses the objective-index framing; external corroboration (STS scholarship) attests the arrangement's actual operation has drifted from public risk-signaling toward interpretive gatekeeping. Classifying this as tangled_rope rather than snare or mountain prevents two mislabeling errors: treating the clock as pure coordination (ignoring the real transfer of interpretive authority away from democratic bodies) and treating it as pure extraction with no coordination value (ignoring that dispersed technical risk information genuinely is hard for lay publics to synthesize unaided).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_vs_normative_synthesis,
    'Is the clock''s composite setting a genuine measurement of an underlying existential-risk quantity, or is it an irreducibly normative synthesis that the objective-index framing mischaracterizes as measurement?',
    'Attempt independent replication: give the same underlying indicator set to multiple independently constituted expert panels with different disciplinary compositions and see whether they converge on the same minute-hand position. Convergence would support the objective-index reading; systematic divergence tied to panel composition would support the hybrid-legitimacy or performative-tool readings.',
    'If independent panels reliably diverge based on which values they bring to weighting incommensurable risks, the objective-index reading''s core premise (that this is measurement) fails, and the constraint''s suppression of normative contestation becomes harder to justify as scientifically warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_vs_normative_synthesis, empirical, 'Whether the clock setting is reproducible measurement or panel-dependent normative synthesis.').

omega_variable(
    which_reading_the_board_actually_endorses,
    'Does the Bulletin''s Science and Security Board itself operate under the objective-index reading, the hybrid-legitimacy reading, or a version closer to the performative-tool reading, and does its public communication match its internal understanding?',
    'Compare internal board deliberation records and member statements (where available) against the public-facing press materials and statements accompanying each year''s setting.',
    'If the board privately treats the setting as a strategic communication choice (performative-tool) while publicly presenting it as objective measurement (this reading), the suppression documented here is doing additional work of concealing the board''s own understanding of its practice, which would sharpen the tangled_rope classification toward the extraction pole.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_the_board_actually_endorses, conceptual, 'Whether the board''s self-understanding matches the objective-index framing it publicly presents.').

omega_variable(
    expert_monopoly_democratic_legitimacy_tradeoff,
    'Is expert monopoly over existential-risk weighting a justified epistemic division of labor (most publics genuinely lack the technical background to weigh nuclear vs. climate vs. AI risk) or an illegitimate usurpation of a decision that should remain contestable by democratic and pluralistic processes regardless of technical complexity?',
    'This is fundamentally a values question about the proper locus of authority over value-laden technical tradeoffs; it could be informed by comparative study of other domains (e.g., IPCC''s more procedurally open synthesis process) but cannot be fully resolved empirically.',
    'If expert monopoly is judged illegitimate even where technically justified, the tangled_rope classification''s victim declaration (democratic accountability) is strengthened; if judged a justified division of labor, the extraction framing weakens toward a more rope-like reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(expert_monopoly_democratic_legitimacy_tradeoff, preference, 'Whether expert monopoly over risk-weighting is a justified division of labor or an accountability failure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__objective_index_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t0, doomsday_clock_metric__objective_index_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(doom_tr_t8, doomsday_clock_metric__objective_index_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(doom_tr_t16, doomsday_clock_metric__objective_index_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(doom_tr_t24, doomsday_clock_metric__objective_index_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(doom_tr_t32, doomsday_clock_metric__objective_index_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(doom_tr_t40, doomsday_clock_metric__objective_index_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(doom_be_t0, doomsday_clock_metric__objective_index_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(doom_be_t8, doomsday_clock_metric__objective_index_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(doom_be_t16, doomsday_clock_metric__objective_index_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(doom_be_t24, doomsday_clock_metric__objective_index_reading, base_extractiveness, 24, 0.51).
narrative_ontology:measurement(doom_be_t32, doomsday_clock_metric__objective_index_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(doom_be_t40, doomsday_clock_metric__objective_index_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t0, doomsday_clock_metric__objective_index_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(doom_su_t8, doomsday_clock_metric__objective_index_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(doom_su_t16, doomsday_clock_metric__objective_index_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement(doom_su_t24, doomsday_clock_metric__objective_index_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(doom_su_t32, doomsday_clock_metric__objective_index_reading, suppression_requirement, 32, 0.68).
narrative_ontology:measurement(doom_su_t40, doomsday_clock_metric__objective_index_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__objective_index_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(doomsday_clock_metric__objective_index_reading, 0.1).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__hybrid_legitimacy_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__performative_tool_reading).

% DUAL FORMULATION NOTE:
% This constraint (objective_index_reading) is one of three sibling readings of the doomsday_clock_metric kernel, decomposed per the epsilon-invariance principle because the three readings assign structurally different epsilon values and different beneficiary/victim sets to the same underlying institutional practice. objective_index_reading claims the setting is measurement and authors the highest suppression score of the three (0.71) because that specific claim is what forecloses normative contestation. hybrid_legitimacy_reading is expected to author lower suppression (it does not claim pure measurement, so it does not need to suppress the normative dimension to sustain its own legitimacy claim). performative_tool_reading is expected to author a different beneficiary structure (mobilization-effectiveness stakeholders rather than credentialed-authority stakeholders) and a different extraction profile (extraction toward whoever is mobilized inaccurately, rather than toward democratic accountability broadly). All three share the same underlying practice but are analytically distinct constraints under DP-001 ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
