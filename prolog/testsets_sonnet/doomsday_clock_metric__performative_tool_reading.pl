% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__performative_tool_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Doomsday Clock Setting as Strategic Mobilization Instrument
 *   domain: science communication/normative epistemology/risk governance
 *
 * SUMMARY:
 *   This story instantiates the performative_tool_reading of the
 *   doomsday_clock_metric kernel: the clock setting is read as an instrument
 *   selected each year primarily for its capacity to maximize media coverage
 *   and mobilize policy advocacy, rather than as a direct output of indicator
 *   synthesis (objective_index_reading) or as an irreducible fusion of
 *   empirical and normative judgment (hybrid_legitimacy_reading). Under this
 *   reading, the coordination function (a shared annual attention focal
 *   point) is real, but it is paired with an active extraction: the setting's
 *   strategic tuning to news cycles and legislative timing extracts long-run
 *   credibility from the pool of public trust in expert risk communication,
 *   while concentrating short-run benefit on the Bulletin's institutional
 *   prominence and allied advocacy coalitions. The three readings are NOT
 *   three measurements of one constraint at different resolutions — they are
 *   three structurally distinct constraints with different ε, different
 *   beneficiary/victim sets, and different persistence mechanisms, linked
 *   only by sharing a label and a governing kernel.
 *
 * KEY AGENTS:
 *   - bulletin_of_atomic_scientists_board: institutional agenda-setter, sets the hand with an eye to policy timing and media pickup
 *   - policy_advocacy_coalitions: organized beneficiary, borrows the setting's authority for campaign and fundraising messaging
 *   - general_public_risk_literacy: powerless payer, absorbs framing effects with no audit capacity
 *   - future_public_epistemic_trust: non-agent diffuse payer, the depleting stock of credibility this reading's persistence depends on
 *   - rival_risk_metrics_and_indices: excluded, crowded out of cultural salience by the clock's media habituation advantage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, 0.58).
domain_priors:suppression_score(doomsday_clock_metric__performative_tool_reading, 0.28).
domain_priors:theater_ratio(doomsday_clock_metric__performative_tool_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__performative_tool_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__performative_tool_reading, "Doomsday Clock Setting as Strategic Mobilization Instrument").
narrative_ontology:topic_domain(doomsday_clock_metric__performative_tool_reading, "science communication/normative epistemology/risk governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__performative_tool_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__performative_tool_reading, '0cedbebb-536f-4b3b-a596-984baeb17a70').
narrative_ontology:cs_kernel_codification('0cedbebb-536f-4b3b-a596-984baeb17a70', distributed).
narrative_ontology:cs_authority_grounding('0cedbebb-536f-4b3b-a596-984baeb17a70', expertise).
narrative_ontology:cs_interpretation_layer_present('0cedbebb-536f-4b3b-a596-984baeb17a70').
narrative_ontology:cs_reading_relation('0cedbebb-536f-4b3b-a596-984baeb17a70', doomsday_clock_metric__objective_index_reading, coexists_with).
narrative_ontology:cs_reading_relation('0cedbebb-536f-4b3b-a596-984baeb17a70', doomsday_clock_metric__hybrid_legitimacy_reading, influences).
narrative_ontology:cs_axiom('0cedbebb-536f-4b3b-a596-984baeb17a70', foundational, strategic_selection_is_the_dominant_mechanism).
narrative_ontology:cs_axiom_status(strategic_selection_is_the_dominant_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('0cedbebb-536f-4b3b-a596-984baeb17a70', strategic_selection_is_the_dominant_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('0cedbebb-536f-4b3b-a596-984baeb17a70', secondary, mobilization_value_justifies_metric_tuning).
narrative_ontology:cs_axiom_status(mobilization_value_justifies_metric_tuning, holdable).
narrative_ontology:cs_axiom_grounding('0cedbebb-536f-4b3b-a596-984baeb17a70', mobilization_value_justifies_metric_tuning, instrumental).
narrative_ontology:cs_reference_frame('0cedbebb-536f-4b3b-a596-984baeb17a70', post_manhattan_project_scientific_stewardship).
narrative_ontology:cs_drift_state('0cedbebb-536f-4b3b-a596-984baeb17a70', contemporary_media_attention_economy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0cedbebb-536f-4b3b-a596-984baeb17a70', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, bulletin_of_atomic_scientists_board).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, policy_advocacy_coalitions).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, existential_risk_communicators).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, future_public_epistemic_trust).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, general_public_risk_literacy).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, rival_risk_metrics_and_indices).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, existential_risk_communicators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes annually and sets the minute hand, explicitly weighing the setting's likely media pickup and legislative timing alongside indicator review. Controls the sole channel through which the clock's public meaning is produced and can move the hand ahead of, or in response to, anticipated policy votes. Its institutional prestige and funding depend on the clock remaining a headline-generating instrument.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, bulletin_of_atomic_scientists_board, agenda_setter,
    institutional, generational, arbitrage, global).

% Cite the clock setting in testimony, fundraising appeals, and campaign messaging as an authoritative crisis signal. Benefit whenever the hand moves closer to midnight because it lends borrowed scientific gravity to their advocacy asks, regardless of whether the setting's magnitude tracks any measurable change in underlying risk that year.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, policy_advocacy_coalitions, beneficiary,
    organized, biographical, mobile, national).

% Encounters the clock through headlines with no visibility into the deliberation behind the number and no comparative benchmark against which to calibrate it. Absorbs whatever framing effect the setting produces — alarm, fatigue, or numbness — without the capacity to audit whether the movement reflects a real annual risk delta or a strategic communications choice.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, general_public_risk_literacy, payer,
    powerless, biographical, trapped, global).

% Represents the diffuse, non-actor stock of public willingness to treat expert risk metrics as trustworthy. Each cycle in which the setting is later shown to track messaging timing rather than measured indicator change depletes this stock further, making future genuine risk communications harder to land regardless of who is speaking.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, future_public_epistemic_trust, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(doomsday_clock_metric__performative_tool_reading, future_public_epistemic_trust).

% Rely on the clock as a shorthand device to open conversations about nuclear, climate, and biosecurity risk with broad publics. Gain audience attention from its salience but also inherit skepticism when critics point out the setting's history of movements timed to summits, elections, or funding cycles.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, existential_risk_communicators, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__performative_tool_reading, existential_risk_communicators, payer).

% Alternative composite risk indices exist but cannot displace the clock's cultural dominance because the clock's decades of media habituation crowd out competing framings from press coverage and legislative citation, regardless of relative methodological rigor.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, rival_risk_metrics_and_indices, excluded,
    moderate, biographical, constrained, global).

% Cover the annual announcement, sometimes probing the board's stated rationale and sometimes reproducing the setting as self-evidently authoritative. Their choice to interrogate or amplify shapes how much of the strategic-communication function becomes visible to the public.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, science_journalists_and_editors, observer,
    moderate, immediate, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__performative_tool_reading, bulletin_of_atomic_scientists_board).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__performative_tool_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, recurring, low-cost focal symbol that lets disparate risk-communication and advocacy efforts synchronize around one annual moment of public attention, avoiding the coordination failure of dozens of competing, uncorrelated risk announcements.
% TRANSFER_FUNCTION: Moves attention, media coverage, and borrowed scientific authority from the Bulletin's technical review process to whichever policy causes the board and allied advocates foreground in a given year; the underlying cost is paid in the erosion of the public's ability to distinguish a genuine risk-indicator shift from a strategically timed communications choice.
% ABSENT_VOICES: Rival metric developers and dissenting board-adjacent scientists who have argued the setting has drifted from indicator-driven to narrative-driven are rarely centered in the annual announcement's own framing; their critiques surface mainly in retrospective academic and journalistic post-mortems, not in the moment the setting is issued.
% DISAPPEARANCE_RATIONALE: Advocacy coalitions and the Bulletin would lose a uniquely potent annual attention vehicle and would need to rebuild comparable salience from scratch — a real rearrangement for them. But independent risk assessment (arms control monitoring, IPCC reporting, biosecurity indices) would continue unaffected, since those functions do not depend on the clock; whether 'the world' rearranges depends entirely on which function of the clock one is asking about, which is exactly the site of contest between this reading and the objective_index_reading.
% FOUNDING_PROBLEM: In 1947, atomic scientists needed a way to keep nuclear-war risk viscerally present to a public and policy establishment that had no lived experience of the bomb's use and little incentive to keep thinking about it between crises.
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin's own science and security board attests the founding problem — sustained public and elite attention to existential risk — remains live and that the setting still serves it. Independent science-communication researchers and several former board members interviewed in retrospective press coverage attest instead that the mechanism has partially shifted from tracking risk to generating media cycles, and that this shift is under-acknowledged in the board's own public rationale.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__performative_tool_reading, contested).
narrative_ontology:founding_problem_status(doomsday_clock_metric__performative_tool_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__performative_tool_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(doomsday_clock_metric__performative_tool_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__performative_tool_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises across the interval (0.32 to 0.58) reflecting the accumulating gap between the setting's public-facing authority and its increasingly documented sensitivity to communications strategy rather than measured indicator change. Theater ratio is the dominant signal here (0.40 rising to 0.71): the annual announcement ritual — press conference, ceremonial hand movement, media embargo choreography — increasingly outweighs the indicator-synthesis work behind it, which is the diagnostic signature this reading claims is structurally true. Suppression stays comparatively low (0.15 to 0.28) because no one is coerced into believing the clock; its persistence depends on narrative dominance and habituation, not force — this is a Tangled Rope sustained by attention economics, not compulsion.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bulletin's board sits nearest full beneficiary: it controls the sole production channel, times the setting for maximum reach, and its institutional standing is directly served by continued salience. Policy advocacy coalitions and existential risk communicators are secondary beneficiaries, borrowing authority they did not generate. General_public_risk_literacy and future_public_epistemic_trust sit at the target end: trapped exit (there is no alternative to 'the public' or to accumulated trust), no capacity to audit the setting's derivation, and no compensating benefit that offsets the framing costs they absorb.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mislabeling the clock as pure coordination (a Rope) by insisting on the victim side of the ledger: something is genuinely lost — calibrated public trust in risk metrics — even though something genuine is also gained — sustained public attention to real existential risks. Classifying it Tangled Rope rather than Snare preserves the real coordination function (the annual focal point does mobilize otherwise-diffuse attention) while still registering the asymmetric extraction (the board's institutional interest in continued salience is not perfectly aligned with, and sometimes trades against, calibrated public understanding).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strategic_selection_vs_indicator_synthesis,
    'In any given year, how much of the observed clock movement is attributable to genuine changes in expert-assessed risk indicators versus anticipated media and policy timing?',
    'Comparative analysis of board deliberation records (where available), indicator datasets cited in Bulletin statements, and the correlation between setting changes and proximate political/media events (summits, elections, funding cycles) across the clock''s full history.',
    'If strategic timing dominates, this reading''s classification as substantially extractive Tangled Rope is strongly supported; if indicator synthesis dominates most years, the mass of cases would belong to the objective_index_reading instead, and this reading would describe only a minority of movements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_selection_vs_indicator_synthesis, empirical, 'Whether historical clock movements are better explained by risk-indicator change or by communications strategy.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the disagreement between the performative_tool_reading, objective_index_reading, and hybrid_legitimacy_reading actually live — in disputed facts about board process, or in an irreducible framing choice about what ''tracking risk'' means for an existential-risk metric?',
    'Structured elicitation of board members'' own accounts of deliberation, cross-checked against the hybrid_legitimacy_reading''s claim that empirical and normative judgment cannot be cleanly separated in this domain at all.',
    'If the disagreement is mainly factual (about process), better documentation of board deliberation could resolve which reading is closer to true. If it is a framing choice (the hybrid reading''s claim), no amount of process transparency would settle which reading is correct, and all three readings would remain permanently coexisting rather than one becoming dominant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Whether the three kernel readings disagree about facts or about an irreducible framing of what the metric is for.').

omega_variable(
    epistemic_trust_depletion_reversibility,
    'Is the erosion of public epistemic trust in the clock (and in expert risk metrics generally) reversible through disclosure and methodological reform, or is it a one-way depletion?',
    'Track public trust survey data before and after major transparency initiatives (e.g., published indicator methodology, board composition changes) to test whether disclosure measurably restores calibration.',
    'If reversible, the victim-side extraction is a correctable design flaw rather than an intrinsic feature of the performative-tool mechanism, softening the Tangled Rope classification toward Scaffold (temporary, fixable) rather than a persistent extractive structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_trust_depletion_reversibility, empirical, 'Whether the credibility cost this reading identifies as a victim harm is structurally reversible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__performative_tool_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t0, doomsday_clock_metric__performative_tool_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(doom_tr_t6, doomsday_clock_metric__performative_tool_reading, theater_ratio, 6, 0.48).
narrative_ontology:measurement(doom_tr_t12, doomsday_clock_metric__performative_tool_reading, theater_ratio, 12, 0.55).
narrative_ontology:measurement(doom_tr_t18, doomsday_clock_metric__performative_tool_reading, theater_ratio, 18, 0.62).
narrative_ontology:measurement(doom_tr_t24, doomsday_clock_metric__performative_tool_reading, theater_ratio, 24, 0.67).
narrative_ontology:measurement(doom_tr_t30, doomsday_clock_metric__performative_tool_reading, theater_ratio, 30, 0.71).

% Extraction over time
narrative_ontology:measurement(doom_be_t0, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(doom_be_t6, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 6, 0.4).
narrative_ontology:measurement(doom_be_t12, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(doom_be_t18, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 18, 0.51).
narrative_ontology:measurement(doom_be_t24, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(doom_be_t30, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t0, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(doom_su_t6, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 6, 0.18).
narrative_ontology:measurement(doom_su_t12, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 12, 0.21).
narrative_ontology:measurement(doom_su_t18, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 18, 0.24).
narrative_ontology:measurement(doom_su_t24, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 24, 0.26).
narrative_ontology:measurement(doom_su_t30, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 30, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__performative_tool_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(doomsday_clock_metric__performative_tool_reading, 0.1).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric__objective_index_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric__hybrid_legitimacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the doomsday_clock_metric kernel (ε-invariance decomposition, not a single constraint measured three ways). objective_index_reading claims low extraction and treats the setting as expert synthesis; hybrid_legitimacy_reading treats the empirical/normative fusion as structurally irreducible and does not assign a clean victim; this performative_tool_reading claims substantial, rising extraction with identified victims (epistemic trust, public risk literacy, crowded-out rival metrics) because it treats strategic-communications selection as the dominant operative mechanism. All three share the kernel_id and must be interpreted together, not averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
