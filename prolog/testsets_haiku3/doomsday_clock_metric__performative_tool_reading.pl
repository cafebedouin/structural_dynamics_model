% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__performative_tool_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: doomsday_clock_metric__performative_tool_reading
 *   human_readable: Doomsday Clock Metric (Performative Tool Reading)
 *   domain: science_communication/risk_governance/normative_epistemology
 *
 * SUMMARY:
 *   The Doomsday Clock, maintained by the Bulletin of the Atomic Scientists,
 *   is an annual symbolic representation of existential risk expressed as
 *   'time to midnight' (apocalypse). The stewardship community uses the clock
 *   to communicate collective threat assessments to policy makers and the
 *   public, with the explicit goal of mobilizing action on disarmament and
 *   existential risk mitigation. This reading frames the clock as a strategic
 *   tool deliberately positioned to maximize policy impact, treating the
 *   annual setting decision as a site where scientific judgment and
 *   communicative intention are entangled. The stewards face a structural
 *   dilemma: a reading that appears purely technical but fails to move policy
 *   is perceived as a diagnosis that does not matter; a reading that clearly
 *   achieves policy impact but appears strategically positioned undermines
 *   the epistemic authority it rests on. This reading accepts that strategic
 *   positioning occurs and examines the constraint that emerges when
 *   coordination (mobilization) is bundled with extraction (epistemic
 *   credibility degradation). The three sibling readings present this kernel
 *   differently: objective_index_reading treats the clock as a measurement
 *   independent of communication goals; hybrid_legitimacy_reading frames the
 *   entanglement of science and normativity as irreducible and legitimate.
 *
 * KEY AGENTS:
 *   - doomsday_clock_stewards: Institutional agenda-setters (generational horizon, mobile exit) — maintain the clock and set annual readings. Face pressure to balance measurement precision against policy impact.
 *   - policy_activism_community: Organized beneficiaries (biographical horizon, mobile exit) — use clock readings to mobilize constituencies and secure legislative attention. Gain focal points for coordination but also bear future cost of eroded epistemic trust.
 *   - scientific_assessment_community: Powerful payers (generational horizon, constrained exit) — contribute expertise to technical analysis; bear reputational risk when clock appears strategically positioned.
 *   - objective_measurement_adherents: Observer seat (powerful, generational horizon, mobile exit) — analyze each clock setting to separate measurable shifts from policy calibration; create pressure that keeps stewardship honest.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, 0.68).
domain_priors:suppression_score(doomsday_clock_metric__performative_tool_reading, 0.42).
domain_priors:theater_ratio(doomsday_clock_metric__performative_tool_reading, 0.76).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, theater_ratio, 0.76).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__performative_tool_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__performative_tool_reading, "Doomsday Clock Metric (Performative Tool Reading)").
narrative_ontology:topic_domain(doomsday_clock_metric__performative_tool_reading, "science_communication/risk_governance/normative_epistemology").

domain_priors:requires_active_enforcement(doomsday_clock_metric__performative_tool_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__performative_tool_reading, '1c9b8bd9-23ca-4d31-9fdd-903e65e50501').
narrative_ontology:cs_kernel_codification('1c9b8bd9-23ca-4d31-9fdd-903e65e50501', distributed).
narrative_ontology:cs_authority_grounding('1c9b8bd9-23ca-4d31-9fdd-903e65e50501', extraction).
narrative_ontology:cs_interpretation_layer_present('1c9b8bd9-23ca-4d31-9fdd-903e65e50501').
narrative_ontology:cs_reading_relation('1c9b8bd9-23ca-4d31-9fdd-903e65e50501', doomsday_clock_metric__objective_index_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c9b8bd9-23ca-4d31-9fdd-903e65e50501', doomsday_clock_metric__hybrid_legitimacy_reading, influences).
narrative_ontology:cs_axiom('1c9b8bd9-23ca-4d31-9fdd-903e65e50501', foundational, strategic_communication_justified_by_political_necessity).
narrative_ontology:cs_axiom_status(strategic_communication_justified_by_political_necessity, holdable).
narrative_ontology:cs_axiom_grounding('1c9b8bd9-23ca-4d31-9fdd-903e65e50501', strategic_communication_justified_by_political_necessity, instrumental).
narrative_ontology:cs_axiom('1c9b8bd9-23ca-4d31-9fdd-903e65e50501', secondary, focal_point_requires_strategic_positioning).
narrative_ontology:cs_axiom_status(focal_point_requires_strategic_positioning, holdable).
narrative_ontology:cs_axiom_grounding('1c9b8bd9-23ca-4d31-9fdd-903e65e50501', focal_point_requires_strategic_positioning, empirically_contingent).
narrative_ontology:cs_reference_frame('1c9b8bd9-23ca-4d31-9fdd-903e65e50501', scientific_measurement_independent_of_policy_goal).
narrative_ontology:cs_drift_state('1c9b8bd9-23ca-4d31-9fdd-903e65e50501', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1c9b8bd9-23ca-4d31-9fdd-903e65e50501', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, policy_activism_community).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, epistemic_credibility_of_futurity_claims).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, public_long_term_trust_in_expert_judgment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, media_institutions).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, policy_activism_community).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, scientific_assessment_community).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__performative_tool_reading, strategic_communication_necessity_for_collective_action).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Bulletin of the Atomic Scientists board sets the clock reading annually. They publicly justify each setting through technical analysis of nuclear risk, climate change, and biosecurity indicators. Internally, they acknowledge that communication effect matters as much as measurement precision — a setting that fails to move policy is perceived as a failed diagnosis. They face pressure to calibrate the reading to maintain public and policy attention without appearing to make up numbers.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, doomsday_clock_stewards, agenda_setter,
    institutional, generational, mobile, global).

% Disarmament and climate advocates use each clock reading as a focal point for mobilizing constituencies and securing media coverage. A clock update that moves closer to midnight (more dire) triggers legislative pushes, fundraising surges, and public attention. They benefit from the clock's authority as a credible signal and its power to dramatize abstract risks into concrete urgency. They also bear the cost: each reading that later appears strategically positioned rather than empirically grounded erodes the platform's future persuasive power.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, policy_activism_community, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__performative_tool_reading, policy_activism_community, payer).

% Individual scientists who contribute to the clock's technical analysis must defend the reading to peers and funders. If the clock is perceived as strategically manipulated, it damages the credibility of the underlying science it purports to represent. They carry reputational risk when the clock's communication logic diverges from measurement precision. Their exit is limited: they can refuse to participate, but that does not stop the clock — it only removes their expertise from the process.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, scientific_assessment_community, payer,
    powerful, generational, constrained, global).

% Each clock update is a recurring, calendar-driven news peg. Media outlets benefit from the predictable, dramatic story: an annual doomsday judgment that justifies coverage of existential risk. Strategic positioning of the clock toward dramatic readings produces better engagement metrics. They have limited incentive to scrutinize whether the reading reflects measurement precision or policy calculation.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, media_institutions, beneficiary,
    powerful, biographical, mobile, global).

% Fossil fuel and nuclear weapons industries, geopolitical actors committed to current deterrence structures, and conservative political factions who would object to the clock's implied policy agenda are structurally excluded from the stewardship process. They rarely contest the clock's technical framing directly; instead, they attack it as alarmism or strategic manipulation — a critique that gains force each time the reading appears more responsive to policy goals than measurement shifts.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, policy_inaction_constituencies, excluded,
    powerful, biographical, trapped, global).

% Scientists and philosophers who believe metrics should track empirical phenomena independently of communication goals, without deliberate strategic adjustment. They analyze each clock setting to separate measurable indicator shifts from apparent policy calibration. Their observational role creates the pressure that keeps stewardship honest — and creates the reputational risk when divergence is detected.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, objective_measurement_adherents, observer,
    powerful, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__performative_tool_reading, policy_activism_community).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__performative_tool_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Produces a focal-point metric that coordinates collective attention on existential risk and enables synchronized policy mobilization. Without a single authoritative clock reading, existential risk narratives remain diffuse and uncoordinated; the clock solves the problem of making abstract, diffuse dangers concrete and focal enough to move constituencies.
% TRANSFER_FUNCTION: Transfers credibility from the scientific assessment community and epistemic authority from the objective-measurement norm to the policy activism community's mobilization agenda. Each strategically positioned reading trades future epistemic standing for present policy impact — moves authority across time horizons and constituencies.
% ABSENT_VOICES: Industries and geopolitical actors whose interests run counter to disarmament and climate action are excluded from the stewardship board and the technical deliberation; they would argue for lower readings or abolition of the metric as an instrument of activist propaganda. Future decision-makers and publics have no seat at today's table and cannot voice the cost to future epistemic trust they will inherit.
% DISAPPEARANCE_RATIONALE: If the clock vanished, the policy activism community would lose a focal point that has repeatedly generated surges in legislative attention and public mobilization. An alternative focal metric would likely emerge, but it would lack the Bulletin's scientific credibility and 80-year legitimacy. The arrangement would fragment into competing doomsday claims, each with less coordinating power. Epistemic trust in futurity claims would improve modestly due to absence of the strategic positioning dynamic.
% FOUNDING_PROBLEM: Existential risks (nuclear weapons, climate change, engineered pandemics) are genuine, empirically grounded threats whose time scales and causal chains make them invisible to political systems optimized for quarterly and election cycles. Expert judgment exists but lacks institutional power to move policy; constituencies exist but lack focal points to coordinate action. The clock was founded to solve the coordination problem: make the diagnosis audible and persistent enough to move policy.
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin attests the founding problem is live and the clock addresses it by maintaining policy salience. Policy researchers cite the clock as a correlate of legislative activity on disarmament (post-hoc attribution, causation contested). Climate scientists attest that public awareness of existential climate risk has remained diffuse and episodic despite decades of scientific consensus — the clock's framing has not solved this problem uniformly. Scholars of science communication and political systems argue the founding problem persists because political-cycle incentives have not fundamentally shifted; the clock is a temporary focal point that must be renewed annually. Corroboration from outside the benefiting parties: objective measurement adherents and journalists who track the clock's apparent divergence from measurement shifts, noting that policy sensitivity often exceeds changes in objective indicators.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__performative_tool_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__performative_tool_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__performative_tool_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(doomsday_clock_metric__performative_tool_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__performative_tool_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderately high (0.68) because the constraint trades epistemic credibility for present policy impact — a real extraction, though from an abstract victim. Suppression is moderate (0.42) because the strategic positioning is not hidden behind technical cover so much as accepted as an open question about whether communication effects justify metric adjustment. Theater ratio is high (0.76) because the performative function — the annual announcement, the media event, the focal point for mobilization — increasingly dominates the technical justification. The measurement series trace the increasing dominance of performative over technical reasoning: theater_ratio rises from 0.32 (1991) to 0.76 (2026) as the stewardship community becomes more explicit about communication goals. Extractiveness and theater_ratio show steady accumulation, indicating that what began as technical synthesis with communication awareness has shifted toward communication-driven technical framing. Accessibility_collapse (0.48) is moderate because alternative framings of existential risk exist (academic probabilistic models, other focal metrics, non-clock-like warnings) but the clock's 80-year legitimacy and simple metaphor make alternatives hard to mobilize at the same scale. Resistance (0.71) is high because the objective-measurement community actively contests the strategic positioning, and journalists now flag apparent divergences between objective indicators and clock movements — this high resistance is consistent with a tangled_rope classification (coordination meets real pushback from measurement-norm adherents).
 *
 * PERSPECTIVAL GAP:
 *   From the stewardship and activism seats, the clock functions appropriately: it coordinates attention on real existential risks and mobilizes policy action that would not occur without the focal point. Strategic positioning is necessary given political system resistance to slow-moving, abstract threats. From the scientific assessment and measurement-norm seats, the clock is extractive and corrosive: each strategically positioned reading trains audiences to expect expert metrics to track political goals rather than phenomena. The measurement series show this divergence sharpening: from 1991 to 2026, theater_ratio rises and resistance rises in tandem, indicating growing visible conflict between the technical and performative functions. From the objective-measurement seat, the constraint approaches snare territory if the strategic positioning can be demonstrated; from the activism seat, it is a necessary tool. The engine's per-seat classification should compute this divergence directly: the stewardship seat (d near 0.15, benefits from control + faces growing pressure) and activism seat (d near 0.20, benefits from focal point) should compute as rope-adjacent or beneficiary-leaning; the scientific assessment seat (d near 0.55, genuine expert input but reputational risk) and measurement-norm seat (d near 0.60, adversarial to positioning) should compute as intermediate or payer-leaning.
 *
 * DIRECTIONALITY LOGIC:
 *   Policy activism community benefits substantially (d ≈ 0.22, overridden from automated derivation ~0.18 because 'organized' power + mobility allows them to leverage the clock for mobilization more effectively than passive coordination alone would suggest). Epistemic credibility and future public trust are targeted (d ≈ 0.98, near full target: they are structural victims with no exit). Scientific assessment community sits intermediate (d ≈ 0.55): they gain platform authority when the clock succeeds but bear reputational risk when strategic positioning becomes visible. Stewardship is the agenda-setter (d ≈ 0.15): they control the metric but increasingly their own authority is contingent on managing perception that they are doing science, not activism. Objective-measurement adherents are observers (no d value); policy-inaction constituencies are excluded (no d value in standard derivation, though they are tangentially affected by the constraint's framing). The override on organized power reflects the fact that organized actors with mobile exit can capture more value from a focal-point coordination tool than the structural derivation captures — they can redirect it, amplify it, use it for constituency mobilization in ways that a standard d derivation would underweight.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (existential risks are real but invisible to political systems optimized for short cycles) was valid in 1991 and remains contested in 2026. The stewardship community argues the founding problem is live: nuclear risk persists, climate change accelerates, biosecurity threats are mounting — the clock's role is to keep these threats from disappearing from policy attention. Objective-measurement adherents and scholars of science policy argue the founding problem is partly solved: climate is on policy agendas, disarmament efforts continue (imperfectly), biosecurity has institutional focus — but the clock's incremental contribution is overstated and its cost is mounting. The constraint avoids piton diagnosis because the clock still produces measurable legislative and media activity post-announcement — the coordination function is real, not merely theatrical (theater_ratio is high but not dominant enough to suggest complete atrophy). It lands on tangled_rope because the coordination function and extraction function are inseparably bundled: strategic positioning is what makes the focal point focal; measurement independence would dilute the signal; but each instance of apparent strategic positioning increments the cost to future expert credibility. The rising theater_ratio (0.32 → 0.76) and steady extractiveness (0.42 → 0.68) over 35 years suggest the constraint is not stabilizing into piton (where performance would grow and function would vanish) but rather becoming more extractive as strategic positioning becomes explicit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_vs_communication_intent,
    'Is the annual clock setting primarily driven by changes in measurable indicators of existential risk, or by the stewardship community''s assessment of what reading will generate sufficient policy impact?',
    'Structured interviews with clock stewards comparing their contemporaneous technical notes (objective assessment) against their public statements and decision criteria; discourse analysis of meeting records; econometric analysis of clock movements versus objective indicator movements (nuclear deployments, climate metrics, pandemic lab incidents) to test whether clock readings track indicators or policy attention cycles.',
    'If communication intent dominates measurement, the reading classification shifts from Rope (coordination with extraction as overhead) to Snare (extraction under coordination cover). The constraint transitions from ''beneficial but imperfectly efficient'' to ''strategically dishonest.'' This reading''s classification rests on accepting substantial strategic positioning; if measurement proves dominant, reclassification to objective_index_reading is warranted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_vs_communication_intent, empirical, 'The factual basis for distinguishing this reading from the objective_index_reading.').

omega_variable(
    epistemic_cost_accumulation,
    'Is the degradation of future expert credibility from today''s strategic positioning a real structural cost, or is public skepticism toward expert judgment already so high that this constraint''s positioning has negligible additional impact?',
    'Longitudinal measurement of public trust in expert judgment on existential risks (surveys at 5-year intervals); attribution analysis comparing trust trajectories in domains where expert metrics are suspected of strategic positioning versus domains where they are not; prospective studies tracking how audiences who detect strategic positioning in the clock generalize their skepticism to other long-term risk forecasts.',
    'If the cost is real and accumulating, the constraint extracts substantially from the abstract victim (epistemic credibility), and the ''tangled rope'' classification holds — coordination (mobilization) rides on extraction (epistemic trust). If public trust is already degraded and additional positioning has marginal effect, the constraint approaches pure extraction (snare) without coordination benefit. If the cost is negligible, the constraint might be classified as Rope with acceptable overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_cost_accumulation, empirical, 'Whether the strategic positioning''s cost to future epistemic trust is substantial or marginal.').

omega_variable(
    kernel_reading_underdetermination,
    'Can a single institutional arrangement (the Bulletin''s stewardship) coherently instantiate all three kernel readings (objective_index, hybrid_legitimacy, performative_tool), or must the readings be considered incompatible formalizations of the same practice?',
    'Reconstruction of the Bulletin''s self-understanding across different documents and eras: official mission statements, steward interviews, published rationales for specific clock settings. Identify moments where stewards explicitly acknowledge or deny that strategic positioning occurs, and where they frame measurement precision versus communication effect.',
    'If the readings can be held simultaneously by a single stewardship structure (stewards see themselves as doing hybrid work: measuring real risk while ensuring message clarity), then they coexist within a single framework and should be marked coexists_with. If stewards must choose between readings and different eras exhibit different framings, the readings foreclose each other across time and should be marked forecloses. If the readings describe different upstream decisions with different consequences but do not logically contradict, they influence each other and should be marked influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether the three kernel readings are logically coexistible in a single stewardship model or represent incompatible framings.').

omega_variable(
    performative_tool_vs_false_summit,
    'Is this reading''s authorization claim (strategic communication as legitimate stewardship function) itself a natural-seeming cover for pure extraction, or is there genuine theoretical warrant for treating communicative power as a valid input to metric-setting?',
    'Philosophical analysis of the distinction between (a) acknowledging that communication effects matter and (b) using that acknowledgment to justify deliberate metric manipulation; comparison to other domains where measurement is known to produce behavioral effects (the ''observer effect'' in physics, publication bias in medicine, etc.) — determine whether those domains'' best practices support strategic positioning or mandate independence.',
    'If performative framing is itself the false summit (communication effects are real but do not justify strategic positioning), this reading should be reclassified as a Snare and marked forecloses against objective_index_reading, which maintains measurement independence. If performative positioning is theoretically warranted as legitimate stewardship in the existential risk domain (unique circumstances of diffuse, slow-moving threats), the reading holds as Tangled Rope and coexists_with hybrid_legitimacy_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(performative_tool_vs_false_summit, preference, 'Whether strategic communication is a legitimate epistemic tool or a false summit normalizing dishonesty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__performative_tool_reading, 1991, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t1991, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1991, 0.32).
narrative_ontology:measurement_basis(doom_tr_t1991, observed).
narrative_ontology:measurement(doom_tr_t2000, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement_basis(doom_tr_t2000, observed).
narrative_ontology:measurement(doom_tr_t2010, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2010, 0.48).
narrative_ontology:measurement_basis(doom_tr_t2010, observed).
narrative_ontology:measurement(doom_tr_t2018, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2018, 0.62).
narrative_ontology:measurement_basis(doom_tr_t2018, observed).
narrative_ontology:measurement(doom_tr_t2023, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2023, 0.71).
narrative_ontology:measurement_basis(doom_tr_t2023, observed).
narrative_ontology:measurement(doom_tr_t2026, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2026, 0.76).
narrative_ontology:measurement_basis(doom_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(doom_be_t1991, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1991, 0.42).
narrative_ontology:measurement_basis(doom_be_t1991, observed).
narrative_ontology:measurement(doom_be_t2000, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement_basis(doom_be_t2000, observed).
narrative_ontology:measurement(doom_be_t2010, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2010, 0.54).
narrative_ontology:measurement_basis(doom_be_t2010, observed).
narrative_ontology:measurement(doom_be_t2018, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2018, 0.61).
narrative_ontology:measurement_basis(doom_be_t2018, observed).
narrative_ontology:measurement(doom_be_t2023, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2023, 0.66).
narrative_ontology:measurement_basis(doom_be_t2023, observed).
narrative_ontology:measurement(doom_be_t2026, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(doom_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t1991, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1991, 0.28).
narrative_ontology:measurement_basis(doom_su_t1991, observed).
narrative_ontology:measurement(doom_su_t2000, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2000, 0.31).
narrative_ontology:measurement_basis(doom_su_t2000, observed).
narrative_ontology:measurement(doom_su_t2010, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2010, 0.35).
narrative_ontology:measurement_basis(doom_su_t2010, observed).
narrative_ontology:measurement(doom_su_t2018, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2018, 0.38).
narrative_ontology:measurement_basis(doom_su_t2018, observed).
narrative_ontology:measurement(doom_su_t2023, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2023, 0.41).
narrative_ontology:measurement_basis(doom_su_t2023, observed).
narrative_ontology:measurement(doom_su_t2026, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2026, 0.42).
narrative_ontology:measurement_basis(doom_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__performative_tool_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(doomsday_clock_metric__performative_tool_reading, 0.12).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric__objective_index_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric__hybrid_legitimacy_reading).

% DUAL FORMULATION NOTE:
% The doomsday_clock_metric kernel decomposes into three constraint stories per the ε-invariance principle. Each reading instantiates a different constraint with a different ε (measurement independence vs. strategic positioning vs. hybrid legitimacy), different beneficiary/victim structures, and different classifications. The objective_index_reading treats the clock as a near-mountain (measurement-driven, low extraction); the performative_tool_reading (this story) treats it as tangled_rope (coordination bundled with epistemic extraction); the hybrid_legitimacy_reading treats it as hybrid, acknowledging irreducible entanglement of science and normativity. All three readings share the same institutional referent but impose different boundaries around what constitutes legitimate stewardship. Network links enable contamination analysis: if strategic positioning (performative reading) becomes visible, epistemic trust in the objective_index_reading is eroded, which pressures the stewardship community toward the hybrid_legitimacy frame as justification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(doomsday_clock_metric__performative_tool_reading, organized, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
